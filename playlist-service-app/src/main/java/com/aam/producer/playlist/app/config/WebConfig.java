package com.aam.producer.playlist.app.config;

import com.aam.authentication.acl.AuthWebMvcConfigurationSupport;
import com.aam.producer.playlist.app.interceptor.MDCInterceptor;
import com.aam.producer.playlist.app.interceptor.OrgInterceptor;
import com.aam.utils.Constants;
import com.aam.utils.model.CommonResult;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.alibaba.fastjson.support.config.FastJsonConfig;
import com.alibaba.fastjson.support.spring.FastJsonHttpMessageConverter;
import io.sentry.Sentry;
import io.sentry.SentryClient;
import io.sentry.spring.SentryExceptionResolver;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.MethodParameter;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.env.Environment;
import org.springframework.http.MediaType;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.lang.Nullable;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.HandlerExceptionResolver;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;

/**
 * web config
 *
 * @author oliver.lo
 * @since 2019/3/25 9:41 AM
 */
@Configuration
public class WebConfig extends AuthWebMvcConfigurationSupport {

    private final static String LANG = "lang";

    @Value("${thunderstorm.restTemplate.connectTimeout}")
    private int connectTimeout = 3000;
    @Value("${thunderstorm.restTemplate.readTimeout}")
    private int readTimeout = 120000;
    @Value("${dsn}")
    private String dsn;
    @Value("${spring.profiles.active}")
    private String env;
    @Autowired
    private Environment environment;

    @Override
    public void addOtherInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(mdcInterceptor());
        registry.addInterceptor(localeChangeInterceptor());
        registry.addInterceptor(orgInterceptor());
    }

    @Bean
    public HandlerExceptionResolver sentryExceptionResolver() {
        SentryClient sentryClient = Sentry.init(dsn);
        sentryClient.setEnvironment(env);
        sentryClient.setRelease(environment.getProperty("release"));
        return new SentryExceptionResolver();
    }

    @Bean
    public RestTemplate restTemplate(RestTemplateBuilder restTemplateBuilder) {
        RestTemplate restTemplate = restTemplateBuilder
                .setConnectTimeout(connectTimeout)
                .setReadTimeout(readTimeout).build();
        restTemplate.setInterceptors(new ArrayList<ClientHttpRequestInterceptor>() {
            {
                add((request, body, execution) -> {
                    String traceId = MDC.get(MDCInterceptor.TRACE_ID_KEY);
                    if (!StringUtils.isEmpty(traceId)) {
                        request.getHeaders().add(MDCInterceptor.TRACE_ID_KEY, traceId);
                    }
                    return execution.execute(request, body);
                });
            }
        });
        return restTemplate;
    }

    @Bean
    public OrgInterceptor orgInterceptor() {
        return new OrgInterceptor();
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler(Constants.SWAGGER_ASSETS)
                .addResourceLocations(Constants.SWAGGER_ASSETS_LOCATIONS);
    }

    @Bean
    public MDCInterceptor mdcInterceptor() {
        return new MDCInterceptor();
    }


    @Override
    public void configureMessageConverters(List<HttpMessageConverter<?>> converters) {
        FastJsonHttpMessageConverter converter = new FastJsonHttpMessageConverter();
        FastJsonConfig fastJsonConfig = new FastJsonConfig();
        fastJsonConfig.setSerializerFeatures(SerializerFeature.WriteMapNullValue,
                SerializerFeature.WriteNullListAsEmpty);
        converter.setFastJsonConfig(fastJsonConfig);
        converters.add(converter);
    }

    @Bean
    public LocaleChangeInterceptor localeChangeInterceptor() {
        LocaleChangeInterceptor lci = new LocaleChangeInterceptor();
        lci.setParamName(LANG);
        return lci;
    }

    @ControllerAdvice
    public static class ResponseAdvice implements ResponseBodyAdvice {

        @Override
        public boolean supports(MethodParameter returnType, Class converterType) {
            return AnnotatedElementUtils
                    .hasAnnotation(returnType.getAnnotatedElement(), ReturnHandler.class);
        }

        @Nullable
        @Override
        public Object beforeBodyWrite(@Nullable Object body, MethodParameter returnType,
                MediaType selectedContentType, Class selectedConverterType,
                ServerHttpRequest request, ServerHttpResponse response) {
            CommonResult<Object> commonResult = new CommonResult<>();
            commonResult.setData(body);
            return commonResult;
        }
    }

}
