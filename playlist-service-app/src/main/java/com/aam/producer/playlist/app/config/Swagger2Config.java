package com.aam.producer.playlist.app.config;

import com.aam.utils.utils.SwaggerUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * swagger2 config
 *
 * @author oliver.lo
 * @since 2019/3/25 9:34 AM
 */
@EnableSwagger2
@Configuration
public class Swagger2Config {

    private static final String API_VERSION = "v1";
    private static final String SECURITY_PATH_REGEX = "/api/v.*";
    private static final String API_BASE_PACKAGE = "com.aam.producer.playlist.app";
    private static final String API_INFO = "playlist service.";
    private static final String CONTACT_URL = "https://github.com/artsalliancemedia/playlist-service";
    private static final String CONTACT_EMAIL = "@artsalliancemedia.com";

    @Value("${spring.application.name}")
    private String groupName;

    @Bean
    public Docket authCenterDocket() {
        return SwaggerUtils
                .createDocket(groupName, API_BASE_PACKAGE, createApiInfo(), SECURITY_PATH_REGEX);
    }

    private ApiInfo createApiInfo() {
        return SwaggerUtils
                .createApiInfo(groupName, API_INFO, API_VERSION, groupName, CONTACT_URL,
                        CONTACT_EMAIL);
    }
}
