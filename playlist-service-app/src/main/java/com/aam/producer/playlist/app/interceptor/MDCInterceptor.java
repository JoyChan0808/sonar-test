package com.aam.producer.playlist.app.interceptor;

import java.util.UUID;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.MDC;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

public class MDCInterceptor extends HandlerInterceptorAdapter {

    public final static String TRACE_ID_KEY = "TS-Request-ID";

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response,
            Object handler) {
        String traceId = request.getHeader(TRACE_ID_KEY);
        if (StringUtils.isEmpty(traceId)) {
            traceId = UUID.randomUUID().toString().replaceAll("-", "");
        }
        MDC.put(TRACE_ID_KEY, traceId);
        return true;
    }

}
