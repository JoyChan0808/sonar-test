package com.aam.producer.playlist.app.interceptor;

import com.aam.authentication.acl.model.TokenPayload;
import com.aam.authentication.acl.utils.UserInfoUtil;
import com.aam.producer.playlist.biz.util.OrgUtil;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

public class OrgInterceptor extends HandlerInterceptorAdapter {

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response,
            Object handler) throws Exception {
        TokenPayload tokenPayload = UserInfoUtil.get();
        if (tokenPayload != null) {
            OrgUtil.orgContenter.set(tokenPayload.getOrganizationId());
        }
        return super.preHandle(request, response, handler);
    }
}
