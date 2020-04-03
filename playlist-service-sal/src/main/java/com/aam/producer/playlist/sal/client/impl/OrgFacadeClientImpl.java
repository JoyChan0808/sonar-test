package com.aam.producer.playlist.sal.client.impl;

import com.aam.producer.playlist.protocol.request.OrganizationDTO;
import com.aam.producer.playlist.sal.client.IOrgFacadeClient;
import com.aam.producer.playlist.sal.client.handler.OrgHandler;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.tomcat.util.buf.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class OrgFacadeClientImpl implements IOrgFacadeClient {

    @Value("${thunderstorm.userService.getComplexGroupUri}")
    private String getComplexGroupUri;

    @Value("${thunderstorm.userService.getOrganizations}")
    private String getOrganizations;

    @Value("${thunderstorm.userService.getOrganization}")
    private String getOrganization;

    @Value("${thunderstorm.complexService.listByGroupsUri}")
    private String listByGroupsUri;

    @Autowired
    private RestTemplate restTemplate;

    @Override
    public List<String> getComplexGroupUuid(String orgId) {
        Map<String, Object> params = new HashMap<>();
        params.put("organization_uuid", orgId);
        String forObject = restTemplate.getForObject(getComplexGroupUri, String.class, params);
        return OrgHandler.getComplexGroupUuid(forObject);
    }

    @Override
    public List<OrganizationDTO> getOrganizations() {
        String forObject = restTemplate.getForObject(getOrganizations, String.class);
        return OrgHandler.getOrganizations(forObject);
    }

    @Override
    public List<String> getComplexUuid(List<String> complexGroups) {
        Map<String, Object> params = new HashMap<>();
        params.put("uuids", StringUtils.join(complexGroups, ','));
        String forObject = restTemplate.getForObject(listByGroupsUri, String.class, params);
        return OrgHandler.getComplexUuid(forObject);
    }

    @Override
    @Cacheable(value="dayOfWeek")
    public Integer getDayOfWeek(String orgId) {
        Map<String, Object> params = new HashMap<>();
        params.put("uuid", orgId);
        String forObject = restTemplate.getForObject(getOrganization, String.class, params);
        return OrgHandler.getDayOfWeek(forObject);
    }

}
