package com.aam.producer.playlist.sal.client.impl;

import com.aam.producer.playlist.sal.CommonResult;
import com.aam.producer.playlist.sal.client.ICplFacadeClient;
import com.aam.producer.playlist.sal.client.handler.CplHandler;
import com.aam.producer.playlist.sal.response.CplInfo;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.client.RestTemplate;

@Service
public class ICplFacadeClientImpl implements ICplFacadeClient {

    private final RestTemplate restTemplate;

    @Value("${thunderstorm.producerViewService.getCplUri}")
    private String getCplUri;

    @Value("${thunderstorm.cplService.checkCplRatings}")
    private String checkCplRatings;

    @Autowired
    public ICplFacadeClientImpl(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    public List<CplInfo> getCplInfoByUuids(Set<String> cplUuids, String orgId) {
        if (CollectionUtils.isEmpty(cplUuids)) {
            return new ArrayList<>();
        }
        Map<String, Object> params = new HashMap<>();
        params.put("cpl_uuids", String.join(",", cplUuids));
        params.put("organization_id", orgId);
        String forObject = restTemplate.getForObject(getCplUri, String.class, params);
        return CplHandler.getCplInfoByUuids(forObject);
    }

    @Override
    public Map<String, Boolean> checkCplRatings(Map<String, Map<String, String>> cplRatings) {
        Map<String, Object> params = new HashMap<>();
        params.put("ratings", JSON.toJSONString(cplRatings));
        String forObject = restTemplate.getForObject(checkCplRatings, String.class, params);
        CommonResult<String> result = JSON
                .parseObject(forObject, new TypeReference<CommonResult<String>>() {
                });
        if (result == null) {
            return null;
        }
        if (result.getCode() != 200) {
            throw new RuntimeException(result.getMessage());
        }
        return JSON.parseObject(result.getData(), new TypeReference<Map<String, Boolean>>() {
        });
    }
}
