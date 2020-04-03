package com.aam.producer.playlist.sal.client.impl;

import com.aam.producer.playlist.protocol.response.SplitComplexInfo;
import com.aam.producer.playlist.sal.CommonResult;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.aam.producer.playlist.sal.client.handler.ComplexHandler;
import com.aam.producer.playlist.sal.response.ComplexGroup;
import com.aam.producer.playlist.sal.response.LmsInfo;
import com.aam.producer.playlist.sal.response.OrganizationInfo;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.client.RestTemplate;

/**
 * complex service impl
 *
 * @author oliver.lo
 * @since 2019/6/11 11:26 AM
 */
@Service
public class ComplexFacadeClientImpl implements IComplexFacadeClient {

    private final RestTemplate restTemplate;
    @Value("${thunderstorm.complexService.getLmsUri}")
    private String getLmsUri;
    @Value("${thunderstorm.userService.getOrgUri}")
    private String getOrgUri;
    @Value("${thunderstorm.complexService.getComplex}")
    private String getComplex;
    @Value("${thunderstorm.complexService.getComplexes}")
    private String getComplexes;
    @Value("${thunderstorm.complexService.getComplexByGroup}")
    private String getComplexByGroup;
    @Value("${thunderstorm.complexService.getComplexGroup}")
    private String getComplexGroup;

    @Autowired
    public ComplexFacadeClientImpl(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    @Cacheable(value = "lms")
    public String getLmsUuid(String complexId) {
        Map<String, String> params = new HashMap<>();
        params.put("complex_uuid", complexId);
        String forObject = restTemplate.getForObject(getLmsUri, String.class, params);
        CommonResult<LmsInfo> result = JSON
                .parseObject(forObject, new TypeReference<CommonResult<LmsInfo>>() {
                });
        if (result != null && result.getData() != null) {
            return result.getData().getLmsUuid();
        }
        return null;
    }

    @Override
    @CacheEvict(value = "lms")
    public void lmsCacheEvict(String complexId) {

    }

    @Override
    @Cacheable(value = "organizationUuid", key = "#complexId", unless = "#result==null")
    public String getOrganizationUuid(String complexId) {
        Map<String, String> params = new HashMap<>();
        params.put("complex_uuid", complexId);
        String forObject = restTemplate.getForObject(getOrgUri, String.class, params);
        CommonResult<OrganizationInfo> result = JSON
                .parseObject(forObject, new TypeReference<CommonResult<OrganizationInfo>>() {
                });
        if (result != null && result.getData() != null) {
            return result.getData().getOrganizationUuid();
        }
        return null;
    }

    @Override
    @CacheEvict(value = "organizationUuid", key = "#complexId")
    public void orgCacheEvict(String complexId) {

    }

    @Override
    @Cacheable(value = "complexLocale", key = "#complexUuid", unless = "#result==null")
    public String getComplexLocale(String complexUuid) {
        if (complexUuid == null || complexUuid.isEmpty()) {
            return null;
        }
        Map<String, String> params = new HashMap<>();
        params.put("uuids", complexUuid);
        String forObject = restTemplate.getForObject(getComplex, String.class, params);
        JSONObject jsonObject = JSON.parseObject(forObject);
        if (jsonObject == null) {
            return null;
        }
        JSONArray objects = jsonObject.getJSONArray("data");
        if (objects == null) {
            return null;
        }
        for (int i = 0; i < objects.size(); i++) {
            JSONObject object = objects.getJSONObject(i);
            if (object == null) {
                continue;
            }
            JSONObject complexInfo = object.getJSONObject("complex_info");
            if (complexInfo == null) {
                continue;
            }
            JSONObject scrwConfigInfo = complexInfo.getJSONObject("scrw_config_info");
            if (scrwConfigInfo == null) {
                continue;
            }
            return scrwConfigInfo.getString("timezone");
        }
        return null;
    }


    public Map<String, List<String>> getComplexDeviceInfo(String complexUuid) {
        Map<String, List<String>> map = new HashMap<>();
        if (complexUuid == null || complexUuid.isEmpty()) {
            return map;
        }
        Map<String, String> params = new HashMap<>();
        params.put("uuids", complexUuid);
        String forObject = restTemplate.getForObject(getComplex, String.class, params);
        JSONObject jsonObject = JSON.parseObject(forObject);
        if (jsonObject == null) {
            return map;
        }
        JSONArray objects = jsonObject.getJSONArray("data");
        if (objects == null) {
            return map;
        }
        for (int i = 0; i < objects.size(); i++) {
            JSONObject object = objects.getJSONObject(i);
            if (object == null) {
                continue;
            }
            JSONArray devices = object.getJSONArray("devices");
            if (devices == null || devices.size() == 0) {
                continue;
            }
            for (int ii = 0; ii < devices.size(); ii++) {
                JSONObject device = devices.getJSONObject(ii);
                String category = device.getString("category");
                if (!StringUtils.isEmpty(category)) {
                    List<String> value = map.get(category);
                    if (value == null) {
                        value = new ArrayList<>();
                    }
                    value.add(device.getString("id"));
                    map.put(category, value);
                }
            }
        }
        return map;
    }

    @Override
    public List<SplitComplexInfo> getComplex(List<String> complexGroup) {
        Map<String, Object> params = new HashMap<>();
        params.put("uuids", String.join(",", complexGroup));
        String forObject = restTemplate.getForObject(getComplexByGroup, String.class, params);
        return ComplexHandler.getComplex(forObject);
    }

    @Override
    public Map<String, String> getComplexNameMap(List<String> complexUuids) {
        Map<String, String> result = new HashMap<>();
        Map<String, String> params = new HashMap<>();
        params.put("uuids", String.join(",", complexUuids));
        String forObject = restTemplate.getForObject(getComplex, String.class, params);
        JSONObject jsonObject = JSON.parseObject(forObject);
        if (jsonObject == null) {
            return null;
        }
        JSONArray objects = jsonObject.getJSONArray("data");
        if (objects == null) {
            return null;
        }
        for (int i = 0; i < objects.size(); i++) {
            JSONObject object = objects.getJSONObject(i);
            String name = object.getString("complex_name");
            String uuid = object.getString("complex_uuid");
            result.put(uuid, name);
        }
        return result;
    }

    @Override
    public ComplexGroup getComplexGroup(String groupUuid) {
        Map<String, String> params = new HashMap<>();
        params.put("uuid", groupUuid);
        return ComplexHandler
                .getComplexGroup(restTemplate.getForObject(getComplexGroup, String.class, params));
    }

    @Override
    public Map<String, String> getAddressCountry(Set<String> complexUuids) {
        Map<String, String> params = new HashMap<>();
        if (CollectionUtils.isEmpty(complexUuids)) {
            return params;
        }
        params.put("uuids", String.join(",", complexUuids));
        return ComplexHandler
                .getAddressCountry(restTemplate.getForObject(getComplexes, String.class, params));
    }


}
