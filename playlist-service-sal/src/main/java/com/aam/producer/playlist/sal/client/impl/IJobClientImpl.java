package com.aam.producer.playlist.sal.client.impl;

import com.aam.producer.playlist.sal.client.IJobClient;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import java.util.HashMap;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

@Service
public class IJobClientImpl implements IJobClient {

    private final RestTemplate restTemplate;
    @Value("${thunderstorm.jobService.add}")
    private String add;
    @Value("${thunderstorm.jobService.remove}")
    private String remove;
    @Value("${thunderstorm.jobService.jobGroup}")
    private String jobGroup;
    @Value("${thunderstorm.jobService.callback}")
    private String callback;

    @Autowired
    public IJobClientImpl(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    public String add(Map<String, String> paramMap) {
        MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("jobGroup", jobGroup);
        params.add("jobDesc", paramMap.get("playlistTitle"));
        params.add("executorRouteStrategy", "FIRST");
        params.add("jobCron", paramMap.get("cron"));
        params.add("glueType", "GLUE_SHELL");
        params.add("triggerStatus", "1");
        params.add("executorBlockStrategy", "SERIAL_EXECUTION");
        params.add("author", "playlist-service");
        params.add("glueSource", ""
                + "#!/bin/bash\n"
                + "curl " + callback
                + "/" + paramMap.get("orgUuid")
                + "/" + paramMap.get("pplUuid")
                + "/" + paramMap.get("versionUuid")
                + "\nexit 0");

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);

        HttpEntity<MultiValueMap<String, String>> requestEntity = new HttpEntity<>(params, headers);

        ResponseEntity<String> result = restTemplate
                .exchange(add, HttpMethod.POST, requestEntity, String.class);

        if (result.getStatusCode() != HttpStatus.OK) {
            throw new RuntimeException(
                    "code:" + result.getStatusCode() + " body:" + result.getBody());
        }
        JSONObject jsonObject = JSON.parseObject(result.getBody());
        Integer code = jsonObject.getInteger("code");
        if (jsonObject.getInteger("code") != 200) {
            throw new RuntimeException(
                    "error code:" + code + " body:" + result.getBody());
        }
        return jsonObject.getString("content");
    }

    @Override
    public void remove(String id) {
        Map<String, String> param = new HashMap<>();
        param.put("id", id);
        restTemplate.getForObject(remove, String.class, param);
    }
}
