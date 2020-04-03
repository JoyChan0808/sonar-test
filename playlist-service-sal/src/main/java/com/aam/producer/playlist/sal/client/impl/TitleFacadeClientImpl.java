package com.aam.producer.playlist.sal.client.impl;

import com.aam.producer.playlist.sal.client.ITitleFacadeClient;
import com.aam.producer.playlist.sal.client.handler.TitleHandler;
import com.aam.producer.playlist.sal.response.TitleInfo;
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

/**
 * title service impl
 *
 * @author kaiden.peng
 * @since 2019/53/16 5:53 PM
 */
@Service
public class TitleFacadeClientImpl implements ITitleFacadeClient {

    private final RestTemplate restTemplate;
    @Value("${thunderstorm.titleService.getTitlesUri}")
    private String getTitlesUri;
    @Value("${thunderstorm.titleService.getTitleUri}")
    private String getTitleUri;

    @Autowired
    public TitleFacadeClientImpl(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }


    @Override
    public List<TitleInfo> geTitlesByUuids(Set<String> titleUuids, List<String> groupUuid) {
        if (CollectionUtils.isEmpty(titleUuids)) {
            return new ArrayList<>();
        }
        Map<String, Object> params = new HashMap<>();
        params.put("uuids", String.join(",", titleUuids));
        params.put("group_uuids", String.join(",", groupUuid));
        String forObject = restTemplate.getForObject(getTitlesUri, String.class, params);
        return TitleHandler.geTitlesByUuids(forObject);
    }

    @Override
    public TitleInfo geTitlesByUuid(String titleUuid) {
        Map<String, Object> params = new HashMap<>();
        params.put("uuid", titleUuid);
        String forObject = restTemplate.getForObject(getTitleUri, String.class, params);
        return TitleHandler.geTitlesByUuid(forObject);
    }

}
