package com.aam.producer.playlist.sal.client.impl;

import com.aam.producer.playlist.sal.client.IFilmHallFacadeClient;
import com.aam.producer.playlist.sal.client.handler.FilmHallHandler;
import com.aam.producer.playlist.sal.response.FilmHallInfo;
import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
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
public class FilmHallFacadeClientImpl implements IFilmHallFacadeClient {

    private final RestTemplate restTemplate;
    @Value("${thunderstorm.complexService.getFilmHallUri}")
    private String getFilmHallUri;

    @Autowired
    public FilmHallFacadeClientImpl(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }


    @Override
    public List<FilmHallInfo> getFilmHallInfosByComplexUuids(Set<String> complexUuids) {
        if (CollectionUtils.isEmpty(complexUuids)) {
            return new ArrayList<>();
        }
        List<List<String>> subSets = Lists.partition(Lists.newArrayList(complexUuids), 30);
        return subSets.parallelStream().map(strings -> {
            Map<String, Object> params = new HashMap<>();
            params.put("uuids", String.join(",", strings));
            String forObject = restTemplate.getForObject(getFilmHallUri, String.class, params);
            return FilmHallHandler.getFilmHallInfosByScreenUuids(forObject);
        }).flatMap(Collection::stream).collect(Collectors.toList());
    }
}
