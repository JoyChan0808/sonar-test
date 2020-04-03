package com.aam.producer.playlist.sal.client.handler;

import com.aam.producer.playlist.sal.response.TitleInfo;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import java.util.List;

public class TitleHandler extends BaseHandler {

    public static List<TitleInfo> geTitlesByUuids(String resultInfo) {
        String objString = getResult(resultInfo);
        return JSON.parseObject(objString, new TypeReference<List<TitleInfo>>() {
        });
    }

    public static TitleInfo geTitlesByUuid(String forObject) {
        String objString = getResult(forObject);
        List<TitleInfo> titleInfos = JSON
                .parseObject(objString, new TypeReference<List<TitleInfo>>() {
                });
        if (titleInfos == null || titleInfos.isEmpty()) {
            return null;
        }
        return titleInfos.get(0);
    }
}
