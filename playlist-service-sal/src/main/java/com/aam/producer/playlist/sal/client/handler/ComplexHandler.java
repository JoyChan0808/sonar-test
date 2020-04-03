package com.aam.producer.playlist.sal.client.handler;

import com.aam.producer.playlist.protocol.response.SplitComplexInfo;
import com.aam.producer.playlist.sal.response.ComplexGroup;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ComplexHandler extends BaseHandler {

    public static List<SplitComplexInfo> getComplex(String forObject) {
        String result = getResult(forObject);
        return JSON.parseArray(result, SplitComplexInfo.class);
    }

    public static ComplexGroup getComplexGroup(String forObject) {
        String result = getResult(forObject);
        return JSON.parseObject(result, ComplexGroup.class);
    }

    public static Map<String, String> getAddressCountry(String forObject) {
        Map<String, String> stringStringMap = new HashMap<>();
        String result = getResult(forObject);
        JSONArray jsonArray = JSON.parseArray(result);
        for (int i = 0; i < jsonArray.size(); i++) {
            JSONObject jsonObject = jsonArray.getJSONObject(i);
            stringStringMap
                    .put(jsonObject.getString("uuid"), jsonObject.getString("address_country"));
        }
        return stringStringMap;
    }
}
