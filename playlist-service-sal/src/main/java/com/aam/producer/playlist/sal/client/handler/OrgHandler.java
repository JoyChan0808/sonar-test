package com.aam.producer.playlist.sal.client.handler;

import com.aam.producer.playlist.protocol.request.OrganizationDTO;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import java.util.ArrayList;
import java.util.List;

public class OrgHandler extends BaseHandler {

    public static List<String> getComplexGroupUuid(String resultInfo) {
        String objString = getResult(resultInfo);
        JSONObject jsonObject = JSON.parseObject(objString);
        return JSON.parseArray(jsonObject.getString("group_uuids"), String.class);
    }

    public static List<OrganizationDTO> getOrganizations(String forObject) {
        String objString = getResult(forObject);
        return JSON.parseArray(objString, OrganizationDTO.class);
    }

    public static List<String> getComplexUuid(String forObject) {
        List<String> result = new ArrayList<>();
        String objString = getResult(forObject);
        JSONArray jsonArray = JSON.parseArray(objString);
        for (int i = 0; i < jsonArray.size(); i++) {
            JSONObject jsonObject = jsonArray.getJSONObject(i);
            result.add(jsonObject.getString("uuid"));
        }
        return result;
    }

    public static Integer getDayOfWeek(String forObject) {
        String objString = getResult(forObject);
        JSONObject jsonObject = JSON.parseObject(objString);
        return jsonObject.getInteger("first_day_of_week");
    }
}
