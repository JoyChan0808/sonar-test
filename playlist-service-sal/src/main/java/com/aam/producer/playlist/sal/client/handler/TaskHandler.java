package com.aam.producer.playlist.sal.client.handler;

import com.aam.producer.task.protocol.response.PPLIssueModel;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import java.util.List;
import java.util.Map;

public class TaskHandler extends BaseHandler {

    public static Map<String, PPLIssueModel> queryPplIssue(String resultInfo) {
        String objString = getResult(resultInfo);
        return JSON.parseObject(objString, new TypeReference<Map<String, PPLIssueModel>>() {
        });
    }

    public static List<PPLIssueModel> queryOnePplIssue(String resultInfo) {
        String objString = getResult(resultInfo);
        return JSON.parseObject(objString, new TypeReference<List<PPLIssueModel>>() {
        });
    }
}
