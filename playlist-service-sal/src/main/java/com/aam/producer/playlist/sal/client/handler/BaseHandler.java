package com.aam.producer.playlist.sal.client.handler;

import com.aam.producer.playlist.sal.CommonResult;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;

public class BaseHandler {

    public static String getResult(String resultInfo) {
        CommonResult<String> result = JSON
                .parseObject(resultInfo, new TypeReference<CommonResult<String>>() {
                });
        if (result == null) {
            throw new RuntimeException();
        }
        return result.getData();
    }

}
