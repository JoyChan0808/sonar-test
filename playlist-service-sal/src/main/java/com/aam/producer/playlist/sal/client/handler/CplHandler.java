package com.aam.producer.playlist.sal.client.handler;

import com.aam.producer.playlist.sal.response.CplInfo;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import java.util.ArrayList;
import java.util.List;
import org.springframework.util.StringUtils;

public class CplHandler extends BaseHandler {

    public static List<CplInfo> getCplInfoByUuids(String forObject) {
        String objString = getResult(forObject);
        List<CplInfo> cplInfos = new ArrayList<>();
        JSONArray jsonArray = JSONObject.parseArray(objString);
        if (jsonArray == null) {
            return cplInfos;
        }
        int count = jsonArray.size();
        for (int i = 0; i < count; i++) {
            JSONObject jsonObject = jsonArray.getJSONObject(i);

            List<String> formatList = new ArrayList<>();
            CplInfo cplInfo = new CplInfo();
            cplInfo.setExtension(jsonObject.toJSONString());
            cplInfo.setUuid(jsonObject.getString("cpl_id"));
            cplInfo.setCreated(jsonObject.getString("created"));

            cplInfo.setTitle(jsonObject.getString("title"));
            if (StringUtils.isEmpty(cplInfo.getTitle())) {
                cplInfo.setTitle(jsonObject.getString("content_title"));
            }
            if (StringUtils.isEmpty(cplInfo.getTitle())) {
                cplInfo.setTitle(jsonObject.getString("content_title_text"));
            }

            formatList.add(jsonObject.getString("playback_mode"));

            String frameRate = jsonObject.getString("edit_rate");

            if (frameRate != null && frameRate.trim().length() != 0) {
                if (frameRate.compareTo("30") > 0) {
                    formatList.add("HFR");
                }
            }

            String resolution = jsonObject.getString("resolution");

            if (resolution != null && resolution.trim().length() != 0) {
                formatList.add(resolution);
            }

            JSONArray audioType = jsonObject.getJSONArray("audio_type");

            if (audioType != null && audioType.size() > 0) {
                for (int ii = 0; ii < audioType.size(); ii++) {
                    formatList.add(audioType.getString(ii));
                }
            }

            JSONArray subtitleLanguage = jsonObject.getJSONArray("subtitle_language");

            if (subtitleLanguage != null && subtitleLanguage.size() > 0) {
                for (int ii = 0; ii < subtitleLanguage.size(); ii++) {
                    formatList.add(subtitleLanguage.getString(ii));
                }
            }

            String aspectRatio = jsonObject.getString("aspect_ratio");

            if (aspectRatio != null && aspectRatio.trim().length() != 0) {
                aspectRatio = getAspectRatio(aspectRatio);
                if (aspectRatio != null) {
                    formatList.add(aspectRatio);
                }
            }

            cplInfo.setLanguage(jsonObject.getString("audio_language"));
            cplInfo.setSubtitleLanguage(jsonObject.getString("subtitle_language"));
            cplInfo.setFormatList(formatList);
            cplInfos.add(cplInfo);
        }

        return cplInfos;
    }


    private static String getAspectRatio(String aspectRatio) {
        switch (aspectRatio) {
            case "S":
                return "Scope";
            case "C":
                return "Full";
            case "F":
                return "Flat";
            default:
                return null;
        }
    }
}
