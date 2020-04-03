package com.aam.producer.playlist.protocol.request;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TitleInfoDTO {

    private String uuid;

    private Map<String,List<SegmentDTO1>> pplSegmentMap = new HashMap<>();

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public Map<String, List<SegmentDTO1>> getPplSegmentMap() {
        return pplSegmentMap;
    }

    public void setPplSegmentMap(
            Map<String, List<SegmentDTO1>> pplSegmentMap) {
        this.pplSegmentMap = pplSegmentMap;
    }
}
