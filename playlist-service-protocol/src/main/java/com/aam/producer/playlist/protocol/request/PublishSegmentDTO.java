package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;

public class PublishSegmentDTO {

    @JSONField(name = "publish_time")
    @JsonProperty("publish_time")
    private Long publishTime;

    private List<TitleInfoDTO> titleInfos = new ArrayList<>();

    public Long getPublishTime() {
        return publishTime;
    }

    public void setPublishTime(Long publishTime) {
        this.publishTime = publishTime;
    }

    public List<TitleInfoDTO> getTitleInfos() {
        return titleInfos;
    }

    public void setTitleInfos(
            List<TitleInfoDTO> titleInfos) {
        this.titleInfos = titleInfos;
    }
}
