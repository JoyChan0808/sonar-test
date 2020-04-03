package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;

public class SegmentDTO {

    private String title;

    private Integer type;

    private Integer purpose;

    @JSONField(name = "split_by_week")
    @JsonProperty("split_by_week")
    private Boolean splitByWeek;

    public Integer getType() {
        return type;
    }

    public void setType(Integer type) {
        this.type = type;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Integer getPurpose() {
        return purpose;
    }

    public void setPurpose(Integer purpose) {
        this.purpose = purpose;
    }

    public Boolean getSplitByWeek() {
        return splitByWeek;
    }

    public void setSplitByWeek(Boolean splitByWeek) {
        this.splitByWeek = splitByWeek;
    }
}
