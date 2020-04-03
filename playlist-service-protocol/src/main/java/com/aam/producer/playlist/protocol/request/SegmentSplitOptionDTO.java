package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;

public class SegmentSplitOptionDTO {

    @JSONField(name = "group_left")
    @JsonProperty("group_left")
    private SegmentSplitDTO groupLeft;

    @JSONField(name = "group_right")
    @JsonProperty("group_right")
    private SegmentSplitDTO groupRight;

    public SegmentSplitDTO getGroupLeft() {
        return groupLeft;
    }

    public void setGroupLeft(SegmentSplitDTO groupLeft) {
        this.groupLeft = groupLeft;
    }

    public SegmentSplitDTO getGroupRight() {
        return groupRight;
    }

    public void setGroupRight(SegmentSplitDTO groupRight) {
        this.groupRight = groupRight;
    }

}
