package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SegmentDTO1 {
    @JSONField(name = "content_id")
    @JsonProperty("content_id")
    private String contentId;
    @JSONField(name = "content_association_uuid")
    @JsonProperty("content_association_uuid")
    private String contentAssociationUuid;
    @JSONField(name = "content_kind")
    @JsonProperty("content_kind")
    private String contentKind;
    @JSONField(name = "status")
    @JsonProperty("status")
    private String status;
}
