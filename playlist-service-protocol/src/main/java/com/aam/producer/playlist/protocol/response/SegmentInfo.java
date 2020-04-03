package com.aam.producer.playlist.protocol.response;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;

public class SegmentInfo {

    private String uuid;

    private String title;

    private String type;

    @JSONField(name = "content_kind")
    @JsonProperty("content_kind")
    private String contentKind;

    private Integer purpose;

    private Long created;

    @JSONField(name = "last_modified")
    @JsonProperty("last_modified")
    private Long lastModified;

    @JSONField(name = "visual_automation")
    @JsonProperty("visual_automation")
    private Boolean visualAutomation;

    @JSONField(name = "audio_automation")
    @JsonProperty("audio_automation")
    private Boolean audioAutomation;

    @JSONField(name = "multi_segment")
    @JsonProperty("multi_segment")
    private Boolean multiSegment;

    @JSONField(name = "split_by_week")
    @JsonProperty("split_by_week")
    private Boolean splitByWeek;

    @JSONField(name = "organization_id")
    @JsonProperty("organization_Id")
    private String organizationId;

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getContentKind() {
        return contentKind;
    }

    public void setContentKind(String contentKind) {
        this.contentKind = contentKind;
    }

    public Integer getPurpose() {
        return purpose;
    }

    public void setPurpose(Integer purpose) {
        this.purpose = purpose;
    }

    public Long getCreated() {
        return created;
    }

    public void setCreated(Long created) {
        this.created = created;
    }

    public Long getLastModified() {
        return lastModified;
    }

    public void setLastModified(Long lastModified) {
        this.lastModified = lastModified;
    }

    public Boolean getVisualAutomation() {
        return visualAutomation;
    }

    public void setVisualAutomation(Boolean visualAutomation) {
        this.visualAutomation = visualAutomation;
    }

    public Boolean getAudioAutomation() {
        return audioAutomation;
    }

    public void setAudioAutomation(Boolean audioAutomation) {
        this.audioAutomation = audioAutomation;
    }

    public Boolean getMultiSegment() {
        return multiSegment;
    }

    public void setMultiSegment(Boolean multiSegment) {
        this.multiSegment = multiSegment;
    }

    public Boolean getSplitByWeek() {
        return splitByWeek;
    }

    public void setSplitByWeek(Boolean splitByWeek) {
        this.splitByWeek = splitByWeek;
    }

    public String getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(String organizationId) {
        this.organizationId = organizationId;
    }
}
