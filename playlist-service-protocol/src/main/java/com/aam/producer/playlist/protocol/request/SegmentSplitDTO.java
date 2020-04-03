package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;

public class SegmentSplitDTO {

    @JSONField(name = "user_group")
    @JsonProperty("user_group")
    private Boolean userGroup;

    @JSONField(name = "uuid")
    @JsonProperty("uuid")
    private String uuid;

    @JSONField(name = "playlist_uuid")
    @JsonProperty("playlist_uuid")
    private String playlistUuid;

    @JSONField(name = "ppl_version_uuid")
    @JsonProperty("ppl_version_uuid")
    private String pplVersionUuid;

    @JSONField(name = "segment_association_uuid")
    @JsonProperty("segment_association_uuid")
    private String segmentAssociationUuid;

    @JSONField(name = "content_list")
    @JsonProperty("content_list")
    private List<ContentDTO> contentList = new ArrayList<>();

    @JSONField(name = "title_uuid")
    @JsonProperty("title_uuid")
    private String titleUuid;


    @JSONField(name = "parent_uuid")
    @JsonProperty("parent_uuid")
    private String parentUuid;


    @JSONField(name = "split_title")
    @JsonProperty("split_title")
    private String splitTitle;


    @JSONField(name = "split_type")
    @JsonProperty("split_type")
    private String splitType;


    @JSONField(name = "split_rule")
    @JsonProperty("split_rule")
    private String splitRule;


    @JSONField(name = "publish_time")
    @JsonProperty("publish_time")
    private Long publishTime;


    @JSONField(name = "valid_time")
    @JsonProperty("valid_time")
    private Long validTime;

    private String status;

    @JSONField(name = "default_group")
    @JsonProperty("default_group")
    private Boolean defaultGroup;

    @JSONField(name = "auto_group")
    @JsonProperty("auto_group")
    private Boolean autoGroup;


    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public List<ContentDTO> getContentList() {
        return contentList;
    }

    public void setContentList(List<ContentDTO> contentList) {
        this.contentList = contentList;
    }

    public String getSegmentAssociationUuid() {
        return segmentAssociationUuid;
    }

    public void setSegmentAssociationUuid(String segmentAssociationUuid) {
        this.segmentAssociationUuid = segmentAssociationUuid;
    }

    public String getParentUuid() {
        return parentUuid;
    }

    public void setParentUuid(String parentUuid) {
        this.parentUuid = parentUuid;
    }

    public String getSplitTitle() {
        return splitTitle;
    }

    public void setSplitTitle(String splitTitle) {
        this.splitTitle = splitTitle;
    }

    public String getSplitType() {
        return splitType;
    }

    public void setSplitType(String splitType) {
        this.splitType = splitType;
    }

    public String getSplitRule() {
        return splitRule;
    }

    public void setSplitRule(String splitRule) {
        this.splitRule = splitRule;
    }

    public Long getPublishTime() {
        return publishTime;
    }

    public void setPublishTime(Long publishTime) {
        this.publishTime = publishTime;
    }

    public Long getValidTime() {
        return validTime;
    }

    public void setValidTime(Long validTime) {
        this.validTime = validTime;
    }

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }

    public String getPplVersionUuid() {
        return pplVersionUuid;
    }

    public void setPplVersionUuid(String pplVersionUuid) {
        this.pplVersionUuid = pplVersionUuid;
    }

    public String getTitleUuid() {
        return titleUuid;
    }

    public void setTitleUuid(String titleUuid) {
        this.titleUuid = titleUuid;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Boolean getDefaultGroup() {
        return defaultGroup;
    }

    public void setDefaultGroup(Boolean defaultGroup) {
        this.defaultGroup = defaultGroup;
    }

    public Boolean getAutoGroup() {
        return autoGroup;
    }

    public void setAutoGroup(Boolean autoGroup) {
        this.autoGroup = autoGroup;
    }

    public Boolean getUserGroup() {
        return userGroup;
    }

    public void setUserGroup(Boolean userGroup) {
        this.userGroup = userGroup;
    }
}
