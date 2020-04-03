package com.aam.producer.playlist.protocol.response;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;

public class SegmentSplitInfo {

    private String uuid;

    private String sign;

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
    private List<ContentInfo> contentList = new ArrayList<>();

    @JSONField(name = "parent_uuid")
    @JsonProperty("parent_uuid")
    private String parentUuid;

    @JSONField(name = "title_uuid")
    @JsonProperty("title_uuid")
    private String titleUuid;

    @JSONField(name = "split_title")
    @JsonProperty("split_title")
    private String splitTitle;

    @JSONField(name = "split_type")
    @JsonProperty("split_type")
    private String splitType;

    @JSONField(name = "split_rule")
    @JsonProperty("split_rule")
    private String splitRule;

    private String status;

    @JSONField(name = "publish_time")
    @JsonProperty("publish_time")
    private Long publishTime;

    private Long created;

    @JSONField(name = "last_modified")
    @JsonProperty("last_modified")
    private Long lastModified;

    @JSONField(name = "sort_num")
    @JsonProperty("sort_num")
    private Long sortNum;

    @JSONField(name = "default_group")
    @JsonProperty("default_group")
    private Boolean defaultGroup;

    @JSONField(name = "auto_group")
    @JsonProperty("auto_group")
    private Boolean autoGroup;

    @JSONField(name = "user_group")
    @JsonProperty("user_group")
    private Boolean userGroup;

    @JSONField(serialize = false)
    @JsonIgnore
    private List<SplitComplexInfo> complexList = new ArrayList<>();

    @JSONField(serialize = false)
    @JsonIgnore
    private List<ShowAttributeGroupInfo> showAttributeGroupInfos = new ArrayList<>();

    private int shows;

    @JSONField(serialize = false)
    @JsonIgnore
    private List<String> showUuids = new ArrayList<>();

    private int sites;

    @JSONField(name = "rule_sites")
    @JsonProperty("rule_sites")
    private int ruleSites;

    private int percentage;

    private int duration;


    @JSONField(serialize = false)
    @JsonIgnore
    private List<SegmentSplitInfo> children = new ArrayList<>();

    @JSONField(serialize = false)
    @JsonIgnore
    private SegmentSplitInfo parent;

    @JSONField(serialize = false)
    @JsonIgnore
    private List<SegmentSplitInfo> siblingsAndSelf = new ArrayList<>();


    public int getRuleSites() {
        return ruleSites;
    }

    public void setRuleSites(int ruleSites) {
        this.ruleSites = ruleSites;
    }

    public String getSign() {
        return sign;
    }

    public void setSign(String sign) {
        this.sign = sign;
    }

    public String getTitleUuid() {
        return titleUuid;
    }

    public void setTitleUuid(String titleUuid) {
        this.titleUuid = titleUuid;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public List<ContentInfo> getContentList() {
        return contentList;
    }

    public void setContentList(List<ContentInfo> contentList) {
        this.contentList = contentList;
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

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public Long getPublishTime() {
        return publishTime;
    }

    public void setPublishTime(Long publishTime) {
        this.publishTime = publishTime;
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

    public String getSegmentAssociationUuid() {
        return segmentAssociationUuid;
    }

    public void setSegmentAssociationUuid(String segmentAssociationUuid) {
        this.segmentAssociationUuid = segmentAssociationUuid;
    }

    public List<SegmentSplitInfo> getChildren() {
        return children;
    }

    public void setChildren(List<SegmentSplitInfo> children) {
        this.children = children;
    }

    public Long getSortNum() {
        return sortNum;
    }

    public void setSortNum(Long sortNum) {
        this.sortNum = sortNum;
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

    public int getShows() {
        return shows;
    }

    public void setShows(int shows) {
        this.shows = shows;
    }

    public int getSites() {
        return sites;
    }

    public void setSites(int sites) {
        this.sites = sites;
    }

    public int getPercentage() {
        return percentage;
    }

    public void setPercentage(int percentage) {
        this.percentage = percentage;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public Boolean getUserGroup() {
        return userGroup;
    }

    public void setUserGroup(Boolean userGroup) {
        this.userGroup = userGroup;
    }

    public List<SegmentSplitInfo> getSiblingsAndSelf() {
        return siblingsAndSelf;
    }

    public void setSiblingsAndSelf(List<SegmentSplitInfo> siblingsAndSelf) {
        this.siblingsAndSelf = siblingsAndSelf;
    }

    public SegmentSplitInfo getParent() {
        return parent;
    }

    public void setParent(SegmentSplitInfo parent) {
        this.parent = parent;
    }

    public List<SplitComplexInfo> getComplexList() {
        return complexList;
    }

    public void setComplexList(List<SplitComplexInfo> complexList) {
        this.complexList = complexList;
    }

    public List<String> getShowUuids() {
        return showUuids;
    }

    public void setShowUuids(List<String> showUuids) {
        this.showUuids = showUuids;
    }

    public List<ShowAttributeGroupInfo> getShowAttributeGroupInfos() {
        return showAttributeGroupInfos;
    }

    public void setShowAttributeGroupInfos(List<ShowAttributeGroupInfo> showAttributeGroupInfos) {
        this.showAttributeGroupInfos = showAttributeGroupInfos;
    }
}
