package com.aam.producer.playlist.protocol.response;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;

public class PlaylistVersionInfo {

    @JSONField(name = "playlist_uuid")
    @JsonProperty("playlist_uuid")
    private String playlistUuid;

    @JSONField(name = "playlist_version_uuid")
    @JsonProperty("playlist_version_uuid")
    private String playlistVersionUuid;

    @JSONField(name = "content_list")
    @JsonProperty("content_list")
    private List<ContentInfo> contentList;

    @JSONField(name = "publish_time")
    @JsonProperty("publish_time")
    private Long publishTime;

    @JSONField(name = "publish_later")
    @JsonProperty("publish_later")
    private Boolean publishLater;

    @JSONField(name = "time_zone")
    @JsonProperty("time_zone")
    private String timeZone;

    private String status;

    private String extension;

    private Long created;

    @JSONField(name = "last_modified")
    @JsonProperty("last_modified")
    private Long lastModified;

    @JSONField(name = "organization_id")
    @JsonProperty("organization_id")
    private String organizationId;

    @JSONField(name = "show_attribute_groups")
    @JsonProperty("show_attribute_groups")
    private List<ShowAttributeGroupInfo> showAttributeGroups;

    public String getPlaylistVersionUuid() {
        return playlistVersionUuid;
    }

    public void setPlaylistVersionUuid(String playlistVersionUuid) {
        this.playlistVersionUuid = playlistVersionUuid;
    }

    public List<ContentInfo> getContentList() {
        return contentList;
    }

    public void setContentList(List<ContentInfo> contentList) {
        this.contentList = contentList;
    }

    public Long getPublishTime() {
        return publishTime;
    }

    public void setPublishTime(Long publishTime) {
        this.publishTime = publishTime;
    }

    public Boolean getPublishLater() {
        return publishLater;
    }

    public void setPublishLater(Boolean publishLater) {
        this.publishLater = publishLater;
    }

    public String getTimeZone() {
        return timeZone;
    }

    public void setTimeZone(String timeZone) {
        this.timeZone = timeZone;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getExtension() {
        return extension;
    }

    public void setExtension(String extension) {
        this.extension = extension;
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

    public String getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(String organizationId) {
        this.organizationId = organizationId;
    }

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }

    public List<ShowAttributeGroupInfo> getShowAttributeGroups() {
        return showAttributeGroups;
    }

    public void setShowAttributeGroups(
            List<ShowAttributeGroupInfo> showAttributeGroups) {
        this.showAttributeGroups = showAttributeGroups;
    }
}
