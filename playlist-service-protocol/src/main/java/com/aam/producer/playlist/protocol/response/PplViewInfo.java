package com.aam.producer.playlist.protocol.response;

import java.util.List;

public class PplViewInfo {

    private String pplUuid;

    private String playlistUuid;

    private String releaseVersionUuid;

    private String complexUuid;

    private String complexName;

    private String organizationId;

    private String title;

    private String status;

    private Integer automaticallyApply;

    private Long lastModified;

    private Long shows;

    private Long sites;

    private String type;

    private Boolean is3d;

    private String issueType;

    private Integer issueLevel;

    private List<ShowAttributeGroupInfo> showTypes;

    public String getComplexName() {
        return complexName;
    }

    public void setComplexName(String complexName) {
        this.complexName = complexName;
    }

    public String getPplUuid() {
        return pplUuid;
    }

    public void setPplUuid(String pplUuid) {
        this.pplUuid = pplUuid;
    }

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }

    public String getComplexUuid() {
        return complexUuid;
    }

    public void setComplexUuid(String complexUuid) {
        this.complexUuid = complexUuid;
    }

    public String getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(String organizationId) {
        this.organizationId = organizationId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Integer getAutomaticallyApply() {
        return automaticallyApply;
    }

    public void setAutomaticallyApply(Integer automaticallyApply) {
        this.automaticallyApply = automaticallyApply;
    }

    public Long getLastModified() {
        return lastModified;
    }

    public void setLastModified(Long lastModified) {
        this.lastModified = lastModified;
    }

    public Long getShows() {
        return shows;
    }

    public void setShows(Long shows) {
        this.shows = shows;
    }

    public Long getSites() {
        return sites;
    }

    public void setSites(Long sites) {
        this.sites = sites;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Boolean getIs3d() {
        return is3d;
    }

    public void setIs3d(Boolean is3d) {
        this.is3d = is3d;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getReleaseVersionUuid() {
        return releaseVersionUuid;
    }

    public void setReleaseVersionUuid(String releaseVersionUuid) {
        this.releaseVersionUuid = releaseVersionUuid;
    }

    public List<ShowAttributeGroupInfo> getShowTypes() {
        return showTypes;
    }

    public void setShowTypes(
            List<ShowAttributeGroupInfo> showTypes) {
        this.showTypes = showTypes;
    }

    public String getIssueType() {
        return issueType;
    }

    public void setIssueType(String issueType) {
        this.issueType = issueType;
    }

    public Integer getIssueLevel() {
        return issueLevel;
    }

    public void setIssueLevel(Integer issueLevel) {
        this.issueLevel = issueLevel;
    }
}
