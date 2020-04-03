package com.aam.producer.playlist.protocol.response;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Objects;

public class ContentInfo {

    @JSONField(name = "content_association_uuid")
    @JsonProperty("content_association_uuid")
    private String contentAssociationUuid;

    @JSONField(name = "content_id")
    @JsonProperty("content_id")
    private String contentId;

    @JSONField(name = "ppl_version_id")
    @JsonProperty("ppl_version_id")
    private String pplVersionId;

    @JSONField(name = "content_type")
    @JsonProperty("content_type")
    private String contentType;

    @JSONField(name = "content_kind")
    @JsonProperty("content_kind")
    private String contentKind;

    @JSONField(name = "title")
    @JsonProperty("title")
    private String title;

    @JSONField(name = "extension")
    @JsonProperty("extension")
    private String extension;

    @JSONField(name = "status")
    @JsonProperty("status")
    private String status;

    @JSONField(name = "version")
    @JsonProperty("version")
    private String version;

    @JSONField(name = "issue_level")
    @JsonProperty("issue_level")
    private Integer issueLevel;

    @JSONField(name = "issue_type")
    @JsonProperty("issue_type")
    private String issueType;

    public String getContentType() {
        return contentType;
    }

    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getExtension() {
        return extension;
    }

    public void setExtension(String extension) {
        this.extension = extension;
    }

    public String getContentAssociationUuid() {
        return contentAssociationUuid;
    }

    public void setContentAssociationUuid(String contentAssociationUuid) {
        this.contentAssociationUuid = contentAssociationUuid;
    }

    public String getContentId() {
        return contentId;
    }

    public void setContentId(String contentId) {
        this.contentId = contentId;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }


    public Integer getIssueLevel() {
        return issueLevel;
    }

    public void setIssueLevel(Integer issueLevel) {
        this.issueLevel = issueLevel;
    }

    public String getIssueType() {
        return issueType;
    }

    public void setIssueType(String issueType) {
        this.issueType = issueType;
    }

    public String getContentKind() {
        return contentKind;
    }

    public void setContentKind(String contentKind) {
        this.contentKind = contentKind;
    }

    public String getPplVersionId() {
        return pplVersionId;
    }

    public void setPplVersionId(String pplVersionId) {
        this.pplVersionId = pplVersionId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ContentInfo that = (ContentInfo) o;
        return Objects.equals(contentAssociationUuid, that.contentAssociationUuid);
    }

    @Override
    public int hashCode() {
        return Objects
                .hash(contentAssociationUuid, contentId, contentType, title, extension, status,
                        version,
                        issueLevel, issueType);
    }
}
