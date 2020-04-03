package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ContentDTO {

    @JSONField(name = "content_association_uuid")
    @JsonProperty("content_association_uuid")
    private String contentAssociationUuid;

    @JSONField(name = "content_id")
    @JsonProperty("content_id")
    private String contentId;

    @JSONField(name = "content_type")
    @JsonProperty("content_type")
    private String contentType;

    @JSONField(name = "title")
    @JsonProperty("title")
    private String title;

    @JSONField(name = "extension")
    @JsonProperty("extension")
    private String extension;

    private String oldContentAssociationUuid;

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

    public String getOldContentAssociationUuid() {
        return oldContentAssociationUuid;
    }

    public void setOldContentAssociationUuid(String oldContentAssociationUuid) {
        this.oldContentAssociationUuid = oldContentAssociationUuid;
    }
}
