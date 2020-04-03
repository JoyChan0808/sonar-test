package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * <p>
 * <p>
 * </p>
 *
 * @author ${author}
 * @since 2019-05-14
 */
public class SegmentDTO {

    /**
     * 主键ID
     */
    private String uuid;

    /**
     * 组织ID
     */
    @JSONField(name = "organization_id")
    @JsonProperty("organization_id")
    private String organizationId;

    /**
     * association Uuid
     */
    @JSONField(name = "association_uuid")
    @JsonProperty("association_uuid")
    private String associationUuid;

    /**
     * 标题
     */
    @JSONField(name = "text")
    @JsonProperty("text")
    private String title;

    /**
     * 类型
     */
    @JSONField(name = "content_kind")
    @JsonProperty("content_kind")
    private String type;


    /**
     * big type
     */
    @JSONField(name = "type")
    @JsonProperty("type")
    private String bigType;


    /**
     * 目的
     */
    private Integer purpose;

    /**
     * 是否删除
     */
    private Boolean deleted;

    /**
     * 创建时间
     */
    private Long created;

    /**
     * 修改时间
     */
    @JSONField(name = "last_modified")
    @JsonProperty("last_modified")
    private Long lastModified;

    @JSONField(name = "split_by_week")
    @JsonProperty("split_by_week")
    private Boolean splitByWeek;

    public Integer getPurpose() {
        return purpose;
    }

    public void setPurpose(Integer purpose) {
        this.purpose = purpose;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
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

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Boolean getDeleted() {
        return deleted;
    }

    public void setDeleted(Boolean deleted) {
        this.deleted = deleted;
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

    public String getBigType() {
        return bigType;
    }

    public void setBigType(String bigType) {
        this.bigType = bigType;
    }

    public String getAssociationUuid() {
        return associationUuid;
    }

    public void setAssociationUuid(String associationUuid) {
        this.associationUuid = associationUuid;
    }

    public Boolean getSplitByWeek() {
        return splitByWeek;
    }

    public void setSplitByWeek(Boolean splitByWeek) {
        this.splitByWeek = splitByWeek;
    }

    @Override
    public String toString() {
        return "SegmentDO{" +
                "uuid=" + uuid +
                ", organizationId=" + organizationId +
                ", title=" + title +
                ", type=" + type +
                ", bigType=" + bigType +
                ", deleted=" + deleted +
                ", created=" + created +
                ", lastModified=" + lastModified +
                ", associationUuid=" + associationUuid +
                ", splitByWeek=" + splitByWeek +
                "}";
    }
}
