package com.aam.producer.playlist.repository.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;

/**
 * <p>
 *
 * </p>
 *
 * @author ${author}
 * @since 2019-04-29
 */
@TableName("tms_playlist")
public class TmsPlaylistDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;

    /**
     * 组织UUID
     */
    @TableField("organization_id")
    private String organizationId;

    /**
     * playlist主键UUID
     */
    @TableField("playlist_uuid")
    private String playlistUuid;

    /**
     * 标题
     */
    @TableField("title")
    private String title;

    /**
     * content列表内容
     */
    @TableField("json")
    private String json;

    /**
     * content uuids
     */
    @TableField("content_ids")
    private String contentIds;

    /**
     * 是否模板
     */
    @TableField("templated")
    private Boolean templated;

    /**
     * 来源，TMS/producer
     */
    @TableField("source")
    private Integer source;

    /**
     * 来源producer播放列表UUID
     */
    @TableField("source_ppl_id")
    private String sourcePplId;

    /**
     * 来源producer播放列表版本UUID
     */
    @TableField("source_ppl_version_id")
    private String sourcePplVersionId;

    /**
     * 来源segment分组UUID
     */
    @TableField("source_segment_split_uuid")
    private String sourceSegmentSplitUuid;

    /**
     * 是否动态播放列表
     */
    @TableField("automatically_apply")
    private Boolean automaticallyApply;

    /**
     * 来源complex_id
     */
    @TableField("source_complex_id")
    private String sourceComplexId;

    /**
     * 来源device_id
     */
    @TableField("source_device_id")
    private String sourceDeviceId;

    /**
     * 除api占位符外所有都填充则为true
     */
    @TableField("filled")
    private Boolean filled;

    /**
     * 是否被TMS更改过
     */
    @TableField("changed")
    private Boolean changed;

    /**
     * 是否删除
     */
    @TableField("deleted")
    private Boolean deleted;

    /**
     * 创建时间
     */
    @TableField(value = "created", fill = FieldFill.INSERT)
    private Long created;

    /**
     * 修改时间
     */
    @TableField(value = "last_modified", fill = FieldFill.INSERT_UPDATE)
    private Long lastModified;

    /**
     * 校验信息
     */
    @TableField(value = "validation")
    private String validation;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
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

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getJson() {
        return json;
    }

    public void setJson(String json) {
        this.json = json;
    }

    public String getContentIds() {
        return contentIds;
    }

    public void setContentIds(String contentIds) {
        this.contentIds = contentIds;
    }

    public Boolean getTemplated() {
        return templated;
    }

    public void setTemplated(Boolean templated) {
        this.templated = templated;
    }

    public Integer getSource() {
        return source;
    }

    public void setSource(Integer source) {
        this.source = source;
    }

    public String getSourcePplId() {
        return sourcePplId;
    }

    public void setSourcePplId(String sourcePplId) {
        this.sourcePplId = sourcePplId;
    }

    public String getSourcePplVersionId() {
        return sourcePplVersionId;
    }

    public void setSourcePplVersionId(String sourcePplVersionId) {
        this.sourcePplVersionId = sourcePplVersionId;
    }

    public String getSourceSegmentSplitUuid() {
        return sourceSegmentSplitUuid;
    }

    public void setSourceSegmentSplitUuid(String sourceSegmentSplitUuid) {
        this.sourceSegmentSplitUuid = sourceSegmentSplitUuid;
    }

    public Boolean getAutomaticallyApply() {
        return automaticallyApply;
    }

    public void setAutomaticallyApply(Boolean automaticallyApply) {
        this.automaticallyApply = automaticallyApply;
    }

    public String getSourceComplexId() {
        return sourceComplexId;
    }

    public void setSourceComplexId(String sourceComplexId) {
        this.sourceComplexId = sourceComplexId;
    }

    public String getSourceDeviceId() {
        return sourceDeviceId;
    }

    public void setSourceDeviceId(String sourceDeviceId) {
        this.sourceDeviceId = sourceDeviceId;
    }

    public Boolean getFilled() {
        return filled;
    }

    public void setFilled(Boolean filled) {
        this.filled = filled;
    }

    public Boolean getChanged() {
        return changed;
    }

    public void setChanged(Boolean changed) {
        this.changed = changed;
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

    public String getValidation() {
        return validation;
    }

    public void setValidation(String validation) {
        this.validation = validation;
    }

    @Override
    public String toString() {
        return "TmsPlaylistDO{" +
                "id=" + id +
                ", organizationId=" + organizationId +
                ", playlistUuid=" + playlistUuid +
                ", title=" + title +
                ", json=" + json +
                ", contentIds=" + contentIds +
                ", templated=" + templated +
                ", source=" + source +
                ", sourcePplId=" + sourcePplId +
                ", sourcePplVersionId=" + sourcePplVersionId +
                ", sourceSegmentSplitUuid=" + sourceSegmentSplitUuid +
                ", automaticallyApply=" + automaticallyApply +
                ", sourceComplexId=" + sourceComplexId +
                ", sourceDeviceId=" + sourceDeviceId +
                ", filled=" + filled +
                ", changed=" + changed +
                ", deleted=" + deleted +
                ", created=" + created +
                ", lastModified=" + lastModified +
                ", validation=" + validation +
                "}";
    }
}
