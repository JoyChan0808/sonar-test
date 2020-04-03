package com.aam.producer.playlist.repository.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;

/**
 * <p>
 *
 * </p>
 *
 * @author ${author}
 * @since 2019-05-14
 */
@TableName("playlist_version")
public class PlaylistVersionDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @TableId(value = "uuid")
    private String uuid;

    /**
     * 组织ID
     */
    @TableField("organization_id")
    private String organizationId;

    /**
     * playlist主键UUID
     */
    @TableField("playlist_uuid")
    private String playlistUuid;

    /**
     * 是否模板
     */
    @TableField("templated")
    private Boolean templated;

    /**
     * 发布时间
     */
    @TableField(value = "publish_time", updateStrategy = FieldStrategy.IGNORED)
    private Long publishTime;

    /**
     * 延迟发布
     */
    @TableField(value = "publish_later", updateStrategy = FieldStrategy.IGNORED)
    private Boolean publishLater;

    /**
     * 有效时间
     */
    @TableField(value = "valid_time", updateStrategy = FieldStrategy.IGNORED)
    private Long validTime;

    /**
     * 时区
     */
    @TableField(value = "time_zone", updateStrategy = FieldStrategy.IGNORED)
    private Integer timeZone;

    /**
     * 状态
     */
    @TableField("status")
    private Integer status;

    /**
     * 扩展信息
     */
    @TableField(value = "extension", updateStrategy = FieldStrategy.IGNORED)
    private String extension;

    /**
     * 是否删除
     */
    @TableField("deleted")
    @TableLogic
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


    public Integer getTimeZone() {
        return timeZone;
    }

    public void setTimeZone(Integer timeZone) {
        this.timeZone = timeZone;
    }

    public Boolean getPublishLater() {
        return publishLater;
    }

    public void setPublishLater(Boolean publishLater) {
        this.publishLater = publishLater;
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

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }

    public Boolean getTemplated() {
        return templated;
    }

    public void setTemplated(Boolean templated) {
        this.templated = templated;
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

    public Integer getStatus() {
        return status;
    }

    public void setStatus(Integer status) {
        this.status = status;
    }

    public String getExtension() {
        return extension;
    }

    public void setExtension(String extension) {
        this.extension = extension;
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

    @Override
    public String toString() {
        return "PlaylistVersionDO{" +
                "uuid=" + uuid +
                ", organizationId=" + organizationId +
                ", playlistUuid=" + playlistUuid +
                ", templated=" + templated +
                ", publishTime=" + publishTime +
                ", validTime=" + validTime +
                ", status=" + status +
                ", extension=" + extension +
                ", deleted=" + deleted +
                ", created=" + created +
                ", lastModified=" + lastModified +
                "}";
    }
}
