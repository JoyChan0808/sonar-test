package com.aam.producer.playlist.repository.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.util.Objects;

/**
 * <p>
 *
 * </p>
 *
 * @author ${author}
 * @since 2019-04-29
 */
@TableName("playlist_sync_log")
public class PlaylistSyncLogDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;

    /**
     * 影院ID
     */
    @TableField("complex_id")
    private String complexId;

    /**
     * 设备主键UUID
     */
    @TableField("device_uuid")
    private String deviceUuid;

    /**
     * 播放列表UUID
     */
    @TableField("playlist_uuid")
    private String playlistUuid;

    /**
     * 标题
     */
    @TableField("title")
    private String title;

    /**
     * 线下数据hash值
     */
    @TableField("hash")
    private String hash;

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
     * 是否3d
     */
    @TableField("as_3d")
    private Boolean as3d;

    /**
     * 是否4k
     */
    @TableField("as_4k")
    private Boolean as4k;

    /**
     * 是否hfr
     */
    @TableField("as_hfr")
    private Boolean asHfr;

    /**
     * 操作，Delete/Create/Update
     */
    @TableField("method")
    private Integer method;

    /**
     * 状态
     */
    @TableField("status")
    private Integer status;

    /**
     * 同步信息，记录失败原因
     */
    @TableField("message")
    private String message;

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


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getComplexId() {
        return complexId;
    }

    public void setComplexId(String complexId) {
        this.complexId = complexId;
    }

    public String getDeviceUuid() {
        return deviceUuid;
    }

    public void setDeviceUuid(String deviceUuid) {
        this.deviceUuid = deviceUuid;
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

    public String getHash() {
        return hash;
    }

    public void setHash(String hash) {
        this.hash = hash;
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

    public Boolean getAs3d() {
        return as3d;
    }

    public void setAs3d(Boolean as3d) {
        this.as3d = as3d;
    }

    public Boolean getAs4k() {
        return as4k;
    }

    public void setAs4k(Boolean as4k) {
        this.as4k = as4k;
    }

    public Boolean getAsHfr() {
        return asHfr;
    }

    public void setAsHfr(Boolean asHfr) {
        this.asHfr = asHfr;
    }

    public Integer getMethod() {
        return method;
    }

    public void setMethod(Integer method) {
        this.method = method;
    }

    public Integer getStatus() {
        return status;
    }

    public void setStatus(Integer status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
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
        return "PlaylistSyncLogDO{" +
                "id=" + id +
                ", complexId=" + complexId +
                ", deviceUuid=" + deviceUuid +
                ", playlistUuid=" + playlistUuid +
                ", title=" + title +
                ", hash=" + hash +
                ", json=" + json +
                ", templated=" + templated +
                ", as3d=" + as3d +
                ", as4k=" + as4k +
                ", asHfr=" + asHfr +
                ", method=" + method +
                ", status=" + status +
                ", message=" + message +
                ", created=" + created +
                ", lastModified=" + lastModified +
                "}";
    }

    @Override
    public int hashCode() {
        return Objects.hash(playlistUuid, complexId);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }
        if (this == obj) {
            return true;
        }
        PlaylistSyncLogDO that = (PlaylistSyncLogDO) obj;
        return that.getPlaylistUuid().equals(playlistUuid) && that.getComplexId().equals(complexId);
    }
}
