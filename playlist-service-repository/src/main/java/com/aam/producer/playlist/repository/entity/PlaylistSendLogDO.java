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
@TableName("playlist_send_log")
public class PlaylistSendLogDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;

    /**
     * receipt uuid
     */
    @TableField("receipt_uuid")
    private String receiptUuid;

    /**
     * playlist action id
     */
    @TableField("action_id")
    private String actionId;

    /**
     * pos id
     */
    @TableField("pos_id")
    private String posId;

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
     * ppl uuid
     */
    @TableField("ppl_uuid")
    private String pplUuid;

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
     * 发送信息，记录失败原因
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

    public String getReceiptUuid() {
        return receiptUuid;
    }

    public void setReceiptUuid(String receiptUuid) {
        this.receiptUuid = receiptUuid;
    }

    public String getActionId() {
        return actionId;
    }

    public void setActionId(String actionId) {
        this.actionId = actionId;
    }

    public String getPosId() {
        return posId;
    }

    public void setPosId(String posId) {
        this.posId = posId;
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
        return "PlaylistSendLogDO{" +
                "id=" + id +
                ", receiptUuid=" + receiptUuid +
                ", actionId=" + actionId +
                ", posId=" + posId +
                ", complexId=" + complexId +
                ", deviceUuid=" + deviceUuid +
                ", playlistUuid=" + playlistUuid +
                ", title=" + title +
                ", json=" + json +
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
        PlaylistSendLogDO that = (PlaylistSendLogDO) obj;
        return that.getPlaylistUuid().equals(playlistUuid) && that.getComplexId().equals(complexId);
    }
}
