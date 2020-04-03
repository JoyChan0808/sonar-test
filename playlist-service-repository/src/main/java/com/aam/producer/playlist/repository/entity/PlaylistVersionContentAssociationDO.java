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
 * @since 2019-05-14
 */
@TableName("playlist_version_content_association")
public class PlaylistVersionContentAssociationDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    @TableField("title")
    private String title;

    /**
     * UUID
     */
    @TableField("uuid")
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
     * ppl version UUID
     */
    @TableField("ppl_version_id")
    private String pplVersionId;

    /**
     * content UUID
     */
    @TableField("content_id")
    private String contentId;

    /**
     * content 类型
     */
    @TableField("content_type")
    private Integer contentType;

    /**
     * 顺序号
     */
    @TableField("sort_number")
    private Integer sortNumber;

    /**
     * 扩展信息
     */
    @TableField("extension")
    private String extension;

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


    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
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

    public String getPplVersionId() {
        return pplVersionId;
    }

    public void setPplVersionId(String pplVersionId) {
        this.pplVersionId = pplVersionId;
    }

    public String getContentId() {
        return contentId;
    }

    public void setContentId(String contentId) {
        this.contentId = contentId;
    }

    public Integer getContentType() {
        return contentType;
    }

    public void setContentType(Integer contentType) {
        this.contentType = contentType;
    }

    public Integer getSortNumber() {
        return sortNumber;
    }

    public void setSortNumber(Integer sortNumber) {
        this.sortNumber = sortNumber;
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

    @Override
    public String toString() {
        return "PlaylistVersionContentAssociationDO{" +
                "id=" + id +
                ", organizationId=" + organizationId +
                ", playlistUuid=" + playlistUuid +
                ", pplVersionId=" + pplVersionId +
                ", contentId=" + contentId +
                ", contentType=" + contentType +
                ", sortNumber=" + sortNumber +
                ", extension=" + extension +
                ", created=" + created +
                ", lastModified=" + lastModified +
                "}";
    }
}
