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
@TableName("playlist_show_attribute_combination")
public class PlaylistShowAttributeCombinationDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

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
     * 组名称
     */
    @TableField("name")
    private String name;

    /**
     * 简码组合
     */
    @TableField("short_code_association")
    private Long shortCodeAssociation;

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

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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

    public Long getShortCodeAssociation() {
        return shortCodeAssociation;
    }

    public void setShortCodeAssociation(Long shortCodeAssociation) {
        this.shortCodeAssociation = shortCodeAssociation;
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
        return "PlaylistShowAttributeCombinationDO{" +
                "id=" + id +
                ", organizationId=" + organizationId +
                ", playlistUuid=" + playlistUuid +
                ", shortCodeAssociation=" + shortCodeAssociation +
                ", created=" + created +
                ", lastModified=" + lastModified +
                "}";
    }
}
