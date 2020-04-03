package com.aam.producer.playlist.repository.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;

/**
 * <p>
 * <p>
 * </p>
 *
 * @author ${author}
 * @since 2019-05-14
 */
@TableName("playlist_segment_split_association")
public class PlaylistSegmentSplitAssociationDO implements Serializable {

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
     * ppl version UUID
     */
    @TableField("ppl_version_id")
    private String pplVersionId;

    /**
     * title主键UUID
     */
    @TableField(value = "title_id", updateStrategy = FieldStrategy.IGNORED)
    private String titleId;

    /**
     * ppl version segment关系主键ID
     */
    @TableField("segment_association_uuid")
    private String segmentAssociationUuid;

    /**
     * segment split主键ID
     */
    @TableField("segment_split_uuid")
    private String segmentSplitUuid;

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


    public String getSegmentAssociationUuid() {
        return segmentAssociationUuid;
    }

    public void setSegmentAssociationUuid(String segmentAssociationUuid) {
        this.segmentAssociationUuid = segmentAssociationUuid;
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

    public String getTitleId() {
        return titleId;
    }

    public void setTitleId(String titleId) {
        this.titleId = titleId;
    }


    public String getSegmentSplitUuid() {
        return segmentSplitUuid;
    }

    public void setSegmentSplitUuid(String segmentSplitUuid) {
        this.segmentSplitUuid = segmentSplitUuid;
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
        return "PlaylistSegmentSplitAssociationDO{" +
                "id=" + id +
                ", organizationId=" + organizationId +
                ", playlistUuid=" + playlistUuid +
                ", pplVersionId=" + pplVersionId +
                ", titleId=" + titleId +
                ", segmentAssociationUuid=" + segmentAssociationUuid +
                ", segmentSplitUuid=" + segmentSplitUuid +
                ", created=" + created +
                ", lastModified=" + lastModified +
                "}";
    }
}
