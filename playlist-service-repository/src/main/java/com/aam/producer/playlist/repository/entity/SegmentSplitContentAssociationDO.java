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
 *
 * </p>
 *
 * @author ${author}
 * @since 2019-05-14
 */
@TableName("segment_split_content_association")
public class SegmentSplitContentAssociationDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;


    /**
     * title
     */
    @TableField("title")
    private String title;

    /**
     * 组织ID
     */
    @TableField("organization_id")
    private String organizationId;

    /**
     * segment split主键ID
     */
    @TableField("segment_split_uuid")
    private String segmentSplitUuid;

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
    @TableField(value = "extension", updateStrategy = FieldStrategy.IGNORED)
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

    public String getSegmentSplitUuid() {
        return segmentSplitUuid;
    }

    public void setSegmentSplitUuid(String segmentSplitUuid) {
        this.segmentSplitUuid = segmentSplitUuid;
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
        return "SegmentSplitContentAssociationDO{" +
                "id=" + id +
                ", organizationId=" + organizationId +
                ", segmentSplitUuid=" + segmentSplitUuid +
                ", contentId=" + contentId +
                ", contentType=" + contentType +
                ", sortNumber=" + sortNumber +
                ", extension=" + extension +
                ", created=" + created +
                ", lastModified=" + lastModified +
                "}";
    }
}
