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
 * <p>
 * </p>
 *
 * @author ${author}
 * @since 2019-05-14
 */
@TableName("segment")
public class SegmentDO implements Serializable {

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
     * 标题
     */
    @TableField("title")
    private String title;

    /**
     * 类型
     */
    @TableField("type")
    private Integer type;

    /**
     * 目的
     */
    @TableField(value = "purpose", updateStrategy = FieldStrategy.IGNORED)
    private Integer purpose;

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

    @TableField("split_by_week")
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

    public Integer getType() {
        return type;
    }

    public void setType(Integer type) {
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
                ", deleted=" + deleted +
                ", created=" + created +
                ", lastModified=" + lastModified +
                ", splitByWeek=" + splitByWeek +
                "}";
    }
}
