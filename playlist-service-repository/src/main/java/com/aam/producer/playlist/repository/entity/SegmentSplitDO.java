package com.aam.producer.playlist.repository.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.FieldStrategy;
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
@TableName("segment_split")
public class SegmentSplitDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @TableId(value = "uuid")
    private String uuid;

    /**
     * 标志
     */
    @TableId(value = "sign")
    private String sign;

    /**
     * parent UUID
     */
    @TableField(value = "parent_uuid", updateStrategy = FieldStrategy.IGNORED)
    private String parentUuid;

    /**
     * 组织ID
     */
    @TableField("organization_id")
    private String organizationId;

    /**
     * 分组标题
     */
    @TableField("split_title")
    private String splitTitle;

    /**
     * 分组类型
     */
    @TableField("split_type")
    private Integer splitType;

    /**
     * 分组规则
     */
    @TableField(value = "split_rule", updateStrategy = FieldStrategy.IGNORED)
    private String splitRule;

    /**
     * 状态
     */
    @TableField("status")
    private Integer status;

    /**
     * sort num
     */
    @TableField("sort_num")
    private Long sortNum;

    /**
     * 发布时间
     */
    @TableField(value = "publish_time", updateStrategy = FieldStrategy.IGNORED)
    private Long publishTime;

    /**
     * 有效期
     */
    @TableField("valid_time")
    private Long validTime;

    /**
     * 是否删除
     */
    @TableField("deleted")
    //@TableLogic
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
     * 默认组
     */
    @TableField(value = "default_group")
    private Boolean defaultGroup;

    /**
     * 系统自动分配组
     */
    @TableField(value = "auto_group")
    private Boolean autoGroup;

    /**
     * 用户自定义分配组
     */
    @TableField(value = "user_group")
    private Boolean userGroup;


    public String getSign() {
        return sign;
    }

    public void setSign(String sign) {
        this.sign = sign;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public String getParentUuid() {
        return parentUuid;
    }

    public void setParentUuid(String parentUuid) {
        this.parentUuid = parentUuid;
    }

    public String getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(String organizationId) {
        this.organizationId = organizationId;
    }

    public String getSplitTitle() {
        return splitTitle;
    }

    public void setSplitTitle(String splitTitle) {
        this.splitTitle = splitTitle;
    }

    public Integer getSplitType() {
        return splitType;
    }

    public void setSplitType(Integer splitType) {
        this.splitType = splitType;
    }

    public String getSplitRule() {
        return splitRule;
    }

    public void setSplitRule(String splitRule) {
        this.splitRule = splitRule;
    }

    public Integer getStatus() {
        return status;
    }

    public void setStatus(Integer status) {
        this.status = status;
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

    public Long getSortNum() {
        return sortNum;
    }

    public void setSortNum(Long sortNum) {
        this.sortNum = sortNum;
    }

    public Boolean getDefaultGroup() {
        return defaultGroup;
    }

    public void setDefaultGroup(Boolean defaultGroup) {
        this.defaultGroup = defaultGroup;
    }

    public Boolean getAutoGroup() {
        return autoGroup;
    }

    public void setAutoGroup(Boolean autoGroup) {
        this.autoGroup = autoGroup;
    }

    public Boolean getUserGroup() {
        return userGroup;
    }

    public void setUserGroup(Boolean userGroup) {
        this.userGroup = userGroup;
    }

    @Override
    public String toString() {
        return "SegmentSplitDO{" +
                "uuid=" + uuid +
                ", parentUuid=" + parentUuid +
                ", organizationId=" + organizationId +
                ", splitTitle=" + splitTitle +
                ", splitType=" + splitType +
                ", splitRule=" + splitRule +
                ", status=" + status +
                ", publishTime=" + publishTime +
                ", validTime=" + validTime +
                ", deleted=" + deleted +
                ", created=" + created +
                ", lastModified=" + lastModified +
                ", sortNum=" + sortNum +
                ", autoGroup=" + autoGroup +
                ", defaultGroup=" + defaultGroup +
                ", userGroup=" + userGroup +
                "}";
    }
}
