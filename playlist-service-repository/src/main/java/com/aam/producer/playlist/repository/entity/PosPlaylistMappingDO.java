package com.aam.producer.playlist.repository.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.FieldStrategy;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * <p>
 *
 * </p>
 *
 * @author ${author}
 * @since 2019-09-17
 */
@Getter
@Setter
@ToString
@TableName("pos_playlist_mapping")
public class PosPlaylistMappingDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * primary id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * organization id
     */
    @TableField("organization_id")
    private String organizationId;

    /**
     * pos uuid
     */
    @TableField("pos_uuid")
    private String posUuid;

    /**
     * pos title
     */
    @TableField("pos_title")
    private String posTitle;

    /**
     * pos start
     */
    @TableField("pos_start")
    private Long posStart;

    /**
     * pos end
     */
    @TableField("pos_end")
    private Long posEnd;

    /**
     * complex uuid
     */
    @TableField("complex_uuid")
    private String complexUuid;

    /**
     * screen uuid
     */
    @TableField("screen_uuid")
    private String screenUuid;

    /**
     * title uuid
     */
    @TableField(value = "title_uuid", updateStrategy = FieldStrategy.IGNORED)
    private String titleUuid;

    /**
     * unmatched_show_attributes
     */
    @TableField(value = "unmatched_show_attributes", updateStrategy = FieldStrategy.IGNORED)
    private String unmatchedShowAttributes;

    /**
     * show attributes json
     */
    @TableField(value = "show_attributes", updateStrategy = FieldStrategy.IGNORED)
    private String showAttributes;

    /**
     * show attributes short code
     */
    @TableField(value = "show_attributes_code", updateStrategy = FieldStrategy.IGNORED)
    private Long showAttributesCode;

    /**
     * language
     */
    @TableField(value = "language", updateStrategy = FieldStrategy.IGNORED)
    private String language;

    /**
     * ppl uuid
     */
    @TableField(value = "ppl_uuid", updateStrategy = FieldStrategy.IGNORED)
    private String pplUuid;

    /**
     * ppl 0-other 1-automatic
     */
    @TableField(value = "ppl_automatic", updateStrategy = FieldStrategy.IGNORED)
    private Boolean pplAutomatic;

    /**
     * tpl uuid
     */
    @TableField(value = "tpl_uuid", updateStrategy = FieldStrategy.IGNORED)
    private String tplUuid;

    /**
     * pos state
     */
    @TableField("state")
    private String state;

    /**
     * assign in 1-Producer/2-TMS
     */
    @TableField(value = "assign_system", updateStrategy = FieldStrategy.IGNORED)
    private Integer assignSystem;

    /**
     * mapping complete 0-false 1-true
     */
    @TableField(value = "mapping_completed", updateStrategy = FieldStrategy.IGNORED)
    private Boolean mappingCompleted;

    /**
     * mapping status
     */
    @TableField(value = "mapping_status", updateStrategy = FieldStrategy.IGNORED)
    private String mappingStatus;

    /**
     * mapping message
     */
    @TableField(value = "mapping_message", updateStrategy = FieldStrategy.IGNORED)
    private String mappingMessage;

    /**
     * deleted 0-false 1-true
     */
    @TableField("deleted")
    private Boolean deleted;

    /**
     * attempted at tms time
     */
    @TableField(value = "attempted", updateStrategy = FieldStrategy.IGNORED)
    private Long attempted;

    /**
     * create time
     */
    @TableField(value = "created", fill = FieldFill.INSERT)
    private Long created;

    /**
     * modified time
     */
    @TableField(value = "last_modified", fill = FieldFill.INSERT_UPDATE)
    private Long lastModified;
}
