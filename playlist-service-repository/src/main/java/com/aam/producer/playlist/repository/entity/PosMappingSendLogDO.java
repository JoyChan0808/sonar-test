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
 * @since 2019-10-23
 */
@Getter
@Setter
@ToString
@TableName("pos_mapping_send_log")
public class PosMappingSendLogDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * primary id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * send receipt uuid
     */
    @TableField("receipt_uuid")
    private String receiptUuid;

    /**
     * complex uuid
     */
    @TableField("complex_id")
    private String complexId;

    /**
     * pos mapping json string
     */
    @TableField("mapping")
    private String mapping;

    /**
     * send status
     */
    @TableField("status")
    private String status;

    /**
     * send message
     */
    @TableField("message")
    private String message;

    /**
     * attempted at tms
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
