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
 * @since 2019-11-01
 */
@Getter
@Setter
@ToString
@TableName("playlist_delete_log")
public class PlaylistDeleteLogDO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * primary id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;

    /**
     * receipt uuid
     */
    @TableField("receipt_uuid")
    private String receiptUuid;

    /**
     * complex uuid
     */
    @TableField("complex_id")
    private String complexId;

    /**
     * device uuid
     */
    @TableField(value = "device_uuid", updateStrategy = FieldStrategy.IGNORED)
    private String deviceUuid;

    /**
     * tpl uuids
     */
    @TableField("tpl_uuids")
    private String tplUuids;

    /**
     * delete in 1-Producer 2-TMS
     */
    @TableField("delete_system")
    private Integer deleteSystem;

    /**
     * task status
     */
    @TableField("status")
    private String status;

    /**
     * task message
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
