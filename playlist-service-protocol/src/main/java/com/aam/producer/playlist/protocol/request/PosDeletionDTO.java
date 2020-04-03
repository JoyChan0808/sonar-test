package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * pos deletion
 *
 * @author oliver.lo
 * @since 2019-09-18 18:37
 */
@Getter
@Setter
@ToString
public class PosDeletionDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @JSONField(name = "pos_uuid")
    private String posUuid;
}
