package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * playlist delete response
 *
 * @author oliver.lo
 * @since 2019-11-01 10:59
 */
@Getter
@Setter
@ToString
public class TPlaylistDeletedDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "receipt_uuid")
    private String receiptUuid;

    private Boolean success;

    private String message;

    @JSONField(name = "resp_data")
    private String respData;

    @JSONField(name = "attempted_at")
    private String attemptedAt;
}
