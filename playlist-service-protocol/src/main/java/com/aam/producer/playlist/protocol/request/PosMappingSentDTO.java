package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * pos mapping response
 *
 * @author oliver.lo
 * @since 2019-09-18 14:26
 */
@Getter
@Setter
@ToString
public class PosMappingSentDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "receipt_uuid")
    private String receiptUuid;

    @JSONField(name = "mapping_status")
    private String mappingStatus;

    @JSONField(name = "resp_message")
    private String respMessage;

    @JSONField(name = "attempted_at")
    private String attemptedAt;
}
