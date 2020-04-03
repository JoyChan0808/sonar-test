package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * com.aam.producer.playlist.protocol.message
 *
 * @author oliver.lo
 * @since 2019-10-23 15:03
 */
@Getter
@Setter
@ToString
public class PosMappingBatchRequestDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "receipt_uuid")
    private String receiptUuid;

    @JSONField(name = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "pos_update_map")
    private List<PosMappingRequestDTO> mappings;
}
