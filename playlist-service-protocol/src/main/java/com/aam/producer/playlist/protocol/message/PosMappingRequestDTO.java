package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * pos mapping dto
 *
 * @author oliver.lo
 * @since 2019-09-18 14:13
 */
@Getter
@Setter
@ToString
public class PosMappingRequestDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "pos_uuid")
    private String posUuid;

    @JSONField(name = "playlist_uuid")
    private String tplUuid;

    @JSONField(name = "placeholder_type")
    private String placeholderType;

    @JSONField(name = "mapping_state")
    private String mappingState;
}
