package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * manual mapping dto
 *
 * @author oliver.lo
 * @since 2019-09-20 11:29
 */
@Getter
@Setter
@ToString
public class PosMappingDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "ppl_uuid")
    @JsonProperty("ppl_uuid")
    private String pplUuid;

    @JSONField(name = "automatic")
    @JsonProperty("automatic")
    private Boolean pplAutomatic;

    @JSONField(name = "pos_uuids")
    @JsonProperty("pos_uuids")
    private List<String> posUuidList;

    private String state;

    @JSONField(name = "mapping_in_system")
    @JsonProperty("mapping_in_system")
    private String mappingInSystem;
}
