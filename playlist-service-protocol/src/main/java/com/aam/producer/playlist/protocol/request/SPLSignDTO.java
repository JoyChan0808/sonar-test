package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class SPLSignDTO {
    @JSONField(name = "complex_uuid")
    @JsonProperty("complex_uuid")
    private String complexUuid;
    @JSONField(name = "spl_payloads")
    @JsonProperty("spl_payloads")
    private List<SPLPayload> splPayloads;
}
