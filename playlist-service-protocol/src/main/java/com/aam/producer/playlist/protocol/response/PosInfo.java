package com.aam.producer.playlist.protocol.response;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class PosInfo {
    private String pplUuid;
    private String posUuid;
}
