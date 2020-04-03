package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class SPLPayload {
    @JSONField(name = "device_uuid")
    @JsonProperty("device_uuid")
    private String deviceUuid;
    @JSONField(name = "playlist_uuid")
    @JsonProperty("playlist_uuid")
    private String playlistUuid;
}
