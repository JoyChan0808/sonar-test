package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;

/**
 * playlist sync request
 *
 * @author oliver.lo
 * @since 2019/4/10 3:28 PM
 */
public class TPlaylistSyncRequestDTO implements Serializable {

    private static final long serialVersionUID = -65029870091294062L;

    @JSONField(name = "complex_uuid")
    @JsonProperty(value = "complex_uuid")
    private String complexId;

    @JSONField(name = "device_uuid")
    @JsonProperty(value = "device_uuid")
    private String deviceId;

    @JSONField(name = "playlist_uuid")
    @JsonProperty(value = "playlist_uuid")
    private String playlistUuid;

    public String getComplexId() {
        return complexId;
    }

    public void setComplexId(String complexId) {
        this.complexId = complexId;
    }

    public String getDeviceId() {
        return deviceId;
    }

    public void setDeviceId(String deviceId) {
        this.deviceId = deviceId;
    }

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }
}
