package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;
import java.util.List;

/**
 * com.aam.producer.playlist.biz.schema
 *
 * @author oliver.lo
 * @since 2019/6/17 10:58 AM
 */
public class TPlaylistHashSyncRequestDTO implements Serializable {

    private static final long serialVersionUID = -3990434187115935960L;

    @JSONField(name = "complex_uuid")
    @JsonProperty(value = "complex_uuid")
    private String complexId;

    @JSONField(name = "device_uuids")
    @JsonProperty(value = "device_uuids")
    private List<String> deviceIds;

    @JSONField(name = "playlist_uuids")
    @JsonProperty(value = "playlist_uuids")
    private List<String> playlistIds;

    public String getComplexId() {
        return complexId;
    }

    public void setComplexId(String complexId) {
        this.complexId = complexId;
    }

    public List<String> getDeviceIds() {
        return deviceIds;
    }

    public void setDeviceIds(List<String> deviceIds) {
        this.deviceIds = deviceIds;
    }

    public List<String> getPlaylistIds() {
        return playlistIds;
    }

    public void setPlaylistIds(List<String> playlistIds) {
        this.playlistIds = playlistIds;
    }
}
