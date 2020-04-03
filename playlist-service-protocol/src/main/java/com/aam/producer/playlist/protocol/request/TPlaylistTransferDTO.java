package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;

/**
 * playlist hash sync request
 *
 * @author oliver.lo
 * @since 2019/7/2 9:52 AM
 */
public class TPlaylistTransferDTO implements Serializable {

    private static final long serialVersionUID = 43071620665271977L;

    @JSONField(name = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "device_uuid")
    private String deviceUuid;

    @JSONField(name = "playlist_uuid")
    private String playlistUuid;

    @JSONField(name = "pos_id")
    private String posId;

    /**
     * if fast_track = true,every time need to transfer(sync/send)
     */
    @JSONField(name = "fast_track")
    private Boolean fastTrack;

    public String getComplexUuid() {
        return complexUuid;
    }

    public void setComplexUuid(String complexUuid) {
        this.complexUuid = complexUuid;
    }

    public String getDeviceUuid() {
        return deviceUuid;
    }

    public void setDeviceUuid(String deviceUuid) {
        this.deviceUuid = deviceUuid;
    }

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }

    public String getPosId() {
        return posId;
    }

    public void setPosId(String posId) {
        this.posId = posId;
    }

    public Boolean getFastTrack() {
        return fastTrack;
    }

    public void setFastTrack(Boolean fastTrack) {
        this.fastTrack = fastTrack;
    }
}
