package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;

/**
 * send playlist request schema
 *
 * @author oliver.lo
 * @since 2019/5/28 11:46 AM
 */
public class TPlaylistSendRequestDTO implements Serializable {

    private static final long serialVersionUID = 6905267538606402468L;

    /**
     * send receipt uuid
     */
    @JSONField(name = "receipt_uuid")
    @JsonProperty(value = "receipt_uuid")
    private String receiptUuid;

    /**
     * complexUuid that where data sync from
     */
    @JSONField(name = "complex_uuid")
    @JsonProperty(value = "complex_uuid")
    private String complexUuid;

    /**
     * device uuid
     */
    @JSONField(name = "device_uuid")
    @JsonProperty(value = "device_uuid")
    private String deviceUuid;

    /**
     * playlist info
     */
    @JSONField(name = "playlist")
    @JsonProperty(value = "playlist")
    private TPlaylistDTO playlist;

    public String getReceiptUuid() {
        return receiptUuid;
    }

    public void setReceiptUuid(String receiptUuid) {
        this.receiptUuid = receiptUuid;
    }

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

    public TPlaylistDTO getPlaylist() {
        return playlist;
    }

    public void setPlaylist(TPlaylistDTO playlist) {
        this.playlist = playlist;
    }
}
