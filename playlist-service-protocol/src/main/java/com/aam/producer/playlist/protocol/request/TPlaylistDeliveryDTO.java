package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * playlist sent response dto
 *
 * @author oliver.lo
 * @since 2019/5/7 2:11 PM
 */
@Getter
@Setter
@ToString
public class TPlaylistDeliveryDTO implements Serializable {

    private static final long serialVersionUID = 6305527132303601276L;

    /**
     * send receipt uuid
     */
    @JSONField(name = "receipt_uuid")
    private String receiptUuid;

    /**
     * complexUuid that where data sync from
     */
    @JSONField(name = "complex_uuid")
    private String complexUuid;

    /**
     * device uuid
     */
    @JSONField(name = "device_uuid")
    private String deviceUuid;

    /**
     * playlist uuid
     */
    @JSONField(name = "playlist_uuid")
    private String playlistUuid;

    /**
     * delivery status
     */
    @JSONField(name = "delivery_status")
    private Boolean deliveryStatus;

    /**
     * action uuid
     */
    @JSONField(name = "action_id")
    private String actionId;

    /**
     * delivery message
     */
    @JSONField(name = "message")
    private String message;
}
