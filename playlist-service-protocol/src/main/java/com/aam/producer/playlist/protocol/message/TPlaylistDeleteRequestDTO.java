package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * playlist.delete.request
 *
 * @author oliver.lo
 * @since 2019-11-01 10:48
 */
@Getter
@Setter
@ToString
public class TPlaylistDeleteRequestDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "receipt_uuid")
    private String receiptUuid;

    @JSONField(name = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "device_uuid")
    private String deviceUuid;

    @JSONField(name = "playlist_uuids")
    private List<String> tplUUIDs;
}
