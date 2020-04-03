package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.Map;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * playlist hash sync dto
 *
 * @author oliver.lo
 * @since 2019/4/8 1:59 PM
 */
@Getter
@Setter
@ToString
public class TPlaylistHashSyncedDTO implements Serializable {

    private static final long serialVersionUID = -391712771776137188L;

    /**
     * complexUuid that where data sync from
     */
    @NotBlank(message = "{complex_uuid.notBlank}")
    @JSONField(name = "complex_uuid")
    private String complexUuid;

    /**
     * map key is device_uuid
     */
    @NotNull(message = "{playlist.notNull}")
    @JSONField(name = "playlists")
    private Map<String, Map<String, String>> playlistHashDeviceMap;

    private Boolean success;

    private String message;
}
