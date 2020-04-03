package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * playlist delete schema
 *
 * @author oliver.lo
 * @since 2019/6/11 4:09 PM
 */
@Getter
@Setter
@ToString
public class TPlaylistDeletionDTO implements Serializable {

    private static final long serialVersionUID = -2021470622963649437L;

    @JSONField(name = "organization_id")
    @JsonProperty(value = "organization_id")
    private String organizationId;

    @JSONField(name = "playlist_uuids")
    @JsonProperty(value = "playlist_uuids")
    private List<String> playlistUUIDs;

    @JSONField(name = "complex_uuid")
    @JsonProperty(value = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "ppl_uuid")
    @JsonProperty(value = "ppl_uuid")
    private String pplUuid;
}
