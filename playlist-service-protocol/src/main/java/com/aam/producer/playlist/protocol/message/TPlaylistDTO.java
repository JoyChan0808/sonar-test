package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;
import java.util.List;
import lombok.Data;

/**
 * playlist schema
 *
 * @author oliver.lo
 * @since 2019/5/28 11:49 AM
 */
@Data
public class TPlaylistDTO implements Serializable {

    private static final long serialVersionUID = 7087385457427934068L;

    /**
     * playlist uuid
     */
    @JSONField(name = "uuid")
    @JsonProperty(value = "uuid")
    private String uuid;

    /**
     * playlist title
     */
    @JSONField(name = "title")
    @JsonProperty(value = "title")
    private String title;

    /**
     * content ids
     */
    @JSONField(name = "content_ids")
    @JsonProperty(value = "content_ids")
    private List<String> contentIds;

    /**
     * playlist content list
     */
    @JSONField(name = "events")
    @JsonProperty(value = "events")
    private Object events;

    /**
     * is 3d
     */
    @JSONField(name = "is_3d")
    @JsonProperty(value = "is_3d")
    private Boolean as3d;

    /**
     * is 4k
     */
    @JSONField(name = "is_4k")
    @JsonProperty(value = "is_4k")
    private Boolean as4k;

    /**
     * is hfr
     */
    @JSONField(name = "is_hfr")
    @JsonProperty(value = "is_hfr")
    private Boolean asHfr;
}
