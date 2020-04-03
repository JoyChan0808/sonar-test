package com.aam.producer.playlist.biz.model;

import com.alibaba.fastjson.annotation.JSONField;
import lombok.Getter;
import lombok.Setter;

/**
 * playlist event model
 *
 * @author oliver.lo
 * @since 2019-07-24 16:41
 */
@Getter
@Setter
public class TPlaylistEventModel {

    private String uuid;

    private String type;

    @JSONField(name = "content_kind")
    private String contentKind;

    private String text;

    private String title;

    private String part;
}
