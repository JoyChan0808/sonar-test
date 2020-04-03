package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class ProducerPlaylistChangedDTO {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "uuid")
    private String uuid;

    @JSONField(name = "title")
    private String title;

}
