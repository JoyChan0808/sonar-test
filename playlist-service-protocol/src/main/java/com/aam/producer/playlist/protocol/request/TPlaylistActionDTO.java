package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * playlist async action response dto
 *
 * @author oliver.lo
 * @since 2019/5/28 11:31 AM
 */
@Getter
@Setter
@ToString
public class TPlaylistActionDTO implements Serializable {

    private static final long serialVersionUID = 7106587111653065829L;

    @JSONField(name = "action_id")
    private String actionId;

    @JSONField(name = "success")
    private Boolean success;

    @JSONField(name = "message")
    private String message;
}
