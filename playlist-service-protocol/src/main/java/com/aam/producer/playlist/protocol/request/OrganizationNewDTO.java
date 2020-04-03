package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * organization new response
 *
 * @author oliver.lo
 * @since 2019-09-18 14:26
 */
@Getter
@Setter
@ToString
public class OrganizationNewDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "uuid")
    private String uuid;

    @JSONField(name = "name")
    private String name;

}
