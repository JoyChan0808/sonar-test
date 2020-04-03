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
public class OrganizationDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "org_uuid",alternateNames={"uuid","org_uuid"})
    private String uuid;

    @JSONField(name = "name")
    private String name;

}
