package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * automation-change.data dto
 *
 * @author oliver.lo
 * @since 2019-12-03 16:53
 */
@Getter
@Setter
@ToString
public class AutomationChangeDataDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "organization_id")
    private String organizationId;

    private String uuid;

    private String name;
}
