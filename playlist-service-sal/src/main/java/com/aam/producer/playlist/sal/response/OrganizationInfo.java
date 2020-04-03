package com.aam.producer.playlist.sal.response;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;

/**
 * organization info
 *
 * @author oliver.lo
 * @since 2019/6/13 9:53 AM
 */
public class OrganizationInfo implements Serializable {

    private static final long serialVersionUID = 8974038003631707042L;

    @JSONField(name = "organization_uuid")
    private String organizationUuid;

    public String getOrganizationUuid() {
        return organizationUuid;
    }

    public void setOrganizationUuid(String organizationUuid) {
        this.organizationUuid = organizationUuid;
    }
}
