package com.aam.producer.playlist.sal.response;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;

/**
 * aam lms info
 *
 * @author oliver.lo
 * @since 2019/6/10 2:45 PM
 */
public class LmsInfo implements Serializable {

    private static final long serialVersionUID = -8425154706696144122L;

    @JSONField(name = "lms_uuid")
    private String lmsUuid;

    public String getLmsUuid() {
        return lmsUuid;
    }

    public void setLmsUuid(String lmsUuid) {
        this.lmsUuid = lmsUuid;
    }
}
