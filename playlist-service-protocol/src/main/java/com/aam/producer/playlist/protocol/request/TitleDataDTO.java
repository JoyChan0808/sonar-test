package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.List;

/**
 * title data
 *
 * @author oliver.lo
 * @since 2019-07-29 19:50
 */
public class TitleDataDTO implements Serializable {

    private static final long serialVersionUID = -1;

    @JSONField(name = "uuid")
    private String titleUuid;

    @JSONField(name = "cpls")
    private List<String> cplIds;

    @JSONField(name = "deleted")
    private Boolean deleted;

    public String getTitleUuid() {
        return titleUuid;
    }

    public void setTitleUuid(String titleUuid) {
        this.titleUuid = titleUuid;
    }

    public List<String> getCplIds() {
        return cplIds;
    }

    public void setCplIds(List<String> cplIds) {
        this.cplIds = cplIds;
    }

    public Boolean getDeleted() {
        return deleted;
    }

    public void setDeleted(Boolean deleted) {
        this.deleted = deleted;
    }
}
