package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

/**
 * cpl meta dto
 *
 * @author oliver.lo
 * @since 2019-08-12 10:12
 */
@Getter
@Setter
public class CplMetaDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "cpl_uuid")
    private String cplUUID;

    @JSONField(name = "organization_id")
    private String organizationId;

    @JSONField(name = "credit_offset")
    private Long creditOffset;

    @JSONField(name = "intermission")
    private Long intermission;

    @JSONField(name = "hardlock")
    private Boolean hardLock;

    @JSONField(name = "ratings")
    private List<Rating> ratings;

    @Getter
    @Setter
    private static class Rating implements Serializable {

        private static final long serialVersionUID = -1L;

        @JSONField(name = "territory")
        private String territory;

        @JSONField(name = "rating")
        private String rating;
    }
}
