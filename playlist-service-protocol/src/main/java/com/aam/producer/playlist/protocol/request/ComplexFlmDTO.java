package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * complex flm dto
 *
 * @author oliver.lo
 * @since 2019-12-30 16:37
 */
@Getter
@Setter
@ToString
public class ComplexFlmDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "complex_metadata")
    private ComplexMeta complexMeta;

    @JSONField(name = "complex_info")
    private ComplexInfo complexInfo;

    @Getter
    @Setter
    public static class ComplexMeta implements Serializable {

        private static final long serialVersionUID = -1L;

        @JSONField(name = "complex_uuid")
        private String complexUuid;
    }

    @Getter
    @Setter
    public static class ComplexInfo implements Serializable {

        private static final long serialVersionUID = -1L;

        @JSONField(name = "organization_uuid")
        private String organizationUuid;
    }
}
