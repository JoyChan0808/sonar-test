package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * This data just contain new or modified tpl list
 *
 * @author oliver.lo
 * @since 2019-09-23 18:14
 */
@Getter
@Setter
@ToString
public class PplContentsDataDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "organization_id")
    private String organizationId;

    @JSONField(name = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "ppl_uuid")
    private String pplUuid;

    @JSONField(name = "ppl_title")
    private String pplTitle;

    @JSONField(name = "tpl_contents_map")
    private Map<String, List<String>> tplContentsMap;
}
