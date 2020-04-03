package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * com.aam.producer.playlist.protocol.request
 *
 * @author oliver.lo
 * @since 2019-10-17 11:09
 */
@Getter
@Setter
@ToString
public class PosFetchDTO implements Serializable {

    private static final long serialVersionUID = -1;

    @JSONField(name = "complex_uuid")
    private String complexUuid;

    private Integer week;

    @JSONField(name = "uuids")
    private List<String> posUuidList;
}
