package com.aam.producer.playlist.protocol.response;

import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO.IssueDetail;
import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

/**
 * playlist info
 *
 * @author oliver.lo
 * @since 2019/5/30 6:09 PM
 */
@Getter
@Setter
public class TmsPlaylistInfo implements Serializable {

    private static final long serialVersionUID = 6523791493811559855L;

    /**
     * playlist uuid
     */
    @JSONField(name = "uuid")
    private String uuid;

    /**
     * ppl uuid
     */
    @JSONField(name = "ppl_uuid")
    private String pplUuid;

    /**
     * complexUuid that where data sync from
     */
    @JSONField(name = "complex_uuid")
    private String complexUuid;

    /**
     * playlist title
     */
    @JSONField(name = "title")
    private String title;

    /**
     * content ids
     */
    @JSONField(name = "content_ids")
    private List<String> contentIds;

    /**
     * playlist content list
     */
    @JSONField(name = "events")
    private Object events;

    /**
     * 是否3d
     */
    @JSONField(name = "is_3d")
    private Boolean as3d;

    /**
     * 是否4k
     */
    @JSONField(name = "is_4k")
    private Boolean as4k;

    /**
     * 是否hfr
     */
    @JSONField(name = "is_hfr")
    private Boolean asHfr;

    /**
     * issues
     */
    @JSONField(name = "issue_details")
    private List<IssueDetail> issueDetails;
}
