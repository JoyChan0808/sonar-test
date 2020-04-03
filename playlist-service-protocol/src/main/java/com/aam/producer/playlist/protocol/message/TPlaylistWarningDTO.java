package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * playlist warning message
 *
 * @author oliver.lo
 * @since 2019-08-05 10:32
 */
@Data
public class TPlaylistWarningDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @JSONField(name = "organization_id")
    @JsonProperty(value = "organization_id")
    private String organizationId;

    @JSONField(name = "complex_uuid")
    @JsonProperty(value = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "task_type")
    @JsonProperty(value = "task_type")
    private String taskType;

    @JSONField(name = "payload")
    @JsonProperty(value = "payload")
    private TPlaylistWarningDetail payload;

    @JSONField(name = "payload_hash")
    @JsonProperty(value = "payload_hash")
    private String payloadHash;

    @JSONField(name = "reported_at")
    @JsonProperty(value = "reported_at")
    private Long reported;

    @EqualsAndHashCode(callSuper = true)
    @Data
    public static class TPlaylistWarningDetail extends TPlaylistDTO {

        private static final long serialVersionUID = -1L;

        @JSONField(name = "ppl_uuid")
        @JsonProperty(value = "ppl_uuid")
        private String pplUuid;

        @JSONField(name = "ppl_name")
        @JsonProperty(value = "ppl_name")
        private String pplTitle;

        @JSONField(name = "issue_details")
        @JsonProperty(value = "issue_details")
        private List<IssueDetail> issueDetails;
    }

    @Data
    public static class IssueDetail implements Serializable {

        private static final long serialVersionUID = -1L;

        @JSONField(name = "type")
        @JsonProperty(value = "type")
        private String type;

        @JSONField(name = "name")
        @JsonProperty(value = "name")
        private String name;

        @JSONField(name = "issue")
        @JsonProperty(value = "issue")
        private String issue;

        @JSONField(name = "content_id")
        @JsonProperty(value = "content_id")
        private String contentId;

        @JSONField(name = "level")
        @JsonProperty(value = "level")
        private String level;

        @JSONField(name = "sort_number")
        @JsonProperty(value = "sort_number")
        private Integer sortNumber;

        @JSONField(name = "redirect_id")
        @JsonProperty(value = "redirect_id")
        private String redirectId;

        @JSONField(name = "extension")
        @JsonProperty(value = "extension")
        private Map<String, Object> extension;
    }
}
