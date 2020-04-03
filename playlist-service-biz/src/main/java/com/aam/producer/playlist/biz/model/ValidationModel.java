package com.aam.producer.playlist.biz.model;

import com.aam.producer.playlist.biz.model.CompositionModel.Rating;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO.IssueDetail;
import com.aam.producer.task.protocol.enums.IssueLevelEnum;
import com.alibaba.fastjson.annotation.JSONField;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;

/**
 * playlist validation model
 *
 * @author oliver.lo
 * @since 2019-08-08 17:53
 */
@Getter
@Setter
public class ValidationModel {

    /**
     * playlist is illegal
     */
    @JSONField(name = "playlist_issue")
    private Boolean playlistIssue = false;

    /**
     * CPL is not on the server
     */
    @JSONField(name = "content_issue")
    private Boolean contentIssue = false;

    /**
     * Valid KDMs were not found
     */
    @JSONField(name = "kdm_issue")
    private Boolean kdmIssue = false;

    /**
     * feature is not assigned
     */
    @JSONField(name = "feature_issue")
    private Boolean featureIssue = false;

    /**
     * segment is empty
     */
    @JSONField(name = "segment_issue")
    private Boolean segmentIssue = false;

    /**
     * offset time is zero
     */
    @JSONField(name = "automation_issue")
    private Boolean automationIssue = false;

    /**
     * feature unrated
     */
    @JSONField(name = "rating_issue")
    private Boolean ratingIssue = false;

    /**
     * rating card missed
     */
    @JSONField(name = "rating_card_issue")
    private Boolean ratingCardIssue = false;

    /**
     * issue detail
     */
    @JSONField(name = "detailed_issues")
    private Map<String, IssueDetail> issueDetails = new HashMap<>();
    @JSONField(serialize = false, deserialize = false)
    private List<Rating> ratingCardRatings;

    @JSONField(name = "issue_level")
    public String getIssueLevel() {
        if (featureIssue) {
            return IssueLevelEnum.SHOW_STOPPER.getIssueLevel();
        } else if (segmentIssue || automationIssue || ratingIssue || ratingCardIssue) {
            return IssueLevelEnum.DISRUPTOR.getIssueLevel();
        } else {
            return null;
        }
    }
}
