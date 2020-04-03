package com.aam.producer.playlist.biz.convert;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.enums.AutomationEnum;
import com.aam.producer.playlist.biz.enums.ContentKindEnum;
import com.aam.producer.playlist.biz.model.CompositionModel;
import com.aam.producer.playlist.biz.model.CompositionModel.Rating;
import com.aam.producer.playlist.biz.model.CplCompareModel;
import com.aam.producer.playlist.biz.model.CplCompareModel.CompareInfo;
import com.aam.producer.playlist.biz.model.SegmentModel;
import com.aam.producer.playlist.biz.model.ValidationModel;
import com.aam.producer.playlist.common.PlaylistConst;
import com.aam.producer.playlist.common.utils.InitUtils;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO.IssueDetail;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.aam.producer.task.protocol.enums.TaskTypeEnum;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

/**
 * com.aam.producer.playlist.biz.convert
 *
 * @author oliver.lo
 * @since 2019-11-19 14:36
 */
@Mapper
public interface TPlaylistWarningConvert {

    TPlaylistWarningConvert mapper = Mappers.getMapper(TPlaylistWarningConvert.class);

    default ValidationModel verifyTpl(String pplUuid, TmsPlaylistDO tpl) {
        String playlistUuid = tpl.getPlaylistUuid();
        CplCompareModel cplCompareInfo = new CplCompareModel();
        Map<String, List<String>> ratingClip = InitUtils.getRatingClip();
        ValidationModel validation = new ValidationModel();
        JSONObject json = JSON.parseObject(tpl.getJson());
        JSONArray events = json.getJSONArray(PlaylistConst.PLAYLIST_EVENTS_KEY);
        for (int i = 0, count = events.size(); i < count; i++) {
            JSONObject event = events.getJSONObject(i);
            String type = event.getString(PlaylistConst.PLAYLIST_EVENT_TYPE_KEY);
            if (ContentTypeEnum.SEGMENT.getName().equals(type)) {
                SegmentModel segment = JSON.parseObject(event.toJSONString(), SegmentModel.class);
                JSONArray segmentEvents = event.getJSONArray(PlaylistConst.PLAYLIST_EVENTS_KEY);
                if (CollectionUtils.isNotEmpty(segmentEvents)) {
                    for (int j = 0, len = segmentEvents.size(); j < len; j++) {
                        JSONObject segmentEvent = segmentEvents.getJSONObject(j);
                        if (Objects.equals("composition", segmentEvent.getString("type"))) {
                            CompositionModel segmentCpl = JSON
                                    .parseObject(segmentEvent.toJSONString(),
                                            CompositionModel.class);
                            verifyCpl(validation, segmentCpl, i, pplUuid, playlistUuid);
                            compareCpl(validation, cplCompareInfo, ratingClip, segmentCpl, segment,
                                    i, pplUuid);
                        }
                    }
                } else {
                    segmentEmptyWarning(validation, segment, i);
                }
            } else {
                if (Objects.equals("composition", event.getString("type"))) {
                    CompositionModel cpl = JSON
                            .parseObject(event.toJSONString(), CompositionModel.class);
                    verifyCpl(validation, cpl, i, pplUuid, playlistUuid);
                    compareCpl(validation, cplCompareInfo, ratingClip, cpl, null, i, pplUuid);
                }
            }
        }
        return validation;
    }

    default void verifyCpl(ValidationModel validation, CompositionModel cpl, int sortNum,
            String pplUuid, String tplUuid) {
        String cplUuid = cpl.getUuid();
        String cplText = StringUtils.isEmpty(cpl.getTitle()) ? cpl.getText() : cpl.getTitle();
        String contentKind = cpl.getContentKind();
        // not cpl
        if (StringUtils.isEmpty(cplUuid) || StringUtils.isEmpty(contentKind)) {
            return;
        }

        List<Rating> producerRatings = cpl.getProducerRatings();

        // cpl
        ContentKindEnum contentKindEnum = ContentKindEnum.getByName(contentKind);
        if (contentKindEnum != null) {
            switch (contentKindEnum) {
                case RATING:
                    validation.setRatingCardRatings(producerRatings);
                    break;
                case FEATURE:
                    if (CollectionUtils.isNotEmpty(cpl.getAutomation())) {
                        cpl.getAutomation().forEach(x -> {
                            AutomationEnum byText = AutomationEnum.getByText(x.getName());
                            // credit offset missing
                            if (AutomationEnum.CREDIT_OFFSET.equals(byText)
                                    && cpl.getCreditOffset() == 0) {
                                missingCreditOffsetWarning(validation, cplUuid, cplText, sortNum);
                            }

                            // intermission missing
                            if (AutomationEnum.INTERMISSION.equals(byText)
                                    && cpl.getIntermission() == 0) {
                                missingIntermissionWarning(validation, cplUuid, cplText, sortNum);
                            }
                        });
                    }

                    if (CollectionUtils.isEmpty(producerRatings)) {
                        // feature unrated
                        featureUnratedWarning(validation, cplUuid, cplText, sortNum);
                    } else {
                        List<Rating> ratingCardRatings = validation.getRatingCardRatings();
                        if (!Boolean.TRUE.equals(cpl.getRatingHardLocked()) && (
                                ratingCardRatings == null || !ratingCardRatings
                                        .containsAll(producerRatings))) {
                            // rating card missing
                            ratingCardMissingWarning(validation, cplUuid, cplText, sortNum,
                                    pplUuid, tplUuid);
                        }
                    }
                    break;
                default:
                    break;
            }
        }
    }

    default void compareCpl(ValidationModel validation, CplCompareModel compareContext,
            Map<String, List<String>> ratingClip, CompositionModel cpl, SegmentModel segment,
            int sortNum, String pplUuid) {
        String cplUuid = cpl.getUuid();
        String cplText = StringUtils.isEmpty(cpl.getTitle()) ? cpl.getText() : cpl.getTitle();
        String contentKind = cpl.getContentKind();
        // not cpl
        if (StringUtils.isEmpty(cplUuid) || StringUtils.isEmpty(contentKind)) {
            return;
        }

        boolean isFeature = ContentKindEnum.FEATURE.equals(ContentKindEnum.getByName(contentKind));
        boolean is3d = PlaylistConst.PLAYLIST_ATTRIBUTE_3D.equals(cpl.getPlaybackMode());
        Map<String, Rating> ratingMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(cpl.getProducerRatings())) {
            for (Rating rating : cpl.getProducerRatings()) {
                if (ratingClip.containsKey(rating.getTerritory())) {
                    ratingMap.put(rating.getTerritory(), rating);
                }
            }
        }

        // compare playback_mode
        if (isFeature) {
            // set feature
            CompareInfo feature = genCompareInfo(true, is3d, cplUuid, cplText, segment,
                    ratingMap, pplUuid, sortNum);
            compareContext.setFeature(feature);

            // playback mode cache warning
            List<CompareInfo> playbackModeCache = compareContext.getPlaybackModeCache();
            if (CollectionUtils.isNotEmpty(playbackModeCache) && !is3d) {
                for (CompareInfo incompatible : playbackModeCache) {
                    incompatibleContentWarning(validation, incompatible.getSortNum(),
                            incompatible.getRedirectUuid(), incompatible.getContentUuid(),
                            incompatible.getContentText(), cplText, incompatible.getSegmentText(),
                            incompatible.getSegmentType());
                }
                // clear cache
                compareContext.setPlaybackModeCache(null);
            }

            // ratings cache warning
            List<CompareInfo> ratingsCache = compareContext.getRatingsCache();
            if (CollectionUtils.isNotEmpty(ratingsCache) && MapUtils.isNotEmpty(ratingMap)) {
                for (CompareInfo compareInfo : ratingsCache) {
                    ratingMap.forEach((x, y) -> {
                        Rating compareRating = compareInfo.getRatingMap().get(x);
                        if (compareRating != null) {
                            List<String> allRatingsPerTer = ratingClip.get(x);
                            int compareIndex = allRatingsPerTer.indexOf(compareRating.getRating());
                            int featureIndex = allRatingsPerTer.indexOf(y.getRating());
                            if (compareIndex > featureIndex) {
                                invalidRatingWarning(validation, compareInfo.getSortNum(),
                                        compareInfo.getRedirectUuid(), compareInfo.getContentUuid(),
                                        compareInfo.getContentText(), cplText,
                                        compareInfo.getSegmentText(), compareInfo.getSegmentType(),
                                        y, compareRating);
                            }
                        }
                    });
                }
                // clear cache
                compareContext.setRatingsCache(null);
            }
        } else {
            CompareInfo feature = compareContext.getFeature();
            if (feature == null) {
                CompareInfo compareInfo = genCompareInfo(false, is3d, cplUuid, cplText, segment,
                        ratingMap, pplUuid, sortNum);

                // add playback mode cache
                if (is3d) {
                    Optional.ofNullable(compareContext.getPlaybackModeCache())
                            .ifPresent(x -> x.add(compareInfo));
                }

                // add ratings cache
                if (MapUtils.isNotEmpty(ratingMap)) {
                    Optional.ofNullable(compareContext.getRatingsCache())
                            .ifPresent(x -> x.add(compareInfo));
                }
            } else {
                // playback mode warning
                if (!feature.getIs3d() && is3d) {
                    incompatibleContentWarning(validation, sortNum,
                            segment == null ? pplUuid : segment.getContentAssociationUuid(),
                            cplUuid, cplText, feature.getContentText(),
                            segment == null ? null : segment.getText(),
                            segment == null ? null : segment.getContentKind());
                }

                // ratings warning
                if (MapUtils.isNotEmpty(feature.getRatingMap()) && MapUtils.isNotEmpty(ratingMap)) {
                    feature.getRatingMap().forEach((x, y) -> {
                        Rating compareRating = ratingMap.get(x);
                        if (compareRating != null) {
                            List<String> allRatingsPerTer = ratingClip.get(x);
                            int compareIndex = allRatingsPerTer.indexOf(compareRating.getRating());
                            int featureIndex = allRatingsPerTer.indexOf(y.getRating());
                            if (compareIndex > featureIndex) {
                                invalidRatingWarning(validation, sortNum,
                                        segment == null ? pplUuid
                                                : segment.getContentAssociationUuid(),
                                        cplUuid, cplText, feature.getContentText(),
                                        segment == null ? null : segment.getText(),
                                        segment == null ? null : segment.getContentKind(),
                                        y, compareRating);
                            }
                        }
                    });
                }
            }
        }
    }

    // private
    default CompareInfo genCompareInfo(boolean isFeature, Boolean is3d, String cplUuid,
            String cplText, SegmentModel segment, Map<String, Rating> ratingMap, String pplUuid,
            int sortNum) {
        CompareInfo compareInfo = new CompareInfo();
        compareInfo.setIsFeature(isFeature);
        compareInfo.setIs3d(is3d);
        compareInfo.setContentUuid(cplUuid);
        compareInfo.setContentText(cplText);
        String redirectUuid = pplUuid;
        if (segment != null) {
            compareInfo.setSegmentText(segment.getText());
            compareInfo.setSegmentType(segment.getContentKind());
            redirectUuid = SegmentTypeEnum.BASE_SEGMENT.getName().equals(segment.getContentKind())
                    ? segment.getUuid() : segment.getContentAssociationUuid();
        }
        compareInfo.setRedirectUuid(redirectUuid);
        compareInfo.setRatingMap(ratingMap);
        compareInfo.setSortNum(sortNum);
        return compareInfo;
    }

    // private
    default void incompatibleContentWarning(ValidationModel validation, int sortNum,
            String redirectUuid, String invalidCplUuid, String invalidCplText,
            String featureText, String segmentText, String segmentType) {
        // warning: other cpl is 3d,but feature is 2d
        validation.setPlaylistIssue(true);
        IssueDetail issueDetail = new IssueDetail();
        issueDetail.setRedirectId(redirectUuid);
        issueDetail.setContentId(invalidCplUuid);
        issueDetail.setName(invalidCplText);
        issueDetail.setType(TaskTypeEnum.INCOMPATIBLE_CONTENT.getTaskType());
        issueDetail.setLevel(
                TaskTypeEnum.INCOMPATIBLE_CONTENT.getLevelEnum().getIssueLevel());
        issueDetail.setIssue(TaskTypeEnum.INCOMPATIBLE_CONTENT.getTitle());
        issueDetail.setSortNumber(sortNum);
        Map<String, Object> ext = new HashMap<>();
        ext.put("feature_text", featureText);
        ext.put("segment_text", segmentText);
        ext.put("segment_type", segmentType);
        issueDetail.setExtension(ext);
        validation.getIssueDetails()
                .putIfAbsent(invalidCplUuid + "-" + issueDetail.getType(), issueDetail);
    }

    // private
    default void invalidRatingWarning(ValidationModel validation, int sortNum, String redirectUuid,
            String invalidCplUuid, String invalidCplText, String featureText, String segmentText,
            String segmentType, Rating featureRating, Rating invalidCplRating) {
        // warning: other cpl rating bigger than feature cpl
        validation.setPlaylistIssue(true);
        IssueDetail issueDetail = new IssueDetail();
        issueDetail.setRedirectId(redirectUuid);
        issueDetail.setContentId(invalidCplUuid);
        issueDetail.setName(invalidCplText);
        issueDetail.setType(TaskTypeEnum.INVALID_RATING.getTaskType());
        issueDetail.setLevel(
                TaskTypeEnum.INVALID_RATING.getLevelEnum().getIssueLevel());
        issueDetail.setIssue(TaskTypeEnum.INVALID_RATING.getTitle());
        issueDetail.setSortNumber(sortNum);
        Map<String, Object> ext = new HashMap<>();
        ext.put("feature_text", featureText);
        ext.put("segment_text", segmentText);
        ext.put("segment_type", segmentType);
        ext.put("feature_rating", featureRating);
        ext.put("content_rating", invalidCplRating);
        issueDetail.setExtension(ext);
        validation.getIssueDetails()
                .putIfAbsent(invalidCplUuid + "-" + issueDetail.getType(), issueDetail);
    }

    default void segmentEmptyWarning(ValidationModel validation, SegmentModel segment,
            int sortNum) {
        String contentAssociationUuid = segment.getContentAssociationUuid();
        if (StringUtils.isEmpty(contentAssociationUuid) || Boolean.TRUE
                .equals(segment.getDraft())) {
            //throw new RuntimeException("segment must have content_association_uuid to direct");
            // if segment is draft then no need to valid
            return;
        }

        SegmentTypeEnum segmentTypeEnum = SegmentTypeEnum.getByName(segment.getContentKind());
        String issue;
        String issueType;
        String issueLevel;
        switch (segmentTypeEnum) {
            case AUTOMATIC_SEGMENT:
                validation.setFeatureIssue(true);
                issue = TaskTypeEnum.FEATURE_NOT_ASSIGNED.getTitle();
                issueType = TaskTypeEnum.FEATURE_NOT_ASSIGNED.getTaskType();
                issueLevel = TaskTypeEnum.FEATURE_NOT_ASSIGNED.getLevelEnum().getIssueLevel();
                break;
            case TITLE_SEGMENT:
                validation.setSegmentIssue(true);
                issue = TaskTypeEnum.TITLE_SEGMENT_NOT_ASSIGNED.getTitle();
                issueType = TaskTypeEnum.TITLE_SEGMENT_NOT_ASSIGNED.getTaskType();
                issueLevel = TaskTypeEnum.TITLE_SEGMENT_NOT_ASSIGNED.getLevelEnum()
                        .getIssueLevel();
                break;
            case BASE_SEGMENT:
                validation.setSegmentIssue(true);
                issue = TaskTypeEnum.BASE_SEGMENT_NOT_ASSIGNED.getTitle();
                issueType = TaskTypeEnum.BASE_SEGMENT_NOT_ASSIGNED.getTaskType();
                issueLevel = TaskTypeEnum.BASE_SEGMENT_NOT_ASSIGNED.getLevelEnum().getIssueLevel();
                break;
            case PLAYLIST_SEGMENT:
                validation.setSegmentIssue(true);
                issue = TaskTypeEnum.SEGMENT_NOT_ASSIGNED.getTitle();
                issueType = TaskTypeEnum.SEGMENT_NOT_ASSIGNED.getTaskType();
                issueLevel = TaskTypeEnum.SEGMENT_NOT_ASSIGNED.getLevelEnum().getIssueLevel();
                break;
            case RATING_SEGMENT:
                validation.setSegmentIssue(true);
                issue = TaskTypeEnum.RATING_SEGMENT_NOT_ASSIGNED.getTitle();
                issueType = TaskTypeEnum.RATING_SEGMENT_NOT_ASSIGNED.getTaskType();
                issueLevel = TaskTypeEnum.RATING_SEGMENT_NOT_ASSIGNED.getLevelEnum().getIssueLevel();
                break;
            default:
                return;
        }
        IssueDetail issueDetail = new IssueDetail();
        issueDetail.setRedirectId(contentAssociationUuid);
        issueDetail.setContentId(segment.getUuid());
        issueDetail.setName(segment.getText());
        issueDetail.setType(issueType);
        issueDetail.setLevel(issueLevel);
        issueDetail.setIssue(issue);
        issueDetail.setSortNumber(sortNum);
        validation.getIssueDetails()
                .putIfAbsent(contentAssociationUuid + "-" + issueDetail.getType(), issueDetail);
    }

    default void missingCreditOffsetWarning(ValidationModel validation, String cplUuid,
            String cplText, int sortNum) {
        validation.setAutomationIssue(true);
        IssueDetail issueDetail = new IssueDetail();
        issueDetail.setRedirectId(cplUuid);
        issueDetail.setContentId(cplUuid);
        issueDetail.setName(cplText);
        issueDetail
                .setType(TaskTypeEnum.MISSING_CREDIT_OFFSET.getTaskType());
        issueDetail
                .setLevel(TaskTypeEnum.MISSING_CREDIT_OFFSET.getLevelEnum()
                        .getIssueLevel());
        issueDetail.setIssue(TaskTypeEnum.MISSING_CREDIT_OFFSET.getTitle());
        issueDetail.setSortNumber(sortNum);
        validation.getIssueDetails()
                .putIfAbsent(cplUuid + "-" + issueDetail.getType(), issueDetail);
    }

    default void missingIntermissionWarning(ValidationModel validation, String cplUuid,
            String cplText, int sortNum) {
        validation.setAutomationIssue(true);
        IssueDetail issueDetail = new IssueDetail();
        issueDetail.setRedirectId(cplUuid);
        issueDetail.setContentId(cplUuid);
        issueDetail.setName(cplText);
        issueDetail
                .setType(TaskTypeEnum.MISSING_INTERMISSION.getTaskType());
        issueDetail
                .setLevel(TaskTypeEnum.MISSING_INTERMISSION.getLevelEnum()
                        .getIssueLevel());
        issueDetail.setIssue(TaskTypeEnum.MISSING_INTERMISSION.getTitle());
        issueDetail.setSortNumber(sortNum);
        validation.getIssueDetails()
                .putIfAbsent(cplUuid + "-" + issueDetail.getType(), issueDetail);
    }

    default void featureUnratedWarning(ValidationModel validation, String cplUuid,
            String cplText, int sortNum) {
        validation.setRatingIssue(true);
        IssueDetail issueDetail = new IssueDetail();
        issueDetail.setRedirectId(cplUuid);
        issueDetail.setContentId(cplUuid);
        issueDetail.setName(cplText);
        issueDetail.setType(TaskTypeEnum.UNRATED_CONTENT.getTaskType());
        issueDetail.setLevel(
                TaskTypeEnum.UNRATED_CONTENT.getLevelEnum().getIssueLevel());
        issueDetail.setIssue(TaskTypeEnum.UNRATED_CONTENT.getTitle());
        issueDetail.setSortNumber(sortNum);
        validation.getIssueDetails()
                .putIfAbsent(cplUuid + "-" + issueDetail.getType(), issueDetail);
    }

    default void ratingCardMissingWarning(ValidationModel validation, String cplUuid,
            String cplText, int sortNum, String pplUuid, String tplUuid) {
        validation.setRatingCardIssue(true);
        IssueDetail issueDetail = new IssueDetail();
        issueDetail.setRedirectId(pplUuid);
        issueDetail.setContentId(cplUuid);
        issueDetail.setName(cplText);
        issueDetail.setType(TaskTypeEnum.RATING_CARD_MISSING.getTaskType());
        issueDetail.setLevel(
                TaskTypeEnum.RATING_CARD_MISSING.getLevelEnum()
                        .getIssueLevel());
        issueDetail.setIssue(TaskTypeEnum.RATING_CARD_MISSING.getTitle());
        issueDetail.setSortNumber(sortNum);
        validation.getIssueDetails()
                .putIfAbsent(tplUuid + "-" + issueDetail.getType(), issueDetail);
    }
}
