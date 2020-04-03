package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.enums.SegmentSplitTypeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.biz.model.FeatureModel;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel.SegmentSplit;
import com.aam.producer.playlist.biz.model.RatingItem;
import com.aam.producer.playlist.biz.service.IPlaylistSegmentSplitAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionService;
import com.aam.producer.playlist.biz.service.ISegmentSplitService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistShowAttributeViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionViewService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitViewService;
import com.aam.producer.playlist.protocol.message.SegmentDTO;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.protocol.response.PlaylistVersionInfo;
import com.aam.producer.playlist.protocol.response.SegmentSplitInfo;
import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.repository.entity.*;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.aam.producer.playlist.sal.response.CplInfo;
import com.aam.producer.playlist.sal.response.PosInfo;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.zone.ZoneRulesException;
import java.util.*;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PlaylistVersionViewServiceImpl implements IPlaylistVersionViewService {

    private final static Logger logger = LoggerFactory
            .getLogger(PlaylistVersionViewServiceImpl.class);

    private final IPlaylistVersionService iPlaylistVersionService;

    private final IPlaylistVersionContentViewService contentViewService;

    private final IPlaylistShowAttributeViewService showAttributeViewService;

    private final ISegmentSplitViewService iSegmentSplitViewService;

    private final ISegmentSplitService iSegmentSplitService;

    private final IPlaylistVersionContentAssociationService contentAssociationService;

    private final IPlaylistSegmentSplitAssociationService splitAssociationService;

    private final IComplexFacadeClient iComplexFacadeClient;

    @Autowired
    public PlaylistVersionViewServiceImpl(IPlaylistVersionService iPlaylistVersionService,
                                          IPlaylistVersionContentViewService contentViewService,
                                          IPlaylistShowAttributeViewService showAttributeViewService,
                                          ISegmentSplitViewService iSegmentSplitViewService,
                                          IPlaylistVersionContentAssociationService contentAssociationService,
                                          IComplexFacadeClient iComplexFacadeClient,
                                          IPlaylistSegmentSplitAssociationService splitAssociationService,
                                          ISegmentSplitService iSegmentSplitService) {
        this.iPlaylistVersionService = iPlaylistVersionService;
        this.contentViewService = contentViewService;
        this.showAttributeViewService = showAttributeViewService;
        this.iSegmentSplitViewService = iSegmentSplitViewService;
        this.contentAssociationService = contentAssociationService;
        this.iComplexFacadeClient = iComplexFacadeClient;
        this.splitAssociationService = splitAssociationService;
        this.iSegmentSplitService = iSegmentSplitService;
    }

    @Override
    public PlaylistVersionInfo getPlaylistVersion(String versionUuid) {
        PlaylistVersionDO playlistVersionDO = iPlaylistVersionService.getById(versionUuid);
        List<ContentInfo> contents = contentViewService
                .getByPplVersionUuid(playlistVersionDO.getUuid());
        List<ShowAttributeGroupInfo> groupInfos = showAttributeViewService
                .getByPplVersionUuid(playlistVersionDO.getUuid());
        return toPlaylistVersionInfo(playlistVersionDO, contents, groupInfos);
    }

    @Override
    public List<PlaylistVersionInfo> getPlaylistVersions(String playlistUuid) {
        List<PlaylistVersionDO> versionInfos = iPlaylistVersionService
                .getPlaylistVersionByPplUuid(playlistUuid);

        List<String> pplVersoinUuid = versionInfos.stream().map(PlaylistVersionDO::getUuid).collect(Collectors.toList());
        List<ContentInfo> contentInfos = contentViewService.getByPplVersionUuids(pplVersoinUuid);
        List<ShowAttributeGroupInfo> groupInfos = showAttributeViewService.getByPplVersionUuids(pplVersoinUuid);

        return versionInfos.stream().map(playlistVersionDO -> {
            List<ContentInfo> contents = contentInfos.stream()
                    .filter(contentInfo -> Objects.equals(contentInfo.getPplVersionId(), playlistVersionDO.getUuid()))
                    .collect(Collectors.toList());
            List<ShowAttributeGroupInfo> groups = groupInfos.stream()
                    .filter(group -> Objects.equals(group.getPplVersionUuid(), playlistVersionDO.getUuid()))
                    .collect(Collectors.toList());
            return toPlaylistVersionInfo(playlistVersionDO, contents, groups);
        }).collect(Collectors.toList());
    }

    @Override
    public List<PlaylistVersionInfo> getPlaylistVersions(String playlistUuid,
                                                         String versions, String titleId) {
        List<PlaylistVersionInfo> versionInfos = getPlaylistVersions(playlistUuid);
        if (StringUtils.isNotBlank(versions) && !versions
                .equals(PlaylistStatusEnum.DRAFT_AND_RELEASE.getStatusStr())) {
            versionInfos = versionInfos.stream()
                    .filter(playlistVersionInfo -> versions.equals(playlistVersionInfo.getStatus()))
                    .collect(
                            Collectors.toList());
        }
        Set<String> contentAssociationUuids = new HashSet<>();
        versionInfos.forEach(playlistVersionInfo -> playlistVersionInfo.getContentList().forEach(contentInfo -> {
            if (ContentTypeEnum.SEGMENT.getName()
                    .equals(contentInfo.getContentType())) {
                if (
                        SegmentTypeEnum.RATING_SEGMENT.getName()
                                .equals(contentInfo.getContentKind()) ||
                                SegmentTypeEnum.TITLE_SEGMENT.getName()
                                        .equals(contentInfo.getContentKind()) ||
                                SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                                        .equals(contentInfo.getContentKind()) ||
                                SegmentTypeEnum.PLAYLIST_SEGMENT.getName()
                                        .equals(contentInfo.getContentKind())
                ) {
                    contentAssociationUuids.add(contentInfo.getContentAssociationUuid());
                } else if (
                        SegmentTypeEnum.BASE_SEGMENT.getName()
                                .equals(contentInfo.getContentKind())
                ) {
                    contentAssociationUuids.add(contentInfo.getContentId());
                }
            }
        }));
        List<PlaylistSegmentSplitAssociationDO> segmentSplitAssociationDOS =
                splitAssociationService.getSegmentSplitByContentAssociationUuids(contentAssociationUuids);

        List<SegmentSplitDO> segmentSplitDOS = iSegmentSplitService.getByIds(segmentSplitAssociationDOS.stream()
                .map(PlaylistSegmentSplitAssociationDO::getSegmentSplitUuid).collect(Collectors.toList()));

        versionInfos.forEach(playlistVersionInfo -> playlistVersionInfo.getContentList().forEach(contentInfo -> {
            if (ContentTypeEnum.SEGMENT.getName()
                    .equals(contentInfo.getContentType())) {
                if (
                        SegmentTypeEnum.RATING_SEGMENT.getName()
                                .equals(contentInfo.getContentKind()) ||
                                SegmentTypeEnum.TITLE_SEGMENT.getName()
                                        .equals(contentInfo.getContentKind()) ||
                                SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                                        .equals(contentInfo.getContentKind())
                ) {
                    contentInfo.setStatus(
                            iSegmentSplitViewService.getStatus(
                                    segmentSplitAssociationDOS.stream().filter(o ->
                                            Objects.equals(o.getTitleId(), titleId) &&
                                                    Objects.equals(o.getSegmentAssociationUuid(),
                                                            contentInfo.getContentAssociationUuid()))
                                            .collect(Collectors.toList()), segmentSplitDOS
                            ).getStatusStr());
                } else if (
                        SegmentTypeEnum.PLAYLIST_SEGMENT.getName()
                                .equals(contentInfo.getContentKind())
                ) {
                    contentInfo.setStatus(
                            iSegmentSplitViewService.getStatus(
                                    segmentSplitAssociationDOS.stream().filter(o ->
                                            Objects.equals(o.getTitleId(), null) &&
                                                    Objects.equals(o.getSegmentAssociationUuid(),
                                                            contentInfo.getContentAssociationUuid()))
                                            .collect(Collectors.toList()), segmentSplitDOS
                            ).getStatusStr());

                } else if (
                        SegmentTypeEnum.BASE_SEGMENT.getName()
                                .equals(contentInfo.getContentKind())
                ) {
                    contentInfo.setStatus(
                            iSegmentSplitViewService.getStatus(segmentSplitAssociationDOS.stream().filter(o ->
                                    Objects.equals(o.getTitleId(), null) &&
                                            Objects.equals(o.getSegmentAssociationUuid(),
                                                    contentInfo.getContentId()))
                                    .collect(Collectors.toList()), segmentSplitDOS
                            ).getStatusStr());
                }
            }
        }));
        return versionInfos;
    }

    @Override
    public List<PlaylistPublishModel> getPublishMessage(PlaylistDO playlist,
            PlaylistVersionDO playlistVersion, List<PosInfo> groupPosInfos) {

        List<PlaylistPublishModel> playlistPublishModels = new ArrayList<>();
        List<String> titleUuids;
        if (Boolean.FALSE.equals(playlist.getAutomaticallyApply())) {
            titleUuids = groupPosInfos.stream().map(PosInfo::getTitleUuid).distinct().collect(
                    Collectors.toList());
        } else {
            titleUuids = getTitleUuidsByPplUuid(playlist.getUuid());
        }

        if (titleUuids.isEmpty()) {
            playlistPublishModels.add(buildAutoPlaylistPublishModel(playlist, playlistVersion,
                    groupPosInfos, null));
        } else {
            titleUuids.forEach(uuid -> playlistPublishModels
                    .add(buildAutoPlaylistPublishModel(playlist, playlistVersion, groupPosInfos,
                            uuid)));
            playlistPublishModels.add(buildAutoPlaylistPublishModel(playlist, playlistVersion,
                    groupPosInfos.stream()
                            .filter(posInfo -> !titleUuids.contains(posInfo.getTitleUuid()))
                            .collect(Collectors.toList()), null)
            );
        }
        return playlistPublishModels;
    }

    private PlaylistPublishModel buildAutoPlaylistPublishModel(PlaylistDO playlist,
            PlaylistVersionDO playlistVersion,
            List<PosInfo> groupPosInfos,
            String titleUuid) {

        if (StringUtils.isNotEmpty(titleUuid)) {
            groupPosInfos = groupPosInfos.stream()
                    .filter(posInfo -> Objects.equals(titleUuid,posInfo.getTitleUuid()))
                    .collect(Collectors.toList());
        }

        PlaylistPublishModel playlistPublishMessage = new PlaylistPublishModel();
        playlistPublishMessage.setPplUuid(playlist.getUuid());
        playlistPublishMessage.setPplVersionUuid(playlistVersion.getUuid());
        playlistPublishMessage.setTitle(playlist.getTitle());
        playlistPublishMessage.setOrganizationId(playlistVersion.getOrganizationId());
        playlistPublishMessage.setAutomatic(playlist.getAutomaticallyApply());

        List<PlaylistVersionContentAssociationDO> contentLib = contentAssociationService
                .getAssociations(playlistVersion.getUuid());

        List<PosInfo> finalGroupPosInfos = groupPosInfos;
        List<PlaylistPublishModel.PlaylistContent> playlistContents = contentLib.stream()
                .sorted((o1, o2) -> {
                    int contentTypeResult = Integer
                            .compare(o2.getContentType(), o1.getContentType());
                    if (contentTypeResult != 0) {
                        return contentTypeResult;
                    } else {
                        if (!ContentTypeEnum.SEGMENT.getCode().equals(o1.getContentType())) {
                            return 0;
                        } else {
                            SegmentDTO segmentDTO1 = JSON
                                    .parseObject(o1.getExtension(),
                                            SegmentDTO.class);
                            SegmentDTO segmentDTO2 = JSON
                                    .parseObject(o2.getExtension(),
                                            SegmentDTO.class);
                            return Integer.compare(
                                    SegmentTypeEnum.getByName(segmentDTO1.getType()).getCode(),
                                    SegmentTypeEnum.getByName(segmentDTO2.getType()).getCode());
                        }
                    }
                })
                .map(playlistVersionContentAssociationDO -> {
                    PlaylistPublishModel.PlaylistContent playlistContent = getPlaylistContent(
                            playlist.getAutomaticallyApply(),
                            playlistVersionContentAssociationDO,
                            titleUuid, finalGroupPosInfos);
                    if (ContentTypeEnum.SEGMENT.getCode()
                            .equals(playlistVersionContentAssociationDO.getContentType())) {
                        SegmentDTO segmentDTO = JSON
                                .parseObject(playlistVersionContentAssociationDO.getExtension(),
                                        SegmentDTO.class);
                        if (SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                                .equals(segmentDTO.getType())) {
                            setRatingForPos(playlistContent, finalGroupPosInfos);
                        }
                    }
                    return playlistContent;
                }).sorted(
                        Comparator
                                .comparingInt(PlaylistPublishModel.PlaylistContent::getSortNumber))
                .collect(Collectors.toList());

        playlistPublishMessage.setContentList(playlistContents);
        playlistPublishMessage.setPosUuidList(groupPosInfos.stream().map(PosInfo::getPosUuid)
                .collect(Collectors.toList()));
        return playlistPublishMessage;
    }

    private void setRatingForPos(PlaylistPublishModel.PlaylistContent playlistContent,
            List<PosInfo> groupPosInfos) {
        playlistContent.getSegmentSplits().forEach(segmentSplit -> {
            List<PlaylistPublishModel.PlaylistContent> playlistContents = segmentSplit
                    .getContentList();
            List<RatingItem> producerRatings = playlistContents.stream()
                    .filter(playlistContent1 -> {
                        if (Objects.isNull(playlistContent1.getExtension())) {
                            return false;
                        }
                        JSONObject jsonObject = JSON.parseObject(playlistContent1.getExtension());
                        String contentKind = jsonObject.getString("content_kind");
                        return "feature".equals(contentKind);
                    })
                    .flatMap(playlistContent1 -> {
                        FeatureModel featureModel = JSON
                                .parseObject(playlistContent1.getExtension(), FeatureModel.class);
                        return featureModel.getProducerRatings().stream();
                    }).collect(Collectors.toList());
            Map<String, List<RatingItem>> map = producerRatings.stream()
                    .filter(ratingItem -> !Objects.isNull(ratingItem.getTerritory()))
                    .collect(Collectors.groupingBy(RatingItem::getTerritory, Collectors.toList()));
            Map<String, String> map1 = new HashMap<>();
            map.forEach((s, ratingItems) -> {
                Set<String> set = ratingItems.stream().map(RatingItem::getRating)
                        .collect(Collectors.toSet());
                if (set.size() == 1) {
                    map1.put(s, ratingItems.get(0).getRating());
                } else {
                    logger.error("different rating of the same territory.cpl:<{}> ",
                            JSON.toJSONString(segmentSplit));
                }
            });
            segmentSplit.getShows().forEach(shows -> {
                groupPosInfos.stream()
                        .filter(posInfo -> Objects.equals(shows.getPosUuid(), posInfo.getPosUuid()))
                        .forEach(posInfo -> posInfo
                                .setRating(map1.get(posInfo.getAddressCountry())));
            });
        });
    }


    private PlaylistPublishModel.PlaylistContent getPlaylistContent(
            Boolean auto,
            PlaylistVersionContentAssociationDO playlistVersionContentAssociationDO,
            String titleUuid, List<PosInfo> groupPosInfos) {
        if (
                ContentTypeEnum.SEGMENT.getCode()
                        .equals(playlistVersionContentAssociationDO.getContentType())
        ) {
            SegmentDTO segmentDTO = JSON
                    .parseObject(playlistVersionContentAssociationDO.getExtension(),
                            SegmentDTO.class);
            List<SegmentSplitInfo> segmentSplitTreeInfos;
            if (SegmentTypeEnum.API_SEGMENT.getName()
                    .equals(segmentDTO.getType())) {
                return buildSimpleContent(playlistVersionContentAssociationDO);
            } else if (SegmentTypeEnum.PLAYLIST_SEGMENT.getName()
                    .equals(segmentDTO.getType())) {
                segmentSplitTreeInfos = iSegmentSplitViewService
                        .getSegmentSplitTree(
                                playlistVersionContentAssociationDO.getUuid(), null,
                                SegmentStatusEnum.RELEASE.getStatus());
            } else if (SegmentTypeEnum.BASE_SEGMENT.getName()
                    .equals(segmentDTO.getType())) {
                segmentSplitTreeInfos = iSegmentSplitViewService
                        .getSegmentSplitTree(
                                playlistVersionContentAssociationDO.getContentId(),
                                null, SegmentStatusEnum.RELEASE.getStatus());
            } else {
                if (StringUtils.isNotEmpty(titleUuid)) {
                    segmentSplitTreeInfos = iSegmentSplitViewService
                            .getSegmentSplitTree(
                                    playlistVersionContentAssociationDO.getUuid(),
                                    titleUuid, SegmentStatusEnum.RELEASE.getStatus());
                } else {
                    segmentSplitTreeInfos = new ArrayList<>();
                }
            }
            return buildSegmentContent(auto, playlistVersionContentAssociationDO,
                    groupPosInfos, segmentSplitTreeInfos);
        } else {
            return buildSimpleContent(playlistVersionContentAssociationDO);
        }
    }

    private List<String> getTitleUuidsByPplUuid(String pplUuid) {
        return splitAssociationService.getSegmentSplitByPlaylistUuid(pplUuid).stream()
                .map(PlaylistSegmentSplitAssociationDO::getTitleId)
                .filter(StringUtils::isNotEmpty).distinct().collect(Collectors.toList());
    }

    private PlaylistPublishModel.PlaylistContent buildSegmentContent(
            Boolean auto,
            PlaylistVersionContentAssociationDO playlistVersionContentAssociationDO,
            List<PosInfo> groupPosInfos, List<SegmentSplitInfo> segmentSplitTreeInfos) {
        PlaylistPublishModel.PlaylistContent playlistContent = buildSimpleContent(
                playlistVersionContentAssociationDO);
        List<SegmentSplit> segmentSplitList = new ArrayList<>();
        if (segmentSplitTreeInfos.isEmpty()) {
            segmentSplitList.add(newEmptyContentSegmentSplit(null, toShows(groupPosInfos)));
            Map<String, Object> map = JSONObject
                    .parseObject(playlistContent.getExtension(),
                            new TypeReference<Map<String, Object>>() {
                            });
            map.put("draft", true);
            playlistContent.setExtension(JSON.toJSONString(map));
        } else {
            List<PlaylistPublishModel.SegmentSplit> segmentSplits = getSegmentSplitModel(
                    segmentSplitTreeInfos, groupPosInfos);
            segmentSplitList.addAll(segmentSplits);
        }
        if(Boolean.TRUE.equals(auto)) {
            playlistContent.setSegmentSplits(filterEmptyShows(segmentSplitList));
        }else{
            playlistContent.setSegmentSplits(segmentSplitList);
        }
        return playlistContent;
    }

    private List<SegmentSplit> filterEmptyShows(List<SegmentSplit> segmentSplits) {
        return segmentSplits.stream().filter(segmentSplit -> !segmentSplit.getShows().isEmpty())
                .collect(Collectors.toList());
    }


    private PlaylistPublishModel.PlaylistContent buildSimpleContent(
            PlaylistVersionContentAssociationDO playlistVersionContentAssociationDO) {
        PlaylistPublishModel.PlaylistContent playlistContent = new PlaylistPublishModel.PlaylistContent();
        playlistContent.setExtension(playlistVersionContentAssociationDO.getExtension());
        playlistContent.setContentUuid(playlistVersionContentAssociationDO.getContentId());
        playlistContent.setContentType(playlistVersionContentAssociationDO.getContentType());
        playlistContent.setContentAssociationUuid(playlistVersionContentAssociationDO.getUuid());
        playlistContent.setSortNumber(playlistVersionContentAssociationDO.getSortNumber());
        return playlistContent;
    }

    private PlaylistPublishModel.SegmentSplit newEmptyContentSegmentSplit(String titleUuid,
            List<PlaylistPublishModel.Shows> shows) {
        PlaylistPublishModel.SegmentSplit segmentSplit = new PlaylistPublishModel.SegmentSplit();
        segmentSplit.setSegmentSplitTitle("default");
        segmentSplit.setSegmentSplitUuid(UUID.nameUUIDFromBytes((titleUuid + "").getBytes(
                StandardCharsets.UTF_8)).toString());
        segmentSplit.setShows(shows);
        return segmentSplit;
    }

    private List<PlaylistPublishModel.Shows> toShows(List<PosInfo> posInfos) {
        List<PlaylistPublishModel.Shows> showsList = new ArrayList<>();
        posInfos.forEach(posInfo -> {
            PlaylistPublishModel.Shows shows = new PlaylistPublishModel.Shows();
            shows.setPosUuid(posInfo.getPosUuid());
            showsList.add(shows);
        });
        return showsList;
    }

    private List<PlaylistPublishModel.SegmentSplit> getSegmentSplitModel(
            List<SegmentSplitInfo> segmentSplitInfoTreeRootList, List<PosInfo> posInfoList) {
        segmentSplitInfoTreeRootList
                .forEach(segmentSplitInfo -> segmentSplitMatchPos(segmentSplitInfo, posInfoList));
        return segmentSplitInfoTreeRootList.stream().flatMap(segmentSplitInfo ->
                buildSegmentSplitModel(segmentSplitInfo).stream()).collect(Collectors.toList());
    }

    private List<PlaylistPublishModel.SegmentSplit> buildSegmentSplitModel(
            SegmentSplitInfo segmentSplitInfo) {
        List<PlaylistPublishModel.SegmentSplit> segmentSplits = new ArrayList<>();
        Stack<SegmentSplitInfo> segmentSplitInfos = new Stack<>();
        segmentSplitInfos.push(segmentSplitInfo);
        while (!segmentSplitInfos.isEmpty()) {
            SegmentSplitInfo segmentSplitInfo1 = segmentSplitInfos.pop();
            if (segmentSplitInfo1.getChildren().isEmpty()) {
                PlaylistPublishModel.SegmentSplit segmentSplit = new PlaylistPublishModel.SegmentSplit();
                segmentSplit.setSegmentSplitUuid(segmentSplitInfo1.getSign());
                setTitleAndShowType(segmentSplit, segmentSplitInfo1);

                segmentSplit.setShows(segmentSplitInfo1.getShowUuids().stream().map(uuid -> {
                    PlaylistPublishModel.Shows shows = new PlaylistPublishModel.Shows();
                    shows.setPosUuid(uuid);
                    return shows;
                }).collect(Collectors.toList()));

                segmentSplit
                        .setContentList(
                                segmentSplitInfo1.getContentList().stream().map(contentInfo -> {
                                    PlaylistPublishModel.PlaylistContent playlistContent1 = new PlaylistPublishModel.PlaylistContent();
                                    playlistContent1
                                            .setContentType(
                                                    ContentTypeEnum
                                                            .getByName(contentInfo.getContentType())
                                                            .getCode());
                                    playlistContent1.setExtension(contentInfo.getExtension());
                                    playlistContent1.setContentUuid(contentInfo.getContentId());
                                    return playlistContent1;
                                }).collect(Collectors.toList()));
                segmentSplit.setSegmentSplitTitle(segmentSplitInfo1.getSplitTitle());
                segmentSplits.add(segmentSplit);
            } else {
                segmentSplitInfo1.getChildren().forEach(segmentSplitInfos::push);
            }
        }
        return segmentSplits;
    }

    @Override
    public void segmentSplitMatchPos(SegmentSplitInfo segmentSplitInfo, List<PosInfo> posInfoList) {
        segmentSplitTreeMatchPos(segmentSplitInfo, posInfoList);
    }

    private int countSite(List<PosInfo> posInfoList, List<String> uuids) {
        return posInfoList.stream().filter(posInfo -> uuids.contains(posInfo.getPosUuid()))
                .map(PosInfo::getComplexId)
                .collect(Collectors.toSet()).size();
    }

    private List<String> getDefaultSegmentSplitMatchPosUuids(SegmentSplitInfo segmentSplitInfo) {
        List<String> parentList = segmentSplitInfo.getParent().getShowUuids();
        List<String> siblings = segmentSplitInfo.getSiblingsAndSelf().stream()
                .flatMap(segmentSplitInfo1 ->
                        segmentSplitInfo1.getShowUuids().stream()).collect(Collectors.toList());
        return parentList.stream().filter(s -> !siblings.contains(s)).collect(Collectors.toList());
    }

    private void segmentSplitTreeMatchPos(SegmentSplitInfo segmentSplitInfo,
            List<PosInfo> posInfoList) {
        //获取当前规则匹配到的场次
        List<PosInfo> posInfos;

        if (Boolean.TRUE.equals(segmentSplitInfo.getDefaultGroup())) {
            posInfos = posInfoList;
        } else {
            posInfos = posInfoFilter(segmentSplitInfo, posInfoList);
        }

        segmentSplitInfo.setShowUuids(
                posInfos.stream().map(PosInfo::getPosUuid).collect(Collectors.toList()));
        segmentSplitInfo.setShows(
                posInfos.stream().map(PosInfo::getPosUuid).collect(Collectors.toSet()).size());
        segmentSplitInfo.setSites(
                posInfos.stream().map(PosInfo::getComplexId).collect(Collectors.toSet()).size());

        List<SegmentSplitInfo> segmentSplitInfos = segmentSplitInfo.getChildren();
        List<SegmentSplitInfo> notDefaultSegmentSplit = findNotDefaultSegmentSplit(
                segmentSplitInfos);
        if (!notDefaultSegmentSplit.isEmpty()) {
            Map<String, List<PosInfo>> map = new HashMap<>();
            SegmentSplitInfo firstSplitInfo = notDefaultSegmentSplit.get(0);
            if (SegmentSplitTypeEnum.PERCENTAGE.getName().equals(firstSplitInfo.getSplitType())) {
                Map<PosInfo, Integer> map1 = new HashMap<>();
                posInfos.forEach(posInfo -> map1.put(posInfo, 1 + (int) (Math.random() * 100)));
                List<Integer> list = new ArrayList<>();
                notDefaultSegmentSplit.forEach(segmentSplitInfo1 -> {
                    int percentage = Integer.parseInt(segmentSplitInfo1.getSplitRule());
                    int start = list.isEmpty() ? 0 : list.get(list.size() - 1);
                    int end = start + percentage;
                    list.add(end);
                    List<PosInfo> posInfos1 = new ArrayList<>();
                    map1.forEach((posInfo, integer) -> {
                        if (integer > start && integer <= end) {
                            posInfos1.add(posInfo);
                        }
                    });
                    map.put(segmentSplitInfo1.getUuid(), posInfos1);
                });
            }

            notDefaultSegmentSplit.forEach(segmentSplitInfo1 -> {
                if (SegmentSplitTypeEnum.PERCENTAGE.getName()
                        .equals(segmentSplitInfo1.getSplitType())) {
                    segmentSplitTreeMatchPos(segmentSplitInfo1,
                            map.get(segmentSplitInfo1.getUuid()));
                } else {
                    segmentSplitTreeMatchPos(segmentSplitInfo1, posInfos);
                }
            });
        }

        List<SegmentSplitInfo> defaultSegmentSplit = findDefaultSegmentSplit(segmentSplitInfos);

        defaultSegmentSplit.forEach(segmentSplitInfo1 -> {
            List<String> uuids = getDefaultSegmentSplitMatchPosUuids(segmentSplitInfo1);
            segmentSplitTreeMatchPos(segmentSplitInfo1,
                    posInfos.stream().filter(posInfo -> uuids.contains(posInfo.getPosUuid()))
                            .collect(Collectors.toList()));
        });

    }

    private List<SegmentSplitInfo> findDefaultSegmentSplit(
            List<SegmentSplitInfo> segmentSplitInfos) {
        return segmentSplitInfos.stream()
                .filter(SegmentSplitInfo::getDefaultGroup)
                .collect(Collectors.toList());

    }


    private void setTitleAndShowType(PlaylistPublishModel.SegmentSplit segmentSplit,
            SegmentSplitInfo segmentSplitInfo) {
        Stack<SegmentSplitInfo> segmentSplitInfos = new Stack<>();
        segmentSplitInfos.push(segmentSplitInfo);
        while (!segmentSplitInfos.isEmpty()) {
            SegmentSplitInfo segmentSplitInfo1 = segmentSplitInfos.pop();
            if (SegmentSplitTypeEnum.SHOW_ATTR.getName().equals(segmentSplitInfo1.getSplitType())) {
                segmentSplit.setShowType(segmentSplitInfo1.getSplitTitle());
            }
            if (SegmentSplitTypeEnum.TITLE.getName().equals(segmentSplitInfo1.getSplitType())) {
                segmentSplit.setTitle(segmentSplitInfo1.getSplitTitle());
            }
            if (segmentSplitInfo1.getParent() != null) {
                segmentSplitInfos.push(segmentSplitInfo1.getParent());
            }
        }
    }


    private List<PosInfo> posInfoFilter(SegmentSplitInfo segmentSplitInfo,
            List<PosInfo> posInfoList) {

        if (SegmentSplitTypeEnum.PERCENTAGE.getName().equals(segmentSplitInfo.getSplitType())) {
            return posInfoList;
        }

        if (Boolean.TRUE.equals(segmentSplitInfo.getDefaultGroup())) {
            return posInfoList;
        }

        if (SegmentSplitTypeEnum.RATING.getName().equals(segmentSplitInfo.getSplitType())) {
            Map<String, List<String>> map = JSON.parseObject(segmentSplitInfo.getSplitRule(),
                    new TypeReference<Map<String, List<String>>>() {
                    });
            return posInfoList.stream().filter(posInfo -> {
                List<String> strings = map.get(posInfo.getAddressCountry());
                if (CollectionUtils.isEmpty(strings)) {
                    return false;
                } else {
                    return strings.contains(posInfo.getRating());
                }
            }).collect(Collectors.toList());
        }

        if (SegmentSplitTypeEnum.DATE.getName().equals(segmentSplitInfo.getSplitType())) {
            JSONArray jsonArray = JSON.parseArray(segmentSplitInfo.getSplitRule());
            return posInfoList.stream().filter(posInfo -> {
                String locale = iComplexFacadeClient.getComplexLocale(posInfo.getComplexId());
                long showStartNum = posInfo.getShowStart();
                if ((showStartNum + "").length() < 13) {
                    showStartNum = showStartNum * 1000;
                }
                String showStart = DateFormatUtils.format(showStartNum, "yyyy/MM/dd",
                        locale != null ? TimeZone.getTimeZone(locale) : TimeZone.getDefault());

                boolean b = false;
                for (int i = 0; i < jsonArray.size(); i++) {
                    JSONObject jsonObject = jsonArray.getJSONObject(i);
                    String start = jsonObject.getString("start");
                    String end = jsonObject.getString("end");
                    if (showStart.compareTo(start) >= 0 && showStart.compareTo(end) <= 0) {
                        b = true;
                        break;
                    }
                }
                return b;
            }).collect(Collectors.toList());
        }

        if (SegmentSplitTypeEnum.DAY.getName().equals(segmentSplitInfo.getSplitType())) {
            List<Integer> days = JSON.parseObject(segmentSplitInfo.getSplitRule(),
                    new TypeReference<List<Integer>>() {
                    });
            return posInfoList.stream().filter(posInfo -> {
                long showStartNum = posInfo.getShowStart();
                if ((showStartNum + "").length() < 13) {
                    showStartNum = showStartNum * 1000;
                }
                String locale = iComplexFacadeClient.getComplexLocale(posInfo.getComplexId());
                Instant timestamp = Instant.ofEpochMilli(showStartNum);
                ZonedDateTime losAngelesTime;
                try {
                    losAngelesTime = timestamp.atZone(ZoneId.of(locale));
                } catch (ZoneRulesException e) {
                    losAngelesTime = timestamp.atZone(ZoneId.systemDefault());
                }
                int value = losAngelesTime.getDayOfWeek().getValue();
                return days.contains(value - 1);
            }).collect(Collectors.toList());
        }

        if (SegmentSplitTypeEnum.ROOT.getName().equals(segmentSplitInfo.getSplitType())) {
            return posInfoList;
        }

        if (SegmentSplitTypeEnum.CPL_FORMAT.getName().equals(segmentSplitInfo.getSplitType())) {
            return posInfoList.stream().filter(posInfo -> segmentSplitInfo
                    .equals(matchJustCpl(segmentSplitInfo.getSiblingsAndSelf(), posInfo)))
                    .collect(Collectors.toList());
        }

        if (SegmentSplitTypeEnum.SHOWS.getName().equals(segmentSplitInfo.getSplitType())) {
            List<String> uuids = JSON
                    .parseObject(segmentSplitInfo.getSplitRule(),
                            new TypeReference<List<String>>() {
                            });
            return posInfoList.stream().filter(posInfo -> uuids.contains(posInfo.getPosUuid()))
                    .collect(Collectors.toList());
        }

        if (SegmentSplitTypeEnum.TITLE.getName().equals(segmentSplitInfo.getSplitType())) {
            return posInfoList.stream().filter(posInfo -> {

                if (posInfo.getTitleUuid() == null) {
                    return Objects.equals(posInfo.getTitle(), segmentSplitInfo.getSplitRule());
                } else {
                    return Objects.equals(posInfo.getTitleUuid(), segmentSplitInfo.getSplitRule());
                }

            }).collect(Collectors.toList());
        }

        if (SegmentSplitTypeEnum.SHOW_ATTR.getName().equals(segmentSplitInfo.getSplitType())) {

            List<List<String>> showTypes;

            try {
                //Compatible with old formats.
                showTypes = JSON.parseObject(segmentSplitInfo.getSplitRule(),
                        new TypeReference<List<List<String>>>() {
                        });
            } catch (Exception e) {
                List<ShowAttributeGroupInfo> showAttributeGroupInfos = JSON
                        .parseObject(segmentSplitInfo.getSplitRule(),
                                new TypeReference<List<ShowAttributeGroupInfo>>() {
                                });
                showTypes = showAttributeGroupInfos.stream().map(
                        ShowAttributeGroupInfo::getAttributes).collect(
                        Collectors.toList());
            }

            List<List<String>> finalShowTypes = showTypes;
            return posInfoList.stream()
                    .filter(posInfo -> {
                        List<String> posShowAttrList = posInfo.getShowAttrList();
                        return containShowAttrList(finalShowTypes, posShowAttrList);
                    }).collect(Collectors.toList());
        }

        if (SegmentSplitTypeEnum.SITES.getName().equals(segmentSplitInfo.getSplitType())) {
            List<String> complexIds = JSON
                    .parseObject(segmentSplitInfo.getSplitRule(),
                            new TypeReference<List<String>>() {
                            });
            return posInfoList.stream()
                    .filter(posInfo -> complexIds.contains(posInfo.getComplexId()))
                    .collect(Collectors.toList());
        }

        if (SegmentSplitTypeEnum.TIME.getName().equals(segmentSplitInfo.getSplitType())) {
            JSONArray jsonArray = JSONObject.parseArray(segmentSplitInfo.getSplitRule());
            if (jsonArray.isEmpty()) {
                return new ArrayList<>();
            }
            JSONObject jsonObject = jsonArray.getJSONObject(0);
            String start = jsonObject.getString("start");
            String end = jsonObject.getString("end");
            return posInfoList.stream().filter(posInfo -> {
                if (posInfo.getShowStart() == null) {
                    return false;
                }
                String locale = iComplexFacadeClient.getComplexLocale(posInfo.getComplexId());
                long showStartNum = posInfo.getShowStart();
                if ((showStartNum + "").length() < 13) {
                    showStartNum = showStartNum * 1000;
                }
                String showStart = DateFormatUtils.format(showStartNum, "yyyyMMddHH:mmss",
                        locale != null ? TimeZone.getTimeZone(locale) : TimeZone.getDefault())
                        .substring(8, 13);
                if (end.compareTo(start) >= 0) {
                    return showStart.compareTo(start) >= 0 && showStart.compareTo(end) < 0;
                } else {
                    return !(showStart.compareTo(end) >= 0 && showStart.compareTo(start) < 0);
                }
            }).collect(Collectors.toList());
        }

        return new ArrayList<>();
    }

    private boolean containShowAttrList(List<List<String>> pplShowAttrList,
            List<String> posShowAttrList) {
        List<String> ppl = pplShowAttrList.stream()
                .map(strings -> strings.stream().sorted().collect(Collectors.joining("|")))
                .collect(Collectors.toList());
        String pos = posShowAttrList.stream().sorted().collect(Collectors.joining("|"));
        return ppl.contains(pos);
    }


    private List<SegmentSplitInfo> findNotDefaultSegmentSplit(
            List<SegmentSplitInfo> segmentSplitInfo) {
        return segmentSplitInfo.stream()
                .filter(segmentSplitInfo1 -> !segmentSplitInfo1.getDefaultGroup())
                .collect(Collectors.toList());
    }

    private List<SegmentSplitInfo> filterByLanguage(List<SegmentSplitInfo> segmentSplitInfos,
            PosInfo posInfo) {
        boolean hasLanguage = StringUtils.isNotBlank(posInfo.getLanguage());
        if (hasLanguage) {
            return segmentSplitInfos.stream().filter(segmentSplitInfo -> {
                CplInfo cplInfo = JSON.parseObject(segmentSplitInfo.getSplitRule(), CplInfo.class);
                return StringUtils.equalsIgnoreCase(cplInfo.getLanguage(), posInfo.getLanguage());
            }).collect(Collectors.toList());
        } else {
            return segmentSplitInfos;
        }
    }

    private List<SegmentSplitInfo> filterByCapability(List<SegmentSplitInfo> segmentSplitInfos,
            PosInfo posInfo) {
        String capability = posInfo.getFilmHallInfo().getCapability();

        if (StringUtils.isBlank(capability)) {
            List<SegmentSplitInfo> resultList = filterSegmentSplitInfoByCapability(capability,
                    segmentSplitInfos);
            if (!resultList.isEmpty()) {
                return resultList;
            }
            resultList = filterSegmentSplitInfoByCapability("ATMOS", segmentSplitInfos);
            if (!resultList.isEmpty()) {
                return resultList;
            }
            resultList = filterSegmentSplitInfoByCapability("7.1", segmentSplitInfos);
            if (!resultList.isEmpty()) {
                return resultList;
            }
            return filterSegmentSplitInfoByCapability("5.1", segmentSplitInfos);
        } else {
            return segmentSplitInfos;
        }

    }

    private List<SegmentSplitInfo> filterByScope(List<SegmentSplitInfo> segmentSplitInfos,
            PosInfo posInfo) {
        return segmentSplitInfos.stream().filter(segmentSplitInfo -> {
            CplInfo cplInfo = JSON.parseObject(segmentSplitInfo.getSplitRule(), CplInfo.class);
            if (posInfo.getFilmHallInfo().isImax()) {
                return cplInfo.getFormatList().stream().map(String::toUpperCase)
                        .collect(Collectors.toList()).contains("FLAT");
            } else {
                return cplInfo.getFormatList().stream().map(String::toUpperCase)
                        .collect(Collectors.toList()).contains("SCOPE");
            }
        }).collect(Collectors.toList());

    }


    private List<SegmentSplitInfo> filterBySubTitle(List<SegmentSplitInfo> segmentSplitInfos,
            PosInfo posInfo) {
        boolean hasSubTitle = !posInfo.getUnmatchedShowAttributes().isEmpty();
        if (hasSubTitle) {
            return segmentSplitInfos.stream().filter(segmentSplitInfo -> {
                CplInfo cplInfo = JSON.parseObject(segmentSplitInfo.getSplitRule(), CplInfo.class);
                if (StringUtils.isBlank(cplInfo.getSubtitleLanguage())) {
                    return false;
                }
                return posInfo.getUnmatchedShowAttributes().stream().map(String::toUpperCase)
                        .collect(Collectors.toList())
                        .contains(cplInfo.getSubtitleLanguage().toUpperCase());
            }).collect(Collectors.toList());
        } else {
            return segmentSplitInfos;
        }
    }

    private SegmentSplitInfo matchJustCpl(List<SegmentSplitInfo> segmentSplitInfos,
            PosInfo posInfo) {

        segmentSplitInfos = segmentSplitInfos.stream().filter(segmentSplitInfo ->
                !SegmentSplitTypeEnum.SHOWS.getName().equals(segmentSplitInfo.getSplitType()))
                .collect(Collectors.toList());

        //如果为空返回空
        if (segmentSplitInfos.isEmpty()) {
            return null;
        }

        List<SegmentSplitInfo> languageResultList = filterByLanguage(segmentSplitInfos, posInfo);

        if (languageResultList.isEmpty()) {
            return null;
        }

        List<SegmentSplitInfo> subTitleResultList = filterBySubTitle(languageResultList, posInfo);

        if (subTitleResultList.isEmpty()) {
            return null;
        }

        if (subTitleResultList.size() == 1) {
            return subTitleResultList.get(0);
        }

        List<SegmentSplitInfo> capabilityResultList = filterByCapability(subTitleResultList,
                posInfo);

        if (capabilityResultList.size() == 1) {
            return capabilityResultList.get(0);
        }

        if (capabilityResultList.isEmpty()) {
            capabilityResultList = languageResultList;
        }

        List<SegmentSplitInfo> scopeResultList = filterByScope(capabilityResultList, posInfo);

        if (scopeResultList.size() == 1) {
            return scopeResultList.get(0);
        }

        if (scopeResultList.isEmpty()) {
            scopeResultList = capabilityResultList;
        }

        return lastModifyCpl(scopeResultList);
    }

    private SegmentSplitInfo lastModifyCpl(List<SegmentSplitInfo> segmentSplitInfos) {
        segmentSplitInfos.sort((o1, o2) -> {
            CplInfo cplInfo1 = JSON.parseObject(o1.getSplitRule(), CplInfo.class);
            CplInfo cplInfo2 = JSON.parseObject(o2.getSplitRule(), CplInfo.class);
            return Objects.compare(cplInfo1.getCreated(), cplInfo2.getCreated(), String::compareTo);
        });
        if (!segmentSplitInfos.isEmpty()) {
            return segmentSplitInfos.get(0);
        }
        return null;
    }

    private List<SegmentSplitInfo> filterSegmentSplitInfoByCapability(String capability,
            List<SegmentSplitInfo> inputList) {
        return inputList.stream().filter(segmentSplitInfo -> {
            CplInfo cplInfo = JSON.parseObject(segmentSplitInfo.getSplitRule(), CplInfo.class);
            return cplInfo.getFormatList().stream().map(String::toUpperCase)
                    .collect(Collectors.toList())
                    .contains(capability);
        }).collect(Collectors.toList());
    }

}
