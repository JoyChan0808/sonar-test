package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.enums.SegmentSplitTypeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.biz.event.ChangeTplEvent;
import com.aam.producer.playlist.biz.event.CreateBaseSegmentRootSplit;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel.SegmentSplit;
import com.aam.producer.playlist.biz.service.IPlaylistSegmentSplitAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.biz.service.ISegmentService;
import com.aam.producer.playlist.biz.service.ISegmentSplitContentAssociationService;
import com.aam.producer.playlist.biz.service.ISegmentSplitService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionActionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionViewService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitViewService;
import com.aam.producer.playlist.biz.service.domain.ISegmentViewService;
import com.aam.producer.playlist.biz.service.domain.ITaskReactorService;
import com.aam.producer.playlist.biz.util.CommonSourcePool;
import com.aam.producer.playlist.biz.util.DefaultSchedulingConfigurer;
import com.aam.producer.playlist.biz.util.EventPublish;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.biz.util.TestUtil;
import com.aam.producer.playlist.protocol.message.SegmentDTO;
import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.protocol.request.PublishDTO;
import com.aam.producer.playlist.protocol.request.SegmentSplitDTO;
import com.aam.producer.playlist.protocol.request.SegmentSplitOptionDTO;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.protocol.response.PlaylistVersionInfo;
import com.aam.producer.playlist.protocol.response.SegmentInfo;
import com.aam.producer.playlist.protocol.response.SegmentSplitInfo;
import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.aam.producer.playlist.repository.entity.SegmentSplitDO;
import com.aam.producer.playlist.sal.client.ICplFacadeClient;
import com.aam.producer.playlist.sal.client.IOrgFacadeClient;
import com.aam.producer.playlist.sal.client.ITitleFacadeClient;
import com.aam.producer.playlist.sal.response.CplInfo;
import com.aam.producer.playlist.sal.response.TitleInfo;
import com.aam.producer.task.protocol.enums.TaskTypeEnum;
import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;
import com.aam.utils.utils.SpringContextUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SegmentSplitActionServiceImpl implements ISegmentSplitActionService,
        ApplicationListener<CreateBaseSegmentRootSplit> {

    private static final String[] CONTRAST_SHOW_ATTRIBUTE = {"2D", "3D", "ATMOS", "IMAX", "HFR",
            "SUB", "VI", "DBOX"};
    private final static Logger logger = LoggerFactory
            .getLogger(SegmentSplitActionServiceImpl.class);
    private final ISegmentSplitService iSegmentSplitService;
    private final IPlaylistVersionContentViewService contentViewService;
    private final ISegmentSplitContentAssociationService segmentContentService;
    private final IPlaylistSegmentSplitAssociationService splitAssociationService;
    private final ISegmentSplitViewService iSegmentSplitViewService;
    private final DefaultSchedulingConfigurer defaultSchedulingConfigurer;
    private final ITitleFacadeClient titleFacadeClient;
    private final IOrgFacadeClient iOrgFacadeClient;
    private final ICplFacadeClient iCplFacadeClient;
    private final EventPublish eventPublish;
    private final ISegmentService iSegmentService;
    private final ISegmentViewService iSegmentViewService;
    private final ITaskReactorService iTaskReactorService;

    @Autowired
    public SegmentSplitActionServiceImpl(
            ISegmentSplitService iSegmentSplitService,
            ISegmentSplitContentAssociationService segmentContentService,
            IPlaylistSegmentSplitAssociationService splitAssociationService,
            ISegmentSplitViewService iSegmentSplitViewService,
            DefaultSchedulingConfigurer defaultSchedulingConfigurer,
            EventPublish eventPublish,
            ITitleFacadeClient titleFacadeClient,
            IOrgFacadeClient iOrgFacadeClient,
            ICplFacadeClient iCplFacadeClient,
            IPlaylistVersionContentViewService contentViewService,
            ISegmentViewService iSegmentViewService,
            ISegmentService iSegmentService,
            ITaskReactorService iTaskReactorService) {
        this.titleFacadeClient = titleFacadeClient;
        this.iOrgFacadeClient = iOrgFacadeClient;
        this.iCplFacadeClient = iCplFacadeClient;
        this.segmentContentService = segmentContentService;
        this.iSegmentSplitService = iSegmentSplitService;
        this.splitAssociationService = splitAssociationService;
        this.iSegmentSplitViewService = iSegmentSplitViewService;
        this.defaultSchedulingConfigurer = defaultSchedulingConfigurer;
        this.eventPublish = eventPublish;
        this.contentViewService = contentViewService;
        this.iSegmentViewService = iSegmentViewService;
        this.iSegmentService = iSegmentService;
        this.iTaskReactorService = iTaskReactorService;
    }


    @Override
    @Transactional
    public void createDefaultSegmentSplit(String associationUuid, String titleUuid) {
        logger.info("create default segment split. associationUuid:<{}> titleUuid:<{}>",
                associationUuid, titleUuid);
        PlaylistVersionContentAssociationDO playlistVersionContentAssociationDO = contentViewService
                .getByContentAssociationUuid(associationUuid);
        if (playlistVersionContentAssociationDO == null) {
            return;
        }
        String versionUuid = playlistVersionContentAssociationDO.getPplVersionId();
        String playlistUuid = playlistVersionContentAssociationDO.getPlaylistUuid();

        IPlaylistVersionViewService iPlaylistVersionViewService = SpringContextUtils
                .getBean(IPlaylistVersionViewService.class);
        PlaylistVersionInfo playlistVersionInfo = iPlaylistVersionViewService
                .getPlaylistVersion(versionUuid);
        if (playlistVersionInfo == null) {
            return;
        }
        List<SegmentSplitInfo> segmentSplitInfos = iSegmentSplitViewService
                .getSegmentSplitList(associationUuid, titleUuid, null);

        ContentInfo contentInfoObj = playlistVersionInfo.getContentList().stream()
                .filter(contentInfo -> associationUuid
                        .equals(contentInfo.getContentAssociationUuid())).findFirst()
                .orElse(null);
        if (contentInfoObj == null) {
            return;
        }
        SegmentInfo segmentInfo = JSON
                .parseObject(contentInfoObj.getExtension(), SegmentInfo.class);
        if (SegmentTypeEnum.PLAYLIST_SEGMENT.getName().equals(segmentInfo.getContentKind())) {
            return;
        }

        if (segmentSplitInfos.isEmpty() || (Boolean.TRUE.equals(segmentInfo.getSplitByWeek()))) {

            List<TitleInfo> titleInfos = buildTitleList(
                    playlistVersionInfo.getShowAttributeGroups().stream()
                            .map(ShowAttributeGroupInfo::getName).collect(Collectors.toList()),
                    Sets.newHashSet(titleUuid));
            titleInfos.forEach(
                    titleInfo -> createTitleSegmentSplit(titleInfo, associationUuid, playlistUuid,
                            versionUuid, playlistVersionInfo.getShowAttributeGroups(),
                            segmentInfo.getContentKind(),
                            Boolean.TRUE.equals(segmentInfo.getMultiSegment()),
                            segmentInfo.getSplitByWeek()));
        } else {
            logger.info("segment split already exist. associationUuid:<{}> titleUuid:<{}>",
                    associationUuid, titleUuid);
        }

    }


    @Override
    @Transactional
    public void resetSegmentSplit(String associationUuid, String titleUuid) {
        logger.info("reset segmentSplit. associationUuid:<{}> titleUuid:<{}>", associationUuid,
                titleUuid);
        PlaylistVersionContentAssociationDO playlistVersionContentAssociationDO = contentViewService
                .getByContentAssociationUuid(associationUuid);
        if (playlistVersionContentAssociationDO == null) {
            return;
        }
        String versionUuid = playlistVersionContentAssociationDO.getPplVersionId();
        String playlistUuid = playlistVersionContentAssociationDO.getPlaylistUuid();
        IPlaylistVersionViewService iPlaylistVersionViewService = SpringContextUtils
                .getBean(IPlaylistVersionViewService.class);
        PlaylistVersionInfo playlistVersionInfo = iPlaylistVersionViewService
                .getPlaylistVersion(versionUuid);
        if (playlistVersionInfo == null) {
            return;
        }
        List<SegmentSplitInfo> segmentSplitInfos = iSegmentSplitViewService
                .getSegmentSplitList(associationUuid, titleUuid,
                        SegmentStatusEnum.DRAFT.getStatus());

        if (!segmentSplitInfos.isEmpty()) {
            iSegmentSplitService.removeByIds(
                    segmentSplitInfos.stream().map(SegmentSplitInfo::getUuid)
                            .collect(Collectors.toList()));
        }

        ContentInfo contentInfoObj = playlistVersionInfo.getContentList().stream()
                .filter(contentInfo -> associationUuid
                        .equals(contentInfo.getContentAssociationUuid())).findFirst().orElse(null);
        if (contentInfoObj == null) {
            return;
        }
        SegmentInfo segmentInfo = JSON
                .parseObject(contentInfoObj.getExtension(), SegmentInfo.class);
        if (SegmentTypeEnum.PLAYLIST_SEGMENT.getName().equals(segmentInfo.getContentKind())) {
            return;
        }

        List<TitleInfo> titleInfos = buildTitleList(
                playlistVersionInfo.getShowAttributeGroups().stream()
                        .map(ShowAttributeGroupInfo::getName).collect(Collectors.toList()),
                Sets.newHashSet(titleUuid));
        titleInfos.forEach(
                titleInfo -> createTitleSegmentSplit(titleInfo, associationUuid, playlistUuid,
                        versionUuid, playlistVersionInfo.getShowAttributeGroups(),
                        segmentInfo.getContentKind(),
                        Boolean.TRUE.equals(segmentInfo.getMultiSegment()),
                        segmentInfo.getSplitByWeek()));

        try {
            IPlaylistVersionActionService iPlaylistVersionActionService = SpringContextUtils
                    .getBean(IPlaylistVersionActionService.class);
            iTaskReactorService
                    .sendSegmentSplitEvent(playlistUuid, titleUuid, associationUuid,
                            TaskTypeEnum.NEW_CONTENT_AVAILABLE, true,
                            SegmentTypeEnum.AUTOMATIC_SEGMENT.getName());
        } catch (Exception e) {
            //降级处理
            logger.error("send segment warning event failed.", e);
        }

    }

    @Override
    public void copy(String oldContentAssociationUuid, String contentAssociationUuid,
                     String pplVersion) {
        logger.info(
                "copy sementSplit. oldContentAssociationUuid:<{}> contentAssociationUuid:<{}> pplVersion:<{}>",
                oldContentAssociationUuid, contentAssociationUuid, pplVersion);
        List<SegmentSplitInfo> segmentSplitInfos = iSegmentSplitViewService
                .getSegmentSplitTree(oldContentAssociationUuid, null, null);
        segmentSplitInfos.forEach(
                segmentSplitInfo -> copySegmentSplit(contentAssociationUuid, segmentSplitInfo, null,
                        pplVersion));
    }

    @Transactional
    @Override
    public void createSplitByWeek(String rootUuid, String playlistUuid, String pplVersionId,
                                  String titleUuid,
                                  String segmentAssociationUuid) {
        logger.info(
                "Create split by week. rootUuid:<{}> playlistUuid:<{}> pplVersionId:<{}> titleUuid:<{}> segmentAssociationUuid:<{}>",
                rootUuid, playlistUuid, pplVersionId, titleUuid, segmentAssociationUuid);

        Calendar cal = Calendar.getInstance();
        Integer day = TestUtil.day.get();
        if (day != null) {
            cal.set(Calendar.DATE, cal.get(Calendar.DATE) + day);
        }
        createSplitByWeek(rootUuid, playlistUuid, pplVersionId, titleUuid, segmentAssociationUuid,
                cal, null);
    }

    private void createSplitByWeek(String rootUuid, String playlistUuid, String pplVersionId,
                                   String titleUuid,
                                   String segmentAssociationUuid, Calendar cal, List<SegmentSplitInfo> segmentSplitInfos) {
        setDateToFirstDayOfWeek(cal);
        deleteSegmentSplitTreeByParentNodeUuid(rootUuid);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance("yyyy/MM/dd");

        SegmentSplitDTO segmentSplitDTO = new SegmentSplitDTO();
        segmentSplitDTO.setPlaylistUuid(playlistUuid);
        segmentSplitDTO.setAutoGroup(true);
        segmentSplitDTO.setParentUuid(rootUuid);
        segmentSplitDTO.setDefaultGroup(false);
        segmentSplitDTO.setPplVersionUuid(pplVersionId);
        segmentSplitDTO.setTitleUuid(titleUuid);
        segmentSplitDTO.setSegmentAssociationUuid(segmentAssociationUuid);
        int week = cal.get(Calendar.WEEK_OF_YEAR);
        segmentSplitDTO.setSplitTitle(buildTitle(week));
        segmentSplitDTO.setSplitType(SegmentSplitTypeEnum.DATE.getName());
        segmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
        Map<String, String> map = new HashMap<>();
        map.put("start", fastDateFormat.format(cal.getTime()));
        cal.set(Calendar.DATE, cal.get(Calendar.DATE) + 6);
        map.put("end", fastDateFormat.format(cal.getTime()));
        segmentSplitDTO.setSplitRule("[" + JSON.toJSONString(map) + "]");
        if (CollectionUtils.isNotEmpty(segmentSplitInfos)) {
            segmentSplitInfos.stream().filter(segmentSplitInfo ->
                    Objects.equals(segmentSplitDTO.getSplitTitle(),
                            segmentSplitInfo.getSplitTitle())
            ).findFirst().ifPresent(splitInfo -> segmentSplitDTO
                    .setContentList(splitInfo.getContentList().stream().map(this::toContentDTO)
                            .collect(Collectors.toList())));
        }
        createSegmentSplit(segmentSplitDTO);

        SegmentSplitDTO segmentSplitDTO1 = new SegmentSplitDTO();
        segmentSplitDTO1.setPlaylistUuid(playlistUuid);
        segmentSplitDTO1.setAutoGroup(true);
        segmentSplitDTO1.setParentUuid(rootUuid);
        segmentSplitDTO1.setDefaultGroup(false);
        segmentSplitDTO1.setPplVersionUuid(pplVersionId);
        segmentSplitDTO1.setTitleUuid(titleUuid);
        segmentSplitDTO1.setSegmentAssociationUuid(segmentAssociationUuid);
        cal.set(Calendar.DATE, cal.get(Calendar.DATE) + 1);
        week = cal.get(Calendar.WEEK_OF_YEAR);
        segmentSplitDTO1.setSplitTitle(buildTitle(week));
        segmentSplitDTO1.setSplitType(SegmentSplitTypeEnum.DATE.getName());
        segmentSplitDTO1.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
        map.put("start", fastDateFormat.format(cal.getTime()));
        cal.set(Calendar.DATE, cal.get(Calendar.DATE) + 6);
        map.put("end", fastDateFormat.format(cal.getTime()));
        segmentSplitDTO1.setSplitRule("[" + JSON.toJSONString(map) + "]");
        if (CollectionUtils.isNotEmpty(segmentSplitInfos)) {
            segmentSplitInfos.stream().filter(segmentSplitInfo ->
                    Objects.equals(segmentSplitDTO1.getSplitTitle(),
                            segmentSplitInfo.getSplitTitle())
            ).findFirst().ifPresent(splitInfo -> segmentSplitDTO1
                    .setContentList(splitInfo.getContentList().stream().map(this::toContentDTO)
                            .collect(Collectors.toList())));
        }
        createSegmentSplit(segmentSplitDTO1);

        SegmentSplitDTO defaultSegmentSplitDTO = new SegmentSplitDTO();
        defaultSegmentSplitDTO.setPlaylistUuid(playlistUuid);
        defaultSegmentSplitDTO.setAutoGroup(true);
        defaultSegmentSplitDTO.setParentUuid(rootUuid);
        defaultSegmentSplitDTO.setDefaultGroup(true);
        defaultSegmentSplitDTO.setPplVersionUuid(pplVersionId);
        defaultSegmentSplitDTO.setTitleUuid(titleUuid);
        defaultSegmentSplitDTO.setSegmentAssociationUuid(segmentAssociationUuid);
        defaultSegmentSplitDTO.setSplitTitle("DEFAULT");
        defaultSegmentSplitDTO.setSplitType(SegmentSplitTypeEnum.DATE.getName());
        defaultSegmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
        if (CollectionUtils.isNotEmpty(segmentSplitInfos)) {
            segmentSplitInfos.stream().filter(segmentSplitInfo ->
                    Objects.equals(defaultSegmentSplitDTO.getSplitTitle(),
                            segmentSplitInfo.getSplitTitle())
            ).findFirst().ifPresent(splitInfo -> defaultSegmentSplitDTO
                    .setContentList(splitInfo.getContentList().stream().map(this::toContentDTO)
                            .collect(Collectors.toList())));
        }
        createSegmentSplit(defaultSegmentSplitDTO);
    }

    private String buildTitle(Integer week) {
        return "WEEK " + week;
    }

    private void setDateToFirstDayOfWeek(Calendar cal) {
        int startDay = Calendar.SUNDAY;
        try {
            Integer i = iOrgFacadeClient.getDayOfWeek(OrgUtil.orgContenter.get());
            if (!Objects.isNull(i)) {
                startDay = i + 1;
            }
        } catch (Exception e) {
            logger.error("get day of week error.orgId:<{}>", OrgUtil.orgContenter.get());
            logger.error("get day of week error.", e);
        }
        cal.setFirstDayOfWeek(startDay);
        int dayOfWeek = cal.get(Calendar.DAY_OF_WEEK);
        if (dayOfWeek >= startDay) {
            cal.set(Calendar.DATE, cal.get(Calendar.DATE) - (dayOfWeek - startDay));
        } else {
            cal.set(Calendar.DATE, cal.get(Calendar.DATE) - (7 - (startDay - dayOfWeek)));
        }
    }

    @Override
    public void scanAndPublish(Calendar baseCal) {
        logger.info("scan and publish segmentSplit of SplitWeek Segment");
        List<SegmentInfo> splitByWeekSegmentInfos = iSegmentViewService.getAllSplitWeekSegment();
        splitByWeekSegmentInfos.forEach(segmentInfo -> {
            OrgUtil.orgContenter.set(segmentInfo.getOrganizationId());
            List<PlaylistVersionContentAssociationDO> associationDOS = contentViewService
                    .getByContentId(segmentInfo.getUuid());
            if (SegmentTypeEnum.PLAYLIST_SEGMENT.getName().equals(segmentInfo.getContentKind())) {
                associationDOS.stream().map(PlaylistVersionContentAssociationDO::getUuid).distinct()
                        .forEach(s -> {
                            List<SegmentSplitInfo> segmentSplitInfosList = iSegmentSplitViewService
                                    .getSegmentSplitList(s, null, null);

                            Map<String, List<SegmentSplitInfo>> map1 = segmentSplitInfosList
                                    .stream().collect(Collectors.groupingBy(
                                            SegmentSplitInfo::getSegmentAssociationUuid,
                                            Collectors.toList()));
                            map1.forEach((s2, segmentSplitInfos) -> {

                                Calendar cal = Calendar.getInstance();
                                cal.setTime(baseCal.getTime());

                                if (hasSplitWeekRelease(segmentSplitInfos)) {
                                    publish(s2, segmentSplitInfos, cal);
                                } else {
                                    if (CollectionUtils.isNotEmpty(segmentSplitInfos)) {
                                        SegmentSplitInfo root = getRootSegmentInfo(
                                                segmentSplitInfos);
                                        recreateSplitByWeek(
                                                getDraftCurrWeek(segmentSplitInfos, cal),
                                                root.getUuid(),
                                                root.getPlaylistUuid(),
                                                root.getPplVersionUuid(),
                                                root.getTitleUuid(),
                                                root.getSegmentAssociationUuid(),
                                                cal
                                        );
                                    }
                                }
                            });
                        });
            } else if (SegmentTypeEnum.BASE_SEGMENT.getName()
                    .equals(segmentInfo.getContentKind())) {
                List<SegmentSplitInfo> segmentSplitInfosList = iSegmentSplitViewService
                        .getSegmentSplitList(segmentInfo.getUuid(), null, null);

                Map<String, List<SegmentSplitInfo>> map1 = segmentSplitInfosList
                        .stream().collect(Collectors.groupingBy(
                                SegmentSplitInfo::getSegmentAssociationUuid,
                                Collectors.toList()));
                map1.forEach((s2, segmentSplitInfos) -> {

                    Calendar cal = Calendar.getInstance();
                    cal.setTime(baseCal.getTime());

                    if (hasSplitWeekRelease(segmentSplitInfos)) {
                        publish(s2, segmentSplitInfos, cal);
                    } else {
                        if (CollectionUtils.isNotEmpty(segmentSplitInfos)) {
                            SegmentSplitInfo root = getRootSegmentInfo(
                                    segmentSplitInfos);
                            recreateSplitByWeek(
                                    getDraftCurrWeek(segmentSplitInfos, cal),
                                    root.getUuid(),
                                    root.getPlaylistUuid(),
                                    root.getPplVersionUuid(),
                                    root.getTitleUuid(),
                                    root.getSegmentAssociationUuid(),
                                    cal
                            );
                        }
                    }
                });
            } else if (SegmentTypeEnum.TITLE_SEGMENT.getName().equals(segmentInfo.getContentKind()) ||
                    SegmentTypeEnum.RATING_SEGMENT.getName().equals(segmentInfo.getContentKind())) {
                associationDOS.stream().map(PlaylistVersionContentAssociationDO::getUuid).distinct()
                        .forEach(s -> {
                            List<SegmentSplitInfo> segmentSplitInfosList = iSegmentSplitViewService
                                    .getSegmentSplitList(s, null, null).stream()
                                    .filter(segmentSplitInfo -> segmentSplitInfo.getTitleUuid() != null)
                                    .collect(Collectors.toList());
                            Map<String, List<SegmentSplitInfo>> map1 = segmentSplitInfosList
                                    .stream().collect(Collectors.groupingBy(
                                            SegmentSplitInfo::getSegmentAssociationUuid,
                                            Collectors.toList()));
                            map1.forEach((s2, segmentSplitInfos) -> {
                                Map<String, List<SegmentSplitInfo>> map2 = segmentSplitInfos
                                        .stream().collect(Collectors.groupingBy(
                                                SegmentSplitInfo::getTitleUuid,
                                                Collectors.toList()));
                                map2.forEach((s1, segmentSplitInfos1) -> {

                                    Calendar cal = Calendar.getInstance();
                                    cal.setTime(baseCal.getTime());

                                    if (hasSplitWeekRelease(segmentSplitInfos1)) {
                                        publish(s2, segmentSplitInfos1, cal);
                                    } else {
                                        if (CollectionUtils.isNotEmpty(segmentSplitInfos)) {
                                            SegmentSplitInfo root = getRootSegmentInfo(
                                                    segmentSplitInfos);
                                            recreateSplitByWeek(
                                                    getDraftCurrWeek(segmentSplitInfos, cal),
                                                    root.getUuid(),
                                                    root.getPlaylistUuid(),
                                                    root.getPplVersionUuid(),
                                                    root.getTitleUuid(),
                                                    root.getSegmentAssociationUuid(),
                                                    cal
                                            );
                                        }
                                    }
                                });
                            });

                        });
            }
        });
    }

    private SegmentSplitInfo getRootSegmentInfo(List<SegmentSplitInfo> segmentSplitInfosList) {
        return segmentSplitInfosList.stream()
                .filter(segmentSplitInfo -> Objects.isNull(segmentSplitInfo.getParentUuid()))
                .findAny().orElse(null);
    }

    public void publish(String associationUuid, List<SegmentSplitInfo> segmentSplitInfosList,
                        Calendar cal) {
        try {
            int week = getHadPublishWeek(segmentSplitInfosList, cal);
            int weekOfYear = cal.get(Calendar.WEEK_OF_YEAR);
            if (weekOfYear != week) {
                PublishDTO publishDTO = new PublishDTO();
                publishDTO.setPublishLater(false);
                publishDTO.setDeletePreWeek(true);
                publishDTO.setPublishCal(cal);
                publishSegmentSplitByAssociation(associationUuid, null, publishDTO);
            }
        } catch (Exception e) {
            logger.error("auto publish split of week error.associationUuid:<{}>", associationUuid);
            logger.error("auto publish split of week error.", e);
        }
    }

    private Integer getHadPublishWeek(List<SegmentSplitInfo> segmentSplitInfos, Calendar cal) {
        SegmentSplitInfo info = segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> SegmentStatusEnum.RELEASE.getStatusStr()
                        .equals(segmentSplitInfo.getStatus()))
                .filter(segmentSplitInfo -> !segmentSplitInfo.getDefaultGroup())
                .filter(segmentSplitInfo -> StringUtils
                        .isNotEmpty(segmentSplitInfo.getParentUuid()))
                .filter(segmentSplitInfo -> Boolean.TRUE.equals(segmentSplitInfo.getAutoGroup()))
                .min(Comparator.comparing(SegmentSplitInfo::getSplitRule)).orElse(null);
        if (info == null) {
            return cal.get(Calendar.WEEK_OF_YEAR) - 1;
        }
        return getWeekFromTitle(info.getSplitTitle());
    }


    private Integer getDraftCurrWeek(List<SegmentSplitInfo> segmentSplitInfos, Calendar cal) {
        SegmentSplitInfo info = segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> SegmentStatusEnum.DRAFT.getStatusStr()
                        .equals(segmentSplitInfo.getStatus()))
                .filter(segmentSplitInfo -> !segmentSplitInfo.getDefaultGroup())
                .filter(segmentSplitInfo -> StringUtils
                        .isNotEmpty(segmentSplitInfo.getParentUuid()))
                .filter(segmentSplitInfo -> Boolean.TRUE.equals(segmentSplitInfo.getAutoGroup()))
                .min(Comparator.comparing(SegmentSplitInfo::getSplitRule)).orElse(null);
        if (info == null) {
            return cal.get(Calendar.WEEK_OF_YEAR) - 1;
        }
        return getWeekFromTitle(info.getSplitTitle());
    }

    private void recreateSplitByWeek(int draftCurrWeek, String rootUuid, String playlistUuid,
                                     String pplVersionId,
                                     String titleUuid,
                                     String segmentAssociationUuid, Calendar cal) {
        try {
            int weekOfYear = cal.get(Calendar.WEEK_OF_YEAR);
            if (weekOfYear != draftCurrWeek) {
                createSplitByWeek(rootUuid, playlistUuid,
                        pplVersionId, titleUuid,
                        segmentAssociationUuid, cal, null);
            }
        } catch (Exception e) {
            logger.error("auto recreate split of week error.associationUuid:<{}>",
                    segmentAssociationUuid);
            logger.error("auto recreate  split of week error.", e);
        }
    }

    private Integer getWeekFromTitle(String title) {
        return Integer.parseInt(title.substring(5));
    }

    private Boolean hasSplitWeekRelease(List<SegmentSplitInfo> segmentSplitInfos) {
        return segmentSplitInfos.stream().anyMatch(
                segmentSplitInfo -> SegmentStatusEnum.RELEASE.getStatusStr()
                        .equals(segmentSplitInfo.getStatus()) && Boolean.TRUE
                        .equals(segmentSplitInfo.getAutoGroup()));
    }


    private void copySegmentSplit(String segmentAssociationUuid, SegmentSplitInfo segmentSplitInfo,
                                  String pid, String pplVersion) {
        SegmentSplitDTO segmentSplitDTO = new SegmentSplitDTO();
        segmentSplitDTO.setDefaultGroup(segmentSplitInfo.getDefaultGroup());
        segmentSplitDTO.setAutoGroup(segmentSplitInfo.getAutoGroup());
        segmentSplitDTO.setSplitRule(segmentSplitInfo.getSplitRule());
        segmentSplitDTO.setSplitTitle(segmentSplitInfo.getSplitTitle());
        segmentSplitDTO.setSegmentAssociationUuid(segmentAssociationUuid);
        segmentSplitDTO.setPplVersionUuid(pplVersion);
        segmentSplitDTO.setPlaylistUuid(segmentSplitInfo.getPlaylistUuid());
        segmentSplitDTO.setParentUuid(pid);
        segmentSplitDTO.setSplitType(segmentSplitInfo.getSplitType());
        segmentSplitDTO.setTitleUuid(segmentSplitInfo.getTitleUuid());
        segmentSplitDTO.setStatus(segmentSplitInfo.getStatus());
        segmentSplitDTO.setUserGroup(segmentSplitInfo.getUserGroup());
        segmentSplitDTO.setContentList(segmentSplitInfo.getContentList()
                .stream().map(this::toContentDTO).collect(Collectors.toList()));
        String uuid = createSegmentSplit(segmentSplitDTO);
        segmentSplitInfo.getChildren().forEach(segmentSplitInfo1 ->
                copySegmentSplit(segmentAssociationUuid, segmentSplitInfo1, uuid, pplVersion));
    }

    private ContentDTO toContentDTO(ContentInfo contentInfo) {
        ContentDTO contentDTO = new ContentDTO();
        contentDTO.setContentAssociationUuid(contentInfo.getContentAssociationUuid());
        contentDTO.setExtension(contentInfo.getExtension());
        contentDTO.setTitle(contentInfo.getTitle());
        contentDTO.setContentId(contentInfo.getContentId());
        contentDTO.setContentType(contentInfo.getContentType());
        return contentDTO;
    }


    private void createTitleSegmentSplit(TitleInfo titleInfo, String associationUuid,
                                         String playlistUuid,
                                         String pplVersionUuid, List<ShowAttributeGroupInfo> showAttributeGroups,
                                         String segmentType, Boolean multiSegment, Boolean splitByWeek) {
        logger.info("create title segment split. associationUuid:<{}> titleUuid:<{}>",
                associationUuid, playlistUuid);
        List<CplInfo> titleCplInfos = titleInfo.getCplDTOList();

        SegmentSplitDTO titleTypeSegmentSplitDTO = buildSegmentDTO(associationUuid, playlistUuid,
                pplVersionUuid);
        titleTypeSegmentSplitDTO.setTitleUuid(titleInfo.getUuid());
        titleTypeSegmentSplitDTO.setSplitTitle(titleInfo.getName());
        titleTypeSegmentSplitDTO.setSplitType(SegmentSplitTypeEnum.TITLE.getName());
        titleTypeSegmentSplitDTO.setSplitRule(titleInfo.getUuid());
        titleTypeSegmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());

        if (SegmentTypeEnum.AUTOMATIC_SEGMENT.getName().equals(segmentType) && !multiSegment) {
            titleTypeSegmentSplitDTO.setContentList(
                    titleCplInfos.stream().map(this::buildFeatureContent)
                            .collect(Collectors.toList()));
        }

        String titleTypeSegmentSplitUuid = createOrUpdateSegmentSplit(titleTypeSegmentSplitDTO);

        if (!SegmentTypeEnum.AUTOMATIC_SEGMENT.getName().equals(segmentType) && Boolean.TRUE
                .equals(splitByWeek)) {
            List<SegmentSplitInfo> segmentSplitInfos = iSegmentSplitViewService
                    .getSegmentSplitList(associationUuid, titleInfo.getUuid(),
                            SegmentStatusEnum.DRAFT.getStatus());
            Calendar cal = Calendar.getInstance();
            Integer day = TestUtil.day.get();
            if (day != null) {
                cal.set(Calendar.DATE, cal.get(Calendar.DATE) + day);
            }
            int week = getDraftCurrWeek(segmentSplitInfos, cal);
            int weekOfYear = cal.get(Calendar.WEEK_OF_YEAR);
            if (week != weekOfYear) {
                createSplitByWeek(titleTypeSegmentSplitUuid, playlistUuid, pplVersionUuid,
                        titleInfo.getUuid(),
                        associationUuid);
            }
        }

        if (SegmentTypeEnum.TITLE_SEGMENT.getName().equals(segmentType) ||
                SegmentTypeEnum.RATING_SEGMENT.getName().equals(segmentType)) {
            return;
        }

        titleInfo.getShowAttrs().forEach(group -> createGroupTypeSegmentSplit(
                showAttributeGroups,
                titleInfo,
                associationUuid,
                playlistUuid,
                pplVersionUuid,
                group,
                titleTypeSegmentSplitUuid,
                multiSegment
        ));

    }

    private void createGroupTypeSegmentSplit(
            List<ShowAttributeGroupInfo> showAttributeGroups,
            TitleInfo titleInfo,
            String associationUuid,
            String playlistUuid,
            String pplVersionUuid,
            String group,
            String titleTypeSegmentSplitUuid,
            boolean multiSegment) {
        logger.info(
                "create show type segment split. associationUuid:<{}> titleUuid:<{}>,showAttributeGroups:<{}>",
                associationUuid, playlistUuid,
                JSON.toJSONString(showAttributeGroups));
        List<String> showAttributes = showAttributeGroups.stream().collect(
                Collectors.toMap(ShowAttributeGroupInfo::getName,
                        ShowAttributeGroupInfo::getAttributes))
                .get(group);
        List<CplInfo> groupCplInfos = matchCplInfosByShowAttributes(titleInfo.getCplDTOList(),
                showAttributes);

        SegmentSplitDTO groupTypeSegmentSplitDTO = buildSegmentDTO(associationUuid, playlistUuid,
                pplVersionUuid);
        groupTypeSegmentSplitDTO.setTitleUuid(titleInfo.getUuid());
        groupTypeSegmentSplitDTO.setSplitTitle(group);
        groupTypeSegmentSplitDTO.setParentUuid(titleTypeSegmentSplitUuid);
        groupTypeSegmentSplitDTO.setSplitType(SegmentSplitTypeEnum.SHOW_ATTR.getName());
        groupTypeSegmentSplitDTO.setSplitRule(
                JSON.toJSONString(showAttributeGroups.stream()
                        .filter(showAttributeGroupInfo -> showAttributeGroupInfo.getName()
                                .equals(group))
                        .map(ShowAttributeGroupInfo::getAttributes)
                        .collect(Collectors.toList()))
        );

        groupTypeSegmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());

        if (!multiSegment) {
            groupTypeSegmentSplitDTO.setContentList(
                    groupCplInfos.stream().map(this::buildFeatureContent)
                            .collect(Collectors.toList()));
        }

        String groupTypeSegmentSplitUuid = createOrUpdateSegmentSplit(groupTypeSegmentSplitDTO);

        if (groupCplInfos.isEmpty()) {
            return;
        }

        if (multiSegment) {
            return;
        }

        groupCplInfos.forEach(cplInfo -> createCplSegmentSplit(
                titleInfo,
                associationUuid,
                playlistUuid,
                pplVersionUuid,
                groupTypeSegmentSplitUuid,
                cplInfo
        ));

    }

    private List<CplInfo> matchCplInfosByShowAttributes(List<CplInfo> cplInfos,
                                                        List<String> showAttributes) {
        return cplInfos.stream().filter(cplInfo -> cplInfo.getFormatList().containsAll(
                showAttributes.stream()
                        .filter(s -> Lists.newArrayList(CONTRAST_SHOW_ATTRIBUTE).contains(s))
                        .collect(Collectors.toList()))).collect(Collectors.toList());
    }

    private void createCplSegmentSplit(
            TitleInfo titleInfo,
            String associationUuid,
            String playlistUuid,
            String pplVersionUuid,
            String groupTypeSegmentSplitUuid,
            CplInfo cplInfo) {
        logger.info(
                "create cpl segment split. associationUuid:<{}> titleUuid:<{}>,cplInfo:<{}>",
                associationUuid, playlistUuid,
                JSON.toJSONString(cplInfo));
        SegmentSplitDTO cplTypeSegmentSplitDTO = buildSegmentDTO(associationUuid, playlistUuid,
                pplVersionUuid);
        cplTypeSegmentSplitDTO.setTitleUuid(titleInfo.getUuid());
        cplTypeSegmentSplitDTO.setSplitTitle(String.join("_", cplInfo.getFormatList()));
        cplTypeSegmentSplitDTO.setParentUuid(groupTypeSegmentSplitUuid);
        cplTypeSegmentSplitDTO.setSplitType(SegmentSplitTypeEnum.CPL_FORMAT.getName());
        cplTypeSegmentSplitDTO.setSplitRule(JSON.toJSONString(cplInfo));
        cplTypeSegmentSplitDTO.setContentList(Lists.newArrayList(buildFeatureContent(cplInfo)));
        cplTypeSegmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
        createOrUpdateSegmentSplit(cplTypeSegmentSplitDTO);
    }

    private ContentDTO buildFeatureContent(CplInfo cplInfo) {
        ContentDTO contentDTO = new ContentDTO();
        contentDTO.setContentId(cplInfo.getUuid());
        contentDTO.setTitle(cplInfo.getTitle());
        contentDTO.setContentType(ContentTypeEnum.CPL.getName());
        contentDTO.setExtension(cplInfo.getExtension());
        return contentDTO;
    }

    private SegmentSplitDTO buildSegmentDTO(String associationUuid, String playlistUuid,
                                            String pplVersionUuid) {
        SegmentSplitDTO segmentSplitDTO = new SegmentSplitDTO();
        segmentSplitDTO.setPlaylistUuid(playlistUuid);
        segmentSplitDTO.setPplVersionUuid(pplVersionUuid);
        segmentSplitDTO.setSegmentAssociationUuid(associationUuid);
        segmentSplitDTO.setAutoGroup(true);
        return segmentSplitDTO;
    }

    private List<TitleInfo> buildTitleList(List<String> showAttributeNames,
                                           Set<String> titleUuids) {
        List<TitleInfo> titleInfos = titleFacadeClient.geTitlesByUuids(titleUuids,
                iOrgFacadeClient.getComplexGroupUuid(OrgUtil.orgContenter.get()));

        Set<String> cplUuids = titleInfos.stream().map(TitleInfo::getCplUuids)
                .flatMap(Collection::stream).collect(Collectors.toSet());
        List<CplInfo> cplInfos = iCplFacadeClient
                .getCplInfoByUuids(cplUuids, OrgUtil.orgContenter.get());

        titleInfos.forEach(titleInfo -> {
            titleInfo.setShowAttrs(showAttributeNames);
            titleInfo.setCplDTOList(cplInfos.stream().filter(
                    cplInfo -> titleInfo.getCplUuids().contains(cplInfo.getUuid())
            ).collect(Collectors.toList()));
        });

        return titleInfos;
    }

    @Override
    @Transactional
    public String createSegmentSplitDraft(String associationUuid, String titleUuid, Boolean copy) {
        logger.info("create a segment split draft.associationUuid:<{}> titleUuid:<{}> copy:<{}>",
                associationUuid, titleUuid, copy);
        //先删除草稿
        deleteDraftByAssociationUuid(associationUuid, titleUuid);

        List<SegmentSplitInfo> segmentSplitInfos = iSegmentSplitViewService
                .getSegmentSplitTree(associationUuid,
                        titleUuid, SegmentStatusEnum.RELEASE.getStatus());
        if (segmentSplitInfos.isEmpty()) {
            return null;
        }
        AtomicReference<String> root = new AtomicReference<>();
        segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> StringUtils.isBlank(segmentSplitInfo.getParentUuid()))
                .forEach(segmentSplitInfo -> {
                    root.set(createSegmentSplitDraft(segmentSplitInfo, null, copy));
                });
        return root.get();
    }


    private String createSegmentSplitDraft(SegmentSplitInfo segmentSplitInfo, String pid,
                                           Boolean copy) {
        SegmentSplitDTO segmentSplitDTO = new SegmentSplitDTO();
        segmentSplitDTO.setDefaultGroup(segmentSplitInfo.getDefaultGroup());
        segmentSplitDTO.setAutoGroup(segmentSplitInfo.getAutoGroup());
        segmentSplitDTO.setSplitRule(segmentSplitInfo.getSplitRule());
        segmentSplitDTO.setSplitTitle(segmentSplitInfo.getSplitTitle());
        segmentSplitDTO.setSegmentAssociationUuid(segmentSplitInfo.getSegmentAssociationUuid());
        segmentSplitDTO.setPplVersionUuid(segmentSplitInfo.getPplVersionUuid());
        segmentSplitDTO.setPlaylistUuid(segmentSplitInfo.getPlaylistUuid());
        segmentSplitDTO.setParentUuid(pid);
        segmentSplitDTO.setSplitType(segmentSplitInfo.getSplitType());
        segmentSplitDTO.setTitleUuid(segmentSplitInfo.getTitleUuid());
        segmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
        segmentSplitDTO.setUserGroup(segmentSplitInfo.getUserGroup());

        if (!Boolean.FALSE.equals(copy)) {
            segmentSplitDTO.setContentList(
                    segmentSplitInfo.getContentList().stream().map(this::toContentDTO)
                            .collect(Collectors.toList()));
        }

        String uuid = createSegmentSplit(segmentSplitDTO);

        if (!Boolean.FALSE.equals(copy)) {
            segmentSplitInfo.getChildren()
                    .forEach(segmentSplitInfo1 -> createSegmentSplitDraft(segmentSplitInfo1, uuid,
                            copy));
        }
        return uuid;
    }


    @Override
    @Transactional
    public void createSegmentSplit(SegmentSplitOptionDTO segmentSplitOptionDTO) {
        logger.info("create segment split. detail:<{}>", JSON.toJSONString(segmentSplitOptionDTO));
        SegmentSplitDTO right = segmentSplitOptionDTO.getGroupRight();
        SegmentSplitDTO left = segmentSplitOptionDTO.getGroupLeft();
        if (Objects.equals(right.getSplitTitle(), left.getSplitTitle())) {
            throw new BizException(ResultCodeEnum.REPETITION_OF_SEGMENT_SPLIT_TITLE);
        }
        right.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
        left.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());

        left.setAutoGroup(false);
        right.setAutoGroup(false);

        if (StringUtils.isNotEmpty(left.getUuid())) {
            updateSegmentSplit(left.getUuid(), left);
        } else {
            left.setDefaultGroup(true);
            createSegmentSplit(left);
        }
        createSegmentSplit(right);
        checkTitle(left.getParentUuid());
        checkShowAttr(right.getSegmentAssociationUuid(),
                right.getTitleUuid());

        Set<String> playlistUuids = getPlaylistUuids(right.getPlaylistUuid(),
                right.getSegmentAssociationUuid());
        playlistUuids.forEach(playlistUuid -> reportContentNotSelectTask(playlistUuid,
                right.getSegmentAssociationUuid(),
                right.getTitleUuid()));
    }

    private void checkShowAttr(String segmentAssociationUuid, String titleUuid) {
        List<SegmentSplitInfo> segmentSplitInfoList = iSegmentSplitViewService
                .getSegmentSplitList(segmentAssociationUuid,
                        titleUuid, SegmentStatusEnum.DRAFT.getStatus());

        List<List<String>> list = segmentSplitInfoList.stream()
                .filter(segmentSplitInfo -> SegmentSplitTypeEnum.SHOW_ATTR.getName()
                        .equals(segmentSplitInfo.getSplitType())).flatMap(segmentSplitInfo -> {
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
                    return showTypes.stream();
                }).collect(Collectors.toList());

        Set<String> distList = list.stream()
                .map(strings -> strings.stream().sorted().collect(Collectors.joining("|"))).collect(
                        Collectors.toSet());

        if (list.size() != distList.size()) {
            throw new BizException(ResultCodeEnum.SHOW_TAGS_ALREADY_EXIST);
        }
    }


    @Override
    @Transactional
    public void updateSegmentSplit(SegmentSplitOptionDTO segmentSplitOptionDTO) {
        logger.info("update segment split. detail:<{}>", JSON.toJSONString(segmentSplitOptionDTO));
        SegmentSplitDTO left = segmentSplitOptionDTO.getGroupLeft();
        SegmentSplitDTO right = segmentSplitOptionDTO.getGroupRight();
        if (Objects.equals(right.getSplitTitle(), left.getSplitTitle())) {
            throw new BizException(ResultCodeEnum.REPETITION_OF_SEGMENT_SPLIT_TITLE);
        }
        iSegmentSplitService
                .updateSegmentSplit(left.getUuid(), segmentSplitOptionDTO.getGroupLeft());
        iSegmentSplitService
                .updateSegmentSplit(right.getUuid(), segmentSplitOptionDTO.getGroupRight());
        checkTitle(left.getParentUuid());

        Set<String> playlistUuids = getPlaylistUuids(right.getPlaylistUuid(),
                right.getSegmentAssociationUuid());
        playlistUuids.forEach(playlistUuid -> reportContentNotSelectTask(playlistUuid,
                right.getSegmentAssociationUuid(),
                right.getTitleUuid()));
    }

    private void checkTitle(String nodeUuid) {
        String rootNodeUuid = findRootNodeUuid(nodeUuid);
        List<SegmentSplitDO> segmentSplitDOS = loadTreeByRootNodeUuid(rootNodeUuid);
        if (segmentSplitDOS.stream().map(SegmentSplitDO::getSplitTitle).distinct().count()
                != segmentSplitDOS.size()) {
            throw new BizException(ResultCodeEnum.REPETITION_OF_SEGMENT_SPLIT_TITLE);
        }
    }

    private String findRootNodeUuid(String nodeUuid) {
        SegmentSplitDO segmentSplitDO = iSegmentSplitService.getById(nodeUuid);
        if (segmentSplitDO.getParentUuid() == null) {
            return segmentSplitDO.getUuid();
        } else {
            return findRootNodeUuid(segmentSplitDO.getParentUuid());
        }
    }

    private List<SegmentSplitDO> loadTreeByRootNodeUuid(String rootNodeUuid) {
        List<SegmentSplitDO> segmentSplitDOS = new ArrayList<>();
        Stack<String> splitDOStack = new Stack<>();
        splitDOStack.push(rootNodeUuid);
        while (!splitDOStack.isEmpty()) {
            SegmentSplitDO segmentSplitDO = iSegmentSplitService.getById(splitDOStack.pop());
            segmentSplitDOS.add(segmentSplitDO);
            QueryWrapper<SegmentSplitDO> wrapper = new QueryWrapper<>();
            wrapper.eq("parent_uuid", segmentSplitDO.getUuid());
            iSegmentSplitService.list(wrapper)
                    .forEach(segmentSplitDO1 -> splitDOStack.push(segmentSplitDO1.getUuid()));
        }
        return segmentSplitDOS;
    }


    @Override
    @Transactional
    public void deleteSegmentSplit(String uuid) {
        logger.info("delete segment split. uuid:<{}>", uuid);
        SegmentSplitDO segmentSplitDO = iSegmentSplitService.getById(uuid);
        if (segmentSplitDO == null) {
            return;
        }
        deleteSegmentSplitTreeByNodeUuid(uuid);
        if (segmentSplitDO.getParentUuid() != null) {
            List<SegmentSplitDO> list = iSegmentSplitService
                    .listByParentUuid(segmentSplitDO.getParentUuid());
            if (list.size() == 1 && Boolean.TRUE.equals(list.get(0).getDefaultGroup())) {
                deleteSegmentSplitTreeByNodeUuid(list.get(0).getUuid());
            }
        }
    }


    @Transactional
    @Override
    public void updateSegmentSplitContent(Map<String, List<ContentDTO>> contentMap) {
        logger.info("update content of segment split. content:<{}>", JSON.toJSONString(contentMap));
        AtomicReference<String> segmentSplitUuid = new AtomicReference<>();
        contentMap.forEach((s, contentDTOS) -> {
            segmentSplitUuid.set(s);
            if (StringUtils.isBlank(s)) {
                return;
            }
            boolean b = contentDTOS.stream().anyMatch(contentDTO ->
                    ContentTypeEnum.SEGMENT.getName().equals(contentDTO.getContentType()));
            if (b) {
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID,
                        "Segment content type exception.");
            }
            SegmentSplitDO segmentSplitDO = iSegmentSplitService.getById(s);
            iSegmentSplitService.updateById(segmentSplitDO);
            segmentContentService.deleteAssociation(s);
            segmentContentService.batchInsertAssociation(s, contentDTOS);
        });
        if (segmentSplitUuid.get() != null) {
            PlaylistSegmentSplitAssociationDO associationDO = splitAssociationService
                    .getSegmentSplitBySegmentSplitUuid(segmentSplitUuid.get());

            Set<String> playlistUuids = getPlaylistUuids(associationDO.getPlaylistUuid(),
                    associationDO.getSegmentAssociationUuid());
            playlistUuids.forEach(playlistUuid -> {
                reportContentNotSelectTask(playlistUuid,
                        associationDO.getSegmentAssociationUuid(),
                        associationDO.getTitleId());
            });
        }
    }

    @Override
    @Transactional
    public void deleteDraftByAssociationUuid(String contentAssociationUuid, String titleUuid) {
        logger.info("update draft of segment split. contentAssociationUuid:<{}> titleUuid:<{}>",
                contentAssociationUuid, titleUuid);
        deleteByAssociationUuid(contentAssociationUuid, titleUuid, SegmentStatusEnum.DRAFT);
    }

    private void deleteReleaseByAssociationUuid(String contentAssociationUuid, String titleUuid) {
        deleteByAssociationUuid(contentAssociationUuid, titleUuid, SegmentStatusEnum.RELEASE);
    }

    private void deleteByAssociationUuid(String contentAssociationUuid, String titleUuid,
                                         SegmentStatusEnum statusEnum) {
        List<PlaylistSegmentSplitAssociationDO> splitAssociationDOS = splitAssociationService
                .getSegmentSplitByContentAssociationUuid(contentAssociationUuid, titleUuid);
        if (splitAssociationDOS.isEmpty()) {
            return;
        }
        List<String> uuids = splitAssociationDOS.stream().map(
                PlaylistSegmentSplitAssociationDO::getSegmentSplitUuid).collect(
                Collectors.toList());
        List<SegmentSplitDO> segmentSplitDOS = iSegmentSplitService.getByIds(uuids);
        uuids = segmentSplitDOS.stream().filter(segmentSplitDO -> segmentSplitDO.getStatus()
                .equals(statusEnum.getStatus()))
                .map(SegmentSplitDO::getUuid).collect(
                        Collectors.toList());
        if (uuids.isEmpty()) {
            return;
        }
        iSegmentSplitService.removeByIds(uuids);
        segmentContentService.batchDeleteAssociation(uuids);
        splitAssociationService.batchDeleteAssociation(uuids);
    }

    @Override
    @Transactional
    public void publishSegmentSplitByAssociation(String contentAssociationUuid, String titleUuid,
                                                 PublishDTO publishDTO) {
        logger.info(
                "publish segment split. contentAssociationUuid:<{}> titleUuid:<{}> publishDTO:<{}>",
                contentAssociationUuid, titleUuid, JSON.toJSONString(publishDTO));
        List<SegmentSplitInfo> segmentSplitInfos = iSegmentSplitViewService
                .getSegmentSplitTree(contentAssociationUuid, titleUuid,
                        SegmentStatusEnum.DRAFT.getStatus());

        if (segmentSplitInfos.isEmpty()) {
            createDefaultSegmentSplit(contentAssociationUuid, titleUuid);
            segmentSplitInfos = iSegmentSplitViewService
                    .getSegmentSplitTree(contentAssociationUuid, titleUuid,
                            SegmentStatusEnum.DRAFT.getStatus());
            if (segmentSplitInfos.isEmpty()) {
                return;
            }
        }

        //是否取消正在发布的任务
        if (publishDTO.getPublishLater() == null) {
            //修改任务信息
            batchChangeStatus(segmentSplitInfos, null, contentAssociationUuid, titleUuid,
                    SegmentStatusEnum.DRAFT);
            return;
        }

        Set<String> playlistUuids = getPlaylistUuids(segmentSplitInfos.get(0).getPlaylistUuid(),
                contentAssociationUuid);

        //定时发布
        if (publishDTO.getPublishLater()) {
            checkPublishTime(publishDTO.getPublishTime());
            //修改任务信息
            batchChangeStatus(segmentSplitInfos, publishDTO.getPublishTime(),
                    contentAssociationUuid,
                    titleUuid, SegmentStatusEnum.DRAFT);
            //如果已经有任务 先取消任务 再重新设置
            String orgId = OrgUtil.orgContenter.get();
            defaultSchedulingConfigurer.cancelTriggerTask(contentAssociationUuid + titleUuid);
            List<SegmentSplitInfo> finalSegmentSplitInfos = segmentSplitInfos;
            defaultSchedulingConfigurer
                    .addTriggerTask(contentAssociationUuid + titleUuid,
                            publishDTO.getPublishTime(),
                            () -> {
                                OrgUtil.orgContenter.set(orgId);
                                //判断状态
                                boolean b = iSegmentSplitViewService
                                        .getSegmentSplitList(contentAssociationUuid,
                                                titleUuid, SegmentStatusEnum.DRAFT.getStatus())
                                        .stream().allMatch(segmentSplitInfo ->
                                                segmentSplitInfo.getPublishTime() != null);
                                if (!b) {
                                    if (Boolean.TRUE.equals(publishDTO.getOnlyUpdateStatus())) {
                                        publishDTO.getCountDownLatch().countDown();
                                    }
                                    return;
                                }
                                //如果存在已经发布版本,那么先删除该版本
                                deleteReleaseByAssociationUuid(contentAssociationUuid,
                                        titleUuid);
                                //记录任务结果
                                batchChangeStatus(finalSegmentSplitInfos,
                                        System.currentTimeMillis(),
                                        contentAssociationUuid, titleUuid,
                                        SegmentStatusEnum.RELEASE);
                                autoCreateDraftForWeekSplitSegment(contentAssociationUuid,
                                        titleUuid, publishDTO.getPublishCal());
                                ifSplitWeekDeleteSplit(contentAssociationUuid,
                                        finalSegmentSplitInfos, publishDTO.getDeletePreWeek());
                                //执行任务
                                if (!Boolean.TRUE.equals(publishDTO.getOnlyUpdateStatus())) {
                                    publish(playlistUuids);
                                } else {
                                    publishDTO.getCountDownLatch().countDown();
                                }
                            }
                    );

        } else {
            //如果存在已经发布版本,那么先删除该版本
            deleteReleaseByAssociationUuid(contentAssociationUuid, titleUuid);
            batchChangeStatus(segmentSplitInfos, System.currentTimeMillis(),
                    contentAssociationUuid, titleUuid, SegmentStatusEnum.RELEASE);
            autoCreateDraftForWeekSplitSegment(contentAssociationUuid, titleUuid,
                    publishDTO.getPublishCal());
            ifSplitWeekDeleteSplit(contentAssociationUuid, segmentSplitInfos,
                    publishDTO.getDeletePreWeek());
            //执行任务
            if (!Boolean.TRUE.equals(publishDTO.getOnlyUpdateStatus())) {
                publish(playlistUuids);
            }
        }

        String playlistUuid = segmentSplitInfos.get(0).getPlaylistUuid();
        try {
            iTaskReactorService.sendSegmentSplitEvent(
                    playlistUuid,
                    titleUuid,
                    contentAssociationUuid,
                    null,
                    true,
                    null);
        } catch (Exception e) {
            //降级处理
            logger.error("send segment warning event failed.", e);
        }
    }

    private void ifSplitWeekDeleteSplit(String contentAssociationUuid,
                                        List<SegmentSplitInfo> segmentSplitInfos, Boolean deletePreWeek) {
        SegmentInfo segmentInfo;
        PlaylistVersionContentAssociationDO associationDO = contentViewService
                .getByContentAssociationUuid(contentAssociationUuid);
        if (associationDO == null) {
            segmentInfo = iSegmentViewService.getSegment(contentAssociationUuid);
        } else {
            segmentInfo = JSON.parseObject(associationDO.getExtension(), SegmentInfo.class);
        }
        if (Boolean.TRUE.equals(segmentInfo.getSplitByWeek())) {
            SegmentSplitInfo segmentSplitInfo;
            if (Boolean.TRUE.equals(deletePreWeek)) {
                segmentSplitInfo = getPreSplitWeek(segmentSplitInfos.stream()
                        .flatMap(segmentSplitInfo1 -> segmentSplitInfo1.getChildren().stream())
                        .collect(
                                Collectors.toList()));
            } else {
                segmentSplitInfo = getNextSplitWeek(segmentSplitInfos.stream()
                        .flatMap(segmentSplitInfo1 -> segmentSplitInfo1.getChildren().stream())
                        .collect(
                                Collectors.toList()));
            }
            if (segmentSplitInfo != null) {
                deleteSegmentSplit(segmentSplitInfo.getUuid());
            }
        }
    }

    private SegmentSplitInfo getPreSplitWeek(List<SegmentSplitInfo> segmentSplitInfos) {
        return segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> !segmentSplitInfo.getDefaultGroup())
                .filter(segmentSplitInfo -> StringUtils
                        .isNotEmpty(segmentSplitInfo.getParentUuid()))
                .filter(segmentSplitInfo -> Boolean.TRUE.equals(segmentSplitInfo.getAutoGroup()))
                .min(Comparator.comparing(SegmentSplitInfo::getSplitRule)).orElse(null);
    }

    private SegmentSplitInfo getNextSplitWeek(List<SegmentSplitInfo> segmentSplitInfos) {
        return segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> !segmentSplitInfo.getDefaultGroup())
                .filter(segmentSplitInfo -> StringUtils
                        .isNotEmpty(segmentSplitInfo.getParentUuid()))
                .filter(segmentSplitInfo -> Boolean.TRUE.equals(segmentSplitInfo.getAutoGroup()))
                .max(Comparator.comparing(SegmentSplitInfo::getSplitRule)).orElse(null);
    }

    @Override
    public void autoCreateDraftForWeekSplitSegment(String associationUuid, String titleUuid,
                                                   Calendar cal) {
        logger.info(
                "auto create split draft for segment of WeekSplit. associationUuid:<{}> titleUuid:<{}>",
                associationUuid, titleUuid);
        try {
            if (isWeekSplitSegment(associationUuid)) {
                String rootUuid = createSegmentSplitDraft(associationUuid, titleUuid, true);
                List<SegmentSplitInfo> splitInfoList = iSegmentSplitViewService
                        .getSegmentSplitList(associationUuid, titleUuid,
                                SegmentStatusEnum.RELEASE.getStatus());
                if (cal == null) {
                    cal = Calendar.getInstance();
                }
                if (CollectionUtils.isNotEmpty(splitInfoList)) {
                    SegmentSplitInfo root = getRootSegmentInfo(
                            splitInfoList);
                    createSplitByWeek(rootUuid, root.getPlaylistUuid(),
                            root.getPplVersionUuid(), titleUuid,
                            root.getSegmentAssociationUuid(), cal,
                            splitInfoList.stream().filter(segmentSplitInfo -> Boolean.TRUE
                                    .equals(segmentSplitInfo.getAutoGroup())).collect(
                                    Collectors.toList()));
                }
            }
        } catch (Exception e) {
            logger.error("auto create draft for WeekSplitSegment error.", e);
        }
    }


    private boolean isWeekSplitSegment(String contentAssociationUuid) {
        PlaylistVersionContentAssociationDO associationDO = contentViewService
                .getByContentAssociationUuid(contentAssociationUuid);

        if (Objects.isNull(associationDO)) {
            SegmentDO segmentDO = iSegmentService.getById(contentAssociationUuid);
            return segmentDO.getSplitByWeek();
        } else {
            if (Objects.isNull(associationDO.getExtension())) {
                return false;
            }
            SegmentDTO segmentDTO = JSON
                    .parseObject(associationDO.getExtension(), SegmentDTO.class);
            if (Objects.isNull(segmentDTO)) {
                return false;
            }
            return Boolean.TRUE.equals(segmentDTO.getSplitByWeek());
        }
    }

    private void checkPublishTime(Long time) {
        if (time == null) {
            throw new BizException(ResultCodeEnum.PUBLISH_TIME_ERROR);
        }
        if (time <= System.currentTimeMillis()) {
            throw new BizException(ResultCodeEnum.PUBLISH_TIME_ERROR);
        }
    }

    @Transactional
    @Override
    public void publish(Set<String> pplUuids) {
        pplUuids.forEach(pplUuid -> eventPublish.publishEvent(new ChangeTplEvent(pplUuid)));
    }

    private void batchChangeStatus(List<SegmentSplitInfo> segmentSplitInfos, Long publishTime,
                                   String contentAssociationUuid, String titleUuid, SegmentStatusEnum statusEnum) {
        segmentSplitInfos.forEach(
                segmentSplitInfo -> changeStatus(contentAssociationUuid, titleUuid,
                        segmentSplitInfo,
                        statusEnum, publishTime));
    }

    private void changeStatus(String contentAssociationUuid, String titleUuid,
                              SegmentSplitInfo segmentSplitInfo, SegmentStatusEnum statusEnum, Long publishTime) {
        updateInfo(contentAssociationUuid, titleUuid, segmentSplitInfo, statusEnum, publishTime);
        segmentSplitInfo.getChildren().forEach(
                segmentSplitInfo1 -> changeStatus(contentAssociationUuid, titleUuid,
                        segmentSplitInfo1,
                        statusEnum, publishTime));
    }

    private void updateInfo(String contentAssociationUuid, String titleUuid,
                            SegmentSplitInfo segmentSplitInfo, SegmentStatusEnum draft, Long publishTime) {
        SegmentSplitDO segmentSplitDO = iSegmentSplitService.getById(segmentSplitInfo.getUuid());
        if (segmentSplitDO == null) {
            return;
        }
        segmentSplitDO.setPublishTime(publishTime);
        segmentSplitDO.setStatus(draft.getStatus());
        segmentSplitDO.setSign(iSegmentSplitService
                .buildSign(draft.getStatusStr(), contentAssociationUuid, titleUuid,
                        segmentSplitDO.getSplitRule(), segmentSplitDO.getParentUuid(),
                        segmentSplitDO.getSplitTitle()));
        iSegmentSplitService.updateById(segmentSplitDO);
    }

    @Override
    @Transactional
    public void unPublishSegmentSplitByAssociation(String contentAssociationUuid, String titleUuid,
                                                   Boolean keepRelease) {
        logger.info(
                "un-publish segment split. contentAssociationUuid:<{}> titleUuid:<{}> keepRelease:<{}>",
                contentAssociationUuid, titleUuid, keepRelease);
        List<SegmentSplitInfo> segmentSplitInfos = iSegmentSplitViewService
                .getSegmentSplitTree(contentAssociationUuid, titleUuid,
                        SegmentStatusEnum.RELEASE.getStatus());

        if (segmentSplitInfos.isEmpty()) {
            return;
        }

        if (Boolean.TRUE.equals(keepRelease)) {
            deleteDraftByAssociationUuid(contentAssociationUuid, titleUuid);
            batchChangeStatus(segmentSplitInfos, null, contentAssociationUuid, titleUuid,
                    SegmentStatusEnum.DRAFT);
        } else {
            deleteReleaseByAssociationUuid(contentAssociationUuid, titleUuid);
        }

        Set<String> playlistUuids = getPlaylistUuids(segmentSplitInfos.get(0).getPlaylistUuid(),
                contentAssociationUuid);
        publish(playlistUuids);

        playlistUuids.forEach(
                pplUuid -> {
                    reportNotPublishTask(pplUuid, titleUuid, contentAssociationUuid);
                    reportContentNotSelectTask(pplUuid, titleUuid, contentAssociationUuid);
                });
    }

    private void reportNotPublishTask(String pplUuid, String titleUuid,
                                      String contentAssociationUuid) {
        try {
            IPlaylistService iPlaylistService = SpringContextUtils.getBean(IPlaylistService.class);
            PlaylistDO playlistDO = iPlaylistService.getPlaylist(pplUuid);
            if (!PlaylistStatusEnum.DRAFT.getStatus().equals(playlistDO.getStatus())) {
                PlaylistVersionContentAssociationDO associationDO = contentViewService
                        .getByContentAssociationUuid(contentAssociationUuid);
                if (associationDO != null) {
                    SegmentInfo segmentInfo = JSON
                            .parseObject(associationDO.getExtension(), SegmentInfo.class);
                    if (SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                            .equals(segmentInfo.getContentKind())) {
                        iTaskReactorService
                                .sendSegmentSplitEvent(pplUuid, titleUuid, contentAssociationUuid,
                                        TaskTypeEnum.FEATURE_SEGMENT_NOT_PUBLISHED,
                                        false, segmentInfo.getContentKind());
                    } else {
                        if (!checkSegmentSplitDraft(contentAssociationUuid, titleUuid)) {
                            iTaskReactorService
                                    .sendSegmentSplitEvent(pplUuid, titleUuid,
                                            contentAssociationUuid,
                                            TaskTypeEnum.PPL_SEGMENT_NOT_ASSIGNED,
                                            false, segmentInfo.getContentKind());
                        }

                    }
                }
            }
        } catch (Exception e) {
            //降级处理
            logger.error("send segment warning event failed.", e);
        }
    }

    private Set<String> getPlaylistUuids(String playlistUuid,
                                         String contentAssociationUuid) {
        Set<String> playlistUuids = new HashSet<>();
        if (StringUtils.isEmpty(playlistUuid)) {
            //base segment,查找所有关联的ppl
            List<PlaylistVersionContentAssociationDO> playlistVersionContentAssociationDOS = contentViewService
                    .getByContentId(contentAssociationUuid);
            playlistUuids = playlistVersionContentAssociationDOS.stream()
                    .map(PlaylistVersionContentAssociationDO::getPlaylistUuid)
                    .collect(Collectors.toSet());
        } else {
            playlistUuids.add(playlistUuid);
        }
        return playlistUuids;
    }

    @Transactional
    @Override
    public String createOrUpdateSegmentSplit(SegmentSplitDTO segmentSplitDTO) {
        logger.info("create or update segment split. detail:<{}>",
                JSON.toJSONString(segmentSplitDTO));
        SegmentSplitDO segmentSplitDO = iSegmentSplitService.getBySign(
                segmentSplitDTO.getStatus(),
                segmentSplitDTO.getSegmentAssociationUuid(),
                segmentSplitDTO.getTitleUuid(),
                segmentSplitDTO.getSplitRule(),
                segmentSplitDTO.getParentUuid(),
                segmentSplitDTO.getSplitTitle()
        );
        String uuid;
        if (segmentSplitDO == null) {
            uuid = createSegmentSplit(segmentSplitDTO);
        } else {
            uuid = segmentSplitDO.getUuid();
            updateSegmentSplit(uuid, segmentSplitDTO);
        }
        return uuid;
    }

    @Transactional
    @Override
    public String createRootSegmentSplit(SegmentSplitDTO segmentSplitDTO) {
        logger.info("create root segment split. detail:<{}>", JSON.toJSONString(segmentSplitDTO));
        SegmentSplitDO segmentSplitDO = iSegmentSplitService.getBySign(
                segmentSplitDTO.getStatus(),
                segmentSplitDTO.getSegmentAssociationUuid(),
                segmentSplitDTO.getTitleUuid(),
                segmentSplitDTO.getSplitRule(),
                segmentSplitDTO.getParentUuid(),
                segmentSplitDTO.getSplitTitle()
        );
        String uuid;
        if (segmentSplitDO == null) {
            uuid = createSegmentSplit(segmentSplitDTO);
        } else {
            uuid = segmentSplitDO.getUuid();
        }
        return uuid;
    }

    @Override
    @Transactional
    public void deleteByVersionUuid(String versionUuid) {
        logger.info("delete segment split by playlist version<{}>.", versionUuid);
        List<PlaylistSegmentSplitAssociationDO> splitAssociationDOS = splitAssociationService
                .getSegmentSplitByPlaylistVersionUuid(versionUuid);

        splitAssociationDOS.forEach(playlistSegmentSplitAssociationDO -> {
            List<String> uuids = splitAssociationDOS.stream().map(
                    PlaylistSegmentSplitAssociationDO::getSegmentSplitUuid).collect(
                    Collectors.toList());
            if (uuids.isEmpty()) {
                return;
            }
            //iSegmentSplitService.removeByIds(uuids);
            segmentContentService.batchDeleteAssociation(uuids);
            splitAssociationService.batchDeleteAssociation(uuids);
        });
    }

    private String createSegmentSplit(SegmentSplitDTO segmentSplitDTO) {
        String uuid = iSegmentSplitService.createSegmentSplit(segmentSplitDTO);
        splitAssociationService.createAssociation(segmentSplitDTO.getPlaylistUuid(),
                segmentSplitDTO.getPplVersionUuid(),
                segmentSplitDTO.getSegmentAssociationUuid(), segmentSplitDTO.getTitleUuid(),
                uuid);
        segmentContentService.batchInsertAssociation(uuid, segmentSplitDTO.getContentList());
        return uuid;
    }

    private void updateSegmentSplit(String uuid, SegmentSplitDTO segmentSplitDTO) {
        iSegmentSplitService.updateSegmentSplit(uuid, segmentSplitDTO);
        segmentContentService.deleteAssociation(uuid);
        segmentContentService.batchInsertAssociation(uuid, segmentSplitDTO.getContentList());
    }

    private void deleteSegmentSplitTreeByNodeUuid(String nodeUuid) {
        List<String> uuids = iSegmentSplitService.deleteSegmentSplitTreeByNodeUuid(nodeUuid);
        segmentContentService.batchDeleteAssociation(uuids);
        splitAssociationService.batchDeleteAssociation(uuids);
    }

    private void deleteSegmentSplitTreeByParentNodeUuid(String pidNodeUuid) {
        List<String> uuids = iSegmentSplitService
                .deleteSegmentSplitTreeByParentNodeUuid(pidNodeUuid);
        segmentContentService.batchDeleteAssociation(uuids);
        splitAssociationService.batchDeleteAssociation(uuids);
    }

    @Override
    public void onApplicationEvent(CreateBaseSegmentRootSplit createBaseSegmentRootSplit) {
        String segmentUuid = (String) createBaseSegmentRootSplit.getSource();
        Boolean splitByWeek = createBaseSegmentRootSplit.getSplitByWeek();
        logger.info("Create root split of BaseSegment.segmentUuid:<{}> splitByWeek:<{}>",
                segmentUuid, splitByWeek);
        SegmentSplitDTO segmentSplitDTO = new SegmentSplitDTO();
        segmentSplitDTO.setAutoGroup(true);
        segmentSplitDTO.setDefaultGroup(false);
        segmentSplitDTO
                .setSegmentAssociationUuid(segmentUuid);
        segmentSplitDTO.setSplitTitle("root");
        segmentSplitDTO.setSplitType(SegmentSplitTypeEnum.ROOT.getName());
        segmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
        String rootUuid = createRootSegmentSplit(segmentSplitDTO);
        if (Boolean.TRUE.equals(splitByWeek)) {
            createSplitByWeek(rootUuid, null, null, null,
                    segmentUuid);
        }
    }

    @Override
    public boolean checkSegmentSplitDraft(String contentAssociationUuid, String titleUuid) {
        List<SegmentSplitInfo> segmentSplitInfos = iSegmentSplitViewService
                .getDraftByContentAssociationUuid(
                        contentAssociationUuid,
                        titleUuid);

        return segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> CollectionUtils.isEmpty(segmentSplitInfo.getChildren()))
                .noneMatch(segmentSplitInfo -> segmentSplitInfo.getShows() > 0 && CollectionUtils
                        .isEmpty(segmentSplitInfo.getContentList()));
    }

    private void reportContentNotSelectTask(String pplUuid, String associationUuid,
                                            String titleUuid) {
        ExecutorService executorService = CommonSourcePool.getExecutorService();
        final String orgId = OrgUtil.orgContenter.get();
        executorService.submit(() -> {
            OrgUtil.orgContenter.set(orgId);
            IPlaylistService iPlaylistService = SpringContextUtils.getBean(IPlaylistService.class);
            PlaylistDO playlistDO = iPlaylistService.getPlaylist(pplUuid);
            if (PlaylistStatusEnum.DRAFT.getStatus().equals(playlistDO.getStatus())) {
                if (checkSegmentSplitDraft(associationUuid, titleUuid)) {
                    PlaylistVersionContentAssociationDO associationDO = contentViewService
                            .getByContentAssociationUuid(associationUuid);
                    SegmentInfo segmentInfo = JSON
                            .parseObject(associationDO.getExtension(), SegmentInfo.class);
                    iTaskReactorService.sendSegmentSplitEvent(pplUuid, titleUuid,
                            associationUuid,
                            TaskTypeEnum.PPL_SEGMENT_NOT_ASSIGNED,
                            false, segmentInfo.getContentKind());
                } else {
                    iTaskReactorService.sendSegmentSplitEvent(pplUuid, titleUuid,
                            associationUuid,
                            TaskTypeEnum.PPL_SEGMENT_NOT_ASSIGNED,
                            true, null);
                }
            }
        });

    }
}
