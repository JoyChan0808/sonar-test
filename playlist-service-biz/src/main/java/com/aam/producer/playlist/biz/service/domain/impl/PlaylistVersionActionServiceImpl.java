package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.biz.event.ChangePplReleaseVersionEvent;
import com.aam.producer.playlist.biz.event.ChangeTplEvent;
import com.aam.producer.playlist.biz.event.CreateTplEvent;
import com.aam.producer.playlist.biz.event.OnSegmentSplitWeekChanged;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel;
import com.aam.producer.playlist.biz.service.IPlaylistSegmentSplitAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.biz.service.IPlaylistShowAttributeCombinationService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionService;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.ISegmentSplitContentAssociationService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistShowAttributeViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionActionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentActionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionViewService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitViewService;
import com.aam.producer.playlist.biz.service.domain.ISegmentViewService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistActionService;
import com.aam.producer.playlist.biz.service.domain.ITaskReactorService;
import com.aam.producer.playlist.biz.util.EventPublish;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.biz.util.TimeUtil;
import com.aam.producer.playlist.protocol.message.AutomationChangeDataDTO;
import com.aam.producer.playlist.protocol.message.SegmentDTO;
import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.protocol.request.CplMetaDTO;
import com.aam.producer.playlist.protocol.request.PlaylistVersionDTO;
import com.aam.producer.playlist.protocol.request.PublishDTO;
import com.aam.producer.playlist.protocol.request.ShowAttributeGroupDTO;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.protocol.response.PlaylistVersionInfo;
import com.aam.producer.playlist.protocol.response.SegmentInfo;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.aam.producer.playlist.repository.entity.PlaylistShowAttributeCombinationDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.repository.entity.SegmentSplitContentAssociationDO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.aam.producer.playlist.sal.client.IFilmHallFacadeClient;
import com.aam.producer.playlist.sal.client.IJobClient;
import com.aam.producer.playlist.sal.response.FilmHallInfo;
import com.aam.producer.playlist.sal.response.PosInfo;
import com.aam.producer.task.protocol.enums.TaskTypeEnum;
import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;
import com.aam.utils.utils.SpringContextUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.TypeReference;
import com.google.common.collect.Sets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class PlaylistVersionActionServiceImpl implements IPlaylistVersionActionService,
        ApplicationListener {

    private final static Logger logger = LoggerFactory
            .getLogger(PlaylistVersionActionServiceImpl.class);
    private final IPlaylistVersionService iPlaylistVersionService;
    private final IPlaylistVersionViewService iPlaylistVersionViewService;
    private final ISegmentViewService iSegmentViewService;
    private final ISegmentSplitContentAssociationService splitContentAssociationService;
    private final IPlaylistVersionContentAssociationService contentAssociationService;
    private final IPlaylistShowAttributeCombinationService showAttributeCombinationService;
    private final IPlaylistShowAttributeViewService attributeViewService;
    private final IPosMappingService iPosMappingService;
    private final ITPlaylistActionService iTPlaylistActionService;
    private final IFilmHallFacadeClient filmHallFacadeClient;
    private final IPlaylistService iPlaylistService;
    private final IPosPlaylistMappingService iPosPlaylistMappingService;
    private final IPlaylistVersionContentActionService playlistVersionContentActionService;
    private final IPlaylistSegmentSplitAssociationService splitAssociationService;
    private final ISegmentSplitViewService iSegmentSplitViewService;
    private final ISegmentSplitActionService iSegmentSplitActionService;
    private final EventPublish eventPublish;
    private final IJobClient iJobClient;
    private final IComplexFacadeClient iComplexFacadeClient;
    private final ITaskReactorService iTaskReactorService;
    private final IPlaylistVersionContentViewService iPlaylistVersionContentViewService;


    @Autowired
    public PlaylistVersionActionServiceImpl(IPlaylistVersionService iPlaylistVersionService,
            IPlaylistVersionContentAssociationService contentAssociationService,
            ISegmentViewService iSegmentViewService,
            IPlaylistShowAttributeCombinationService showAttributeCombinationService,
            IPlaylistVersionViewService iPlaylistVersionViewService,
            IPosMappingService iPosMappingService,
            IPlaylistShowAttributeViewService attributeViewService,
            ITPlaylistActionService iTPlaylistActionService,
            IFilmHallFacadeClient filmHallFacadeClient,
            ISegmentSplitContentAssociationService splitContentAssociationService,
            IPlaylistService iPlaylistService,
            IPosPlaylistMappingService iPosPlaylistMappingService,
            IPlaylistVersionContentActionService playlistVersionContentActionService,
            EventPublish eventPublish,
            IPlaylistSegmentSplitAssociationService splitAssociationService,
            ISegmentSplitViewService iSegmentSplitViewService,
            IJobClient iJobClient,
            IPlaylistVersionContentViewService iPlaylistVersionContentViewService,
            IComplexFacadeClient iComplexFacadeClient,
            ISegmentSplitActionService iSegmentSplitActionService,
            ITaskReactorService iTaskReactorService) {
        this.iPlaylistVersionService = iPlaylistVersionService;
        this.contentAssociationService = contentAssociationService;
        this.iSegmentViewService = iSegmentViewService;
        this.showAttributeCombinationService = showAttributeCombinationService;
        this.iPlaylistVersionViewService = iPlaylistVersionViewService;
        this.iPosMappingService = iPosMappingService;
        this.attributeViewService = attributeViewService;
        this.iTPlaylistActionService = iTPlaylistActionService;
        this.filmHallFacadeClient = filmHallFacadeClient;
        this.splitContentAssociationService = splitContentAssociationService;
        this.iPlaylistService = iPlaylistService;
        this.iPosPlaylistMappingService = iPosPlaylistMappingService;
        this.playlistVersionContentActionService = playlistVersionContentActionService;
        this.eventPublish = eventPublish;
        this.splitAssociationService = splitAssociationService;
        this.iSegmentSplitViewService = iSegmentSplitViewService;
        this.iJobClient = iJobClient;
        this.iComplexFacadeClient = iComplexFacadeClient;
        this.iSegmentSplitActionService = iSegmentSplitActionService;
        this.iTaskReactorService = iTaskReactorService;
        this.iPlaylistVersionContentViewService = iPlaylistVersionContentViewService;
    }

    @Transactional
    @Override
    public String createPlaylistVersion(PlaylistVersionDTO playListVersionDTO) {
        logger.info("create a playlist version. detail:<{}>",
                JSON.toJSONString(playListVersionDTO));
        //创建ppl version
        String pplVersionUuid = iPlaylistVersionService
                .createPlaylistVersion(playListVersionDTO.getPlaylistUuid());

        //设置content
        setContent(!playListVersionDTO.getShowAttributeGroups().isEmpty(),
                playListVersionDTO.getPlaylistUuid(),
                pplVersionUuid,
                playListVersionDTO.getContentList());

        //设置show type
        setShowType(pplVersionUuid, playListVersionDTO.getShowAttributeGroups());

        checkCodes(pplVersionUuid);

        return pplVersionUuid;
    }

    private void checkCodes(String pplVersionUuid) {
        List<Long> codes = attributeViewService
                .getCodesByPplVersionUuid(pplVersionUuid);
        codes.forEach(code -> {
            List<PlaylistShowAttributeCombinationDO> combinationDOs = showAttributeCombinationService
                    .getCombinationsBySumCode(code);
            /*if (combinationDOs.size() > 2) {
                throw new BizException(ResultCodeEnum.SHOW_TAGS_ALREADY_EXIST);
            } else if (combinationDOs.size() == 2) {*/

            List<String> playlistVersionUuids = combinationDOs.stream().map(
                    PlaylistShowAttributeCombinationDO::getPlaylistUuid).collect(
                    Collectors.toList());

            List<PlaylistVersionDO> playlistVersionDOS = iPlaylistVersionService
                    .getByIds(playlistVersionUuids);

            Set<String> pplSet = playlistVersionDOS.stream()
                    .map(PlaylistVersionDO::getPlaylistUuid).collect(
                            Collectors.toSet());

            if (pplSet.size() != 1) {
                throw new BizException(ResultCodeEnum.SHOW_TAGS_ALREADY_EXIST);
            }
            /*}*/
        });
    }

    @Transactional
    @Override
    public void updatePlaylistVersion(String versionUuid, PlaylistVersionDTO playListVersionDTO) {
        logger.info("update playlist version<{}>. detail:<{}>", versionUuid,
                JSON.toJSONString(playListVersionDTO));
        PlaylistVersionDO playlistVersionDO = iPlaylistVersionService.getById(versionUuid);
        if (playlistVersionDO == null || Objects
                .equals(playlistVersionDO.getStatus(), PlaylistStatusEnum.RELEASE.getStatus())) {
            return;
        }
        IPlaylistService iPlaylistService = SpringContextUtils.getBean(IPlaylistService.class);
        PlaylistDO playlistDO = iPlaylistService.getPlaylist(playlistVersionDO.getPlaylistUuid());
        contentAssociationService.deleteAssociation(versionUuid);
        if (!playListVersionDTO.getContentList().isEmpty()) {
            setContent(playlistDO.getAutomaticallyApply(),
                    playListVersionDTO.getPlaylistUuid(),
                    versionUuid,
                    playListVersionDTO.getContentList());
        }
        if (playlistDO.getAutomaticallyApply()) {
            showAttributeCombinationService.deleteShowAttributeCombination(versionUuid);
            setShowType(versionUuid, playListVersionDTO.getShowAttributeGroups());
        }

        checkCodes(versionUuid);

    }

    @Transactional
    @Override
    public void deletePlaylistVersion(String versionUuid) {
        logger.info("delete playlist version<{}>.", versionUuid);
        //
        //segmentSplitActionService.deleteByVersionUuid(versionUuid);
        //删除content
        playlistVersionContentActionService.deleteAssociation(versionUuid);
        //删除show type
        showAttributeCombinationService.deleteShowAttributeCombination(versionUuid);
        //删除ppl version
        iPlaylistVersionService.deletePlaylistVersion(versionUuid);
    }


    @Transactional
    @Override
    public void publishPlaylistVersion(String versionUuid, PublishDTO publishDTO) {
        logger.info("publish playlist version<{}>.options:<{}>", versionUuid,
                JSON.toJSONString(publishDTO));
        PlaylistVersionDO playlistVersionDO = iPlaylistVersionService.getById(versionUuid);
        if (playlistVersionDO == null || Objects
                .equals(playlistVersionDO.getStatus(), PlaylistStatusEnum.RELEASE.getStatus())) {
            return;
        }

        IPlaylistService iPlaylistService = SpringContextUtils.getBean(IPlaylistService.class);
        PlaylistDO playlistDO = iPlaylistService.getPlaylist(playlistVersionDO.getPlaylistUuid());

        List<Long> codes = new ArrayList<>();
        if (playlistDO.getAutomaticallyApply()) {
            codes = attributeViewService
                    .getCodesByPplVersionUuid(playlistVersionDO.getUuid());
        }

        //是否取消正在发布的任务
        if (publishDTO.getPublishLater() == null) {
            //取消任务
            if (StringUtils.isNotEmpty(playlistVersionDO.getExtension())) {
                iJobClient.remove(playlistVersionDO.getExtension());
                recordJobId(null, playlistVersionDO);
            }
            //修改发布时间
            iPlaylistVersionService.updatePublishTime(playlistVersionDO.getUuid(), null, null);
            return;
        }

        //定时发布
        if (publishDTO.getPublishLater()) {
            checkPublishTime(publishDTO.getPublishTime());
            //修改发布时间
            iPlaylistVersionService
                    .updatePublishTime(playlistVersionDO.getUuid(), publishDTO.getPublishTime(),
                            true);

            //如果已经有任务 先取消任务 再重新设置
            String orgId = OrgUtil.orgContenter.get();
            //playlistVersionDO.getUuid()
            if (StringUtils.isNotEmpty(playlistVersionDO.getExtension())) {
                iJobClient.remove(playlistVersionDO.getExtension());
                recordJobId(null, playlistVersionDO);
            }
            Map<String, String> paramMap = new HashMap<>();
            paramMap.put("orgUuid", orgId);
            paramMap.put("pplUuid", playlistDO.getUuid());
            paramMap.put("versionUuid", playlistVersionDO.getUuid());
            paramMap.put("playlistTitle", playlistDO.getTitle());
            paramMap.put("cron", TimeUtil.getCron(new Date(publishDTO.getPublishTime())));
            String jobUuid = iJobClient.add(paramMap);
            recordJobId(jobUuid, playlistVersionDO);
        } else {
            handlePublish(playlistDO, playlistVersionDO, codes, false);
        }
    }

    private void recordJobId(String jobId, PlaylistVersionDO playlistVersionDO) {
        playlistVersionDO.setExtension(jobId);
        iPlaylistVersionService.updateById(playlistVersionDO);
    }

    @Override
    public void publishPlaylistVersion(String pplUuid, String versionUuid) {
        logger.info("publish playlist<{}> version<{}>", pplUuid, versionUuid);
        IPlaylistService iPlaylistService = SpringContextUtils.getBean(IPlaylistService.class);
        PlaylistDO playlistDO = iPlaylistService.getPlaylist(pplUuid);
        PlaylistVersionDO playlistVersionDO = iPlaylistVersionService.getById(versionUuid);
        if (playlistVersionDO == null || Objects
                .equals(playlistVersionDO.getStatus(), PlaylistStatusEnum.RELEASE.getStatus())) {
            return;
        }
        List<Long> codes = new ArrayList<>();
        if (playlistDO.getAutomaticallyApply()) {
            codes = attributeViewService
                    .getCodesByPplVersionUuid(playlistVersionDO.getUuid());
        }
        handlePublish(playlistDO, playlistVersionDO, codes, true);
        try {
            iJobClient.remove(playlistVersionDO.getExtension());
        } catch (Exception e) {
            logger.error("remove task <{}> fail.", playlistVersionDO.getExtension());
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

    private void handlePublish(PlaylistDO playlistDO, PlaylistVersionDO playlistVersionDO,
            List<Long> codes, boolean publishLater) {

        //删除存在的发布状态
        PlaylistVersionDO releaseVersionDO = iPlaylistVersionService
                .getReleasePlaylistVersionByPplUuid(playlistVersionDO.getPlaylistUuid());
        List<Long> releaseCodes = new ArrayList<>();
        if (releaseVersionDO != null) {
            if (playlistDO.getAutomaticallyApply()) {
                releaseCodes = showAttributeCombinationService
                        .getByPplVersionUuid(releaseVersionDO.getUuid()).stream()
                        .map(PlaylistShowAttributeCombinationDO::getShortCodeAssociation).collect(
                                Collectors.toList());
            }
            deletePlaylistVersion(releaseVersionDO.getUuid());

            ChangePplReleaseVersionEvent event = new ChangePplReleaseVersionEvent(
                    releaseVersionDO.getUuid()
                    , playlistVersionDO.getUuid());
            eventPublish.publishEvent(event);
        }

        //修改发布时间
        iPlaylistVersionService
                .updatePublishTime(playlistVersionDO.getUuid(), System.currentTimeMillis(),
                        publishLater);
        //修改发布状态
        iPlaylistVersionService.updatePlaylistVersion(playlistVersionDO.getUuid(),
                PlaylistStatusEnum.RELEASE.getStatus());

        //重新发布show type有变化,通知重新计算正片分组
        if (releaseVersionDO != null && playlistDO.getAutomaticallyApply()) {
            if (isChangeShowType(releaseCodes, codes)) {
                iPosMappingService.posUnMapping(playlistDO.getUuid());
                sendChangeSegmentSplitTask(playlistDO, playlistVersionDO.getUuid());
            }
        }
        iPosMappingService.handlePplPublish(playlistDO, codes);
    }

    private void sendChangeSegmentSplitTask(PlaylistDO playlistDO, String releaseVersion) {

        List<PlaylistSegmentSplitAssociationDO> playlistSegmentSplitAssociationDOS = splitAssociationService
                .getSegmentSplitByPlaylistUuid(playlistDO.getUuid());

        List<String> ids = contentAssociationService.getAssociations(releaseVersion).stream()
                .filter(playlistVersionContentAssociationDO ->
                        ContentTypeEnum.SEGMENT.getCode()
                                .equals(playlistVersionContentAssociationDO.getContentType()))
                .filter(playlistVersionContentAssociationDO -> {
                    SegmentDTO segmentDTO = JSON
                            .parseObject(playlistVersionContentAssociationDO.getExtension(),
                                    SegmentDTO.class);
                    return SegmentTypeEnum.AUTOMATIC_SEGMENT.getName().equals(segmentDTO.getType());
                }).map(PlaylistVersionContentAssociationDO::getUuid).collect(Collectors.toList());

        playlistSegmentSplitAssociationDOS.stream().filter(playlistSegmentSplitAssociationDO ->
                ids.contains(playlistSegmentSplitAssociationDO.getSegmentAssociationUuid()))
                .forEach(playlistSegmentSplitAssociationDO ->
                        iTaskReactorService.sendSegmentSplitEvent(playlistDO.getUuid(),
                                playlistSegmentSplitAssociationDO.getTitleId(),
                                playlistSegmentSplitAssociationDO.getSegmentAssociationUuid(),
                                TaskTypeEnum.NEW_CONTENT_AVAILABLE, false,
                                SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()));
    }

    private boolean isChangeShowType(List<Long> releaseCodes, List<Long> codes) {
        String sum1 = releaseCodes.stream().sorted().map(Object::toString).collect(
                Collectors.joining("-"));
        String sum2 = codes.stream().sorted().map(Object::toString).collect(
                Collectors.joining("-"));
        return !sum2.equals(sum1);
    }

    @Transactional
    @Override
    public void createTpl(PlaylistDO playlist, List<PosPlaylistMappingDO> posPlaylistMappingDOS,
            int action) {
        logger.info("create tpl by ppl<{}> pos<{}> action<{}>", JSON.toJSONString(playlist),
                posPlaylistMappingDOS.stream()
                        .map(PosPlaylistMappingDO::getPosUuid)
                        .collect(Collectors.toList()), action);

        PlaylistVersionDO playlistVersionDO = iPlaylistVersionService
                .getReleasePlaylistVersionByPplUuid(playlist.getUuid());
        if (playlistVersionDO == null) {
            return;
        }

        List<PosInfo> posInfos = toPosInfo(posPlaylistMappingDOS);
        List<PlaylistPublishModel> publishModels = iPlaylistVersionViewService
                .getPublishMessage(playlist, playlistVersionDO, posInfos);

        //1创建 2匹配场次 3有新的cpl 4cpl内容发生变化 5发布占位符 6修改title
        switch (action) {
            case 1:
                publishModels.forEach(iTPlaylistActionService::handleTplPublish);
                reportNotPublishTask(playlist.getUuid(), playlistVersionDO.getUuid(),
                        posPlaylistMappingDOS);
                break;
            case 2:
                publishModels.forEach(iTPlaylistActionService::handlePosMatch);
                break;
            case 3:
                reportNewContentAvailable(playlist, posPlaylistMappingDOS, playlistVersionDO);
                break;
            case 4:
            case 5:
                publishModels.forEach(iTPlaylistActionService::handleTplPublish);
                break;
            case 6:
                publishModels.forEach(iTPlaylistActionService::handleTplTitleChange);
                break;
            default:
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    private void reportNewContentAvailable(PlaylistDO playlist,
            List<PosPlaylistMappingDO> posPlaylistMappingDOS, PlaylistVersionDO playlistVersionDO) {
        try {
            if (playlist.getAutomaticallyApply()) {
                posPlaylistMappingDOS.stream().map(PosPlaylistMappingDO::getTitleUuid)
                        .distinct().forEach(titleUuid ->
                        iTaskReactorService
                                .sendSegmentSplitEvent(playlist.getUuid(), titleUuid,
                                        playlistVersionContentActionService
                                                .getAutoSegmentUuid(
                                                        playlistVersionDO.getUuid()),
                                        TaskTypeEnum.NEW_CONTENT_AVAILABLE,
                                        false, SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()));
            }
        } catch (Exception e) {
            //降级处理
            logger.error("send NewContentAvailable warning event failed.", e);
        }

    }

    private void reportNotPublishTask(String pplUuid, String pplVersionUuid,
            List<PosPlaylistMappingDO> posPlaylistMappingDOS) {
        try {
            List<ContentInfo> contentInfos = iPlaylistVersionContentViewService
                    .getByPplVersionUuid(pplVersionUuid);

            //过滤出segment
            Map<String, SegmentInfo> map = filterSegment(contentInfos);

            //处理title维度的segment
            map.entrySet().stream().filter(entry -> {
                SegmentInfo segmentInfo = entry.getValue();
                return SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                        .equals(segmentInfo.getContentKind()) ||
                        SegmentTypeEnum.TITLE_SEGMENT.getName().equals(segmentInfo.getContentKind())
                        || SegmentTypeEnum.RATING_SEGMENT.getName()
                        .equals(segmentInfo.getContentKind());
            }).forEach(entry -> {
                SegmentInfo segmentInfo = entry.getValue();
                String associationUuid = entry.getKey();
                posPlaylistMappingDOS.stream().map(PosPlaylistMappingDO::getTitleUuid)
                        .distinct().forEach(titleUuid -> {
                    SegmentStatusEnum status = iSegmentSplitViewService
                            .getStatus(titleUuid, associationUuid);
                    if (SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                            .equals(segmentInfo.getContentKind())) {
                        if (status == SegmentStatusEnum.NULL ||
                                status == SegmentStatusEnum.DRAFT) {
                            iTaskReactorService.sendSegmentSplitEvent(pplUuid, titleUuid,
                                    associationUuid,
                                    TaskTypeEnum.FEATURE_SEGMENT_NOT_PUBLISHED,
                                    false, segmentInfo.getContentKind());
                        }
                    } else {
                        if ((status == SegmentStatusEnum.NULL ||
                                status == SegmentStatusEnum.DRAFT) && !iSegmentSplitActionService
                                .checkSegmentSplitDraft(associationUuid, titleUuid)) {
                            iTaskReactorService.sendSegmentSplitEvent(pplUuid, titleUuid,
                                    associationUuid,
                                    TaskTypeEnum.PPL_SEGMENT_NOT_ASSIGNED,
                                    false, segmentInfo.getContentKind());
                        }
                    }
                });
            });

            //处理playlist维度的segment
            map.entrySet().stream().filter(entry -> {
                SegmentInfo segmentInfo = entry.getValue();
                return SegmentTypeEnum.PLAYLIST_SEGMENT.getName()
                        .equals(segmentInfo.getContentKind());
            }).forEach(entry -> {
                SegmentInfo segmentInfo = entry.getValue();
                String associationUuid = entry.getKey();
                SegmentStatusEnum status = iSegmentSplitViewService
                        .getStatus(null, associationUuid);
                if ((status == SegmentStatusEnum.NULL ||
                        status == SegmentStatusEnum.DRAFT) && !iSegmentSplitActionService
                        .checkSegmentSplitDraft(associationUuid, null)) {
                    iTaskReactorService.sendSegmentSplitEvent(pplUuid, null,
                            associationUuid,
                            TaskTypeEnum.PPL_SEGMENT_NOT_ASSIGNED,
                            false, segmentInfo.getContentKind());
                }
            });

            //处理scope维度的segment
            map.entrySet().stream().filter(entry -> {
                SegmentInfo segmentInfo = entry.getValue();
                return SegmentTypeEnum.BASE_SEGMENT.getName()
                        .equals(segmentInfo.getContentKind());
            }).forEach(entry -> {
                SegmentInfo segmentInfo = entry.getValue();
                String associationUuid = segmentInfo.getUuid();
                SegmentStatusEnum status = iSegmentSplitViewService
                        .getStatus(null, associationUuid);
                if ((status == SegmentStatusEnum.NULL ||
                        status == SegmentStatusEnum.DRAFT) && !iSegmentSplitActionService
                        .checkSegmentSplitDraft(associationUuid, null)) {
                    iTaskReactorService.sendSegmentSplitEvent(pplUuid, null,
                            associationUuid,
                            TaskTypeEnum.PPL_SEGMENT_NOT_ASSIGNED,
                            false, segmentInfo.getContentKind());
                }
            });

        } catch (Exception e) {
            //降级处理
            logger.error("send NotPublishTask warning event failed.", e);
        }
    }

    private Map<String, SegmentInfo> filterSegment(List<ContentInfo> contentInfos) {
        return contentInfos.stream()
                .filter(contentInfo -> ContentTypeEnum.SEGMENT.getName()
                        .equals(contentInfo.getContentType())).collect(Collectors.toMap(
                        ContentInfo::getContentAssociationUuid, contentInfo -> JSON
                                .parseObject(contentInfo.getExtension(), SegmentInfo.class)));
    }

    @Override
    public void setPosFilmHallInfos(List<PosInfo> posInfos) {
        Set<String> complexUuids = posInfos.stream().map(PosInfo::getComplexId)
                .collect(Collectors.toSet());
        List<FilmHallInfo> filmHallInfos = filmHallFacadeClient
                .getFilmHallInfosByComplexUuids(complexUuids);

        Map<String, String> addressCountryMap = iComplexFacadeClient
                .getAddressCountry(complexUuids);

        posInfos.forEach(posInfo -> {
            posInfo.setFilmHallInfo(
                    filmHallInfos.stream()
                            .collect(Collectors.toMap(FilmHallInfo::getScreenUuid, o -> o))
                            .get(posInfo.getScreenUuid()));
            posInfo.setAddressCountry(addressCountryMap.get(posInfo.getComplexId()));
        });
    }

    @Override
    public List<PosInfo> toPosInfo(List<PosPlaylistMappingDO> posPlaylistMappingDOS) {
        if (CollectionUtils.isEmpty(posPlaylistMappingDOS)) {
            return new ArrayList<>();
        }

        List<PosInfo> posInfos = posPlaylistMappingDOS.stream().map(posPlaylistMappingDO -> {
            PosInfo posInfo = new PosInfo();
            Map<String, String> showTypeMap = JSON
                    .parseObject(posPlaylistMappingDO.getShowAttributes(),
                            new TypeReference<Map<String, String>>() {
                            });
            posInfo.setShowAttrList(new ArrayList<>(showTypeMap.values()));
            posInfo.setComplexId(posPlaylistMappingDO.getComplexUuid());
            posInfo.setLanguage(posPlaylistMappingDO.getLanguage());
            posInfo.setPlaylistUuid(posPlaylistMappingDO.getTplUuid());
            posInfo.setPosUuid(posPlaylistMappingDO.getPosUuid());
            posInfo.setScreenUuid(posPlaylistMappingDO.getScreenUuid());
            posInfo.setShowStart(posPlaylistMappingDO.getPosStart());
            posInfo.setTitle(posPlaylistMappingDO.getPosTitle());
            posInfo.setTitleUuid(posPlaylistMappingDO.getTitleUuid());
            posInfo.setState(posPlaylistMappingDO.getState());
            if (StringUtils.isNotEmpty(posPlaylistMappingDO.getUnmatchedShowAttributes())) {
                Map<String, String> map = JSON
                        .parseObject(posPlaylistMappingDO.getUnmatchedShowAttributes(),
                                new TypeReference<Map<String, String>>() {
                                });
                posInfo.setUnmatchedShowAttributes(new ArrayList<>(map.values()));
            }
            return posInfo;
        }).collect(Collectors.toList());
        setPosFilmHallInfos(posInfos);
        return posInfos;
    }

    @Transactional
    @Override
    public void unPublishPlaylistVersion(String versionUuid, Boolean keepRelease) {
        logger.info("unPublish playlist version<{}>. keepRelease:<{}>", versionUuid, keepRelease);
        PlaylistVersionDO playlistVersionDO = iPlaylistVersionService.getById(versionUuid);
        if (playlistVersionDO == null || Objects
                .equals(playlistVersionDO.getStatus(), PlaylistStatusEnum.DRAFT.getStatus())) {
            return;
        }
        PlaylistVersionDO playlistVersionDO1 = iPlaylistVersionService
                .getDraftPlaylistVersionByPplUuid(playlistVersionDO.getPlaylistUuid());
        if (Boolean.TRUE.equals(keepRelease)) {
            if (playlistVersionDO1 != null) {
                deletePlaylistVersion(playlistVersionDO1.getUuid());
            }
            //修改发布时间
            iPlaylistVersionService.updatePublishTime(playlistVersionDO.getUuid(), null, null);
            //修改发布状态
            iPlaylistVersionService.updatePlaylistVersion(playlistVersionDO.getUuid(),
                    PlaylistStatusEnum.DRAFT.getStatus());

            try {
                iTaskReactorService
                        .sendSegmentSplitEvent(playlistVersionDO.getPlaylistUuid(), null, null,
                                null, true, null);
            } catch (Exception e) {
                //降级处理
                logger.error("cancel ppl warning event failed.", e);
                logger.error("cancel ppl<{}> warning event failed.",
                        playlistVersionDO.getPlaylistUuid());
            }

        } else {
            ChangePplReleaseVersionEvent event = new ChangePplReleaseVersionEvent(versionUuid
                    , playlistVersionDO1.getUuid());
            eventPublish.publishEvent(event);
            deletePlaylistVersion(versionUuid);
        }
        unPublish(playlistVersionDO.getPlaylistUuid(), playlistVersionDO.getUuid());
    }

    @Override
    @Transactional
    public void unPublish(String pplUuid, String pplVersionUuid) {
        logger.info("unPublish playlist<{}> version<{}>.", pplUuid, pplVersionUuid);
        iPosMappingService.posUnMapping(pplUuid);
        PlaylistPublishModel playlistUnPublishMessage = new PlaylistPublishModel();
        playlistUnPublishMessage.setPplUuid(pplUuid);
        playlistUnPublishMessage.setPplVersionUuid(pplVersionUuid);
        playlistUnPublishMessage.setOrganizationId(OrgUtil.orgContenter.get());
        iTPlaylistActionService.handleTplUnPublish(playlistUnPublishMessage);
    }

    @Override
    public String createPlaylistVersionDraft(String versionUuid, Boolean copyContent) {
        logger.info("unPublish draft of playlist version<{}>. copyContent:<{}>", versionUuid,
                copyContent);
        PlaylistVersionInfo playlistVersionInfo = iPlaylistVersionViewService
                .getPlaylistVersion(versionUuid);
        if (playlistVersionInfo == null) {
            throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
        PlaylistVersionDO draft = iPlaylistVersionService
                .getDraftPlaylistVersionByPplUuid(playlistVersionInfo.getPlaylistUuid());
        if (draft != null) {
            return draft.getUuid();
        }
        PlaylistVersionDTO playlistVersionDTO = new PlaylistVersionDTO();
        playlistVersionDTO.setPlaylistUuid(playlistVersionInfo.getPlaylistUuid());
        playlistVersionDTO.setShowAttributeGroups(
                playlistVersionInfo.getShowAttributeGroups().stream()
                        .map(showAttributeGroupInfo -> {
                            ShowAttributeGroupDTO showAttributeGroupDTO = new ShowAttributeGroupDTO();
                            showAttributeGroupDTO.setName(showAttributeGroupInfo.getName());
                            showAttributeGroupDTO
                                    .setAttributes(showAttributeGroupInfo.getAttributes());
                            return showAttributeGroupDTO;
                        }).collect(Collectors.toList()));
        if (Boolean.TRUE.equals(copyContent)) {
            playlistVersionDTO.setContentList(
                    playlistVersionInfo.getContentList().stream().map(contentInfo -> {
                        ContentDTO contentDTO = new ContentDTO();
                        contentDTO.setContentId(contentInfo.getContentId());
                        contentDTO.setContentType(contentInfo.getContentType());
                        contentDTO.setExtension(contentInfo.getExtension());
                        contentDTO.setTitle(contentInfo.getTitle());

                        if (ContentTypeEnum.SEGMENT.getName()
                                .equals(contentInfo.getContentType())) {
                            SegmentDTO segmentDTO = JSON
                                    .parseObject(contentInfo.getExtension(), SegmentDTO.class);
                            if (segmentDTO.getType()
                                    .equals(SegmentTypeEnum.PLAYLIST_SEGMENT.getName())) {
                                contentDTO.setOldContentAssociationUuid(
                                        contentInfo.getContentAssociationUuid());
                            } else {
                                contentDTO.setContentAssociationUuid(
                                        contentInfo.getContentAssociationUuid());
                            }
                        }
                        return contentDTO;
                    }).collect(Collectors.toList())
            );
        }
        return createPlaylistVersion(playlistVersionDTO);
    }

    @Override
    @Transactional
    public List<PlaylistDO> automationChanged(AutomationChangeDataDTO dto) {
        logger.info("automation had changed. detail:<{}>", JSON.toJSONString(dto));
        String automationUuid = dto.getUuid();
        String automationName = dto.getName();
        Set<String> pplUUIDs = new HashSet<>();

        // update automation in events
        modifyPlaylistVersionContentAssociation(automationUuid, automationName, null, pplUUIDs);

        // update automation in segment
        modifyPlaylistSplitContentAssociation(automationUuid, automationName, null, pplUUIDs);

        if (CollectionUtils.isNotEmpty(pplUUIDs)) {
            return iPlaylistService.selectBatchIds(pplUUIDs);
        } else {
            return Collections.emptyList();
        }
    }

    @Override
    public void allSegmentPublish(String playlistUuid, String pplVersionUuid) {
        PlaylistVersionInfo playlistVersionInfo = iPlaylistVersionViewService
                .getPlaylistVersion(pplVersionUuid);
        List<ContentInfo> contentInfos = playlistVersionInfo.getContentList();
        if (CollectionUtils.isNotEmpty(contentInfos)) {
            List<PosPlaylistMappingDO> posPlaylistMappingDOS = iPosPlaylistMappingService
                    .getMappingsByPpl(false, playlistUuid);
            List<String> titleUuids = posPlaylistMappingDOS.stream()
                    .map(PosPlaylistMappingDO::getTitleUuid).filter(
                            StringUtils::isNotEmpty).collect(Collectors.toList());
            Map<String, SegmentInfo> map = filterSegment(contentInfos);
            map.forEach((contentAssociationUuid, value) -> {
                PublishDTO publishDTO = new PublishDTO();
                publishDTO.setPublishLater(false);
                publishDTO.setOnlyUpdateStatus(true);
                if (Boolean.TRUE.equals(value.getSplitByWeek())) {
                    return;
                }
                if (
                        SegmentTypeEnum.RATING_SEGMENT.getName()
                                .equals(value.getContentKind()) ||
                                SegmentTypeEnum.TITLE_SEGMENT.getName()
                                        .equals(value.getContentKind()) ||
                                SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                                        .equals(value.getContentKind())
                ) {
                    titleUuids.forEach(s -> iSegmentSplitActionService
                            .publishSegmentSplitByAssociation(contentAssociationUuid, s,
                                    publishDTO));
                } else if (
                        SegmentTypeEnum.PLAYLIST_SEGMENT.getName()
                                .equals(value.getContentKind())
                ) {
                    iSegmentSplitActionService
                            .publishSegmentSplitByAssociation(contentAssociationUuid, null,
                                    publishDTO);
                } else if (
                        SegmentTypeEnum.BASE_SEGMENT.getName()
                                .equals(value.getContentKind())
                ) {
                    iSegmentSplitActionService
                            .publishSegmentSplitByAssociation(value.getUuid(), null,
                                    publishDTO);
                }
            });
            iSegmentSplitActionService.publish(Sets.newHashSet(playlistUuid));
        }
    }

    @Transactional
    @Override
    public void copyPlaylistVersion(String fromPlaylistUuid, String toPlaylistUuid, boolean draft,
            List<ShowAttributeGroupDTO> showAttributeGroupDTOS) {
        logger.info("copy playlist<{}> to new playlist<{}>. draft:<{}> showAttributeGroup:<{}>",
                fromPlaylistUuid, toPlaylistUuid, draft, JSON.toJSONString(showAttributeGroupDTOS));
        List<PlaylistVersionInfo> versionInfos = iPlaylistVersionViewService
                .getPlaylistVersions(fromPlaylistUuid,
                        draft ? PlaylistStatusEnum.DRAFT.getStatusStr()
                                : PlaylistStatusEnum.RELEASE.getStatusStr(), null);
        PlaylistVersionInfo copyTagVersion = versionInfos.get(0);

        PlaylistVersionDTO playlistVersionDTO = new PlaylistVersionDTO();
        playlistVersionDTO.setPlaylistUuid(toPlaylistUuid);
        playlistVersionDTO.setShowAttributeGroups(showAttributeGroupDTOS);

        List<ContentDTO> contentList = copyTagVersion.getContentList().stream()
                .map(contentInfo -> {
                    ContentDTO contentDTO = new ContentDTO();
                    contentDTO.setContentType(contentInfo.getContentType());
                    contentDTO.setContentId(contentInfo.getContentId());
                    contentDTO.setTitle(contentInfo.getTitle());
                    contentDTO.setExtension(contentInfo.getExtension());
                    return contentDTO;
                }).collect(Collectors.toList());

        playlistVersionDTO.setContentList(contentList);
        List<PlaylistVersionInfo> releaseList = iPlaylistVersionViewService
                .getPlaylistVersions(toPlaylistUuid, PlaylistStatusEnum.DRAFT.getStatusStr(), null);
        updatePlaylistVersion(releaseList.get(0).getPlaylistVersionUuid(), playlistVersionDTO);
    }

    private void setShowType(String pplVersionUuid,
            List<ShowAttributeGroupDTO> showAttributeGroups) {
        showAttributeCombinationService
                .createShowAttributeCombination(pplVersionUuid, showAttributeGroups);
    }


    private void setContent(boolean autoPpl, String pplUuid, String pplVersionUuid,
            List<ContentDTO> contentDTOS) {
        if (autoPpl) {
            if (contentDTOS.isEmpty()) {
                SegmentInfo segmentInfo = iSegmentViewService.getAutomaticSegment();
                ContentDTO contentDTO = new ContentDTO();
                contentDTO.setContentType(ContentTypeEnum.SEGMENT.getName());
                contentDTO.setContentId(segmentInfo.getUuid());
                contentDTO.setExtension(JSON.toJSONString(segmentInfo));
                contentDTO.setTitle(segmentInfo.getTitle());
                contentDTOS.add(contentDTO);
            }
        } else {
            staticPplContentCheck(contentDTOS);
        }
        playlistVersionContentActionService
                .insertAssociationBatch(pplUuid, pplVersionUuid, contentDTOS);
    }

    private void staticPplContentCheck(List<ContentDTO> newContentDTOS) {
        boolean b = newContentDTOS.stream().filter(contentDTO -> ContentTypeEnum.SEGMENT.getName()
                .equals(contentDTO.getContentType()))
                .anyMatch(contentDTO -> {
                    SegmentDTO segmentDTO = JSON
                            .parseObject(contentDTO.getExtension(), SegmentDTO.class);
                    return !SegmentTypeEnum.API_SEGMENT.getName().equals(segmentDTO.getType())
                            && !SegmentTypeEnum.BASE_SEGMENT.getName().equals(segmentDTO.getType())
                            && !SegmentTypeEnum.TITLE_SEGMENT.getName()
                            .equals(segmentDTO.getType());
                });
        if (b) {
            throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    @Override
    public void onApplicationEvent(@NotNull ApplicationEvent applicationEvent) {
        if (applicationEvent instanceof CreateTplEvent) {
            CreateTplEvent createTplEvent = (CreateTplEvent) applicationEvent;
            createTpl(createTplEvent.getPlaylist(), createTplEvent.getPosPlaylistMappingDOS(),
                    createTplEvent.getAction());
        }
        if (applicationEvent instanceof ChangeTplEvent) {
            ChangeTplEvent changeTplEvent = (ChangeTplEvent) applicationEvent;
            createTpl(iPlaylistService.getById((String) changeTplEvent.getSource()),
                    iPosPlaylistMappingService
                            .getMappingsByPpl(false, (String) changeTplEvent.getSource()),
                    5);
        }
        if (applicationEvent instanceof OnSegmentSplitWeekChanged) {
            OnSegmentSplitWeekChanged changeEvent = (OnSegmentSplitWeekChanged) applicationEvent;
            modifySegmentSplitWeek((String) changeEvent.getSource(), changeEvent.getSplitByWeek());
        }
    }

    private void modifySegmentSplitWeek(String segmentUuid, Boolean splitByWeek) {
        List<PlaylistVersionContentAssociationDO> list = contentAssociationService
                .getAssociationsByContentUuid(segmentUuid);
        list.forEach(playlistVersionContentAssociationDO -> {
            Map<String, Object> objectMap = JSON
                    .parseObject(playlistVersionContentAssociationDO.getExtension(),
                            new TypeReference<Map<String, Object>>() {
                            });
            objectMap.put("split_by_week", splitByWeek);
            playlistVersionContentAssociationDO.setExtension(JSON.toJSONString(objectMap));
            contentAssociationService.updateById(playlistVersionContentAssociationDO);
        });
        if (Boolean.TRUE.equals(splitByWeek)) {
            SegmentInfo segmentInfo = iSegmentViewService.getSegment(segmentUuid);
            if (SegmentTypeEnum.BASE_SEGMENT.getName().equals(segmentInfo.getContentKind())) {

                iSegmentSplitActionService
                        .deleteDraftByAssociationUuid(
                                segmentInfo.getUuid(),
                                null);
                playlistVersionContentActionService.createDefaultSplit(true,
                        null,
                        null,
                        segmentInfo.getUuid());
            }
            if (SegmentTypeEnum.PLAYLIST_SEGMENT.getName().equals(segmentInfo.getContentKind())) {
                list.forEach(playlistVersionContentAssociationDO -> {
                    iSegmentSplitActionService
                            .deleteDraftByAssociationUuid(
                                    playlistVersionContentAssociationDO.getUuid(),
                                    null);
                    playlistVersionContentActionService.createDefaultSplit(true,
                            playlistVersionContentAssociationDO.getPlaylistUuid(),
                            playlistVersionContentAssociationDO.getPplVersionId(),
                            playlistVersionContentAssociationDO.getUuid());

                });
            }
        }
    }

    @Override
    @Transactional
    public List<PlaylistDO> modifyCplMeta(CplMetaDTO cplMetaDTO) {
        logger.info("receive message cpl-meta.data,dto:<{}>", JSON.toJSONString(cplMetaDTO));
        Set<String> pplUUIDs = new HashSet<>();

        // update meta in events
        modifyPlaylistVersionContentAssociation(cplMetaDTO.getCplUUID(), null, cplMetaDTO,
                pplUUIDs);

        // update meta in segment
        modifyPlaylistSplitContentAssociation(cplMetaDTO.getCplUUID(), null, cplMetaDTO, pplUUIDs);

        if (CollectionUtils.isNotEmpty(pplUUIDs)) {
            return iPlaylistService.selectBatchIds(pplUUIDs);
        } else {
            return Collections.emptyList();
        }
    }

    private void modifyPlaylistSplitContentAssociation(String contentUuid, String contentName,
            CplMetaDTO cplMetaDTO, Set<String> pplUUIDs) {
        Set<String> segmentSplitUUIDs = new HashSet<>();
        List<SegmentSplitContentAssociationDO> associationDOS = splitContentAssociationService
                .getSplitAssociationsByContentUuid(contentUuid);
        associationDOS.forEach(splitContentAssociationDO -> {
            JSONObject map = JSON.parseObject(splitContentAssociationDO.getExtension());
            if (contentName != null) {
                splitContentAssociationDO.setTitle(contentName);
                modifyContentName(map, contentName);
            }
            if (cplMetaDTO != null) {
                modifyContentMeta(map, cplMetaDTO);
            }
            splitContentAssociationDO.setExtension(JSON.toJSONString(map));

            segmentSplitUUIDs.add(splitContentAssociationDO.getSegmentSplitUuid());
        });

        if (CollectionUtils.isNotEmpty(segmentSplitUUIDs)) {
            splitContentAssociationService.updateBatchById(associationDOS);

            List<PlaylistSegmentSplitAssociationDO> playlistSegmentSplitAssociationDOS = splitAssociationService
                    .getSegmentSplitBySegmentSplitUuids(segmentSplitUUIDs);
            playlistSegmentSplitAssociationDOS.forEach(x -> pplUUIDs.add(x.getPlaylistUuid()));
        }
    }

    private void modifyPlaylistVersionContentAssociation(String contentUuid, String contentName,
            CplMetaDTO cplMetaDTO, Set<String> pplUUIDs) {
        List<PlaylistVersionContentAssociationDO> contentAssociationDOS = contentAssociationService
                .getAssociationsByContentUuid(contentUuid);
        contentAssociationDOS.forEach(contentAssociationDO -> {
            JSONObject map = JSON.parseObject(contentAssociationDO.getExtension());
            if (contentName != null) {
                contentAssociationDO.setTitle(contentName);
                modifyContentName(map, contentName);
            }
            if (cplMetaDTO != null) {
                modifyContentMeta(map, cplMetaDTO);
            }
            contentAssociationDO.setExtension(JSON.toJSONString(map));

            pplUUIDs.add(contentAssociationDO.getPlaylistUuid());
        });
        if (CollectionUtils.isNotEmpty(contentAssociationDOS)) {
            contentAssociationService.updateBatchById(contentAssociationDOS);
        }
    }

    private void modifyContentMeta(JSONObject map, CplMetaDTO cplMetaDTO) {
        List<Map<String, Object>> automation = map
                .getObject("automation", new TypeReference<List<Map<String, Object>>>() {
                });

        List<Map<String, Object>> list = new ArrayList<>();
        Object frameRate = map.get("frame_rate");
        long frameRateInt = 0;
        if (frameRate instanceof Integer) {
            frameRateInt = (Integer) frameRate;
        }

        if (!Objects.isNull(cplMetaDTO.getCreditOffset())) {
            map.put("producer_credit_offset", cplMetaDTO.getCreditOffset());
            Map<String, Object> objectMap = new HashMap<>();
            objectMap.put("name", "Credit Offset");
            objectMap.put("type", "credit_offset");
            Map<String, Long> integerMap = new HashMap<>();
            integerMap.put("offset_in_seconds", cplMetaDTO.getCreditOffset() / 1000);
            integerMap.put("offset_in_frames", cplMetaDTO.getCreditOffset() / 1000 * frameRateInt);
            objectMap.put("type_specific", integerMap);
            if (automation != null) {
                if (automation.stream().anyMatch(
                        stringObjectMap -> "credit_offset".equals(stringObjectMap.get("type")))) {
                    list.add(objectMap);
                }
            }
        }
        if (!Objects.isNull(cplMetaDTO.getHardLock())) {
            map.put("rating_hardlocked", cplMetaDTO.getHardLock());
        }
        if (!Objects.isNull(cplMetaDTO.getRatings())) {
            map.put("producer_ratings", cplMetaDTO.getRatings());
        }
        if (!Objects.isNull(cplMetaDTO.getIntermission())) {
            map.put("producer_intermission", cplMetaDTO.getIntermission());
            Map<String, Object> objectMap = new HashMap<>();
            objectMap.put("name", "Intermission");
            objectMap.put("type", "intermission");
            Map<String, Long> integerMap = new HashMap<>();
            integerMap.put("offset_in_seconds", cplMetaDTO.getIntermission() / 1000);
            integerMap.put("offset_in_frames", cplMetaDTO.getIntermission() / 1000 * frameRateInt);
            objectMap.put("type_specific", integerMap);
            if (automation != null) {
                if (automation.stream().anyMatch(
                        stringObjectMap -> "intermission".equals(stringObjectMap.get("type")))) {
                    list.add(objectMap);
                }
            }
        }

        if (automation == null || automation.isEmpty()) {
            return;
        }

        automation.forEach(stringObjectMap -> {
            boolean b = list.stream().noneMatch(stringObjectMap1 -> stringObjectMap1.get("type")
                    .equals(stringObjectMap.get("type")));
            if (b) {
                list.add(stringObjectMap);
            }
        });
        map.put("automation", list);
    }

    private void modifyContentName(JSONObject map, String name) {
        map.put("text", name);
    }
}