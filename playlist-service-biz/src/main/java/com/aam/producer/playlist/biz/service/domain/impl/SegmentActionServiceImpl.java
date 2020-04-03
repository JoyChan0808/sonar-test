package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.biz.event.CreateBaseSegmentRootSplit;
import com.aam.producer.playlist.biz.event.OnSegmentSplitWeekChanged;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.ISegmentEventService;
import com.aam.producer.playlist.biz.service.ISegmentService;
import com.aam.producer.playlist.biz.service.domain.ISegmentActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitActionService;
import com.aam.producer.playlist.biz.util.DefaultSchedulingConfigurer;
import com.aam.producer.playlist.biz.util.EventPublish;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.common.utils.InitUtils;
import com.aam.producer.playlist.protocol.request.OrganizationDTO;
import com.aam.producer.playlist.protocol.request.PublishDTO;
import com.aam.producer.playlist.protocol.request.PublishSegmentDTO;
import com.aam.producer.playlist.protocol.request.SegmentDTO;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.aam.producer.playlist.sal.client.IOrgFacadeClient;
import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;
import com.alibaba.fastjson.JSON;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SegmentActionServiceImpl implements ISegmentActionService,
        ApplicationListener<ContextRefreshedEvent> {

    private final static Logger logger = LoggerFactory.getLogger(SegmentActionServiceImpl.class);
    private final ISegmentService iSegmentService;
    private final IPlaylistVersionContentAssociationService associationService;
    private final ISegmentEventService segmentEventService;
    private final EventPublish eventPublish;
    private final ISegmentSplitActionService iSegmentSplitActionService;
    private final DefaultSchedulingConfigurer schedulingConfigurer;

    @Autowired
    public SegmentActionServiceImpl(ISegmentService iSegmentService,
                                    IPlaylistVersionContentAssociationService associationService,
                                    ISegmentEventService segmentEventService,
                                    EventPublish eventPublish,
                                    ISegmentSplitActionService iSegmentSplitActionService,
                                    DefaultSchedulingConfigurer schedulingConfigurer) {
        this.iSegmentService = iSegmentService;
        this.associationService = associationService;
        this.segmentEventService = segmentEventService;
        this.eventPublish = eventPublish;
        this.iSegmentSplitActionService = iSegmentSplitActionService;
        this.schedulingConfigurer = schedulingConfigurer;
    }

    @Transactional
    @Override
    public String createSegment(SegmentDTO segmentDTO) {
        logger.info("Create a segment. detail:<{}>", JSON.toJSONString(segmentDTO));
        SegmentTypeEnum segmentTypeEnum = SegmentTypeEnum.getByCode(segmentDTO.getType());
        if (SegmentTypeEnum.RATING_SEGMENT.equals(segmentTypeEnum)) {
            SegmentDO segmentDO = iSegmentService.getRatingSegment();
            if (segmentDO != null) {
                logger.info("Rating Segment already exists. detail:<{}> orgId:<{}>",
                        JSON.toJSONString(segmentDTO), OrgUtil.orgContenter.get());
                return segmentDO.getUuid();
            }
        }
        checkCreateSegmentDTO(segmentDTO);
        String uuid = UUID.randomUUID().toString();
        iSegmentService.createSegment(segmentDTO, uuid);
        segmentEventService.sendDataEvent(uuid, false);
        if (SegmentTypeEnum.BASE_SEGMENT.equals(segmentTypeEnum)) {
            eventPublish.publishEvent(
                    new CreateBaseSegmentRootSplit(uuid, segmentDTO.getSplitByWeek()));
        }
        return uuid;
    }

    @Transactional
    @Override
    public void updateSegment(String segmentUuid, SegmentDTO segmentDTO) {
        logger.info("update  segment<{}>. detail:<{}>", segmentUuid, JSON.toJSONString(segmentDTO));
        SegmentDO segmentDO = iSegmentService.getById(segmentUuid);
        boolean isChanged = !Objects
                .equals(segmentDO.getSplitByWeek(), segmentDTO.getSplitByWeek());
        iSegmentService.updateSegment(segmentUuid, segmentDTO.getTitle(), segmentDTO.getPurpose(),
                segmentDTO.getSplitByWeek());
        checkUpdateSegmentDTO(segmentDTO);
        segmentEventService.sendDataEvent(segmentUuid, false);
        if (isChanged) {
            eventPublish.publishEvent(
                    new OnSegmentSplitWeekChanged(segmentUuid, segmentDTO.getSplitByWeek()));
        }
    }

    @Transactional
    @Override
    public void deleteSegment(String segmentUuid) {
        logger.info("delete  segment<{}>. ", segmentUuid);
        checkDeleteSegment(segmentUuid);
        iSegmentService.deleteSegment(segmentUuid);
        segmentEventService.sendDataEvent(segmentUuid, true);
    }

    @Override
    public void createRatingSegment() {
        logger.info("Create a Rating Segment for org<{}>. ", OrgUtil.orgContenter.get());
        SegmentDTO segmentDTO = new SegmentDTO();
        segmentDTO.setType(SegmentTypeEnum.RATING_SEGMENT.getCode());
        segmentDTO.setTitle("Rating");
        createSegment(segmentDTO);
    }

    @Override
    public void synchronizedSegment() {
        logger.info("synchronized all segment to producer-view. ");
        List<SegmentDO> segmentDOS = iSegmentService.getAllSegment();
        segmentDOS.forEach(segmentDO -> {
            OrgUtil.orgContenter.set(segmentDO.getOrganizationId());
            segmentEventService.sendDataEvent(segmentDO.getUuid(), false);
        });
    }

    @Override
    public void cancelWeek(String segmentUuid) {
        logger.info("cancel split week for segment<{}>.", segmentUuid);
        iSegmentService.updateSegment(segmentUuid, null, null, false);
    }

    @Override
    public void batchPublishSegment(PublishSegmentDTO publishDTO) {
        PublishDTO publishDTO1 = new PublishDTO();
        publishDTO1.setOnlyUpdateStatus(true);
        if (publishDTO.getPublishTime() != null) {
            publishDTO1.setPublishLater(true);
            publishDTO1.setPublishTime(publishDTO.getPublishTime());
        } else {
            publishDTO1.setPublishLater(false);
        }
        Set<String> pplUuids = new HashSet<>();
        Set<String> playlistSegmentUuids = new HashSet<>();
        Set<String> playlistSegmentUuids1 = new HashSet<>();
        AtomicInteger count = new AtomicInteger();
        publishDTO.getTitleInfos().forEach(titleInfoDTO -> {
            pplUuids.addAll(titleInfoDTO.getPplSegmentMap().keySet());
            titleInfoDTO.getPplSegmentMap().forEach(
                    (s, segmentDTO1s) -> segmentDTO1s.forEach(segmentDTO1 -> {
                        if (!SegmentStatusEnum.RELEASE.getStatusStr()
                                .equals(segmentDTO1.getStatus())) {
                            if (
                                    SegmentTypeEnum.RATING_SEGMENT.getName()
                                            .equals(segmentDTO1.getContentKind()) ||
                                            SegmentTypeEnum.TITLE_SEGMENT.getName()
                                                    .equals(segmentDTO1.getContentKind()) ||
                                            SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                                                    .equals(segmentDTO1.getContentKind())
                            ) {
                                count.incrementAndGet();
                            } else if (
                                    SegmentTypeEnum.PLAYLIST_SEGMENT.getName()
                                            .equals(segmentDTO1.getContentKind())
                            ) {
                                if (!playlistSegmentUuids
                                        .contains(segmentDTO1.getContentAssociationUuid())) {
                                    count.incrementAndGet();
                                    playlistSegmentUuids
                                            .add(segmentDTO1.getContentAssociationUuid());
                                }
                            } else if (
                                    SegmentTypeEnum.BASE_SEGMENT.getName()
                                            .equals(segmentDTO1.getContentKind())
                            ) {
                                count.incrementAndGet();
                            }
                        }
                    }));
        });
        CountDownLatch countDownLatch = new CountDownLatch(count.get());
        publishDTO.getTitleInfos()
                .forEach(titleInfoDTO -> {
                    titleInfoDTO.getPplSegmentMap().forEach((s, segmentDTO1s) -> {
                        segmentDTO1s.forEach(segmentDTO1 -> {
                            if (!SegmentStatusEnum.RELEASE.getStatusStr()
                                    .equals(segmentDTO1.getStatus())) {
                                if (
                                        SegmentTypeEnum.RATING_SEGMENT.getName()
                                                .equals(segmentDTO1.getContentKind()) ||
                                                SegmentTypeEnum.TITLE_SEGMENT.getName()
                                                        .equals(segmentDTO1.getContentKind()) ||
                                                SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                                                        .equals(segmentDTO1.getContentKind())
                                ) {
                                    iSegmentSplitActionService
                                            .publishSegmentSplitByAssociation(
                                                    segmentDTO1.getContentAssociationUuid(),
                                                    titleInfoDTO.getUuid(),
                                                    publishDTO1);
                                } else if (
                                        SegmentTypeEnum.PLAYLIST_SEGMENT.getName()
                                                .equals(segmentDTO1.getContentKind())
                                ) {
                                    if (!playlistSegmentUuids1
                                            .contains(segmentDTO1.getContentAssociationUuid())) {
                                        iSegmentSplitActionService
                                                .publishSegmentSplitByAssociation(
                                                        segmentDTO1.getContentAssociationUuid(),
                                                        null,
                                                        publishDTO1);
                                        playlistSegmentUuids1
                                                .add(segmentDTO1.getContentAssociationUuid());
                                    }
                                } else if (
                                        SegmentTypeEnum.BASE_SEGMENT.getName()
                                                .equals(segmentDTO1.getContentKind())
                                ) {
                                    iSegmentSplitActionService
                                            .publishSegmentSplitByAssociation(
                                                    segmentDTO1.getContentId(), null,
                                                    publishDTO1);
                                }
                            }
                        });
                    });
                });
        if (publishDTO1.getPublishLater()) {
            schedulingConfigurer.addTriggerTask(UUID.randomUUID().toString(),
                    publishDTO1.getPublishTime(), () -> {
                        try {
                            countDownLatch.await(20, TimeUnit.SECONDS);
                        } catch (InterruptedException e) {
                            logger.error("Batch publish segment fail.", e);
                        }
                        iSegmentSplitActionService.publish(pplUuids);
                    });
        } else {
            iSegmentSplitActionService.publish(pplUuids);
        }
    }

    private void checkDeleteSegment(String segmentUuid) {
        if (associationService.contains(segmentUuid)) {
            throw new BizException(ResultCodeEnum.DELETE_SEGMENT_FAIL);
        }
    }

    private void checkCreateSegmentDTO(SegmentDTO segmentDTO) {
        SegmentTypeEnum segmentTypeEnum = SegmentTypeEnum.getByCode(segmentDTO.getType());
        if (segmentTypeEnum == null || segmentTypeEnum == SegmentTypeEnum.AUTOMATIC_SEGMENT) {
            throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
        List<SegmentDO> segmentDOS = iSegmentService.getSegmentByTitle(segmentDTO.getTitle());
        if (!segmentDOS.isEmpty()) {
            throw new BizException(ResultCodeEnum.REPETITION_OF_SEGMENT_NAMES);
        }
    }

    private void checkUpdateSegmentDTO(SegmentDTO segmentDTO) {
        List<SegmentDO> segmentDOS = iSegmentService.getSegmentByTitle(segmentDTO.getTitle());
        if (segmentDOS.size() > 1) {
            throw new BizException(ResultCodeEnum.REPETITION_OF_SEGMENT_NAMES);
        }
    }

    private String genSegmentUuid(SegmentDTO segmentDTO) {
        SegmentTypeEnum segmentTypeEnum = SegmentTypeEnum.getByCode(segmentDTO.getType());
        if (SegmentTypeEnum.API_SEGMENT.equals(segmentTypeEnum)) {
            return InitUtils
                    .genUuid(SegmentTypeEnum.API_SEGMENT.getName(), segmentDTO.getTitle(), true);
        } else {
            return UUID.randomUUID().toString();
        }
    }

    @Override
    public void onApplicationEvent(ContextRefreshedEvent contextRefreshedEvent) {
        IOrgFacadeClient iOrgFacadeClient = contextRefreshedEvent.getApplicationContext()
                .getBean(IOrgFacadeClient.class);
        List<OrganizationDTO> organizationDTOS = null;
        try {
            organizationDTOS = iOrgFacadeClient.getOrganizations();
        } catch (Exception e) {
            logger.error("Init rating segment exception", e);
        }
        logger.info("Init rating segment for orgs<{}>",JSON.toJSONString(organizationDTOS));
        if (organizationDTOS != null) {
            organizationDTOS.forEach(organizationDTO -> {
                OrgUtil.orgContenter.set(organizationDTO.getUuid());
                try {
                    createRatingSegment();
                } catch (Exception e) {
                    logger.error("Init rating segment for organization<" + organizationDTO.getUuid()
                            + "> exception.", e);
                }
            });
        }
    }
}
