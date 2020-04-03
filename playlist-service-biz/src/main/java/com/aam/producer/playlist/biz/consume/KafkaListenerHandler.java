package com.aam.producer.playlist.biz.consume;

import com.aam.producer.lib.protocol.TaskDTO;
import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.enums.TPlaylistTaskActionEnum;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistDevOpsService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionActionService;
import com.aam.producer.playlist.biz.service.domain.IPosEventService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingSendService;
import com.aam.producer.playlist.biz.service.domain.ISegmentActionService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistDeleteService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSendService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSyncService;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.protocol.message.AutomationChangeDataDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeletionDTO;
import com.aam.producer.playlist.protocol.request.ComplexFlmDTO;
import com.aam.producer.playlist.protocol.request.CplMetaDTO;
import com.aam.producer.playlist.protocol.request.OrganizationNewDTO;
import com.aam.producer.playlist.protocol.request.PosDataDTO;
import com.aam.producer.playlist.protocol.request.PosDeletionDTO;
import com.aam.producer.playlist.protocol.request.PosMappingSentDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistActionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeletedDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeliveryDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistHashSyncedDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistSyncedDTO;
import com.aam.producer.playlist.protocol.request.TitleDataDTO;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

/**
 * kafka listener handler
 *
 * @author oliver.lo
 * @since 2019-07-16 09:29
 */
@Component
public class KafkaListenerHandler {

    private static final Logger logger = LoggerFactory.getLogger(KafkaListenerHandler.class);

    private final ITPlaylistSyncService playlistSyncService;
    private final ITPlaylistSendService playlistSendService;
    private final IPosEventService posEventService;
    private final IPlaylistVersionActionService playlistVersionActionService;
    private final IPosMappingSendService mappingSendService;
    private final IPosPlaylistMappingService playlistMappingService;
    private final ISegmentActionService iSegmentActionService;
    private final ITPlaylistDeleteService playlistDeleteService;
    private final IPlaylistDevOpsService playlistDevOpsService;

    @Autowired
    public KafkaListenerHandler(
            IPlaylistVersionActionService playlistVersionActionService,
            ITPlaylistSyncService playlistSyncService,
            ITPlaylistSendService playlistSendService,
            IPosEventService posEventService,
            IPosMappingSendService mappingSendService,
            IPosPlaylistMappingService playlistMappingService,
            ISegmentActionService iSegmentActionService,
            ITPlaylistDeleteService playlistDeleteService,
            IPlaylistDevOpsService playlistDevOpsService) {
        this.playlistVersionActionService = playlistVersionActionService;
        this.playlistSyncService = playlistSyncService;
        this.playlistSendService = playlistSendService;
        this.posEventService = posEventService;
        this.mappingSendService = mappingSendService;
        this.playlistMappingService = playlistMappingService;
        this.iSegmentActionService = iSegmentActionService;
        this.playlistDeleteService = playlistDeleteService;
        this.playlistDevOpsService = playlistDevOpsService;
    }

    @KafkaListener(topics = {"pos.data"})
    public void posConsumer(ConsumerRecord<String, String> record) {
        posData(record.value());
    }

    @KafkaListener(topics = {"playlist.sync-hash.response", "playlist.sync.response",
            "playlist.send.response", "playlist.action.response", "playlist.delete.response",
            "playlist.delete.notify", "pos.batch-mapping.response"})
    public void tsAgentConsumer(ConsumerRecord<String, String> record) {
        String topic = record.topic();
        String body = record.value();
        switch (topic) {
            case "playlist.sync-hash.response":
                tplHashResponse(body);
                break;
            case "playlist.sync.response":
                tplSyncedResponse(body);
                break;
            case "playlist.send.response":
                tplSendResponse(body);
                break;
            case "playlist.action.response":
                tplActionResponse(body);
                break;
            case "playlist.delete.response":
                tmsPlaylistDeleteResponse(body);
                break;
            case "playlist.delete.notify":
                tmsPlaylistDeletedPush(body);
                break;
            case "pos.batch-mapping.response":
                posMappingResponse(body);
                break;
            default:
                break;
        }
    }

    @KafkaListener(topics = {"pos-deletion.data", "playlist-deletion.data", "title.data",
            "cpl-meta.data", "user.organization.new", "automation-change.data", "complex-flm.data"})
    public void pro2Consumer(ConsumerRecord<String, String> record) {
        String topic = record.topic();
        String body = record.value();
        switch (topic) {
            case "pos-deletion.data":
                posDeleted(body);
                break;
            case "playlist-deletion.data":
                playlistDeletionData(body);
                break;
            case "title.data":
                titleData(body);
                break;
            case "cpl-meta.data":
                cplMetaChanged(body);
                break;
            case "user.organization.new":
                newOrganization(body);
                break;
            case "automation-change.data":
                automationChange(body);
                break;
            case "complex-flm.data":
                handleComplexFlmData(body);
                break;
            default:
                break;
        }
    }

    private void tplHashResponse(String message) {
        TaskDTO<TPlaylistHashSyncedDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<TPlaylistHashSyncedDTO>>() {
                });
        this.playlistSyncService.handlePlaylistHashResponse(taskDTO.getData());
    }

    private void tplSyncedResponse(String message) {
        TaskDTO<TPlaylistSyncedDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<TPlaylistSyncedDTO>>() {
                });
        this.playlistSyncService.handlePlaylistSyncedResponse(taskDTO.getData());
    }

    private void tplSendResponse(String message) {
        TaskDTO<TPlaylistDeliveryDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<TPlaylistDeliveryDTO>>() {
                });
        this.playlistSendService.handlePlaylistDeliveredResponse(taskDTO.getData());
    }

    private void tplActionResponse(String message) {
        TaskDTO<TPlaylistActionDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<TPlaylistActionDTO>>() {
                });
        this.playlistSendService.handlePlaylistActionedResponse(taskDTO.getData());
    }

    private void posData(String message) {
        TaskDTO<PosDataDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<PosDataDTO>>() {
                });
        if (taskDTO == null || taskDTO.getData() == null) {
            return;
        }
        this.posEventService.posDataHandler(taskDTO.getData());
    }

    private void titleData(String message) {
        TaskDTO<TitleDataDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<TitleDataDTO>>() {
                });
        this.posEventService.titleDataHandler(taskDTO.getData());
    }

    private void cplMetaChanged(String message) {
        TaskDTO<CplMetaDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<CplMetaDTO>>() {
                });
        CplMetaDTO cplMetaDTO = taskDTO.getData();
        OrgUtil.orgContenter.set(cplMetaDTO.getOrganizationId());
        List<PlaylistDO> playlistDOS = playlistVersionActionService.modifyCplMeta(cplMetaDTO);
        reCreateTpl(playlistDOS);
    }

    private void posMappingResponse(String message) {
        TaskDTO<PosMappingSentDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<PosMappingSentDTO>>() {
                });
        logger.info("receive a pos.batch-mapping.response,message:<{}>", message);
        mappingSendService.handleMappingSendResponse(taskDTO.getData());
    }

    private void posDeleted(String message) {
        TaskDTO<PosDeletionDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<PosDeletionDTO>>() {
                });
        logger.info("receive a pos-deletion.data,message:<{}>", message);
        PosDeletionDTO pos = taskDTO.getData();
        PosPlaylistMappingDO mapping = playlistMappingService.getOneMapping(pos.getPosUuid());
        if (mapping != null) {
            playlistMappingService.removeById(mapping.getId());
        }
    }

    private void newOrganization(String message) {
        TaskDTO<OrganizationNewDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<OrganizationNewDTO>>() {
                });
        logger.info("user.organization.new,message:<{}>", message);
        OrgUtil.orgContenter.set(taskDTO.getData().getUuid());
        iSegmentActionService.createRatingSegment();
    }

    private void playlistDeletionData(String message) {
        TaskDTO<TPlaylistDeletionDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<TPlaylistDeletionDTO>>() {
                });
        logger.info("receive a playlist-deletion.data,message:<{}>", message);
        TPlaylistDeletionDTO data = taskDTO.getData();
        playlistDeleteService.handleProTplDeletedRequest(data);
    }

    private void tmsPlaylistDeleteResponse(String message) {
        TaskDTO<TPlaylistDeletedDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<TPlaylistDeletedDTO>>() {
                });
        logger.info("receive a playlist.delete.response,message:<{}>", message);
        TPlaylistDeletedDTO data = taskDTO.getData();
        playlistDeleteService.handleProTplDeletedResponse(data);
    }

    private void tmsPlaylistDeletedPush(String message) {
        TaskDTO<TPlaylistDeletionDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<TPlaylistDeletionDTO>>() {
                });
        logger.info("receive a playlist.delete.notify,message:<{}>", message);
        TPlaylistDeletionDTO data = taskDTO.getData();
        playlistDeleteService.handleTmsTplDeletedPush(data);
    }

    private void automationChange(String message) {
        TaskDTO<AutomationChangeDataDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<AutomationChangeDataDTO>>() {
                });
        logger.info("receive a automation-change.data,message:<{}>", message);
        AutomationChangeDataDTO data = taskDTO.getData();
        OrgUtil.orgContenter.set(data.getOrganizationId());
        List<PlaylistDO> playlistDOS = playlistVersionActionService.automationChanged(data);
        reCreateTpl(playlistDOS);
    }

    private void reCreateTpl(List<PlaylistDO> playlistDOS) {
        if (CollectionUtils.isNotEmpty(playlistDOS)) {
            playlistDOS.stream().filter(playlistDO -> !PlaylistStatusEnum.DRAFT.getStatus()
                    .equals(playlistDO.getStatus()))
                    .forEach(playlistDO -> playlistVersionActionService
                            .createTpl(playlistDO, playlistMappingService
                                            .getMappingsByPpl(false, playlistDO.getUuid()),
                                    TPlaylistTaskActionEnum.CONTENT_CHANGED.getCode()));
        }
    }

    private void handleComplexFlmData(String message) {
        TaskDTO<ComplexFlmDTO> taskDTO = JSON
                .parseObject(message, new TypeReference<TaskDTO<ComplexFlmDTO>>() {
                });
        playlistDevOpsService.handleComplexFlmData(taskDTO.getData());
    }
}
