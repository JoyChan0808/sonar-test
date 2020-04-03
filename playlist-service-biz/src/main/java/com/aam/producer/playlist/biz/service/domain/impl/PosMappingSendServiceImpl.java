package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.convert.MessageQueueConvert;
import com.aam.producer.playlist.biz.convert.PosMappingConvert;
import com.aam.producer.playlist.biz.enums.PosMappingStatusEnum;
import com.aam.producer.playlist.biz.model.TransferTaskModel;
import com.aam.producer.playlist.biz.service.IPosMappingSendLogService;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingSendService;
import com.aam.producer.playlist.common.utils.TimeUtils;
import com.aam.producer.playlist.protocol.message.PosMappingBatchRequestDTO;
import com.aam.producer.playlist.protocol.message.PosMappingRequestDTO;
import com.aam.producer.playlist.protocol.request.PosMappingSentDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistActionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.repository.entity.PosMappingSendLogDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.alibaba.fastjson.JSON;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * pos mapping send service impl
 *
 * @author oliver.lo
 * @since 2019-09-18 15:34
 */
@Service
public class PosMappingSendServiceImpl implements IPosMappingSendService {

    private static final Logger logger = LoggerFactory.getLogger(PosMappingSendServiceImpl.class);

    private static final int BATCH_SEND_NUM = 50;
    private final static long NOT_DONE_AND_TRY_AGAIN = 5 * 60 * 1000;
    private final ConcurrentHashMap<String, TransferTaskModel> taskMap = new ConcurrentHashMap<>();

    private final IPosPlaylistMappingService playlistMappingService;
    private final IMessageQueueService mqService;
    private final IPosMappingSendLogService mappingSendLogService;

    @Autowired
    public PosMappingSendServiceImpl(
            IPosPlaylistMappingService playlistMappingService,
            IMessageQueueService mqService,
            IPosMappingSendLogService mappingSendLogService) {
        this.playlistMappingService = playlistMappingService;
        this.mqService = mqService;
        this.mappingSendLogService = mappingSendLogService;
    }

    @Override
    @Transactional
    public void handleMappingSendRequest(String pplUuid, String tplUuid, TPlaylistActionDTO dto,
            String complexUuid) {

        // get mappings by tpl
        List<PosPlaylistMappingDO> mappings = playlistMappingService
                .getMappingByTpl(pplUuid, tplUuid, complexUuid);

        // filter
        mappings = mappings.stream()
                .filter(x -> PosMappingStatusEnum.MARK.getTitle().equals(x.getMappingStatus()))
                .collect(Collectors.toList());

        // no mappings with this tpl
        if (CollectionUtils.isEmpty(mappings)) {
            logger.info(
                    "tpl sent to complex,but no pos mappings found to send,tpl_uuid:<{}>,complex_uuid:<{}>",
                    tplUuid, complexUuid);
            return;
        }

        if (Boolean.TRUE.equals(dto.getSuccess())) {
            // playlist send success,now send mapping
            posMappingsRequest(mappings, PosMappingStatusEnum.ASSIGNED.getTitle());
            // update send request to mapping table
            updatePosMappingStatusAfterRequest(mappings, tplUuid);
        } else {
            List<String> posUUIDs = mappings.stream().map(PosPlaylistMappingDO::getPosUuid)
                    .collect(Collectors.toList());
            // playlist send failed,update mappings
            int updated = playlistMappingService
                    .updateMappingSendInfo(posUUIDs, PosMappingStatusEnum.FAILED.getTitle(),
                            "tpl send failed:" + dto.getMessage(), tplUuid, false);
            if (updated == 0) {
                logger.warn(
                        "tpl send failed,and mapping status update failed.pos:<{}>",
                        posUUIDs);
            }
        }
    }

    @Override
    @Transactional
    public void handleMappingSendRequest(String pplUuid, TPlaylistTransferDTO dto) {

        String tplUuid = dto.getPlaylistUuid();
        // get mappings by tpl
        List<PosPlaylistMappingDO> mappings = playlistMappingService
                .getMappingByTpl(pplUuid, tplUuid, dto.getComplexUuid());

        // filter
        mappings = mappings.stream()
                .filter(x -> PosMappingStatusEnum.MARK.getTitle().equals(x.getMappingStatus()))
                .collect(Collectors.toList());

        // no mappings with this tpl
        if (CollectionUtils.isEmpty(mappings)) {
            logger.info(
                    "tpl sent to complex,but no pos mappings found to send,tpl_uuid:<{}>,complex_uuid:<{}>",
                    tplUuid, dto.getComplexUuid());
            return;
        }

        posMappingsRequest(mappings, PosMappingStatusEnum.ASSIGNED.getTitle());

        updatePosMappingStatusAfterRequest(mappings, tplUuid);
    }

    @Override
    @Transactional
    public void handleMappingSendRequest(List<PosPlaylistMappingDO> posMappings, String action) {

        if (CollectionUtils.isNotEmpty(posMappings)) {

            String tplUuid = posMappings.get(0).getTplUuid();

            posMappingsRequest(posMappings, action);

            updatePosMappingStatusAfterRequest(posMappings, tplUuid);
        }
    }

    @Override
    @Transactional
    public void handleMappingSendResponse(PosMappingSentDTO dto) {
        String receiptUuid = dto.getReceiptUuid();
        TransferTaskModel taskModel = taskMap.getOrDefault(receiptUuid, null);
        boolean needToRemoveTask = true;
        PosMappingSendLogDO oneLog = mappingSendLogService.getOneLog(receiptUuid);
        if (oneLog == null) {
            logger.warn("receive pos.batch-mapping.response,but not exist in log,dto:<{}>", dto);
            return;
        }

        // update log
        Long attempted = TimeUtils.changeMillisecondTimestamp(dto.getAttemptedAt());
        PosMappingConvert.MAPPER.posMappingSendLogFilling(oneLog, dto, attempted);

        // get sent mappings
        List<PosMappingRequestDTO> mappings = JSON
                .parseArray(oneLog.getMapping(), PosMappingRequestDTO.class);

        if (CollectionUtils.isNotEmpty(mappings)) {
            Set<String> posUUIDs = mappings.stream().map(PosMappingRequestDTO::getPosUuid)
                    .collect(Collectors.toSet());
            String tplUuid = mappings.get(0).getTplUuid();

            List<PosPlaylistMappingDO> allBatches = playlistMappingService
                    .getMappingsByPos(false, new ArrayList<>(posUUIDs));
            List<PosPlaylistMappingDO> notChangedMappings = allBatches.stream()
                    .filter(x -> Objects.equals(x.getTplUuid(), tplUuid))
                    .collect(Collectors.toList());

            // if failed try again when mappings all valid
            if (PosMappingStatusEnum.FAILED.getTitle().equals(dto.getMappingStatus())) {
                if (posUUIDs.size() == notChangedMappings.size()) {
                    // handle retry by schedule job
                    /*PosMappingBatchRequestDTO requestDTO = MessageQueueConvert.MAPPER
                            .toPosMappingBatchDTO(oneLog.getComplexId(), mappings,
                                    oneLog.getReceiptUuid());
                    mqService.posMappingRequest(requestDTO);
                    // update task
                    if (taskModel != null) {
                        needToRemoveTask = false;
                        taskModel.setLatestTriggerTime(System.currentTimeMillis());
                        taskModel.increaseTriggerNum();
                    }*/
                    needToRemoveTask = false;
                    logger.warn("receive pos.batch-mapping.response failed,try later,response:<{}>",
                            dto);
                } else if (CollectionUtils.isNotEmpty(notChangedMappings)) {
                    handleMappingSendRequest(notChangedMappings,
                            tplUuid == null ? PosMappingStatusEnum.UNASSIGNED.getTitle()
                                    : PosMappingStatusEnum.ASSIGNED.getTitle());
                    logger.warn(
                            "receive pos.batch-mapping.response failed,some mappings had changed,not changed mappings try again,response:<{}>,not changed mappings:<{}>",
                            dto, notChangedMappings);
                    // invalid this receipt log
                    PosMappingConvert.MAPPER.invalidPosMappingSendLog(oneLog);
                } else {
                    logger.warn(
                            "receive pos.batch-mapping.response failed,but mappings had all changed,so let it go,response:<{}>",
                            dto);
                    // invalid this receipt log
                    PosMappingConvert.MAPPER.invalidPosMappingSendLog(oneLog);
                }
            }

            if (needToRemoveTask && taskModel != null) {
                taskMap.remove(receiptUuid);
            }

            mappingSendLogService.updateById(oneLog);
        }
    }

    @Override
    public void handleTimeoutTask() {
        int taskSumTotal = 0;
        int timeoutCount = 0;
        long now = System.currentTimeMillis();
        Iterator<Entry<String, TransferTaskModel>> iterator = taskMap.entrySet().iterator();
        while (iterator.hasNext()) {
            Entry<String, TransferTaskModel> next = iterator.next();
            String receiptUuid = next.getKey();
            TransferTaskModel taskDetail = next.getValue();
            Long latestTriggerTime = taskDetail.getLatestTriggerTime();
            if (latestTriggerTime == null
                    || (now - latestTriggerTime) > NOT_DONE_AND_TRY_AGAIN) {
                PosMappingSendLogDO oneLog = mappingSendLogService.getOneLog(receiptUuid);
                if (oneLog == null || PosMappingStatusEnum.SUCCEEDED.getTitle()
                        .equals(oneLog.getStatus()) || PosMappingStatusEnum.INVALID.getTitle()
                        .equals(oneLog.getStatus())) {
                    logger.info(
                            "This task send timeout,but db status is done or invalid,remove this task,receipt_uuid:<{}>",
                            receiptUuid);
                    iterator.remove();
                } else {
                    List<PosMappingRequestDTO> mappings = JSON
                            .parseArray(oneLog.getMapping(), PosMappingRequestDTO.class);
                    List<String> posUUIDs = mappings.stream().map(PosMappingRequestDTO::getPosUuid)
                            .collect(Collectors.toList());
                    String tplUuid = mappings.get(0).getTplUuid();
                    // get pos mappings
                    List<PosPlaylistMappingDO> allBatches = playlistMappingService
                            .getMappingsByPos(false, posUUIDs);
                    // invalid when mapping tpl uuid is not equal with send request
                    List<String> validPosUUIDs = allBatches.stream()
                            .filter(x -> Objects.equals(tplUuid, x.getTplUuid()))
                            .map(PosPlaylistMappingDO::getPosUuid).collect(Collectors.toList());
                    posUUIDs.retainAll(validPosUUIDs);
                    mappings = mappings.stream()
                            .filter(x -> CollectionUtils.isNotEmpty(posUUIDs) && posUUIDs
                                    .contains(x.getPosUuid())).collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(mappings)) {
                        logger.info(
                                "This task send timeout,but db status is not done yet,do it again,receipt_uuid:<{}>",
                                receiptUuid);
                        PosMappingBatchRequestDTO requestDTO = MessageQueueConvert.MAPPER
                                .toPosMappingBatchDTO(oneLog.getComplexId(), mappings,
                                        oneLog.getReceiptUuid());
                        mqService.posMappingRequest(requestDTO);
                        taskDetail.setLatestTriggerTime(System.currentTimeMillis());
                        taskDetail.increaseTriggerNum();
                        timeoutCount++;
                    } else {
                        logger.info(
                                "This task send timeout,and db status is not done,but no valid pos mapping,remove this task,receipt_uuid:<{}>",
                                receiptUuid);
                        iterator.remove();
                    }
                }
            } else {
                logger.info("This pos mapping send task is pending,receipt_uuid:<{}>",
                        receiptUuid);
            }
            taskSumTotal++;
        }
        logger.info(
                "[Scheduled] checking pos mapping send task done,task_num_total:<{}>,timeout_count:<{}>",
                taskSumTotal, timeoutCount);
    }

    private void posMappingsRequest(List<PosPlaylistMappingDO> mappingDOS, String action) {
        Map<String, List<PosPlaylistMappingDO>> mapOfComplex = mappingDOS.stream()
                .collect(Collectors.groupingBy(PosPlaylistMappingDO::getComplexUuid));
        mapOfComplex.forEach((complexUuid, DOs) -> {
            int size = DOs.size();
            if (size <= BATCH_SEND_NUM) {
                doRequest(complexUuid, DOs, size, action);
            } else {
                List<List<PosPlaylistMappingDO>> partition = ListUtils
                        .partition(DOs, BATCH_SEND_NUM);
                for (List<PosPlaylistMappingDO> pDOS : partition) {
                    doRequest(complexUuid, pDOS, pDOS.size(), action);
                }
            }
        });
    }

    private void doRequest(String complexUuid, List<PosPlaylistMappingDO> mappingDOS, int size,
            String action) {
        List<PosMappingRequestDTO> mappings = new ArrayList<>(size);
        for (PosPlaylistMappingDO aDo : mappingDOS) {
            mappings.add(MessageQueueConvert.MAPPER
                    .toPosMappingDTO(aDo.getPosUuid(), aDo.getTplUuid(), action));
        }
        PosMappingBatchRequestDTO requestDTO = MessageQueueConvert.MAPPER
                .toPosMappingBatchDTO(complexUuid, mappings, null);
        mqService.posMappingRequest(requestDTO);

        // create send log
        PosMappingSendLogDO logDO = PosMappingConvert.MAPPER.toPosMappingSendLogDO(requestDTO);
        mappingSendLogService.save(logDO);

        // create send task
        long now = System.currentTimeMillis();
        TransferTaskModel taskModel = new TransferTaskModel();
        taskModel.setCreated(now);
        taskModel.setLatestTriggerTime(now);
        taskModel.increaseTriggerNum();
        taskMap.putIfAbsent(requestDTO.getReceiptUuid(), taskModel);
    }

    private void updatePosMappingStatusAfterRequest(List<PosPlaylistMappingDO> posMappings,
            String tplUuid) {
        List<String> posUUIDs = posMappings.stream().map(PosPlaylistMappingDO::getPosUuid)
                .collect(Collectors.toList());
        // update send request to mapping table
        int updated = playlistMappingService
                .updateMappingSendInfo(posUUIDs, PosMappingStatusEnum.REQUEST.getTitle(),
                        PosMappingStatusEnum.REQUEST.getTitle(), tplUuid, false);
        if (updated == 0) {
            logger.warn(
                    "pos mapping request have sent,but mapping status update failed.pos:<{}>",
                    posUUIDs);
        }
    }
}
