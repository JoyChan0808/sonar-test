package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.convert.MessageQueueConvert;
import com.aam.producer.playlist.biz.convert.TPlaylistSendConvert;
import com.aam.producer.playlist.biz.enums.TPlaylistActionEnum;
import com.aam.producer.playlist.biz.enums.TPlaylistActionStatusEnum;
import com.aam.producer.playlist.biz.model.TransferTaskModel;
import com.aam.producer.playlist.biz.service.IPlaylistSendLogService;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingSendService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistLogTaskService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSendService;
import com.aam.producer.playlist.protocol.message.TPlaylistSendRequestDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistActionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeliveryDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.repository.entity.PlaylistSendLogDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.alibaba.fastjson.JSON;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * playlist send service impl
 *
 * @author oliver.lo
 * @since 2019/5/15 10:55 AM
 */
@Service
public class TPlaylistSendServiceImpl implements ITPlaylistSendService {

    private static final Logger logger = LoggerFactory.getLogger(TPlaylistSendServiceImpl.class);

    // cause no batch number,so use this to protect batch sync in one minute
    private final static long NOT_DONE_AND_TRY_AGAIN = 5 * 60 * 1000;
    private final ConcurrentHashMap<Serializable, TransferTaskModel> taskMap = new ConcurrentHashMap<>();

    private final IPlaylistSendLogService playlistSendLogService;
    private final ITmsPlaylistService tmsPlaylistService;
    private final IComplexFacadeClient complexFacadeClient;
    private final IPosMappingSendService mappingSendService;
    private final IMessageQueueService mqService;
    private final ITPlaylistLogTaskService taskService;
    private final IPosPlaylistMappingService posPlaylistMappingService;

    @Autowired
    public TPlaylistSendServiceImpl(
            IPlaylistSendLogService playlistSendLogService,
            ITmsPlaylistService tmsPlaylistService,
            IComplexFacadeClient complexFacadeClient,
            IPosMappingSendService mappingSendService,
            IMessageQueueService mqService,
            ITPlaylistLogTaskService taskService,
            IPosPlaylistMappingService posPlaylistMappingService) {
        this.playlistSendLogService = playlistSendLogService;
        this.tmsPlaylistService = tmsPlaylistService;
        this.complexFacadeClient = complexFacadeClient;
        this.mappingSendService = mappingSendService;
        this.mqService = mqService;
        this.taskService = taskService;
        this.posPlaylistMappingService = posPlaylistMappingService;
    }

    @Override
    public void handlePlaylistSendRequest(TPlaylistTransferDTO dto, TmsPlaylistDO toBeSend) {

        if (toBeSend == null) {
            toBeSend = this.tmsPlaylistService.getOneTplScopeInPro(dto.getPlaylistUuid());
        }
        if (toBeSend == null) {
            logger.warn("someone trigger tpl sending,but this tpl is not exist,tpl_uuid:<{}>",
                    dto.getPlaylistUuid());
            return;
        }

        // save send log
        PlaylistSendLogDO sendLogDO = saveOrUpdateSendLog(dto, toBeSend);

        // send playlist send message
        if (sendLogDO != null) {
            if (Boolean.TRUE.equals(dto.getFastTrack())) {
                sendingTplToSite(sendLogDO, null);
            } else {
                sendPlaylistSendRequest(sendLogDO);
            }
        }
    }

    @Override
    public void handlePlaylistDeliveredResponse(TPlaylistDeliveryDTO dto) {
        logger.info("received message playlist.send.response,response:<{}>", dto);

        PlaylistSendLogDO sendLogDO = getSendLogFromDBOrTaskMap(dto.getPlaylistUuid(),
                dto.getComplexUuid(), null, dto.getReceiptUuid(), null);
        if (sendLogDO != null && !TPlaylistActionStatusEnum.DONE
                .equals(TPlaylistActionStatusEnum.getByCode(sendLogDO.getStatus()))) {
            TPlaylistSendConvert.MAPPER.playlistSendLogDODelivered(sendLogDO, dto);
            this.playlistSendLogService.updateById(sendLogDO);

            if (Boolean.FALSE.equals(dto.getDeliveryStatus())) {
                // handle retry by schedule job
                /*TPlaylistTransferDTO transferDTO = PosMappingConvert.MAPPER
                        .toPlaylistTransferDTO(sendLogDO.getComplexId(), sendLogDO.getDeviceUuid(),
                                sendLogDO.getPlaylistUuid(), sendLogDO.getPosId(), true);
                handlePlaylistSendRequest(transferDTO, null);*/
                logger.warn("playlist.send.response failed,retry later,response:<{}>", dto);
            }
        } else {
            // if not have valid playlist send
            logger.warn(
                    "received message playlist.send.response,but can't find send log marked before,receipt_uuid:<{}>,complex_uuid:<{}>,device_uuid:<{}>,playlist_uuid:<{}>",
                    dto.getReceiptUuid(), dto.getComplexUuid(), dto.getDeviceUuid(),
                    dto.getPlaylistUuid());
        }
    }

    @Override
    public void handlePlaylistActionedResponse(TPlaylistActionDTO dto) {

        logger.info("received message playlist.action.response,response:<{}>", dto);

        PlaylistSendLogDO sendLogDO = getSendLogFromDBOrTaskMap(null, null, null, null,
                dto.getActionId());

        if (sendLogDO != null && !TPlaylistActionStatusEnum.DONE
                .equals(TPlaylistActionStatusEnum.getByCode(sendLogDO.getStatus()))) {
            TPlaylistSendConvert.MAPPER.playlistSendLogDOActioned(sendLogDO, dto);
            this.playlistSendLogService.updateById(sendLogDO);

            // send pos mapping to tms
            mappingSendService
                    .handleMappingSendRequest(sendLogDO.getPplUuid(), sendLogDO.getPlaylistUuid(),
                            dto, sendLogDO.getComplexId());

            if (Boolean.FALSE.equals(dto.getSuccess())) {
                // handle retry by schedule job
                /*TPlaylistTransferDTO transferDTO = PosMappingConvert.MAPPER
                        .toPlaylistTransferDTO(sendLogDO.getComplexId(), sendLogDO.getDeviceUuid(),
                                sendLogDO.getPlaylistUuid(), sendLogDO.getPosId(), true);
                handlePlaylistSendRequest(transferDTO, null);*/
                logger.warn("playlist.action.response failed,retry later,response:<{}>", dto);
            } else {
                //remove task
                taskMap.remove(sendLogDO);
                complexFacadeClient.lmsCacheEvict(sendLogDO.getComplexId());
            }
        } else {
            // if not have valid playlist send
            logger.warn(
                    "received message playlist.action.response,but can't find send log marked before,action_id:<{}>",
                    dto.getActionId());
        }
    }

    @Override
    public void handlePlaylistChanged(TmsPlaylistDO tmsPlaylistDO) {
        List<PlaylistSendLogDO> latestPlaylistSendLogs = this.playlistSendLogService
                .getLatestPlaylistSendLogs(null, tmsPlaylistDO.getPlaylistUuid());
        List<PlaylistSendLogDO> saveOrUpdate = new ArrayList<>();
        latestPlaylistSendLogs.forEach(x -> {
            TPlaylistActionStatusEnum statusEnum = TPlaylistActionStatusEnum
                    .getByCode(x.getStatus());
            if (TPlaylistActionStatusEnum.DONE.equals(statusEnum)) {
                PlaylistSendLogDO changeLog = TPlaylistSendConvert.MAPPER
                        .toPlaylistSendLogDO(null, x.getComplexId(), x.getDeviceUuid(),
                                TPlaylistActionEnum.UPDATE.getCode(), tmsPlaylistDO);
                saveOrUpdate.add(changeLog);
            } else {
                TPlaylistSendConvert.MAPPER.playlistSendLogDOFilling(x, null, null, tmsPlaylistDO);
                saveOrUpdate.add(x);
            }
        });
        if (CollectionUtils.isNotEmpty(saveOrUpdate)) {
            playlistSendLogService.saveOrUpdateBatch(saveOrUpdate);
            saveOrUpdate.forEach(x -> sendingTplToSite(x, null));
        }
    }

    @Override
    public void handleTimeoutTask() {
        int taskSumTotal = 0;
        int timeoutCount = 0;
        long now = System.currentTimeMillis();
        Iterator<Entry<Serializable, TransferTaskModel>> iterator = taskMap.entrySet().iterator();
        while (iterator.hasNext()) {
            Entry<Serializable, TransferTaskModel> next = iterator.next();
            PlaylistSendLogDO taskLog = (PlaylistSendLogDO) next.getKey();
            String complexId = taskLog.getComplexId();
            String playlistUuid = taskLog.getPlaylistUuid();
            TransferTaskModel taskDetail = next.getValue();
            Long latestTriggerTime = taskDetail.getLatestTriggerTime();
            if (latestTriggerTime == null
                    || (now - latestTriggerTime) > NOT_DONE_AND_TRY_AGAIN) {
                List<PlaylistSendLogDO> dbLogs = playlistSendLogService
                        .getNotDonePlaylistSendLogs(complexId, playlistUuid);
                PlaylistSendLogDO dbLog = dbLogs.stream().findAny().orElse(null);
                if (dbLog == null) {
                    logger.info(
                            "This task send timeout,but db status is done,remove this task,complex_id:<{}>,tpl_uuid:<{}>",
                            complexId, playlistUuid);
                    iterator.remove();
                } else {
                    List<PosPlaylistMappingDO> posMappings = posPlaylistMappingService
                            .getMappingByTpl(dbLog.getPplUuid(), dbLog.getPlaylistUuid(),
                                    dbLog.getComplexId());
                    if (CollectionUtils.isEmpty(posMappings)) {
                        logger.info(
                                "This task send timeout,and db status is not done,but no pos mapping match it,remove this task,complex_id:<{}>,tpl_uuid:<{}>",
                                complexId, playlistUuid);
                        iterator.remove();
                    } else {
                        logger.info(
                                "This task send timeout,but db status is not done yet,do it again,complex_id:<{}>,tpl_uuid:<{}>",
                                complexId, playlistUuid);
                        sendingTplToSite(dbLog, taskDetail);
                        timeoutCount++;
                    }
                }
            } else {
                logger.info("This send task is pending,complex_id:<{}>,tpl_uuid:<{}>",
                        complexId, playlistUuid);
            }
            taskSumTotal++;
        }
        logger.info(
                "[Scheduled] checking tpl send task done,task_num_total:<{}>,timeout_count:<{}>",
                taskSumTotal, timeoutCount);
    }

    private void sendingTplToSite(PlaylistSendLogDO sendLogDO, TransferTaskModel task) {
        try {
            TPlaylistSendRequestDTO request = MessageQueueConvert.MAPPER
                    .toPlaylistSendRequest(sendLogDO);
            this.mqService.playlistSendRequest(request);
            TPlaylistSendConvert.MAPPER.playlistSendLogDOFilling(sendLogDO,
                    TPlaylistActionStatusEnum.REQUESTED.getCode(),
                    TPlaylistActionStatusEnum.REQUESTED.name(), null);
            if (task != null) {
                task.setLatestTriggerTime(System.currentTimeMillis());
            }
        } catch (Exception e) {
            logger.error(
                    "request playlist send,send playlist.send.request message failed.",
                    e);
            TPlaylistSendConvert.MAPPER.playlistSendLogDOFilling(sendLogDO,
                    TPlaylistActionStatusEnum.FAILED.getCode(),
                    "send playlist.send.request message failed." + e.getMessage(), null);
        }
        playlistSendLogService.updateById(sendLogDO);
    }

    private PlaylistSendLogDO saveOrUpdateSendLog(TPlaylistTransferDTO dto,
            TmsPlaylistDO toBeSend) {
        PlaylistSendLogDO sendLogDO = getSendLogFromDBOrTaskMap(dto.getPlaylistUuid(),
                dto.getComplexUuid(), dto.getPosId(), null, null);
        // after playlist sent to complex,it was not modified yet
        if (sendLogDO != null && sendLogDO.getStatus()
                .equals(TPlaylistActionStatusEnum.DONE.getCode())
                && toBeSend.getLastModified() < sendLogDO.getLastModified() && !Boolean.TRUE
                .equals(dto.getFastTrack())) {
            logger.info(
                    "request playlist send,latest playlist have been sent to complex.");

            // send pos mapping to tms
            mappingSendService.handleMappingSendRequest(toBeSend.getSourcePplId(), dto);

            // remove send task
            taskMap.remove(sendLogDO);
            complexFacadeClient.lmsCacheEvict(dto.getComplexUuid());
            return null;
        }

        if (sendLogDO == null || sendLogDO.getStatus()
                .equals(TPlaylistActionStatusEnum.DONE.getCode())) {
            TPlaylistActionEnum actionEnum =
                    sendLogDO == null ? TPlaylistActionEnum.CREATE : TPlaylistActionEnum.UPDATE;
            sendLogDO = TPlaylistSendConvert.MAPPER
                    .toPlaylistSendLogDO(dto.getPosId(), dto.getComplexUuid(), dto.getDeviceUuid(),
                            actionEnum.getCode(), toBeSend);
            // save send log
            taskService.savePlaylistLog(taskMap, sendLogDO, dto.getPosId());
        } else {
            TPlaylistSendConvert.MAPPER
                    .playlistSendLogDOFilling(sendLogDO, null, null, toBeSend);
        }
        return sendLogDO;
    }

    private PlaylistSendLogDO getSendLogFromDBOrTaskMap(String playlistUuid, String complexId,
            String posId, String receiptUuid, String actionId) {
        if (StringUtils.isNotEmpty(playlistUuid) && StringUtils.isNotEmpty(complexId)) {
            PlaylistSendLogDO uniqueLog = TPlaylistSendConvert.MAPPER
                    .toUniqueLog(playlistUuid, complexId);

            for (Entry<Serializable, TransferTaskModel> entry : taskMap.entrySet()) {
                PlaylistSendLogDO key = (PlaylistSendLogDO) entry.getKey();
                if (key.equals(uniqueLog)) {
                    TransferTaskModel task = entry.getValue();
                    taskService.updatePlaylistLogTask(task, posId);
                    return key;
                }
            }
        }

        PlaylistSendLogDO logDO;
        if (StringUtils.isNotEmpty(receiptUuid)) {
            logDO = playlistSendLogService.getPlaylistSendLogByReceiptUuid(receiptUuid);
        } else if (StringUtils.isNotEmpty(actionId)) {
            logDO = playlistSendLogService.getPlaylistSendLogByActionId(actionId);
        } else {
            List<PlaylistSendLogDO> sendLogs = playlistSendLogService
                    .getLatestPlaylistSendLogs(complexId, playlistUuid);
            logDO = sendLogs.stream().findAny().orElse(null);
        }
        if (logDO != null) {
            if (!TPlaylistActionStatusEnum.DONE
                    .equals(TPlaylistActionStatusEnum.getByCode(logDO.getStatus()))) {
                taskService.saveOrUpdatePlaylistLogTask(taskMap, logDO, posId);
            }
        }
        return logDO;
    }

    private void sendPlaylistSendRequest(PlaylistSendLogDO logDO) {
        if (taskMap.containsKey(logDO)) {
            TransferTaskModel task = taskMap.get(logDO);
            Long latestTriggerTime = task.getLatestTriggerTime();
            long now = System.currentTimeMillis();
            if (latestTriggerTime == null || (now - latestTriggerTime) > NOT_DONE_AND_TRY_AGAIN) {
                sendingTplToSite(logDO, task);
            } else {
                logger.info(
                        "pos <{}> are waiting for playlist <{}> send to complex <{}>,request time num:<{}>.",
                        JSON.toJSONString(task.getTriggerUuidList()), logDO.getPlaylistUuid(),
                        logDO.getComplexId(), task.getTriggerNum());
            }
        } else {
            sendingTplToSite(logDO, null);
        }
    }
}
