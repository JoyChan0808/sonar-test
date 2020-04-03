package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.convert.MessageQueueConvert;
import com.aam.producer.playlist.biz.convert.TPlaylistSendConvert;
import com.aam.producer.playlist.biz.enums.AAMSysEnum;
import com.aam.producer.playlist.biz.enums.TPlaylistActionStatusEnum;
import com.aam.producer.playlist.biz.service.IPlaylistDeleteLogService;
import com.aam.producer.playlist.biz.service.IPlaylistSendLogService;
import com.aam.producer.playlist.biz.service.IPlaylistSyncLogService;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistDeleteService;
import com.aam.producer.playlist.common.utils.TimeUtils;
import com.aam.producer.playlist.protocol.message.TPlaylistDeleteRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeletionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeletedDTO;
import com.aam.producer.playlist.repository.entity.PlaylistDeleteLogDO;
import com.aam.producer.playlist.repository.entity.PlaylistSendLogDO;
import com.aam.producer.playlist.repository.entity.PlaylistSyncLogDO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * playlist delete service
 *
 * @author oliver.lo
 * @since 2019-11-01 12:04
 */
@Service
public class TPlaylistDeleteServiceImpl implements ITPlaylistDeleteService {

    private static final Logger logger = LoggerFactory.getLogger(TPlaylistDeleteServiceImpl.class);

    private final IPlaylistSendLogService sendLogService;
    private final IComplexFacadeClient complexFacadeClient;
    private final IMessageQueueService mqService;
    private final IPlaylistDeleteLogService deleteLogService;
    private final ITmsPlaylistService tmsPlaylistService;
    private final IPlaylistSyncLogService syncLogService;

    @Autowired
    public TPlaylistDeleteServiceImpl(
            IPlaylistSendLogService sendLogService,
            IComplexFacadeClient complexFacadeClient,
            IMessageQueueService mqService,
            IPlaylistDeleteLogService deleteLogService,
            ITmsPlaylistService tmsPlaylistService,
            IPlaylistSyncLogService syncLogService) {
        this.sendLogService = sendLogService;
        this.complexFacadeClient = complexFacadeClient;
        this.mqService = mqService;
        this.deleteLogService = deleteLogService;
        this.tmsPlaylistService = tmsPlaylistService;
        this.syncLogService = syncLogService;
    }

    @Override
    @Transactional
    public void handleProTplDeletedRequest(TPlaylistDeletionDTO dto) {
        Map<String, List<String>> complexTplListMap = sendLogService
                .getComplexTplListMap(dto.getPlaylistUUIDs());
        if (complexTplListMap != null) {
            List<PlaylistDeleteLogDO> logs = new ArrayList<>(complexTplListMap.size());
            complexTplListMap.forEach((complexUuid, playlistIds) -> {
                String lmsUuid = complexFacadeClient.getLmsUuid(complexUuid);
                PlaylistDeleteLogDO deleteLogDO = TPlaylistSendConvert.MAPPER
                        .toPlaylistDeleteLogDO(complexUuid, lmsUuid, playlistIds,
                                AAMSysEnum.PRODUCER.getCode());
                logs.add(deleteLogDO);

                TPlaylistDeleteRequestDTO requestDTO = MessageQueueConvert.MAPPER
                        .toTPlaylistDeleteRequest(deleteLogDO.getReceiptUuid(), complexUuid,
                                lmsUuid, playlistIds);
                mqService.playlistDeleteRequest(requestDTO);
            });
            deleteLogService.saveBatch(logs, 100);

            // delete send logs
            sendLogService.remove(new QueryWrapper<PlaylistSendLogDO>().lambda()
                    .in(PlaylistSendLogDO::getPlaylistUuid, dto.getPlaylistUUIDs()));
        }
    }

    @Override
    public void handleProTplDeletedResponse(TPlaylistDeletedDTO dto) {
        Long attempted = TimeUtils.changeMillisecondTimestamp(dto.getAttemptedAt());
        PlaylistDeleteLogDO deleteLogDO = deleteLogService.getByReceiptUuid(dto.getReceiptUuid());
        if (deleteLogDO != null) {
            TPlaylistSendConvert.MAPPER.playlistDeleteLogDOFilling(deleteLogDO, dto, attempted);
            deleteLogService.updateById(deleteLogDO);

            if (Boolean.FALSE.equals(dto.getSuccess())) {
                List<String> tplUUIDs = JSON.parseArray(deleteLogDO.getTplUuids(), String.class);
                TPlaylistDeleteRequestDTO requestDTO = MessageQueueConvert.MAPPER
                        .toTPlaylistDeleteRequest(deleteLogDO.getReceiptUuid(),
                                deleteLogDO.getComplexId(), deleteLogDO.getDeviceUuid(), tplUUIDs);
                mqService.playlistDeleteRequest(requestDTO);
            }
        } else {
            logger.warn("receive playlist.delete.response,but not found any delete log,dto:<{}>",
                    dto);
        }
    }

    @Override
    @Transactional
    public void handleTmsTplDeletedPush(TPlaylistDeletionDTO dto) {
        String complexUuid = dto.getComplexUuid();
        List<String> playlistUUIDs = dto.getPlaylistUUIDs();
        PlaylistDeleteLogDO deleteLogDO = TPlaylistSendConvert.MAPPER
                .toPlaylistDeleteLogDO(complexUuid, null, playlistUUIDs,
                        AAMSysEnum.TMS.getCode());
        int num = tmsPlaylistService
                .deleteTplWhichFromTms(complexUuid, playlistUUIDs);
        if (num > 0) {
            deleteLogDO.setStatus(TPlaylistActionStatusEnum.DONE.name());
            deleteLogDO.setMessage(TPlaylistActionStatusEnum.DONE.name());
            deleteLogService.save(deleteLogDO);

            // delete sync logs
            syncLogService.remove(new QueryWrapper<PlaylistSyncLogDO>().lambda()
                    .eq(PlaylistSyncLogDO::getComplexId, complexUuid)
                    .in(PlaylistSyncLogDO::getPlaylistUuid, playlistUUIDs));
        }
        // delete send logs
        sendLogService.remove(new QueryWrapper<PlaylistSendLogDO>().lambda()
                .eq(PlaylistSendLogDO::getComplexId, complexUuid)
                .in(PlaylistSendLogDO::getPlaylistUuid, playlistUUIDs));
    }
}
