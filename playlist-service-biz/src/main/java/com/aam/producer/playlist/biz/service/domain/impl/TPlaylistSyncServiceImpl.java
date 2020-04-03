package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.enums.PlaylistSourceEnum;
import com.aam.producer.playlist.biz.convert.MessageQueueConvert;
import com.aam.producer.playlist.biz.convert.TPlaylistActionConvert;
import com.aam.producer.playlist.biz.convert.TPlaylistSyncConvert;
import com.aam.producer.playlist.biz.enums.TPlaylistActionEnum;
import com.aam.producer.playlist.biz.enums.TPlaylistActionStatusEnum;
import com.aam.producer.playlist.biz.model.TransferTaskModel;
import com.aam.producer.playlist.biz.service.IPlaylistSyncLogService;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistLogTaskService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSyncService;
import com.aam.producer.playlist.protocol.message.PlaylistDataDTO;
import com.aam.producer.playlist.protocol.message.PplContentsDataDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistHashSyncRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistSyncRequestDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistHashSyncedDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistSyncedDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistSyncedDTO.Playlist;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.repository.entity.PlaylistSyncLogDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.alibaba.fastjson.JSON;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * playlist sync service
 *
 * @author oliver.lo
 * @since 2019/4/8 6:50 PM
 */
@Service
public class TPlaylistSyncServiceImpl implements ITPlaylistSyncService {

    private static final Logger logger = LoggerFactory.getLogger(TPlaylistSyncServiceImpl.class);

    // cause no batch number,so use this to protect batch sync in one minute
    private final static long NOT_DONE_AND_TRY_AGAIN = 5 * 60 * 1000;
    private final ConcurrentHashMap<Serializable, TransferTaskModel> taskMap = new ConcurrentHashMap<>();

    private final IPlaylistSyncLogService playlistSyncLogService;
    private final IComplexFacadeClient complexFacadeClient;
    private final ITmsPlaylistService tmsPlaylistService;
    private final IMessageQueueService mqService;
    private final ITPlaylistLogTaskService taskService;
    private final IPosPlaylistMappingService posPlaylistMappingService;

    @Autowired
    public TPlaylistSyncServiceImpl(IPlaylistSyncLogService playlistSyncLogService,
            IComplexFacadeClient complexFacadeClient,
            ITmsPlaylistService tmsPlaylistService,
            IMessageQueueService mqService,
            ITPlaylistLogTaskService taskService,
            IPosPlaylistMappingService posPlaylistMappingService) {
        this.playlistSyncLogService = playlistSyncLogService;
        this.complexFacadeClient = complexFacadeClient;
        this.tmsPlaylistService = tmsPlaylistService;
        this.mqService = mqService;
        this.taskService = taskService;
        this.posPlaylistMappingService = posPlaylistMappingService;
    }

    @Override
    public void handlePlaylistHashRequest(TPlaylistTransferDTO dto) {
        if (logger.isDebugEnabled()) {
            logger.debug("request playlist hash,dto:<{}>", JSON.toJSONString(dto));
        }
        logger.info(
                "request playlist hash,complex_uuid:<{}>,device_uuid:<{}>,playlist_uuid:<{}>,pos_id:<{}>",
                dto.getComplexUuid(), dto.getDeviceUuid(), dto.getPlaylistUuid(), dto.getPosId());

        // get or create a sync log
        PlaylistSyncLogDO logDO = getOrCreateSyncLog(dto);

        // sending request message
        if (Boolean.TRUE.equals(dto.getFastTrack())) {
            requestTplFromSite(logDO);
        } else {
            sendPlaylistHashRequest(logDO);
        }
    }

    @Override
    public void handlePlaylistHashResponse(TPlaylistHashSyncedDTO dto) {
        logger.info("received message playlist.hash-sync.response,response:<{}>", dto);

        // request failed
        if (Boolean.FALSE.equals(dto.getSuccess())) {
            logger.warn("request playlist hash failed,response:<{}>", dto);
            return;
        }

        // which will create a new sync log
        List<PlaylistSyncLogDO> addSyncLogs = new ArrayList<>();
        // which will update an exist sync log(status&message)
        List<PlaylistSyncLogDO> updateSyncLogs = new ArrayList<>();
        // contain all the sync log ids which not sync request or request failed
        List<PlaylistSyncLogDO> needToRequestLogs = new ArrayList<>();

        // compare sync list from agent-service with sync log from playlist-DB,per each device
        compareSyncedHashWithPro(dto, addSyncLogs, updateSyncLogs, needToRequestLogs);

        // create/update sync logs
        createOrUpdateSyncLog(addSyncLogs, updateSyncLogs, needToRequestLogs);

        // send playlist sync request
        if (CollectionUtils.isNotEmpty(needToRequestLogs)) {
            needToRequestLogs.forEach(this::requestTplFromSite);
        } else {
            logger.info(
                    "received message playlist.hash-sync.response,but hash compare nothing need to sync.");
        }
    }

    @Override
    public void handlePlaylistSyncedResponse(TPlaylistSyncedDTO dto) {
        logger.info(
                "received message playlist.sync.response,success:<{}>,complex_uuid:<{}>,playlist_uuid:<{}>",
                dto.getSuccess(), dto.getComplexUuid(), dto.getPlaylistUuid());

        // get and fill sync log
        PlaylistSyncLogDO logDO = getAndFillSyncLog(dto);

        // save tms playlist
        if (logDO != null) {
            if (Boolean.FALSE.equals(dto.getSuccess())) {
                // update playlist sync log
                playlistSyncLogService.updateById(logDO);
            } else {
                if (!ifSpl(dto.getPlaylist().getDeviceUuid(), dto.getComplexUuid())) {
                    handleTplSyncedFromSite(logDO);
                }
                sendPlaylistData(dto);

                // remove sync task
                taskMap.remove(logDO);
                complexFacadeClient.lmsCacheEvict(logDO.getComplexId());
            }
        }
    }

    private void sendPlaylistData(TPlaylistSyncedDTO dto) {
        logger.info("send playlist.data .data:<{}>", JSON.toJSONString(dto));
        Playlist playlist = dto.getPlaylist();
        PlaylistDataDTO playlistDataDTO = new PlaylistDataDTO();
        playlistDataDTO.setComplexUuid(dto.getComplexUuid());
        playlistDataDTO.setDeviceUuid(playlist.getDeviceUuid());
        playlistDataDTO.setPlaylistUuid(dto.getPlaylistUuid());
        PlaylistDataDTO.PlaylistDTO playlistDTO = new PlaylistDataDTO.PlaylistDTO();
        playlistDTO.setClean(playlist.getClean());
        playlistDTO.setContentIds(playlist.getContentIds());
        playlistDTO.setDeviceUuid(playlist.getDeviceUuid());
        playlistDTO.setDurationInSeconds(playlist.getDurationInSeconds());
        playlistDTO.setHfr(playlist.getAsHfr());
        playlistDTO.setIs3d(playlist.getAs3d());
        playlistDTO.setIs4k(playlist.getAs4k());
        playlistDTO.setPreshowDuration(playlist.getPreShowDuration());
        playlistDTO.setTitle(playlist.getTitle());
        playlistDTO.setTemplate(playlist.getTemplated());
        playlistDTO.setTotalDurationInSeconds(playlist.getTotalDurationInSeconds());
        playlistDTO.setPlaylistIds(playlist.getPlaylistIds());
        playlistDTO.setUuid(playlist.getUuid());
        playlistDTO.setPlaylist(playlist.getJson());
        playlistDataDTO.setPlaylist(playlistDTO);
        mqService.playlistData(playlistDataDTO);
        syncChildrenPlaylist(playlist, dto.getComplexUuid(), playlist.getDeviceUuid());
    }

    private void syncChildrenPlaylist(Playlist playlist, String compelxUuid, String deviceUuid) {
        try {
            if (CollectionUtils.isNotEmpty(playlist.getPlaylistIds())) {
                playlist.getPlaylistIds().forEach(splUuid -> {
                    logger.info(
                            "request a child SPL. splUuid:<{}> compelxUuid:<{}> deviceUuid:<{}>",
                            splUuid, compelxUuid, deviceUuid);
                    TPlaylistTransferDTO dto = new TPlaylistTransferDTO();
                    dto.setComplexUuid(compelxUuid);
                    dto.setDeviceUuid(deviceUuid);
                    dto.setFastTrack(true);
                    dto.setPlaylistUuid(splUuid);
                    handlePlaylistHashRequest(dto);
                });
            }
        } catch (Exception e) {
            logger.info(
                    "sync spl children playlist. compelxUuid:<{}> deviceUuid:<{}> playlist:<{}>",
                    compelxUuid, deviceUuid, JSON.toJSONString(playlist));
            logger.error("sync spl children playlist error.", e);
        }
    }

    private boolean ifSpl(String deviceUuid, String complexUuid) {
        try {
            String lmsUuid = complexFacadeClient.getLmsUuid(complexUuid);
            return !Objects.equals(lmsUuid, deviceUuid);
        } catch (Exception e) {
            logger.info("Judge SPL error.deviceUuid:<{}> complexUuid:<{}>", deviceUuid,
                    complexUuid);
            logger.error("Judge SPL error.", e);
            return false;
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
            PlaylistSyncLogDO taskLog = (PlaylistSyncLogDO) next.getKey();
            String complexId = taskLog.getComplexId();
            String playlistUuid = taskLog.getPlaylistUuid();
            TransferTaskModel taskDetail = next.getValue();
            Long latestTriggerTime = taskDetail.getLatestTriggerTime();
            if (latestTriggerTime == null
                    || (now - latestTriggerTime) > NOT_DONE_AND_TRY_AGAIN) {
                List<PlaylistSyncLogDO> dbLogs = playlistSyncLogService
                        .getNotDonePlaylistSyncLogs(complexId, playlistUuid);
                PlaylistSyncLogDO dbLog = dbLogs.stream().findAny().orElse(null);
                if (dbLog == null) {
                    logger.info(
                            "This task sync timeout,but db status is done,remove this task,complex_id:<{}>,tpl_uuid:<{}>",
                            complexId, playlistUuid);
                    iterator.remove();
                } else {
                    List<PosPlaylistMappingDO> posMappings = posPlaylistMappingService
                            .getMappingByTpl(dbLog.getPlaylistUuid(), dbLog.getPlaylistUuid(),
                                    dbLog.getComplexId());
                    if (CollectionUtils.isEmpty(posMappings)) {
                        logger.info(
                                "This task sync timeout,and db status is not done,but no pos mapping match it,remove this task,complex_id:<{}>,tpl_uuid:<{}>",
                                complexId, playlistUuid);
                        iterator.remove();
                    } else {
                        logger.info(
                                "This task sync timeout,but db status is not done yet,do it again,complex_id:<{}>,tpl_uuid:<{}>",
                                complexId, playlistUuid);
                        requestTplHashFromSite(dbLog, taskDetail);
                        timeoutCount++;
                    }
                }
            } else {
                logger.info("This sync task is pending,complex_id:<{}>,tpl_uuid:<{}>",
                        complexId, playlistUuid);
            }
            taskSumTotal++;
        }
        logger.info(
                "[Scheduled] checking tpl sync task done,task_num_total:<{}>,timeout_count:<{}>",
                taskSumTotal, timeoutCount);
    }

    private void handleTplSyncedFromSite(PlaylistSyncLogDO syncLogDO) {
        try {
            savePlaylistSyncedFromSite(syncLogDO);

            logger.info("received message playlist.sync.response,done.");

            TPlaylistSyncConvert.MAPPER
                    .playlistSyncLogDOFilling(syncLogDO, null,
                            TPlaylistActionStatusEnum.DONE.getCode(),
                            TPlaylistActionStatusEnum.DONE.name());
        } catch (Exception e) {
            logger.error("received message playlist.sync.response,but handling failed.",
                    e);
            TPlaylistSyncConvert.MAPPER
                    .playlistSyncLogDOFilling(syncLogDO, null,
                            TPlaylistActionStatusEnum.FAILED.getCode(),
                            "received message playlist.sync.response,but handling failed." + e
                                    .getMessage());
        }
        // update playlist sync log
        this.playlistSyncLogService.updateById(syncLogDO);
    }

    private void requestTplFromSite(PlaylistSyncLogDO syncLogDO) {
        try {
            TPlaylistSyncRequestDTO request = MessageQueueConvert.MAPPER
                    .toPlaylistSyncRequest(syncLogDO.getComplexId(), syncLogDO.getDeviceUuid(),
                            syncLogDO.getPlaylistUuid());
            this.mqService.playlistSyncRequest(request);
            TPlaylistSyncConvert.MAPPER
                    .playlistSyncLogDOFilling(syncLogDO, null,
                            TPlaylistActionStatusEnum.REQUESTED.getCode(),
                            "Have sent playlist.sync.request message.");
        } catch (Exception e) {
            logger.error(
                    "received message playlist.hash-sync.response,send playlist.sync.request message failed.",
                    e);
            TPlaylistSyncConvert.MAPPER
                    .playlistSyncLogDOFilling(syncLogDO, null,
                            TPlaylistActionStatusEnum.FAILED.getCode(),
                            "send playlist.sync.request message failed." + e.getMessage());
        }

        // update log
        this.playlistSyncLogService.updateById(syncLogDO);
    }

    private void requestTplHashFromSite(PlaylistSyncLogDO syncLogDO, TransferTaskModel task) {
        try {
            TPlaylistHashSyncRequestDTO request = MessageQueueConvert.MAPPER
                    .toPlaylistHashSyncRequest(syncLogDO.getComplexId(), syncLogDO.getDeviceUuid(),
                            syncLogDO.getPlaylistUuid());
            this.mqService.playlistHashSyncRequest(request);
            TPlaylistSyncConvert.MAPPER
                    .playlistSyncLogDOFilling(syncLogDO, null,
                            TPlaylistActionStatusEnum.REQUESTED.getCode(),
                            "Have sent playlist.hash-sync.request message.");
            if (task != null) {
                task.setLatestTriggerTime(System.currentTimeMillis());
            }
        } catch (Exception e) {
            logger.error(
                    "request playlist hash,send playlist.hash-sync.request message failed.",
                    e);
            TPlaylistSyncConvert.MAPPER
                    .playlistSyncLogDOFilling(syncLogDO, null,
                            TPlaylistActionStatusEnum.FAILED.getCode(),
                            "send playlist.hash-sync.request message failed." + e.getMessage());
        }

        // update log
        this.playlistSyncLogService.updateById(syncLogDO);
    }

    private void savePlaylistSyncedFromSite(PlaylistSyncLogDO syncLogDO) {
        String tplUuid = syncLogDO.getPlaylistUuid();
        String organizationUuid = this.complexFacadeClient
                .getOrganizationUuid(syncLogDO.getComplexId());
        TmsPlaylistDO tmsPlaylistDO = tmsPlaylistService
                .getOneTplScopeInOrg(organizationUuid, tplUuid);
        if (tmsPlaylistDO == null) {
            tmsPlaylistDO = TPlaylistActionConvert.MAPPER
                    .toTmsPlaylistDOFromSynced(PlaylistSourceEnum.TMS.getCode(), false, syncLogDO,
                            organizationUuid,
                            tplUuid);
        } else {
            TPlaylistActionConvert.MAPPER
                    .tmsPlaylistDOFillingFromSynced(tmsPlaylistDO, syncLogDO);
        }
        tmsPlaylistService.saveOrUpdate(tmsPlaylistDO);

        // send ppl-contents.data
        Map<String, List<String>> tplContentMap = new HashMap<>();
        List<String> contentIds = JSON.parseArray(tmsPlaylistDO.getContentIds(), String.class);
        tplContentMap.put(tmsPlaylistDO.getPlaylistUuid(),
                CollectionUtils.isEmpty(contentIds) ? new ArrayList<>() : contentIds);
        PplContentsDataDTO pplContentsDataDTO = MessageQueueConvert.MAPPER
                .toPplContentsDataDTO(organizationUuid, tmsPlaylistDO.getSourceComplexId(),
                        tmsPlaylistDO.getPlaylistUuid(), tplContentMap, syncLogDO.getTitle());
        mqService.pplContentsData(pplContentsDataDTO);
    }

    private PlaylistSyncLogDO getOrCreateSyncLog(TPlaylistTransferDTO dto) {
        // get latest playlist sync logs
        List<PlaylistSyncLogDO> logDOList = getSyncLogFromDBOrTaskMap(
                Collections.singletonList(dto.getPlaylistUuid()), dto.getComplexUuid(),
                dto.getPosId());
        PlaylistSyncLogDO logDO = logDOList.stream().findAny().orElse(null);
        if (logDO == null) {
            logDO = TPlaylistSyncConvert.MAPPER
                    .toPlaylistSyncLogDO(dto.getComplexUuid(), dto.getDeviceUuid(),
                            dto.getPlaylistUuid(), null, TPlaylistActionEnum.CREATE.getCode(),
                            TPlaylistActionStatusEnum.MARKED.getCode(),
                            TPlaylistActionStatusEnum.MARKED.name());
            // save sync log
            taskService.savePlaylistLog(taskMap, logDO, dto.getPosId());
        }
        return logDO;
    }

    private List<PlaylistSyncLogDO> getSyncLogFromDBOrTaskMap(List<String> playlistUuidList,
            String complexId, String posId) {
        List<PlaylistSyncLogDO> result = new ArrayList<>();
        List<String> notInTaskPlaylistIds = new ArrayList<>();

        playlistUuidList.forEach(x -> {
            boolean notInTask = true;
            PlaylistSyncLogDO uniqueLog = TPlaylistSyncConvert.MAPPER.toUniqueLog(x, complexId);
            for (Entry<Serializable, TransferTaskModel> entry : taskMap.entrySet()) {
                PlaylistSyncLogDO key = (PlaylistSyncLogDO) entry.getKey();
                if (key.equals(uniqueLog)) {
                    TransferTaskModel task = entry.getValue();
                    notInTask = false;
                    result.add(key);
                    taskService.updatePlaylistLogTask(task, posId);
                }
            }
            if (notInTask) {
                notInTaskPlaylistIds.add(x);
            }
        });

        if (CollectionUtils.isNotEmpty(notInTaskPlaylistIds)) {
            Optional<List<PlaylistSyncLogDO>> syncLogs = Optional.ofNullable(playlistSyncLogService
                    .getLatestPlaylistSyncLogs(complexId, notInTaskPlaylistIds));
            syncLogs.ifPresent(logs -> {
                result.addAll(logs);
                logs.forEach(x -> {
                    if (!TPlaylistActionStatusEnum.DONE
                            .equals(TPlaylistActionStatusEnum.getByCode(x.getStatus()))) {
                        taskService.saveOrUpdatePlaylistLogTask(taskMap, x, posId);
                    }
                });
            });
        }

        return result;
    }

    private void sendPlaylistHashRequest(PlaylistSyncLogDO logDO) {
        if (taskMap.containsKey(logDO)) {
            TransferTaskModel task = taskMap.get(logDO);
            Long latestTriggerTime = task.getLatestTriggerTime();
            long now = System.currentTimeMillis();
            if (latestTriggerTime == null || (now - latestTriggerTime) > NOT_DONE_AND_TRY_AGAIN) {
                requestTplHashFromSite(logDO, task);
            } else {
                logger.info(
                        "pos <{}> are waiting for playlist <{}> hash sync back from complex <{}>,request time num:<{}>.",
                        JSON.toJSONString(task.getTriggerUuidList()), logDO.getPlaylistUuid(),
                        logDO.getComplexId(), task.getTriggerNum());
            }
        } else {
            requestTplHashFromSite(logDO, null);
        }
    }

    private void compareSyncedHashWithPro(TPlaylistHashSyncedDTO dto,
            List<PlaylistSyncLogDO> addSyncLogs, List<PlaylistSyncLogDO> updateSyncLogs,
            List<PlaylistSyncLogDO> needToRequestLogs) {
        String lmsUuid = this.complexFacadeClient.getLmsUuid(dto.getComplexUuid());
        Map<String, Map<String, String>> playlistHashDeviceMap = dto.getPlaylistHashDeviceMap();
        playlistHashDeviceMap.forEach((key, value) -> {
            // String device_uuid = key,just sync tpl in lms device
            if (key.equals(lmsUuid)) {
                if (MapUtils.isNotEmpty(value)) {
                    List<PlaylistSyncLogDO> logDOList = getSyncLogFromDBOrTaskMap(
                            new ArrayList<>(value.keySet()), dto.getComplexUuid(), null);
                    Map<String, PlaylistSyncLogDO> dbLogMap = logDOList.stream().collect(Collectors
                            .toMap(PlaylistSyncLogDO::getPlaylistUuid, Function.identity()));
                    for (Entry<String, String> entry : value.entrySet()) {
                        String playlistUuid = entry.getKey();
                        String hash = entry.getValue();
                        if (dbLogMap.containsKey(playlistUuid)) {
                            // method: update
                            updateActionFilling(dbLogMap.get(playlistUuid), hash, addSyncLogs,
                                    updateSyncLogs, needToRequestLogs);
                        } else {
                            // method: create
                            createActionFilling(dto.getComplexUuid(), key, playlistUuid, hash,
                                    addSyncLogs);
                        }
                    }
                }
            }
        });
    }

    private void createActionFilling(String complexUuid, String deviceUuid, String playlistUuid,
            String hash, List<PlaylistSyncLogDO> addSyncLogs) {
        addSyncLogs.add(TPlaylistSyncConvert.MAPPER
                .toPlaylistSyncLogDO(complexUuid, deviceUuid, playlistUuid,
                        hash, TPlaylistActionEnum.CREATE.getCode(),
                        TPlaylistActionStatusEnum.MARKED.getCode(),
                        TPlaylistActionStatusEnum.MARKED.name()));
    }

    private void updateActionFilling(PlaylistSyncLogDO logDO, String hash,
            List<PlaylistSyncLogDO> addSyncLogs, List<PlaylistSyncLogDO> updateSyncLogs,
            List<PlaylistSyncLogDO> needToRequestLogs) {
        TPlaylistActionStatusEnum status = TPlaylistActionStatusEnum.getByCode(logDO.getStatus());
        if (hash.equals(logDO.getHash())) {
            // playlist sync have marked,but not request to sync
            if (TPlaylistActionStatusEnum.MARKED.equals(status)) {
                needToRequestLogs.add(logDO);
            }
        } else {
            if (TPlaylistActionStatusEnum.DONE.equals(status)) {
                addSyncLogs.add(TPlaylistSyncConvert.MAPPER
                        .toPlaylistSyncLogDO(logDO.getComplexId(), logDO.getDeviceUuid(),
                                logDO.getPlaylistUuid(), hash,
                                TPlaylistActionEnum.UPDATE.getCode(),
                                TPlaylistActionStatusEnum.MARKED.getCode(),
                                TPlaylistActionStatusEnum.MARKED.name()));
            } else {
                if (StringUtils.isEmpty(logDO.getHash())) {
                    logDO.setHash(hash);
                    updateSyncLogs.add(logDO);
                    needToRequestLogs.add(logDO);
                } else {
                    addSyncLogs.add(TPlaylistSyncConvert.MAPPER
                            .toPlaylistSyncLogDO(logDO.getComplexId(), logDO.getDeviceUuid(),
                                    logDO.getPlaylistUuid(), hash,
                                    TPlaylistActionEnum.UPDATE.getCode(),
                                    TPlaylistActionStatusEnum.MARKED.getCode(),
                                    TPlaylistActionStatusEnum.MARKED.name()));

                    // if the previous sync not done,should be expired
                    logDO.setStatus(TPlaylistActionStatusEnum.DONE.getCode());
                    logDO.setMessage("A new [update] task has caused this task to expire");
                    updateSyncLogs.add(logDO);
                }
            }
        }
    }

    private void createOrUpdateSyncLog(List<PlaylistSyncLogDO> addSyncLogs,
            List<PlaylistSyncLogDO> updateSyncLogs, List<PlaylistSyncLogDO> needToRequestLogs) {
        if (CollectionUtils.isNotEmpty(addSyncLogs)) {
            playlistSyncLogService.saveBatch(addSyncLogs);
            // set task
            addSyncLogs.forEach(x -> taskService.savePlaylistLog(taskMap, x, null));
            // add sync logs to request list
            needToRequestLogs.addAll(addSyncLogs);
        }
        if (CollectionUtils.isNotEmpty(updateSyncLogs)) {
            playlistSyncLogService.updateBatchById(updateSyncLogs);
            // set task
            updateSyncLogs.forEach(x -> taskService.savePlaylistLog(taskMap, x, null));
        }
    }

    private PlaylistSyncLogDO getAndFillSyncLog(TPlaylistSyncedDTO dto) {
        // get latest playlist sync logs
        Playlist playlist = dto.getPlaylist();
        String playlistUuid = playlist == null ? dto.getPlaylistUuid() : playlist.getUuid();
        List<PlaylistSyncLogDO> logDOList = getSyncLogFromDBOrTaskMap(
                Collections.singletonList(playlistUuid), dto.getComplexUuid(), null);
        PlaylistSyncLogDO logDO = logDOList.stream().findAny().orElse(null);
        if (logDO == null || TPlaylistActionStatusEnum.DONE
                .equals(TPlaylistActionStatusEnum.getByCode(logDO.getStatus()))) {
            // stop tms create playlist and first push
            logger.info(
                    "received message playlist.sync.response,but not exist sync log.");
            return null;
        } else {
            // previous sync not done yet
            TPlaylistSyncConvert.MAPPER
                    .playlistSyncLogDOFilling(logDO, playlist,
                            Boolean.FALSE.equals(dto.getSuccess())
                                    ? TPlaylistActionStatusEnum.FAILED
                                    .getCode() : null, dto.getMessage());
        }
        return logDO;
    }
}
