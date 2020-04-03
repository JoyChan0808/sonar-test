package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.enums.TPlaylistActionStatusEnum;
import com.aam.producer.playlist.biz.model.TransferTaskModel;
import com.aam.producer.playlist.biz.service.IPlaylistSendLogService;
import com.aam.producer.playlist.biz.service.IPlaylistSyncLogService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistLogTaskService;
import com.aam.producer.playlist.repository.entity.PlaylistSendLogDO;
import com.aam.producer.playlist.repository.entity.PlaylistSyncLogDO;
import java.io.Serializable;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * com.aam.producer.playlist.biz.service.domain.impl
 *
 * @author oliver.lo
 * @since 2019-09-20 17:07
 */
@Service
public class TPlaylistLogTaskServiceImpl implements ITPlaylistLogTaskService {

    private final IPlaylistSendLogService playlistSendLogService;
    private final IPlaylistSyncLogService playlistSyncLogService;

    @Autowired
    public TPlaylistLogTaskServiceImpl(
            IPlaylistSendLogService playlistSendLogService,
            IPlaylistSyncLogService playlistSyncLogService) {
        this.playlistSendLogService = playlistSendLogService;
        this.playlistSyncLogService = playlistSyncLogService;
    }

    @Override
    public void savePlaylistLog(ConcurrentHashMap<Serializable, TransferTaskModel> taskMap,
            Serializable log, String posId) {
        if (taskMap.containsKey(log)) {
            // concurrence have save sync log
            updatePlaylistLogTask(taskMap.get(log), posId);
            return;
        }

        // save new send log
        if (log instanceof PlaylistSyncLogDO) {
            playlistSyncLogService.save((PlaylistSyncLogDO) log);

            PlaylistSyncLogDO compare = (PlaylistSyncLogDO) log;
            List<PlaylistSyncLogDO> notDone = playlistSyncLogService
                    .getNotDonePlaylistSyncLogs(compare.getComplexId(), compare.getPlaylistUuid());
            notDone.stream().filter(x -> x.getId() < compare.getId()).findAny()
                    .ifPresent(x -> {
                        playlistSyncLogService.removeById(compare);
                        compare.setId(x.getId());
                        compare.setStatus(x.getStatus());
                    });
        } else {
            playlistSendLogService.save((PlaylistSendLogDO) log);

            PlaylistSendLogDO compare = (PlaylistSendLogDO) log;
            List<PlaylistSendLogDO> latest = playlistSendLogService
                    .getNotDonePlaylistSendLogs(compare.getComplexId(), compare.getPlaylistUuid());
            latest.stream().filter(x -> x.getId() < compare.getId()).findAny()
                    .ifPresent(x -> {
                        playlistSendLogService.removeById(compare);
                        compare.setId(x.getId());
                        compare.setStatus(x.getStatus());
                    });
        }

        saveOrUpdatePlaylistLogTask(taskMap, log, posId);
    }

    @Override
    public void saveOrUpdatePlaylistLogTask(
            ConcurrentHashMap<Serializable, TransferTaskModel> taskMap, Serializable log,
            String posId) {
        taskMap.putIfAbsent(log, new TransferTaskModel());
        // init
        TransferTaskModel task = taskMap.get(log);
        if (task.getCreated() == null) {
            task.setCreated(System.currentTimeMillis());
        }
        if (log instanceof PlaylistSyncLogDO) {
            PlaylistSyncLogDO logDO = (PlaylistSyncLogDO) log;
            if (!TPlaylistActionStatusEnum.MARKED
                    .equals(TPlaylistActionStatusEnum.getByCode(logDO.getStatus()))) {
                task.setLatestTriggerTime(logDO.getLastModified());
            }
        } else {
            PlaylistSendLogDO logDO = (PlaylistSendLogDO) log;
            if (!TPlaylistActionStatusEnum.MARKED
                    .equals(TPlaylistActionStatusEnum.getByCode(logDO.getStatus()))) {
                task.setLatestTriggerTime(logDO.getLastModified());
            }
        }
        // update
        updatePlaylistLogTask(task, posId);
    }

    @Override
    public void updatePlaylistLogTask(TransferTaskModel task, String posId) {
        task.increaseTriggerNum();
        if (StringUtils.isNotEmpty(posId)) {
            task.getTriggerUuidList().add(posId);
        }
    }
}
