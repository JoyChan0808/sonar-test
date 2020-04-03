package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.repository.entity.PlaylistSyncLogDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-04
 */
public interface IPlaylistSyncLogService extends IService<PlaylistSyncLogDO> {

    /**
     * get latest playlist sync logs
     *
     * @param complexId complex uuid
     * @param playlistUuids playlist uuid list
     * @return PlaylistSyncLogDO
     */
    List<PlaylistSyncLogDO> getLatestPlaylistSyncLogs(String complexId, List<String> playlistUuids);

    /**
     * get NotDone PlaylistSyncLogs
     *
     * @param complexId complex id
     * @param playlistUuid playlist uuid
     * @return
     */
    List<PlaylistSyncLogDO> getNotDonePlaylistSyncLogs(String complexId, String playlistUuid);
}
