package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.repository.entity.PlaylistSendLogDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;
import java.util.Map;

/**
 * playlist send log service
 *
 * @author oliver.lo
 * @since 2019/5/14 5:53 PM
 */
public interface IPlaylistSendLogService extends IService<PlaylistSendLogDO> {

    /**
     * get latest playlist send logs
     *
     * @param complexId complex uuid
     * @param playlistUuid playlist uuid
     * @return PlaylistSyncLogDO
     */
    List<PlaylistSendLogDO> getLatestPlaylistSendLogs(String complexId, String playlistUuid);

    /**
     * get one playlist send log by receipt uuid
     *
     * @param receiptUuid receipt uuid
     * @return one send log
     */
    PlaylistSendLogDO getPlaylistSendLogByReceiptUuid(String receiptUuid);

    /**
     * get one playlist send log by action id
     *
     * @param actionId action id
     * @return one send log
     */
    PlaylistSendLogDO getPlaylistSendLogByActionId(String actionId);

    /**
     * get not done log
     *
     * @param complexId complex id
     * @param playlistUuid playlist uuid
     * @return logs
     */
    List<PlaylistSendLogDO> getNotDonePlaylistSendLogs(String complexId, String playlistUuid);

    /**
     * get complex tpl list map
     *
     * @param tplUUIDs tpl uuid
     * @return map
     */
    Map<String, List<String>> getComplexTplListMap(List<String> tplUUIDs);
}
