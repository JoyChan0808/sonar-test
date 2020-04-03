package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.biz.model.TransferTaskModel;
import java.io.Serializable;
import java.util.concurrent.ConcurrentHashMap;

/**
 * tpl task
 *
 * @author oliver.lo
 * @since 2019-09-20 17:06
 */
public interface ITPlaylistLogTaskService {

    /**
     * save playlist sync/send log
     *
     * @param taskMap task map
     * @param log sync/send log
     * @param posId pos id
     */
    void savePlaylistLog(ConcurrentHashMap<Serializable, TransferTaskModel> taskMap,
            Serializable log, String posId);

    /**
     * create task
     *
     * @param taskMap task map
     * @param log sync/send log
     * @param posId  pos id
     */
    void saveOrUpdatePlaylistLogTask(ConcurrentHashMap<Serializable, TransferTaskModel> taskMap,
            Serializable log, String posId);

    /**
     * update task
     *
     * @param task task
     * @param posId  pos id
     */
    void updatePlaylistLogTask(TransferTaskModel task, String posId);
}
