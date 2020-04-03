package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.TPlaylistHashSyncedDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistSyncedDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;

/**
 * playlist sync service
 *
 * @author oliver.lo
 * @since 2019/4/8 6:47 PM
 */
public interface ITPlaylistSyncService {

    /**
     * ask playlist hash
     * 1.create sync log
     * 2.send playlist hash sync request
     *
     * @param dto single detail
     */
    void handlePlaylistHashRequest(TPlaylistTransferDTO dto);

    /**
     * sync playlist hash compare with db playlist sync log,then action below
     * 1.all exist but hash not equal,then sync request from ts-agent (update)
     * 2.exist in sync but not exist in db,then sync request from ts-agent (create)
     *
     * @param dto single detail
     */
    void handlePlaylistHashResponse(TPlaylistHashSyncedDTO dto);

    /**
     * receive one playlist data,then action below
     * 1.check if one playlist sync log valid or not
     * 2.if not have valid playlist sync log,then create one sync log
     * 3.save playlist data and update playlist sync log
     *
     * @param dto single detail
     */
    void handlePlaylistSyncedResponse(TPlaylistSyncedDTO dto);

    /**
     * clean up timeout task or retry it
     */
    void handleTimeoutTask();
}
