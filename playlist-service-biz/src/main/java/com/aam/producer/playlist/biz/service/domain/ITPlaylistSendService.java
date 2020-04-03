package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.TPlaylistActionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeliveryDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;

/**
 * playlist send service
 *
 * @author oliver.lo
 * @since 2019/5/14 6:06 PM
 */
public interface ITPlaylistSendService {

    /**
     * request playlist send to complex
     * 1.create send log
     * 2.send playlist.send.request message
     *
     * @param dto single detail
     * @param toBeSend tpl
     */
    void handlePlaylistSendRequest(TPlaylistTransferDTO dto, TmsPlaylistDO toBeSend);

    /**
     * receive one playlist delivery response,then action below
     * 1.check if one playlist send log valid or not
     * 2.if not have valid playlist send log,then do nothing
     * 3.else update playlist send log status
     *
     * @param dto single detail
     */
    void handlePlaylistDeliveredResponse(TPlaylistDeliveryDTO dto);

    /**
     * receive one playlist save action response,then action below
     * 1.check if one playlist send log valid or not
     * 2.if not have valid playlist send log,then do nothing
     * 3.else update playlist send log status and get playlist back from tms
     *
     * @param dto single detail
     */
    void handlePlaylistActionedResponse(TPlaylistActionDTO dto);

    /**
     * tpl changed should be sent to tms
     *
     * @param tmsPlaylistDO tpl
     */
    void handlePlaylistChanged(TmsPlaylistDO tmsPlaylistDO);

    /**
     * clean up timeout task or retry it
     */
    void handleTimeoutTask();
}
