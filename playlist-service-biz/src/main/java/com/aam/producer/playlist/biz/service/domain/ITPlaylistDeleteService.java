package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.message.TPlaylistDeletionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeletedDTO;

/**
 * com.aam.producer.playlist.biz.service.domain
 *
 * @author oliver.lo
 * @since 2019-11-01 11:36
 */
public interface ITPlaylistDeleteService {

    /**
     * handle pro tpl deleted request to tms
     *
     * @param dto dto
     */
    void handleProTplDeletedRequest(TPlaylistDeletionDTO dto);

    /**
     * handle pro tpl deleted response from tms
     *
     * @param dto dto
     */
    void handleProTplDeletedResponse(TPlaylistDeletedDTO dto);

    /**
     * handle tms tpl deleted push
     *
     * @param dto dto
     */
    void handleTmsTplDeletedPush(TPlaylistDeletionDTO dto);
}
