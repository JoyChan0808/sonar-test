package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.message.PlaylistDataDTO;
import com.aam.producer.playlist.protocol.message.PosMappingBatchRequestDTO;
import com.aam.producer.playlist.protocol.message.PplContentsDataDTO;
import com.aam.producer.playlist.protocol.message.ProducerPlaylistChangedDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeleteRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeletionDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistHashSyncRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistSendRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistSyncRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO;
import com.aam.producer.playlist.protocol.request.PosDataDTO;
import com.aam.producer.playlist.protocol.request.PosFetchDTO;
import com.aam.producer.task.protocol.request.SyncReportModel;
import java.util.List;
import java.util.Map;

/**
 * schema service
 *
 * @author oliver.lo
 * @since 2019/4/11 4:28 PM
 */
public interface IMessageQueueService {

    /**
     * playlist hash sync request
     *
     * @param dto request dto
     */
    void playlistHashSyncRequest(TPlaylistHashSyncRequestDTO dto);

    /**
     * playlist sync request
     *
     * @param dto request dto
     */
    void playlistSyncRequest(TPlaylistSyncRequestDTO dto);

    /**
     * playlist send request
     *
     * @param dto playlist send request dto
     */
    void playlistSendRequest(TPlaylistSendRequestDTO dto);

    /**
     * playlist delete notify
     *
     * @param dto playlist delete dto
     */
    void playlistDeletionData(TPlaylistDeletionDTO dto);

    /**
     * playlist warning data
     *
     * @param dto dto
     */
    void playlistWarningData(TPlaylistWarningDTO dto);

    /**
     * segment warning data
     *
     * @param dto dto
     */
    void segmentWarningData(SyncReportModel dto);

    /**
     * pos mapping request
     *
     * @param dto dto
     */
    void posMappingRequest(PosMappingBatchRequestDTO dto);

    /**
     * pos-match.data
     *
     * @param dto dto
     */
    void posMatchData(PosDataDTO dto);

    /**
     * ppl-contents.data
     *
     * @param dto dto
     */
    void pplContentsData(PplContentsDataDTO dto);

    /**
     * producer playlist changed data
     *
     * @param dto dto
     */
    void producerPlaylistChangedData(ProducerPlaylistChangedDTO dto);

    /**
     * pos fetch
     *
     * @param dto dto
     */
    void posFetch(PosFetchDTO dto);

    /**
     * tpl delete request
     *
     * @param dto dto
     */
    void playlistDeleteRequest(TPlaylistDeleteRequestDTO dto);

    /**
     * pos tpl matched data
     *
     * @param map map
     */
    void tplPosMatchedData(Map<String, List<String>> map);

    /**
     * playlist data
     *
     * @param playlistDataDTO playlistDataDTO
     */
    void playlistData(PlaylistDataDTO playlistDataDTO);
}
