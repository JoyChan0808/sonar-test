package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.PosMappingSentDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistActionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import java.util.List;

/**
 * pos assign send to tms
 *
 * @author oliver.lo
 * @since 2019-09-18 12:01
 */
public interface IPosMappingSendService {

    /**
     * playlist first send to tms response
     *
     * @param dto dto
     */
    void handleMappingSendRequest(String pplUuid, String tplUuid, TPlaylistActionDTO dto,
            String complexUuid);

    /**
     * playlist had sent to tms
     *
     * @param dto dto
     */
    void handleMappingSendRequest(String pplUuid, TPlaylistTransferDTO dto);

    /**
     * send pos mappings
     *
     * @param posMappings pos mappings
     * @param action action
     */
    void handleMappingSendRequest(List<PosPlaylistMappingDO> posMappings, String action);

    /**
     * handle pos.batch-mapping.response
     *
     * @param dto dto
     */
    void handleMappingSendResponse(PosMappingSentDTO dto);

    /**
     * clean up timeout task or retry
     */
    void handleTimeoutTask();
}
