package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.PosDataDTO;
import com.aam.producer.playlist.protocol.request.TitleDataDTO;
import com.aam.producer.playlist.protocol.response.TmsPlaylistInfo;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * playlist assign service
 *
 * @author oliver.lo
 * @since 2019/7/2 9:25 AM
 */
public interface IPosMappingService {

    /**
     * pos mapping manual
     *
     * @param pplUuid ppl uuid
     * @param posUuidList pos uuid list
     */
    void posMappingManual(String pplUuid, Set<String> posUuidList);

    /**
     * pos mapping automatic
     *
     * @param playlistDO ppl do
     * @param showAttributeCodes show attribute codes
     */
    void handlePplPublish(PlaylistDO playlistDO, List<Long> showAttributeCodes);

    /**
     * pos un-mapping
     *
     * @param pplUuid ppl uuid
     */
    void posUnMapping(String pplUuid);

    /**
     * review mapped tpl
     *
     * @param pplUuid ppl uuid
     * @param posUuid pos uuid
     * @return tpl info
     */
    TmsPlaylistInfo reviewMappedTpl(String pplUuid, String posUuid);

    /**
     * handle pos mapped tpl
     *
     * @param posTplMap pos tpl map
     * @param tplList tpl
     */
    void handlePosMappedTpl(Map<String, String> posTplMap, List<TmsPlaylistDO> tplList);

    /**
     * handle pos.data,if need to fetch a new pos.data,return true
     *
     * @param posData pos.data
     */
    boolean handlePosData(PosDataDTO posData);

    /**
     * handle title.data
     *
     * @param titleData title.data
     */
    void handleTitleData(TitleDataDTO titleData);
}
