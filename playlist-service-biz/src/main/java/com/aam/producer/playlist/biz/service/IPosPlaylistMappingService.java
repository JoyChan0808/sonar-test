package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.protocol.response.PosInfo;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;
import java.util.Map;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-09-17
 */
public interface IPosPlaylistMappingService extends IService<PosPlaylistMappingDO> {

    /**
     * get one mapping
     *
     * @param posUuid pos uuid
     * @return mapping
     */
    PosPlaylistMappingDO getOneMapping(String posUuid);

    /**
     * get list mapping
     *
     * @param withoutProtected without protected pos
     * @param inPosIds in pos uuid list
     * @return list mapping
     */
    List<PosPlaylistMappingDO> getMappingsByPos(boolean withoutProtected, List<String> inPosIds);

    /**
     * get list mapping
     *
     * @param orgId org uuid
     * @param withoutProtected without protected pos
     * @param withoutNoTitleMapped without no title mapped
     * @param showAttributeCodes show attribute codes
     * @param pplUuid ppl uuid
     * @return list mapping
     */
    List<PosPlaylistMappingDO> getMappingsByAutoPpl(String orgId, boolean withoutProtected,
            boolean withoutNoTitleMapped, List<Long> showAttributeCodes, String pplUuid);

    /**
     * get list mapping
     *
     * @param withoutProtected without protected pos
     * @param pplUuid ppl uuid
     * @return list mapping
     */
    List<PosPlaylistMappingDO> getMappingsByPpl(boolean withoutProtected, String pplUuid);

    /**
     * get list mapping
     *
     * @param withoutProtected without protected pos
     * @param pplUuid ppl uuid
     * @param titleId title id
     * @return list mapping
     */
    List<PosPlaylistMappingDO> getMappingsByPplFilterTitle(boolean withoutProtected, String pplUuid,
            String titleId);

    /**
     * get list mapping
     *
     * @param tplUuid tpl uuid
     * @return list mapping
     */
    List<PosPlaylistMappingDO> getMappingByTpl(String pplUuid, String tplUuid, String complexUuid);

    /**
     * get list mapping
     *
     * @param withoutProtected without protected pos
     * @param titleId title uuid
     * @return list mapping
     */
    List<PosPlaylistMappingDO> getMappingByTitle(boolean withoutProtected, String titleId);

    /**
     * get protected mappings
     *
     * @param pplUuid ppl uuid
     * @return list mapping
     */
    List<PosPlaylistMappingDO> getProtectedMappings(String pplUuid);

    /**
     * get mapping by show attribute
     *
     * @param orgId org id
     * @param withoutProtected without protected
     * @param titleId title id
     * @param showAttributeCodes show attribute codes
     * @return list mapping
     */
    List<PosPlaylistMappingDO> getMappingsByShowAttribute(String orgId, boolean withoutProtected,
            String titleId, List<Long> showAttributeCodes);

    /**
     * update mapping sent response
     *
     * @param posUUIDs pos ids
     * @param reStatus response status
     * @param reMessage re message
     * @param tplUuid tpl uuid
     * @param unMapped un-mapping success
     * @return updated num
     */
    int updateMappingSendInfo(List<String> posUUIDs, String reStatus, String reMessage,
            String tplUuid, boolean unMapped);

    /**
     * update org id
     *
     * @param orgId org id
     * @param complexId complex id
     * @return updated num
     */
    int correctPosOrgId(String orgId, String complexId);

    /**
     * update batch by ids in order
     *
     * @param mappings mappings
     */
    void updateBatchByOrderIds(List<PosPlaylistMappingDO> mappings);

    /**
     * count ppl matched pos
     * @param playlistUuids ppl uuid list
     * @return count result
     */
    Map<String, Long> getPosCount(List<String> playlistUuids);

    /**
     * get pos list
     *
     * @param titleId title uuid
     * @return pos list
     */
    List<PosInfo> getPosByTitle(String titleId);

}
