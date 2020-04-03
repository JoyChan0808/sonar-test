package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;
import java.util.Set;

/**
 * tms_playlist service
 *
 * @author oliver.lo
 * @since 2019/7/8 6:38 PM
 */
public interface ITmsPlaylistService extends IService<TmsPlaylistDO> {

    /**
     * get one tpl scope in pro
     *
     * @param playlistUuid playlist uuid
     * @return TmsPlaylistDO
     */
    TmsPlaylistDO getOneTplScopeInPro(String playlistUuid);

    /**
     * get tpl list scope in producer
     *
     * @param pplId ppl uuid
     * @param pplVersionId ppl version uuid
     * @return List<TmsPlaylistDO>
     */
    List<TmsPlaylistDO> getTplListScopeInPro(String pplId, String pplVersionId);

    /**
     * get one tpl scope in complex
     *
     * @param OrgId org uuid
     * @param tplUuid tpl uuid
     * @return TmsPlaylistDO
     */
    TmsPlaylistDO getOneTplScopeInOrg(String OrgId, String tplUuid);

    /**
     * get tpl list By cplUUID
     * @param cplUUID uuid
     * @return TmsPlaylistDO list
     */
    @Deprecated
    List<TmsPlaylistDO> getTplListByCplUuid(String cplUUID);

    /**
     * delete tpl which from tms
     *
     * @param complexUuid complex uuid
     * @param tplUUIDs tpl uuid
     */
    int deleteTplWhichFromTms(String complexUuid, List<String> tplUUIDs);

    /**
     * correct tpl org
     *
     * @param orgId org uuid
     * @param complexId complex uuid
     * @return updated num
     */
    int correctTplOrgId(String orgId, String complexId);

    /**
     * get tpl list for update
     *
     * @param pplUuid ppl uuid
     * @param tplUUIDs tpl uuids
     * @return tpl list
     */
    List<TmsPlaylistDO> getTplListForUpdate(String pplUuid, Set<String> tplUUIDs);
}
