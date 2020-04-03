package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.lib.enums.PlaylistSourceEnum;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.repository.dao.TmsPlaylistMapper;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.List;
import java.util.Set;
import org.springframework.stereotype.Service;

/**
 * tms_playlist service impl
 *
 * @author oliver.lo
 * @since 2019/7/8 6:41 PM
 */
@Service
public class TmsPlaylistServiceImpl extends ServiceImpl<TmsPlaylistMapper, TmsPlaylistDO> implements
        ITmsPlaylistService {

    @Override
    public TmsPlaylistDO getOneTplScopeInPro(String playlistUuid) {
        QueryWrapper<TmsPlaylistDO> tmsPlaylistDOWrapper = new QueryWrapper<>();
        tmsPlaylistDOWrapper.eq("playlist_uuid", playlistUuid);
        tmsPlaylistDOWrapper.eq("source", PlaylistSourceEnum.PRODUCER.getCode());
        tmsPlaylistDOWrapper.isNull("source_complex_id");
        return this.baseMapper.selectOne(tmsPlaylistDOWrapper);
    }

    @Override
    public List<TmsPlaylistDO> getTplListScopeInPro(String pplId, String pplVersionId) {
        return this.baseMapper
                .getTplListScopeInPro(PlaylistSourceEnum.PRODUCER.getCode(), pplId, pplVersionId);
    }

    @Override
    public TmsPlaylistDO getOneTplScopeInOrg(String OrgId, String tplUuid) {
        QueryWrapper<TmsPlaylistDO> wrapper = new QueryWrapper<>();
        wrapper.lambda().eq(TmsPlaylistDO::getOrganizationId, OrgId)
                .eq(TmsPlaylistDO::getPlaylistUuid, tplUuid);
        return this.baseMapper.selectOne(wrapper);
    }

    @Override
    public List<TmsPlaylistDO> getTplListByCplUuid(String cplUUID) {
        return this.baseMapper.getTplListByCplUuid(cplUUID);
    }

    @Override
    public int deleteTplWhichFromTms(String complexUuid, List<String> tplUUIDs) {
        return this.baseMapper.delete(new QueryWrapper<TmsPlaylistDO>().lambda()
                .eq(TmsPlaylistDO::getSourceComplexId, complexUuid)
                .in(TmsPlaylistDO::getPlaylistUuid, tplUUIDs));
    }

    @Override
    public int correctTplOrgId(String orgId, String complexId) {
        return this.baseMapper.correctTplOrgId(orgId, complexId);
    }

    @Override
    public List<TmsPlaylistDO> getTplListForUpdate(String pplUuid, Set<String> tplUUIDs) {
        return this.baseMapper.getTplListForUpdate(pplUuid, tplUUIDs);
    }

}
