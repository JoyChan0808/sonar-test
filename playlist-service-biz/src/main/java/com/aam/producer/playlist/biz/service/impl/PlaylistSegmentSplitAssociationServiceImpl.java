package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.event.ChangePplReleaseVersionEvent;
import com.aam.producer.playlist.biz.service.IPlaylistSegmentSplitAssociationService;
import com.aam.producer.playlist.repository.dao.PlaylistSegmentSplitAssociationMapper;
import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-05-09
 */
@Service
public class PlaylistSegmentSplitAssociationServiceImpl extends
        ServiceImpl<PlaylistSegmentSplitAssociationMapper, PlaylistSegmentSplitAssociationDO> implements
        IPlaylistSegmentSplitAssociationService, ApplicationListener<ChangePplReleaseVersionEvent> {

    @Override
    public void createAssociation(String pplUuid, String pplVersionUuid,
            String segmentAssociationUuid, String titleUuid, String segmentSplitUuid) {
        PlaylistSegmentSplitAssociationDO playlistSegmentSplitAssociationDO = new PlaylistSegmentSplitAssociationDO();
        playlistSegmentSplitAssociationDO.setPlaylistUuid(pplUuid);
        playlistSegmentSplitAssociationDO.setPplVersionId(pplVersionUuid);
        playlistSegmentSplitAssociationDO.setSegmentSplitUuid(segmentSplitUuid);
        playlistSegmentSplitAssociationDO.setTitleId(titleUuid);
        playlistSegmentSplitAssociationDO.setSegmentAssociationUuid(segmentAssociationUuid);
        baseMapper.insert(playlistSegmentSplitAssociationDO);
    }

    @Override
    public void deleteAssociation(String segmentSplitUuid) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("segment_split_uuid", segmentSplitUuid);
        baseMapper.delete(wrapper);
    }

    @Override
    public void batchDeleteAssociation(List<String> uuids) {
        if(CollectionUtils.isEmpty(uuids)){
            return;
        }
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.in("segment_split_uuid", uuids);
        baseMapper.delete(wrapper);
    }

    @Override
    public List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByPlaylistUuid(
            String playlistUuid) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("playlist_uuid", playlistUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByPlaylistVersionUuid(
            String versionUuid) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("ppl_version_id", versionUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByContentAssociationUuid(
            String contentAssociationUuid) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("segment_association_uuid", contentAssociationUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByContentAssociationUuids(Set<String> contentAssociationUuids) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        if (CollectionUtils.isEmpty(contentAssociationUuids)){return new ArrayList<>();}
        wrapper.in("segment_association_uuid", contentAssociationUuids);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByContentAssociationUuid(
            String contentAssociationUuid, String titleUuid) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("segment_association_uuid", contentAssociationUuid);
        if (StringUtils.isNotBlank(titleUuid)) {
            wrapper.eq("title_id", titleUuid);
        }
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByTitle(String titleUuid) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("title_id", titleUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistSegmentSplitAssociationDO> getAllSegmentSplitByTitle(String titleUuid) {
        return baseMapper.getAllSegmentSplitByTitle(titleUuid);
    }

    @Override
    public PlaylistSegmentSplitAssociationDO getSegmentSplitBySegmentSplitUuid(
            String segmentSplitUuid) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("segment_split_uuid", segmentSplitUuid);
        return baseMapper.selectOne(wrapper);
    }

    @Override
    public List<PlaylistSegmentSplitAssociationDO> getSegmentSplitBySegmentSplitUuids(
            Set<String> segmentSplitUuids) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        if (CollectionUtils.isEmpty(segmentSplitUuids)){return new ArrayList<>(0);}
        wrapper.in("segment_split_uuid", segmentSplitUuids);
        return baseMapper.selectList(wrapper);
    }


    @Override
    public void onApplicationEvent(ChangePplReleaseVersionEvent changePplReleaseVersionEvent) {
        QueryWrapper<PlaylistSegmentSplitAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("ppl_version_id", changePplReleaseVersionEvent.getOldVersionUuid());
        wrapper.isNotNull("title_id");
        List<PlaylistSegmentSplitAssociationDO> playlistSegmentSplitAssociationDOS = baseMapper
                .selectList(wrapper);
        playlistSegmentSplitAssociationDOS.forEach(playlistSegmentSplitAssociationDO -> {
            playlistSegmentSplitAssociationDO
                    .setPplVersionId(changePplReleaseVersionEvent.getNewVersionUuid());
            baseMapper.updateById(playlistSegmentSplitAssociationDO);
        });
    }

}
