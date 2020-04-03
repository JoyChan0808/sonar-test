package com.aam.producer.playlist.biz.service.impl;


import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.repository.dao.PlaylistVersionContentAssociationMapper;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-28
 */
@Service
public class PlaylistVersionContentAssociationServiceImpl extends
        ServiceImpl<PlaylistVersionContentAssociationMapper, PlaylistVersionContentAssociationDO> implements
        IPlaylistVersionContentAssociationService {

    private static final Logger logger = LoggerFactory
            .getLogger(PlaylistVersionContentAssociationServiceImpl.class);


    @Override
    public void deleteAssociation(String versionUuid) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("ppl_version_id", versionUuid);
        baseMapper.delete(wrapper);
    }

    @Override
    public void deleteAssociation(String versionUuid, String contentUuid) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("ppl_version_id", versionUuid);
        wrapper.eq("content_id", contentUuid);
        baseMapper.delete(wrapper);
    }


    @Override
    public String insertAssociation(String playlistUuid, String pplVersionId, ContentDTO contentDTO,
            Integer sortNumber) {
        PlaylistVersionContentAssociationDO playlistVersionContentAssociationDO = new PlaylistVersionContentAssociationDO();
        playlistVersionContentAssociationDO.setContentId(contentDTO.getContentId());
        playlistVersionContentAssociationDO
                .setContentType(ContentTypeEnum.getByName(contentDTO.getContentType()).getCode());
        playlistVersionContentAssociationDO.setSortNumber(sortNumber);
        playlistVersionContentAssociationDO.setPlaylistUuid(playlistUuid);
        playlistVersionContentAssociationDO.setPplVersionId(pplVersionId);
        playlistVersionContentAssociationDO.setUuid(
                contentDTO.getContentAssociationUuid() == null ? UUID.randomUUID().toString()
                        : contentDTO.getContentAssociationUuid());
        playlistVersionContentAssociationDO.setExtension(contentDTO.getExtension());
        playlistVersionContentAssociationDO.setTitle(contentDTO.getTitle());
        baseMapper.insert(playlistVersionContentAssociationDO);
        return playlistVersionContentAssociationDO.getUuid();
    }

    @Override
    public Integer maxSortNumber(String playlistVersionUuid) {
        Integer maxSortNumber = baseMapper.maxSortNumber(playlistVersionUuid);
        if (maxSortNumber == null) {
            maxSortNumber = 0;
        }
        return maxSortNumber;
    }

    @Override
    public boolean hasAutomaticSegment(String playlistVersionUuid) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("ppl_version_id", playlistVersionUuid);
        wrapper.eq("content_type", SegmentTypeEnum.AUTOMATIC_SEGMENT.getCode());
        return baseMapper.selectCount(wrapper) > 0;
    }

    @Override
    public List<PlaylistVersionContentAssociationDO> getAssociations(String versionUuid) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("ppl_version_id", versionUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistVersionContentAssociationDO> getAssociations(List<String> versionUuids) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        if (CollectionUtils.isEmpty(versionUuids)){return new ArrayList<>();}
        wrapper.in("ppl_version_id", versionUuids);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public boolean contains(String segmentUuid) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("content_id", segmentUuid);
        wrapper.eq("content_type", ContentTypeEnum.SEGMENT);
        return baseMapper.selectCount(wrapper) > 0;
    }

    @Override
    public PlaylistVersionContentAssociationDO getAssociationsByUuid(String associationUuid,
            String versionUuid) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("uuid", associationUuid);
        wrapper.eq("ppl_version_id", versionUuid);
        return baseMapper.selectOne(wrapper);
    }

    @Override
    public List<PlaylistVersionContentAssociationDO> listAssociationsByUuid(
            String associationUuid) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("uuid", associationUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistVersionContentAssociationDO> getAssociationsByContentUuid(String cplUUID) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("content_id", cplUUID);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistVersionContentAssociationDO> getAssociationsByContentUuids(
            List<String> contentUuids) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        if (CollectionUtils.isEmpty(contentUuids)){return new ArrayList<>();}
        wrapper.in("content_id", contentUuids);
        return baseMapper.selectList(wrapper);
    }


}
