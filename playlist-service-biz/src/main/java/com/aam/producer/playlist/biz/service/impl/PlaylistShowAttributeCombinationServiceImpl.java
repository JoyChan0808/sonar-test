package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.service.IPlaylistShowAttributeCombinationService;
import com.aam.producer.playlist.biz.service.IShowAttributeService;
import com.aam.producer.playlist.protocol.request.ShowAttributeGroupDTO;
import com.aam.producer.playlist.repository.dao.PlaylistShowAttributeCombinationMapper;
import com.aam.producer.playlist.repository.entity.PlaylistShowAttributeCombinationDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
public class PlaylistShowAttributeCombinationServiceImpl extends
        ServiceImpl<PlaylistShowAttributeCombinationMapper, PlaylistShowAttributeCombinationDO> implements
        IPlaylistShowAttributeCombinationService {

    private static final Logger logger = LoggerFactory
            .getLogger(PlaylistShowAttributeCombinationServiceImpl.class);

    private final IShowAttributeService iShowAttributeService;

    @Autowired
    public PlaylistShowAttributeCombinationServiceImpl(
            IShowAttributeService iShowAttributeService) {
        this.iShowAttributeService = iShowAttributeService;
    }

    @Override
    public void createShowAttributeCombination(String uuid,
            List<ShowAttributeGroupDTO> showTagsGroup) {
        showTagsGroup.forEach(showTags -> {
            Long shortCodeSum = iShowAttributeService
                    .getShortCodeSumByTitles(showTags.getAttributes());
            PlaylistShowAttributeCombinationDO playlistShowAttributeCombinationDO = new PlaylistShowAttributeCombinationDO();
            playlistShowAttributeCombinationDO.setPlaylistUuid(uuid);
            playlistShowAttributeCombinationDO.setName(showTags.getName());
            playlistShowAttributeCombinationDO.setShortCodeAssociation(shortCodeSum);
            baseMapper.insert(playlistShowAttributeCombinationDO);
        });
    }

    @Override
    public List<PlaylistShowAttributeCombinationDO> getByPplVersionUuid(String pplVersionUuid) {
        QueryWrapper<PlaylistShowAttributeCombinationDO> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("playlist_uuid", pplVersionUuid);
        return baseMapper.selectList(queryWrapper);
    }

    @Override
    public void deleteShowAttributeCombination(String versionUuid) {
        QueryWrapper<PlaylistShowAttributeCombinationDO> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("playlist_uuid", versionUuid);
        baseMapper.delete(queryWrapper);
    }


    @Override
    public List<PlaylistShowAttributeCombinationDO> getCombinationsBySumCode(Long code) {
        QueryWrapper<PlaylistShowAttributeCombinationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("short_code_association", code);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<PlaylistShowAttributeCombinationDO> getByPplVersionUuids(List<String> pplVersoinUuids) {
        QueryWrapper<PlaylistShowAttributeCombinationDO> wrapper = new QueryWrapper<>();
        if (CollectionUtils.isEmpty(pplVersoinUuids)){
            return new ArrayList<>();
        }
        wrapper.in("playlist_uuid", pplVersoinUuids);
        return baseMapper.selectList(wrapper);
    }

}
