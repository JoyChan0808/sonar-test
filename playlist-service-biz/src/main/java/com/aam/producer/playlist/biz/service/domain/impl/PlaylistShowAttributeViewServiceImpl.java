package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.service.IPlaylistShowAttributeCombinationService;
import com.aam.producer.playlist.biz.service.IShowAttributeService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistShowAttributeViewService;
import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.repository.entity.PlaylistShowAttributeCombinationDO;
import java.util.List;
import java.util.stream.Collectors;

import com.aam.producer.playlist.repository.entity.ShowAttributeDO;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;

@Service
public class PlaylistShowAttributeViewServiceImpl implements IPlaylistShowAttributeViewService {

    private final IPlaylistShowAttributeCombinationService combinationService;

    private final IShowAttributeService iShowAttributeService;

    public PlaylistShowAttributeViewServiceImpl(
            IPlaylistShowAttributeCombinationService combinationService,
            IShowAttributeService iShowAttributeService) {
        this.combinationService = combinationService;
        this.iShowAttributeService = iShowAttributeService;
    }

    @Override
    public List<ShowAttributeGroupInfo> getByPplVersionUuid(String pplVersionUuid) {
        List<PlaylistShowAttributeCombinationDO> combinationDOS = combinationService
                .getByPplVersionUuid(pplVersionUuid);
        return getShowAttributeGroupInfos(combinationDOS);
    }

    @NotNull
    private List<ShowAttributeGroupInfo> getShowAttributeGroupInfos(List<PlaylistShowAttributeCombinationDO> combinationDOS) {
        List<ShowAttributeDO> showAttributeDOS = iShowAttributeService.list();
        return combinationDOS.stream().map(playlistShowAttributeCombinationDO -> {
            List<String> strings = showAttributeDOS.stream().filter(showAttributeDO ->
                    ((1L<<showAttributeDO.getShortCode())&playlistShowAttributeCombinationDO.getShortCodeAssociation())>0)
                    .map(ShowAttributeDO::getTitle).collect(Collectors.toList());
            return toShowAttributeGroupInfo(playlistShowAttributeCombinationDO, strings);
        }).collect(Collectors.toList());
    }

    @Override
    public List<Long> getCodesByPplVersionUuid(String pplVersionUuid) {
        List<PlaylistShowAttributeCombinationDO> combinationDOS = combinationService
                .getByPplVersionUuid(pplVersionUuid);
        return combinationDOS.stream()
                .map(PlaylistShowAttributeCombinationDO::getShortCodeAssociation)
                .collect(
                        Collectors.toList());
    }

    public List<ShowAttributeGroupInfo> getByPplVersionUuids(List<String> pplVersionUuids) {
        List<PlaylistShowAttributeCombinationDO> combinationDOS = combinationService
                .getByPplVersionUuids(pplVersionUuids);
        return getShowAttributeGroupInfos(combinationDOS);
    }


}
