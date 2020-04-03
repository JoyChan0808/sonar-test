package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentViewService;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.springframework.stereotype.Service;

@Service
public class PlaylistVersionContentViewServiceImpl implements IPlaylistVersionContentViewService {

    private final IPlaylistVersionContentAssociationService contentAssociationService;

    private final IPlaylistVersionService playlistVersionService;

    public PlaylistVersionContentViewServiceImpl(
            IPlaylistVersionContentAssociationService contentAssociationService,
            IPlaylistVersionService playlistVersionService) {
        this.contentAssociationService = contentAssociationService;
        this.playlistVersionService = playlistVersionService;
    }

    @Override
    public List<ContentInfo> getByPplVersionUuid(String pplVersionUuid) {
        List<PlaylistVersionContentAssociationDO> contentAssociationDOS = contentAssociationService
                .getAssociations(pplVersionUuid);
        return contentAssociationDOS.stream().map(this::toContentInfo).collect(Collectors.toList());
    }

    @Override
    public List<PlaylistVersionContentAssociationDO> getByContentId(String contentId) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("content_id", contentId);
        return contentAssociationService.list(wrapper);
    }

    @Override
    public PlaylistVersionContentAssociationDO getByContentAssociationUuid(
            String contentAssociationUuid) {
        QueryWrapper<PlaylistVersionContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("uuid", contentAssociationUuid);
        List<PlaylistVersionContentAssociationDO> associationDOS = contentAssociationService
                .list(wrapper);
        if (associationDOS.size() == 1) {
            return associationDOS.get(0);
        }
        return associationDOS.stream().filter(playlistVersionContentAssociationDO -> {
            PlaylistVersionDO playlistVersionDO = playlistVersionService.
                    getById(playlistVersionContentAssociationDO.getPplVersionId());
            return Objects
                    .equals(playlistVersionDO.getStatus(), PlaylistStatusEnum.RELEASE.getStatus());
        }).findFirst().orElse(null);
    }

    @Override
    public List<String> getPlaylistUuidsByContentUuid(String contentUuid) {
        List<PlaylistVersionContentAssociationDO> playlistVersionContentAssociationDOS = getByContentId(
                contentUuid);
        if (playlistVersionContentAssociationDOS.isEmpty()) {
            return new ArrayList<>();
        }
        return playlistVersionContentAssociationDOS.stream()
                .filter(playlistVersionContentAssociationDO -> {
                    PlaylistVersionDO playlistVersionDO = playlistVersionService.getById(
                            playlistVersionContentAssociationDO.getPplVersionId());
                    return Objects.equals(PlaylistStatusEnum.RELEASE.getStatus(),
                            playlistVersionDO.getStatus());
                }).map(PlaylistVersionContentAssociationDO::getPlaylistUuid).distinct()
                .collect(Collectors.toList());
    }

    @Override
    public List<ContentInfo> getByPplVersionUuids(List<String> pplVersionUuids) {
        List<PlaylistVersionContentAssociationDO> contentAssociationDOS = contentAssociationService
                .getAssociations(pplVersionUuids);
        return contentAssociationDOS.stream().map(this::toContentInfo).collect(Collectors.toList());
    }

}
