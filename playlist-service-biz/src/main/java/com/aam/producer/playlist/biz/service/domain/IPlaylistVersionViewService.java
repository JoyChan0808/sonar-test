package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.protocol.response.PlaylistVersionInfo;
import com.aam.producer.playlist.protocol.response.SegmentSplitInfo;
import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionDO;
import com.aam.producer.playlist.sal.response.PosInfo;
import java.util.List;

public interface IPlaylistVersionViewService {

    PlaylistVersionInfo getPlaylistVersion(String versionUuid);

    List<PlaylistVersionInfo> getPlaylistVersions(String playlistUuid);

    List<PlaylistVersionInfo> getPlaylistVersions(String playlistUuid, String versions,
            String titleId);

    List<PlaylistPublishModel> getPublishMessage(PlaylistDO playlist,
            PlaylistVersionDO playlistVersion, List<PosInfo> groupPosInfos);

    void segmentSplitMatchPos(SegmentSplitInfo segmentSplitInfo, List<PosInfo> posInfoList);

    default PlaylistVersionInfo toPlaylistVersionInfo(PlaylistVersionDO playlistVersionDO,
            List<ContentInfo> contents, List<ShowAttributeGroupInfo> groupInfos) {
        PlaylistVersionInfo playlistVersionInfo = new PlaylistVersionInfo();
        playlistVersionInfo.setPlaylistUuid(playlistVersionDO.getPlaylistUuid());
        playlistVersionInfo.setCreated(playlistVersionDO.getCreated());
        playlistVersionInfo.setOrganizationId(playlistVersionDO.getOrganizationId());
        playlistVersionInfo.setExtension(playlistVersionDO.getExtension());
        playlistVersionInfo.setLastModified(playlistVersionDO.getLastModified());
        playlistVersionInfo.setPublishLater(playlistVersionDO.getPublishLater());
        playlistVersionInfo.setPlaylistVersionUuid(playlistVersionDO.getUuid());
        playlistVersionInfo
                .setStatus(PlaylistStatusEnum.getEmunStrByStatus(playlistVersionDO.getStatus()));
        playlistVersionInfo.setPublishTime(playlistVersionDO.getPublishTime());
        playlistVersionInfo.setContentList(contents);
        playlistVersionInfo.setShowAttributeGroups(groupInfos);
        return playlistVersionInfo;
    }

}
