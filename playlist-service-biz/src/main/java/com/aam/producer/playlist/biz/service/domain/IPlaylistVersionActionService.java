package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.message.AutomationChangeDataDTO;
import com.aam.producer.playlist.protocol.request.CplMetaDTO;
import com.aam.producer.playlist.protocol.request.PlaylistVersionDTO;
import com.aam.producer.playlist.protocol.request.PublishDTO;
import com.aam.producer.playlist.protocol.request.ShowAttributeGroupDTO;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.sal.response.PosInfo;
import java.util.List;

public interface IPlaylistVersionActionService {

    String createPlaylistVersion(PlaylistVersionDTO playListVersionDTO);

    void updatePlaylistVersion(String versionUuid, PlaylistVersionDTO playListVersionDTO);

    void deletePlaylistVersion(String versionUuid);

    void publishPlaylistVersion(String versionUuid, PublishDTO publishDTO);

    void publishPlaylistVersion(String pplUuid,String versionUuid);

    void unPublishPlaylistVersion(String versionUuid, Boolean keepRelease);

    void copyPlaylistVersion(String fromPlaylistUuid, String toPlaylistUuid, boolean draft,
            List<ShowAttributeGroupDTO> showAttributeGroupDTOS);

    void createTpl(PlaylistDO playlist, List<PosPlaylistMappingDO> posPlaylistMappingDOS,
            int action);

    List<PlaylistDO> modifyCplMeta(CplMetaDTO cplMetaDTO);

    void setPosFilmHallInfos(List<PosInfo> posInfos);

    List<PosInfo> toPosInfo(List<PosPlaylistMappingDO> posPlaylistMappingDOS);

    void unPublish(String pplUuid, String pplVersionUuid);

    String createPlaylistVersionDraft(String versionUuid, Boolean copyContent);

    List<PlaylistDO> automationChanged(AutomationChangeDataDTO dto);

    void allSegmentPublish(String playlistUuid,String pplVersionUuid);
}
