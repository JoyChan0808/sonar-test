package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.protocol.response.PlaylistInfo;
import com.aam.producer.playlist.protocol.response.PplViewInfo;
import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.protocol.response.TitleInfo1;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PplViewDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.aam.utils.model.PageListResult;
import java.util.List;

public interface IPlaylistViewService {

    /**
     * 获取ppl
     *
     * @param playlistUuid playlistUuid
     * @return PlaylistInfo
     */
    PlaylistInfo getPlaylist(String playlistUuid);

    /**
     * 获取ppl
     *
     * @param playlistUuid playlistUuid
     * @param versions     versions
     * @param titleId      titleId
     * @return PlaylistInfo
     */
    PlaylistInfo getPlaylist(String playlistUuid, String versions, String titleId);

    /**
     * 根据code获取ppl
     *
     * @param code code
     * @return PlaylistDO
     */
    PlaylistDO getPlaylist(Long code);

    PageListResult<PplViewInfo> search(Integer pageNum, Integer pageSize, String search,
            String useId);

    default PlaylistInfo toPlaylistInfo(PlaylistDO playlistDO) {
        if (playlistDO == null) {
            return null;
        }
        PlaylistInfo playlistInfo = new PlaylistInfo();
        playlistInfo.setAutomaticallyApply(playlistDO.getAutomaticallyApply());
        playlistInfo.setTitle(playlistDO.getTitle());
        playlistInfo.setCreated(playlistDO.getCreated());
        playlistInfo.setUuid(playlistDO.getUuid());
        playlistInfo.setLastModified(playlistDO.getLastModified());
        playlistInfo.setStatus(statusToString(playlistDO.getStatus()));
        return playlistInfo;
    }

    default String statusToString(Integer status) {
        if (status == null || status == 0) {
            return "";
        }
        String[] statusArray = new String[]{
                PlaylistStatusEnum.DRAFT.getStatusStr(),
                PlaylistStatusEnum.RELEASE.getStatusStr(),
                PlaylistStatusEnum.DRAFT_AND_RELEASE.getStatusStr()
        };
        return statusArray[(status & 3) - 1];
    }

    default PplViewInfo toPplViewInfo(PplViewDO pplViewDO, List<ShowAttributeGroupInfo> showTypes) {
        PplViewInfo playlistInfo = new PplViewInfo();
        playlistInfo.setPplUuid(pplViewDO.getPplUuid());
        playlistInfo.setPlaylistUuid(pplViewDO.getPlaylistUuid());
        playlistInfo.setReleaseVersionUuid(pplViewDO.getReleaseVersionUuid());
        playlistInfo.setComplexUuid(pplViewDO.getComplexUuid());
        playlistInfo.setOrganizationId(pplViewDO.getOrganizationId());
        playlistInfo.setTitle(pplViewDO.getTitle());
        playlistInfo.setStatus(statusToString(pplViewDO.getStatus()));
        playlistInfo.setAutomaticallyApply(pplViewDO.getAutomaticallyApply());
        playlistInfo.setLastModified(pplViewDO.getLastModified());
        playlistInfo.setShows(pplViewDO.getShows());
        playlistInfo.setSites(pplViewDO.getSites());
        playlistInfo.setType(pplViewDO.getType());
        playlistInfo.setIs3d(pplViewDO.getIs3d());
        playlistInfo.setShowTypes(showTypes);
        return playlistInfo;
    }

    PageListResult<PplViewInfo> searchTpl(Integer pageNum, Integer pageSize, String search,
            String organizationId);

    PageListResult<PplViewInfo> searchByTitle(String titleUuid, String title, String useId);

    PageListResult<PplViewInfo> searchByContentId(String title, String contentId);

    default PplViewInfo toPplViewInfo(PlaylistDO playlistDO, Long size) {
        PplViewInfo pplViewInfo = new PplViewInfo();
        pplViewInfo.setTitle(playlistDO.getTitle());
        pplViewInfo.setPplUuid(playlistDO.getUuid());
        pplViewInfo.setStatus(PlaylistStatusEnum.getEmunStrByStatus(playlistDO.getStatus()));
        pplViewInfo.setAutomaticallyApply(playlistDO.getAutomaticallyApply() ? 1 : 0);
        pplViewInfo.setLastModified(playlistDO.getLastModified());
        pplViewInfo.setType("producer");
        pplViewInfo.setShows(size);
        return pplViewInfo;
    }

    default PplViewInfo toPplViewInfo(TmsPlaylistDO tmsPlaylistDO){
        PplViewInfo playlistInfo = new PplViewInfo();
        playlistInfo.setPplUuid(tmsPlaylistDO.getSourcePplId());
        playlistInfo.setPlaylistUuid(tmsPlaylistDO.getPlaylistUuid());
        playlistInfo.setComplexUuid(tmsPlaylistDO.getSourceComplexId());
        playlistInfo.setOrganizationId(tmsPlaylistDO.getOrganizationId());
        playlistInfo.setTitle(tmsPlaylistDO.getTitle());
        playlistInfo.setLastModified(tmsPlaylistDO.getLastModified());
        playlistInfo.setType("tms");
        return playlistInfo;
    }

    List<TitleInfo1> getPlaylistSegmentTitleInfo(String playlistUuid, String associationUuid);
}
