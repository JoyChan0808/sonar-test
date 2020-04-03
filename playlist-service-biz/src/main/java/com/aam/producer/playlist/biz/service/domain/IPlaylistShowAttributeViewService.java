package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.repository.entity.PlaylistShowAttributeCombinationDO;
import java.util.List;

public interface IPlaylistShowAttributeViewService {

    List<ShowAttributeGroupInfo> getByPplVersionUuid(String pplVersionUuid);

    List<Long> getCodesByPplVersionUuid(String pplVersionUuid);

    default ShowAttributeGroupInfo toShowAttributeGroupInfo(
            PlaylistShowAttributeCombinationDO playlistShowAttributeCombinationDO,
            List<String> attributes) {
        ShowAttributeGroupInfo showAttributeGroupInfo = new ShowAttributeGroupInfo();
        showAttributeGroupInfo.setName(playlistShowAttributeCombinationDO.getName());
        showAttributeGroupInfo.setAttributes(attributes);
        showAttributeGroupInfo.setPplVersionUuid(playlistShowAttributeCombinationDO.getPlaylistUuid());
        return showAttributeGroupInfo;
    }

    List<ShowAttributeGroupInfo> getByPplVersionUuids(List<String> pplVersionUuids);
}
