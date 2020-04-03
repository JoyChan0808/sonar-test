package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.protocol.request.ShowAttributeGroupDTO;
import com.aam.producer.playlist.repository.entity.PlaylistShowAttributeCombinationDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-28
 */
public interface IPlaylistShowAttributeCombinationService extends
        IService<PlaylistShowAttributeCombinationDO> {

    void createShowAttributeCombination(String uuid,
            List<ShowAttributeGroupDTO> showAttributeGroups);

    void deleteShowAttributeCombination(String versionUuid);

    List<PlaylistShowAttributeCombinationDO> getByPplVersionUuid(String pplVersionUuid);

    List<PlaylistShowAttributeCombinationDO> getCombinationsBySumCode(Long code);

    List<PlaylistShowAttributeCombinationDO> getByPplVersionUuids(List<String> pplVersoinUuids);
}
