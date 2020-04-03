package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author ${author}
 * @since 2019-04-29
 */
public interface PlaylistVersionContentAssociationMapper extends
        BaseMapper<PlaylistVersionContentAssociationDO> {

    Integer maxSortNumber(String playlistVersionUuid);

}
