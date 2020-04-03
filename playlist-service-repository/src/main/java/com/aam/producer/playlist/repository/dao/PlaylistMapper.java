package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author ${author}
 * @since 2019-04-29
 */
public interface PlaylistMapper extends BaseMapper<PlaylistDO> {

    PlaylistDO getByIdIgnoreOrgId(String pplUuid);
}
