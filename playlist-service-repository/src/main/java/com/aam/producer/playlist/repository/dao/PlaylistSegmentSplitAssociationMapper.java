package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author ${author}
 * @since 2019-04-29
 */
public interface PlaylistSegmentSplitAssociationMapper extends
        BaseMapper<PlaylistSegmentSplitAssociationDO> {

    List<PlaylistSegmentSplitAssociationDO> getAllSegmentSplitByTitle(String titleUuid);

}
