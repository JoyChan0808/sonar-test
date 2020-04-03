package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.SegmentDO;
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
public interface SegmentMapper extends BaseMapper<SegmentDO> {

    SegmentDO getAutomaticSegment(Integer type);

    List<String> getApiSegmentNames(Integer type);

    List<SegmentDO> getAllSplitWeekSegment();

    List<SegmentDO> getAllSegment();
}
