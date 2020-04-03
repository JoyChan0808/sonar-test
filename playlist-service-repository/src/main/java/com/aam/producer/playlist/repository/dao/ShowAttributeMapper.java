package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.ShowAttributeDO;
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
public interface ShowAttributeMapper extends BaseMapper<ShowAttributeDO> {

    Integer maxShortCode();

    List<String> getTitlesBySumCode(Long shortCodeAssociation);

}
