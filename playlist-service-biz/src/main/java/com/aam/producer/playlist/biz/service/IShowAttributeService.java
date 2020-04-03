package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.repository.entity.ShowAttributeDO;
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
public interface IShowAttributeService extends IService<ShowAttributeDO> {

    List<String> getTitlesBySumCode(Long shortCodeAssociation);

    Long getShortCodeSumByTitles(List<String> showTags);

    List<Long> getShortCodeSumByTitlesList(String showTagsList);

}
