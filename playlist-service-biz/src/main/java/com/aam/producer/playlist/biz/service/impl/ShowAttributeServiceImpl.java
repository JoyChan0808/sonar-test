package com.aam.producer.playlist.biz.service.impl;


import com.aam.producer.playlist.biz.service.IShowAttributeService;
import com.aam.producer.playlist.repository.dao.ShowAttributeMapper;
import com.aam.producer.playlist.repository.entity.ShowAttributeDO;
import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-28
 */
@Service
public class ShowAttributeServiceImpl extends
        ServiceImpl<ShowAttributeMapper, ShowAttributeDO> implements IShowAttributeService {

    private static final Logger logger = LoggerFactory.getLogger(ShowAttributeServiceImpl.class);


    @Override
    public List<String> getTitlesBySumCode(Long shortCodeAssociation) {
        return baseMapper.getTitlesBySumCode(shortCodeAssociation);
    }

    @Override
    public List<Long> getShortCodeSumByTitlesList(String showTagsList) {
        List<List<String>> lists = JSON
                .parseObject(showTagsList, new TypeReference<List<List<String>>>() {
                });
        return lists.stream().map(this::getShortCodeSumByTitles).collect(Collectors.toList());
    }

    @Override
    public Long getShortCodeSumByTitles(List<String> showTags) {
        return showTags.stream().mapToLong(value -> 1l << getShortCodeByTitle(value)).sum();
    }

    private Integer getShortCodeByTitle(String title) {
        QueryWrapper<ShowAttributeDO> wrapper = new QueryWrapper<>();
        wrapper.eq("title", title);
        ShowAttributeDO showAttributeDO = baseMapper.selectOne(wrapper);
        if (showAttributeDO != null) {
            return showAttributeDO.getShortCode();
        }
        return createShowAttributeDO(title).getShortCode();
    }


    private ShowAttributeDO createShowAttributeDO(String title) {
        Integer maxShortCode = baseMapper.maxShortCode();
        if (maxShortCode == null) {
            maxShortCode = 0;
        } else {
            maxShortCode++;
        }

        if (maxShortCode > 62) {
            logger.error(
                    "Show tags type is beyond the scope of system design.Show tags have a maximum of 63.");
            throw new BizException(BaseResultCode.SYSTEM_ERROR);
        }

        ShowAttributeDO showAttributeDO = new ShowAttributeDO();
        showAttributeDO.setShortCode(maxShortCode);
        showAttributeDO.setTitle(title);
        showAttributeDO.setUuid(UUID.randomUUID().toString());
        baseMapper.insert(showAttributeDO);
        return showAttributeDO;
    }
}
