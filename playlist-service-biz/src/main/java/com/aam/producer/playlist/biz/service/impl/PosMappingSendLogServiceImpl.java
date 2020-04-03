package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.service.IPosMappingSendLogService;
import com.aam.producer.playlist.repository.dao.PosMappingSendLogMapper;
import com.aam.producer.playlist.repository.entity.PosMappingSendLogDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-10-23
 */
@Service
public class PosMappingSendLogServiceImpl extends
        ServiceImpl<PosMappingSendLogMapper, PosMappingSendLogDO> implements
        IPosMappingSendLogService {

    @Override
    public PosMappingSendLogDO getOneLog(String receiptUuid) {
        if (StringUtils.isEmpty(receiptUuid)) {
            return null;
        }
        return this.getOne(new QueryWrapper<PosMappingSendLogDO>().lambda()
                .eq(PosMappingSendLogDO::getReceiptUuid, receiptUuid));
    }
}
