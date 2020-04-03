package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.repository.entity.PosMappingSendLogDO;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-10-23
 */
public interface IPosMappingSendLogService extends IService<PosMappingSendLogDO> {

    /**
     * get a log by receiptUuid
     *
     * @param receiptUuid receiptUuid
     * @return log
     */
    PosMappingSendLogDO getOneLog(String receiptUuid);
}
