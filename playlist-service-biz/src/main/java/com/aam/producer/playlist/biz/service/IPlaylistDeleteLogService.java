package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.repository.entity.PlaylistDeleteLogDO;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-11-01
 */
public interface IPlaylistDeleteLogService extends IService<PlaylistDeleteLogDO> {

    PlaylistDeleteLogDO getByReceiptUuid(String uuid);
}
