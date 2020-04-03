package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.service.IPlaylistDeleteLogService;
import com.aam.producer.playlist.repository.dao.PlaylistDeleteLogMapper;
import com.aam.producer.playlist.repository.entity.PlaylistDeleteLogDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-11-01
 */
@Service
public class PlaylistDeleteLogServiceImpl extends
        ServiceImpl<PlaylistDeleteLogMapper, PlaylistDeleteLogDO> implements
        IPlaylistDeleteLogService {

    @Override
    public PlaylistDeleteLogDO getByReceiptUuid(String uuid) {
        return this.baseMapper.selectOne(new QueryWrapper<PlaylistDeleteLogDO>().lambda()
                .eq(PlaylistDeleteLogDO::getReceiptUuid, uuid));
    }
}
