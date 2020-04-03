package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.enums.TPlaylistActionStatusEnum;
import com.aam.producer.playlist.biz.service.IPlaylistSyncLogService;
import com.aam.producer.playlist.repository.dao.PlaylistSyncLogMapper;
import com.aam.producer.playlist.repository.entity.PlaylistSyncLogDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.ArrayList;
import java.util.List;
import org.springframework.stereotype.Service;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-04
 */
@Service
public class PlaylistSyncLogServiceImpl extends
        ServiceImpl<PlaylistSyncLogMapper, PlaylistSyncLogDO> implements IPlaylistSyncLogService {

    @Override
    public List<PlaylistSyncLogDO> getLatestPlaylistSyncLogs(String complexId,
            List<String> playlistUuids) {
        long[] latestIds = this.baseMapper.getLatestIds(complexId, playlistUuids);
        if (latestIds == null || latestIds.length == 0) {
            return new ArrayList<>();
        }
        return this.baseMapper.getLatestPlaylistSyncLogs(latestIds);
    }

    @Override
    public List<PlaylistSyncLogDO> getNotDonePlaylistSyncLogs(String complexId,
            String playlistUuid) {
        QueryWrapper<PlaylistSyncLogDO> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("complex_id", complexId);
        queryWrapper.eq("playlist_uuid", playlistUuid);
        queryWrapper.ne("status", TPlaylistActionStatusEnum.DONE.getCode());
        return this.baseMapper.selectList(queryWrapper);
    }
}
