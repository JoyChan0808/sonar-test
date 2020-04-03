package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.enums.TPlaylistActionStatusEnum;
import com.aam.producer.playlist.biz.service.IPlaylistSendLogService;
import com.aam.producer.playlist.repository.dao.PlaylistSendLogMapper;
import com.aam.producer.playlist.repository.entity.PlaylistSendLogDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

/**
 * playlist send log service impl
 *
 * @author oliver.lo
 * @since 2019/5/14 5:57 PM
 */
@Service
public class PlaylistSendLogServiceImpl extends
        ServiceImpl<PlaylistSendLogMapper, PlaylistSendLogDO> implements IPlaylistSendLogService {

    @Override
    public List<PlaylistSendLogDO> getLatestPlaylistSendLogs(String complexId,
            String playlistUuid) {
        long[] latestIds = this.baseMapper.getLatestIds(complexId, playlistUuid);
        if (latestIds == null || latestIds.length == 0) {
            return new ArrayList<>();
        }
        return this.baseMapper.getLatestPlaylistSendLogs(latestIds);
    }

    @Override
    public PlaylistSendLogDO getPlaylistSendLogByReceiptUuid(String receiptUuid) {
        QueryWrapper<PlaylistSendLogDO> wrapper = new QueryWrapper<>();
        wrapper.eq("receipt_uuid", receiptUuid);
        return this.baseMapper.selectOne(wrapper);
    }

    @Override
    public PlaylistSendLogDO getPlaylistSendLogByActionId(String actionId) {
        QueryWrapper<PlaylistSendLogDO> wrapper = new QueryWrapper<>();
        wrapper.eq("action_id", actionId);
        return this.baseMapper.selectOne(wrapper);
    }

    @Override
    public List<PlaylistSendLogDO> getNotDonePlaylistSendLogs(String complexId,
            String playlistUuid) {
        QueryWrapper<PlaylistSendLogDO> wrapper = new QueryWrapper<>();
        wrapper.eq("complex_id", complexId);
        wrapper.eq("playlist_uuid", playlistUuid);
        wrapper.ne("status", TPlaylistActionStatusEnum.DONE.getCode());
        return this.baseMapper.selectList(wrapper);
    }

    @Override
    public Map<String, List<String>> getComplexTplListMap(List<String> tplUUIDs) {
        List<PlaylistSendLogDO> groupByTplAndComplex = this.baseMapper
                .getGroupByTplAndComplex(tplUUIDs);
        if (CollectionUtils.isEmpty(groupByTplAndComplex)) {
            return null;
        }
        return groupByTplAndComplex.stream()
                .collect(Collectors.groupingBy(PlaylistSendLogDO::getComplexId, Collectors
                        .mapping(PlaylistSendLogDO::getPlaylistUuid, Collectors.toList())));
    }
}
