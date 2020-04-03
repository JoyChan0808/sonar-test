package com.aam.producer.playlist.biz.service.impl;


import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.event.ChangePplStatusEvent;
import com.aam.producer.playlist.biz.service.IPlaylistVersionService;
import com.aam.producer.playlist.biz.util.EventPublish;
import com.aam.producer.playlist.repository.dao.PlaylistVersionMapper;
import com.aam.producer.playlist.repository.entity.PlaylistVersionDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.List;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-04
 */
@Service
public class PlaylistVersionServiceImpl extends
        ServiceImpl<PlaylistVersionMapper, PlaylistVersionDO> implements IPlaylistVersionService {

    private final EventPublish eventPublish;

    @Autowired
    public PlaylistVersionServiceImpl(EventPublish eventPublish) {
        this.eventPublish = eventPublish;
    }

    @Override
    public String createPlaylistVersion(String playlistUuid) {
        PlaylistVersionDO playlistVersionDO = new PlaylistVersionDO();
        playlistVersionDO.setStatus(PlaylistStatusEnum.DRAFT.getStatus());
        playlistVersionDO.setPlaylistUuid(playlistUuid);
        playlistVersionDO.setUuid(UUID.randomUUID().toString());
        baseMapper.insert(playlistVersionDO);
        eventPublish.publishEvent(new ChangePplStatusEvent(playlistUuid));
        return playlistVersionDO.getUuid();
    }

    @Override
    public void updatePlaylistVersion(String versionUuid, int status) {
        PlaylistVersionDO playlistVersionDO = baseMapper.selectById(versionUuid);
        playlistVersionDO.setStatus(status);
        baseMapper.updateById(playlistVersionDO);
        eventPublish.publishEvent(new ChangePplStatusEvent(playlistVersionDO.getPlaylistUuid()));
    }

    @Override
    public void deletePlaylistVersion(String versionUuid) {
        PlaylistVersionDO playlistVersionDO = baseMapper.selectById(versionUuid);
        if (playlistVersionDO == null) {
            return;
        }
        baseMapper.deleteById(versionUuid);
        eventPublish.publishEvent(new ChangePplStatusEvent(playlistVersionDO.getPlaylistUuid()));
    }

    @Override
    public List<PlaylistVersionDO> getPlaylistVersionByPplUuid(String playlistUuid) {
        QueryWrapper<PlaylistVersionDO> wrapper = new QueryWrapper<>();
        wrapper.eq("playlist_uuid", playlistUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public void updatePublishTime(String uuid, Long time, Boolean publishLater) {
        PlaylistVersionDO playlistVersionDO = baseMapper.selectById(uuid);
        playlistVersionDO.setPublishTime(time);
        playlistVersionDO.setPublishLater(publishLater);
        baseMapper.updateById(playlistVersionDO);
    }

    @Override
    public void deletePlaylistVersion(String playlistUuid, Integer status) {
        QueryWrapper<PlaylistVersionDO> wrapper = new QueryWrapper<>();
        wrapper.eq("playlist_uuid", playlistUuid);
        wrapper.eq("status", status);
        baseMapper.delete(wrapper);
        eventPublish.publishEvent(new ChangePplStatusEvent(playlistUuid));
    }

    @Override
    public PlaylistVersionDO getReleasePlaylistVersionByPplUuid(String playlistUuid) {
        QueryWrapper<PlaylistVersionDO> wrapper = new QueryWrapper<>();
        wrapper.eq("playlist_uuid", playlistUuid);
        wrapper.eq("status", PlaylistStatusEnum.RELEASE.getStatus());
        return baseMapper.selectOne(wrapper);
    }

    @Override
    public PlaylistVersionDO getDraftPlaylistVersionByPplUuid(String playlistUuid) {
        QueryWrapper<PlaylistVersionDO> wrapper = new QueryWrapper<>();
        wrapper.eq("playlist_uuid", playlistUuid);
        wrapper.eq("status", PlaylistStatusEnum.DRAFT.getStatus());
        return baseMapper.selectOne(wrapper);
    }

    @Override
    public List<PlaylistVersionDO> getByIds(List<String> playlistVersionUuids) {
        return baseMapper.selectBatchIds(playlistVersionUuids);
    }

}
