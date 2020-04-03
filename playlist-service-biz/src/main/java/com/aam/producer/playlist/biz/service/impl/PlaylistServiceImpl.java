package com.aam.producer.playlist.biz.service.impl;


import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.repository.dao.PlaylistMapper;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.List;
import java.util.Set;
import java.util.UUID;
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
public class PlaylistServiceImpl extends ServiceImpl<PlaylistMapper, PlaylistDO> implements
        IPlaylistService {

    @Override
    public String createPlaylist(String title, boolean automaticallyApply) {
        PlaylistDO playlistDO = new PlaylistDO();
        playlistDO.setUuid(UUID.randomUUID().toString());
        playlistDO.setTitle(title);
        playlistDO.setAutomaticallyApply(automaticallyApply);
        baseMapper.insert(playlistDO);
        return playlistDO.getUuid();
    }

    @Override
    public void updatePlaylist(String playlistUuid, String title) {
        PlaylistDO playlistDO = baseMapper.selectById(playlistUuid);
        playlistDO.setTitle(title);
        baseMapper.updateById(playlistDO);
    }

    @Override
    public void deletePlaylist(String playlistUuid) {
        baseMapper.deleteById(playlistUuid);
    }

    @Override
    public PlaylistDO getPlaylist(String playlistUuid) {
        return baseMapper.selectById(playlistUuid);
    }

    @Override
    public PlaylistDO getByTitle(String title) {
        QueryWrapper<PlaylistDO> wrapper = new QueryWrapper<>();
        wrapper.eq("title", title);
        return baseMapper.selectOne(wrapper);
    }

    @Override
    public List<PlaylistDO> selectBatchIds(Set<String> pplUuids) {
        return baseMapper.selectBatchIds(pplUuids);
    }

    @Override
    public PlaylistDO getByIdIgnoreOrgId(String pplUuid) {
        return baseMapper.getByIdIgnoreOrgId(pplUuid);
    }

}
