package com.aam.producer.playlist.biz.service;


import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;
import java.util.Set;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-04
 */
public interface IPlaylistService extends IService<PlaylistDO> {

    String createPlaylist(String title, boolean automaticallyApply);

    void updatePlaylist(String playlistUuid, String title);

    void deletePlaylist(String playlistUuid);

    PlaylistDO getPlaylist(String playlistUuid);

    PlaylistDO getByTitle(String title);

    List<PlaylistDO> selectBatchIds(Set<String> pplUuids);

    PlaylistDO getByIdIgnoreOrgId(String pplUuid);
}
