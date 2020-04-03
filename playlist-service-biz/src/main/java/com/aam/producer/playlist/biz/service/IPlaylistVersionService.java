package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.repository.entity.PlaylistVersionDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-04
 */
public interface IPlaylistVersionService extends IService<PlaylistVersionDO> {

    String createPlaylistVersion(String playlistUuid);

    void updatePlaylistVersion(String versionUuid, int status);

    void deletePlaylistVersion(String versionUuid);

    List<PlaylistVersionDO> getPlaylistVersionByPplUuid(String playlistUuid);

    void updatePublishTime(String uuid, Long time, Boolean publishLater);

    void deletePlaylistVersion(String playlistUuid, Integer status);

    PlaylistVersionDO getReleasePlaylistVersionByPplUuid(String playlistUuid);

    PlaylistVersionDO getDraftPlaylistVersionByPplUuid(String playlistUuid);

    List<PlaylistVersionDO> getByIds(List<String> playlistVersionUuids);

}