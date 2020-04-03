package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.PlaylistDTO;

public interface IPlaylistActionService {

    /**
     * 创建ppl,创建ppl version,为版本添加自动占位符
     *
     * @param playlistDTO PlaylistDTO
     * @return UUID
     */
    String createPlaylist(PlaylistDTO playlistDTO);

    /**
     * 修改ppl title
     *
     * @param playUuid playUuid
     * @param title title
     */
    void updatePlaylist(String playUuid, String title);

    /**
     * 删除ppl
     *
     * @param playlistUuid UUID
     */
    void deletePlaylist(String playlistUuid);

    /**
     * 拷贝ppl
     *
     * @param playlistUuid playlistUuid
     * @param playlistDTO PlaylistDTO
     * @return UUID
     */
    String copyPlaylist(String playlistUuid, PlaylistDTO playlistDTO);

    /**
     * All segment publish.
     *
     * @param playlistUuid playlistUuid
     */
    void allSegmentPublish(String playlistUuid);
}
