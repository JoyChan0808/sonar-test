package com.aam.producer.playlist.biz.convert;

import com.aam.producer.playlist.protocol.request.TPlaylistSyncedDTO.Playlist;
import com.aam.producer.playlist.repository.entity.PlaylistSyncLogDO;
import com.alibaba.fastjson.JSON;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

/**
 * TPlaylistSyncConvert
 *
 * @author oliver.lo
 * @since 2019/4/10 8:11 PM
 */
@Mapper
public interface TPlaylistSyncConvert {

    TPlaylistSyncConvert MAPPER = Mappers.getMapper(TPlaylistSyncConvert.class);

    default PlaylistSyncLogDO toPlaylistSyncLogDO(String complexId, String deviceId,
            String playlistUuid, String hash, Integer method, Integer status, String message) {
        PlaylistSyncLogDO playlistSyncLogDO = new PlaylistSyncLogDO();
        playlistSyncLogDO.setComplexId(complexId);
        playlistSyncLogDO.setDeviceUuid(deviceId);
        playlistSyncLogDO.setPlaylistUuid(playlistUuid);
        playlistSyncLogDO.setHash(hash);
        playlistSyncLogDO.setMethod(method);
        playlistSyncLogDO.setStatus(status);
        playlistSyncLogDO.setMessage(message);
        return playlistSyncLogDO;
    }

    default void playlistSyncLogDOFilling(PlaylistSyncLogDO validPlaylistSync,
            Playlist playlist, Integer status, String message) {
        if (playlist != null) {
            validPlaylistSync.setTitle(playlist.getTitle());
            validPlaylistSync.setJson(JSON.toJSONString(playlist.getJson()));
            validPlaylistSync.setContentIds(JSON.toJSONString(playlist.getContentIds()));
            validPlaylistSync.setTemplated(playlist.getTemplated());
            validPlaylistSync.setAs3d(playlist.getAs3d());
            validPlaylistSync.setAs4k(playlist.getAs4k());
            validPlaylistSync.setAsHfr(playlist.getAsHfr());
        }
        if (status != null) {
            validPlaylistSync.setStatus(status);
        }
        if (message != null) {
            validPlaylistSync.setMessage(message);
        }
    }

    default PlaylistSyncLogDO toUniqueLog(String playlistUuid, String complexId) {
        PlaylistSyncLogDO logDO = new PlaylistSyncLogDO();
        logDO.setPlaylistUuid(playlistUuid);
        logDO.setComplexId(complexId);
        return logDO;
    }
}
