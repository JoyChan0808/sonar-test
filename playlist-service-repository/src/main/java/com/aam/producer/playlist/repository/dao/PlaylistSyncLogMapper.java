package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.PlaylistSyncLogDO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.List;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author ${author}
 * @since 2019-04-29
 */
public interface PlaylistSyncLogMapper extends BaseMapper<PlaylistSyncLogDO> {

    long[] getLatestIds(@Param("complexId") String complexId,
            @Param("playlistUuids") List<String> playlistUuids);

    List<PlaylistSyncLogDO> getLatestPlaylistSyncLogs(@Param("latestIds") long... latestIds);
}
