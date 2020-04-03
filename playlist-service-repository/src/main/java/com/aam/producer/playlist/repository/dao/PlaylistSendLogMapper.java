package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.PlaylistSendLogDO;
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
public interface PlaylistSendLogMapper extends BaseMapper<PlaylistSendLogDO> {

    long[] getLatestIds(@Param("complexId") String complexId,
            @Param("playlistUuid") String playlistUuid);

    List<PlaylistSendLogDO> getLatestPlaylistSendLogs(@Param("latestIds") long... latestIds);

    List<PlaylistSendLogDO> getGroupByTplAndComplex(@Param("tplUUIDs") List<String> tplUUIDs);
}
