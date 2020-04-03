package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author ${author}
 * @since 2019-09-17
 */
public interface PosPlaylistMappingMapper extends BaseMapper<PosPlaylistMappingDO> {

    Set<Integer> getIdsWithOrder(@Param("posUUIDs") List<String> posUUIDs,
            @Param("tplUuid") String tplUuid);

    int updateMappingSendInfo(@Param("ids") Set<Integer> ids,
            @Param("reStatus") String reStatus,
            @Param("reMessage") String reMessage,
            @Param("tplUuid") String tplUuid,
            @Param("unMapped") boolean unMapped);

    int correctPosOrgId(@Param("orgId") String orgId,
            @Param("complexId") String complexId);

    List<Map<String, Object>> getPosCount( @Param("playlistUuids") List<String> playlistUuids);

    List<Map<String, String>> getPosByTitle( @Param("titleId") String titleId);
}
