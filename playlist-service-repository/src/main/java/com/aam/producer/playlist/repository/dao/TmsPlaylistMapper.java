package com.aam.producer.playlist.repository.dao;

import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.List;
import java.util.Set;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author ${author}
 * @since 2019-04-29
 */
public interface TmsPlaylistMapper extends BaseMapper<TmsPlaylistDO> {

    List<TmsPlaylistDO> getTplListByCplUuid(String cplUUID);

    int correctTplOrgId(@Param("orgId") String orgId,
            @Param("complexId") String complexId);

    List<TmsPlaylistDO> getTplListForUpdate(@Param("pplUuid") String pplUuid,
            @Param("tplUUIDs") Set<String> tplUUIDs);

    List<TmsPlaylistDO> getTplListScopeInPro(@Param("source") Integer source,
            @Param("pplId") String pplId,
            @Param("pplVersionId") String pplVersionId);
}
