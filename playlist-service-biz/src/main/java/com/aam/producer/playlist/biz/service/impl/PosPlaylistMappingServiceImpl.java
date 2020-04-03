package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.protocol.response.PosInfo;
import com.aam.producer.playlist.repository.dao.PosPlaylistMappingMapper;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

import java.util.*;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-09-17
 */
@Service
public class PosPlaylistMappingServiceImpl extends
        ServiceImpl<PosPlaylistMappingMapper, PosPlaylistMappingDO> implements
        IPosPlaylistMappingService {

    @Value("${thunderstorm.common.posProtectOffset}")
    private long posProtectOffset = 30 * 60 * 1000;

    @Override
    public PosPlaylistMappingDO getOneMapping(String posUuid) {
        if (StringUtils.isEmpty(posUuid)) {
            return null;
        }
        return this.getOne(new QueryWrapper<PosPlaylistMappingDO>().lambda()
                .eq(PosPlaylistMappingDO::getPosUuid, posUuid));
    }

    @Override
    public List<PosPlaylistMappingDO> getMappingsByPos(boolean withoutProtected,
            List<String> inPosIds) {
        if (CollectionUtils.isEmpty(inPosIds)) {
            throw new RuntimeException("illegal param，inPosIds must be not empty.");
        }

        QueryWrapper<PosPlaylistMappingDO> wrapper = buildQueryWrapper(withoutProtected);

        wrapper.lambda().in(PosPlaylistMappingDO::getPosUuid, inPosIds)
                .orderByAsc(PosPlaylistMappingDO::getId);

        return this.list(wrapper);
    }

    @Override
    public List<PosPlaylistMappingDO> getMappingsByAutoPpl(String orgId, boolean withoutProtected,
            boolean withoutNoTitleMapped, List<Long> showAttributeCodes, String pplUuid) {
        if (StringUtils.isEmpty(orgId) || CollectionUtils.isEmpty(showAttributeCodes) || StringUtils
                .isEmpty(pplUuid)) {
            throw new RuntimeException(
                    "illegal param，orgId,showAttributeCodes and pplUuid must be not empty.");
        }

        QueryWrapper<PosPlaylistMappingDO> wrapper = buildQueryWrapper(withoutProtected);

        if (withoutNoTitleMapped) {
            wrapper.lambda().isNotNull(PosPlaylistMappingDO::getTitleUuid);
        }
        wrapper.lambda().eq(PosPlaylistMappingDO::getOrganizationId, orgId)
                .in(PosPlaylistMappingDO::getShowAttributesCode, showAttributeCodes);

        wrapper.and(x -> x.eq("ppl_uuid", pplUuid).or().eq("ppl_uuid", StringUtils.EMPTY));

        wrapper.lambda().orderByAsc(PosPlaylistMappingDO::getId);

        return this.list(wrapper);
    }

    @Override
    public List<PosPlaylistMappingDO> getMappingsByPpl(boolean withoutProtected, String pplUuid) {
        if (StringUtils.isEmpty(pplUuid)) {
            throw new RuntimeException("illegal param，pplUuid must be not empty.");
        }

        QueryWrapper<PosPlaylistMappingDO> wrapper = buildQueryWrapper(withoutProtected);

        wrapper.lambda().eq(PosPlaylistMappingDO::getPplUuid, pplUuid)
                .orderByAsc(PosPlaylistMappingDO::getId);

        return this.list(wrapper);
    }

    @Override
    public List<PosPlaylistMappingDO> getMappingsByPplFilterTitle(boolean withoutProtected,
            String pplUuid, String titleId) {
        if (StringUtils.isEmpty(pplUuid)) {
            throw new RuntimeException("illegal param，pplUuid must be not empty.");
        }

        QueryWrapper<PosPlaylistMappingDO> wrapper = buildQueryWrapper(withoutProtected);

        wrapper.lambda().eq(PosPlaylistMappingDO::getPplUuid, pplUuid)
                .eq(PosPlaylistMappingDO::getTitleUuid, titleId)
                .orderByAsc(PosPlaylistMappingDO::getId);

        return this.list(wrapper);
    }

    @Override
    public List<PosPlaylistMappingDO> getMappingByTpl(String pplUuid, String tplUuid,
            String complexUuid) {
        if (StringUtils.isEmpty(tplUuid)) {
            throw new RuntimeException("illegal param，tplUuid must be not empty.");
        }

        QueryWrapper<PosPlaylistMappingDO> wrapper = buildQueryWrapper(false);
        if (StringUtils.isNotEmpty(pplUuid)) {
            wrapper.lambda().eq(PosPlaylistMappingDO::getPplUuid, pplUuid);
        }
        wrapper.lambda().eq(PosPlaylistMappingDO::getTplUuid, tplUuid);

        if (StringUtils.isNotEmpty(complexUuid)) {
            wrapper.lambda().eq(PosPlaylistMappingDO::getComplexUuid, complexUuid);
        }

        wrapper.lambda().orderByAsc(PosPlaylistMappingDO::getId);

        return this.list(wrapper);
    }

    @Override
    public List<PosPlaylistMappingDO> getMappingByTitle(boolean withoutProtected, String titleId) {
        if (StringUtils.isEmpty(titleId)) {
            throw new RuntimeException("illegal param，titleId must be not empty.");
        }

        QueryWrapper<PosPlaylistMappingDO> wrapper = buildQueryWrapper(withoutProtected);

        wrapper.lambda().eq(PosPlaylistMappingDO::getTitleUuid, titleId)
                .orderByAsc(PosPlaylistMappingDO::getId);

        return this.list(wrapper);
    }

    @Override
    public List<PosPlaylistMappingDO> getProtectedMappings(String pplUuid) {
        if (StringUtils.isEmpty(pplUuid)) {
            throw new RuntimeException("illegal param，pplUuid must be not empty.");
        }

        long now = System.currentTimeMillis();
        return this.list(new QueryWrapper<PosPlaylistMappingDO>().lambda()
                .between(PosPlaylistMappingDO::getPosStart, now, now + posProtectOffset)
                .eq(PosPlaylistMappingDO::getPplUuid, pplUuid)
                .eq(PosPlaylistMappingDO::getDeleted, false)
                .orderByAsc(PosPlaylistMappingDO::getId));
    }

    @Override
    public List<PosPlaylistMappingDO> getMappingsByShowAttribute(String orgId,
            boolean withoutProtected, String titleId, List<Long> showAttributeCodes) {
        if (StringUtils.isEmpty(orgId) || CollectionUtils.isEmpty(showAttributeCodes)) {
            throw new RuntimeException(
                    "illegal param，orgId and showAttributeCodes must be not empty.");
        }

        QueryWrapper<PosPlaylistMappingDO> wrapper = buildQueryWrapper(withoutProtected);

        wrapper.lambda().eq(PosPlaylistMappingDO::getOrganizationId, orgId)
                .in(PosPlaylistMappingDO::getShowAttributesCode, showAttributeCodes);

        if (StringUtils.isNotEmpty(titleId)) {
            wrapper.lambda().eq(PosPlaylistMappingDO::getTitleUuid, titleId);
        }

        wrapper.lambda().orderByAsc(PosPlaylistMappingDO::getId);

        return this.list(wrapper);
    }

    @Override
    public int updateMappingSendInfo(List<String> posUUIDs, String reStatus, String reMessage,
            String tplUuid, boolean unMapped) {
        Set<Integer> idsWithOrder = this.baseMapper.getIdsWithOrder(posUUIDs, tplUuid);
        if (CollectionUtils.isNotEmpty(idsWithOrder)) {
            return this.baseMapper
                    .updateMappingSendInfo(idsWithOrder, reStatus, reMessage, tplUuid, unMapped);
        } else {
            return 0;
        }
    }

    @Override
    public int correctPosOrgId(String orgId, String complexId) {
        return this.baseMapper.correctPosOrgId(orgId, complexId);
    }

    @Override
    public void updateBatchByOrderIds(List<PosPlaylistMappingDO> mappings) {
        List<PosPlaylistMappingDO> orderList = mappings.stream()
                .sorted(Comparator.comparingInt(PosPlaylistMappingDO::getId))
                .collect(Collectors.toList());
        this.updateBatchById(orderList);
    }

    private QueryWrapper<PosPlaylistMappingDO> buildQueryWrapper(boolean withoutProtected) {
        QueryWrapper<PosPlaylistMappingDO> wrapper = new QueryWrapper<>();
        long now = System.currentTimeMillis();
        if (withoutProtected) {
            wrapper.lambda().gt(PosPlaylistMappingDO::getPosStart, now + posProtectOffset);
        } else {
            wrapper.lambda().ge(PosPlaylistMappingDO::getPosStart, now);
        }
        wrapper.lambda().eq(PosPlaylistMappingDO::getDeleted, false);
        return wrapper;
    }

    @Override
    public Map<String, Long> getPosCount(List<String> playlistUuids) {
        Map<String, Long> map = new HashMap<>();
        playlistUuids.forEach(s -> map.put(s, 0L));
        List<Map<String, Object>> mapList = baseMapper.getPosCount(playlistUuids);
        mapList.forEach(stringObjectMap -> map.put(stringObjectMap.get("key").toString(),
                (Long) stringObjectMap.get("value")));
        return map;
    }

    @Override
    public List<PosInfo> getPosByTitle(String titleId) {
        List<PosInfo> list = new ArrayList<>();
        List<Map<String, String>> mapList = baseMapper.getPosByTitle(titleId);
        mapList.forEach(stringObjectMap -> {
            PosInfo posInfo = new PosInfo();
            posInfo.setPplUuid(stringObjectMap.get("ppl_uuid"));
            posInfo.setPosUuid(stringObjectMap.get("pos_uuid"));
            list.add(posInfo);
        });
        return list;
    }
}
