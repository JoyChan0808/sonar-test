package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.enums.SegmentSplitTypeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.biz.service.ISegmentSplitService;
import com.aam.producer.playlist.protocol.request.SegmentSplitDTO;
import com.aam.producer.playlist.repository.dao.SegmentSplitMapper;
import com.aam.producer.playlist.repository.entity.SegmentSplitDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
public class SegmentSplitServiceImpl extends
        ServiceImpl<SegmentSplitMapper, SegmentSplitDO> implements ISegmentSplitService {

    private static final Logger logger = LoggerFactory.getLogger(SegmentSplitServiceImpl.class);

    @Override
    public String createSegmentSplit(SegmentSplitDTO segmentSplitDTO) {
        SegmentSplitDO segmentSplitDO = new SegmentSplitDO();
        segmentSplitDO.setSplitRule(segmentSplitDTO.getSplitRule());
        segmentSplitDO.setSplitTitle(segmentSplitDTO.getSplitTitle());
        segmentSplitDO.setValidTime(segmentSplitDTO.getValidTime());
        segmentSplitDO.setParentUuid(segmentSplitDTO.getParentUuid());
        segmentSplitDO.setAutoGroup(segmentSplitDTO.getAutoGroup());
        segmentSplitDO.setDefaultGroup(segmentSplitDTO.getDefaultGroup());
        segmentSplitDO.setSortNum(System.currentTimeMillis());
        segmentSplitDO.setPublishTime(segmentSplitDTO.getPublishTime());
        segmentSplitDO
                .setStatus(SegmentStatusEnum.getEmunByStatusStr(segmentSplitDTO.getStatus())
                        .getStatus());
        segmentSplitDO.setSplitType(
                SegmentSplitTypeEnum.getSplitTypeByName(segmentSplitDTO.getSplitType()).getCode());
        segmentSplitDO.setSign(
                buildSign(segmentSplitDTO.getStatus(), segmentSplitDTO.getSegmentAssociationUuid(),
                        segmentSplitDTO.getTitleUuid(), segmentSplitDTO.getSplitRule(),
                        segmentSplitDTO.getParentUuid(), segmentSplitDTO.getSplitTitle()));
        segmentSplitDO.setUuid(UUID.randomUUID().toString());
        segmentSplitDO.setUserGroup(segmentSplitDTO.getUserGroup());
        baseMapper.insert(segmentSplitDO);

        return segmentSplitDO.getUuid();
    }

    @Override
    public String buildSign(String status, String associationUuid, String titleUuid, String rule,
            String parentUuid, String title) {
        String parentSignUuid;
        if (StringUtils.isNotBlank(parentUuid)) {
            SegmentSplitDO segmentSplitDO = getById(parentUuid);
            parentSignUuid = segmentSplitDO.getSign();
        } else {
            parentSignUuid = null;
        }
        return UUID.nameUUIDFromBytes(
                (status + associationUuid + titleUuid + rule + parentSignUuid + title)
                        .getBytes(StandardCharsets.UTF_8))
                .toString();
    }

    @Override
    public void updateSegmentSplit(String uuid, SegmentSplitDTO segmentSplitDTO) {
        SegmentSplitDO segmentSplitDO = baseMapper.selectById(uuid);
        segmentSplitDO.setUserGroup(segmentSplitDTO.getUserGroup());
        segmentSplitDO.setSplitTitle(segmentSplitDTO.getSplitTitle());
        segmentSplitDO.setSplitRule(segmentSplitDTO.getSplitRule());
        segmentSplitDO
                .setSign(buildSign(SegmentStatusEnum.getEmunStrByStatus(segmentSplitDO.getStatus()),
                        segmentSplitDTO.getSegmentAssociationUuid(), segmentSplitDTO.getTitleUuid(),
                        segmentSplitDO.getSplitRule(), segmentSplitDO.getParentUuid(),
                        segmentSplitDO.getSplitTitle()));
        baseMapper.updateById(segmentSplitDO);
    }

    @Override
    public SegmentSplitDO
    getBySign(String status, String associationUuid, String titleUuid,
            String rule, String parentUuid, String title) {
        String sign = buildSign(status, associationUuid, titleUuid, rule, parentUuid, title);
        QueryWrapper<SegmentSplitDO> wrapper = new QueryWrapper<>();
        wrapper.eq("sign", sign);
        return baseMapper.selectOne(wrapper);
    }

    @Override
    public List<SegmentSplitDO> listByParentUuid(String parentUuid) {
        QueryWrapper<SegmentSplitDO> wrapper = new QueryWrapper<>();
        wrapper.eq("parent_uuid", parentUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<SegmentSplitDO> listSegmentSplitTreeByNodeUuid(String nodeUuid) {
        List<SegmentSplitDO> result = new ArrayList<>();

        Stack<String> stack = new Stack<>();
        stack.push(nodeUuid);

        while (!stack.empty()) {
            String uuid = stack.pop();
            SegmentSplitDO segmentSplitDO = baseMapper.selectById(uuid);
            result.add(segmentSplitDO);
            List<SegmentSplitDO> segmentSplitDOS = listByParentUuid(segmentSplitDO.getUuid());
            segmentSplitDOS.forEach(segmentSplitDO1 -> stack.push(segmentSplitDO1.getUuid()));
        }

        return result;
    }

    @Override
    public List<String> deleteSegmentSplitTreeByNodeUuid(String nodeUuid) {
        List<SegmentSplitDO> segmentSplitDOS = listSegmentSplitTreeByNodeUuid(nodeUuid);
        List<String> uuids = segmentSplitDOS.stream().map(SegmentSplitDO::getUuid)
                .collect(Collectors.toList());
        baseMapper.deleteBatchIds(uuids);
        return uuids;
    }

    @Override
    public List<String> deleteSegmentSplitTreeByParentNodeUuid(String pidNodeUuid) {
        List<SegmentSplitDO> segmentSplitDOS = listSegmentSplitTreeByNodeUuid(pidNodeUuid);
        List<String> uuids = segmentSplitDOS.stream().map(SegmentSplitDO::getUuid)
                .filter(s -> !s.equals(pidNodeUuid))
                .collect(Collectors.toList());
        if (!uuids.isEmpty()) {
            baseMapper.deleteBatchIds(uuids);
        }
        return uuids;
    }

    @Override
    public List<SegmentSplitDO> getByStatus(List<String> uuids, Integer status) {
        if(CollectionUtils.isEmpty(uuids)){
            return new ArrayList<>();
        }
        QueryWrapper<SegmentSplitDO> wrapper = new QueryWrapper<>();
        wrapper.in("uuid", uuids);
        if (status != null) {
            wrapper.eq("status", status);
        }
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<SegmentSplitDO> getByIds(List<String> segmentSplitUuid) {
        if (CollectionUtils.isEmpty(segmentSplitUuid)){return new ArrayList<>(0);}
        return baseMapper.selectBatchIds(segmentSplitUuid);
    }

}
