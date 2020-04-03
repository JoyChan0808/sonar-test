package com.aam.producer.playlist.biz.service.impl;


import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.service.ISegmentService;
import com.aam.producer.playlist.protocol.request.SegmentDTO;
import com.aam.producer.playlist.repository.dao.SegmentMapper;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-28
 */
@Service
public class SegmentServiceImpl extends ServiceImpl<SegmentMapper, SegmentDO> implements
        ISegmentService {

    @Override
    public SegmentDO getAutomaticSegment() {
        SegmentDO segmentDO = baseMapper
                .getAutomaticSegment(SegmentTypeEnum.AUTOMATIC_SEGMENT.getCode());
        if (segmentDO == null) {
            segmentDO = new SegmentDO();
            segmentDO.setUuid("5546e68d-6991-11e9-831c-0242ac140002");
            segmentDO.setTitle("Automatic Feature Selector");
            segmentDO.setType(SegmentTypeEnum.AUTOMATIC_SEGMENT.getCode());
        }
        return segmentDO;
    }

    @Override
    @Transactional
    public String createSegment(SegmentDTO segmentDTO, String segmentUuid) {
        SegmentDO segmentDO = new SegmentDO();
        segmentDO.setTitle(segmentDTO.getTitle());
        segmentDO.setType(segmentDTO.getType());
        segmentDO.setPurpose(segmentDTO.getPurpose());
        segmentDO.setUuid(segmentUuid);
        segmentDO.setSplitByWeek(segmentDTO.getSplitByWeek());
        baseMapper.insert(segmentDO);
        return segmentDO.getUuid();
    }


    @Override
    @Transactional
    public void updateSegment(String segmentUuid, String title, Integer purpose,
            Boolean isSplitWeek) {
        SegmentDO segmentDO = baseMapper.selectById(segmentUuid);
        segmentDO.setTitle(title);
        segmentDO.setPurpose(purpose);
        segmentDO.setSplitByWeek(isSplitWeek);
        baseMapper.updateById(segmentDO);
    }

    @Override
    @Transactional
    public void deleteSegment(String segmentUuid) {
        baseMapper.deleteById(segmentUuid);
    }

    @Override
    public List<SegmentDO> getSegmentByTitle(String title) {
        QueryWrapper<SegmentDO> wrapper = new QueryWrapper<>();
        wrapper.eq("title", title);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<SegmentDO> searchSegment(List<Integer> types, String title) {
        QueryWrapper<SegmentDO> wrapper = new QueryWrapper<>();
        if (CollectionUtils.isNotEmpty(types)) {
            wrapper.in("type", types);
        }
        if (StringUtils.isNotEmpty(title)) {
            wrapper.like("title", title);
        }
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<String> getApiSegmentNames() {
        return baseMapper.getApiSegmentNames(SegmentTypeEnum.API_SEGMENT.getCode());
    }

    @Override
    public SegmentDO getSegmentDO(String uuid) {
        return baseMapper.selectById(uuid);
    }

    @Override
    public SegmentDO getRatingSegment() {
        QueryWrapper<SegmentDO> wrapper = new QueryWrapper<>();
        wrapper.eq("type", SegmentTypeEnum.RATING_SEGMENT.getCode());
        return baseMapper.selectOne(wrapper);
    }

    @Override
    public List<SegmentDO> getAllSplitWeekSegment() {
        return baseMapper.getAllSplitWeekSegment();
    }

    @Override
    public List<SegmentDO> getAllSegment() {
        return baseMapper.getAllSegment();
    }

}
