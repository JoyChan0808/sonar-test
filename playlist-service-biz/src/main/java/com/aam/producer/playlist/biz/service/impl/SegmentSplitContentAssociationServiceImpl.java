package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.playlist.biz.service.ISegmentSplitContentAssociationService;
import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.repository.dao.SegmentSplitContentAssociationMapper;
import com.aam.producer.playlist.repository.entity.SegmentSplitContentAssociationDO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author ${author}
 * @since 2019-05-09
 */
@Service
public class SegmentSplitContentAssociationServiceImpl extends
        ServiceImpl<SegmentSplitContentAssociationMapper, SegmentSplitContentAssociationDO> implements
        ISegmentSplitContentAssociationService {


    @Override
    public void insertAssociation(String segmentSplitUuid, ContentDTO contentDTO,
            Integer sortNumber) {
        SegmentSplitContentAssociationDO segmentSplitContentAssociationDO = new SegmentSplitContentAssociationDO();
        segmentSplitContentAssociationDO.setContentId(contentDTO.getContentId());
        segmentSplitContentAssociationDO
                .setContentType(ContentTypeEnum.getByName(contentDTO.getContentType()).getCode());
        segmentSplitContentAssociationDO.setSortNumber(sortNumber);
        segmentSplitContentAssociationDO.setExtension(contentDTO.getExtension());
        segmentSplitContentAssociationDO.setTitle(contentDTO.getTitle());
        segmentSplitContentAssociationDO.setSegmentSplitUuid(segmentSplitUuid);
        baseMapper.insert(segmentSplitContentAssociationDO);
    }

    @Override
    public void batchInsertAssociation(String segmentSplitUuid, List<ContentDTO> contentDTOList) {
        for (int i = 0; i < contentDTOList.size(); i++) {
            insertAssociation(segmentSplitUuid, contentDTOList.get(i), i + 1);
        }
    }

    @Override
    public void deleteAssociation(String segmentSplitUuid) {
        QueryWrapper<SegmentSplitContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("segment_split_uuid", segmentSplitUuid);
        baseMapper.delete(wrapper);
    }

    @Override
    public void batchDeleteAssociation(List<String> segmentSplitUuids) {
        if(CollectionUtils.isEmpty(segmentSplitUuids)){
            return;
        }
        QueryWrapper<SegmentSplitContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.in("segment_split_uuid", segmentSplitUuids);
        baseMapper.delete(wrapper);
    }

    @Override
    public List<SegmentSplitContentAssociationDO> getContentList(String segmentSplitUuid) {
        QueryWrapper<SegmentSplitContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("segment_split_uuid", segmentSplitUuid);
        return baseMapper.selectList(wrapper);
    }

    @Override
    public List<SegmentSplitContentAssociationDO> getSplitAssociationsByContentUuid(
            String cplUUID) {
        QueryWrapper<SegmentSplitContentAssociationDO> wrapper = new QueryWrapper<>();
        wrapper.eq("content_id", cplUUID);
        return baseMapper.selectList(wrapper);
    }

}
