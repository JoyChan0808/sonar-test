package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.repository.entity.SegmentSplitContentAssociationDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-05-09
 */
public interface ISegmentSplitContentAssociationService extends
        IService<SegmentSplitContentAssociationDO> {

    void insertAssociation(String segmentSplitUuid, ContentDTO contentDTO, Integer sortNumber);

    void batchInsertAssociation(String segmentSplitUuid, List<ContentDTO> contentDTOList);

    void deleteAssociation(String segmentSplitUuid);

    void batchDeleteAssociation(List<String> segmentSplitUuids);

    List<SegmentSplitContentAssociationDO> getContentList(String segmentSplitUuid);

    List<SegmentSplitContentAssociationDO> getSplitAssociationsByContentUuid(String cplUUID);
}
