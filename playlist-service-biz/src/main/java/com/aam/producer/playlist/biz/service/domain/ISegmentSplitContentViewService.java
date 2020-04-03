package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.repository.entity.SegmentSplitContentAssociationDO;
import java.util.List;

public interface ISegmentSplitContentViewService {

    List<ContentDTO> listContentDTO(String segmentSplitUuid);

    List<ContentInfo> listContentInfo(String segmentSplitUuid);

    default ContentDTO toContentDTO(SegmentSplitContentAssociationDO splitContentAssociationDO,
            String contentAssociationUuid) {
        ContentDTO contentDTO = new ContentDTO();
        contentDTO.setContentId(splitContentAssociationDO.getContentId());
        contentDTO.setContentType(
                ContentTypeEnum.getByCode(splitContentAssociationDO.getContentType()).getName());
        contentDTO.setExtension(splitContentAssociationDO.getExtension());
        contentDTO.setTitle(splitContentAssociationDO.getTitle());
        contentDTO.setContentAssociationUuid(contentAssociationUuid);
        return contentDTO;
    }

    default ContentInfo toContentInfo(SegmentSplitContentAssociationDO splitContentAssociationDO) {
        ContentInfo contentInfo = new ContentInfo();
        contentInfo.setExtension(splitContentAssociationDO.getExtension());
        contentInfo.setContentType(
                ContentTypeEnum.getByCode(splitContentAssociationDO.getContentType()).getName());
        contentInfo.setContentId(splitContentAssociationDO.getContentId());
        contentInfo.setTitle(splitContentAssociationDO.getTitle());
        return contentInfo;
    }

}
