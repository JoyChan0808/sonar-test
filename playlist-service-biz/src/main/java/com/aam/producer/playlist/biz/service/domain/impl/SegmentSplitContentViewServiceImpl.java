package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.service.IPlaylistSegmentSplitAssociationService;
import com.aam.producer.playlist.biz.service.ISegmentSplitContentAssociationService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitContentViewService;
import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.aam.producer.playlist.repository.entity.SegmentSplitContentAssociationDO;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SegmentSplitContentViewServiceImpl implements ISegmentSplitContentViewService {

    private final ISegmentSplitContentAssociationService contentAssociationService;

    private final IPlaylistSegmentSplitAssociationService segmentSplitAssociationService;

    @Autowired
    public SegmentSplitContentViewServiceImpl(
            ISegmentSplitContentAssociationService contentAssociationService,
            IPlaylistSegmentSplitAssociationService segmentSplitAssociationService) {
        this.contentAssociationService = contentAssociationService;
        this.segmentSplitAssociationService = segmentSplitAssociationService;
    }

    @Override
    public List<ContentDTO> listContentDTO(String segmentSplitUuid) {
        List<SegmentSplitContentAssociationDO> associationDOS = contentAssociationService
                .getContentList(segmentSplitUuid);
        PlaylistSegmentSplitAssociationDO splitAssociationDO = segmentSplitAssociationService
                .getSegmentSplitBySegmentSplitUuid(segmentSplitUuid);
        return associationDOS.stream().map(
                splitContentAssociationDO -> toContentDTO(splitContentAssociationDO,
                        splitAssociationDO.getSegmentAssociationUuid()))
                .collect(Collectors.toList());
    }

    @Override
    public List<ContentInfo> listContentInfo(String segmentSplitUuid) {
        List<SegmentSplitContentAssociationDO> associationDOS = contentAssociationService
                .getContentList(segmentSplitUuid);
        return associationDOS.stream().map(
                this::toContentInfo).collect(Collectors.toList());
    }

}
