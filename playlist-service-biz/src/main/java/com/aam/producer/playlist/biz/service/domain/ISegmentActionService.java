package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.PublishSegmentDTO;
import com.aam.producer.playlist.protocol.request.SegmentDTO;

public interface ISegmentActionService {

    String createSegment(SegmentDTO segmentDTO);

    void updateSegment(String segmentUuid, SegmentDTO segmentDTO);

    void deleteSegment(String segmentUuid);

    void createRatingSegment();

    void synchronizedSegment();

    void cancelWeek(String segmentUuid);

    void batchPublishSegment(PublishSegmentDTO publishDTO);
}
