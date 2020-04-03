package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.protocol.response.SegmentInfo;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.aam.producer.playlist.sal.response.TitleInfo;
import java.util.List;

public interface ISegmentViewService {

    SegmentInfo getAutomaticSegment();

    default SegmentInfo toSegmentInfo(SegmentDO segmentDO) {
        if (segmentDO == null) {
            return null;
        }
        SegmentInfo segmentInfo = new SegmentInfo();
        segmentInfo.setCreated(segmentDO.getCreated());
        segmentInfo.setLastModified(segmentDO.getLastModified());
        segmentInfo.setTitle(segmentDO.getTitle());
        segmentInfo.setPurpose(segmentDO.getPurpose());
        segmentInfo.setContentKind(SegmentTypeEnum.getByCode(segmentDO.getType()).getName());
        segmentInfo.setUuid(segmentDO.getUuid());
        segmentInfo.setType(ContentTypeEnum.SEGMENT.getName());
        segmentInfo.setSplitByWeek(segmentDO.getSplitByWeek());
        segmentInfo.setOrganizationId(segmentDO.getOrganizationId());
        return segmentInfo;
    }

    SegmentInfo getSegment(String uuid);

    List<SegmentInfo> getAllSegment(String search);

    @Deprecated
    List<String> getApiSegmentNames();

    List<SegmentInfo> getAllSplitWeekSegment();

    List<TitleInfo> getAllSegmentDetail(String search);
}
