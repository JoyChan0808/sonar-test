package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.protocol.request.SegmentDTO;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-28
 */
public interface ISegmentService extends IService<SegmentDO> {

    SegmentDO getAutomaticSegment();

    String createSegment(SegmentDTO segmentDTO, String segmentUuid);

    void updateSegment(String segmentUuid, String title, Integer purpose,Boolean isSplitWeek);

    void deleteSegment(String segmentUuid);

    List<SegmentDO> getSegmentByTitle(String title);

    List<SegmentDO> searchSegment(List<Integer> types, String title);

    List<String> getApiSegmentNames();

    SegmentDO getSegmentDO(String uuid);

    SegmentDO getRatingSegment();

    List<SegmentDO> getAllSplitWeekSegment();

    List<SegmentDO> getAllSegment();
}
