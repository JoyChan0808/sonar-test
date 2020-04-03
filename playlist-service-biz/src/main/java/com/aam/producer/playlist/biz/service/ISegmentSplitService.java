package com.aam.producer.playlist.biz.service;


import com.aam.producer.playlist.protocol.request.SegmentSplitDTO;
import com.aam.producer.playlist.repository.entity.SegmentSplitDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-04-04
 */
public interface ISegmentSplitService extends IService<SegmentSplitDO> {

    String createSegmentSplit(SegmentSplitDTO segmentSplitDTO);

    void updateSegmentSplit(String uuid, SegmentSplitDTO segmentSplitDTO);

    SegmentSplitDO getBySign(String status, String associationUuid, String titleUuid, String rule,
            String parentUuid, String title);

    List<SegmentSplitDO> listByParentUuid(String parentUuid);

    List<SegmentSplitDO> listSegmentSplitTreeByNodeUuid(String nodeUuid);

    List<String> deleteSegmentSplitTreeByNodeUuid(String nodeUuid);

    List<String> deleteSegmentSplitTreeByParentNodeUuid(String pidNodeUuid);

    List<SegmentSplitDO> getByStatus(List<String> uuids, Integer status);

    List<SegmentSplitDO> getByIds(List<String> segmentSplitUuid);

    String buildSign(String status, String associationUuid, String titleUuid, String rule,
            String parentUuid, String title);

}
