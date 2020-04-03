package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.biz.enums.SegmentSplitTypeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.protocol.response.SegmentSplitInfo;
import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.protocol.response.SplitComplexInfo1;
import com.aam.producer.playlist.protocol.response.TitleInfo;
import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.aam.producer.playlist.repository.entity.SegmentSplitDO;
import java.util.List;

public interface ISegmentSplitViewService {

    List<SegmentSplitInfo> getByContentAssociationUuid(String contentAssociationUuid,
            String titleUuid);

    List<SegmentSplitInfo> getDraftByContentAssociationUuid(
            String contentAssociationUuid,
            String titleUuid);

    List<SplitComplexInfo1> getSplitComplex(String uuid, List<String> complexGroupIds);

    List<SegmentSplitInfo> getSegmentSplitTree(String associationUuid, String titleUuid,
            Integer status);

    default SegmentSplitInfo toSegmentSplitInfo(SegmentSplitDO segmentSplitDO,
            String contentAssociationUuid, String playlistUuid, String pplVersionUuid,
            String titleUuid,
            List<ContentInfo> contentList) {
        SegmentSplitInfo segmentSplitInfo = new SegmentSplitInfo();
        segmentSplitInfo.setCreated(segmentSplitDO.getCreated());
        segmentSplitInfo.setLastModified(segmentSplitDO.getLastModified());
        segmentSplitInfo.setParentUuid(segmentSplitDO.getParentUuid());
        segmentSplitInfo.setPublishTime(segmentSplitDO.getPublishTime());
        segmentSplitInfo.setSplitRule(segmentSplitDO.getSplitRule());
        segmentSplitInfo.setUuid(segmentSplitDO.getUuid());
        segmentSplitInfo.setSplitTitle(segmentSplitDO.getSplitTitle());
        segmentSplitInfo.setSplitType(
                SegmentSplitTypeEnum.getSplitTypeByCode(segmentSplitDO.getSplitType()).getName());
        segmentSplitInfo
                .setStatus(SegmentStatusEnum.getEmunStrByStatus(segmentSplitDO.getStatus()));
        segmentSplitInfo
                .setContentList(contentList);
        segmentSplitInfo.setSortNum(segmentSplitDO.getSortNum());
        segmentSplitInfo.setSegmentAssociationUuid(contentAssociationUuid);
        segmentSplitInfo.setPlaylistUuid(playlistUuid);
        segmentSplitInfo.setPplVersionUuid(pplVersionUuid);
        segmentSplitInfo.setTitleUuid(titleUuid);
        segmentSplitInfo.setSign(segmentSplitDO.getSign());
        segmentSplitInfo.setDefaultGroup(segmentSplitDO.getDefaultGroup());
        segmentSplitInfo.setAutoGroup(segmentSplitDO.getAutoGroup());
        segmentSplitInfo.setUserGroup(segmentSplitDO.getUserGroup());
        return segmentSplitInfo;
    }

    SegmentStatusEnum getStatus(String titleUuid, String associationUuid);

    SegmentStatusEnum getStatus(List<PlaylistSegmentSplitAssociationDO> associationDOS, List<SegmentSplitDO> segmentSplitDOS);

    List<String> getShowsBySegmentSplitUuid(String segmentSplitUuid);

    List<ShowAttributeGroupInfo> getShowType(String uuid);

    List<SegmentSplitInfo> getSegmentSplitList(String associationUuid, String titleUuid,
            Integer status);

    TitleInfo getTitleInfo(String contentAssociationUuid, String titleUuid);


}
