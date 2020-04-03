package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;
import java.util.Set;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author ${author}
 * @since 2019-05-09
 */
public interface IPlaylistSegmentSplitAssociationService extends
        IService<PlaylistSegmentSplitAssociationDO> {

    void createAssociation(String pplUuid, String pplVersionUuid, String segmentAssociationUuid,
            String titleUuid, String segmentSplitUuid);

    void deleteAssociation(String segmentSplitUuid);

    void batchDeleteAssociation(List<String> uuids);

    List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByPlaylistUuid(String playlistUuid);

    List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByPlaylistVersionUuid(
            String versionUuid);

    List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByContentAssociationUuid(
            String contentAssociationUuid);

    List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByContentAssociationUuids(
            Set<String> contentAssociationUuid);

    List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByContentAssociationUuid(
            String contentAssociationUuid, String titleUuid);

    List<PlaylistSegmentSplitAssociationDO> getSegmentSplitByTitle(String titleUuid);

    List<PlaylistSegmentSplitAssociationDO> getAllSegmentSplitByTitle(String titleUuid);

    PlaylistSegmentSplitAssociationDO getSegmentSplitBySegmentSplitUuid(String segmentSplitUuid);

    List<PlaylistSegmentSplitAssociationDO> getSegmentSplitBySegmentSplitUuids(
            Set<String> segmentSplitUuids);
}
