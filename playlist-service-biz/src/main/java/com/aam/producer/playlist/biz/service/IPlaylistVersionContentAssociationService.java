package com.aam.producer.playlist.biz.service;

import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
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
public interface IPlaylistVersionContentAssociationService extends
        IService<PlaylistVersionContentAssociationDO> {

    void deleteAssociation(String versionUuid);

    void deleteAssociation(String versionUuid, String contentUuid);

    String insertAssociation(String playlistUuid, String pplVersionId, ContentDTO contentDTO,
            Integer sortNumber);

    Integer maxSortNumber(String playlistVersionUuid);

    boolean hasAutomaticSegment(String playlistVersionUuid);

    List<PlaylistVersionContentAssociationDO> getAssociations(String versionUuid);

    List<PlaylistVersionContentAssociationDO> getAssociations(List<String> versionUuids);

    boolean contains(String segmentUuid);

    PlaylistVersionContentAssociationDO getAssociationsByUuid(String associationUuid,
            String versionUuid);

    List<PlaylistVersionContentAssociationDO> listAssociationsByUuid(String associationUuid);

    List<PlaylistVersionContentAssociationDO> getAssociationsByContentUuid(String cplUUID);

    List<PlaylistVersionContentAssociationDO> getAssociationsByContentUuids(List<String> contentUuids);

}
