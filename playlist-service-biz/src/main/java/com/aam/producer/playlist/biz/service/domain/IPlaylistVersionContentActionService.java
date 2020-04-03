package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.ContentDTO;
import java.util.List;

public interface IPlaylistVersionContentActionService {

    void deleteAssociation(String versionUuid);

    String getAutoSegmentUuid(String versionUuid);

    void insertAssociationBatch(String playlistUuid, String pplVersionId,
            List<ContentDTO> newContentDTOS);

    String getContentKind(String associationUuid);

    void createDefaultSplit(Boolean splitByWeek, String playlistUuid, String pplVersionId,
            String uuid);
}
