package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.enums.SegmentSplitTypeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitActionService;
import com.aam.producer.playlist.biz.service.domain.ITaskReactorService;
import com.aam.producer.playlist.protocol.message.SegmentDTO;
import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.protocol.request.SegmentSplitDTO;
import com.aam.producer.playlist.protocol.response.SegmentInfo;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionDO;
import com.aam.utils.utils.SpringContextUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import java.util.List;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class PlaylistVersionContentActionServiceImpl implements
        IPlaylistVersionContentActionService {

    private final static Logger logger = LoggerFactory
            .getLogger(PlaylistVersionContentActionServiceImpl.class);

    private final IPlaylistVersionContentAssociationService iPlaylistVersionContentAssociationService;

    private final ISegmentSplitActionService segmentSplitActionService;

    private final ITaskReactorService iTaskReactorService;

    @Autowired
    public PlaylistVersionContentActionServiceImpl(
            IPlaylistVersionContentAssociationService iPlaylistVersionContentAssociationService,
            ISegmentSplitActionService segmentSplitActionService,
            ITaskReactorService iTaskReactorService) {
        this.iPlaylistVersionContentAssociationService = iPlaylistVersionContentAssociationService;
        this.segmentSplitActionService = segmentSplitActionService;
        this.iTaskReactorService = iTaskReactorService;
    }

    @Override
    @Transactional
    public void deleteAssociation(String versionUuid) {
        logger.info("delete content by ppl version<{}>.", versionUuid);
        try {
            IPlaylistVersionService iPlaylistVersionService = SpringContextUtils
                    .getBean(IPlaylistVersionService.class);
            PlaylistVersionDO playlistVersionDO = iPlaylistVersionService.getById(versionUuid);
            if (PlaylistStatusEnum.RELEASE.getStatus().equals(playlistVersionDO.getStatus())) {
                iTaskReactorService
                        .sendSegmentSplitEvent(playlistVersionDO.getPlaylistUuid(), null, null,
                                null,
                                true, null);
            }
        } catch (Exception e) {
            //降级处理
            logger.error("send segment warning event failed.", e);
        }
        iPlaylistVersionContentAssociationService.deleteAssociation(versionUuid);
    }

    @Override
    public String getAutoSegmentUuid(String versionUuid) {
        List<PlaylistVersionContentAssociationDO> playlistVersionContentAssociationDOS = iPlaylistVersionContentAssociationService
                .getAssociations(versionUuid);
        return playlistVersionContentAssociationDOS.stream()
                .filter(playlistVersionContentAssociationDO ->
                        ContentTypeEnum.SEGMENT.getCode()
                                .equals(playlistVersionContentAssociationDO.getContentType()))
                .filter(playlistVersionContentAssociationDO -> {
                    SegmentInfo segmentInfo = JSON
                            .parseObject(playlistVersionContentAssociationDO.getExtension(),
                                    SegmentInfo.class);
                    return SegmentTypeEnum.AUTOMATIC_SEGMENT.getName()
                            .equals(segmentInfo.getContentKind());
                }).map(PlaylistVersionContentAssociationDO::getUuid).findFirst().orElse(null);
    }


    @Transactional
    @Override
    public void insertAssociationBatch(String playlistUuid, String pplVersionId,
            List<ContentDTO> newContentDTOS) {
        for (int i = 0; i < newContentDTOS.size(); i++) {
            ContentDTO contentDTO = newContentDTOS.get(i);
            String uuid = iPlaylistVersionContentAssociationService
                    .insertAssociation(playlistUuid, pplVersionId, contentDTO, i + 1);
            if (ContentTypeEnum.SEGMENT.getName().equals(contentDTO.getContentType())) {
                SegmentDTO segmentDTO = JSON
                        .parseObject(contentDTO.getExtension(), SegmentDTO.class);
                if (segmentDTO.getType().equals(SegmentTypeEnum.PLAYLIST_SEGMENT.getName())) {
                    if (StringUtils.isNotEmpty(contentDTO.getOldContentAssociationUuid())) {
                        segmentSplitActionService
                                .copy(contentDTO.getOldContentAssociationUuid(), uuid,
                                        pplVersionId);
                    } else if (StringUtils.isEmpty(contentDTO.getContentAssociationUuid())) {
                        createDefaultSplit(segmentDTO.getSplitByWeek(), playlistUuid, pplVersionId,
                                uuid);
                    }
                }
            }
        }
    }

    @Override
    @Transactional
    public void createDefaultSplit(Boolean splitByWeek, String playlistUuid, String pplVersionId,
            String uuid) {
        SegmentSplitDTO segmentSplitDTO = new SegmentSplitDTO();
        segmentSplitDTO.setPlaylistUuid(playlistUuid);
        segmentSplitDTO.setAutoGroup(true);
        segmentSplitDTO.setDefaultGroup(false);
        segmentSplitDTO.setPplVersionUuid(pplVersionId);
        segmentSplitDTO
                .setSegmentAssociationUuid(uuid);
        segmentSplitDTO.setSplitTitle("root");
        segmentSplitDTO.setSplitType(SegmentSplitTypeEnum.ROOT.getName());
        segmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
        String rootUuid = segmentSplitActionService
                .createRootSegmentSplit(segmentSplitDTO);
        if (Boolean.TRUE.equals(splitByWeek)) {
            segmentSplitActionService
                    .createSplitByWeek(rootUuid, playlistUuid, pplVersionId, null,
                            uuid);
        }
    }

    @Override
    public String getContentKind(String associationUuid) {
        List<PlaylistVersionContentAssociationDO> dos = iPlaylistVersionContentAssociationService
                .listAssociationsByUuid(associationUuid);
        if (dos.isEmpty()) {
            return null;
        }
        PlaylistVersionContentAssociationDO playlistVersionContentAssociationDO = dos.get(0);
        IPlaylistVersionService iPlaylistVersionService = SpringContextUtils
                .getBean(IPlaylistVersionService.class);
        PlaylistVersionDO playlistVersionDO = iPlaylistVersionService
                .getReleasePlaylistVersionByPplUuid(
                        playlistVersionContentAssociationDO.getPlaylistUuid());
        PlaylistVersionContentAssociationDO associationDO = dos.stream()
                .filter(playlistVersionContentAssociationDO1 ->
                        Objects.equals(playlistVersionContentAssociationDO1.getPplVersionId(),
                                playlistVersionDO.getUuid())).findFirst().get();
        JSONObject jsonObject = JSON.parseObject(associationDO.getExtension());
        if (Boolean.TRUE.equals(jsonObject.getBoolean("multi_segment"))) {
            return "multi_segment";
        } else {
            return "single_segment";
        }
    }


}
