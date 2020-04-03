package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistDevOpsService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingSendService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSendService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSyncService;
import com.aam.producer.playlist.protocol.request.ComplexFlmDTO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * dev ops service
 *
 * @author oliver.lo
 * @since 2019-11-27 15:24
 */
@Service
public class PlaylistDevOpsServiceImpl implements IPlaylistDevOpsService {

    private static final Logger logger = LoggerFactory.getLogger(PlaylistDevOpsServiceImpl.class);

    private final IComplexFacadeClient complexFacadeClient;
    private final IPosPlaylistMappingService playlistMappingService;
    private final ITmsPlaylistService tmsPlaylistService;
    private final ITPlaylistSendService playlistSendService;
    private final ITPlaylistSyncService playlistSyncService;
    private final IPosMappingSendService posMappingSendService;

    @Autowired
    public PlaylistDevOpsServiceImpl(
            IComplexFacadeClient complexFacadeClient,
            IPosPlaylistMappingService playlistMappingService,
            ITmsPlaylistService tmsPlaylistService,
            ITPlaylistSendService playlistSendService,
            ITPlaylistSyncService playlistSyncService,
            IPosMappingSendService posMappingSendService) {
        this.complexFacadeClient = complexFacadeClient;
        this.playlistMappingService = playlistMappingService;
        this.tmsPlaylistService = tmsPlaylistService;
        this.playlistSendService = playlistSendService;
        this.playlistSyncService = playlistSyncService;
        this.posMappingSendService = posMappingSendService;
    }

    @Override
    @Transactional
    public void handleComplexChangeOrg(String complexId) {
        complexFacadeClient.orgCacheEvict(complexId);
        String newOrgId = complexFacadeClient.getOrganizationUuid(complexId);
        int posUpdated = playlistMappingService.correctPosOrgId(newOrgId, complexId);
        int tplUpdated = tmsPlaylistService.correctTplOrgId(newOrgId, complexId);
        logger.info(
                "[DevOps] complex change org,complex_uuid:<{}>,new_org:<{}>,changed_pos_num:<{}>,changed_tpl_num:<{}>",
                complexId, newOrgId, posUpdated, tplUpdated);
    }

    @Override
    @Transactional
    public void handleTimeoutTask() {
        logger.info("[Scheduled] start checking task");
        playlistSendService.handleTimeoutTask();
        playlistSyncService.handleTimeoutTask();
        posMappingSendService.handleTimeoutTask();
    }

    @Override
    public void handleComplexFlmData(ComplexFlmDTO dto) {
        logger.info("receive a complex-flm.data,message:<{}>", dto);
        String complexUuid = dto.getComplexMeta().getComplexUuid();
        String orgId = dto.getComplexInfo().getOrganizationUuid();
        String currentOrgId = complexFacadeClient.getOrganizationUuid(complexUuid);
        if (StringUtils.isNotEmpty(orgId) && !orgId.equals(currentOrgId)) {
            handleComplexChangeOrg(complexUuid);
        }
    }
}
