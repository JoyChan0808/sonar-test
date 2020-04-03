package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.service.domain.ISPlService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSyncService;
import com.aam.producer.playlist.protocol.request.SPLSignDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

@Service
public class SPlServiceImpl implements ISPlService {

    private final static Logger logger = LoggerFactory.getLogger(SPlServiceImpl.class);
    private final ITPlaylistSyncService itPlaylistSyncService;
    private final IComplexFacadeClient complexFacadeClient;

    @Autowired
    public SPlServiceImpl(ITPlaylistSyncService itPlaylistSyncService,
            IComplexFacadeClient complexFacadeClient) {
        this.itPlaylistSyncService = itPlaylistSyncService;
        this.complexFacadeClient = complexFacadeClient;
    }

    @Override
    public void requestSPL(SPLSignDTO splSignDTO) {
        logger.info("Request SPLs sync.dto:<{}>", splSignDTO);

        splSignDTO.getSplPayloads().forEach(splPayload -> {
            String deviceUuid = splPayload.getDeviceUuid();
            if (StringUtils.isEmpty(deviceUuid)) {
                deviceUuid = complexFacadeClient.getLmsUuid(splSignDTO.getComplexUuid());
            }
            if (StringUtils.isEmpty(deviceUuid)) {
                logger.error("Request SPLs sync error,deviceUuid not fault.dto:<{}>", splSignDTO);
                return;
            }
            TPlaylistTransferDTO dto = new TPlaylistTransferDTO();
            dto.setComplexUuid(splSignDTO.getComplexUuid());
            dto.setDeviceUuid(deviceUuid);
            dto.setFastTrack(true);
            dto.setPlaylistUuid(splPayload.getPlaylistUuid());
            itPlaylistSyncService.handlePlaylistHashRequest(dto);
        });
    }

}
