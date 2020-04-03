package com.aam.producer.playlist.app.controller;

import com.aam.producer.playlist.biz.service.domain.IPlaylistDevOpsService;
import com.aam.producer.playlist.biz.service.domain.ISegmentActionService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSendService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSyncService;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * playlist dev ops controller
 *
 * @author oliver.lo
 * @since 2019-11-27 15:20
 */
@RestController
@RequestMapping("/api/core/playlist/ops/")
public class DevOpsCoreController {

    private final IPlaylistDevOpsService opsService;
    private final ITPlaylistSyncService playlistSyncService;
    private final ITPlaylistSendService playlistSendService;
    private final IComplexFacadeClient complexFacadeClient;
    private final ISegmentActionService iSegmentActionService;
    @Autowired
    public DevOpsCoreController(
            IPlaylistDevOpsService opsService,
            ITPlaylistSyncService playlistSyncService,
            ITPlaylistSendService playlistSendService,
            IComplexFacadeClient complexFacadeClient,
            ISegmentActionService iSegmentActionService) {
        this.opsService = opsService;
        this.playlistSyncService = playlistSyncService;
        this.playlistSendService = playlistSendService;
        this.complexFacadeClient = complexFacadeClient;
        this.iSegmentActionService = iSegmentActionService;
    }

    @GetMapping("/complex_change_org")
    public void requestPosChangeOrg(@RequestParam(value = "complex_id") String complexId) {
        opsService.handleComplexChangeOrg(complexId);
    }

    @GetMapping("/clean_up_tasks")
    public void cleanUpTasks() {
        opsService.handleTimeoutTask();
    }

    @GetMapping("/request_playlist_hash")
    public void sendPlaylistHashSyncRequest(
            @RequestParam(value = "complex_id") String complexId,
            @RequestParam(value = "device_uuid", required = false) String deviceUuid,
            @RequestParam(value = "playlist_uuid", required = false) String playlistUuid) {
        TPlaylistTransferDTO dto = createTransferDTO(complexId, deviceUuid, playlistUuid);
        this.playlistSyncService.handlePlaylistHashRequest(dto);
    }

    @GetMapping("/request_playlist_send")
    public void sendPlaylistSendRequest(
            @RequestParam(value = "complex_id") String complexId,
            @RequestParam(value = "device_uuid", required = false) String deviceUuid,
            @RequestParam(value = "playlist_uuid") String playlistUuid) {
        TPlaylistTransferDTO dto = createTransferDTO(complexId, deviceUuid, playlistUuid);
        this.playlistSendService.handlePlaylistSendRequest(dto, null);
    }

    private TPlaylistTransferDTO createTransferDTO(String complexId, String deviceUuid,
            String playlistUuid) {
        TPlaylistTransferDTO dto = new TPlaylistTransferDTO();
        dto.setComplexUuid(complexId);
        if (StringUtils.isEmpty(deviceUuid)) {
            deviceUuid = complexFacadeClient.getLmsUuid(complexId);
        }
        dto.setDeviceUuid(deviceUuid);
        dto.setPlaylistUuid(playlistUuid);
        dto.setFastTrack(true);
        return dto;
    }

    @PutMapping("/synchronized_segment")
    public void synchronizedSegment(){
        iSegmentActionService.synchronizedSegment();
    }
}
