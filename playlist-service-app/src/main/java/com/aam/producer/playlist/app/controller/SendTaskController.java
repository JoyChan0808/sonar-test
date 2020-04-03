package com.aam.producer.playlist.app.controller;

import com.aam.producer.playlist.biz.service.domain.ITPlaylistSendService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSyncService;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * send task api
 *
 * @author oliver.lo
 * @since 2019/6/14 5:10 PM
 */
@Deprecated
@RestController
@RequestMapping("/api/core/playlist/mq")
public class SendTaskController {

    private final ITPlaylistSyncService playlistSyncService;
    private final ITPlaylistSendService playlistSendService;

    @Autowired
    public SendTaskController(
            ITPlaylistSyncService playlistSyncService,
            ITPlaylistSendService playlistSendService) {
        this.playlistSyncService = playlistSyncService;
        this.playlistSendService = playlistSendService;
    }

    @GetMapping("/request_playlist_hash")
    public void sendPlaylistHashSyncRequest(
            @RequestParam(value = "complex_id") String complexId,
            @RequestParam(value = "device_uuid") String deviceUuid,
            @RequestParam(value = "playlist_uuid", required = false) String playlistUuid) {
        TPlaylistTransferDTO dto = new TPlaylistTransferDTO();
        dto.setComplexUuid(complexId);
        dto.setDeviceUuid(deviceUuid);
        dto.setPlaylistUuid(playlistUuid);
        dto.setFastTrack(true);
        this.playlistSyncService.handlePlaylistHashRequest(dto);
    }

    @GetMapping("/request_playlist_send")
    public void sendPlaylistSendRequest(@RequestParam(value = "complex_id") String complexId,
            @RequestParam(value = "device_uuid") String deviceUuid,
            @RequestParam(value = "playlist_uuid") String playlistUuid) {
        TPlaylistTransferDTO dto = new TPlaylistTransferDTO();
        dto.setComplexUuid(complexId);
        dto.setDeviceUuid(deviceUuid);
        dto.setPlaylistUuid(playlistUuid);
        dto.setFastTrack(true);
        this.playlistSendService.handlePlaylistSendRequest(dto, null);
    }
}
