package com.aam.producer.playlist.app.rest;

import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistActionService;
import com.aam.producer.playlist.protocol.response.TmsPlaylistInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * playlist rest controller
 *
 * @author oliver.lo
 * @since 2019/5/17 2:35 PM
 */
@RestController
@RequestMapping("/api/v1/playlist/tpl")
public class TPlaylistRestController {

    private final ITPlaylistActionService playlistActionService;

    @Autowired
    public TPlaylistRestController(ITPlaylistActionService playlistActionService) {
        this.playlistActionService = playlistActionService;
    }

    @GetMapping("/{playlist_uuid}")
    @ReturnHandler
    public TmsPlaylistInfo detail(@PathVariable("playlist_uuid") String playlistUuid,
            @RequestParam(value = "complex_id") String complexId) {
        return this.playlistActionService.getByPlaylistUuid(playlistUuid, complexId);
    }
}
