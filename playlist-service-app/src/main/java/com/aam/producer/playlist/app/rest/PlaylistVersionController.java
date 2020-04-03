package com.aam.producer.playlist.app.rest;


import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionActionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionViewService;
import com.aam.producer.playlist.protocol.request.PlaylistVersionDTO;
import com.aam.producer.playlist.protocol.request.PublishDTO;
import com.aam.producer.playlist.protocol.response.PlaylistVersionInfo;
import java.util.List;
import javax.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Playlist management controller
 *
 * @author kaiden.peng
 * @since 2019/16/04 10:16 AM
 */
@RestController
@RequestMapping("/api/v1/playlist/playlist_version")
public class PlaylistVersionController {

    private final IPlaylistVersionActionService iPlaylistVersionActionService;

    private final IPlaylistVersionViewService iPlaylistVersionViewService;

    @Autowired
    public PlaylistVersionController(IPlaylistVersionActionService iPlaylistVersionActionService,
            IPlaylistVersionViewService iPlaylistVersionViewService) {
        this.iPlaylistVersionActionService = iPlaylistVersionActionService;
        this.iPlaylistVersionViewService = iPlaylistVersionViewService;
    }

    /**
     * get playlist version by playlist
     *
     * @param playlistUuid playlistUuid
     * @return playlistVersionInfos
     */
    @GetMapping
    @ReturnHandler
    public List<PlaylistVersionInfo> getPlaylistVersions(
            @RequestParam("playlist_uuid") String playlistUuid) {
        return iPlaylistVersionViewService.getPlaylistVersions(playlistUuid);
    }


    /**
     * Create a new playlist version
     *
     * @param playListVersionDTO PlayListVersionDTO
     * @return uuid
     */
    @PostMapping
    @ReturnHandler
    public String createPlaylistVersion(@RequestBody @Valid PlaylistVersionDTO playListVersionDTO) {
        return iPlaylistVersionActionService.createPlaylistVersion(playListVersionDTO);
    }

    /**
     * create a draft version
     *
     * @param versionUuid version uuid
     * @return draft version uuid
     */
    @PostMapping("/{version_uuid}/createDraft")
    @ReturnHandler
    public String createPlaylistVersionDraft(@PathVariable("version_uuid") String versionUuid,
            @RequestParam(value = "copy_content", required = false) Boolean copyContent) {
        return iPlaylistVersionActionService.createPlaylistVersionDraft(versionUuid, copyContent);
    }

    /**
     * update a playlist version
     *
     * @param playListVersionDTO PlayListVersionDTO
     */
    @PutMapping("/{version_uuid}")
    public void updatePlaylistVersion(@PathVariable("version_uuid") String versionUuid,
            @RequestBody @Valid PlaylistVersionDTO playListVersionDTO) {
        iPlaylistVersionActionService.updatePlaylistVersion(versionUuid, playListVersionDTO);
    }

    /**
     * Delete a specified version of the playlist
     *
     * @param versionUuid version_uuid
     */
    @DeleteMapping("/{version_uuid}")
    public void deletePlaylistVersion(@PathVariable("version_uuid") String versionUuid) {
        iPlaylistVersionActionService.deletePlaylistVersion(versionUuid);
    }


    /**
     * Publish a specified version playlists,the specified version must exist before publish it
     *
     * @param versionUuid version_uuid
     */
    @PutMapping("/{version_uuid}/publish")
    public void publishPlaylistVersion(@PathVariable("version_uuid") String versionUuid,
            @RequestBody PublishDTO publishDTO) {
        iPlaylistVersionActionService.publishPlaylistVersion(versionUuid, publishDTO);
    }

    /**
     * UnPublish a specified version playlists,the specified version must exist before publish it
     *
     * @param versionUuid version_uuid
     */
    @PutMapping("/{version_uuid}/unpublish")
    public void unPublishPlaylistVersion(@PathVariable("version_uuid") String versionUuid,
            @RequestParam("keep_release") Boolean keepRelease) {
        iPlaylistVersionActionService.unPublishPlaylistVersion(versionUuid, keepRelease);
    }


}
