package com.aam.producer.playlist.app.rest;


import com.aam.authentication.acl.utils.UserInfoUtil;
import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.domain.IPlaylistActionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistViewService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingService;
import com.aam.producer.playlist.protocol.request.PlaylistDTO;
import com.aam.producer.playlist.protocol.response.PlaylistInfo;
import com.aam.producer.playlist.protocol.response.PplViewInfo;
import com.aam.producer.playlist.protocol.response.TitleInfo1;
import com.aam.producer.playlist.protocol.response.TmsPlaylistInfo;
import com.aam.utils.model.PageListResult;
import com.google.common.collect.Sets;
import java.util.List;
import javax.validation.Valid;
import org.apache.commons.lang3.StringUtils;
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
@RequestMapping("/api/v1/playlist/ppl")
public class PlaylistController {

    private final IPosMappingService posMappingService;
    private IPlaylistActionService iPlaylistActionService;
    private IPlaylistViewService iPlaylistViewService;

    @Autowired
    public PlaylistController(IPlaylistActionService iPlaylistActionService,
            IPlaylistViewService iPlaylistViewService,
            IPosMappingService posMappingService) {
        this.iPlaylistActionService = iPlaylistActionService;
        this.iPlaylistViewService = iPlaylistViewService;
        this.posMappingService = posMappingService;
    }

    /**
     * Create a new playlist
     *
     * @param playlistDTO PlaylistDTO
     * @return uuid
     */
    @PostMapping
    @ReturnHandler
    public String createPlaylist(@RequestBody @Valid PlaylistDTO playlistDTO) {
        return iPlaylistActionService.createPlaylist(playlistDTO);
    }

    /**
     * Update title of an existing playlist
     *
     * @param playlistDTO PlaylistDTO
     */
    @PutMapping("/{playlist_uuid}")
    public void updatePlaylist(@PathVariable("playlist_uuid") String playlistUuid,
            @RequestBody PlaylistDTO playlistDTO) {
        iPlaylistActionService.updatePlaylist(playlistUuid, playlistDTO.getTitle());
    }

    /**
     * Delete a playlist
     *
     * @param playlistUuid playlist_uuid
     */
    @DeleteMapping("/{playlist_uuid}")
    public void deletePlaylist(@PathVariable("playlist_uuid") String playlistUuid) {
        iPlaylistActionService.deletePlaylist(playlistUuid);
    }

    /**
     * Copy a playlist
     *
     * @param playlistUuid playlist_uuid
     */
    @PostMapping("/{playlist_uuid}/copy")
    @ReturnHandler
    public String copyPlaylist(@PathVariable("playlist_uuid") String playlistUuid,
            @RequestBody @Valid PlaylistDTO playlistDTO) {
        return iPlaylistActionService.copyPlaylist(playlistUuid, playlistDTO);
    }


    /**
     * playlist match shows
     *
     * @param pplUuid playlist_uuid
     */
    @PutMapping("/{ppl_uuid}/match_pos")
    @ReturnHandler
    public void matchPos(@PathVariable("ppl_uuid") String pplUuid,
            @RequestBody String[] posUuids) {
        posMappingService.posMappingManual(pplUuid, Sets.newHashSet(posUuids));
    }


    /**
     * Get a playlist
     *
     * @param playlistUuid playlist_uuid
     */
    @GetMapping("/{playlist_uuid}")
    @ReturnHandler
    public PlaylistInfo getPlaylist(@PathVariable("playlist_uuid") String playlistUuid,
            @RequestParam(value = "versions", required = false) String versions,
            @RequestParam(value = "title_id", required = false) String titleId) {
        return iPlaylistViewService.getPlaylist(playlistUuid, versions, titleId);
    }

    /**
     * get tpl info
     *
     * @param pplUuid ppl uuid
     * @param posUuid pos uuid
     * @return tpl info
     */
    @GetMapping("/review/{ppl_uuid}")
    @ReturnHandler
    public TmsPlaylistInfo getByPplUuid(@PathVariable("ppl_uuid") String pplUuid,
            @RequestParam(value = "pos_uuid") String posUuid) {
        return posMappingService.reviewMappedTpl(pplUuid, posUuid);
    }

    @GetMapping
    public PageListResult<PplViewInfo> getPlaylist(
            @RequestParam(value = "page_num") Integer pageNum,
            @RequestParam(value = "page_size") Integer pageSize,
            @RequestParam(value = "search", required = false) String search,
            @RequestParam(value = "source", required = false) String source) {
        if ("tms".equals(source)) {
            return iPlaylistViewService
                    .searchTpl(pageNum, pageSize, search, UserInfoUtil.getOrganizationId());
        } else {
            return iPlaylistViewService
                    .search(pageNum, pageSize, search, UserInfoUtil.get().getUserId());
        }
    }

    @GetMapping("/by_title")
    public PageListResult<PplViewInfo> getPlaylistByTitle(
            @RequestParam(value = "title_uuid", required = false) String titleUuid,
            @RequestParam(value = "title", required = false) String title) {
        if (StringUtils.isEmpty(titleUuid)) {
            com.aam.utils.model.page.Page<PplViewInfo> tPage = new com.aam.utils.model.page.Page<>();
            return new PageListResult<>(tPage);
        }
        return iPlaylistViewService
                .searchByTitle(titleUuid, title, UserInfoUtil.get().getUserId());
    }

    @GetMapping("/by_content_id")
    public PageListResult<PplViewInfo> getPlaylistByContent(
            @RequestParam(value = "content_id") String contentId,
            @RequestParam(value = "title", required = false) String title) {
        return iPlaylistViewService
                .searchByContentId(title, contentId);
    }

    /**
     * Get a playlist segment of title info
     * @param playlistUuid playlist_uuid
     * @param associationUuid association_uuid
     * @return title info
     */
    @GetMapping("/{playlist_uuid}/{association_uuid}")
    @ReturnHandler
    public List<TitleInfo1> getPlaylistSegmentTitleInfo(@PathVariable("playlist_uuid") String playlistUuid,
            @PathVariable("association_uuid") String associationUuid) {
        return iPlaylistViewService.getPlaylistSegmentTitleInfo(playlistUuid, associationUuid);
    }

    /**
     *  All segment publish.
     *
     * @param playlistUuid playlistUuid
     */
    @PutMapping("/{playlist_uuid}/all_segment/publish")
    public void allSegmentPublish(@PathVariable("playlist_uuid") String playlistUuid) {
        iPlaylistActionService.allSegmentPublish(playlistUuid);
    }


}
