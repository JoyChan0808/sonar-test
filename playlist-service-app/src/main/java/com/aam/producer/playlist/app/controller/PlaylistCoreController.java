package com.aam.producer.playlist.app.controller;

import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionActionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentViewService;
import com.aam.producer.playlist.biz.util.OrgUtil;
import io.swagger.annotations.ApiOperation;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/core/playlist/ppl")
public class PlaylistCoreController {

    private IPlaylistVersionContentViewService contentViewService;
    private IPlaylistVersionActionService actionService;

    @Autowired
    public PlaylistCoreController(IPlaylistVersionContentViewService contentViewService,
            IPlaylistVersionActionService actionService) {
        this.contentViewService = contentViewService;
        this.actionService = actionService;
    }

    @GetMapping
    @ReturnHandler
    @ApiOperation(value = "根据content uuid 获取已发布播放列表ppl uuid")
    public List<String> getPlaylistUuids(@RequestParam("content_uuid") String contentUuid,
            @RequestParam("org_uuid") String orgUuid) {
        OrgUtil.orgContenter.set(orgUuid);
        return contentViewService.getPlaylistUuidsByContentUuid(contentUuid);
    }

    @GetMapping("/{orgUuid}/{pplUuid}/{versionUuid}")
    public void publishPpl(
            @PathVariable("orgUuid") String orgUuid,
            @PathVariable("pplUuid") String pplUuid,
            @PathVariable("versionUuid") String versionUuid
    ) {
        OrgUtil.orgContenter.set(orgUuid);
        actionService.publishPlaylistVersion(pplUuid, versionUuid);
    }
}
