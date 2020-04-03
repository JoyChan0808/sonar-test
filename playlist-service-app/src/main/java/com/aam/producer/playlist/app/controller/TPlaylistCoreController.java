package com.aam.producer.playlist.app.controller;

import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.domain.IPosMappingService;
import com.aam.producer.playlist.protocol.response.TmsPlaylistInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * com.aam.producer.playlist.app.controller
 *
 * @author oliver.lo
 * @since 2019-10-12 11:18
 */
@RestController
@RequestMapping("/api/core/playlist/tpl")
public class TPlaylistCoreController {

    private final IPosMappingService posMappingService;

    @Autowired
    public TPlaylistCoreController(
            IPosMappingService posMappingService) {
        this.posMappingService = posMappingService;
    }

    /**
     * get tpl info
     *
     * @param pplUuid ppl uuid
     * @param posUuid pos uuid
     * @return tpl info
     */
    @GetMapping("/review")
    @ReturnHandler
    public TmsPlaylistInfo review(@RequestParam(value = "ppl_uuid") String pplUuid,
            @RequestParam(value = "pos_uuid") String posUuid) {
        return posMappingService.reviewMappedTpl(pplUuid, posUuid);
    }
}
