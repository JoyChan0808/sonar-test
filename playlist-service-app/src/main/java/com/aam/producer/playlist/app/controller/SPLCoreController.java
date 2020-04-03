package com.aam.producer.playlist.app.controller;

import com.aam.producer.playlist.biz.service.domain.ISPlService;
import com.aam.producer.playlist.protocol.request.SPLSignDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * com.aam.producer.playlist.app.controller
 *
 * @author kaiden
 * @since 2020-03-09 11:58
 */
@RestController
@RequestMapping("/api/core/playlist/spl")
public class SPLCoreController {

    private final ISPlService isPlService;

    @Autowired
    public SPLCoreController(ISPlService isPlService) {
        this.isPlService = isPlService;
    }

    /**
     * spl sync
     * @param splSignDTO splSignDTO
     */
    @PutMapping("/spl_sync")
    @Async
    public void splSync(@RequestBody SPLSignDTO splSignDTO) {
        isPlService.requestSPL(splSignDTO);
    }

}
