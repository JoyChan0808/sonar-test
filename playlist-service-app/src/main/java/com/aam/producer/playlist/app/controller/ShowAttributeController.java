package com.aam.producer.playlist.app.controller;

import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.IShowAttributeService;
import com.aam.producer.playlist.biz.util.OrgUtil;
import java.util.List;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/core/playlist/show_attribute")
public class ShowAttributeController {

    private final IShowAttributeService iShowAttributeService;

    public ShowAttributeController(IShowAttributeService iShowAttributeService) {
        this.iShowAttributeService = iShowAttributeService;
    }

    @GetMapping("/sum_sort_code")
    @ReturnHandler
    public List<Long> sumSortCode(String group, String orgId) {
        OrgUtil.orgContenter.set(orgId);
        return iShowAttributeService.getShortCodeSumByTitlesList(group);
    }

}
