package com.aam.producer.playlist.app.controller;


import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitViewService;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.biz.util.TestUtil;
import com.aam.producer.playlist.protocol.response.TitleInfo;
import io.swagger.annotations.ApiOperation;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;


/**
 * PlaylistContentGroup management controller
 *
 * @author kaiden.peng
 * @since 2019/01/04 11:01 AM
 */
@RestController
@RequestMapping("/api/core/playlist/segment_split")
public class SegmentSplitCoreController {

    private final ISegmentSplitViewService iSegmentSplitViewService;
    private final ISegmentSplitActionService iSegmentSplitActionService;

    @Autowired
    public SegmentSplitCoreController(ISegmentSplitViewService iSegmentSplitViewService,
            ISegmentSplitActionService iSegmentSplitActionService) {
        this.iSegmentSplitViewService = iSegmentSplitViewService;
        this.iSegmentSplitActionService = iSegmentSplitActionService;
    }


    @GetMapping("/getShows")
    @ReturnHandler
    @ApiOperation(value = "获取分组匹配的场次")
    public List<String> getShowsBySegmentSplitUuid(
            @RequestParam("segment_split_uuid") String segmentSplitUuid,
            @RequestParam("org_id") String orgId) {
        OrgUtil.orgContenter.set(orgId);
        return iSegmentSplitViewService.getShowsBySegmentSplitUuid(segmentSplitUuid);
    }


    @GetMapping("/test")
    @ReturnHandler
    @ApiOperation(value = "影院周自动发布测试")
    public void scanAndPublishTest(@RequestParam(defaultValue = "0") Integer day) {
        Calendar cal = Calendar.getInstance();
        cal.set(Calendar.DATE, cal.get(Calendar.DATE) + day);
        iSegmentSplitActionService.scanAndPublish(cal);
    }

    @GetMapping("/test1")
    @ReturnHandler
    @ApiOperation(value = "影院周生成title Split测试")
    public TitleInfo getByContentAssociationUuid(
            @RequestParam(defaultValue = "0") Integer day,
            @RequestParam("org_id") String orgId,
            @RequestParam("content_association_uuid") String contentAssociationUuid,
            @RequestParam(value = "title_uuid") String titleUuid) {
        OrgUtil.orgContenter.set(orgId);
        TestUtil.day.set(day);
        return iSegmentSplitViewService.getTitleInfo(contentAssociationUuid, titleUuid);
    }

    @GetMapping("/scanAndPublish")
    @ReturnHandler
    @ApiOperation(value = "定时扫描发布周分组")
    @Async
    public void scanAndPublish() {
        Calendar cal = Calendar.getInstance();
        cal.setTime(new Date(System.currentTimeMillis() + 1000 * 60 * 30));//提前25分钟发布，小于场次的受保护时间
        iSegmentSplitActionService.scanAndPublish(cal);
    }
}
