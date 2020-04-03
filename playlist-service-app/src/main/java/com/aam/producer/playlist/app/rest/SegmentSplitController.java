package com.aam.producer.playlist.app.rest;


import com.aam.authentication.acl.utils.UserInfoUtil;
import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitViewService;
import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.protocol.request.PublishDTO;
import com.aam.producer.playlist.protocol.request.SegmentSplitOptionDTO;
import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.protocol.response.SplitComplexInfo1;
import com.aam.producer.playlist.protocol.response.TitleInfo;
import io.swagger.annotations.ApiOperation;
import java.util.List;
import java.util.Map;
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
 * PlaylistContentGroup management controller
 *
 * @author kaiden.peng
 * @since 2019/01/04 11:01 AM
 */
@RestController
@RequestMapping("/api/v1/playlist/segment_split")
public class SegmentSplitController {

    private final ISegmentSplitViewService iSegmentSplitViewService;

    private final ISegmentSplitActionService iSegmentSplitActionService;

    @Autowired
    public SegmentSplitController(ISegmentSplitActionService iSegmentSplitActionService,
            ISegmentSplitViewService iSegmentSplitViewService) {
        this.iSegmentSplitViewService = iSegmentSplitViewService;
        this.iSegmentSplitActionService = iSegmentSplitActionService;
    }


    @PutMapping("/reset_segment_split")
    @ReturnHandler
    @ApiOperation(value = "重新生成默认分组接口")
    public void resetSegmentSplit(
            @RequestParam("content_association_uuid") String contentAssociationUuid,
            @RequestParam(value = "title_uuid") String titleUuid) {
        iSegmentSplitActionService.resetSegmentSplit(contentAssociationUuid, titleUuid);
    }

    @PostMapping
    @ReturnHandler
    @ApiOperation(value = "添加分组接口")
    public void createSegmentSplit(@RequestBody SegmentSplitOptionDTO segmentSplitOptionDTO) {
        iSegmentSplitActionService.createSegmentSplit(segmentSplitOptionDTO);
    }

    @PutMapping
    @ApiOperation(value = "修改分组接口")
    public void updateSegmentSplit(@RequestBody SegmentSplitOptionDTO segmentSplitOptionDTO) {
        iSegmentSplitActionService.updateSegmentSplit(segmentSplitOptionDTO);
    }

    @PutMapping("/update_content")
    @ApiOperation(value = "修改分组内容接口")
    public void updateSegmentSplitContent(@RequestBody Map<String, List<ContentDTO>> contentMap) {
        iSegmentSplitActionService.updateSegmentSplitContent(contentMap);
    }

    @DeleteMapping("/{uuid}")
    @ApiOperation(value = "删除分组接口")
    public void deleteSegmentSplit(@PathVariable("uuid") String uuid) {
        iSegmentSplitActionService.deleteSegmentSplit(uuid);
    }

    @GetMapping
    @ReturnHandler
    @ApiOperation(value = "获取分组列表接口")
    public TitleInfo getByContentAssociationUuid(
            @RequestParam("content_association_uuid") String contentAssociationUuid,
            @RequestParam(value = "title_uuid", required = false) String titleUuid) {
        return iSegmentSplitViewService.getTitleInfo(contentAssociationUuid, titleUuid);
    }

    @PutMapping("/create_draft")
    @ApiOperation(value = "添加草稿接口")
    public void createSegmentSplitDraft(
            @RequestParam("content_association_uuid") String contentAssociationUuid,
            @RequestParam(value = "title_uuid", required = false) String titleUuid,
            @RequestParam(value = "copy", required = false) Boolean copy) {
        iSegmentSplitActionService.createSegmentSplitDraft(contentAssociationUuid, titleUuid, copy);
    }

    @DeleteMapping("/delete_draft")
    @ApiOperation(value = "删除草稿接口")
    public void deleteSegmentSplitDraft(
            @RequestParam("content_association_uuid") String contentAssociationUuid,
            @RequestParam(value = "title_uuid", required = false) String titleUuid) {
        iSegmentSplitActionService.deleteDraftByAssociationUuid(contentAssociationUuid, titleUuid);
    }

    @PutMapping("/publish")
    @ApiOperation(value = "发布接口")
    public void publishSegmentSplitByAssociation(
            @RequestParam("content_association_uuid") String contentAssociationUuid,
            @RequestParam(value = "title_uuid", required = false) String titleUuid,
            @RequestBody PublishDTO publishDTO) {
        iSegmentSplitActionService
                .publishSegmentSplitByAssociation(contentAssociationUuid, titleUuid, publishDTO);
    }

    @PutMapping("/unPublish")
    @ApiOperation(value = "取消发布接口")
    public void unPublishSegmentSplitByAssociation(
            @RequestParam("content_association_uuid") String contentAssociationUuid,
            @RequestParam(value = "title_uuid", required = false) String titleUuid,
            @RequestParam("keep_release") Boolean keepRelease) {
        iSegmentSplitActionService
                .unPublishSegmentSplitByAssociation(contentAssociationUuid, titleUuid, keepRelease);
    }


    @GetMapping("/{uuid}/complex")
    @ReturnHandler
    @ApiOperation(value = "获取分组对应的影院uuid")
    public List<SplitComplexInfo1> getComplex(@PathVariable("uuid") String uuid) {
        return iSegmentSplitViewService
                .getSplitComplex(uuid, UserInfoUtil.get().getComplexGroupIds());
    }

    @GetMapping("/{uuid}/show_type")
    @ReturnHandler
    @ApiOperation(value = "获取分组对应的showType")
    public List<ShowAttributeGroupInfo> getShowType(@PathVariable("uuid") String uuid) {
        return iSegmentSplitViewService
                .getShowType(uuid);
    }

}
