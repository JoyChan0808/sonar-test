package com.aam.producer.playlist.app.rest;


import com.aam.producer.playlist.app.config.ReturnHandler;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentViewService;
import com.aam.producer.playlist.protocol.request.PublishSegmentDTO;
import com.aam.producer.playlist.protocol.request.SegmentDTO;
import com.aam.producer.playlist.protocol.response.SegmentInfo;
import com.aam.producer.playlist.sal.response.TitleInfo;
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
import org.springframework.web.bind.annotation.RestController;

/**
 * Segment management controller
 *
 * @author kaiden.peng
 * @since 2019/16/04 10:16 AM
 */
@RestController
@RequestMapping("/api/v1/playlist/segment")
public class SegmentController {

    private final ISegmentActionService iSegmentActionService;

    private final ISegmentViewService iSegmentViewService;

    private final IPlaylistVersionContentActionService associationService;

    @Autowired
    public SegmentController(ISegmentActionService iSegmentActionService,
            ISegmentViewService iSegmentViewService,
            IPlaylistVersionContentActionService associationService) {
        this.iSegmentActionService = iSegmentActionService;
        this.iSegmentViewService = iSegmentViewService;
        this.associationService = associationService;
    }


    /**
     * Create a new segment
     *
     * @param segmentDTO SegmentDTO
     * @return uuid
     */
    @PostMapping
    @ReturnHandler
    public String createSegment(@RequestBody @Valid SegmentDTO segmentDTO) {
        return iSegmentActionService.createSegment(segmentDTO);
    }


    /**
     * Update the information of an existing segment
     *
     * @param segmentDTO SegmentDTO
     */
    @PutMapping("/{segment_uuid}")
    public void updateSegment(@PathVariable("segment_uuid") String segmentUuid,
            @RequestBody @Valid SegmentDTO segmentDTO) {
        iSegmentActionService.updateSegment(segmentUuid, segmentDTO);
    }


    @PutMapping("/{segment_uuid}/cancel_week")
    public void cancelWeek(@PathVariable("segment_uuid") String segmentUuid) {
        iSegmentActionService.cancelWeek(segmentUuid);
    }


    /**
     * Delete a segment
     *
     * @param segmentUuid segment_uuid
     */
    @DeleteMapping("/{segment_uuid}")
    public void deleteSegment(@PathVariable("segment_uuid") String segmentUuid) {
        iSegmentActionService.deleteSegment(segmentUuid);
    }

    @GetMapping("/{uuid}")
    @ReturnHandler
    public SegmentInfo getSegment(@PathVariable String uuid) {
        return iSegmentViewService.getSegment(uuid);
    }

    @GetMapping
    @ReturnHandler
    public List<SegmentInfo> getAllSegment(String search) {
        return iSegmentViewService.getAllSegment(search);
    }

    @GetMapping("/apiSegment/names")
    @ReturnHandler
    public List<String> getApiSegmentNames() {
        return iSegmentViewService.getApiSegmentNames();
    }

    @GetMapping("/{association_uuid}/getType")
    @ReturnHandler
    public String getType(@PathVariable("association_uuid") String associationUuid) {
        return associationService.getContentKind(associationUuid);
    }

    @GetMapping("/get_all_title")
    @ReturnHandler
    public List<TitleInfo> getAllTitleInfo(String search) {
        return iSegmentViewService.getAllSegmentDetail(search);
    }

    /**
     * Batch publish segment
     *
     * @param publishDTO PublishSegmentDTO
     */
    @PutMapping("/batch_publish_segment")
    public void batchPublishSegment(@RequestBody PublishSegmentDTO publishDTO) {
        iSegmentActionService.batchPublishSegment( publishDTO);
    }

}
