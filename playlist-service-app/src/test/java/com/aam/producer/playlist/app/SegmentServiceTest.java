package com.aam.producer.playlist.app;

import static org.mockito.Mockito.when;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.ISegmentEventService;
import com.aam.producer.playlist.biz.service.ISegmentService;
import com.aam.producer.playlist.biz.service.domain.impl.SegmentActionServiceImpl;
import com.aam.producer.playlist.biz.service.domain.impl.SegmentViewServiceImpl;
import com.aam.producer.playlist.biz.util.EventPublish;
import com.aam.producer.playlist.protocol.request.SegmentDTO;
import com.aam.producer.playlist.protocol.request.SegmentSearchDTO;
import com.aam.producer.playlist.protocol.response.SegmentInfo;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.alibaba.fastjson.JSON;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.runners.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class SegmentServiceTest {

    @InjectMocks
    SegmentActionServiceImpl segmentActionService;
    @InjectMocks
    SegmentViewServiceImpl segmentViewService;
    @Mock
    ISegmentService iSegmentService;
    @Mock
    IPlaylistVersionContentAssociationService associationService;
    @Mock
    ISegmentEventService segmentEventService;
    @Mock
    EventPublish eventPublish;

    @Before
    public void init() {
        MockitoAnnotations.initMocks(SegmentActionServiceImpl.class);
        MockitoAnnotations.initMocks(SegmentViewServiceImpl.class);
    }

    @Test
    public void createPlaylistSegment() {
        String segmentTitle = "playlist segment";
        when(iSegmentService.getSegmentByTitle(segmentTitle)).thenReturn(new ArrayList<>());
        SegmentDTO segmentDTO = new SegmentDTO();
        int PLAYLIST_SEGMENT = 2;
        segmentDTO.setType(PLAYLIST_SEGMENT);
        segmentDTO.setTitle(segmentTitle);
        segmentDTO.setPurpose(1);
        segmentActionService.createSegment(segmentDTO);
    }

    @Test
    public void createTitleSegment() {
        String segmentTitle = "title segment";
        when(iSegmentService.getSegmentByTitle(segmentTitle)).thenReturn(new ArrayList<>());
        SegmentDTO segmentDTO = new SegmentDTO();
        int TITLE_SEGMENT = 3;
        segmentDTO.setType(TITLE_SEGMENT);
        segmentDTO.setTitle(segmentTitle);
        segmentDTO.setPurpose(1);
        segmentActionService.createSegment(segmentDTO);
    }

    @Test
    public void createThirdPartySegment() {
        String segmentTitle = "third party segment";
        when(iSegmentService.getSegmentByTitle(segmentTitle)).thenReturn(new ArrayList<>());
        SegmentDTO segmentDTO = new SegmentDTO();
        int API_SEGMENT = 4;
        segmentDTO.setType(API_SEGMENT);
        segmentDTO.setTitle(segmentTitle);
        segmentDTO.setPurpose(1);
        segmentActionService.createSegment(segmentDTO);
    }

    @Test
    public void createBaseSegment() {
        String segmentTitle = "base segment";
        when(iSegmentService.getSegmentByTitle(segmentTitle)).thenReturn(new ArrayList<>());
        SegmentDTO segmentDTO = new SegmentDTO();
        int BASE_SEGMENT = 6;
        segmentDTO.setType(BASE_SEGMENT);
        segmentDTO.setTitle(segmentTitle);
        segmentDTO.setPurpose(1);
        segmentActionService.createSegment(segmentDTO);
    }

    @Test
    public void createRatingSegment() {
        String segmentTitle = "Rating";
        when(iSegmentService.getSegmentByTitle(segmentTitle)).thenReturn(new ArrayList<>());
        segmentActionService.createRatingSegment();
    }

    @Test
    public void createDuplicateNameSegment() {
        String segmentTitle = "playlist segment";
        List<SegmentDO> segmentDOS = new ArrayList<>();
        SegmentDO segmentDO = new SegmentDO();
        segmentDOS.add(segmentDO);
        when(iSegmentService.getSegmentByTitle(segmentTitle)).thenReturn(segmentDOS);
        SegmentDTO segmentDTO = new SegmentDTO();
        int PLAYLIST_SEGMENT = 5;
        segmentDTO.setType(PLAYLIST_SEGMENT);
        segmentDTO.setTitle(segmentTitle);
        segmentDTO.setPurpose(1);
        try {
            segmentActionService.createSegment(segmentDTO);
        } catch (Exception e) {
            Assert.assertTrue(e instanceof NullPointerException);
        }
    }

   /* @Test
    public void updateSegmentTitle() {
        String segmentTitle = "Rating";
        String segmentUuid = UUID.randomUUID().toString();
        SegmentDTO segmentDTO = new SegmentDTO();
        segmentDTO.setPurpose(2);
        segmentDTO.setTitle(segmentTitle);
        when(iSegmentService.getSegmentByTitle(segmentTitle)).thenReturn(new ArrayList<>());
        segmentActionService.updateSegment(segmentUuid, segmentDTO);
    }*/


    @Test
    public void deleteSegment() {
        String segmentUuid = UUID.randomUUID().toString();
        when(associationService.contains(segmentUuid)).thenReturn(false);
        segmentActionService.deleteSegment(segmentUuid);
    }

    @Test
    public void getAutomaticSegment() {
        String segmentTitle = "Automatic Feature Selector";
        String segmentUuid = "5546e68d-6991-11e9-831c-0242ac140002";
        int segmentType = SegmentTypeEnum.AUTOMATIC_SEGMENT.getCode();
        SegmentDO segmentDO = new SegmentDO();
        segmentDO.setType(segmentType);
        segmentDO.setTitle(segmentTitle);
        segmentDO.setUuid(segmentUuid);
        when(iSegmentService.getAutomaticSegment()).thenReturn(segmentDO);
        SegmentInfo segmentInfo = segmentViewService.getAutomaticSegment();
        Assert.assertEquals(segmentInfo.getContentKind(),
                SegmentTypeEnum.AUTOMATIC_SEGMENT.getName());
        Assert.assertEquals(segmentInfo.getType(), ContentTypeEnum.SEGMENT.getName());
        Assert.assertEquals(segmentInfo.getTitle(), segmentTitle);
        Assert.assertEquals(segmentInfo.getUuid(), segmentUuid);
        Assert.assertEquals(segmentInfo.getVisualAutomation(), true);
        Assert.assertEquals(segmentInfo.getAudioAutomation(), true);
    }

    @Test
    public void getSegment() {
        String segmentUuid = UUID.randomUUID().toString();
        String segmentTitle = "segment";
        int segmentType = SegmentTypeEnum.PLAYLIST_SEGMENT.getCode();
        SegmentDO segmentDO = new SegmentDO();
        segmentDO.setUuid(segmentUuid);
        segmentDO.setTitle(segmentTitle);
        segmentDO.setType(segmentType);
        when(iSegmentService.getSegmentDO(segmentUuid)).thenReturn(segmentDO);
        SegmentInfo segmentInfo = segmentViewService.getSegment(segmentUuid);
        Assert.assertEquals(segmentInfo.getContentKind(),
                SegmentTypeEnum.PLAYLIST_SEGMENT.getName());
        Assert.assertEquals(segmentInfo.getType(), ContentTypeEnum.SEGMENT.getName());
        Assert.assertEquals(segmentInfo.getTitle(), segmentTitle);
        Assert.assertEquals(segmentInfo.getUuid(), segmentUuid);
    }

    @Test
    public void getAllSegment() {
        String segmentTitle = "segment";
        int segmentType = SegmentTypeEnum.PLAYLIST_SEGMENT.getCode();
        String segmentUuid = UUID.randomUUID().toString();
        List<Integer> types = new ArrayList<>();
        types.add(segmentType);
        List<SegmentDO> segmentDOS = new ArrayList<>();
        SegmentDO segmentDO = new SegmentDO();
        segmentDO.setUuid(segmentUuid);
        segmentDO.setTitle(segmentTitle);
        segmentDO.setType(segmentType);
        segmentDOS.add(segmentDO);
        when(iSegmentService.searchSegment(types, segmentTitle)).thenReturn(segmentDOS);
        SegmentSearchDTO segmentSearchDTO = new SegmentSearchDTO();
        segmentSearchDTO.setTitle(segmentTitle);
        segmentSearchDTO.setTypes(types);
        List<SegmentInfo> segmentInfos = segmentViewService
                .getAllSegment(JSON.toJSONString(segmentSearchDTO));
        Assert.assertEquals(1, segmentInfos.size());
        SegmentInfo segmentInfo = segmentInfos.get(0);
        Assert.assertEquals(segmentInfo.getContentKind(),
                SegmentTypeEnum.PLAYLIST_SEGMENT.getName());
        Assert.assertEquals(segmentInfo.getType(), ContentTypeEnum.SEGMENT.getName());
        Assert.assertEquals(segmentInfo.getTitle(), segmentTitle);
        Assert.assertEquals(segmentInfo.getUuid(), segmentUuid);
    }

}
