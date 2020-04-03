package com.aam.producer.playlist.app;

import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.impl.TPlaylistActionServiceImpl;
import com.aam.producer.playlist.biz.service.domain.impl.TPlaylistSendServiceImpl;
import com.aam.producer.playlist.biz.service.impl.PlaylistSendLogServiceImpl;
import com.aam.producer.playlist.protocol.request.TPlaylistActionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeliveryDTO;
import com.aam.producer.playlist.repository.dao.PlaylistSendLogMapper;
import com.aam.producer.playlist.repository.dao.TmsPlaylistMapper;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.util.ReflectionTestUtils;

/**
 * com.aam.producer.playlist.app
 *
 * @author oliver.lo
 * @since 2019/6/20 4:29 PM
 */
@RunWith(SpringJUnit4ClassRunner.class)
public class PlaylistSendServiceTest {

    @InjectMocks
    private TPlaylistSendServiceImpl playlistSendService;

    @Spy
    @InjectMocks
    private PlaylistSendLogServiceImpl playlistSendLogService;

    @Spy
    @InjectMocks
    private TPlaylistActionServiceImpl playlistActionService;

    @Mock
    private IMessageQueueService messageQueueService;

    @Mock
    private IComplexFacadeClient complexFacade;

    @Mock
    private TmsPlaylistMapper tmsPlaylistMapper;

    @Mock
    private PlaylistSendLogMapper playlistSendLogMapper;

    @Before
    public void setUp() throws Exception {
        // mock
        MockitoAnnotations.initMocks(this);
        ReflectionTestUtils
                .setField(playlistSendService, "playlistSendLogService", playlistSendLogService);
        //ReflectionTestUtils.setField(playlistSendService, "playlistActionService", playlistActionService);
        ReflectionTestUtils.setField(playlistActionService, "mqService", messageQueueService);
        ReflectionTestUtils.setField(playlistSendLogService, "baseMapper", playlistSendLogMapper);
        //ReflectionTestUtils.setField(playlistActionService, "complexFacade", complexFacade);
    }

    @Test
    public void testPlaylistDeliveredResponse() {
        TPlaylistDeliveryDTO dto = new TPlaylistDeliveryDTO();
        dto.setComplexUuid("99f0f82d-20c1-4b8f-8c79-644b454e0226");
        dto.setReceiptUuid("8e386902-2fbd-482f-b6a5-9b0451d01700");
        dto.setDeviceUuid("63a71d3c-140a-440a-a09f-d724b4b5fb0d");
        dto.setDeliveryStatus(false);
        dto.setMessage("failed");
        playlistSendService.handlePlaylistDeliveredResponse(dto);
    }

    @Test
    public void testPlaylistActionedResponse() {
        TPlaylistActionDTO dto = new TPlaylistActionDTO();
        dto.setActionId("8e386902-2fbd-482f-b6a5-9b0451d01700");
        dto.setSuccess(false);
        dto.setMessage("failed");
        playlistSendService.handlePlaylistActionedResponse(dto);
    }
}
