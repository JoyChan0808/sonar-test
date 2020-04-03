package com.aam.producer.playlist.app;

import static org.junit.Assert.assertNotNull;

import com.aam.producer.playlist.biz.service.domain.impl.TPlaylistActionServiceImpl;
import com.aam.producer.playlist.biz.service.domain.impl.TPlaylistSyncServiceImpl;
import com.aam.producer.playlist.biz.service.impl.PlaylistSyncLogServiceImpl;
import com.aam.producer.playlist.protocol.request.TPlaylistHashSyncedDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistSyncedDTO;
import com.aam.producer.playlist.repository.dao.PlaylistSyncLogMapper;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.alibaba.fastjson.JSON;
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
 * @since 2019/4/22 3:54 PM
 */
@RunWith(SpringJUnit4ClassRunner.class)
public class PlaylistSyncServiceTest {

    @InjectMocks
    private TPlaylistSyncServiceImpl playlistSyncService;

    @Spy
    @InjectMocks
    private PlaylistSyncLogServiceImpl playlistSyncLogService;

    @Spy
    @InjectMocks
    private TPlaylistActionServiceImpl playlistActionService;

    @Mock
    private IComplexFacadeClient complexFacade;

    @Mock
    private PlaylistSyncLogMapper playlistSyncLogMapper;

    @Before
    public void setUp() throws Exception {
        // mock
        MockitoAnnotations.initMocks(this);
        ReflectionTestUtils
                .setField(playlistSyncService, "playlistSyncLogService", playlistSyncLogService);
    }

    @Test
    public void testPlaylistHashSyncResponse() {
        String dto = "{\"complex_uuid\":\"edf8636d-a1e2-4aac-971b-3b34684b6863\",\"playlists\":{\"79f99628-5b23-494c-bf81-b7fedcfd9c56\":{\"hashed_playlists\":{\"3a7becee-0af8-486d-bd10-e69a7ef9f4f1\":\"d6bd7a2bcb146b23f5794e00fa43b76faf49f1ed\"}}}}";
        TPlaylistHashSyncedDTO hashSyncDTO = JSON.parseObject(dto, TPlaylistHashSyncedDTO.class);
        // assert parse
        assertNotNull(hashSyncDTO);
        this.playlistSyncService.handlePlaylistHashResponse(hashSyncDTO);
    }

    @Test
    public void testPlaylistSyncResponse() {
        String dto = "{\"complex_uuid\":\"edf8636d-a1e2-4aac-971b-3b34684b6863\",\"playlist\":{\"device_uuid\":\"79f99628-5b23-494c-bf81-b7fedcfd9c56\",\"playlist\":{\"title\":\"performance_test_351\",\"is_hfr\":false,\"is_3d\":true,\"is_4k\":false,\"automation\":[],\"is_template\":false,\"duration_in_seconds\":120.2083333333333,\"last_modified\":1560233710.7006071,\"events\":[{\"duration_in_frames\":365,\"cpl_end_time_in_frames\":365,\"cpl_id\":\"130ed46a-7113-439f-82be-7385fee5b09d\",\"edit_rate\":[24,1],\"text\":\"1122_lucheng_WX_JPEG_239\",\"cpl_end_time_in_seconds\":15.2083333333333,\"automation\":[],\"duration_in_seconds\":15.2083333333333,\"playback_mode\":\"2D\",\"type\":\"composition\",\"cpl_start_time_in_frames\":0,\"content_kind\":\"advertisement\",\"total_duration_in_seconds\":15.2083333333333,\"cpl_start_time_in_seconds\":0}],\"id\":\"3a7becee-0af8-486d-bd10-e69a7ef9f4f1\"},\"content_ids\":[\"130ed46a-7113-439f-82be-7385fee5b09d\"],\"uuid\":\"3a7becee-0af8-486d-bd10-e69a7ef9f4f1\",\"title\":\"performance_test_351\",\"total_duration_in_seconds\":120.2083333333333,\"is_hfr\":false,\"is_3d\":true,\"is_4k\":false,\"is_template\":true,\"duration_in_seconds\":120.2083333333333,\"preshow_duration\":25.2083333333333}}";
        TPlaylistSyncedDTO syncDTO = JSON.parseObject(dto, TPlaylistSyncedDTO.class);
        // assert parse
        assertNotNull(syncDTO);
        this.playlistSyncService.handlePlaylistSyncedResponse(syncDTO);
    }
}
