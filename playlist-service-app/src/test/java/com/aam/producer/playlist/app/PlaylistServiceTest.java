package com.aam.producer.playlist.app;

import static org.mockito.Mockito.when;

import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionActionService;
import com.aam.producer.playlist.biz.service.domain.impl.PlaylistActionServiceImpl;
import com.aam.producer.playlist.protocol.request.PlaylistDTO;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.utils.exception.BizException;
import java.util.UUID;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class PlaylistServiceTest {

    @InjectMocks
    PlaylistActionServiceImpl playlistActionService;
    @Mock
    IPlaylistService iPlaylistService;
    @Mock
    IPlaylistVersionService iPlaylistVersionService;
    @Mock
    IPlaylistVersionActionService iPlaylistVersionActionService;
    @Mock
    IMessageQueueService iMessageQueueService;

    /**
     * Create a static playlist.
     */
    @Test
    public void createStaticPlaylist() {
        String uuid = UUID.randomUUID().toString();
        String title = "static";
        boolean b = false;
        PlaylistDTO playlistDTO = new PlaylistDTO();
        playlistDTO.setAutomaticallyApply(b);
        playlistDTO.setTitle(title);
        when(iPlaylistService.createPlaylist(title, b)).thenReturn(uuid);
        String result = playlistActionService.createPlaylist(playlistDTO);
        Assert.assertEquals(result, uuid);
    }

    /**
     * Create a static Playlist with duplicate names.
     */
    @Test
    public void createStaticPlaylistWithDuplicateName() {
        String title = "static";
        boolean b = false;
        PlaylistDTO playlistDTO = new PlaylistDTO();
        playlistDTO.setAutomaticallyApply(b);
        playlistDTO.setTitle(title);
        when(iPlaylistService.getByTitle(title)).thenReturn(new PlaylistDO());
        try {
            playlistActionService.createPlaylist(playlistDTO);
            throw new RuntimeException();
        } catch (BizException bizException) {
            Assert.assertEquals(bizException.getCode(), ResultCodeEnum.TITLE_ALREADY_EXISTS);
        } catch (NullPointerException e) {

        }
    }


}
