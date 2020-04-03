package com.aam.producer.playlist.biz.event;

import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import java.util.List;
import org.springframework.context.ApplicationEvent;

public class CreateTplEvent extends ApplicationEvent {

    private PlaylistDO playlist;

    private List<PosPlaylistMappingDO> posPlaylistMappingDOS;

    private int action;

    public CreateTplEvent(PlaylistDO playlist, List<PosPlaylistMappingDO> posPlaylistMappingDOS,
            int action) {
        super(playlist);
        this.playlist = playlist;
        this.posPlaylistMappingDOS = posPlaylistMappingDOS;
        this.action = action;
    }

    public PlaylistDO getPlaylist() {
        return playlist;
    }

    public void setPlaylist(PlaylistDO playlist) {
        this.playlist = playlist;
    }

    public List<PosPlaylistMappingDO> getPosPlaylistMappingDOS() {
        return posPlaylistMappingDOS;
    }

    public void setPosPlaylistMappingDOS(
            List<PosPlaylistMappingDO> posPlaylistMappingDOS) {
        this.posPlaylistMappingDOS = posPlaylistMappingDOS;
    }

    public int getAction() {
        return action;
    }

    public void setAction(int action) {
        this.action = action;
    }
}
