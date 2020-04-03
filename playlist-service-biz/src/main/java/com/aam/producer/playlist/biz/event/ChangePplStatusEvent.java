package com.aam.producer.playlist.biz.event;

import org.springframework.context.ApplicationEvent;

public class ChangePplStatusEvent extends ApplicationEvent {

    public ChangePplStatusEvent(String playlistUuid) {
        super(playlistUuid);
    }

}
