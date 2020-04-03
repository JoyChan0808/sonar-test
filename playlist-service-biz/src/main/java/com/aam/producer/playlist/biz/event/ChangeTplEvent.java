package com.aam.producer.playlist.biz.event;

import org.springframework.context.ApplicationEvent;

public class ChangeTplEvent extends ApplicationEvent {

    public ChangeTplEvent(String pplUuid) {
        super(pplUuid);

    }

}
