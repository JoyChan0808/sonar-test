package com.aam.producer.playlist.biz.event;

import org.springframework.context.ApplicationEvent;

public class ChangePplReleaseVersionEvent extends ApplicationEvent {

    private String oldVersionUuid;

    private String newVersionUuid;

    public ChangePplReleaseVersionEvent(String oldVersionUuid, String newVersionUuid) {
        super(newVersionUuid);
        this.newVersionUuid = newVersionUuid;
        this.oldVersionUuid = oldVersionUuid;
    }

    public String getOldVersionUuid() {
        return oldVersionUuid;
    }

    public void setOldVersionUuid(String oldVersionUuid) {
        this.oldVersionUuid = oldVersionUuid;
    }

    public String getNewVersionUuid() {
        return newVersionUuid;
    }

    public void setNewVersionUuid(String newVersionUuid) {
        this.newVersionUuid = newVersionUuid;
    }
}
