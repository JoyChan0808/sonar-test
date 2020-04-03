package com.aam.producer.playlist.biz.event;

import org.springframework.context.ApplicationEvent;

public class OnSegmentSplitWeekChanged extends ApplicationEvent {

    private Boolean splitByWeek;

    public OnSegmentSplitWeekChanged(Object source,Boolean splitByWeek) {
        super(source);
        this.splitByWeek = splitByWeek;
    }

    public Boolean getSplitByWeek() {
        return splitByWeek;
    }

    public void setSplitByWeek(Boolean splitByWeek) {
        this.splitByWeek = splitByWeek;
    }
}
