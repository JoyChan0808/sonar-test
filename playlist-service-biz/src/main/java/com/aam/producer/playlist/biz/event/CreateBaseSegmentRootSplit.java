package com.aam.producer.playlist.biz.event;

import org.springframework.context.ApplicationEvent;

public class CreateBaseSegmentRootSplit extends ApplicationEvent {

    private Boolean splitByWeek;

    public CreateBaseSegmentRootSplit(String segmentUuid,Boolean splitByWeek) {
        super(segmentUuid);
        this.splitByWeek = splitByWeek;
    }

    public Boolean getSplitByWeek() {
        return splitByWeek;
    }

}
