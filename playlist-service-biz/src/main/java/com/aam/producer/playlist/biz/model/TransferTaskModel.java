package com.aam.producer.playlist.biz.model;

import java.util.HashSet;
import java.util.Set;

/**
 * sync/send task model
 *
 * @author oliver.lo
 * @since 2019-07-26 11:03
 */
public class TransferTaskModel {

    private Integer triggerNum;

    private Long latestTriggerTime;

    private Set<String> triggerUuidList = new HashSet<>();

    private Long created;

    public Integer getTriggerNum() {
        return triggerNum;
    }

    public void setTriggerNum(Integer triggerNum) {
        this.triggerNum = triggerNum;
    }

    public Long getLatestTriggerTime() {
        return latestTriggerTime;
    }

    public void setLatestTriggerTime(Long latestTriggerTime) {
        this.latestTriggerTime = latestTriggerTime;
    }

    public Set<String> getTriggerUuidList() {
        return triggerUuidList;
    }

    public void setTriggerUuidList(Set<String> triggerUuidList) {
        this.triggerUuidList = triggerUuidList;
    }

    public Long getCreated() {
        return created;
    }

    public void setCreated(Long created) {
        this.created = created;
    }

    public void increaseTriggerNum() {
        if ((triggerNum == null)) {
            triggerNum = 1;
        } else {
            triggerNum = triggerNum + 1;
        }
    }
}
