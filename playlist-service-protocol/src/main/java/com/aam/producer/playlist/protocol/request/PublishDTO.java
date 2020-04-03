package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Calendar;
import java.util.concurrent.CountDownLatch;

public class PublishDTO {

    @JSONField(name = "publish_later")
    @JsonProperty("publish_later")
    private Boolean publishLater;

    @JSONField(name = "publish_time")
    @JsonProperty("publish_time")
    private Long publishTime;

    @JSONField(name = "time_zone")
    @JsonProperty("time_zone")
    private String timeZone;
/*

    @JSONField(name = "specially_shows")
    @JsonProperty("specially_shows")
    private boolean speciallyShows;

    private List<String> shows;
*/
    private Boolean onlyUpdateStatus;

    private Boolean deletePreWeek;

    private Calendar publishCal;

    private CountDownLatch countDownLatch;

    public Boolean getPublishLater() {
        return publishLater;
    }

    public void setPublishLater(Boolean publishLater) {
        this.publishLater = publishLater;
    }

    public Long getPublishTime() {
        return publishTime;
    }

    public void setPublishTime(Long publishTime) {
        this.publishTime = publishTime;
    }

    public String getTimeZone() {
        return timeZone;
    }

    public void setTimeZone(String timeZone) {
        this.timeZone = timeZone;
    }

    public Boolean getOnlyUpdateStatus() {
        return onlyUpdateStatus;
    }

    public void setOnlyUpdateStatus(Boolean onlyUpdateStatus) {
        this.onlyUpdateStatus = onlyUpdateStatus;
    }

    public Boolean getDeletePreWeek() {
        return deletePreWeek;
    }

    public void setDeletePreWeek(Boolean deletePreWeek) {
        this.deletePreWeek = deletePreWeek;
    }

    public Calendar getPublishCal() {
        return publishCal;
    }

    public void setPublishCal(Calendar publishCal) {
        this.publishCal = publishCal;
    }

    public CountDownLatch getCountDownLatch() {
        return countDownLatch;
    }

    public void setCountDownLatch(CountDownLatch countDownLatch) {
        this.countDownLatch = countDownLatch;
    }
}
