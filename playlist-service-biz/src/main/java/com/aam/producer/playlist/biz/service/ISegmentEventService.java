package com.aam.producer.playlist.biz.service;

public interface ISegmentEventService {

    void sendDataEvent(String uuid, boolean deleted);

}
