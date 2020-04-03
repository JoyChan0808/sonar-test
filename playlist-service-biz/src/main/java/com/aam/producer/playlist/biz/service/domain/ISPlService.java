package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.SPLSignDTO;

public interface ISPlService {
    void requestSPL(SPLSignDTO splSignDTO);
}
