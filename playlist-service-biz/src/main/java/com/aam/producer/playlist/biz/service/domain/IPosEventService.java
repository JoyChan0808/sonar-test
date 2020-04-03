package com.aam.producer.playlist.biz.service.domain;


import com.aam.producer.playlist.protocol.request.PosDataDTO;
import com.aam.producer.playlist.protocol.request.TitleDataDTO;

public interface IPosEventService {

    void posDataHandler(PosDataDTO dto);

    void titleDataHandler(TitleDataDTO dto);
}
