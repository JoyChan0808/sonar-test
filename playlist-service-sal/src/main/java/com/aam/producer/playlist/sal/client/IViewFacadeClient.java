package com.aam.producer.playlist.sal.client;

import com.aam.producer.playlist.protocol.request.PosMappingDTO;

/**
 * producer view facade client
 *
 * @author oliver.lo
 * @since 2019-09-20 11:34
 */
public interface IViewFacadeClient {

    void sendPosMapping(PosMappingDTO dto);
}
