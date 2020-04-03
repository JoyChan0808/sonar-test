package com.aam.producer.playlist.sal.client.impl;

import com.aam.producer.playlist.protocol.request.PosMappingDTO;
import com.aam.producer.playlist.sal.client.IViewFacadeClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

/**
 * producer view facade client impl
 *
 * @author oliver.lo
 * @since 2019-09-20 11:36
 */
@Service
public class ViewFacadeClientImpl implements IViewFacadeClient {

    private static final Logger logger = LoggerFactory.getLogger(ViewFacadeClientImpl.class);
    private final RestTemplate restTemplate;
    @Value("${thunderstorm.producerViewService.sendPosMapping}")
    private String posMappingUri;

    @Autowired
    public ViewFacadeClientImpl(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    public void sendPosMapping(PosMappingDTO dto) {
        try {
            restTemplate.postForObject(posMappingUri, dto, String.class);
        } catch (Exception e) {
            logger.error("send pos and ppl mapping to producer_view error,msg:<{}>", e.getMessage(),
                    e);
        }
    }
}
