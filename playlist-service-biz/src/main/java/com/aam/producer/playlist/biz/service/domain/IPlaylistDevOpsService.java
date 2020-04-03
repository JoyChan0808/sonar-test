package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.ComplexFlmDTO;

/**
 * dev ops service
 *
 * @author oliver.lo
 * @since 2019-11-27 15:24
 */
public interface IPlaylistDevOpsService {

    /**
     * complex change org,remove cache and update new org to pos and tpl
     *
     * @param complexId complex uuid
     */
    void handleComplexChangeOrg(String complexId);

    /**
     * 1.playlist send task
     * 2.playlist sync task
     * 3.pos mapping send task
     */
    void handleTimeoutTask();

    /**
     * check complex org
     *
     * @param dto complex flm
     */
    void handleComplexFlmData(ComplexFlmDTO dto);
}
