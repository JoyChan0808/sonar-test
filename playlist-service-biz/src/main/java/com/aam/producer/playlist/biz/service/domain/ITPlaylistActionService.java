package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.biz.model.PlaylistPublishModel;
import com.aam.producer.playlist.protocol.response.TmsPlaylistInfo;

/**
 * playlist action service
 *
 * @author oliver.lo
 * @since 2019/7/8 6:41 PM
 */
public interface ITPlaylistActionService {

    /**
     * get tpl
     *
     * @param playlistUuid playlist uuid
     * @param complexId complex id
     * @return playlist info
     */
    TmsPlaylistInfo getByPlaylistUuid(String playlistUuid, String complexId);

    /**
     * handle playlist publish
     * 1.save tpl in db
     *
     * @param publishModel publish model
     */
    void handleTplPublish(PlaylistPublishModel publishModel);

    /**
     * handle playlist un-publish
     * 1.delete tpl in db
     * 2.send playlist.deletion.data
     *
     * @param publishModel publish model
     */
    void handleTplUnPublish(PlaylistPublishModel publishModel);

    /**
     * change tpl title
     *
     * @param publishModel publish model
     */
    void handleTplTitleChange(PlaylistPublishModel publishModel);

    /**
     * pos match title,and then pos match tpl
     *
     * @param model publish model
     */
    void handlePosMatch(PlaylistPublishModel model);
}
