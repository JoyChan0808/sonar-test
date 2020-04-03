package com.aam.producer.playlist.biz.model;

import com.aam.producer.playlist.biz.model.PlaylistPublishModel.PlaylistContent;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel.SegmentSplit;
import java.util.ArrayList;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

/**
 * automatic playlist
 *
 * @author oliver.lo
 * @since 2019-07-16 15:01
 */
@Getter
@Setter
public class PlaylistFlatModel {

    private String organizationId;

    private String playlistUuid;

    private String pplUuid;

    private String pplVersionUuid;

    private Boolean automatic;

    private String pplTitle;

    private String tplTitle;

    private List<PlaylistContent> contents = new ArrayList<>();

    private List<SegmentSplit> splits = new ArrayList<>();

    private List<String> posIds = new ArrayList<>();
}
