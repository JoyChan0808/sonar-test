package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;

public class PlaylistVersionDTO {

    @JSONField(name = "playlist_uuid")
    @JsonProperty("playlist_uuid")
    private String playlistUuid;

    @JSONField(name = "content_list")
    @JsonProperty("content_list")
    private List<ContentDTO> contentList = new ArrayList<>();

    @JSONField(name = "show_attribute_groups")
    @JsonProperty("show_attribute_groups")
    private List<ShowAttributeGroupDTO> showAttributeGroups = new ArrayList<>();

    public List<ContentDTO> getContentList() {
        return contentList;
    }

    public void setContentList(List<ContentDTO> contentList) {
        this.contentList = contentList;
    }

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }

    public List<ShowAttributeGroupDTO> getShowAttributeGroups() {
        return showAttributeGroups;
    }

    public void setShowAttributeGroups(
            List<ShowAttributeGroupDTO> showAttributeGroups) {
        this.showAttributeGroups = showAttributeGroups;
    }
}
