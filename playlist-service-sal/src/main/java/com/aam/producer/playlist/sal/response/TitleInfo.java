package com.aam.producer.playlist.sal.response;

import com.aam.producer.playlist.protocol.response.PlaylistInfo;
import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;

public class TitleInfo {

    @JSONField(name = "movie_image")
    @JsonProperty("movie_image")
    private String movieImage;

    private String uuid;

    private String name;

    private List<CplInfo> cplDTOList = new ArrayList<>();

    private List<PlaylistInfo> playlistInfos = new ArrayList<>();

    @JSONField(name = "cpl_uuids")
    private List<String> cplUuids = new ArrayList<>();

    private List<String> showAttrs = new ArrayList<>();

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<CplInfo> getCplDTOList() {
        return cplDTOList;
    }

    public void setCplDTOList(List<CplInfo> cplDTOList) {
        this.cplDTOList = cplDTOList;
    }

    public List<String> getCplUuids() {
        return cplUuids;
    }

    public void setCplUuids(List<String> cplUuids) {
        this.cplUuids = cplUuids;
    }

    public List<String> getShowAttrs() {
        return showAttrs;
    }

    public void setShowAttrs(List<String> showAttrs) {
        this.showAttrs = showAttrs;
    }

    public String getMovieImage() {
        return movieImage;
    }

    public void setMovieImage(String movieImage) {
        this.movieImage = movieImage;
    }

    public List<PlaylistInfo> getPlaylistInfos() {
        return playlistInfos;
    }

    public void setPlaylistInfos(
            List<PlaylistInfo> playlistInfos) {
        this.playlistInfos = playlistInfos;
    }
}
