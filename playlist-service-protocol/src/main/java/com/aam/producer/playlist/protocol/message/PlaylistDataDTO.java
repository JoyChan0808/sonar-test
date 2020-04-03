package com.aam.producer.playlist.protocol.message;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.Map;
import sun.management.resources.agent;

public class PlaylistDataDTO {

    @JSONField(name = "complex_uuid")
    @JsonProperty("complex_uuid")
    private String complexUuid;
    @JSONField(name = "device_uuid")
    @JsonProperty("device_uuid")
    private String deviceUuid;
    @JSONField(name = "playlist_Uuid")
    @JsonProperty("playlist_uuid")
    private String playlistUuid;
    @JSONField(name = "playlist")
    @JsonProperty("playlist")
    private PlaylistDTO playlist;

    public static class PlaylistDTO {

        @JSONField(name = "uuid")
        @JsonProperty("uuid")
        private String uuid;
        @JSONField(name = "clean")
        @JsonProperty("clean")
        private Boolean clean;
        @JSONField(name = "content_ids")
        @JsonProperty("content_ids")
        private List<String> contentIds;
        @JSONField(name = "duration_in_seconds")
        @JsonProperty("duration_in_seconds")
        private Float durationInSeconds;
        @JSONField(name = "is_3d")
        @JsonProperty("is_3d")
        private Boolean is3d;
        @JSONField(name = "is_4k")
        @JsonProperty("is_4k")
        private Boolean is4k;
        @JSONField(name = "is_hfr")
        @JsonProperty("is_hfr")
        private Boolean isHfr;
        @JSONField(name = "is_template")
        @JsonProperty("is_template")
        private Boolean isTemplate;
        @JSONField(name = "playlist")
        @JsonProperty("playlist")
        private Object playlist;
        @JSONField(name = "playlist_ids")
        @JsonProperty("playlist_ids")
        private List<String> playlistIds;
        @JSONField(name = "preshow_duration")
        @JsonProperty("preshow_duration")
        private Float preshowDuration;
        @JSONField(name = "title")
        @JsonProperty("title")
        private String title;
        @JSONField(name = "total_duration_in_seconds")
        @JsonProperty("total_duration_in_seconds")
        private Float totalDurationInSeconds;
        @JSONField(name = "device_uuid")
        @JsonProperty("device_uuid")
        private String deviceUuid;


        public String getUuid() {
            return uuid;
        }

        public void setUuid(String uuid) {
            this.uuid = uuid;
        }

        public Boolean getClean() {
            return clean;
        }

        public void setClean(Boolean clean) {
            this.clean = clean;
        }

        public List<String> getContentIds() {
            return contentIds;
        }

        public void setContentIds(List<String> contentIds) {
            this.contentIds = contentIds;
        }

        public Float getDurationInSeconds() {
            return durationInSeconds;
        }

        public void setDurationInSeconds(Float durationInSeconds) {
            this.durationInSeconds = durationInSeconds;
        }

        public Boolean getIs3d() {
            return is3d;
        }

        public void setIs3d(Boolean is3d) {
            this.is3d = is3d;
        }

        public Boolean getIs4k() {
            return is4k;
        }

        public void setIs4k(Boolean is4k) {
            this.is4k = is4k;
        }

        public Boolean getHfr() {
            return isHfr;
        }

        public void setHfr(Boolean hfr) {
            isHfr = hfr;
        }

        public Boolean getTemplate() {
            return isTemplate;
        }

        public void setTemplate(Boolean template) {
            isTemplate = template;
        }

        public Object getPlaylist() {
            return playlist;
        }

        public void setPlaylist(Object playlist) {
            this.playlist = playlist;
        }

        public List<String> getPlaylistIds() {
            return playlistIds;
        }

        public void setPlaylistIds(List<String> playlistIds) {
            this.playlistIds = playlistIds;
        }

        public Float getPreshowDuration() {
            return preshowDuration;
        }

        public void setPreshowDuration(Float preshowDuration) {
            this.preshowDuration = preshowDuration;
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        public Float getTotalDurationInSeconds() {
            return totalDurationInSeconds;
        }

        public void setTotalDurationInSeconds(Float totalDurationInSeconds) {
            this.totalDurationInSeconds = totalDurationInSeconds;
        }

        public String getDeviceUuid() {
            return deviceUuid;
        }

        public void setDeviceUuid(String deviceUuid) {
            this.deviceUuid = deviceUuid;
        }
    }

    public String getComplexUuid() {
        return complexUuid;
    }

    public void setComplexUuid(String complexUuid) {
        this.complexUuid = complexUuid;
    }

    public String getDeviceUuid() {
        return deviceUuid;
    }

    public void setDeviceUuid(String deviceUuid) {
        this.deviceUuid = deviceUuid;
    }

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }

    public PlaylistDTO getPlaylist() {
        return playlist;
    }

    public void setPlaylist(PlaylistDTO playlist) {
        this.playlist = playlist;
    }
}
