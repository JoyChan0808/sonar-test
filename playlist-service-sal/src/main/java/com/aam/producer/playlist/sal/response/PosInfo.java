package com.aam.producer.playlist.sal.response;


import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class PosInfo implements Serializable {

    private static final long serialVersionUID = -1L;

    private String posUuid;

    private String titleUuid;

    private String screenUuid;

    private String playlistUuid;

    private String state;

    private String title;

    private String complexId;

    private Long showStart;

    private List<String> showAttrList = new ArrayList<>();

    private FilmHallInfo filmHallInfo;

    private String language;

    private List<String> unmatchedShowAttributes = new ArrayList<>();

    private String addressCountry;

    private String rating;

    public String getScreenUuid() {
        return screenUuid;
    }

    public void setScreenUuid(String screenUuid) {
        this.screenUuid = screenUuid;
    }

    public String getPosUuid() {
        return posUuid;
    }

    public void setPosUuid(String posUuid) {
        this.posUuid = posUuid;
    }

    public String getTitleUuid() {
        return titleUuid;
    }

    public void setTitleUuid(String titleUuid) {
        this.titleUuid = titleUuid;
    }

    public FilmHallInfo getFilmHallInfo() {
        return filmHallInfo;
    }

    public void setFilmHallInfo(FilmHallInfo filmHallInfo) {
        this.filmHallInfo = filmHallInfo;
    }

    public String getPlaylistUuid() {
        return playlistUuid;
    }

    public void setPlaylistUuid(String playlistUuid) {
        this.playlistUuid = playlistUuid;
    }

    public String getComplexId() {
        return complexId;
    }

    public void setComplexId(String complexId) {
        this.complexId = complexId;
    }

    public Long getShowStart() {
        return showStart;
    }

    public void setShowStart(Long showStart) {
        this.showStart = showStart;
    }

    public List<String> getShowAttrList() {
        return showAttrList;
    }

    public void setShowAttrList(List<String> showAttrList) {
        this.showAttrList = showAttrList;
    }

    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }

    public String getState() {
        return state;
    }

    public void setState(String state) {
        this.state = state;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public List<String> getUnmatchedShowAttributes() {
        return unmatchedShowAttributes;
    }

    public void setUnmatchedShowAttributes(List<String> unmatchedShowAttributes) {
        this.unmatchedShowAttributes = unmatchedShowAttributes;
    }

    public String getAddressCountry() {
        return addressCountry;
    }

    public void setAddressCountry(String addressCountry) {
        this.addressCountry = addressCountry;
    }

    public String getRating() {
        return rating;
    }

    public void setRating(String rating) {
        this.rating = rating;
    }
}
