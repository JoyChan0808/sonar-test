package com.aam.producer.playlist.protocol.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;

public class PplFilterDTO {

    private String title;
    @JsonProperty("title_uuid")
    private String titleUuid;
    @JsonProperty("content_id")
    private String contentId;
    private List<Integer> status;
    private List<Integer> types;
    private List<Integer> shows;
    @JsonProperty("order_by_name")
    private String orderByName;
    @JsonProperty("order_by_desc")
    private boolean orderByDesc;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getContentId() {
        return contentId;
    }

    public void setContentId(String contentId) {
        this.contentId = contentId;
    }

    public List<Integer> getStatus() {
        return status;
    }

    public void setStatus(List<Integer> status) {
        this.status = status;
    }

    public List<Integer> getTypes() {
        return types;
    }

    public void setTypes(List<Integer> types) {
        this.types = types;
    }

    public List<Integer> getShows() {
        return shows;
    }

    public void setShows(List<Integer> shows) {
        this.shows = shows;
    }

    public String getOrderByName() {
        return orderByName;
    }

    public void setOrderByName(String orderByName) {
        this.orderByName = orderByName;
    }

    public boolean isOrderByDesc() {
        return orderByDesc;
    }

    public void setOrderByDesc(boolean orderByDesc) {
        this.orderByDesc = orderByDesc;
    }

    public String getTitleUuid() {
        return titleUuid;
    }

    public void setTitleUuid(String titleUuid) {
        this.titleUuid = titleUuid;
    }
}
