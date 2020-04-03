package com.aam.producer.playlist.protocol.request;

import java.util.List;

public class SegmentSearchDTO {

    private String title;

    private List<Integer> types;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public List<Integer> getTypes() {
        return types;
    }

    public void setTypes(List<Integer> types) {
        this.types = types;
    }
}
