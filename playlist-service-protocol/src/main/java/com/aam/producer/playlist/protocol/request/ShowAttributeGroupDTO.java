package com.aam.producer.playlist.protocol.request;

import java.util.List;

public class ShowAttributeGroupDTO {

    private String name;

    private List<String> attributes;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<String> getAttributes() {
        return attributes;
    }

    public void setAttributes(List<String> attributes) {
        this.attributes = attributes;
    }
}
