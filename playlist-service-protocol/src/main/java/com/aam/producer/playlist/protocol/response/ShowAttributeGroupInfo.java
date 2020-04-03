package com.aam.producer.playlist.protocol.response;

import java.util.List;
import java.util.Objects;

public class ShowAttributeGroupInfo {

    private String pplVersionUuid;

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

    public String getPplVersionUuid() {
        return pplVersionUuid;
    }

    public void setPplVersionUuid(String pplVersionUuid) {
        this.pplVersionUuid = pplVersionUuid;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (obj instanceof ShowAttributeGroupInfo) {
            ShowAttributeGroupInfo showAttributeGroupInfo = (ShowAttributeGroupInfo) obj;
            return Objects.equals(showAttributeGroupInfo.getName(), this.getName());
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return 1;
    }


}
