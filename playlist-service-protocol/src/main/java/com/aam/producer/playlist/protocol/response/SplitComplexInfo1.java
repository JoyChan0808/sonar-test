package com.aam.producer.playlist.protocol.response;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;
import java.util.Objects;

public class SplitComplexInfo1 {
    private String uuid;
    private String name;
    @JSONField(name = "group_names")
    @JsonProperty("group_names")
    private List<String> groupNames;
    private Boolean enable;

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

    public List<String> getGroupNames() {
        return groupNames;
    }

    public void setGroupNames(List<String> groupNames) {
        this.groupNames = groupNames;
    }

    public Boolean getEnable() {
        return enable;
    }

    public void setEnable(Boolean enable) {
        this.enable = enable;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        SplitComplexInfo1 that = (SplitComplexInfo1) o;
        return Objects.equals(uuid, that.uuid) &&
                Objects.equals(name, that.name) &&
                Objects.equals(groupNames, that.groupNames) &&
                Objects.equals(enable, that.enable);
    }

    @Override
    public int hashCode() {
        return Objects.hash(uuid, name, groupNames, enable);
    }
}
