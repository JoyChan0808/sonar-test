package com.aam.producer.playlist.sal.response;

import java.util.ArrayList;
import java.util.List;

public class ComplexGroup {
    private String uuid;
    private String name;
    private List<Complex> complexes = new ArrayList<>();

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

    public List<Complex> getComplexes() {
        return complexes;
    }

    public void setComplexes(List<Complex> complexes) {
        this.complexes = complexes;
    }
}
