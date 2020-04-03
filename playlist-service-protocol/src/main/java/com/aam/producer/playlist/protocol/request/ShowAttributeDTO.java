package com.aam.producer.playlist.protocol.request;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;

public class ShowAttributeDTO {

    @NotBlank(message = "{title.notBlank}")
    @Size(max = 100, message = "{title.outOfSize}")
    private String title;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }
}
