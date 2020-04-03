package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class PlaylistDTO {

    @NotBlank(message = "{title.notBlank}")
    @Size(max = 100, message = "{title.outOfSize}")
    private String title;

    @JSONField(name = "show_attribute_groups")
    @JsonProperty("show_attribute_groups")
    private List<ShowAttributeGroupDTO> showAttributeGroups = new ArrayList<>();

    @NotNull(message = "{automaticallyApply.notBlank}")
    @JSONField(name = "automatically_apply")
    @JsonProperty("automatically_apply")
    private Boolean automaticallyApply;

    @JSONField(name = "draft")
    @JsonProperty("draft")
    private Boolean draft = true;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public List<ShowAttributeGroupDTO> getShowAttributeGroups() {
        return showAttributeGroups;
    }

    public void setShowAttributeGroups(List<ShowAttributeGroupDTO> showAttributeGroups) {
        this.showAttributeGroups = showAttributeGroups;
    }

    public Boolean getAutomaticallyApply() {
        return automaticallyApply;
    }

    public void setAutomaticallyApply(Boolean automaticallyApply) {
        this.automaticallyApply = automaticallyApply;
    }

    public Boolean getDraft() {
        return draft;
    }

    public void setDraft(Boolean draft) {
        this.draft = draft;
    }
}
