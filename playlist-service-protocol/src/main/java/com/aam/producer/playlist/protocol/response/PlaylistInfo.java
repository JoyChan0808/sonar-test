package com.aam.producer.playlist.protocol.response;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 *
 * </p>
 *
 * @author ${author}
 * @since 2019-05-14
 */
public class PlaylistInfo {

    private String uuid;

    private String title;

    @JSONField(name = "automatically_apply")
    @JsonProperty("automatically_apply")
    private Boolean automaticallyApply;

    private Long created;

    @JSONField(name = "last_modified")
    @JsonProperty("last_modified")
    private Long lastModified;

    @JSONField(name = "versions")
    @JsonProperty("versions")
    private List<PlaylistVersionInfo> versions = new ArrayList<>();

    @JSONField(name = "status")
    @JsonProperty("status")
    private String status;

    public String getUuid() {
        return uuid;
    }

    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Boolean getAutomaticallyApply() {
        return automaticallyApply;
    }

    public void setAutomaticallyApply(Boolean automaticallyApply) {
        this.automaticallyApply = automaticallyApply;
    }

    public Long getCreated() {
        return created;
    }

    public void setCreated(Long created) {
        this.created = created;
    }

    public Long getLastModified() {
        return lastModified;
    }

    public void setLastModified(Long lastModified) {
        this.lastModified = lastModified;
    }

    public List<PlaylistVersionInfo> getVersions() {
        return versions;
    }

    public void setVersions(List<PlaylistVersionInfo> versions) {
        this.versions = versions;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

}
