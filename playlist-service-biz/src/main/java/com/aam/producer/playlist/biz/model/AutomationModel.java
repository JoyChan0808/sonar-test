package com.aam.producer.playlist.biz.model;

import com.alibaba.fastjson.annotation.JSONField;

/**
 * automation
 *
 * @author oliver.lo
 * @since 2019-08-20 16:36
 */
public class AutomationModel {

    private String name;

    private String type;

    @JSONField(name = "type_specific")
    private Specific specific;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Specific getSpecific() {
        return specific;
    }

    public void setSpecific(
            Specific specific) {
        this.specific = specific;
    }

    public static class Specific {

        @JSONField(name = "offset_in_seconds")
        private Integer seconds;

        @JSONField(name = "offset_in_frames")
        private Long frames;

        public Integer getSeconds() {
            return seconds;
        }

        public void setSeconds(Integer seconds) {
            this.seconds = seconds;
        }

        public Long getFrames() {
            return frames;
        }

        public void setFrames(Long frames) {
            this.frames = frames;
        }
    }
}
