package com.aam.producer.playlist.biz.model;

import com.aam.producer.playlist.biz.enums.AutomationEnum;
import com.alibaba.fastjson.annotation.JSONField;
import java.util.List;
import java.util.Objects;

/**
 * composition model
 *
 * @author oliver.lo
 * @since 2019-07-24 16:39
 */
public class CompositionModel extends TPlaylistEventModel {

    @JSONField(name = "frame_rate")
    private Integer frameRate = 24;

    @JSONField(name = "producer_intermission")
    private Integer intermission = 0;

    @JSONField(name = "producer_credit_offset")
    private Integer creditOffset = 0;

    private List<AutomationModel> automation;

    @JSONField(name = "producer_ratings")
    private List<Rating> producerRatings;

    @JSONField(name = "rating_hardlocked")
    private Boolean ratingHardLocked;

    @JSONField(name = "playback_mode")
    private String playbackMode;

    public Integer getFrameRate() {
        return frameRate;
    }

    public void setFrameRate(Integer frameRate) {
        this.frameRate = frameRate;
    }

    public Integer getIntermission() {
        return intermission;
    }

    public void setIntermission(Integer intermission) {
        this.intermission = intermission;
    }

    public Integer getCreditOffset() {
        return creditOffset;
    }

    public void setCreditOffset(Integer creditOffset) {
        this.creditOffset = creditOffset;
    }

    public List<AutomationModel> getAutomation() {
        return automation;
    }

    public void setAutomation(List<AutomationModel> automation) {
        this.automation = automation;
    }

    public Boolean getRatingHardLocked() {
        return ratingHardLocked;
    }

    public void setRatingHardLocked(Boolean ratingHardLocked) {
        this.ratingHardLocked = ratingHardLocked;
    }

    public List<Rating> getProducerRatings() {
        return producerRatings;
    }

    public void setProducerRatings(
            List<Rating> producerRatings) {
        this.producerRatings = producerRatings;
    }

    public String getPlaybackMode() {
        return playbackMode;
    }

    public void setPlaybackMode(String playbackMode) {
        this.playbackMode = playbackMode;
    }

    public Integer getSecondsByAutomation(AutomationEnum automationEnum) {
        if (automationEnum == null) {
            return 0;
        }
        switch (automationEnum) {
            case CREDIT_OFFSET:
                return creditOffset;
            case INTERMISSION:
                return intermission;
            default:
                return 0;
        }
    }

    public static class Rating {

        private String territory;
        private String rating;

        @JSONField(name = "is_rating_in_list")
        private Boolean registered;

        public String getTerritory() {
            return territory;
        }

        public void setTerritory(String territory) {
            this.territory = territory;
        }

        public String getRating() {
            return rating;
        }

        public void setRating(String rating) {
            this.rating = rating;
        }

        public Boolean getRegistered() {
            return registered;
        }

        public void setRegistered(Boolean registered) {
            this.registered = registered;
        }

        @Override
        public int hashCode() {
            return Objects.hash(territory, rating);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null || getClass() != obj.getClass()) {
                return false;
            }
            if (this == obj) {
                return true;
            }
            Rating that = (Rating) obj;
            return that.getTerritory().equals(territory) && that.getRating().equals(rating);
        }
    }
}
