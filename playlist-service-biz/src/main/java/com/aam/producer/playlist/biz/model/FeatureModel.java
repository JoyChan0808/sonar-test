package com.aam.producer.playlist.biz.model;

import com.alibaba.fastjson.annotation.JSONField;
import java.util.List;

public class FeatureModel {

    @JSONField(name = "producer_ratings")
    private List<RatingItem> producerRatings;

    public List<RatingItem> getProducerRatings() {
        return producerRatings;
    }

    public void setProducerRatings(
            List<RatingItem> producerRatings) {
        this.producerRatings = producerRatings;
    }
}
