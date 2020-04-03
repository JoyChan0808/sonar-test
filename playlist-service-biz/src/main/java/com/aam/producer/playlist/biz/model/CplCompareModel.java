package com.aam.producer.playlist.biz.model;

import com.aam.producer.playlist.biz.model.CompositionModel.Rating;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;

/**
 * com.aam.producer.playlist.biz.model
 *
 * @author oliver.lo
 * @since 2020-02-13 22:18
 */
@Getter
@Setter
public class CplCompareModel {

    private List<CompareInfo> ratingsCache = new ArrayList<>();

    private List<CompareInfo> playbackModeCache = new ArrayList<>();

    private CompareInfo feature;

    @Getter
    @Setter
    public static class CompareInfo {

        private Boolean isFeature;

        private String contentUuid;

        private String contentText;

        private String segmentText;

        private String segmentType;

        private String redirectUuid;

        private int sortNum;

        private Map<String, Rating> ratingMap;

        private Boolean is3d;
    }
}
