package com.aam.producer.playlist.protocol.response;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.ArrayList;
import java.util.List;

public class TitleInfo {

    @JSONField(name = "movie_image")
    @JsonProperty("movie_image")
    private String movieImage;

    @JSONField(name = "title_name")
    @JsonProperty("title_name")
    private String titleName;

    @JSONField(name = "ppl_name")
    @JsonProperty("ppl_name")
    private String pplName;

    @JSONField(name = "segment_name")
    @JsonProperty("segment_name")
    private String segmentName;

    @JSONField(name = "segment_info")
    @JsonProperty("segment_info")
    private String segmentInfo;

    @JSONField(name = "ppl_uuid")
    @JsonProperty("ppl_uuid")
    private String pplUuid;

    @JSONField(name = "automatically_apply")
    @JsonProperty("automatically_apply")
    private Boolean automaticallyApply;

    @JSONField(name = "segment_split_infos")
    @JsonProperty("segment_split_infos")
    private List<SegmentSplitInfo> segmentSplitInfos = new ArrayList<>();

    public String getTitleName() {
        return titleName;
    }

    public void setTitleName(String titleName) {
        this.titleName = titleName;
    }

    public String getPplName() {
        return pplName;
    }

    public void setPplName(String pplName) {
        this.pplName = pplName;
    }

    public String getSegmentName() {
        return segmentName;
    }

    public void setSegmentName(String segmentName) {
        this.segmentName = segmentName;
    }

    public String getPplUuid() {
        return pplUuid;
    }

    public void setPplUuid(String pplUuid) {
        this.pplUuid = pplUuid;
    }

    public List<SegmentSplitInfo> getSegmentSplitInfos() {
        return segmentSplitInfos;
    }

    public void setSegmentSplitInfos(List<SegmentSplitInfo> segmentSplitInfos) {
        this.segmentSplitInfos = segmentSplitInfos;
    }

    public String getMovieImage() {
        return movieImage;
    }

    public void setMovieImage(String movieImage) {
        this.movieImage = movieImage;
    }

    public String getSegmentInfo() {
        return segmentInfo;
    }

    public void setSegmentInfo(String segmentInfo) {
        this.segmentInfo = segmentInfo;
    }

    public Boolean getAutomaticallyApply() {
        return automaticallyApply;
    }

    public void setAutomaticallyApply(Boolean automaticallyApply) {
        this.automaticallyApply = automaticallyApply;
    }
}
