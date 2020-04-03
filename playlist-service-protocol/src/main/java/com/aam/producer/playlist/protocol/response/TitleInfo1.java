package com.aam.producer.playlist.protocol.response;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonProperty;

public class TitleInfo1 {
    @JSONField(name = "movie_image")
    @JsonProperty("movie_image")
    private String movieImage;

    @JSONField(name = "title_name")
    @JsonProperty("title_name")
    private String titleName;


    @JSONField(name = "title_uuid")
    @JsonProperty("title_uuid")
    private String titleUuid;


    @JSONField(name = "ppl_name")
    @JsonProperty("ppl_name")
    private String pplName;

    @JSONField(name = "segment_name")
    @JsonProperty("segment_name")
    private String segmentName;

    @JSONField(name = "segment_uuid")
    @JsonProperty("segment_uuid")
    private String segmentUuid;

    @JSONField(name = "issue_type")
    @JsonProperty("issue_type")
    private String issueType;

    @JSONField(name = "issue_level")
    @JsonProperty("issue_level")
    private Integer issueLevel;

    public String getIssueType() {
        return issueType;
    }

    public void setIssueType(String issueType) {
        this.issueType = issueType;
    }

    public Integer getIssueLevel() {
        return issueLevel;
    }

    public void setIssueLevel(Integer issueLevel) {
        this.issueLevel = issueLevel;
    }

    public String getMovieImage() {
        return movieImage;
    }

    public void setMovieImage(String movieImage) {
        this.movieImage = movieImage;
    }

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

    public String getSegmentUuid() {
        return segmentUuid;
    }

    public void setSegmentUuid(String segmentUuid) {
        this.segmentUuid = segmentUuid;
    }

    public String getTitleUuid() {
        return titleUuid;
    }

    public void setTitleUuid(String titleUuid) {
        this.titleUuid = titleUuid;
    }
}
