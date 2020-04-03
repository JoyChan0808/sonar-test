package com.aam.producer.playlist.protocol.message;

public class SegmentWarningDTO {

    private String pplUuid;

    private String titleUuid;

    private String contentAssociationUuid;

    private String segmentType;

    private String pplTitle;

    private String taskType;

    private Boolean deleted = false;

    public String getSegmentType() {
        return segmentType;
    }

    public void setSegmentType(String segmentType) {
        this.segmentType = segmentType;
    }

    public String getTitleUuid() {
        return titleUuid;
    }

    public void setTitleUuid(String titleUuid) {
        this.titleUuid = titleUuid;
    }

    public String getContentAssociationUuid() {
        return contentAssociationUuid;
    }

    public void setContentAssociationUuid(String contentAssociationUuid) {
        this.contentAssociationUuid = contentAssociationUuid;
    }

    public String getPplTitle() {
        return pplTitle;
    }

    public void setPplTitle(String pplTitle) {
        this.pplTitle = pplTitle;
    }

    public String getTaskType() {
        return taskType;
    }

    public void setTaskType(String taskType) {
        this.taskType = taskType;
    }

    public Boolean getDeleted() {
        return deleted;
    }

    public void setDeleted(Boolean deleted) {
        this.deleted = deleted;
    }

    public String getPplUuid() {
        return pplUuid;
    }

    public void setPplUuid(String pplUuid) {
        this.pplUuid = pplUuid;
    }

    @Override
    public String toString() {
        return "SegmentWarningDTO{" +
                "pplUuid='" + pplUuid + '\'' +
                ", titleUuid='" + titleUuid + '\'' +
                ", contentAssociationUuid='" + contentAssociationUuid + '\'' +
                ", pplTitle='" + pplTitle + '\'' +
                ", taskType='" + taskType + '\'' +
                ", deleted=" + deleted +
                '}';
    }
}
