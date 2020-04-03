package com.aam.producer.playlist.biz.enums;

import com.aam.utils.model.ResultCode;

public enum ResultCodeEnum implements ResultCode {

    RESOURCE_NOT_FOUND(404),
    DRAF_ALREADY_EXISTEDS(550),
    NOT_FIND_GROUP_CONTENT(551),
    REPETITION_EXISTS_TAGS(552),
    SHOW_TAGS_ALREADY_EXIST(554),
    TITLE_ALREADY_EXISTS(555),
    DELETE_SEGMENT_FAIL(556),
    REPETITION_OF_SEGMENT_NAMES(557),
    DELETE_SHOWATTR_FAIL(558),
    REPETITION_OF_SHOWATTRIBUTE_NAMES(559),
    MISSING_AUTOMATIC_SEGMENT(560),
    NOT_FIND_GROUP_NAME(561),
    STATIC_PPL_ONLY_APISEGMENT(562),
    CANNOT_MODIFY_APISEGMENT(563),
    ORGANIZATION_NOT_FIND(564),
    NOT_FIND_AUTOMATIC_SEGMENT(565),
    GROUP_NAME_REPETITION(567),
    NOT_FIND_PPL(568),
    POS_NOT_EXIST_OR_DELETED(569),
    NOT_FIND_TPL(570),
    PUBLISH_TIME_ERROR(571),
    INVALID_POS(572),
    POS_MATCH_TPL_FAILED(573),
    UNFINISHED_POS_MAPPING(574),
    UNFINISHED_PLAYLIST_SYNCHRONIZATION(575),
    REPETITION_OF_SEGMENT_SPLIT_TITLE(576),
    POS_DATA_UNRELIABLE(578);


    private int code;
    private String name;

    private ResultCodeEnum(int code) {
        this.code = code;
        this.name = this.name();
    }

    private ResultCodeEnum(int code, String name) {
        this.code = code;
        this.name = name;
    }

    public int getCode() {
        return this.code;
    }

    public String getName() {
        return this.name;
    }
}
