package com.aam.producer.playlist.biz.enums;

/**
 * com.aam.producer.playlist.biz.enums
 *
 * @author oliver.lo
 * @since 2019-09-24 10:25
 */
public enum TPlaylistTaskActionEnum {

    PUBLISH(1),
    MATCH(2),
    NEW_CONTENT(3),
    CONTENT_CHANGED(4),
    PUBLISH_SEGMENT(5),
    PPL_TITLE_NAME_CHANGED(6),
    ;

    private int code;

    TPlaylistTaskActionEnum(int code) {
        this.code = code;
    }

    public int getCode() {
        return code;
    }
}
