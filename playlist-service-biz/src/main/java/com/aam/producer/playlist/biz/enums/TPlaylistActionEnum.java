package com.aam.producer.playlist.biz.enums;

/**
 * playlist sync method enum
 *
 * @author oliver.lo
 * @since 2019/4/10 9:34 AM
 */
public enum TPlaylistActionEnum {
    CREATE(1),
    UPDATE(2),
    ;

    private int code;

    TPlaylistActionEnum(int code) {
        this.code = code;
    }

    public static TPlaylistActionEnum getByCode(Integer code) {
        switch (code) {
            case 1:
                return CREATE;
            case 2:
                return UPDATE;
            default:
                return null;
        }
    }

    public int getCode() {
        return code;
    }
}
