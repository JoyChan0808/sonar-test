package com.aam.producer.playlist.biz.enums;

/**
 * com.aam.producer.playlist.biz.enums
 *
 * @author oliver.lo
 * @since 2019-09-17 11:00
 */
public enum AAMSysEnum {

    PRODUCER(1),
    TMS(2);

    private int code;

    AAMSysEnum(int code) {
        this.code = code;
    }

    public static AAMSysEnum getByCode(Integer source) {
        if (source == null) {
            return null;
        }
        switch (source) {
            case 1:
                return PRODUCER;
            case 2:
                return TMS;
            default:
                return null;
        }
    }

    public int getCode() {
        return this.code;
    }
}
