package com.aam.producer.playlist.biz.enums;

import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;

public enum TimeZoneEnum {

    SITE(1, "site"), LOCAL(2, "local");

    private Integer code;

    private String name;

    TimeZoneEnum(Integer code, String name) {
        this.code = code;
        this.name = name;
    }

    public static TimeZoneEnum getByCode(int code) {
        switch (code) {
            case 1:
                return SITE;
            case 2:
                return LOCAL;
            default:
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    public static TimeZoneEnum getByName(String name) {
        switch (name) {
            case "site":
                return SITE;
            case "local":
                return LOCAL;
            default:
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    public Integer getCode() {
        return code;
    }

    public String getName() {
        return name;
    }

}
