package com.aam.producer.playlist.biz.enums;

import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;

public enum SegmentSplitTypeEnum {

    SITES(1, "sites"), SITE_GROUPS(2, "site_groups"), TIME(3, "time"),
    SHOWS(4, "shows"), SEATS(5, "seats"), TITLE(6, "title"),
    SHOW_ATTR(9, "show_attr"), CPL_FORMAT(10, "cpl_format"), ROOT(11, "root"),
    PERCENTAGE(12, "percentage"), DAY(13, "day"), DATE(14, "date"),
    RATING(15, "rating");

    private Integer code;

    private String name;

    SegmentSplitTypeEnum(Integer code, String name) {
        this.code = code;
        this.name = name;
    }

    public static SegmentSplitTypeEnum getSplitTypeByName(String name) {
        switch (name) {
            case "sites":
                return SITES;
            case "site_groups":
                return SITE_GROUPS;
            case "time":
                return TIME;
            case "shows":
                return SHOWS;
            case "seats":
                return SEATS;
            case "title":
                return TITLE;
            case "show_attr":
                return SHOW_ATTR;
            case "cpl_format":
                return CPL_FORMAT;
            case "root":
                return ROOT;
            case "percentage":
                return PERCENTAGE;
            case "day":
                return DAY;
            case "date":
                return DATE;
            case "rating":
                return RATING;
            default:
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    public static SegmentSplitTypeEnum getSplitTypeByCode(Integer code) {
        switch (code) {
            case 1:
                return SITES;
            case 2:
                return SITE_GROUPS;
            case 3:
                return TIME;
            case 4:
                return SHOWS;
            case 5:
                return SEATS;
            case 6:
                return TITLE;
            case 9:
                return SHOW_ATTR;
            case 10:
                return CPL_FORMAT;
            case 11:
                return ROOT;
            case 12:
                return PERCENTAGE;
            case 13:
                return DAY;
            case 14:
                return DATE;
            case 15:
                return RATING;
            default:
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    public Integer getCode() {
        return this.code;
    }

    public String getName() {
        return this.name;
    }


}
