package com.aam.producer.playlist.biz.enums;

import org.apache.commons.lang3.StringUtils;

/**
 * pos mapping status enum
 *
 * @author oliver.lo
 * @since 2019-09-18 16:24
 */
public enum PosMappingStatusEnum {

    // producer
    MARK("mark"),
    REQUEST("request"),
    FAILED("failed"),
    SUCCEEDED("succeeded"),
    INVALID("invalid"),

    // tms confirm
    UNASSIGNED("unassigned"),
    ASSIGNED("assigned"),
    DELETED("deleted"),
    WARNING("warning"),
    ERROR("error"),

    // view
    PENDING("pending"),
    ;

    private String title;

    PosMappingStatusEnum(String title) {
        this.title = title;
    }

    // for producer view
    public static String titleForView(String title, String pplUuid) {
        if (UNASSIGNED.getTitle().equals(title) || StringUtils.isEmpty(pplUuid)) {
            return UNASSIGNED.getTitle();
        } else if (MARK.getTitle().equals(title) || REQUEST.getTitle().equals(title)) {
            return PENDING.getTitle();
        } else {
            return ASSIGNED.getTitle();
        }
    }

    public String getTitle() {
        return title;
    }
}
