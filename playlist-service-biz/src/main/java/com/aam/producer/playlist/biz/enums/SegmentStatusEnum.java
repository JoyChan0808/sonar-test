package com.aam.producer.playlist.biz.enums;

import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;

public enum SegmentStatusEnum {

    DRAFT(1), RELEASE(2), DRAFT_AND_RELEASE(3), NULL(4);


    private Integer status;

    SegmentStatusEnum(Integer status) {
        this.status = status;
    }

    public static SegmentStatusEnum getEmunByStatusStr(String statusStr) {
        switch (statusStr) {
            case "draft":
                return DRAFT;
            case "release":
                return RELEASE;
            case "draft&release":
                return DRAFT_AND_RELEASE;
            case "null":
                return NULL;
            default:
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    public static String getEmunStrByStatus(Integer status) {
        return getStatusStr(status);
    }

    private static String getStatusStr(Integer status) {
        switch (status) {
            case 1:
                return "draft";
            case 2:
                return "release";
            case 3:
                return "draft&release";
            case 4:
                return "null";
            default:
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    public Integer getStatus() {
        return status;
    }

    public String getStatusStr() {
        return getStatusStr(status);
    }

}
