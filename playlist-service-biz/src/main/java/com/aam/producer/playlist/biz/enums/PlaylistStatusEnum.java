package com.aam.producer.playlist.biz.enums;

import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;

public enum PlaylistStatusEnum {

    DRAFT(1), RELEASE(2), DRAFT_AND_RELEASE(3);


    private Integer status;

    PlaylistStatusEnum(Integer status) {
        this.status = status;
    }

    public static PlaylistStatusEnum getEmunByStatusStr(String statusStr) {
        switch (statusStr) {
            case "draft":
                return DRAFT;
            case "release":
                return RELEASE;
            case "draft&release":
                return DRAFT_AND_RELEASE;
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
            default:
                throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }
    }

    public static String getStatusStr(boolean hasDraft, boolean hasRelease) {
        String status;
        if (hasDraft && !hasRelease) {
            status = DRAFT.getStatusStr();
        } else if (hasDraft) {
            status = DRAFT_AND_RELEASE.getStatusStr();
        } else {
            status = RELEASE.getStatusStr();
        }
        return status;
    }

    public static boolean contains(Integer status) {
        return status == 1 || status == 2 || status == 3;
    }

    public Integer getStatus() {
        return status;
    }

    public String getStatusStr() {
        return getStatusStr(status);
    }
}
