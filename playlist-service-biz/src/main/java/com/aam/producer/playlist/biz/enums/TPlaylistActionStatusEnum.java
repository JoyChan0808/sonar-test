package com.aam.producer.playlist.biz.enums;

/**
 * playlist sync status enum
 *
 * @author oliver.lo
 * @since 2019/4/9 7:42 PM
 */
public enum TPlaylistActionStatusEnum {

    FAILED(-1),//failed when doing the sync/send/publish work
    MARKED(0),//has marked to sync/send/publish
    REQUESTED(1),//has sent the sync/send/publish request
    DELIVERED(2),//has delivered the request
    DONE(3),//has finished the playlist sync/send/publish success
    ;

    private int code;

    TPlaylistActionStatusEnum(int code) {
        this.code = code;
    }

    public static TPlaylistActionStatusEnum getByCode(Integer code) {
        switch (code) {
            case -1:
                return FAILED;
            case 0:
                return MARKED;
            case 1:
                return REQUESTED;
            case 2:
                return DELIVERED;
            case 3:
                return DONE;
            default:
                return null;
        }
    }

    public int getCode() {
        return code;
    }
}
