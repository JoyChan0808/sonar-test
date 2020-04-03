package com.aam.producer.playlist.common.utils;

import org.apache.commons.lang3.StringUtils;

/**
 * com.aam.producer.playlist.common.utils
 *
 * @author oliver.lo
 * @since 2019-10-23 16:22
 */
public class TimeUtils {

    public static Long changeMillisecondTimestamp(final String time) {
        if (StringUtils.isEmpty(time)) {
            return null;
        }
        final double timeStamp = Double.parseDouble(time);
        return (timeStamp <= 99999999999L) ? Math.round(timeStamp * 1000) : Math.round(timeStamp);
    }
}
