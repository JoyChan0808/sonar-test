package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * pos data
 *
 * @author oliver.lo
 * @since 2019-07-29 20:00
 */
@Getter
@Setter
@ToString
public class PosDataDTO implements Serializable {

    private static final long serialVersionUID = -1;

    private String id;

    @JSONField(name = "uuid")
    private String posUuid;

    @JSONField(name = "ppl_uuid")
    private String pplUuid;

    @JSONField(name = "automatic")
    private Boolean pplAutomatic;

    @JSONField(name = "playlist_id")
    private String tplUuid;

    @JSONField(name = "show_attributes")
    private Map<String, String> showAttributes;

    private Boolean expired;

    private Boolean deleted;

    @JSONField(name = "complex_uuid")
    private String complexUuid;

    @JSONField(name = "device_uuid")
    private String deviceUuid;

    private String state;

    @JSONField(name = "screen_identifier")
    private String screenIdentifier;

    private String hash;

    @JSONField(name = "short_start_time")
    private String shortStartTime;

    @JSONField(name = "short_start_date")
    private String shortStartDate;

    @JSONField(name = "title_uuid")
    private String titleUuid;

    private Boolean scheduled;

    private String title;

    @JSONField(name = "schedule_id")
    private String scheduleId;

    @JSONField(name = "seats_available")
    private Integer seatsAvailable;

    @JSONField(name = "overall_duration")
    private Integer overallDuration;

    @JSONField(name = "screen_uuid")
    private String screenUuid;

    @JSONField(name = "week_number")
    private Integer weekNumber;

    @JSONField(name = "print_number")
    private String printNumber;

    @JSONField(name = "title_name")
    private String titleName;

    @JSONField(name = "sms_device_uuid")
    private String smsDeviceUuid;

    @JSONField(name = "source_start")
    private String sourceStart;

    @JSONField(name = "feature_duration")
    private Integer featureDuration;

    @JSONField(name = "seats_sold")
    private Integer seatsSold;

    private String start;

    private String end;

    private String message;

    @JSONField(name = "autoMappable")
    private Boolean auto_mappable;

    @JSONField(name = "complex_identifier")
    private String complexIdentifier;

    private String source;

    private String language;

    @JSONField(name = "placeholder_type")
    private String placeholderType;

    @JSONField(name = "last_modified")
    private String lastModified;

    @JSONField(name = "mapping_in_system")
    private String mappingInSystem;

    @JSONField(name = "unmatched_show_attributes")
    private Map<String, String> unmatchedShowAttributes;

    @JSONField(name = "pos_last_modified")
    private String posLastModified;
}
