package com.aam.producer.playlist.biz.model;

import com.alibaba.fastjson.annotation.JSONField;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

/**
 * segment model
 *
 * @author oliver.lo
 * @since 2019-07-24 17:47
 */
@Getter
@Setter
public class SegmentModel extends TPlaylistEventModel {

    private List<TPlaylistEventModel> events;

    private List<AutomationModel> automation;

    @JSONField(name = "visual_automation")
    private Boolean visualAutomation;

    @JSONField(name = "audio_automation")
    private Boolean audioAutomation;

    @JSONField(name = "visual_automation_part_2")
    private Boolean visualAutomation2;

    @JSONField(name = "audio_automation_part_2")
    private Boolean audioAutomation2;

    @JSONField(name = "content_association_uuid")
    private String contentAssociationUuid;

    private Boolean draft;
}
