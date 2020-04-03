package com.aam.producer.playlist.biz.model;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;

/**
 * ppl publish dto
 *
 * @author oliver.lo
 * @since 2019/5/28 4:20 PM
 */
@Data
public class PlaylistPublishModel implements Serializable {

    private static final long serialVersionUID = 1554972355329295056L;

    @JSONField(name = "organization_id")
    private String organizationId;

    @JSONField(name = "ppl_uuid")
    private String pplUuid;

    @JSONField(name = "ppl_version_uuid")
    private String pplVersionUuid;

    @JSONField(name = "automatically_apply")
    private Boolean automatic;

    @JSONField(name = "title")
    private String title;

    @JSONField(name = "playlist_content_list")
    private List<PlaylistContent> contentList;

    @JSONField(name = "pos_uuid_list")
    private List<String> posUuidList;

    @Data
    public static class PlaylistContent implements Serializable {

        private static final long serialVersionUID = -2365064679315830964L;

        @JSONField(name = "content_type")
        private int contentType;

        @JSONField(name = "content_kind")
        private String contentKind;

        @JSONField(name = "text")
        private String text;

        @JSONField(name = "content_uuid")
        private String contentUuid;

        @JSONField(name = "extension")
        private String extension;

        @JSONField(name = "segment_split_list")
        private List<SegmentSplit> segmentSplits;

        @JSONField(name = "content_association_uuid")
        private String contentAssociationUuid;

        @JSONField(name = "sort_number")
        private Integer sortNumber;
    }

    @Data
    public static class SegmentSplit implements Serializable {

        private static final long serialVersionUID = -5119597589911048968L;

        @JSONField(name = "title")
        private String title;

        @JSONField(name = "show_type")
        private String showType;

        @JSONField(name = "segment_split_uuid")
        private String segmentSplitUuid;

        @JSONField(name = "segment_split_title")
        private String segmentSplitTitle;

        @JSONField(name = "playlist_content_list", serialize = false)
        private List<PlaylistContent> contentList = new ArrayList<>();

        @JSONField(name = "shows_list", serialize = false)
        private List<Shows> shows = new ArrayList<>();

        @JSONField(name = "filled")
        private Boolean filled;
    }

    @Data
    public static class Shows implements Serializable {

        private static final long serialVersionUID = -218684903527320442L;

        @JSONField(name = "pos_uuid")
        private String posUuid;

        @JSONField(name = "extension")
        private String extension;
    }
}

