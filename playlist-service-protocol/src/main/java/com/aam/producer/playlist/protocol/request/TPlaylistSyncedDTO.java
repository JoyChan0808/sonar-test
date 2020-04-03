package com.aam.producer.playlist.protocol.request;

import com.alibaba.fastjson.annotation.JSONField;
import java.io.Serializable;
import java.util.List;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import lombok.Data;

/**
 * playlist sync dto
 *
 * @author oliver.lo
 * @since 2019/4/8 2:00 PM
 */
@Data
public class TPlaylistSyncedDTO implements Serializable {

    private static final long serialVersionUID = -3263791506081348660L;

    /**
     * complexUuid that where data sync from
     */
    @NotBlank(message = "{complex_uuid.notBlank}")
    @JSONField(name = "complex_uuid")
    private String complexUuid;

    /**
     * playlist detail
     */
    @NotNull(message = "{playlist.notNull}")
    @JSONField(name = "playlist")
    private Playlist playlist;

    private Boolean success;

    private String message;

    @JSONField(name = "playlist_uuid")
    private String playlistUuid;

    @Data
    public static class Playlist implements Serializable {

        private static final long serialVersionUID = -3462075950121532139L;

        /**
         * Primary identifier of a Playlist
         */
        @NotBlank(message = "{playlist_uuid.notBlank}")
        @JSONField(name = "uuid")
        private String uuid;

        /**
         * True if the playlist is empty
         */
        @JSONField(name = "clean")
        private Boolean clean;

        /**
         * Primary identifiers of playlist contents
         */
        @JSONField(name = "content_ids")
        private List<String> contentIds;

        /**
         * Duration of all events in the playlist that are 'non-recursive'
         */
        @JSONField(name = "duration_in_seconds")
        private Float durationInSeconds;

        /**
         * True if Playlist contains 3D contents
         */
        @JSONField(name = "is_3d")
        private Boolean as3d;

        /**
         * True if Playlist contains 4K contents
         */
        @JSONField(name = "is_4k")
        private Boolean as4k;

        /**
         * True if Playlist contains High Frame Rate contents
         */
        @JSONField(name = "is_hfr")
        private Boolean asHfr;

        /**
         * True if the Playlist contains any placeholder items (Pack, Macro)
         */
        @JSONField(name = "is_template")
        private Boolean templated;

        /**
         * SPL json data
         */
        @JSONField(name = "playlist")
        private Object json;

        /**
         * List of playlists inside a playlist (recursion). Primarly for internal handling of intermissions
         */
        @JSONField(name = "playlist_ids")
        private List<String> playlistIds;

        /**
         * Preshow duration in seconds
         */
        @JSONField(name = "preshow_duration")
        private Float preShowDuration;

        /**
         * Playlist Title
         */
        @JSONField(name = "title")
        private String title;

        /**
         * Duration of all events in the playlist including recursive ones
         */
        @JSONField(name = "total_duration_in_seconds")
        private Float totalDurationInSeconds;

        /**
         * Primary identifier of a Device
         */
        @JSONField(name = "device_uuid")
        private String deviceUuid;
    }
}


