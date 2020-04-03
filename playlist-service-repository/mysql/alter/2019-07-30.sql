update tms_playlist set source_ppl_id = playlist_uuid,source_ppl_version_id = playlist_uuid where source_ppl_id is null;
ALTER TABLE tms_playlist ADD INDEX `playlist_complex_index`(`playlist_uuid`, `source_complex_id`) USING BTREE COMMENT 'playlist&complex index';
ALTER TABLE tms_playlist ADD INDEX `ppl_version_index`(`source_ppl_id`, `source_ppl_version_id`) USING BTREE COMMENT 'ppl&version index';

ALTER TABLE playlist_send_log ADD INDEX `playlist_complex_index`(`playlist_uuid`, `complex_id`) USING BTREE COMMENT 'playlist&complex index';

ALTER TABLE playlist_sync_log ADD INDEX `complex_playlist_index`(`complex_id`, `playlist_uuid`) USING BTREE COMMENT 'complex&playlist index';