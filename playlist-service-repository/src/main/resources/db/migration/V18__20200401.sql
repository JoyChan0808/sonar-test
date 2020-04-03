alter table playlist_version add index `playlist_uuid_idx`(`playlist_uuid`)
USING BTREE COMMENT 'playlist_uuid index';

alter table playlist_version_content_association add index `ppl_version_id_idx`(`ppl_version_id`)
USING BTREE COMMENT 'ppl_version_id index';

alter table playlist_version_content_association add index `uuid_idx`(`uuid`)
USING BTREE COMMENT 'uuid index';

alter table playlist_show_attribute_combination add index `ppl_version_id_idx`(`playlist_uuid`)
USING BTREE COMMENT 'ppl_version_id(playlist_uuid) index';

alter table playlist_segment_split_association add index `segment_association_title_idx`(`segment_association_uuid`,`title_id`)
USING BTREE COMMENT 'segment_association_uuid title_id index';
