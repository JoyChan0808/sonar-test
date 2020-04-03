alter table pos_playlist_mapping add index `idx_title_pos_ppl`(`title_uuid`,`ppl_uuid`,`pos_uuid`)
USING BTREE COMMENT 'title_uuid ppl_uuid pos_uuid index';