alter table tms_playlist add index `idx_source_complex_id`(`source_complex_id`) USING BTREE COMMENT 'complex_id index';