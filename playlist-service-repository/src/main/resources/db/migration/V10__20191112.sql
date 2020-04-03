alter table playlist_send_log add column ppl_uuid varchar(36) COMMENT 'ppl uuid' AFTER `device_uuid`;

drop index `idx_show_start_code` on pos_playlist_mapping;

update pos_playlist_mapping set ppl_uuid = '' where ppl_uuid is null;
update pos_playlist_mapping set show_attributes_code = -1 where show_attributes_code is null;
alter table pos_playlist_mapping add index `idx_org_show_code_start`(`organization_id`, `show_attributes_code`, `pos_start`) USING BTREE COMMENT 'org&attributes_code&pos_start index';
alter table pos_playlist_mapping add index `idx_ppl_show_start`(`ppl_uuid`, `pos_start`) USING BTREE COMMENT 'ppl&pos_start index';