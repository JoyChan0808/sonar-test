-- ----------------------------
-- Table structure for pos_playlist_mapping
-- ----------------------------
DROP TABLE IF EXISTS `pos_playlist_mapping`;
CREATE TABLE `pos_playlist_mapping` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `pos_uuid` varchar(36) NOT NULL COMMENT 'pos uuid',
  `pos_title` varchar(100) NOT NULL COMMENT 'pos title',
  `pos_start` bigint(20) NOT NULL COMMENT 'pos start',
  `pos_end` bigint(20) NOT NULL COMMENT 'pos end',
  `complex_uuid` varchar(36) NOT NULL COMMENT 'complex uuid',
  `screen_uuid` varchar(36) NOT NULL COMMENT 'screen uuid',
  `title_uuid` varchar(36) DEFAULT NULL COMMENT 'title uuid',
  `show_attributes` json DEFAULT NULL COMMENT 'show attributes json',
  `show_attributes_code` bigint(20) DEFAULT NULL COMMENT 'show attributes short code',
  `language` varchar(100) DEFAULT NULL COMMENT 'language',
  `ppl_uuid` varchar(36) DEFAULT NULL COMMENT 'ppl uuid',
  `ppl_automatic` tinyint(1) DEFAULT NULL COMMENT 'ppl 0-other 1-automatic',
  `tpl_uuid` varchar(36) DEFAULT NULL COMMENT 'tpl uuid',
  `state` varchar(36) NOT NULL COMMENT 'pos state',
  `assign_system` tinyint(2) DEFAULT NULL COMMENT 'assign in 1-Producer 2-TMS',
  `mapping_completed` tinyint(1) DEFAULT '0' COMMENT 'mapping complete 0-false 1-true',
  `mapping_status` varchar(36) COMMENT 'mapping status',
  `mapping_message` varchar(255) COMMENT 'mapping message',
  `deleted` tinyint(1) NOT NULL DEFAULT '0' COMMENT 'deleted 0-false 1-true',
  `created` bigint(20) NOT NULL COMMENT 'create time',
  `last_modified` bigint(20) NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uqx_pos_uuid` (`pos_uuid`) USING BTREE COMMENT 'unique key',
  KEY `idx_show_start_code` (`pos_start`,`show_attributes_code`) USING BTREE COMMENT 'pos show start and attribute code'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


alter table playlist add column status tinyint(2) COMMENT 'status bin index1-draft index2-release';

DROP VIEW IF EXISTS `ppl_view`;
CREATE VIEW `ppl_view` AS
select
	ppl_uuid,
    playlist_uuid,
    release_version_uuid,
    complex_uuid,
    organization_id,
    title,
    status,
    automatically_apply,
    last_modified,
    type,
    (select count(*) from pos_playlist_mapping where pos_start>unix_timestamp(now())*1000 and ppl_uuid = t3.ppl_uuid) as shows,
    (select count(distinct complex_uuid) from pos_playlist_mapping where pos_start>unix_timestamp(now())*1000 and ppl_uuid = t3.ppl_uuid) as sites,
    is3d
 from (select
	t1.uuid as ppl_uuid,
    t2.playlist_uuid,
    t4.uuid as release_version_uuid,
    t2.source_complex_id as complex_uuid,
    t1.organization_id,
    t1.title,
    t1.status,
    t1.automatically_apply,
    t1.last_modified,
    'producer' as type,
    t2.json->'$.is_3d' as is3d
 from playlist t1
    left join tms_playlist t2 on t1.uuid=t2.source_ppl_id and t1.automatically_apply=0 and source = 'PRODUCER'
    left join playlist_version t4 on t4.playlist_uuid=t1.uuid and t4.status=2 where t1.deleted=0
 union all
 select
    source_ppl_id as ppl_uuid,
	playlist_uuid,
	null as release_version_uuid,
    source_complex_id as complex_uuid,
    organization_id,
    title,
    '' as status,
    null as automatically_apply,
    last_modified,
    'tms' as type,
    json->'$.is_3d' as is3d
 from tms_playlist where source = 2 and deleted=0) t3;