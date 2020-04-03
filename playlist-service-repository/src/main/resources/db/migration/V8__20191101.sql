DROP TABLE IF EXISTS `playlist_delete_log`;
CREATE TABLE `playlist_delete_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `receipt_uuid` varchar(36) NOT NULL COMMENT 'receipt uuid',
  `complex_id` varchar(36) NOT NULL COMMENT 'complex uuid',
  `device_uuid` varchar(36) DEFAULT NULL COMMENT 'device uuid',
  `tpl_uuids` json COMMENT 'tpl uuids',
  `delete_system` tinyint(2) NOT NULL COMMENT 'delete in 1-Producer 2-TMS',
  `status` varchar(36) NOT NULL COMMENT 'task status',
  `message` text COMMENT 'task message',
  `attempted` bigint(20) DEFAULT NULL COMMENT 'attempted at tms',
  `created` bigint(20) NOT NULL COMMENT 'create time',
  `last_modified` bigint(20) NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uqx_receipt_uuid` (`receipt_uuid`) USING BTREE COMMENT 'unique key'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;