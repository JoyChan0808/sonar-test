ALTER TABLE pos_playlist_mapping
ADD COLUMN attempted bigint(20) DEFAULT NULL COMMENT 'attempted at tms' AFTER `deleted`;

DROP TABLE IF EXISTS `pos_mapping_send_log`;
CREATE TABLE `pos_mapping_send_log` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `receipt_uuid` varchar(36) NOT NULL COMMENT 'send receipt uuid',
  `complex_id` varchar(36) NOT NULL COMMENT 'complex uuid',
  `mapping` json COMMENT 'pos mapping json string',
  `status` varchar(36) NOT NULL COMMENT 'send status',
  `message` text COMMENT 'send message',
  `attempted` bigint(20) DEFAULT NULL COMMENT 'attempted at tms',
  `created` bigint(20) NOT NULL COMMENT 'create time',
  `last_modified` bigint(20) NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uqx_receipt_uuid` (`receipt_uuid`) USING BTREE COMMENT 'unique key'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;