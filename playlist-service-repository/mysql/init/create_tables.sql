/*
 Navicat Premium Data Transfer

 Source Server Type    : MySQL
 Source Server Version : 50725
 Source Schema         : playlist

 Target Server Type    : MySQL
 Target Server Version : 50725
 File Encoding         : 65001

 Date: 02/04/2019 18:47:51
*/

use playlist;
SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for tms_playlist
-- ----------------------------
DROP TABLE IF EXISTS `tms_playlist`;
CREATE TABLE `tms_playlist` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `organization_id` varchar(36) NULL DEFAULT NULL COMMENT 'organization id',
  `playlist_uuid` varchar(36) NOT NULL COMMENT 'playlist uuid',
  `title` varchar(200) NOT NULL COMMENT 'playlist title',
  `json` JSON COMMENT 'playlist json',
  `content_ids` JSON COMMENT 'content ids',
  `templated` tinyint(1) NULL DEFAULT NULL COMMENT 'template or spl',
  `source` tinyint(2) NULL DEFAULT NULL COMMENT 'playlist source，1-Producer,2-TMS',
  `source_ppl_id` varchar(36) NULL DEFAULT NULL COMMENT 'ppl uuid',
  `source_ppl_version_id` varchar(36) NULL DEFAULT NULL COMMENT 'ppl version uuid',
  `source_segment_split_uuid` JSON NULL DEFAULT NULL COMMENT 'segment split json',
  `automatically_apply` tinyint(1) NULL DEFAULT NULL COMMENT 'automatic or static',
  `source_complex_id` varchar(36) NULL DEFAULT NULL COMMENT 'complex uuid',
  `source_device_id` varchar(36) NULL DEFAULT NULL COMMENT 'lms device uuid',
  `filled` tinyint(1) NULL DEFAULT NULL COMMENT 'filled or not',
  `changed` tinyint(1) NULL DEFAULT NULL COMMENT 'changed by TMS or not',
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT 'deleted or not',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for playlist_sync_log
-- ----------------------------
DROP TABLE IF EXISTS `playlist_sync_log`;
CREATE TABLE `playlist_sync_log` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `complex_id` varchar(36) NOT NULL COMMENT 'complex uuid',
  `device_uuid` varchar(36) NOT NULL COMMENT 'lms device uuid',
  `playlist_uuid` varchar(36) NOT NULL COMMENT 'playlist uuid',
  `title` varchar(200) NULL DEFAULT NULL COMMENT 'playlist title',
  `hash` varchar(128) NULL DEFAULT NULL COMMENT 'playlist hash',
  `json` JSON NULL DEFAULT NULL COMMENT 'playlist json',
  `content_ids` JSON NULL DEFAULT NULL COMMENT 'content ids',
  `templated` tinyint(1) NULL DEFAULT NULL COMMENT 'template or spl',
  `as_3d` tinyint(1) NULL DEFAULT NULL COMMENT 'is 3d or not',
  `as_4k` tinyint(1) NULL DEFAULT NULL COMMENT 'is 4k or not',
  `as_hfr` tinyint(1) NULL DEFAULT NULL COMMENT 'is hfr or not',
  `method` tinyint(2) NOT NULL COMMENT 'playlist action method，1-Create,2-Update',
  `status` tinyint(2) NOT NULL COMMENT 'sync status,-1-Failed,0-Marked,1-Requested,3-Done',
  `message` text NULL DEFAULT NULL COMMENT 'sync message',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for playlist_send_log
-- ----------------------------
DROP TABLE IF EXISTS `playlist_send_log`;
CREATE TABLE `playlist_send_log` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `receipt_uuid` varchar(36) NOT NULL COMMENT 'send receipt uuid',
  `action_id` varchar(36) NULL DEFAULT NULL COMMENT 'async save playlist action uuid',
  `pos_id` varchar(36) NULL DEFAULT NULL COMMENT 'pos uuid',
  `complex_id` varchar(36) NOT NULL COMMENT 'complex uuid',
  `device_uuid` varchar(36) NOT NULL COMMENT 'lms device uuid',
  `playlist_uuid` varchar(36) NOT NULL COMMENT 'playlist uuid',
  `title` varchar(200) NULL DEFAULT NULL COMMENT 'playlist title',
  `json` JSON NULL DEFAULT NULL COMMENT 'playlist json',
  `content_ids` JSON NULL DEFAULT NULL COMMENT 'content ids',
  `method` tinyint(2) NOT NULL COMMENT 'playlist action method，1-Create,2-Update',
  `status` tinyint(2) NOT NULL COMMENT 'send status,-1-Failed,0-Marked,1-Requested,2-Delivered,3-Done',
  `message` text NULL DEFAULT NULL COMMENT 'send message',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


-- ----------------------------
-- Table structure for playlist
-- ----------------------------
DROP TABLE IF EXISTS `playlist`;
CREATE TABLE `playlist` (
  `uuid` varchar(36) NOT NULL COMMENT 'primary id',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `title` varchar(200) NOT NULL COMMENT 'title',
  `automatically_apply` tinyint(1) NOT NULL COMMENT 'is automatically playlist  1-true 0-false',
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT 'is deleted 1-true 0-false',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`uuid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for show_attribute
-- ----------------------------
DROP TABLE IF EXISTS `show_attribute`;
CREATE TABLE `show_attribute` (
  `uuid` varchar(36) NOT NULL COMMENT 'primary id',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `short_code` int NOT NULL COMMENT 'short code',
  `title` varchar(100) NOT NULL COMMENT 'title',
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT 'is deleted 1-true 0-false',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`uuid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for playlist_show_attribute_combination
-- ----------------------------
DROP TABLE IF EXISTS `playlist_show_attribute_combination`;
CREATE TABLE `playlist_show_attribute_combination` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `playlist_uuid` varchar(36) NOT NULL COMMENT 'playlist uuid',
  `name` varchar(64) NOT NULL COMMENT 'name',
  `short_code_association` bigint NOT NULL COMMENT 'short code count value',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for playlist_version
-- ----------------------------
DROP TABLE IF EXISTS `playlist_version`;
CREATE TABLE `playlist_version` (
  `uuid` varchar(36) NOT NULL COMMENT 'primary id',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `playlist_uuid` varchar(36) NOT NULL COMMENT 'playlist uuid',
  `templated` tinyint(1) NULL DEFAULT NULL COMMENT 'is templated 1-true 0-false',
  `publish_time` bigint NULL DEFAULT NULL COMMENT 'publish time',
  `valid_time` bigint NULL DEFAULT NULL COMMENT 'valid time',
  `publish_later` tinyint(1) NULL DEFAULT NULL   COMMENT 'is publish later 1-true 0-false',
  `time_zone` tinyint(1) NULL DEFAULT NULL   COMMENT 'time zone',
  `status` tinyint(2) NOT NULL COMMENT 'status 1-draft 2-release',
  `extension` text NULL DEFAULT NULL COMMENT 'extension info',
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT 'is deleted 1-true 0-false',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`uuid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for playlist_version_content_association
-- ----------------------------
DROP TABLE IF EXISTS `playlist_version_content_association`;
CREATE TABLE `playlist_version_content_association` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `uuid`  varchar(36) NOT NULL COMMENT 'UUID',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `title` varchar(256) NOT NULL COMMENT 'title',
  `playlist_uuid` varchar(36) NOT NULL COMMENT 'playlist uuid',
  `ppl_version_id` varchar(36) NOT NULL  COMMENT 'playlist version uuid',
  `content_id` varchar(36) NOT NULL COMMENT 'content uuid',
  `content_type` tinyint(2) NOT NULL COMMENT 'content type 1-cpl 2-automation 3-segment',
  `sort_number` int NOT NULL COMMENT 'sort number',
  `extension` text NULL DEFAULT NULL COMMENT 'extension',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for segment
-- ----------------------------
DROP TABLE IF EXISTS `segment`;
CREATE TABLE `segment` (
  `uuid` varchar(36) NOT NULL COMMENT 'primary id',
  `organization_id` varchar(36) COMMENT 'organization id',
  `title` varchar(100) NOT NULL COMMENT 'title',
  `type` tinyint(2) NOT NULL COMMENT 'type 1-automatic_segment 2-playlist_segment 3-title_segment 4-api_segment',
  `purpose` tinyint(2) NULL DEFAULT NULL   COMMENT 'purpose',
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT 'is deleted 1-true 0-false',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`uuid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;


-- ----------------------------
-- Table structure for segment_split
-- ----------------------------
DROP TABLE IF EXISTS `segment_split`;
CREATE TABLE `segment_split` (
  `uuid` varchar(36) NOT NULL COMMENT 'primary id',
  `parent_uuid` varchar(36)  COMMENT 'parent uuid',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `split_title` varchar(100) NOT NULL COMMENT 'title',
  `split_type` tinyint(2) NOT NULL COMMENT 'split type 1-sites 2-site_groups 3-time 4-shows 5-seats',
  `split_rule` text  NULL DEFAULT NULL  COMMENT 'split rule',
  `status` tinyint(2) NOT NULL COMMENT 'status 1-draft 2-release',
  `publish_time` bigint NULL DEFAULT NULL COMMENT 'publish time',
  `valid_time` bigint NULL DEFAULT NULL COMMENT 'valid time',
  `deleted` tinyint(1) NOT NULL DEFAULT 0 COMMENT '',
  `created` bigint NOT NULL COMMENT 'is deleted 1-true 0-false',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`uuid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for segment_split_content_association
-- ----------------------------
DROP TABLE IF EXISTS `segment_split_content_association`;
CREATE TABLE `segment_split_content_association` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `segment_split_uuid` varchar(36) NOT NULL COMMENT 'segment split uuid',
  `content_id` varchar(36) NOT NULL COMMENT 'content uuid',
  `content_type` tinyint(2) NOT NULL COMMENT 'content type',
  `sort_number` int NOT NULL COMMENT 'sort number',
  `extension` text NULL DEFAULT NULL COMMENT 'extension',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ----------------------------
-- Table structure for playlist_segment_split_association
-- ----------------------------
DROP TABLE IF EXISTS `playlist_segment_split_association`;
CREATE TABLE `playlist_segment_split_association` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT 'primary id',
  `organization_id` varchar(36) NOT NULL COMMENT 'organization id',
  `playlist_uuid` varchar(36) NOT NULL COMMENT 'playlist uuid',
  `ppl_version_id` varchar(36) NOT NULL COMMENT 'playlist version UUID',
  `title_id` varchar(36) COMMENT 'title uuid',
  `segment_association_uuid` varchar(36)  NOT NULL COMMENT 'playlist version segment association uuid',
  `segment_split_uuid` varchar(36) NOT NULL COMMENT 'segment split uuid',
  `created` bigint NOT NULL COMMENT 'create time',
  `last_modified` bigint NOT NULL COMMENT 'modified time',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;