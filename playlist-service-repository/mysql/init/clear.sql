use playlist;
delete from playlist;
delete from playlist_data;
delete from playlist_ppl_association;
delete from playlist_segment_split_association;
delete from playlist_send_log;
delete from playlist_show_association;
delete from playlist_show_attribute_combination;
delete from playlist_sync_log;
delete from playlist_version;
delete from playlist_version_content_association;
delete from ppl_data;
delete from segment;
delete from segment_split;
delete from segment_split_content_association;
delete from show_attribute;
delete from tms_playlist;

INSERT INTO segment(uuid,organization_id,title,type,deleted,created,last_modified) VALUES('5546e68d-6991-11e9-831c-0242ac140002',null,'Automatic Feature Selector',1,0,0,0);
INSERT INTO segment(uuid,organization_id,title,type,deleted,created,last_modified) VALUES('5546e68d-6991-11e9-831c-0242ac140003',null,'Cineworld Ident 1',4,0,0,0);
INSERT INTO segment(uuid,organization_id,title,type,deleted,created,last_modified) VALUES('5546e68d-6991-11e9-831c-0242ac140004',null,'Cineworld Ident 2',4,0,0,0);
INSERT INTO segment(uuid,organization_id,title,type,deleted,created,last_modified) VALUES('5546e68d-6991-11e9-831c-0242ac140005',null,'Cineworld Ident 3',4,0,0,0);
INSERT INTO segment(uuid,organization_id,title,type,deleted,created,last_modified) VALUES('5546e68d-6991-11e9-831c-0242ac140006',null,'Cineworld Ident 4',4,0,0,0);
INSERT INTO segment(uuid,organization_id,title,type,deleted,created,last_modified) VALUES('5546e68d-6991-11e9-831c-0242ac140007',null,'Gold Spot Advertisement',4,0,0,0);
INSERT INTO segment(uuid,organization_id,title,type,deleted,created,last_modified) VALUES('5546e68d-6991-11e9-831c-0242ac140008',null,'National Advertisement',4,0,0,0);

use producer_view_service;

delete from ppl_data;
delete from playlist_data;