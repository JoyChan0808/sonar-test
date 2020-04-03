alter table segment_split add column default_group int not null  DEFAULT  0 COMMENT 'default group';
alter table segment_split add column auto_group int not null  DEFAULT  0 COMMENT 'auto group';