update segment_split set sort_num = 0 where sort_num is null;
alter table segment_split modify column sort_num bigint not null default 0 COMMENT 'sort number';