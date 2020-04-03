alter table playlist_segment_split_association modify playlist_uuid varchar(36) null comment 'playlist uuid';
alter table playlist_segment_split_association modify ppl_version_id varchar(36) null comment 'playlist version UUID';

drop view ppl_view;
create view ppl_view as
select t1.uuid                                                                     as ppl_uuid,
       ''                                                                          as playlist_uuid,
       t4.uuid                                                                     as release_version_uuid,
       ''                                                                          as complex_uuid,
       t1.organization_id,
       t1.title,
       t1.status,
       t1.automatically_apply,
       t1.last_modified,
       'producer'                                                                  as type,
       (select count(*)
        from pos_playlist_mapping
        where pos_start > unix_timestamp(now()) * 1000 and ppl_uuid = t1.uuid ) as shows,
       (select count(distinct complex_uuid)
        from pos_playlist_mapping
        where pos_start > unix_timestamp(now()) * 1000
          and ppl_uuid = t1.uuid )                                              as sites,
       ''                                                                       as is3d
from playlist t1 left join playlist_version t4 on t4.playlist_uuid = t1.uuid and t4.status = 2 and t4.deleted = 0
where t1.deleted = 0;