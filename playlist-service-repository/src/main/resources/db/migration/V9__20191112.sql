drop view ppl_view;
create view ppl_view as
select t1.uuid                                                                     as ppl_uuid,
       t2.playlist_uuid,
       t4.uuid                                                                     as release_version_uuid,
       t2.source_complex_id                                                        as complex_uuid,
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
       t2.json -> '$.is_3d'                                                        as is3d
from playlist t1
         left join tms_playlist t2 on t1.uuid = t2.source_ppl_id and t1.automatically_apply = 0 and source = 1 and t2.deleted = 0
         left join playlist_version t4 on t4.playlist_uuid = t1.uuid and t4.status = 2 and t4.deleted = 0
where t1.deleted = 0;