alter table show_attribute add constraint uk_title unique(title,organization_id);
alter table show_attribute add constraint uk_short_code unique(short_code,organization_id);