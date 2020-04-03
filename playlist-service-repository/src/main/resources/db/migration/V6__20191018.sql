ALTER TABLE pos_playlist_mapping
ADD COLUMN unmatched_show_attributes json COMMENT 'unmatched show attributes' AFTER `title_uuid`;