ALTER TABLE location ADD COLUMN location_formatname TEXT;
CREATE INDEX location_location_formatname_idx on location using btree (location_formatname);

UPDATE location SET location_formatname=formatLocationName(location_id);
