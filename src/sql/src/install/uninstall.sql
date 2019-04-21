-- Drops all Gateway tables from the database.
DROP TABLE IF EXISTS
  gateway_error,
  player, player_group, players_groups,
  persona, players_personas,
  timeline, chapter, post, chapter_links,
  chapters_permissions, timelines_permissions
CASCADE;



-- Drops all Gateway enum types from the database.
DROP TYPE IF EXISTS
  chapter_permission_type,
  timeline_permission_type
CASCADE;



-- Drops all Gateway sequences.
DROP SEQUENCE IF EXISTS
  gateway_error_id
CASCADE;



-- Drops all Gateway functions.
DROP FUNCTION IF EXISTS
  base36_encode, base36_decode
CASCADE;
