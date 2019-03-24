-- Drops all Gateway tables from the database.
DROP TABLE IF EXISTS
  player, player_group, players_groups,
  persona, players_personas,
  timeline, chapter, post, chapter_link,
  chapter_permission, timeline_permission
CASCADE;



-- Drops all Gateway enum types from the database.
DROP TYPE IF EXISTS
  chapter_permission_type,
  timeline_permission_type
CASCADE;
