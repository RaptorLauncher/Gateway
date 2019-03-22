-- Drops all Gateway tables from the database.
DROP TABLE IF EXISTS
  player, player_group, players_groups,
  persona, owners_borrowers,
  timeline, chapter, post, chapter_link,
  chapter_permission, timeline_permission;



-- Drops all Gateway enum types from the database.
DROP TYPE IF EXISTS
  chapter_permission_type,
  timeline_permission_type;



-- Drops all Gateway trigger functions.
DROP FUNCTION IF EXISTS
  player_group_ensure_everyone,
  player_ensure_narrator,
  player_group_insert_into_everyone,
  players_groups_never_remove_everyone,
  persona_ensure_narrator;
