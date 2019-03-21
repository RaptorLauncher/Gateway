-- name: drop-tables @execute
-- Drops all Gateway tables from the database.
DROP TABLE IF EXISTS
    player, player_group, players_groups,
    persona, owners_borrowers,
    timeline, chapter, post, chapter_link,
    chapter_permission, timeline_permission;

-- name: drop-types @execute
-- Drops all Gateway enum types from the database.
DROP TYPE IF EXISTS
    chapter_permission_type, timeline_permission_type;