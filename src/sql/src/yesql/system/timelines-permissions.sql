-- name: select-all-timelines-permissions-of-player
-- Selects all timeline permissions of a given player.
-- This includes global permissions.
-- TODO everywhere: /acl variant that only lists timelines the player can actually see
SELECT player_id, player_group_id, timeline_id, permission
  FROM timelines_permissions
  WHERE player_id = ? OR (player_id IS NULL AND player_group_id IS NULL);

-- name: select-all-timelines-permissions-of-player-group
-- Selects all timeline permissions of a given player group.
-- This includes global permissions.
SELECT player_id, player_group_id, timeline_id, permission
  FROM timelines_permissions
  WHERE player_group_id = ? OR (player_id IS NULL AND player_group_id IS NULL);

-- name: select-all-timelines-permissions-of-timeline
-- Selects all timeline permissions of a given timeline.
SELECT player_id, player_group_id, timeline_id, permission
  FROM timelines_permissions
  WHERE timeline_id = ?;

-- name: select-all-timelines-permissions-of-player-and-timeline
-- Selects all permissions for all timelines of a given player and timeline.
SELECT player_id, player_group_id, timeline_id, permission
  FROM timelines_permissions
  WHERE player_id = ? AND timeline_id = ?;

-- name: select-all-timelines-permissions-of-player-group-and-timeline
-- Selects all permissions for all timelines of a given player group and
-- timeline.
SELECT player_id, player_group_id, timeline_id, permission
  FROM timelines_permissions
  WHERE player_group_id = ? AND timeline_id = ?;



-- name: add-timeline-permission-to-player
-- Adds a given timeline permission to a given player.
WITH timeline_id AS (SELECT ?::integer AS p),
     player_id AS (SELECT ?::integer AS p),
     permission AS (SELECT ?::timeline_permission_type AS p)
INSERT INTO timeline_permission (timeline_id, player_id, permission)
  VALUES ((SELECT p FROM timeline_id),
          (SELECT p FROM player_id),
          (SELECT p FROM permission))
  ON CONFLICT DO NOTHING;

-- name: add-timeline-permission-to-player-group
-- Adds a given timeline permission to a given player group.
WITH timeline_id AS (SELECT ?::integer AS p),
     player_group_id AS (SELECT ?::integer AS p),
     permission AS (SELECT ?::timeline_permission_type AS p)
INSERT INTO timeline_permission (timeline_id, player_group_id, permission)
  VALUES ((SELECT p FROM timeline_id),
          (SELECT p FROM player_group_id),
          (SELECT p FROM permission))
  ON CONFLICT DO NOTHING;

-- name: add-global-timeline-permission
WITH timeline_id AS (SELECT ?::integer AS p),
     permission AS (SELECT ?::timeline_permission_type AS p)
INSERT INTO timeline_permission (timeline_id, permission)
  VALUES ((SELECT p FROM timeline_id),
          (SELECT p FROM permission))
  ON CONFLICT DO NOTHING;



-- name: remove-timeline-permission-from-player
SELECT 'TODO';

-- name: remove-timeline-permission-from-player-group
SELECT 'TODO';

-- name: remove-global-timeline-permission
SELECT 'TODO';
