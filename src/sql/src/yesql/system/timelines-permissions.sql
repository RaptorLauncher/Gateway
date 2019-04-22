-- name: select-all-timelines-permissions-of-player
-- Selects all timeline permissions of a given player.
SELECT 'TODO';

-- name: select-all-timelines-permissions-of-player-group
-- Selects all timeline permissions of a given player group.
SELECT 'TODO';

-- name: select-all-timelines-permissions-of-timeline
-- Selects all timeline permissions of a given timeline.
SELECT 'TODO';

-- name: select-all-timelines-permissions-of-player-and-timeline
-- Selects all permissions for all timelines of a given player and timeline.
SELECT 'TODO';

-- name: select-all-timelines-permissions-of-player-group-and-timeline
-- Selects all permissions for all timelines of a given player group and
-- timeline.
SELECT 'TODO';

-- name: player-timeline-permission-p
-- Checks if the player or any of the player groups he participates in has the
-- provided timeline permission.
WITH player_id AS (SELECT ?::integer AS p),
     timeline_id AS (SELECT ?::integer AS p),
     permission AS (SELECT ?::timeline_permission_type AS p)
SELECT CASE
  WHEN EXISTS (SELECT 1 FROM timelines_permissions AS p
               WHERE p.player_id = (SELECT p FROM player_id)
               AND p.timeline_id = (SELECT p FROM timeline_id)
               AND (p.permission = (SELECT p FROM permission)
                    OR p.permission = 'administer'::timeline_permission_type))
    THEN TRUE
  WHEN EXISTS (SELECT 1 FROM timelines_permissions AS p, players_groups AS g
               WHERE g.player_id = (SELECT p FROM player_id)
               AND p.player_group_id = g.player_group_id
               AND p.timeline_id = (SELECT p FROM timeline_id)
               AND (p.permission = (SELECT p FROM permission)
                    OR p.permission = 'administer'::timeline_permission_type))
    THEN TRUE
  ELSE FALSE
END;
