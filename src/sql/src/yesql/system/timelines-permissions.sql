-- name: select-all-timelines-permissions-of-player
-- Selects all timeline permissions of a given player.
-- This includes global permissions.
-- TODO everywhere: /acl variant that only lists timelines the player can actually see
SELECT player_id, player_group_id, timeline_id, permission FROM timeline
  WHERE player_id = ? OR (player_id IS NULL AND player_group_id IS NULL);

-- name: select-all-timelines-permissions-of-player-group
-- Selects all timeline permissions of a given player group.
-- This includes global permissions.
SELECT player_id, player_group_id, timeline_id, permission FROM timeline
  WHERE player_group_id = ? OR (player_id IS NULL AND player_group_id IS NULL);

-- name: select-all-timelines-permissions-of-timeline
-- Selects all timeline permissions of a given timeline.
SELECT player_id, player_group_id, timeline_id, permission FROM timeline
  WHERE timeline_id = ?;

-- name: select-all-timelines-permissions-of-player-and-timeline
-- Selects all permissions for all timelines of a given player and timeline.
SELECT 'TODO';

-- name: select-all-timelines-permissions-of-player-group-and-timeline
-- Selects all permissions for all timelines of a given player group and
-- timeline.
SELECT 'TODO';

-- name: player-timeline-permission-p
-- Checks if the player has the provided permission on the provided timeline.
-- The permission may be global or belong to one of their player groups.
-- TODO move to acl/
WITH player_id AS (SELECT ?::integer AS p),
     timeline_id AS (SELECT ?::integer AS p),
     permission AS (SELECT ?::timeline_permission_type AS p)
SELECT CASE
  WHEN NOT EXISTS(SELECT 1 FROM player
                  WHERE player.id = (SELECT p FROM player_id))
    THEN gateway_error('no_such_user_id', (SELECT p FROM player_id))::text = ''
  WHEN NOT EXISTS(SELECT 1 FROM timeline
                  WHERE timeline.id = (SELECT p FROM timeline_id))
    THEN gateway_error('no_such_timeline_id', (SELECT p FROM timeline_id))::text = ''
  ELSE EXISTS(
    SELECT 1 FROM timelines_permissions AS p, players_groups AS g
      WHERE p.timeline_id = (SELECT p FROM timeline_id)
      AND (p.permission = (SELECT p FROM permission)
           OR p.permission = 'administer'::timeline_permission_type)
      AND ((p.player_id IS NULL AND p.player_group_id IS NULL)
           OR (p.player_id = (SELECT p FROM player_id))
           OR (g.player_id = (SELECT p FROM player_id)
               AND p.player_group_id = g.player_group_id)))
END;
