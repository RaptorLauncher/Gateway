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
SELECT player_id, player_group_id, timeline_id, permission FROM timeline
 WHERE player_id = ? AND timeline_id = ?;

-- name: select-all-timelines-permissions-of-player-group-and-timeline
-- Selects all permissions for all timelines of a given player group and
-- timeline.
SELECT player_id, player_group_id, timeline_id, permission FROM timeline
 WHERE player_group_id = ? AND timeline_id = ?;
