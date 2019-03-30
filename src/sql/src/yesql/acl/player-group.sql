-- name: update-player-group-name-by-id/acl
-- Sets the name of the player group with the provided ID if the provided player
-- is the owner of that group.
WITH name AS (SELECT ?::text AS p),
     player_group_id AS (select ?::integer AS p) -- TODO test
UPDATE player_group SET name = (SELECT p FROM name)
  WHERE id = (SELECT p FROM player_group_id)
  AND EXISTS (SELECT 1 FROM players_groups
              WHERE player_id = ?
              AND player_group_id = (SELECT p FROM player_group_id)
              AND is_owner = TRUE);

-- name: update-player-group-description-by-id/acl
-- Sets the description of the player group with the provided ID if the provided player
-- is the owner of that group.
WITH description AS (SELECT ?::text AS p),
     player_group_id AS (select ?::integer AS p) -- TODO test
UPDATE player_group SET description = (SELECT p FROM description)
  WHERE id = (SELECT p FROM player_group_id)
  AND EXISTS (SELECT 1 FROM players_groups
              WHERE player_id = ?
              AND player_group_id = (SELECT p FROM player_group_id)
              AND is_owner = TRUE);