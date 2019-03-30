-- name: add-player-into-player-group-when-owner @execute
-- Adds a player to a player group if the provided player is the owner of that group.
WITH player_id AS (SELECT ?::integer AS p),
     player_group_id AS (SELECT ?::integer AS p),
     is_owner AS (SELECT ?::boolean AS p),
     owner_id AS (SELECT ?::integer AS p)
INSERT INTO players_groups (player_id, player_group_id, is_owner)
  SELECT player_id.p, player_group_id.p, is_owner.p
  FROM player_id, player_group_id, is_owner
  WHERE EXISTS (SELECT 1 FROM players_groups
                WHERE player_id = (SELECT p FROM owner_id)
                AND player_group_id = (SELECT p FROM player_group_id)
                AND is_owner = TRUE);



-- name: update-player-group-name-by-id-when-owner
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

-- name: update-player-group-description-by-id-when-owner
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

-- name: update-player-group-ownerp-by-id-when-owner
-- Sets the owner status of the player group with the provided ID if the provided player
-- is the owner of that group.
WITH is_owner AS (SELECT ?::boolean AS p),
     player_group_id AS (select ?::integer AS p) -- TODO test
UPDATE player_group SET is_owner = (SELECT p FROM is_owner)
  WHERE id = (SELECT p FROM player_group_id)
  AND EXISTS (SELECT 1 FROM players_groups
              WHERE player_id = ?
              AND player_group_id = (SELECT p FROM player_group_id)
              AND is_owner = TRUE);



-- name: upsert-player-group-owner-when-owner
-- Sets whether the provided player is the owner of the provided player group
-- if the second provided player is the owner of the second provided group.
-- TODO the third and fifth argument MUST be the same
-- TODO move upsert tests into separate test suite
WITH is_owner AS (SELECT ?::boolean AS p),
     player_id AS (SELECT ?::integer AS p),
     -- TODO remove casts when postmodern fixes #194
     player_group_id AS (SELECT ?::integer AS p)
INSERT INTO players_groups (is_owner, player_id, player_group_id)
  SELECT is_owner.p, player_id.p, player_group_id.p
  FROM is_owner, player_id, player_group_id
  WHERE EXISTS (SELECT 1 FROM players_groups
                WHERE player_id = ? AND player_group_id = ?
                -- TODO modify the above to use player_group_id.p
                AND is_owner = TRUE)
  ON CONFLICT (player_id, player_group_id) DO UPDATE
  SET is_owner = (SELECT p FROM is_owner);
