-- name: add-player-into-player-group @execute
-- Adds a player to a player group.
INSERT INTO players_groups (player_id, player_group_id, is_owner)
  VALUES(?, ?, ?);



-- name: select-player-owner-of-group-p @single
-- Returns whether the provided player is the owner of the provided group.
-- Returns NULL if the player is not a member of the group.
SELECT COALESCE
  ((SELECT is_owner FROM players_groups
      WHERE player_id = ? AND player_group_id = ?));

-- name: select-players-belonging-to-group
-- Returns all players belonging to the group with the provided ID.
-- One additional column is returned stating whether that player is
-- an owner of that group.
SELECT p.id, p.login, p.email, p.name, p.pass_hash, p.pass_salt,
       p.activatedp, p.creation_time, p.last_edit_time, g.is_owner
  FROM player AS p, players_groups AS g
  WHERE g.player_group_id = ? AND g.player_id = p.id;

-- name: select-groups-player-belongs-to
-- Returns all groups the player with the provided ID belongs to.
-- One additional column is returned stating whether that player is
-- an owner of that group.
SELECT p.id, p.name, p.description, g.is_owner
  FROM player_group AS p, players_groups AS g
  WHERE g.player_id = ? AND g.player_group_id = p.id;



-- name: update-player-group-owner @execute
-- Sets whether the provided player is the owner of the provided player group.
-- TODO ?foo argument names for functions
UPDATE players_groups SET is_owner = ?
  WHERE player_id = ? AND player_group_id = ?;



-- name: upsert-player-group-owner-when-owner
-- Sets whether the provided player is the owner of the provided player group
-- if the second provided player is the owner of the second provided group.
-- TODO the third and fifth argument MUST be the same
WITH is_owner AS (SELECT ?::boolean AS p),
     player_id AS (SELECT ?::integer AS p),
     player_group_id AS (SELECT ?::integer AS p)
INSERT INTO players_groups (is_owner, player_id, player_group_id)
  SELECT is_owner.p, player_id.p, player_group_id.p
  FROM is_owner, player_id, player_group_id
  WHERE EXISTS (SELECT 1 FROM players_groups
                WHERE player_id = ? AND player_group_id = ?
                AND is_owner = TRUE)
  ON CONFLICT (player_id, player_group_id) DO UPDATE
  SET is_owner = (SELECT p FROM is_owner);



-- name: remove-player-from-player-group @execute
-- Removes a player from a player group.
DELETE FROM players_groups
  WHERE player_id = ? AND player_group_id = ?;
