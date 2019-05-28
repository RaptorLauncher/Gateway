-- name: select-player-group-by-id @row
-- Returns a player group whose ID matches the argument.
SELECT id, name, description FROM player_group
  WHERE id = ?;

-- name: select-player-groups-by-name
-- Returns an ordered list of player groups whose name is similar to the argument.
-- At most :LIMIT player groups are returned.
SELECT id, name, description FROM player_group
  ORDER BY name <-> ? LIMIT :limit;



-- name: insert-player-group @single
-- Inserts a new player group into the database.
INSERT INTO player_group (name, description)
  VALUES(:name, :description) RETURNING id;



-- name: update-player-group-name-by-id @execute
-- Sets the name of the player group with the provided ID.
UPDATE player_group SET name = ?
  WHERE id = ?;

-- name: update-player-group-description-by-id @execute
-- Sets the description of the player group with the provided ID.
UPDATE player_group SET description = ?
  WHERE id = ?;



-- name: delete-player-group-by-id @execute
-- Deletes the player group with the provided ID.
DELETE FROM player_group WHERE id = ?;
