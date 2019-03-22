-- name: insert-player-group @single
-- Inserts a new player group into the database.
INSERT INTO player_group (name)
  VALUES(:name) RETURNING id;



-- name: select-player-group-by-id @row
-- Selects a player group whose ID matches the argument.
SELECT * from player_group
  WHERE id = ?;

-- name: select-player-groups-by-name
-- Selects player groups whose name is similar to the argument.
-- TODO https://www.rdegges.com/2013/easy-fuzzy-text-searching-with-postgresql/
SELECT * from player_group
  WHERE name ~ ?;



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
