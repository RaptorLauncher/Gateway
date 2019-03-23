-- name: insert-persona @single
-- Inserts a new persona into the database.
INSERT INTO persona (name, description)
  VALUES(:name, :description)
  RETURNING id;



-- name: select-persona-by-id @row
-- Selects a persona whose ID matches the argument.
SELECT id, name, description, creation_time, last_edit_time FROM persona
  WHERE id = ? LIMIT 1;

-- name: select-personas-by-name
-- Selects an ordered list of personas whose name matches the argument.
-- At most :LIMIT players are returned.
SELECT id, name, description, creation_time, last_edit_time FROM persona
  ORDER BY name <-> ? LIMIT :limit;



-- name: update-persona-name-by-id @execute
-- Sets the name of the persona with the provided ID.
UPDATE persona SET name = ?, last_edit_time = now()
  WHERE id = ?;

-- name: update-persona-description-by-id @execute
-- Sets the description of the persona with the provided ID.
UPDATE persona SET description = ?, last_edit_time = now()
  WHERE id = ?;



-- name: delete-persona-by-id @execute
-- Deletes the persona with the provided ID.
DELETE FROM persona WHERE id = ?;
