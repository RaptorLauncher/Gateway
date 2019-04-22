-- name: select-timeline-by-id @row
-- Selects a timeline whose ID matches the argument.
SELECT id, name, description, creation_time, last_edit_time FROM timeline
  WHERE id = ? LIMIT 1;

-- name: select-timelines-by-name
-- Selects an ordered list of timelines whose name matches the argument.
-- At most :LIMIT players are returned.
SELECT id, name, description, creation_time, last_edit_time FROM timeline
  ORDER BY name <-> ? LIMIT :limit;



-- name: insert-timeline @single
-- Inserts a new timeline into the database.
INSERT INTO timeline (name, description)
  VALUES(:name, :description)
  RETURNING id;



-- name: update-timeline-name-by-id @execute
-- Sets the name of the timeline with the provided ID.
-- TODO update-timeline-name-by-id-when-owner
UPDATE timeline SET name = ?, last_edit_time = now()
  WHERE id = ?;

-- name: update-timeline-description-by-id @execute
-- Sets the description of the timeline with the provided ID.
-- TODO update-timeline-description-by-id-when-owner
UPDATE timeline SET description = ?, last_edit_time = now()
  WHERE id = ?;



-- name: delete-timeline-by-id @execute
-- Deletes the timeline with the provided ID.
DELETE FROM timeline WHERE id = ?;
