-- name: insert-player @single
-- Inserts a new player into the database.
INSERT INTO player (login, email, display_name, pass_hash, pass_salt)
  VALUES(:login, lower(:email), :display_name,
         decode(:hash, 'hex'), decode(:salt, 'hex'))
  RETURNING id;



-- name: select-player-by-id @row
-- Selects a player whose ID matches the argument.
SELECT id, login, email, display_name, pass_hash, pass_salt,
       activatedp, creation_time, last_edit_time FROM player
  WHERE id = ? LIMIT 1;

-- name: select-player-by-login @row
-- Selects a player whose login matches the argument.
SELECT id, login, email, display_name, pass_hash, pass_salt,
       activatedp, creation_time, last_edit_time FROM player
  WHERE login = ? LIMIT 1;

-- name: select-player-by-email @row
-- Selects a player whose email matches the argument.
SELECT id, login, email, display_name, pass_hash, pass_salt,
       activatedp, creation_time, last_edit_time FROM player
  WHERE email = lower(?) LIMIT 1;

-- name: select-players-by-display-name
-- Selects an ordered list of players whose display name is similar to the argument.
-- At most :LIMIT players are returned.
SELECT id, login, email, display_name, pass_hash, pass_salt,
       activatedp, creation_time, last_edit_time FROM player
  ORDER BY display_name <-> ? LIMIT :limit;



-- name: update-player-login-by-id @execute
-- Sets the login of the player with the provided ID.
UPDATE player SET login = ?, last_edit_time = now()
  WHERE id = ?;

-- name: update-player-email-by-id @execute
-- Sets the email of the player with the provided ID.
UPDATE player SET email = ?, last_edit_time = now()
  WHERE id = ?;

-- name: update-player-display-name-by-id @execute
-- Sets the display name of the player with the provided ID.
UPDATE player SET display_name = ?, last_edit_time = now()
  WHERE id = ?;

-- name: update-player-password-by-id @execute
-- Sets the password hash and salt of the player with the provided ID.
UPDATE player SET pass_hash = ?, pass_salt = ?, last_edit_time = now()
  WHERE id = ?;

-- name: update-player-activatedp-by-id @execute
-- Sets the activated status of the player with the provided ID.
UPDATE player SET activatedp = ?, last_edit_time = now()
  WHERE id = ?;



-- name: delete-player-by-id @execute
-- Deletes the player with the provided ID.
DELETE FROM player WHERE id = ?;
