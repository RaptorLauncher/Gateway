-- name: insert-player @single
-- Inserts a new player into the database.
INSERT INTO player (login, email, display_name, pass_hash, pass_salt)
    VALUES(:login, lower(:email), :display_name,
           decode(:hash, 'hex'), decode(:salt, 'hex'))
    RETURNING player_id;



-- name: select-player-by-login @row
-- Retrieves a player whose login matches the argument.
SELECT * from player
    WHERE login = ?;

-- name: select-player-by-email @row
-- Retrieves a player whose email matches the argument.
SELECT * from player
    WHERE email = lower(?);

-- name: select-players-by-display-name
-- Retrieves all players whose display name is similar to the argument.
-- TODO https://www.rdegges.com/2013/easy-fuzzy-text-searching-with-postgresql/
SELECT * from player
    WHERE display_name ~ ?;



-- name: update-player-login-by-id @execute
-- Sets the login of the player with the provided ID.
UPDATE player SET login = ?, player_last_edit_time = now()
    WHERE player_id = ?;

-- name: update-player-email-by-id @execute
-- Sets the email of the player with the provided ID.
UPDATE player SET email = ?, player_last_edit_time = now()
    WHERE player_id = ?;

-- name: update-player-display-name-by-id @execute
-- Sets the display name of the player with the provided ID.
UPDATE player SET display_name = ?, player_last_edit_time = now()
    WHERE player_id = ?;

-- name: update-player-password-by-id @execute
-- Sets the password hash and salt of the player with the provided ID.
UPDATE player SET pass_hash = ?, pass_salt = ?, player_last_edit_time = now()
    WHERE player_id = ?;

-- name: update-player-activatedp-by-id @execute
-- Sets the activated status of the player with the provided ID.
UPDATE player SET activatedp = ?, player_last_edit_time = now()
    WHERE player_id = ?;



-- name: delete-player-by-id @execute
-- Deletes the player with the provided ID.
DELETE FROM player WHERE player_id = ?;
