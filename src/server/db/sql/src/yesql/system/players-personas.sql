-- name: select-player-owner-of-persona-p
-- Returns whether the provided player is the owner of the provided persona.
-- Returns NULL if the player has no access whatsoever to the persona.
SELECT COALESCE
  ((SELECT is_owner FROM players_personas
      WHERE player_id = ? AND persona_id = ?));

-- name: select-players-of-persona
-- Returns players who have access to a given persona.
-- One additional column is returned stating whether that player is
-- an owner of that persona.
SELECT p.id, p.login, p.email, p.name, p.pass_hash, p.pass_salt,
       p.activatedp, p.creation_time, p.last_edit_time, g.is_owner
  FROM player AS p, players_personas AS g
  WHERE g.persona_id = ? AND p.id = g.player_id;

-- name: select-personas-of-player
-- Returns personas belonging to a given player.
-- One additional column is returned stating whether that player is
-- an owner of that persona.
SELECT p.id, p.name, p.description, p.creation_time, p.last_edit_time, g.is_owner
  FROM persona as p, players_personas AS g
  WHERE p.id = g.persona_id AND g.player_id = ?;



-- name: add-persona-to-player @execute
-- Adds a persona to a player and specifies whether the player becomes
-- that persona's owner.
INSERT INTO players_personas (player_id, persona_id, is_owner)
  VALUES(?, ?, ?);



-- name: update-persona-owner @execute
-- Sets whether the provided player is the owner of the provided persona.
UPDATE players_personas SET is_owner = ?
  WHERE player_id = ? AND persona_id = ?;



-- name: remove-persona-from-player @execute
-- Removes a persona from a given player.
DELETE FROM players_personas
  WHERE player_id = ? AND persona_id = ?;
