-- Inserts dummy data into the database.
INSERT INTO player (login, email, name, activatedp)
  VALUES ('player1', 'player1@gate.way', 'Player 1', TRUE),
         ('player2', 'player2@gate.way', 'Player 2', FALSE),
         ('player3', 'player3@gate.way', 'Player 3', TRUE),
         ('player4', 'player4@gate.way', 'Player 4', FALSE),
         ('player5', 'player5@gate.way', 'Player 5', TRUE),
         ('player6', 'player6@gate.way', 'Player 6', FALSE),
         ('player7', 'player7@gate.way', 'Player 7', TRUE),
         ('player8', 'player8@gate.way', 'Player 8', FALSE);

INSERT INTO player_group (name, description)
  VALUES ('Group 1', 'Group 1 (1234) (1)'),
         ('Group 2', 'Group 2 (345) (345)'),
         ('Group 3', 'Group 3 (2468) (2)');

INSERT INTO players_groups (player_id, player_group_id, is_owner)
  VALUES (1, 1, TRUE), (2, 1, FALSE), (3, 1, FALSE), (4, 1, FALSE),
         (3, 2, TRUE), (4, 2, TRUE),  (5, 2, TRUE),
         (2, 3, TRUE), (4, 3, FALSE), (6, 3, FALSE), (8, 3, FALSE);

INSERT INTO persona (name, description, creation_time, last_edit_time)
  VALUES ('Persona 1', 'Persona 1', now() - INTERVAL '1 HOUR', now()),
         ('Persona 2', 'Persona 2', now() - INTERVAL '1 HOUR', now()),
         ('Persona 3', 'Persona 3', now() - INTERVAL '1 HOUR', now()),
         ('Persona 4', 'Persona 4', now() - INTERVAL '1 HOUR', now()),
         ('Persona 5', 'Persona 5', now() - INTERVAL '1 HOUR', now()),
         ('Persona 6', 'Persona 6', now() - INTERVAL '1 HOUR', now()),
         ('Persona 7', 'Persona 7', now() - INTERVAL '1 HOUR', now()),
         ('Persona 8', 'Persona 8', now() - INTERVAL '1 HOUR', now());

INSERT INTO players_personas (player_id, persona_id, is_owner)
  VALUES (1, 1, TRUE), (1, 2, TRUE), (1, 3, TRUE), (1, 4, FALSE),
         (2, 4, TRUE), (2, 5, TRUE), (2, 6, TRUE),
         (3, 1, FALSE), (3, 2, FALSE),
         (4, 1, FALSE);