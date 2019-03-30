-- Inserts dummy data into the database.
INSERT INTO player (id, login, email, name,
                    activatedp, pass_hash, pass_salt,
                    creation_time, last_edit_time)
  VALUES (1, 'player1', 'player1@gate.way', 'Player 1',
          TRUE,  decode('01', 'hex'), decode('01', 'hex'),
          now() - INTERVAL '1 HOUR', now()),
         (2, 'player2', 'player2@gate.way', 'Player 2',
          FALSE, decode('02', 'hex'), decode('02', 'hex'),
          now() - INTERVAL '1 HOUR', now()),
         (3, 'player3', 'player3@gate.way', 'Player 3',
          TRUE,  decode('03', 'hex'), decode('03', 'hex'),
          now() - INTERVAL '1 HOUR', now()),
         (4, 'player4', 'player4@gate.way', 'Player 4',
          FALSE, decode('04', 'hex'), decode('04', 'hex'),
          now() - INTERVAL '1 HOUR', now()),
         (5, 'player5', 'player5@gate.way', 'Player 5',
          TRUE,  decode('05', 'hex'), decode('05', 'hex'),
          now() - INTERVAL '1 HOUR', now()),
         (6, 'player6', 'player6@gate.way', 'Player 6',
          FALSE, decode('06', 'hex'), decode('06', 'hex'),
          now() - INTERVAL '1 HOUR', now()),
         (7, 'player7', 'player7@gate.way', 'Player 7',
          TRUE,  decode('07', 'hex'), decode('07', 'hex'),
          now() - INTERVAL '1 HOUR', now()),
         (8, 'player8', 'player8@gate.way', 'Player 8',
          FALSE, decode('08', 'hex'), decode('08', 'hex'),
          now() - INTERVAL '1 HOUR', now());

INSERT INTO player_group (id, name, description)
  VALUES (1, 'Group 1', 'Group 1 (1234) (1)'),
         (2, 'Group 2', 'Group 2 (345) (345)'),
         (3, 'Group 3', 'Group 3 (2468) (2)');

INSERT INTO players_groups (player_id, player_group_id, is_owner)
  VALUES (1, 1, TRUE), (2, 1, FALSE), (3, 1, FALSE), (4, 1, FALSE),
         (3, 2, TRUE), (4, 2, TRUE),  (5, 2, TRUE),
         (2, 3, TRUE), (4, 3, FALSE), (6, 3, FALSE), (8, 3, FALSE);

INSERT INTO persona (id, name, description, creation_time, last_edit_time)
  VALUES (1, 'Persona 1', 'Persona 1', now() - INTERVAL '1 HOUR', now()),
         (2, 'Persona 2', 'Persona 2', now() - INTERVAL '1 HOUR', now()),
         (3, 'Persona 3', 'Persona 3', now() - INTERVAL '1 HOUR', now()),
         (4, 'Persona 4', 'Persona 4', now() - INTERVAL '1 HOUR', now()),
         (5, 'Persona 5', 'Persona 5', now() - INTERVAL '1 HOUR', now()),
         (6, 'Persona 6', 'Persona 6', now() - INTERVAL '1 HOUR', now()),
         (7, 'Persona 7', 'Persona 7', now() - INTERVAL '1 HOUR', now()),
         (8, 'Persona 8', 'Persona 8', now() - INTERVAL '1 HOUR', now());

INSERT INTO players_personas (player_id, persona_id, is_owner)
  VALUES (1, 1, TRUE),  (1, 2, TRUE),  (1, 3, TRUE), (1, 4, FALSE),
         (2, 4, TRUE),  (2, 5, TRUE),  (2, 6, TRUE),
         (3, 1, FALSE), (3, 2, FALSE),
         (4, 1, FALSE);

INSERT INTO timeline (id, name, description, creation_time, last_edit_time)
  VALUES (1, 'Timeline 1', 'Timeline 1',  now() - INTERVAL '1 HOUR', now()),
         (2, 'Timeline 2', 'Timeline 2',  now() - INTERVAL '1 HOUR', now()),
         (3, 'Timeline 3', 'Timeline 3',  now() - INTERVAL '1 HOUR', now()),
         (4, 'Timeline 4', 'Timeline 4',  now() - INTERVAL '1 HOUR', now()),
         (5, 'Timeline 5', 'Timeline 5',  now() - INTERVAL '1 HOUR', now()),
         (6, 'Timeline 6', 'Timeline 6',  now() - INTERVAL '1 HOUR', now());

INSERT INTO timeline_permission (player_id, timeline_id, permission)
  VALUES (1, 1, 'administer'),
         (2, 2, 'change_permissions'),
         (3, 2, 'change_name'),
         (4, 2, 'view'),
         (5, 2, 'link_within'),
         (6, 2, 'link_from'),
         (7, 2, 'link_to');

INSERT INTO timeline_permission (player_group_id, timeline_id, permission)
  VALUES (1, 3, 'administer'),
         (2, 4, 'change_permissions'),
         (2, 4, 'change_name'),
         (2, 4, 'view'),
         (2, 4, 'link_within'),
         (2, 4, 'link_from'),
         (2, 4, 'link_to');

INSERT INTO timeline_permission (timeline_id, permission)
  VALUES (5, 'view');
