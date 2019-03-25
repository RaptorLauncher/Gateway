-- Asserts the required extensions are available.
DO $$
  BEGIN
    IF NOT EXISTS(SELECT 1 FROM pg_extension WHERE extname = 'pg_trgm') THEN
      RAISE EXCEPTION 'Extension pg_trgm not installed.';
    END IF;
  END;
$$;



-- Creates the chapter_permission enum type.
CREATE TYPE chapter_permission_type AS ENUM (
  'administer',
  'change_permissions',
  'change_name',
  'view',
  'post_create',
  'post_edit',
  'post_delete',
  'post_rearrange');



-- Creates the timeline_permission enum type.
CREATE TYPE timeline_permission_type AS ENUM (
  'administer',
  'change_permissions',
  'change_name'
  'view',
  'link_within',
  'link_from',
  'link_to');



-- Creates the player table.
CREATE TABLE player (
  id             serial       NOT NULL PRIMARY KEY,
  ------------
  login          varchar(64)  NOT NULL UNIQUE,
  email          varchar(256) NOT NULL UNIQUE,
  name           varchar(256) NOT NULL,
  ------------
  pass_hash      bytea        NOT NULL DEFAULT ''::bytea,
  pass_salt      bytea        NOT NULL DEFAULT ''::bytea,
  activatedp     boolean      NOT NULL DEFAULT FALSE,
  creation_time  timestamp    NOT NULL DEFAULT now(),
  last_edit_time timestamp    NOT NULL DEFAULT now(),
  CONSTRAINT player_login_valid
  CHECK (login ~ '^[a-zA-Z0-9._-]{3,}$'),
  CONSTRAINT player_name_not_empty
  CHECK (name <> ''),
  CONSTRAINT player_email_valid
  CHECK (email ~ '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
  CONSTRAINT player_pass_hash_length
  CHECK (length(pass_hash) <= 256),
  CONSTRAINT player_pass_salt_length
  CHECK (length(pass_salt) <= 256),
  CONSTRAINT player_last_edit_not_before_creation
  CHECK (creation_time <= last_edit_time));



-- Creates the player group table.
CREATE TABLE player_group (
  id          serial NOT NULL PRIMARY KEY,
  ------------
  name        text   NOT NULL,
  ------------
  description text   NOT NULL DEFAULT '',
  CONSTRAINT player_group_name_not_empty
  CHECK (name <> ''));



-- Creates the table binding players to player groups.
CREATE TABLE players_groups (
  player_id       integer NOT NULL REFERENCES player(id)
                          ON UPDATE CASCADE ON DELETE CASCADE,
  player_group_id integer NOT NULL REFERENCES player_group(id)
                          ON UPDATE CASCADE ON DELETE CASCADE,
  ------------
  is_owner        boolean NOT NULL DEFAULT FALSE,
  CONSTRAINT players_groups_primary_key
  PRIMARY KEY (player_id, player_group_id));



-- Creates the persona table.
CREATE TABLE persona (
  id             serial    NOT NULL PRIMARY KEY,
  ------------
  name           text      NOT NULL,
  ------------
  description    text      NOT NULL DEFAULT '',
  creation_time  timestamp NOT NULL DEFAULT now(),
  last_edit_time timestamp NOT NULL DEFAULT now(),
  CONSTRAINT persona_name_not_empty
  CHECK (name <> ''));



-- Creates the table binding players to personas.
CREATE TABLE players_personas (
  player_id  integer NOT NULL REFERENCES player(id)
                     ON UPDATE CASCADE ON DELETE CASCADE,
  persona_id integer NOT NULL REFERENCES persona(id)
                     ON UPDATE CASCADE ON DELETE CASCADE,
  ------------
  is_owner   boolean NOT NULL DEFAULT FALSE,
  CONSTRAINT players_personas_primary_key
  PRIMARY KEY (player_id, persona_id));

-- Assure there may be only one owner of any persona.
CREATE UNIQUE INDEX players_personas_only_one_owner
  ON players_personas (persona_id)
  WHERE is_owner = TRUE;



-- Creates the timeline table.
CREATE TABLE timeline (
  id             serial    NOT NULL PRIMARY KEY,
  ------------
  name           text      NOT NULL,
  description    text      NOT NULL DEFAULT '',
  ------------
  creation_time  timestamp NOT NULL DEFAULT now(),
  last_edit_time timestamp NOT NULL DEFAULT now());



-- Creates the chapter table.
CREATE TABLE chapter (
  id             serial    NOT NULL PRIMARY KEY,
  ------------
  timeline_id    integer   NOT NULL REFERENCES timeline(id)
                           ON UPDATE CASCADE ON DELETE CASCADE,
  name           text      NOT NULL,
  description    text      NOT NULL DEFAULT '',
  ------------
  creation_time  timestamp NOT NULL DEFAULT now(),
  last_edit_time timestamp NOT NULL DEFAULT now());



-- Creates the post table.
CREATE TABLE post (
  id             serial    NOT NULL PRIMARY KEY,
  ------------
  player_id      integer   NULL REFERENCES player(id)
                           ON UPDATE CASCADE ON DELETE SET NULL,
  persona_id     integer   NULL REFERENCES persona(id)
                           ON UPDATE CASCADE ON DELETE SET NULL,
  chapter_id     integer   NOT NULL REFERENCES chapter(id)
                           ON UPDATE CASCADE ON DELETE CASCADE,
  contents       text      NOT NULL,
  ------------
  creation_time  timestamp NOT NULL DEFAULT now(),
  last_edit_time timestamp NOT NULL DEFAULT now(),
  chapter_order  serial    NOT NULL);



-- Creates the chapter link table.
CREATE TABLE chapter_link (
  link_from integer NOT NULL REFERENCES chapter(id)
                    ON UPDATE CASCADE ON DELETE CASCADE,
  link_to   integer NOT NULL REFERENCES chapter(id)
                    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT chapter_link_primary_key
  PRIMARY KEY (link_from, link_to));



-- Creates the chapter permission table.
CREATE TABLE chapter_permission (
  player_id        integer                 NULL REFERENCES player(id)
                                           ON UPDATE CASCADE ON DELETE CASCADE,
  player_group_id  integer                 NULL REFERENCES player_group(id)
                                           ON UPDATE CASCADE ON DELETE CASCADE,
  permission       chapter_permission_type NOT NULL,
  chapter_id       integer                 NOT NULL REFERENCES chapter(id)
                                           ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT chapter_permission_player_or_group
  CHECK (NOT (player_id IS NOT NULL AND player_group_id IS NOT NULL)));

-- Unique partial indices to ensure uniqueness in the chapter permission table.
CREATE UNIQUE INDEX chapter_permission_player_index
  ON chapter_permission (player_id, permission, chapter_id)
  WHERE player_id IS NOT NULL;

CREATE UNIQUE INDEX chapter_permission_player_group_index
  ON chapter_permission (player_group_id, permission, chapter_id)
  WHERE player_group_id IS NOT NULL;

CREATE UNIQUE INDEX chapter_permission_global_index
  ON chapter_permission ( permission, chapter_id)
  WHERE player_id IS NULL AND player_group_id IS NULL;



-- Creates the timeline permission table.
CREATE TABLE timeline_permission (
  player_id       integer                  NULL REFERENCES player(id)
                                           ON UPDATE CASCADE ON DELETE CASCADE,
  player_group_id integer                  NULL REFERENCES player_group(id)
                                           ON UPDATE CASCADE ON DELETE CASCADE,
  permission      timeline_permission_type NOT NULL,
  timeline_id     integer                  NOT NULL REFERENCES timeline(id)
                                           ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT timeline_permission_player_or_group
  CHECK ((player_id IS NULL     AND player_group_id IS NOT NULL) OR
         (player_id IS NOT NULL AND player_group_id IS NULL)));

-- Unique partial indices to ensure uniqueness in the timeline permission table.
CREATE UNIQUE INDEX timeline_permission_player_index
  ON timeline_permission (player_id, permission, timeline_id)
  WHERE player_id IS NOT NULL;

CREATE UNIQUE INDEX timeline_permission_player_group_index
  ON timeline_permission (player_group_id, permission, timeline_id)
  WHERE player_group_id IS NOT NULL;

CREATE UNIQUE INDEX timeline_permission_global_index
  ON timeline_permission (permission, timeline_id)
  WHERE player_id IS NULL AND player_group_id IS NULL;
