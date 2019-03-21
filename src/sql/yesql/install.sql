-- name: create-chapter-permission @execute
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

-- name: create-timeline-permission @execute
-- Creates the timeline_permission enum type.
CREATE TYPE timeline_permission_type AS ENUM (
    'administer',
    'change_permissions',
    'change_name'
    'view',
    'link_within',
    'link_from',
    'link_to');

-- name: create-table-player @execute
-- Creates the player table.
CREATE TABLE player (
    login                  text      NOT NULL UNIQUE,
    email                  text      NOT NULL UNIQUE,
    display_name           text      NOT NULL,
    pass_hash              bytea     NOT NULL,
    pass_salt              bytea     NOT NULL,

    activatedp             boolean   NOT NULL DEFAULT FALSE,
    player_creation_time   timestamp NOT NULL DEFAULT now(),
    player_last_edit_time  timestamp NOT NULL DEFAULT now(),
    player_id              serial    NOT NULL PRIMARY KEY,
    CONSTRAINT player_name_valid
    CHECK (login ~ '^[a-zA-Z0-9]{3,}$'),
    CONSTRAINT display_name_not_empty
    CHECK (display_name <> ''),
    CONSTRAINT email_valid
    CHECK (email ~ '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'));

-- name: create-table-player-group @execute
-- Creates the player group table.
CREATE TABLE player_group (
    player_group_name text   NOT NULL,

    player_group_id   serial NOT NULL PRIMARY KEY);

-- name: create-table-players-groups @execute
-- Creates the table bindings players to groups.
CREATE TABLE players_groups (
    player_id       integer NOT NULL REFERENCES player(player_id)
                            ON UPDATE CASCADE ON DELETE CASCADE,
    player_group_id integer NOT NULL REFERENCES player_group(player_group_id)
                            ON UPDATE CASCADE ON DELETE CASCADE,

    is_owner        boolean NOT NULL DEFAULT FALSE,
    CONSTRAINT players_groups_primary_key
    PRIMARY KEY (player_id, player_group_id));

-- name: create-table-persona @execute
-- Creates the persona table.
CREATE TABLE persona (
    persona_name           text      NOT NULL,

    description            text      NOT NULL DEFAULT '',
    persona_creation_time  timestamp NOT NULL DEFAULT now(),
    persona_last_edit_time timestamp NOT NULL DEFAULT now(),
    persona_id             serial    NOT NULL PRIMARY KEY);

-- name: create-table-owners-borrowers @execute
-- Creates the owners and borrowers table.
CREATE TABLE owners_borrowers (
    player_id  integer NOT NULL DEFAULT 1 REFERENCES player(player_id)
                       ON UPDATE CASCADE ON DELETE SET DEFAULT,
    persona_id integer NOT NULL REFERENCES persona(persona_id)
                       ON UPDATE CASCADE ON DELETE CASCADE,

    is_owner   boolean NOT NULL DEFAULT FALSE,
    CONSTRAINT owners_borrowers_primary_key
    PRIMARY KEY (player_id, persona_id),
    CONSTRAINT owners_borrowers_narrator_not_borrower
    CHECK (NOT (player_id = 1 AND is_owner IS FALSE)));

-- name: create-table-timeline @execute
-- Creates the timeline table.
CREATE TABLE timeline (
    timeline_name text                NOT NULL,

    timeline_creation_time  timestamp NOT NULL DEFAULT now(),
    timeline_last_edit_time timestamp NOT NULL DEFAULT now(),
    timeline_id             serial    NOT NULL PRIMARY KEY);

-- name: create-table-chapter @execute
-- Creates the chapter table.
CREATE TABLE chapter (
    chapter_name           text      NOT NULL,
    timeline_id            integer   NOT NULL REFERENCES timeline(timeline_id)
                                     ON UPDATE CASCADE ON DELETE CASCADE,

    chapter_creation_time  timestamp NOT NULL DEFAULT now(),
    chapter_last_edit_time timestamp NOT NULL DEFAULT now(),
    chapter_id             serial    NOT NULL PRIMARY KEY);

-- name: create-table-post @execute
-- Creates the post table
CREATE TABLE post (
    contents            text      NOT NULL,
    player_id           integer   NOT NULL REFERENCES player(player_id)
                                  ON UPDATE CASCADE,
    persona_id          integer   NOT NULL REFERENCES persona(persona_id)
                                  ON UPDATE CASCADE,
    chapter_id          integer   NOT NULL REFERENCES chapter(chapter_id)
                                  ON UPDATE CASCADE,

    post_creation_time  timestamp NOT NULL DEFAULT now(),
    post_last_edit_time timestamp NOT NULL DEFAULT now(),
    post_id             serial    NOT NULL PRIMARY KEY,
    chapter_order       serial    NOT NULL);

-- name: create-table-chapter-link @execute
-- Creates the chapter link table.
CREATE TABLE chapter_link (
    link_from integer NOT NULL REFERENCES chapter(chapter_id)
                      ON UPDATE CASCADE,
    link_to   integer NOT NULL REFERENCES chapter(chapter_id)
                      ON UPDATE CASCADE,
    CONSTRAINT chapter_link_primary_key
    PRIMARY KEY (link_from, link_to));

-- name: create-table-chapter-permission @execute
-- Creates the chapter permission table.
CREATE TABLE chapter_permission (
    player_id        integer                 REFERENCES player(player_id)
                                             ON UPDATE CASCADE,
    player_group_id  integer                 REFERENCES player_group(player_group_id)
                                             ON UPDATE CASCADE,
    permission       chapter_permission_type NOT NULL,
    chapter_id       integer                 NOT NULL REFERENCES chapter(chapter_id)
                                             ON UPDATE CASCADE,
    CONSTRAINT chapter_permission_player_or_group
    CHECK ((player_id IS NULL     AND player_group_id IS NOT NULL) OR
           (player_id IS NOT NULL AND player_group_id IS NULL)));

-- name: create-table-chapter-permission-index-1 @execute
-- Creates the first unique index of the chapter permission table.
CREATE UNIQUE INDEX chapter_permission_player_index
    ON chapter_permission (player_id, permission, chapter_id)
    WHERE player_id IS NOT NULL;

-- name: create-table-chapter-permission-index-2 @execute
-- Creates the second unique index of the chapter permission table.
CREATE UNIQUE INDEX chapter_permission_player_group_index
    ON chapter_permission (player_group_id, permission, chapter_id)
    WHERE player_group_id IS NOT NULL;

-- name: create-table-timeline-permission @execute
-- Creates the timeline permission table.
CREATE TABLE timeline_permission (
    player_id       integer                  REFERENCES player(player_id)
                                             ON UPDATE CASCADE,
    player_group_id integer                  REFERENCES player_group(player_group_id)
                                             ON UPDATE CASCADE,
    permission      timeline_permission_type NOT NULL,
    timeline_id     integer                  NOT NULL REFERENCES timeline(timeline_id)
                                             ON UPDATE CASCADE,
    CONSTRAINT timeline_permission_player_or_group
    CHECK ((player_id IS NULL     AND player_group_id IS NOT NULL) OR
           (player_id IS NOT NULL AND player_group_id IS NULL)));

-- name: create-table-timeline-permission-index-1 @execute
-- Creates the first unique index of the timeline permission table.
CREATE UNIQUE INDEX timeline_permission_player_index
    ON timeline_permission (player_id, permission, timeline_id)
    WHERE player_id IS NOT NULL;

-- name: create-table-timeline-permission-index-2 @execute
-- Creates the second unique index of the timeline permission table.
CREATE UNIQUE INDEX timeline_permission_player_group_index
    ON timeline_permission (player_group_id, permission, timeline_id)
    WHERE player_group_id IS NOT NULL;

-- name: set-initial-data @execute
-- Fills the database with the initial data.
BEGIN;
    INSERT INTO player(player_id, login, email, display_name, pass_hash, pass_salt)
        VALUES (1, 'narrator', 'narrator@gateway.localhost', 'Narrator', ''::bytea, ''::bytea);
    INSERT INTO persona(persona_id, persona_name)
        VALUES (1, 'Narrator');
    INSERT INTO player_group(player_group_id, player_group_name)
        VALUES (1, 'Everyone');
    INSERT INTO players_groups(player_id, player_group_id)
        VALUES (1, 1);
    INSERT INTO owners_borrowers(player_id, persona_id, is_owner)
        VALUES (1, 1, TRUE);
COMMIT;
