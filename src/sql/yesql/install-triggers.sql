-- Create the default and immutable Everyone player group.
INSERT INTO player_group(player_group_id, player_group_name)
  VALUES (0, 'Everyone');

CREATE FUNCTION player_group_ensure_everyone() RETURNS trigger AS $$
  BEGIN
    IF OLD.player_group__id = 0 THEN
      RAISE EXCEPTION 'Cannot modify the default Everyone player group.' USING ERRCODE = 'GW001';
    END IF;
    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER player_group_ensure_everyone BEFORE UPDATE OR DELETE ON player_group
  FOR EACH ROW EXECUTE PROCEDURE player_group_ensure_everyone();



-- Create the default and immutable Narrator player.
INSERT INTO player(player_id, login, email, display_name, pass_hash, pass_salt)
  VALUES (0, 'narrator', 'narrator@gateway.localhost', 'Narrator', ''::bytea, ''::bytea);

INSERT INTO players_groups(player_id, player_group_id, is_owner)
  VALUES(0, 0, TRUE);

CREATE FUNCTION player_ensure_narrator() RETURNS trigger AS $$
  BEGIN
    IF OLD.player_id = 0 THEN
      RAISE EXCEPTION 'Cannot modify the default Narrator player.' USING ERRCODE = 'GW001';
    END IF;
    RETURN OLD;
  END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER player_ensure_narrator BEFORE UPDATE OR DELETE ON player
  FOR EACH ROW EXECUTE PROCEDURE player_ensure_narrator();



-- Add all players into the Everyone group after they are created.
CREATE FUNCTION player_group_insert_into_everyone() RETURNS trigger AS $$
  BEGIN
    INSERT INTO players_groups(player_id, player_group_id)
      VALUES(NEW.player_id, 0);
    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER player_group_insert_into_everyone AFTER INSERT ON player
  FOR EACH ROW EXECUTE PROCEDURE player_group_insert_into_everyone();



-- All players must be a part of the Everyone group.
CREATE FUNCTION players_groups_never_remove_everyone() RETURNS trigger AS $$
  BEGIN
    IF (OLD.player_group_id = 0) AND
       (EXISTS(SELECT 1 FROM player WHERE player.player_id = OLD.player_id)) THEN
      RAISE EXCEPTION 'Cannot remove a player from the Everyone group.' USING ERRCODE = 'GW001';
    END IF;
    RETURN OLD;
  END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER players_groups_never_remove_everyone AFTER UPDATE OR DELETE ON players_groups
  FOR EACH ROW EXECUTE PROCEDURE players_groups_never_remove_everyone();



-- Create the default and immutable Narrator persona.
INSERT INTO persona(persona_id, persona_name)
  VALUES (0, 'Narrator');

INSERT INTO owners_borrowers(player_id, persona_id, is_owner)
  VALUES (0, 0, TRUE);

CREATE FUNCTION persona_ensure_narrator() RETURNS trigger AS $$
  BEGIN
    IF OLD.persona_id = 0 THEN
      RAISE EXCEPTION 'Cannot modify the default Narrator persona.' USING ERRCODE = 'GW001';
    END IF;
    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER persona_ensure_narrator BEFORE UPDATE OR DELETE ON persona
  FOR EACH ROW EXECUTE PROCEDURE persona_ensure_narrator();
