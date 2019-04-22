-- Drops all Gateway tables from the database.
DROP TABLE IF EXISTS
  gateway_error,
  player, player_group, players_groups,
  persona, players_personas,
  timeline, chapter, post, chapter_links,
  chapters_permissions, timelines_permissions
CASCADE;



-- Drops all Gateway enum types from the database.
DROP TYPE IF EXISTS
  chapter_permission_type,
  timeline_permission_type
CASCADE;



-- Drops all Gateway sequences.
DROP SEQUENCE IF EXISTS
  gateway_error_id
CASCADE;



-- Drops all Gateway functions.
DROP FUNCTION IF EXISTS
  base36_encode, base36_decode,
  raise_error, generate_gateway_error
CASCADE;



-- Drops all generated Gateway functions.
DO $$
DECLARE
  sql text;
  dropped int;
BEGIN
  SELECT count(*)::int, 'DROP FUNCTION ' || string_agg(oid::regprocedure::text, '; DROP FUNCTION ')
  FROM pg_proc
    WHERE proname ='gateway_error' AND pg_function_is_visible(oid)
  INTO dropped, sql;
  IF dropped > 0 THEN
    EXECUTE sql;
  END IF;
END;
$$ LANGUAGE plpgsql;
