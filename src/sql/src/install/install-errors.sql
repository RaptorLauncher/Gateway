-- TODO create utils.sql

-- The following two functions are © Jamie Begin, 2010.
-- source: http://www.jamiebegin.com/base36-conversion-in-postgresql/

CREATE OR REPLACE FUNCTION base36_encode(IN digits bigint, IN min_width int = 0)
  RETURNS varchar AS $$
  DECLARE
    chars char[];
    ret varchar;
    val bigint;
  BEGIN
    chars := ARRAY['0','1','2','3','4','5','6','7','8','9'
      ,'A','B','C','D','E','F','G','H','I','J','K','L','M'
      ,'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'];
    val := digits;
    ret := '';
    IF val < 0 THEN
      val := val * -1;
    END IF;
    WHILE val != 0 LOOP
      ret := chars[(val % 36)+1] || ret;
      val := val / 36;
    END LOOP;
    IF min_width > 0 AND char_length(ret) < min_width THEN
      ret := lpad(ret, min_width, '0');
    END IF;
    RETURN ret;
  END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION base36_decode(IN base36 varchar)
  RETURNS bigint AS $$
  DECLARE
    a char[];
    ret bigint;
    i int;
    val int;
    chars varchar;
  BEGIN
    chars := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    FOR i IN REVERSE char_length(base36)..1 LOOP
      a := a || substring(upper(base36) FROM i FOR 1)::char;
    END LOOP;
    i := 0;
    ret := 0;
    WHILE i < (array_length(a,1)) LOOP
      val := position(a[i+1] IN chars)-1;
      ret := ret + (val * (36 ^ i));
      i := i + 1;
    END LOOP;
    RETURN ret;
  END;
$$ LANGUAGE plpgsql IMMUTABLE;



-- Creates the Gateway error table sequence.
CREATE SEQUENCE gateway_error_id START 1;

-- Creates the Gateway error table.
CREATE TABLE gateway_error (
  id       varchar(5) NOT NULL PRIMARY KEY
                      DEFAULT 'GW' || base36_encode(nextval('gateway_error_id'), 3),
  ------------
  name     text       NOT NULL UNIQUE,
  reason   text       NOT NULL,
  ------------
  argcount int        NOT NULL DEFAULT 0,
  CONSTRAINT gateway_error_id_valid
  CHECK (id ~ '^GW[A-Z0-9]{3}$'),
  CONSTRAINT gateway_error_name_valid
  CHECK (id ~ '^GW[a-z0-9_]+$'),
  CONSTRAINT gateway_error_reason_not_empty
  CHECK (reason <> ''));

-- Inserts error data into the database.
INSERT INTO gateway_error (name, argcount, reason)
  VALUES
  ('general_error', 0,
   'A general Gateway database error has been signaled.'),
  ------------
  ('gateway_error_not_found', 1,
   'Database function gateway_error called with unknown error %I.'),
  ------------
  ('gateway_error_argcount_invalid', 2,
   'Database function gateway_error called with invalid argument count %I ' ||
   'for error name %I.'),
  ------------
  ('user_does_not_exist', 1,
   'No user with the login %I exists in the system.');

-- Creates the function signaling Gateway errors.
CREATE OR REPLACE FUNCTION raise_error(IN message text, IN id text)
  RETURNS void AS $$
  BEGIN
    RAISE EXCEPTION USING MESSAGE = message, ERRCODE = id;
  END;
$$ LANGUAGE plpgsql STABLE;

-- Creates the function signaling Gateway errors.
CREATE OR REPLACE FUNCTION gateway_error_int(IN error_name text, VARIADIC format_args text[])
  RETURNS void AS $$
  SELECT
    CASE
      WHEN NOT EXISTS (SELECT 1 FROM gateway_error e
                       WHERE e.name = error_name)
        THEN gateway_error_int('gateway_error_not_found', VARIADIC ARRAY [error_name])
      WHEN array_upper(format_args, 1) <>
                       (SELECT argcount FROM gateway_error e WHERE e.name = error_name)
        THEN gateway_error_int('gateway_error_argcount_invalid',
                         VARIADIC ARRAY
                         [error_name, (SELECT argcount FROM gateway_error e
                                       WHERE e.name = error_name)::text])
      ELSE
        raise_error(format((SELECT reason FROM gateway_error e WHERE e.name = error_name),
                           VARIADIC format_args),
                    (SELECT id FROM gateway_error e WHERE e.name = error_name)::text)
    END;
$$ LANGUAGE sql STABLE;

-- Generates the overloads for the error-signaling function.
CREATE OR REPLACE FUNCTION generate_gateway_error() RETURNS SETOF text AS $A$
  WITH RECURSIVE t(i) AS (
    VALUES ('integer'), ('boolean'), ('text')
  ), cte AS (
    SELECT '' AS arglist, '' AS varlist, i, 1 AS ct FROM t
     UNION ALL
    SELECT cte.arglist || ', ' || 'v' || ct || ' ' || t.i,
           cte.varlist || ', ' || 'v' || ct || '::text',
           t.i, ct + 1
      FROM cte, t
     WHERE ct <= 3
  ), data AS (
    SELECT DISTINCT arglist AS a, RIGHT(varlist, LENGTH(varlist) - 2) AS v
      FROM cte ORDER BY a, v
  ), functions AS (
    SELECT 'CREATE OR REPLACE FUNCTION gateway_error(IN error_name text' || data.a || ') ' || E'\n'
             || 'RETURNS void AS $$' || E'\n'
             || '  SELECT gateway_error_int(error_name, VARIADIC ARRAY ['
             || data.v || ']::text[]) ' || E'\n'
             || '$$ LANGUAGE sql STABLE;' AS function
      FROM data
  ) SELECT function FROM functions; $A$ LANGUAGE sql;

DO $$
DECLARE r record;
BEGIN
  FOR r IN SELECT generate_gateway_error()
  LOOP
    EXECUTE r.generate_gateway_error;
  END LOOP;
END;
$$ LANGUAGE plpgsql;
