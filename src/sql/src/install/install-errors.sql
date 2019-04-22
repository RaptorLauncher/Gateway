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
