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
  VALUES ('general_error', 0, 'A general Gateway database error has been signaled.');
