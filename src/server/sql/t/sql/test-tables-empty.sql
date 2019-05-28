-- name: select-row-counts-of-all-tables
-- Returns the row counts of all tables in the schema.
SELECT table_name,
       (xpath('/row/count/text()',
              query_to_xml('select count(*) from '||format('%I.%I', table_schema, table_name),
                           true, true, '')))[1]::text::int AS row_count
  FROM information_schema.tables
  WHERE table_schema = 'public';