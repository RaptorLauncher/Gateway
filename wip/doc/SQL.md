# Gateway SQL layer

## Overview 

The SQL layer consists of two functional parts. One consists of SQL scripts for
installation and uninstallation and is executed via Postmodern's file-execution
functionality. The other is a set of functions loaded into Lisp image and turned
into Lisp functions via CL-YESQL.

This layer depends on two databases: the main database (exposed via the
`WITH-DB` macro) and the test database (exposed via the `WITH-TEST-DB` macro).
The main database is considered production-ready, while the test database is
intended for running tests on; the tests are free to mutate its contents during
their execution.

The `GATEWAY.SQL` package exports the following symbols:

  * **Function `INSTALL`** 
  
    Installs the Gateway schema in the currently active database.
    
  * **Function `UNINSTALL`**
  
    Uninstalls the Gateway schema from the currently active database.
  
  * **Function `INSTALL-DUMMY-DATA`**
  
    Installs the provided dummy/example data into the currently active database.
    This function must be called on a database with the installed Gateway 
    schema.
  
  * **Macro `WITH-DB`**
  
    Syntax: `(with-db () &body body)`
    
    Sets the currently active database to the main database.
    
  * **Macro `WITH-TEST-DB`**
  
    Syntax: `(with-test-db () &body body)`
    
    Sets the currently active database to the test database.

## Tests

The SQL tests currently consist of the main `SQL` test suite that is inherited 
by three subsuites: `SQL-POSITIVE` (for positive test scenarios), `SQL-NEGATIVE`
(for negative test scenarios and constraint checking), and `SQL-SELECT-DUMMY`
(for testing selecting functions on the provided dummy Gateway data).

A test database must be configured in order to run the tests. See the
documentation for `GATEWAY.CONFIG` for details.
