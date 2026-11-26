\ runtime-test.f - src/lib/sparql/runtime.sql against a live PostgreSQL server.
\ test/db/pg-cluster.f starts a private cluster and runs this file with the
\ cluster's conninfo as its one script argument (`bin/hb --load
\ test/db/pg-cluster.f`). The file loads the runtime, then
\ test/sparql/runtime-test.sql, whose blocks raise on a wrong answer; a raised
\ exception fails the file. Loaded without the argument, the file dies naming
\ the harness.

require src/lib/test/assert.f
require test/sparql/server.f

package SPARQL-RUNTIME-TEST
private

: SESSION ( -- )
   s" src/lib/sparql/runtime.sql" SPARQL-TEST:RUN-SQL
   s" test/sparql/runtime-test.sql" SPARQL-TEST:RUN-SQL ;

T-RESET
s" runtime-test" ' SESSION SPARQL-TEST:MAIN
T-REPORT

;package
