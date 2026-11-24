\ log-main.f - the log program, built by tools/hb-build.f: log-first.f, then a
\ MAIN that logs a line of its own through LOG's facility and takes and
\ releases the facility the build left held.

require test/fixtures/file-objects/log-first.f
require src/lib/log.f
require src/lib/task.f

package FILE-OBJECTS-LOG-MAIN
public
: RAN ( -- )
   FILE-OBJECTS-LOG-FIRST:HELD TASK:GET
   LOG:WARN s" main" LOG:LINE s" ran" LOG:TEXT+ LOG:;LINE
   FILE-OBJECTS-LOG-FIRST:HELD TASK:RELEASE ;
;package

: MAIN ( -- ) FILE-OBJECTS-LOG-MAIN:RAN ;
