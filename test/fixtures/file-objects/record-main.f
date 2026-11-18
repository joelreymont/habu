split-str
\ record-main.f - the record program, built by tools/hb-build.f: record-first.f,
\ then a MAIN that prints a record of its own through REJECTION.

require test/fixtures/file-objects/record-first.f

package FILE-OBJECTS-RECORD-MAIN
public
: RAN ( -- )
   s" E-MAIN" REJECTION:OPEN  s" ran" REJECTION:EMIT-STR  REJECTION:KEEP ;
;package

: MAIN ( -- ) FILE-OBJECTS-RECORD-MAIN:RAN ;
