\ scratch-main.f - built by tools/hb-build.f after scratch-first.f and
\ scratch-second.f; its top level keeps the five scratch stores as constants,
\ and MAIN prints them and then reads each again: ten times 0, since the stores
\ ended with scratch-second.f.

require test/fixtures/file-objects/scratch-first.f
require test/fixtures/file-objects/scratch-second.f

package FILE-OBJECTS-SCRATCH-MAIN
public
FILE-OBJECTS-SCRATCH-FIRST:FETCH constant SEEN
FILE-OBJECTS-SCRATCH-FIRST:VAR-FETCH constant VAR-SEEN
FILE-OBJECTS-SCRATCH-FIRST:BUF-FETCH constant BUF-SEEN
FILE-OBJECTS-SCRATCH-FIRST:TVAR-FETCH constant TVAR-SEEN
FILE-OBJECTS-SCRATCH-FIRST:TBUF-FETCH constant TBUF-SEEN
: SHOW ( -- )
   SEEN . VAR-SEEN . BUF-SEEN . TVAR-SEEN . TBUF-SEEN .
   FILE-OBJECTS-SCRATCH-FIRST:FETCH . FILE-OBJECTS-SCRATCH-FIRST:VAR-FETCH . FILE-OBJECTS-SCRATCH-FIRST:BUF-FETCH . FILE-OBJECTS-SCRATCH-FIRST:TVAR-FETCH . FILE-OBJECTS-SCRATCH-FIRST:TBUF-FETCH . ;
;package

: MAIN ( -- ) FILE-OBJECTS-SCRATCH-MAIN:SHOW ;
