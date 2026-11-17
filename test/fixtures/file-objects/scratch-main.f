\ scratch-main.f - built by tools/hb-build.f after scratch-first.f and
\ scratch-second.f; its top level keeps the five scratch stores as constants,
\ and MAIN prints them and then reads each again: ten times 0, since the stores
\ ended with scratch-second.f.

require test/fixtures/file-objects/scratch-first.f
require test/fixtures/file-objects/scratch-second.f

package FILE-OBJECTS-SCRATCH-MAIN
using FILE-OBJECTS-SCRATCH-FIRST
public
FETCH constant SEEN
VAR-FETCH constant VAR-SEEN
BUF-FETCH constant BUF-SEEN
TVAR-FETCH constant TVAR-SEEN
TBUF-FETCH constant TBUF-SEEN
: SHOW ( -- )
   SEEN . VAR-SEEN . BUF-SEEN . TVAR-SEEN . TBUF-SEEN .
   FETCH . VAR-FETCH . BUF-FETCH . TVAR-FETCH . TBUF-FETCH . ;
;using
;package

: MAIN ( -- ) FILE-OBJECTS-SCRATCH-MAIN:SHOW ;
