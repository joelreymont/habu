\ scratch-main.f - built by tools/hb-build.f after scratch-first.f and
\ scratch-second.f; its top level keeps the scratch cell as a constant, and
\ MAIN prints it: 0, since the store ended with scratch-second.f.

require test/fixtures/file-objects/scratch-first.f
require test/fixtures/file-objects/scratch-second.f

package FILE-OBJECTS-SCRATCH-MAIN
public
FILE-OBJECTS-SCRATCH-FIRST:FETCH constant SEEN
: SHOW ( -- ) SEEN . ;
;package

: MAIN ( -- ) FILE-OBJECTS-SCRATCH-MAIN:SHOW ;
