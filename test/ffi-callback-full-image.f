\ A full saved image retains a callback's descriptor, owned C stub and body.
require test/gate-common.f
require src/lib/engine-candidate.f

package FFI-CB-FULL-IMAGE-TEST

600000 constant TIMEOUT-MS
create IMAGE FS-PATH-CAP allot
variable IMAGE-U

: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: SAVE-IMAGE ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --" GE-ARG+ IMAGE$ GE-ARG+
   ENGINE-CANDIDATE:PATH$
   S\" require test/ffi-callback-full-image-prefix.f\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   TIMEOUT-MS GE-RUN-STDIN
   s" full callback image save" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" full callback image missing" GE-FAIL then ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   s" --load" GE-ARG+
   s" test/ffi-callback-full-image-rebind.f" GE-ARG+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" full callback image run" GE-EXPECT-OK
   S\" full-image-callback-rebind: ok\n" s" full callback image output" GE-EXPECT-OUT
   s" " s" full callback image stderr" GE-EXPECT-ERR ;

: BODY ( -- )
   s" ffi-callback-full-image" GT-START
   s" saved-engine" IMAGE GT-PATH IMAGE-U !
   SAVE-IMAGE RUN-IMAGE
   s" PASS: full callback image rebind" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package
