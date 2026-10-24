\ A full saved image retains a callback's descriptor, owned C stub and body.
\ On macOS it also retains a FUNCTION: binding that passes a struct over 16
\ bytes by value from a library outside the process, so the restored image
\ reopens that library and copies the record again on every call.
require test/gate-common.f
require src/lib/engine-candidate.f
require src/lib/codegen.f
require test/ffi-struct-fixture.f

package FFI-CB-FULL-IMAGE-TEST

600000 constant TIMEOUT-MS
create IMAGE FS-PATH-CAP allot
variable IMAGE-U
2048 CODEGEN:BUFFER SCRIPT

: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: L+ ( ptr u8 n -- )
   SCRIPT CODEGEN:APPEND-STRING
   10 SCRIPT CODEGEN:APPEND-BYTE ;

\ The save child's input. SAVED-SUM24 is called and checked before the save
\ and again after restore. BUILD runs cc through the shared argv list, so
\ this runs before the child's argv is staged.
: SCRIPT$ ( -- ptr u8 n )
   SCRIPT CODEGEN:RESET
   s" require test/ffi-callback-full-image-prefix.f" L+
   s" require src/habu/app-image.f" L+
   HB-TARGET-MACOS? if
      s" LIBRARY " SCRIPT CODEGEN:APPEND-STRING
      GT-ROOT STRUCT-FIXTURE:BUILD L+
      s" FUNCTION: SAVED-SUM24 hb_sum24 ( ptr u8 -- n ) 0 24 STRUCT-ARG ;FUNCTION" L+
      s" require test/ffi-callback-full-image-struct.f" L+
      s" SAVED-STRUCT-CHECK" L+
   then
   s" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE" L+
   SCRIPT CODEGEN:CONTENTS ;

: SAVE-IMAGE ( -- )
   SCRIPT$ {: in:ptr inu:n :}
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --" GE-ARG+ IMAGE$ GE-ARG+
   ENGINE-CANDIDATE:PATH$ in inu TIMEOUT-MS GE-RUN-STDIN
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
