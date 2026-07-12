\ owner-wid-internal.f - run the owner proof builder in an isolated process.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package OWNER-WID-INTERNAL

$4000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: RUN-CHILD ( -- n )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/owner-wid-child.f" >LEN PROC-ARGV+
   HB$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE {: outu:len erru:len code:rc :}
   outu LEN>N OUT-U !
   erru LEN>N ERR-U !
   code RC>N ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: BODY ( -- )
   RUN-CHILD 0 T=
   OUT$ s" owner-wid-child-test: ok" CONTAINS? TTRUE
   ERR-U @ 0 T= ;

public

: RUN ( -- )
   T-RESET
   BODY
   T-REPORT
   s" owner-wid-internal-test: ok" type cr ;

;package

OWNER-WID-INTERNAL:RUN
