\ engine-candidate-test.f - exact nested-engine candidate coverage.

require lib/test.f
require lib/engine-candidate.f
require lib/fs-mutate.f

package ENGINE-CANDIDATE-TEST

private

$4000 constant CAP
$1000 constant PATH-CAP
$1388 constant TIMEOUT-MS

create OUT CAP allot
create ERR CAP allot
create ROOT PATH-CAP allot
create EXEC-PATH PATH-CAP allot
create PLAIN-PATH PATH-CAP allot

variable ROOT-U
variable EXEC-U
variable PLAIN-U
variable SAVE-N
variable SAVE-OFF
variable SAVE-TABLE
variable SAVE-BUF

: SAVE-STATE ( -- )
   PROC-ENV-DEF-N @ SAVE-N !
   PROC-ENV-DEF-OFF @ SAVE-OFF !
   PROC-ENV-DEF-TABLE-A @ SAVE-TABLE !
   PROC-ENV-DEF-BUF-A @ SAVE-BUF ! ;

: STATE= ( -- )
   PROC-ENV-DEF-N @ SAVE-N @ T=
   PROC-ENV-DEF-OFF @ SAVE-OFF @ T=
   PROC-ENV-DEF-TABLE-A @ SAVE-TABLE @ T=
   PROC-ENV-DEF-BUF-A @ SAVE-BUF @ T= ;

: DEFAULT$= ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n val:ptr valu:n :}
   name nameu >LEN PROC-ENV-DEFAULT$? TTRUE
   LEN>N val valu T$= ;

: PREPARED ( -- )
   PROC-ENV-DEFAULT-RESET
   s" ENGINE_CANDIDATE_SENTINEL" >LEN s" keep" >LEN PROC-ENV-DEFAULT+
   s" HABU_UNDER_TEST" >LEN s" /bin/sh" >LEN PROC-ENV-DEFAULT+
   SAVE-STATE
   ENGINE-CANDIDATE:PATH$ s" /bin/sh" T$=
   ENGINE-CANDIDATE:OVERRIDE$? TTRUE s" /bin/sh" T$=
   s" ENGINE_CANDIDATE_SENTINEL" s" keep" DEFAULT$=
   s" HABU_UNDER_TEST" s" /bin/sh" DEFAULT$=
   STATE= ;

: HOST ( -- )
   PROC-ENV-DEFAULT-RESET
   SAVE-STATE
   ENGINE-CANDIDATE:PATH$ s" /bin/sh" T$=
   ENGINE-CANDIDATE:OVERRIDE$? TTRUE s" /bin/sh" T$=
   STATE= ;

: FALLBACK ( -- )
   PROC-ENV-DEFAULT-RESET
   SAVE-STATE
   ENGINE-CANDIDATE:PATH$ s" bin/hb" T$=
   ENGINE-CANDIDATE:OVERRIDE$? TFALSE 2drop
   STATE= ;

: CHILD-RUN ( -- )
   T-RESET
   1 SCRIPT-ARGV$ s" prepared" STR= if PREPARED else
   1 SCRIPT-ARGV$ s" host" STR= if HOST else
   1 SCRIPT-ARGV$ s" fallback" STR= if FALLBACK else
      E-PROC-OUTPUT throw
   then then then
   T-REPORT ;

: CHILD? ( -- bool )
   SCRIPT-ARGC 2 <> if 0 0= 0= exit then
   0 SCRIPT-ARGV$ s" engine-candidate-child" STR= ;

: ARGV! ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" lib/engine-candidate-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" engine-candidate-child" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+ ;

: CHILD ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n host:ptr hostu:n :}
   mode modeu ARGV!
   PROC-ENV-RESET
   hostu 0 > if
      s" HABU_UNDER_TEST" >LEN host hostu >LEN PROC-ENV+
   then
   ENGINE-CANDIDATE:PATH$ >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE {: outu:len erru:len rc:rc :}
   rc RC>N 0 T=
   erru LEN>N 0 T=
   outu LEN>N 0 > TTRUE ;

: CHILDREN ( -- )
   s" prepared" s" /bin/false" CHILD
   s" host" s" /bin/sh" CHILD
   s" fallback" s" " CHILD ;

: SETUP ( -- )
   CLEANUP-RESET
   s" hb-engine-candidate" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY
   u ROOT-U !
   ROOT ROOT-U @ CLEANUP-TREE+
   ROOT ROOT-U @ s" executable" EXEC-PATH JOIN-PATH EXEC-U !
   ROOT ROOT-U @ s" plain" PLAIN-PATH JOIN-PATH PLAIN-U !
   EXEC-PATH EXEC-U @ s" x" WRITE-ALL
   EXEC-PATH EXEC-U @ CHMOD-X
   PLAIN-PATH PLAIN-U @ s" x" WRITE-ALL ;

: VALIDATE-PLAIN ( -- )
   PLAIN-PATH PLAIN-U @ ENGINE-CANDIDATE:VALIDATE$ 2drop ;

: VALIDATE-DOT ( -- )
   s" ." ENGINE-CANDIDATE:VALIDATE$ 2drop ;

: VALIDATE-DIR ( -- )
   ROOT ROOT-U @ ENGINE-CANDIDATE:VALIDATE$ 2drop ;

: VALIDATE-MISSING ( -- )
   s" no-such-habu-engine-candidate" ENGINE-CANDIDATE:VALIDATE$ 2drop ;

: VALIDATE ( -- )
   SETUP
   EXEC-PATH EXEC-U @ ENGINE-CANDIDATE:VALIDATE$
   EXEC-PATH EXEC-U @ T$=
   [: VALIDATE-PLAIN ;] E-FS-OPEN TTHROWSQ
   [: VALIDATE-DOT ;] E-FS-OPEN TTHROWSQ
   [: VALIDATE-DIR ;] E-FS-OPEN TTHROWSQ
   [: VALIDATE-MISSING ;] E-FS-OPEN TTHROWSQ
   CLEANUP-RUN ;

: RUN ( -- )
   T-RESET
   VALIDATE
   CHILDREN
   T-REPORT ;

: MAIN ( -- )
   CHILD? if CHILD-RUN else RUN then ;

MAIN

;package
