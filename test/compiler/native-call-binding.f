\ A recorded call keeps the checker-selected record and effect until the next scan.
require lib/test.f
require lib/process-fork.f
require src/compiler/native/checker-owner.f
require src/compiler/native/feed.f
require src/compiler/native/compiler.f
require lib/ffi-abi.f
require test/compiler/native-eval-fixture.f

package NCB-LEFT
public
: CLASH ( -- n ) 1 ;
;package

package NCB-RIGHT
public
: CLASH ( -- n ) 2 ;
;package

package NATIVE-CALL-BINDING-TEST
private

TRUSTED: OPEN-ID ( a -- a ) ;
variable OPEN-CALL-RC

1 set-tier
TRUSTED: FOREIGN-CALL ( n -- n )
   >r FFI:ARGS FFI:REG-LENS 0 r> ffi-call-bounded ;
\ The raw pointer read refuses judgment before the open generic call, so only
\ its uninstantiated declaration could supply a width. This body never runs.
s" TRUSTED: OPEN-CALL ( -- ) 5 @ OPEN-ID drop ;"
   NATIVE-EVAL:DEFINE-RC OPEN-CALL-RC !
0 set-tier

: FOREIGN-ABI ( -- )
   HB-TARGET-MACOS? if -2 else 0 then
   S\" getpid\z" drop FFI:DLSYM {: fn:n :}
   fn 0<> TTRUE
   fn FOREIGN-CALL getpid T=
   OPEN-CALL-RC @ E-NELAB-BUNDLE T= ;

: NO-SCAN ( ptr u8 n -- ) 2drop ;
: NO-TOKEN ( ptr u8 n n n n n -- ) 2drop 2drop 2drop ;
: NO-DONE ( ptr u8 n n -- ) 2drop drop ;

public
: EARLIER ( -- n ) 41 ;
private

: FIELD@ ( ptr u8 n -- n ) cells + CELL-VIEW @ ;

: BOUND ( -- )
   1 [: NO-SCAN ;] [: NO-TOKEN ;] [: NO-DONE ;] CHECKER-OWNER:TAPE-INSTALL
   CHECKER-OWNER:TAPE-ARM
   s" PROBE ( -- n ) EARLIER" CHECKER-OWNER:CHECK -1 T=
   0 CHECKER-OWNER:CALL-BINDING {: p:ptr bytes:n :}
   bytes 0 T=
   1 CHECKER-OWNER:CALL-BINDING {: row:ptr size:n :}
   size CHECKER-OWNER-ABI:BOUND-CELLS cells T=
   row CHECKER-OWNER-ABI:BOUND-KIND FIELD@ CHECKER-OWNER-ABI:BOUND-DICT T=
   row CHECKER-OWNER-ABI:BOUND-SYM FIELD@ 0 > TTRUE
   row CHECKER-OWNER-ABI:BOUND-EFFECT FIELD@ 0 > TTRUE
   row CHECKER-OWNER-ABI:BOUND-ENTRY FIELD@
      s" NATIVE-CALL-BINDING-TEST:EARLIER" XREF-FIND XREF-START T=
   row CHECKER-OWNER-ABI:BOUND-RECORD FIELD@ XREF-REC
      s" NATIVE-CALL-BINDING-TEST:EARLIER" XREF-FIND = TTRUE
   [: 2 CHECKER-OWNER:CALL-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

: UNJUDGED ( -- )
   CHECKER-OWNER:TAPE-ARM
   s" PROBE ( -- n ) 5 @ EARLIER" CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   3 CHECKER-OWNER:UNJUDGED-BINDING {: row:ptr size:n :}
   size CHECKER-OWNER-ABI:BOUND-CELLS cells T=
   row CHECKER-OWNER-ABI:BOUND-KIND FIELD@ CHECKER-OWNER-ABI:BOUND-DICT T=
   row CHECKER-OWNER-ABI:BOUND-ENTRY FIELD@
      s" NATIVE-CALL-BINDING-TEST:EARLIER" XREF-FIND XREF-START T=
   row CHECKER-OWNER-ABI:BOUND-EFFECT FIELD@ 0 > TTRUE
   [: 3 CHECKER-OWNER:CALL-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

: UNSAFE-CALLS ( -- )
   CHECKER-OWNER:TAPE-ARM
   s" PROBE ( -- n ) 0 set-check 0 set-preflight EARLIER"
      CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   2 CHECKER-OWNER:UNJUDGED-BINDING nip 0 > TTRUE
   4 CHECKER-OWNER:UNJUDGED-BINDING nip 0 > TTRUE
   5 CHECKER-OWNER:UNJUDGED-BINDING nip 0 > TTRUE
   CHECKER-OWNER:TAPE-DISARM ;

: REFUSED-JUDGED ( -- )
   CHECKER-OWNER:TAPE-ARM
   s" PROBE ( -- n ) 5 @ EARLIER" CHECKER-OWNER:CHECK -1 <> TTRUE
   [: 3 CHECKER-OWNER:CALL-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   [: 3 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

: INCOMPLETE ( -- )
   CHECKER-OWNER:TAPE-ARM
   s" PROBE ( -- n ) EARLIER is" CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   [: 1 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   s" PROBE ( -- n ) EARLIER [']" CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   [: 1 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   S\" PROBE ( -- n ) EARLIER s\q open" CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   [: 1 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

: REPEATED-INTRINSIC ( -- )
   CHECKER-OWNER:TAPE-ARM
   s" REPEAT-PROBE ( n -- n n n ) dup dup" CHECKER-OWNER:CHECK -1 T=
   1 CHECKER-OWNER:CALL-BINDING {: first:ptr n1:n :}
   2 CHECKER-OWNER:CALL-BINDING {: second:ptr n2:n :}
   n1 CHECKER-OWNER-ABI:BOUND-CELLS cells T=
   n2 CHECKER-OWNER-ABI:BOUND-CELLS cells T=
   first CHECKER-OWNER-ABI:BOUND-ENTRY FIELD@
      s" dup" XREF-FIND XREF-START T=
   second CHECKER-OWNER-ABI:BOUND-ENTRY FIELD@
      s" dup" XREF-FIND XREF-START T=
   CHECKER-OWNER:TAPE-DISARM ;

: THREW ( -- )
   CHECKER-OWNER:TAPE-ARM
   MULTI-ERR-BEGIN
   s" THROW-PROBE ( -- n ) EARLIER CLASH" CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   [: 1 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   MULTI-ERR-END 0 > TTRUE
   CHECKER-OWNER:TAPE-DISARM ;

TRUSTED: SCOPE+ ( -- ) CHECKER-SCOPE-START ;
TRUSTED: SCOPE- ( -- ) CHECKER-SCOPE-DONE ;
TRUSTED: RESET-SOURCE ( -- ) CHECKER-RESET-SOURCE ;
TRUSTED: REWIND ( -- ) CHECKER-BOUND:REWIND ;
TRUSTED: EMPTY-STORE ( -- ) CHECKER-BOUND:EMPTY-STORE ;

: JUDGED-RECORDED ( -- )
   CHECKER-OWNER:TAPE-ARM
   s" RESET-JUDGED-PROBE ( -- n ) EARLIER" CHECKER-OWNER:CHECK -1 T=
   1 CHECKER-OWNER:CALL-BINDING nip 0 > TTRUE ;

: UNJUDGED-RECORDED ( -- )
   CHECKER-OWNER:TAPE-ARM
   s" RESET-UNJUDGED-PROBE ( -- n ) 5 @ EARLIER"
      CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   3 CHECKER-OWNER:UNJUDGED-BINDING nip 0 > TTRUE ;

: RETIRED-JUDGED ( -- )
   [: 1 CHECKER-OWNER:CALL-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

: RETIRED-UNJUDGED ( -- )
   [: 3 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

: COMMENT-CHILD ( -- )
   T-RESET
   CHECKER-OWNER:TAPE-ARM
   s" COMMENT-PROBE ( -- n ) EARLIER ( open" CHECKER-OWNER:CHECK-UNJUDGED drop
   [: 1 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM
   T-REPORT
   s" unfinished comment refused bindings" type cr ;

: REWIND-JUDGED ( -- )
   T-RESET
   JUDGED-RECORDED
   REWIND
   RETIRED-JUDGED
   T-REPORT
   s" rewind retired judged binding" type cr ;

: REWIND-UNJUDGED ( -- )
   T-RESET
   UNJUDGED-RECORDED
   REWIND
   RETIRED-UNJUDGED
   T-REPORT
   s" rewind retired unjudged binding" type cr ;

: EMPTY-JUDGED ( -- )
   T-RESET
   JUDGED-RECORDED
   EMPTY-STORE
   RETIRED-JUDGED
   T-REPORT
   s" empty store retired judged binding" type cr ;

: EMPTY-UNJUDGED ( -- )
   T-RESET
   UNJUDGED-RECORDED
   EMPTY-STORE
   RETIRED-UNJUDGED
   T-REPORT
   s" empty store retired unjudged binding" type cr ;

: CHILD-RUN ( [ -- ] -- )
   {: body :}
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= IF body execute s" " 0 die THEN
   pid PROC-WAIT-STATUS 0 T= ;

: RESET-BOUNDARIES ( -- )
   [: REWIND-JUDGED ;] CHILD-RUN
   [: REWIND-UNJUDGED ;] CHILD-RUN
   [: EMPTY-JUDGED ;] CHILD-RUN
   [: EMPTY-UNJUDGED ;] CHILD-RUN ;

: UNCLOSED-COMMENT ( -- )
   [: COMMENT-CHILD ;] CHILD-RUN ;

: RETIRED ( -- )
   CHECKER-OWNER:TAPE-ARM
   SCOPE+
   s" PROBE ( -- n ) 5 @ EARLIER" CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   3 CHECKER-OWNER:UNJUDGED-BINDING nip 0 > TTRUE
   SCOPE-
   [: 3 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

: RESET-RETIRED ( -- )
   CHECKER-OWNER:TAPE-ARM
   s" PROBE ( -- n ) 5 @ EARLIER" CHECKER-OWNER:CHECK-UNJUDGED -1 <> TTRUE
   3 CHECKER-OWNER:UNJUDGED-BINDING nip 0 > TTRUE
   RESET-SOURCE
   [: 3 CHECKER-OWNER:UNJUDGED-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

: RESET-CASE ( -- )
   CHECKER-OWNER:TAPE-ARM
   [: 1 CHECKER-OWNER:CALL-BINDING 2drop ;] E-NCOMP-BINDING TTHROWSQ
   CHECKER-OWNER:TAPE-DISARM ;

public
using NCB-LEFT
using NCB-RIGHT
: RUN ( -- )
   T-RESET
   s" native FFI calls keep their fixed pointer ABI" T-LABEL FOREIGN-ABI T-NEXT
   s" resolved call witness" T-LABEL BOUND T-NEXT
   s" completed unjudged source retains suffix calls" T-LABEL UNJUDGED T-NEXT
   s" unsafe source calls bind before type refusal" T-LABEL UNSAFE-CALLS T-NEXT
   s" judged refusal cannot grant either window" T-LABEL REFUSED-JUDGED T-NEXT
   s" incomplete unjudged parses grant no binding" T-LABEL INCOMPLETE T-NEXT
   s" unfinished body comment grants no binding" T-LABEL UNCLOSED-COMMENT T-NEXT
   s" repeated intrinsic calls have separate original sites" T-LABEL REPEATED-INTRINSIC T-NEXT
   s" caught binding throw grants no partial stream" T-LABEL THREW T-NEXT
   s" rollback retires unjudged bindings" T-LABEL RETIRED T-NEXT
   s" prefix and store reset retire both binding windows" T-LABEL RESET-BOUNDARIES T-NEXT
   s" reset invalidates borrowed call rows" T-LABEL RESET-CASE T-NEXT
   s" reset retires unjudged bindings" T-LABEL RESET-RETIRED T-NEXT
   NFEED:OBSERVE
   T-REPORT ;

RUN
;using
;using
;package
