\ maki/target/target-test.f - target descriptor and nominal-id regressions.

require lib/test.f
require test/checker-assert.f
require maki/target/target.f

package TARGET-TEST

variable BASE-N

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: ALT-DESC ( n -- TARGET:descriptor ) {: caps:n :}
   TARGET:ISA-PTX 87 32 1024 49152 caps TARGET:DESCRIPTOR ;

: ROUND ( CAD-KIND:target-id -- CAD-KIND:target-id )
   TARGET:DESCRIPTOR@ TARGET:RESOLVE ;

: ALIAS ( -- CAD-KIND:target-id )
   s" orin-nx" TARGET:SM87 TARGET:DESCRIPTOR@ TARGET:REGISTER ;

: ALT ( -- CAD-KIND:target-id )
   s" sm_87-nobarrier" TARGET:CAP-ALL TARGET:CAP-BARRIER invert and
   ALT-DESC TARGET:REGISTER ;

: BAD-ISA ( -- )
   s" bad-isa" 2 87 32 1024 49152 TARGET:CAP-PTX
   TARGET:DESCRIPTOR TARGET:REGISTER drop ;

: BAD-WARP ( -- )
   s" bad-warp" TARGET:ISA-PTX 87 48 1024 49152 TARGET:CAP-PTX
   TARGET:DESCRIPTOR TARGET:REGISTER drop ;

: BAD-LABEL ( -- )
   s" " TARGET:SM87 TARGET:DESCRIPTOR@ TARGET:REGISTER drop ;

: UNKNOWN ( -- )
   TARGET:ISA-PTX 89 32 1024 49152 TARGET:CAP-PTX
   TARGET:DESCRIPTOR TARGET:RESOLVE drop ;

: CAP-DESC ( n -- TARGET:descriptor )   \ distinct arch per fill registration
   TARGET:ISA-PTX swap 32 1024 49152 TARGET:CAP-PTX TARGET:DESCRIPTOR ;

: CAP-FILL ( -- )   \ fill the append-only registry to its cap (TGT-CAP = 16)
   16 TARGET:COUNT - 0 ?do
      s" cap-fill" 100 i + CAP-DESC TARGET:REGISTER drop
   loop ;

: CAP-17TH ( -- )
   s" cap-over" 900 CAP-DESC TARGET:REGISTER drop ;

T-RESET

TARGET:COUNT BASE-N !
TARGET:SM87 TARGET:LABEL$ s" sm_87" T$=
TARGET:SM87 TARGET:ISA@ TARGET:ISA-PTX T=
TARGET:SM87 TARGET:ARCH@ 87 T=
TARGET:SM87 TARGET:WARP@ 32 T=
TARGET:SM87 TARGET:THREADS@ 1024 T=
TARGET:SM87 TARGET:SHARED@ 49152 T=
TARGET:SM87 TARGET:CAPS@ TARGET:CAP-MMA and 0<> TTRUE
TARGET:SM87 TARGET:FACTS$
s" isa=1|arch=87|warp=32|threads=1024|shared=49152|caps=127" T$=

TARGET:SM87 ROUND TARGET:SM87 TARGET:EQUAL? TTRUE
ALIAS
dup TARGET:SM87 TARGET:EQUAL? TTRUE
TARGET:DIGEST@ TARGET:SM87 TARGET:DIGEST@ = TTRUE
TARGET:COUNT BASE-N @ T=
TARGET:SM87 TARGET:LABEL$ s" sm_87" T$=

ALT
dup TARGET:SM87 TARGET:EQUAL? TFALSE
TARGET:DIGEST@ TARGET:SM87 TARGET:DIGEST@ = TFALSE
TARGET:COUNT BASE-N @ 1+ T=

' BAD-ISA E-TARGET-FACT TTHROWS
' BAD-WARP E-TARGET-FACT TTHROWS
' BAD-LABEL E-TARGET-LABEL TTHROWS
' UNKNOWN E-TARGET-UNKNOWN TTHROWS

s" TGT-OK ( CAD-KIND:target-id -- CAD-KIND:target-id ) TARGET:VALIDATE" YES
s" TGT-ROUND ( CAD-KIND:target-id -- CAD-KIND:target-id ) TARGET:DESCRIPTOR@ TARGET:RESOLVE" YES
s" TGT-TOOL ( CAD-KIND:toolchain-id -- ptr u8 n ) TARGET:LABEL$" NO
s" TGT-ART ( CAD-KIND:artifact-id -- ptr u8 n ) TARGET:LABEL$" NO
s" TARGET:RAW>TARGET-ID" 0 search-wl 0= TTRUE

\ capacity: the seventeenth distinct descriptor rejects. The registry is
\ process-global and append-only, so this fill runs LAST; no later maki/test.f
\ suite registers targets (sched-key-test runs earlier).
CAP-FILL
TARGET:COUNT 16 T=
' CAP-17TH E-TARGET-CAP TTHROWS

;package

\ Nominal-id corruption seam: a bad CAD-KIND:target-id is only mintable via the
\ private refinement, so reopen the owning package for the E-TARGET-ID
\ negatives (qualified lookup is public-only; TT- names are test-owned).
package TARGET

: TT-ID-NEG ( -- )  -1 RAW>TARGET-ID VALIDATE drop ;
: TT-ID-BIG ( -- )  99 RAW>TARGET-ID DESCRIPTOR@ drop ;

' TT-ID-NEG E-TARGET-ID TTHROWS
' TT-ID-BIG E-TARGET-ID TTHROWS

;package

T-REPORT
