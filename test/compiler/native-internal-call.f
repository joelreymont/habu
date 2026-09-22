\ Internal engine calls use the compiler's authorization, not public lookup.
\ Tier-neutral by design: each subject is compiled in a child this file runs at
\ both tiers, so the tier of this row selects nothing.
require lib/test.f
require lib/test/subject.f
require test/checker-assert.f

package INTERNAL-CALL-TEST
private

\ Compilation is the regression; truncation runs only in the build driver.
TRUSTED: TRUNCATE ( n -- ) seed-ndict! ;


$1000 constant CAP
20000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED


: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;


\ The child owns the marked record. Its checked effect remains present, so a
\ missing effect cannot stand in for the compiler's internal-call guard.
: SOURCE$ ( n bool -- ptr u8 n )
   {: tier:n trusted:bool :}
   SB-RESET
   s" 0 set-tier " SB-APPEND
   s" TRUSTED: NIC-MARK-LAST ( -- ) ndict@ 1- int-mark ; " SB-APPEND
   s" : NIC-HIDDEN ( -- n ) 41 ; NIC-MARK-LAST " SB-APPEND
   S\" s\" NIC-KNOWN ( -- n ) NIC-HIDDEN\" CHECK-QUIET-CANDIDATE! . cr " SB-APPEND
   tier 0= if s" 0 set-tier " else s" 1 set-tier " then SB-APPEND
   trusted if s" TRUSTED: " else s" : " then SB-APPEND
   s" NIC-CALL ( -- n ) NIC-HIDDEN ; NIC-CALL . cr" SB-APPEND
   SB$ ;


: STORE! ( len len outcome -- )
   MATCH outcome
      exited OF RC ! true EXITED ! ENDOF
      signaled OF RC ! false EXITED ! ENDOF
      timeout OF 0 RC ! false EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U ! LEN>N OUT-U ! ;


: RUN-SOURCE ( ptr u8 n -- )
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN STORE! ;


: ?KNOWN ( -- )
   s" the internal word retains its checked effect" T-LABEL
   OUT$ S\" -1\n" CONTAINS? TTRUE
   EXITED @ TTRUE ;


: REJECT-CALL ( n -- )
   {: tier:n :}
   tier false SOURCE$ RUN-SOURCE
   ?KNOWN
   tier 0= if s" JIT rejects a known internal target"
   else s" native compiler rejects a known internal target" then T-LABEL
   tier 0= if 70 else 67 then RC @ swap T=
   ERR$ s" NIC-HIDDEN" CONTAINS? TTRUE
   OUT$ s" 41" CONTAINS? TFALSE ;


: TRUST-CALL ( n -- )
   true SOURCE$ RUN-SOURCE
   ?KNOWN
   s" an explicit trusted caller can execute the internal word" T-LABEL
   RC @ 0 T=
   OUT$ S\" 41\n" CONTAINS? TTRUE
   ERR-U @ 0 T= ;


: RUN ( -- )
   T-RESET
   s" seed-ndict!" 0 search-wl 0 T=
   s" seed-ndict!" NDICT:CALL-TARGET 0 T=
   s" BAD-INTERNAL ( n -- ) seed-ndict!" CHECK-QUIET-CANDIDATE! 0 T=
   0 REJECT-CALL 1 REJECT-CALL
   0 TRUST-CALL 1 TRUST-CALL
   T-REPORT ;

RUN
;package
