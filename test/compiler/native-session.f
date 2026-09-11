\ native-session.f - the compiler session a load opens, and the definitions
\ compiled inside it.
\
\ A load opens ONE compiler context and compiles every definition in a child
\ context of it. The session holds what is the same for every definition of the
\ load - the dialect vocabulary, interned once into an interner every module
\ starts as a copy of; a definition holds its own modules, builders and arenas,
\ and its context gives them all back when it ends.
\
\ TWO CLAIMS PAY FOR THIS FILE. A load of any length still compiles: the
\ per-definition registries are bounded and are given back one definition at a
\ time, and a session that tried to keep them instead died at the eighth
\ definition with E-IR-BUILD-SLOTS. And a REFUSED definition leaves the session
\ exactly as it found it, so the next definition compiles against a whole
\ vocabulary and not the wreckage of the last one.
\
\ Every case is a real child process at tier 1. A session is process-wide state
\ and there is no in-process way to ask a fresh process for its first
\ definition.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f

package NSESSION-TEST

private

$1000 constant CAP
30000 constant TIMEOUT-MS
30 constant MANY                   \ definitions in one load; the slot bound was 8

create OUT CAP allot
create ERR CAP allot
variable OUT-U  variable ERR-U
variable RC     variable EXITED

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited   OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout  OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

\ The source goes in on stdin of the engine under test, so a candidate build is
\ measured and not whatever bin/hb happens to be.
: EXEC ( ptr u8 n -- ) {: src:ptr u:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN  src u >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

\ The child exited zero and printed what the definitions computed. Printing the
\ answer is what proves the bodies RAN; an exit code alone passes for a load
\ that compiled nothing.
: ASSERT-OK ( ptr u8 n -- ) {: want:ptr wu:n :}
   EXITED @ TTRUE
   RC @ 0 T=
   OUT$ want wu CONTAINS? TTRUE ;

\ ---- the sources -------------------------------------------------------------
\ Built rather than spelled out, so the count tracks MANY instead of a typist.
: DEF ( n -- ) {: k:n :}
   s" : ZD" SB-APPEND  k FMT:SB-U  s"  ( -- n ) " SB-APPEND  k FMT:SB-U  s"  ;" SB-APPEND
   S\" \n" SB-APPEND ;

: MANY-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" 1 set-tier\n" SB-APPEND
   MANY 0 ?do i DEF loop
   s" ZD0 ZD" SB-APPEND  MANY 1- FMT:SB-U  S\"  + . cr\n" SB-APPEND
   SB$ ;

\ A definition the checker refuses, evaluated under a catch so the load goes on.
\ The refusals are interpreted at top level rather than wrapped in a helper,
\ because the helper would be one more definition and the case is about what
\ the REFUSED one leaves behind.
: REFUSAL ( n -- ) {: k:n :}
   S\" s\q : ZBAD" SB-APPEND  k FMT:SB-U
   S\"  ( n -- n n ) dup * ;\q ' evaluate catch drop\n" SB-APPEND ;

: REFUSED-SRC$ ( n -- ptr u8 n ) {: many:n :}
   SB-RESET
   S\" 1 set-tier\n" SB-APPEND
   S\" : ZA ( -- n ) 1 ;\n" SB-APPEND
   many 0 ?do i REFUSAL loop
   S\" : ZB ( -- n ) 2 ;\n" SB-APPEND
   S\" ZA ZB + . cr\n" SB-APPEND
   SB$ ;

\ What the session is FOR, measured on the real load: the second definition
\ interns no opcode name in either dialect, because its modules were cloned from
\ the vocabulary the session interned once.
: MISSES-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" 1 set-tier\n" SB-APPEND
   S\" : ZM1 ( -- n ) 1 ;\n" SB-APPEND
   S\" HIR:MISSES-CLEAR A64IR:MISSES-CLEAR\n" SB-APPEND
   S\" : ZM2 ( -- n ) 2 ;\n" SB-APPEND
   S\" HIR:MISSES . A64IR:MISSES . ZM1 ZM2 + . cr\n" SB-APPEND
   SB$ ;

\ The capture sequence, in a real captured engine. The session is a registry row
\ no scope will leave, so the compiler's own capture refuses while one stands;
\ the image lifecycle callback the session registers when it is TAKEN is what
\ gives it back. A captured image carries no callback registry - the capture
\ that built it ran and removed every one - so a registration made when this
\ file was loaded would never happen again in the engine shipped from it, and
\ the first capture after the first tier-1 definition would refuse. That is
\ exactly what this runs: a definition, then the capture sequence, in the child.
: CAPTURE-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" 1 set-tier\n" SB-APPEND
   S\" : ZC1 ( -- n ) 1 ;\n" SB-APPEND
   S\" 0 set-tier\n" SB-APPEND
   S\" IMAGE-LIFECYCLE:PREPARE\n" SB-APPEND
   S\" NCOMP:CAPTURE-PREPARE\n" SB-APPEND
   S\" IR-CTX:SESSION-LIVE? . ZC1 . cr\n" SB-APPEND
   SB$ ;

: SESSION-CASES ( -- )
   s" thirty definitions in one load compile and run" T-LABEL
   MANY-SRC$ EXEC  s" 29" ASSERT-OK

   s" a refused definition leaves the session compiling" T-LABEL
   1 REFUSED-SRC$ EXEC  s" 3" ASSERT-OK

   s" five refusals in a row leave the session compiling" T-LABEL
   5 REFUSED-SRC$ EXEC  s" 3" ASSERT-OK

   s" the second definition of a load interns no opcode name" T-LABEL
   MISSES-SRC$ EXEC  S\" 0\n0\n3\n" ASSERT-OK

   s" a capture after a tier-1 definition gives the session back" T-LABEL
   CAPTURE-SRC$ EXEC  S\" 0\n1\n" ASSERT-OK ;

public

: RUN ( -- )
   T-RESET
   SESSION-CASES
   T-REPORT
   s" native-session: ok" type cr ;

;package

NSESSION-TEST:RUN
