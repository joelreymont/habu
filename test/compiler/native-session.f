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
\ Tier-neutral by design: every session under test is a child whose own source
\ sets its tier, so the tier of this row selects nothing.

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

\ WHAT A LIVE SESSION COSTS A DEFINITION'S ARENAS, counted through the real
\ entry point: nothing at all. An arena was a row of a sixty-four slot registry
\ and a live session held four of them, so what a definition could build was
\ measured against what the session had taken. An arena is now a record in the
\ owning context's own region, so a definition's context takes as many as it
\ takes: this asks for a thousand of them in one context - fifty-eight modules'
\ worth, and sixteen times what the whole registry held - with no session
\ standing and then with one, and both answer the same number.
: SLOTS-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" package ZSLOT\n" SB-APPEND
   S\" public\n" SB-APPEND
   S\" 1000 constant WANT\n" SB-APPEND
   S\" : TAKE-MANY ( IR-CTX:ctx -- n )\n" SB-APPEND
   S\"    WANT 0 ?do dup 1 IR-ARENA:NEW drop loop drop WANT ;\n" SB-APPEND
   S\" : COUNT ( -- n ) NABI:BINDING [: TAKE-MANY ;] IR-CTX:WITH-CONTEXT ;\n" SB-APPEND
   S\" ;package\n" SB-APPEND
   S\" ZSLOT:COUNT .\n" SB-APPEND
   S\" 1 set-tier\n" SB-APPEND
   S\" : ZL1 ( -- n ) 1 ;\n" SB-APPEND
   S\" 0 set-tier\n" SB-APPEND
   S\" ZSLOT:COUNT . IR-CTX:SESSION-LIVE? . cr\n" SB-APPEND
   SB$ ;

\ THE FLAGS GO OUT WITH THE SESSION. IMAGE-LIFECYCLE:PREPARE is one entry point
\ of a capture and NCOMP:CAPTURE-PREPARE is another, so anything that says "the
\ session's vocabulary is readable" and is cleared by the second one is still
\ saying it after the first has unmapped the arena it describes: asking an
\ ordinary question - how many rows the vocabulary holds - answered
\ E-IR-ARENA-STALE. The stand-down now belongs to the close itself, so a bare
\ PREPARE leaves no such claim, and the next definition opens a fresh session.
: STANDDOWN-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" 1 set-tier\n" SB-APPEND
   S\" : ZW1 ( -- n ) 1 ;\n" SB-APPEND
   S\" 0 set-tier\n" SB-APPEND
   S\" IMAGE-LIFECYCLE:PREPARE\n" SB-APPEND
   S\" HIR-WORD:SESSION-ROWS . IR-CTX:SESSION-LIVE? . cr\n" SB-APPEND
   S\" 1 set-tier\n" SB-APPEND
   S\" : ZW2 ( -- n ) 2 ;\n" SB-APPEND
   S\" 0 set-tier\n" SB-APPEND
   S\" HIR-WORD:SESSION-ROWS . ZW2 . cr\n" SB-APPEND
   SB$ ;

\ A SESSION OPENED INSIDE A CALLER'S OWN CONTEXT IS A NAMED REFUSAL. A session
\ outlives every scope, so it is the bottom row of the context registry or it is
\ nothing; served inside a scope, the scope's exit retired the session's row
\ instead of its own and left a live row over a released mapping, which read as a
\ SIGSEGV. The definition is refused instead, and the load carries on: no session
\ stands afterwards and the next definition compiles and runs.
: NESTED-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" package ZNEST\n" SB-APPEND
   S\" public\n" SB-APPEND
   S\" TRUSTED: EVAL$ ( ptr u8 n -- ) evaluate ;\n" SB-APPEND
   S\" : BODY ( IR-CTX:ctx -- )\n" SB-APPEND
   S\"    drop [: s\q : ZN1 ( -- n ) 7 ;\q EVAL$ ;] catch\n" SB-APPEND
   S\"    E-IR-CTX-STATE = if s\q refused\q else s\q served\q then type cr ;\n" SB-APPEND
   S\" : RUN ( -- ) NABI:BINDING [: BODY ;] IR-CTX:WITH-CONTEXT ;\n" SB-APPEND
   S\" ;package\n" SB-APPEND
   S\" 1 set-tier\n" SB-APPEND
   S\" ZNEST:RUN\n" SB-APPEND
   S\" 0 set-tier\n" SB-APPEND
   S\" IR-CTX:SESSION-LIVE? .\n" SB-APPEND
   S\" 1 set-tier\n" SB-APPEND
   S\" : ZN2 ( -- n ) 5 ;\n" SB-APPEND
   S\" 0 set-tier\n" SB-APPEND
   S\" ZN2 . cr\n" SB-APPEND
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
   CAPTURE-SRC$ EXEC  S\" 0\n1\n" ASSERT-OK

   s" a live session costs a definition no arena capacity" T-LABEL
   SLOTS-SRC$ EXEC  S\" 1000\n1000\n-1\n" ASSERT-OK

   s" a bare PREPARE leaves no claim on the arenas it unmapped" T-LABEL
   STANDDOWN-SRC$ EXEC  S\" 0\n0\n" ASSERT-OK

   \ The count is the whole of the dialect's vocabulary, HIR-WORD:WORDS rows,
   \ which is what a fresh session holds; test/compiler/native-hir.f pins that
   \ equality structurally, and this case reads the number back out of a child
   \ that stood its session down first.
   s" and the definition after it opens a fresh session" T-LABEL
   OUT$ S\" 94\n2\n" CONTAINS? TTRUE

   s" a definition compiled inside a context is refused, not served" T-LABEL
   NESTED-SRC$ EXEC  S\" refused\n0\n5\n" ASSERT-OK ;

public

: RUN ( -- )
   T-RESET
   SESSION-CASES
   T-REPORT
   s" native-session: ok" type cr ;

;package

NSESSION-TEST:RUN
