\ maki/db/diff-runner-inject.f - SUBJECT-SOURCE INJECTION + the real spawned-child TENSOR
\ subject adapter for the differential runner (maki/db/diff-runner-spawn.f +
\ maki/db/diff-runner-tensor.f; dot habu-v2-differential-runner-13359019).
\
\ CONCERN: the injection protocol the base spawn adapter (an identity placeholder subject)
\ deferred. The runner HANDS THE SUBJECT PROGRAM ITS SOURCE and the case input, then runs
\ it isolated in a fresh bin/hb child (the SPAWN-CAPTURE isolation harness): SUBJECT-SRC!
\ stores the subject-under-test source (a checked `: SUBJECT ...` definition); INJECT-SCALAR
\ / INJECT-TENSOR compose it with a harness that binds the case INPUT and prints the
\ subject's output; SPAWN-INJECTED / SPAWN-TENSOR-INJECTED spawn+classify it. So a DIFFERENT
\ injected subject deterministically yields a DIFFERENT, correctly-classified outcome, and a
\ crashing/dying injected subject still faults (never a grader casualty). This REOPENS
\ package DIFFRUN and reuses the whole scalar/tensor core: SPAWN-CAPTURE (isolation),
\ PARSE-INT (scalar output), the SUBJ-T compare buffer + SUBJ-T! (tensor output), and the
\ run-result taxonomy.
\
\ ---- TENSOR OUTPUT PARSE --------------------------------------------------------------
\ PARSE-TENSOR folds a clean child's stdout (whitespace-separated float tokens) into the
\ SUBJ-T compare buffer, returning produced <count> (a parseable non-empty float list) or
\ faulted (an unparseable token, an empty output, or a length over the buffer cap). So the
\ tensor spawn subject is a real isolated bin/hb child whose float tensor is graded exactly
\ like the in-process tensor adapter.
\
\ maki -> habu only. No new error codes (reuses the runner's + lib/process's).

require lib/prelude.f               \ fdrop
require lib/string.f
require lib/float.f                 \ STR>FLOAT (tensor token parse)
require maki/db/diff-runner-spawn.f  \ SPAWN-CAPTURE / SPAWN-MS / SP-OUT / PARSE-INT
require maki/db/diff-runner-tensor.f \ SUBJ-T / SUBJ-T! / T-SUBJECT! / TCAP

package DIFFRUN
private

$400 constant INJ-CAP               \ max injected subject-source bytes
create INJ-SRC INJ-CAP allot        \ stable copy of the subject-under-test source
variable INJ-SRC-U

: INJ-SRC$ ( -- ptr u8 n )   INJ-SRC INJ-SRC-U @ ;

\ ---- whitespace-delimited float tokeniser (fixed buffer; byte-typed cursor) -----------
$800 constant PT-CAP
create PT-BUF PT-CAP allot
variable PT-U variable PT-I variable PT-CNT variable PT-START

: PT-WS? ( n -- bool )   dup 32 = over 10 = or over 13 = or swap 9 = or ;
: PT-C@ ( -- n )   PT-BUF PT-I @ + c@ ;
: PT-AT-WS? ( -- bool )   PT-I @ PT-U @ < if PT-C@ PT-WS? else false then ;
: PT-AT-TOK? ( -- bool )  PT-I @ PT-U @ < if PT-C@ PT-WS? 0= else false then ;
: PT-SKIP-WS ( -- )   begin PT-AT-WS? while 1 PT-I +! repeat ;
: PT-TOKEN ( -- ptr u8 n )   \ the next non-whitespace run
   PT-I @ PT-START !
   begin PT-AT-TOK? while 1 PT-I +! repeat
   PT-BUF PT-START @ +  PT-I @ PT-START @ - ;
: PT-STORE ( r -- bool )   \ store the parsed float at PT-CNT, advance; false if over cap
   PT-CNT @ TCAP >= if fdrop false exit then
   PT-CNT @ SUBJ-T!  1 PT-CNT +!  true ;
: PT-STEP ( -- bool )   \ parse one token into SUBJ-T; false on parse/overflow
   PT-TOKEN STR>FLOAT MATCH option
      none OF false ENDOF
      some OF PT-STORE ENDOF
   ;MATCH ;

public

\ SUBJECT-SRC! stores the subject-under-test source (a checked `: SUBJECT ...` definition):
\ scalar subjects declare ( n -- n ); tensor subjects declare ( n -- ) and print their float
\ output. The runner injects it into a fresh child through INJECT-SCALAR / INJECT-TENSOR.
: SUBJECT-SRC! ( ptr u8 n -- ) {: a:ptr u:n :}
   u INJ-CAP > if E-DIFFRUN-BUF throw then
   a INJ-SRC u BYTE-COPY  u INJ-SRC-U ! ;

\ INJECT-SCALAR composes the injected subject with a harness that binds the case index and
\ prints the scalar SUBJECT ( n -- n ) result; the child signals success by clean completion.
: INJECT-SCALAR ( n -- ptr u8 n ) {: n:n :}
   SB-RESET
   s" require lib/string.f" SB-APPEND   10 SB-APPEND-C
   s" require lib/fmt.f" SB-APPEND      10 SB-APPEND-C
   INJ-SRC$ SB-APPEND                   10 SB-APPEND-C
   s" SB-RESET " SB-APPEND  n SB-INT  s"  SUBJECT SB-INT SB$ type" SB-APPEND  10 SB-APPEND-C
   SB$ ;

\ INJECT-TENSOR composes the injected tensor subject with a harness that binds the case index
\ and runs SUBJECT ( n -- ), which prints its float tensor (whitespace-separated) to stdout.
: INJECT-TENSOR ( n -- ptr u8 n ) {: n:n :}
   SB-RESET
   s" require lib/string.f" SB-APPEND   10 SB-APPEND-C
   s" require lib/fmt.f" SB-APPEND      10 SB-APPEND-C
   s" require lib/float.f" SB-APPEND    10 SB-APPEND-C
   INJ-SRC$ SB-APPEND                   10 SB-APPEND-C
   n SB-INT  s"  SUBJECT" SB-APPEND     10 SB-APPEND-C
   SB$ ;

\ PARSE-TENSOR folds a clean child's whitespace-separated float stdout into SUBJ-T.
: PARSE-TENSOR ( ptr u8 n -- run-result ) {: a:ptr u:n :}
   u PT-CAP > if >FAULTED exit then
   a PT-BUF u BYTE-COPY  u PT-U !  0 PT-I !  0 PT-CNT !
   begin PT-SKIP-WS PT-AT-TOK? while
      PT-STEP 0= if >FAULTED exit then
   repeat
   PT-CNT @ 0= if >FAULTED exit then
   PT-CNT @ >PRODUCED ;

\ SPAWN-INJECTED runs the injected SCALAR subject for case n in a fresh isolated child.
: SPAWN-INJECTED ( n -- run-result )   INJECT-SCALAR SPAWN-MS SPAWN-SRC ;
: INJECT-SUBJECT! ( -- )               [: SPAWN-INJECTED ;] SUBJECT! ;

\ SPAWN-TENSOR-SRC / SPAWN-TENSOR-INJECTED: the real spawned-child tensor subject adapter.
: SPAWN-TENSOR-SRC ( ptr u8 n n -- run-result )
   SPAWN-CAPTURE {: outlen:n clean:bool :}
   clean if SP-OUT outlen PARSE-TENSOR else >FAULTED then ;
: SPAWN-TENSOR-INJECTED ( n -- run-result )   INJECT-TENSOR SPAWN-MS SPAWN-TENSOR-SRC ;
: INJECT-TSUBJECT! ( -- )                     [: SPAWN-TENSOR-INJECTED ;] T-SUBJECT! ;

;package
