\ maki/eval-transcript.f - committed generation-transcript replay (package EVAL).
\
\ The durable pass@k harness: a generation run (model samples + repair rounds)
\ is recorded as a plain-text TRANSCRIPT file, and this module replays it
\ through the committed checker judge - no /tmp scripts, no ad hoc subagent
\ logs (dot habu-eval-matrix-live). Formats v1 and v1.1, line-oriented, one
\ transcript = one target arm (see docs/maki/eval.md):
\
\   habu-eval-transcript v1        header, first significant line (or `v1.1`)
\   target habu-ptx                the target arm, once, before any task
\   task saxpy                     opens/rejoins the tally row for a task
\   sample s1                      starts one generation sample
\   candidate <kernel source>      one authoring round (draft, then repairs)
\   tokens 137                     v1.1 only, OPTIONAL: generator-reported
\                                  model-token count for the candidate line
\                                  above; replaces that candidate's whitespace
\                                  proxy count in the tokens-to-green tally
\   result green|fail              recorded verdict for an EXTERNALLY graded
\                                  sample (e.g. the Triton arm) - a sample is
\                                  either replayed candidates OR one result
\
\ Blank lines and `\ ...` comment lines are ignored. Every `candidate` line is
\ graded live through the shared repair metric engine (maki/eval-repair-loop.f
\ REPAIR-STEP -> EVAL:CHECK-PASSES?, the checker as judge), so replay is
\ deterministic from the committed tree. Per-task tallies: n samples, GREEN =
\ first-attempt passes (the pass@k c), REPAIRED = green only after >=1
\ checker-guided repair, ROUNDS/TOKENS summed over green-reaching replayed
\ samples, REC = externally recorded samples. Malformed input fails closed.
\ One transcript never mixes token units: either every candidate carries a
\ `tokens` directive (TS-TOKSRC@ = TS-TOK-MODEL) or none does (TS-TOK-PROXY);
\ a mixed file is rejected so a tokens-to-green sum is always unit-pure.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require maki/eval-repair-loop.f

\ eval-transcript owns error codes -5262..-5267 and -5272.
-5262 constant E-TS-HEADER   \ missing or duplicate transcript header
-5263 constant E-TS-LINE     \ unknown directive or bad directive argument
-5264 constant E-TS-ORDER    \ directive out of order (target/task/sample/candidate/tokens)
-5265 constant E-TS-CAP      \ task table, name, or file capacity exceeded
-5266 constant E-TS-IDX      \ task index out of range
-5267 constant E-TS-EMPTY    \ sample closed with no candidate and no result
-5272 constant E-TS-TOKENS   \ transcript mixes model-counted and proxy-counted candidates

package EVAL

public

\ tokens-to-green source marker: which unit the summed TOKENS tally carries
0 constant TS-TOK-NONE    \ no replayed token data (recorded-only or empty task)
1 constant TS-TOK-PROXY   \ whitespace source-token proxy (no tokens directives)
2 constant TS-TOK-MODEL   \ generator-reported model tokens (tokens directives)

private

8 constant TS-TASK-MAX
64 constant TS-NAME-CAP
$40000 constant TS-FILE-CAP
1 constant TSV-1          \ header `habu-eval-transcript v1`
2 constant TSV-11         \ header `habu-eval-transcript v1.1` (adds `tokens N`)

\ ---- per-task tally table ----------------------------------------------------
create TS-NAMES TS-TASK-MAX TS-NAME-CAP * allot
create TS-NAME-US TS-TASK-MAX cells allot
6 constant TS-SLOTS
0 constant TSL-N         \ samples graded
1 constant TSL-GREEN     \ first-attempt passes (pass@k c)
2 constant TSL-REPAIRED  \ green only after >=1 repair round
3 constant TSL-ROUNDS    \ repair rounds summed over green replayed samples
4 constant TSL-TOKENS    \ tokens-to-green summed over green replayed samples
5 constant TSL-REC       \ externally recorded samples
create TS-TAB TS-TASK-MAX TS-SLOTS * cells allot

\ ---- parser state --------------------------------------------------------------
create TS-TARGET-BUF TS-NAME-CAP allot
variable TS-TARGET-U
variable TS-TASK#     \ tasks seen
variable TS-CUR       \ current task index
variable TS-HDR?      \ header line consumed
variable TS-OPEN?     \ a sample is open
variable TS-CAND#     \ candidates fed to the open sample
variable TS-REC       \ open-sample recorded verdict: 0 none / 1 green / 2 fail
variable TS-POS       \ line-split cursor
variable TS-BUF-A     \ lazy file buffer
variable TS-VER       \ header format version (TSV-1 / TSV-11)
variable TS-TOKMODE   \ settled token unit for this transcript (TS-TOK-*)
variable TS-PEND?     \ the last candidate may still take a tokens directive
variable TS-CNT?      \ ...and was engine-counted (fed before the sample went green)
variable TS-PROXY     \ ...and contributed this whitespace-proxy token count
variable TS-ADJ       \ open sample: model tokens minus proxy over counted candidates

: TS-SLOT ( n n -- ptr a ) {: idx:n slot:n :}
   idx TS-SLOTS * slot + cells TS-TAB + ;

: TS-CK-IDX ( n -- n ) {: idx:n :}
   idx 0 < idx TS-TASK# @ >= or if E-TS-IDX throw then
   idx ;

: TS-NAME-AT ( n -- ptr u8 n ) {: idx:n :}
   idx TS-NAME-CAP * TS-NAMES +  idx cells TS-NAME-US + @ ;

\ tally row of the open sample's task, one slot
: TS-BUMP ( n n -- ) {: slot:n by:n :}
   TS-CUR @ slot TS-SLOT dup @ by + swap ! ;

\ ---- sample lifecycle ----------------------------------------------------------
\ close the last candidate's tokens window: no `tokens` directive means proxy
\ counting, which a model-counted transcript may not silently mix in
: TS-SETTLE ( -- )
   TS-PEND? @ 0= if exit then
   0 TS-PEND? !
   TS-TOKMODE @ TS-TOK-MODEL = if E-TS-TOKENS throw then
   TS-TOK-PROXY TS-TOKMODE ! ;

: TS-CLOSE-REPLAYED ( -- )
   TSL-N 1 TS-BUMP
   REPAIR-GREEN? 0= if exit then
   REPAIR-ROUNDS@ 0= if TSL-GREEN 1 TS-BUMP else TSL-REPAIRED 1 TS-BUMP then
   TSL-ROUNDS REPAIR-ROUNDS@ TS-BUMP
   TSL-TOKENS REPAIR-TOKENS@ TS-ADJ @ + TS-BUMP ;

: TS-CLOSE-RECORDED ( -- )
   TSL-N 1 TS-BUMP
   TSL-REC 1 TS-BUMP
   TS-REC @ 1 = if TSL-GREEN 1 TS-BUMP then ;

: TS-CLOSE-SAMPLE ( -- )
   TS-SETTLE
   TS-OPEN? @ 0= if exit then
   TS-CAND# @ 0 >          if TS-CLOSE-REPLAYED else
      TS-REC @ 0 >         if TS-CLOSE-RECORDED else
         E-TS-EMPTY throw
      then
   then
   0 TS-OPEN? !  0 TS-CAND# !  0 TS-REC ! ;

\ ---- task rows -----------------------------------------------------------------
\ index of an existing task name, else -1
: TS-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup TS-TASK# @ < while
      dup TS-NAME-AT a u STR= if exit then
      1+
   repeat drop -1 ;

: TS-ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   TS-TASK# @ TS-TASK-MAX >= if E-TS-CAP throw then
   u TS-NAME-CAP > if E-TS-CAP throw then
   a TS-TASK# @ TS-NAME-CAP * TS-NAMES + u BYTE-COPY
   u TS-TASK# @ cells TS-NAME-US + !
   TS-SLOTS 0 ?do 0 TS-TASK# @ i TS-SLOT ! loop
   TS-TASK# @  TS-TASK# @ 1+ TS-TASK# ! ;

\ ---- directives ----------------------------------------------------------------
\ strip a matched directive prefix
: TS-REST ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n p:ptr v:n :}
   a v + u v - ;

\ a name may not contain '|': it is interpolated raw between the rendered
\ matrix column separators, so a pipe would silently misalign the table
: TS-NAME-CK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u $7C INDEX-OF MATCH option
     none OF ENDOF
     some OF drop E-TS-LINE throw ENDOF
   ;MATCH ;

: TS-D-TARGET ( ptr u8 n -- ) {: a:ptr u:n :}
   TS-TARGET-U @ 0 > TS-TASK# @ 0 > or if E-TS-ORDER throw then
   u 0= if E-TS-LINE throw then
   u TS-NAME-CAP > if E-TS-CAP throw then
   a u TS-NAME-CK
   a TS-TARGET-BUF u BYTE-COPY  u TS-TARGET-U ! ;

: TS-D-TASK ( ptr u8 n -- ) {: a:ptr u:n :}
   TS-TARGET-U @ 0= if E-TS-ORDER throw then
   u 0= if E-TS-LINE throw then
   a u TS-NAME-CK
   TS-CLOSE-SAMPLE
   a u TS-FIND dup 0 < if drop a u TS-ADD then
   TS-CUR ! ;

: TS-D-SAMPLE ( ptr u8 n -- ) {: a:ptr u:n :}
   TS-CUR @ 0 < if E-TS-ORDER throw then
   u 0= if E-TS-LINE throw then
   TS-CLOSE-SAMPLE
   REPAIR-RESET
   0 TS-ADJ !
   -1 TS-OPEN? ! ;

: TS-D-CANDIDATE ( ptr u8 n -- ) {: a:ptr u:n :}
   TS-OPEN? @ 0= TS-REC @ 0 > or if E-TS-ORDER throw then
   u 0= if E-TS-LINE throw then
   TS-SETTLE
   REPAIR-GREEN? 0= TS-CNT? !
   REPAIR-TOKENS@ TS-PROXY !
   a u REPAIR-STEP
   REPAIR-TOKENS@ TS-PROXY @ - TS-PROXY !
   -1 TS-PEND? !
   TS-CAND# @ 1+ TS-CAND# ! ;

\ v1.1: the generator-reported model-token count for the candidate line above
\ replaces that candidate's whitespace proxy in the sample's tokens-to-green
: TS-D-TOKENS ( ptr u8 n -- ) {: a:ptr u:n :}
   TS-PEND? @ 0= if E-TS-ORDER throw then
   a u STR-DIGITS? 0= if E-TS-LINE throw then
   a u STR>NUMBER? MATCH option
     none OF E-TS-LINE throw ENDOF
     some OF {: n:n :}
        n 1 < if E-TS-LINE throw then
        TS-TOKMODE @ TS-TOK-PROXY = if E-TS-TOKENS throw then
        0 TS-PEND? !
        TS-TOK-MODEL TS-TOKMODE !
        TS-CNT? @ if n TS-PROXY @ - TS-ADJ @ + TS-ADJ ! then
     ENDOF
   ;MATCH ;

: TS-D-RESULT ( ptr u8 n -- ) {: a:ptr u:n :}
   TS-OPEN? @ 0= TS-CAND# @ 0 > or TS-REC @ 0 > or if E-TS-ORDER throw then
   a u s" green" STR= if 1 TS-REC ! exit then
   a u s" fail"  STR= if 2 TS-REC ! exit then
   E-TS-LINE throw ;

\ header line -> format version, 0 when the line is not a header
: TS-HDR-VER ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" habu-eval-transcript v1"   STR= if TSV-1  exit then
   a u s" habu-eval-transcript v1.1" STR= if TSV-11 exit then
   0 ;

public

: TS-RESET ( -- )
   0 TS-TARGET-U !  0 TS-TASK# !  -1 TS-CUR !
   0 TS-HDR? !  0 TS-OPEN? !  0 TS-CAND# !  0 TS-REC !
   TSV-1 TS-VER !  TS-TOK-NONE TS-TOKMODE !
   0 TS-PEND? !  0 TS-CNT? !  0 TS-PROXY !  0 TS-ADJ ! ;

\ one transcript line (trailing whitespace tolerated; CR for foreign editors)
: TS-LINE ( ptr u8 n -- )
   RTRIM {: a:ptr u:n :}
   u 0= if exit then
   a u s" \" STARTS-WITH? if exit then
   TS-HDR? @ 0= if
      a u TS-HDR-VER dup 0= if E-TS-HEADER throw then
      TS-VER !  -1 TS-HDR? !  exit
   then
   a u TS-HDR-VER 0 <> if E-TS-HEADER throw then
   a u s" target "    STARTS-WITH? if a u s" target "    TS-REST TS-D-TARGET    exit then
   a u s" task "      STARTS-WITH? if a u s" task "      TS-REST TS-D-TASK      exit then
   a u s" sample "    STARTS-WITH? if a u s" sample "    TS-REST TS-D-SAMPLE    exit then
   a u s" candidate " STARTS-WITH? if a u s" candidate " TS-REST TS-D-CANDIDATE exit then
   a u s" result "    STARTS-WITH? if a u s" result "    TS-REST TS-D-RESULT    exit then
   TS-VER @ TSV-11 = if
      a u s" tokens "  STARTS-WITH? if a u s" tokens "   TS-REST TS-D-TOKENS    exit then
   then
   E-TS-LINE throw ;

: TS-END ( -- )
   TS-HDR? @ 0= if E-TS-HEADER throw then
   TS-CLOSE-SAMPLE ;

\ replay one whole transcript (resets, feeds each LF line, finalizes)
: TS-FEED ( ptr u8 n -- ) {: a:ptr u:n :}
   TS-RESET
   0 TS-POS !
   begin a u STR-LF TS-POS @ SPLIT-NEXT while
      TS-POS !
      TS-LINE
   repeat 2drop drop
   TS-END ;

private

: TS-BUF ( -- ptr u8 )
   TS-BUF-A 0 ptr-field {: slot:ptr :}
   slot @ 0= if TS-FILE-CAP MEM-ALLOC-BYTES drop slot ! then
   slot @ ;

public

\ replay a transcript file from disk
: TS-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu TS-BUF TS-FILE-CAP READ-ALL {: u:n :}
   u TS-FILE-CAP >= if E-TS-CAP throw then
   TS-BUF u TS-FEED ;

\ ---- tally accessors -----------------------------------------------------------
: TS-TARGET$   ( -- ptr u8 n )  TS-TARGET-BUF TS-TARGET-U @ ;
: TS-TASKS@    ( -- n )  TS-TASK# @ ;
: TS-TASK$     ( n -- ptr u8 n )  TS-CK-IDX TS-NAME-AT ;
: TS-N@        ( n -- n )  TS-CK-IDX TSL-N        TS-SLOT @ ;
: TS-GREEN@    ( n -- n )  TS-CK-IDX TSL-GREEN    TS-SLOT @ ;
: TS-REPAIRED@ ( n -- n )  TS-CK-IDX TSL-REPAIRED TS-SLOT @ ;
: TS-ROUNDS@   ( n -- n )  TS-CK-IDX TSL-ROUNDS   TS-SLOT @ ;
: TS-TOKENS@   ( n -- n )  TS-CK-IDX TSL-TOKENS   TS-SLOT @ ;
: TS-REC@      ( n -- n )  TS-CK-IDX TSL-REC      TS-SLOT @ ;
\ unit of the task's TOKENS sum; unit-pure by the E-TS-TOKENS mixing rejection
: TS-TOKSRC@   ( n -- n )
   TS-CK-IDX TSL-TOKENS TS-SLOT @ 0= if TS-TOK-NONE exit then
   TS-TOKMODE @ ;

end-package
