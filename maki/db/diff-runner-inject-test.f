\ maki/db/diff-runner-inject-test.f - REAL spawned-child acceptance for subject-source
\ INJECTION and the tensor spawn adapter (maki/db/diff-runner-inject.f; dot
\ habu-v2-differential-runner-13359019).
\
\ Proven with ACTUAL spawned bin/hb children (not constructed outcomes):
\   INJ-* : subject-source injection - the runner hands a checked `: SUBJECT ( n -- n )`
\           source + a case input to a fresh isolated child. A DIFFERENT injected subject
\           yields a DIFFERENT, correctly-classified produced value (2n vs n*n at case 5 ->
\           10 vs 25); a dying injected subject faults, never a value.
\   TINJ-*: the real spawned-child TENSOR subject - an injected `: SUBJECT ( n -- )` prints
\           its float tensor, parsed elementwise into SUBJ-T (produced <count>). A different
\           injected tensor subject yields a different tensor (element 2: i vs i*i -> 2 vs 4);
\           a dying tensor subject faults.
\
\ This is the real-process complement to maki/db/diff-runner-tensor-test.f (constructed
\ tensor outcomes) and the injection follow-on to maki/db/diff-runner-spawn-test.f (the
\ identity-subject isolation proof). Child sources use S\" escaped literals. Reopens package
\ DIFFRUN (a friend) for the injection + tensor-spawn surface.

require lib/test.f
require lib/string.f
require lib/float.f
require maki/db/diff-runner-inject.f

package DIFFRUN

: TCLOSE ( r r -- bool )   f- fabs 0.0001 f< ;

\ ---- injected SCALAR subjects (checked `: SUBJECT ( n -- n )`) ------------------------
: INJ-A ( -- ptr u8 n )      s" : SUBJECT ( n -- n ) 2 * ;" ;
: INJ-B ( -- ptr u8 n )      s" : SUBJECT ( n -- n ) dup * ;" ;
: INJ-FAULT ( -- ptr u8 n )  s\" : SUBJECT ( n -- n ) drop s\q boom\q 7 die ;" ;

\ ---- injected TENSOR subjects (checked `: SUBJECT ( n -- )` printing a float tensor) ---
: TINJ-A ( -- ptr u8 n )      s" : SUBJECT ( n -- ) drop 4 0 ?do i s>f 6 FMT:F.N 32 emit loop ;" ;
: TINJ-B ( -- ptr u8 n )      s" : SUBJECT ( n -- ) drop 4 0 ?do i s>f i s>f f* 6 FMT:F.N 32 emit loop ;" ;
: TINJ-FAULT ( -- ptr u8 n )  s\" : SUBJECT ( n -- ) drop s\q boom\q 7 die ;" ;

\ ---- scalar injection legs -----------------------------------------------------------
: INJ-A-VAL ( -- n )    INJ-A SUBJECT-SRC!  5 SPAWN-INJECTED RUN-RESULT-VALUE ;
: INJ-B-VAL ( -- n )    INJ-B SUBJECT-SRC!  5 SPAWN-INJECTED RUN-RESULT-VALUE ;
: INJ-FAULT-N ( -- n )  INJ-FAULT SUBJECT-SRC!  5 SPAWN-INJECTED RUN-RESULT>N ;
: INJ-DIFFERENT ( -- bool )
   INJ-A SUBJECT-SRC!  5 SPAWN-INJECTED RUN-RESULT-VALUE
   INJ-B SUBJECT-SRC!  5 SPAWN-INJECTED RUN-RESULT-VALUE
   <> ;

\ ---- tensor injection legs -----------------------------------------------------------
: TINJ-A-LEN ( -- n )    TINJ-A SUBJECT-SRC!  5 SPAWN-TENSOR-INJECTED RUN-RESULT-VALUE ;
: TINJ-A-E2 ( -- r )     TINJ-A SUBJECT-SRC!  5 SPAWN-TENSOR-INJECTED drop  2 SUBJ-T@ ;
: TINJ-B-E2 ( -- r )     TINJ-B SUBJECT-SRC!  5 SPAWN-TENSOR-INJECTED drop  2 SUBJ-T@ ;
: TINJ-FAULT-N ( -- n )  TINJ-FAULT SUBJECT-SRC!  5 SPAWN-TENSOR-INJECTED RUN-RESULT>N ;
: TINJ-DIFFERENT ( -- bool )   TINJ-A-E2 TINJ-B-E2 TCLOSE 0= ;

T-RESET

\ ---- subject-source injection: different subject -> different classified outcome -----
INJ-A-VAL 10 T=            \ injected `2 *` at case 5 -> produced 10
INJ-B-VAL 25 T=           \ injected `dup *` at case 5 -> produced 25
INJ-DIFFERENT TTRUE       \ a different injected subject produces a different outcome
INJ-FAULT-N 1 T=          \ a dying injected subject faults, never a value

\ ---- real spawned-child tensor subject -----------------------------------------------
TINJ-A-LEN 4 T=           \ the injected tensor subject prints 4 floats -> produced <4>
TINJ-A-E2 2.0 TCLOSE TTRUE   \ subject `i`   -> element 2 = 2.0
TINJ-B-E2 4.0 TCLOSE TTRUE   \ subject `i*i` -> element 2 = 4.0
TINJ-DIFFERENT TTRUE      \ a different injected tensor subject produces a different tensor
TINJ-FAULT-N 1 T=         \ a dying tensor subject faults

T-REPORT

;package
