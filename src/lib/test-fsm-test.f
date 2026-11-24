split-str
\ test-fsm-test.f - TEST-FSM:TABLE-MODULE on a source file written under
\ TMPDIR: a two-state table renders to its whole Table module past a table
\ and a comment its name prefixes, and a file without the named table or a
\ PICK line fails the running test, seen in a subject child.

require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/arena.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/test-fsm.f
require src/core/bytes.f
require src/core/exit-status.f

package TEST-FSM-TEST

PATH-CAP BUFFER: TFT-DIR-BUF
variable TFT-DIR-U
PATH-CAP BUFFER: TFT-FILE-BUF
variable TFT-FILE-U
$1000 constant TFT-CAP
TFT-CAP BUFFER: TFT-OUT
TFT-CAP BUFFER: TFT-ERR

: TFT-DIR$ ( -- str ) TFT-DIR-BUF TFT-DIR-U @ ;
: TFT-FILE$ ( -- str ) TFT-FILE-BUF TFT-FILE-U @ ;

: TFT-SOURCE ( -- str )
   SB-RESET
   S\" FSM:DEFINE DOOR-LOCK ( ^n ) lock-state lock-input\n" SB-APPEND
   S\"    IN locked LOCK-SENSE turn - >open\n" SB-APPEND
   S\"    FINAL open -\n;DEFINE\n" SB-APPEND
   S\" \\ before FSM:DEFINE DOOR ( ^n ) a b\n" SB-APPEND
   S\" \\ door\nFSM:DEFINE DOOR ( ^n ) door-state door-input\n"
   SB-APPEND
   S\"    IN shut DOOR-SENSE push - >open  hold - >shut\n" SB-APPEND
   S\"    FINAL open -\n;DEFINE\n" SB-APPEND
   S\" FSM:DEFINE GATE ( ^n ) gate-state gate-input\n" SB-APPEND
   S\"    IN shut GATE-SENSE push - PICK-GATE\n" SB-APPEND
   S\"    FINAL open -\n;DEFINE\n" SB-APPEND SB$ ;

\ The fixture file door.f in a temp dir of its own: DOOR-LOCK and a
\ comment that names DOOR before DOOR, then GATE with a PICK line.
: TFT-FIXTURE ( -- )
   s" test-fsm" FS:MKTEMP-DIR {: a:^ u:n :}
   a TFT-DIR-BUF u BYTE-COPY  u TFT-DIR-U !
   TFT-DIR$ s" door.f" TFT-FILE-BUF FS:JOIN-PATH TFT-FILE-U !
   TFT-FILE$ TFT-SOURCE FS:WRITE-ALL ;

: TFT-LINE ( str -- ) SB-APPEND 10 SB-APPEND-C ;

\ The module TABLE-MODULE answers for DOOR as model Door.
: TFT-WANT$ ( -- str )
   SB-RESET s" ---- MODULE DoorTable ----" TFT-LINE
   s" \* The lines of DOOR, " SB-APPEND TFT-FILE$ SB-APPEND
   s" , as <<state, input," TFT-LINE
   s" \* next state>>. A test fails unless this file is that table."
   TFT-LINE  s" Lines == {" TFT-LINE
   S\"     <<\qshut\q, \qpush\q, \qopen\q>>," TFT-LINE
   S\"     <<\qshut\q, \qhold\q, \qshut\q>>" TFT-LINE
   s" }" TFT-LINE  s" ====" TFT-LINE  SB$ ;

: TFT-RENDER ( ^ARENA:arena -- )
   {: ar :}
   TFT-FILE$ s" DOOR" s" Door" ar TEST-FSM:TABLE-MODULE
   TFT-WANT$ T$= ;

: TFT-RENDERS ( -- )
   s" renders a table as its module" T-LABEL
   [: TFT-RENDER ;] ARENA:WITH ;

\ A file without the table answers an empty text.
: TFT-ABSENT ( ^ARENA:arena -- )
   {: ar :}
   TFT-FILE$ s" WINDOW" s" Window" ar TEST-FSM:TABLE-MODULE
   nip 0 T= ;

\ GATE's one PICK line rendered as model Gate.
: TFT-PICKED ( ^ARENA:arena -- )
   {: ar :}
   TFT-FILE$ s" GATE" s" Gate" ar TEST-FSM:TABLE-MODULE
   2drop ;

public

\ The subject child's half of TFT-MISSING: one failure, an empty text.
: TFT-MISSING-CHILD ( -- )
   T-RESET  [: TFT-ABSENT ;] ARENA:WITH  T-REPORT ;

\ The subject child's half of TFT-PICK: one failure, the PICK line.
: TFT-PICK-CHILD ( -- )
   T-RESET  [: TFT-PICKED ;] ARENA:WITH  T-REPORT ;

private

\ The child src prints `test: 1 failure(s)` and exits FAILURE.
: TFT-FAILS ( str -- )
   {: src:^ su:n :}
   src su TFT-OUT TFT-CAP >LEN TFT-ERR TFT-CAP >LEN
   PROCESS:WAIT-FOREVER SUBJECT:RUN {: ou:len eu:len oc :}
   src su TFT-OUT ou LEN>N TFT-ERR eu LEN>N oc
   EXIT-STATUS:FAILURE T-ENDING-EXITED=
   TFT-OUT ou LEN>N s" test: 1 failure(s)" STR:CONTAINS? TTRUE ;

: TFT-MISSING ( -- )
   s" fails without the table" T-LABEL
   s" TEST-FSM-TEST:TFT-MISSING-CHILD" TFT-FAILS ;

: TFT-PICK ( -- )
   s" fails on a PICK line" T-LABEL
   s" TEST-FSM-TEST:TFT-PICK-CHILD" TFT-FAILS ;

public

: RUN ( -- )
   T-RESET
   TFT-FIXTURE
   TFT-RENDERS
   TFT-MISSING
   TFT-PICK
   TFT-DIR$ FS:REMOVE-TREE
   T-REPORT ;

;package

TEST-FSM-TEST:RUN
