\ ffi-callback-test.f - C calling back into checked Habu: qsort, bsearch and
\ pthread_create are the C callers. Each case's facts, with every child's and
\ the stripped image's exit status and streams, form a transcript written to
\ build/ffi-callback-transcript.txt, read back and compared whole. A callback
\ body that defines ends the process; that is a case of its own.
\ Run: bin/hb --load src/lib/ffi-callback-test.f
require src/lib/test.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/test/runner.f
require src/lib/task.f
require test/ffi-callback-fixture.f

package FFI-CB-TEST
using FFI-CB

$3000 constant SCRIPT-CAP
$4000 constant CAP
60000 constant CHILD-MS               \ includes compiling the fixture in each child
600000 constant BUILD-MS
1000 constant OWNER-READS
200 constant HELD-MS                  \ how long a held row keeps a call waiting
10 constant LF

create SCRIPT-BUF SCRIPT-CAP allot    \ the transcript this run captured
create WANT-BUF SCRIPT-CAP allot      \ the transcript this file expects
create BACK-BUF SCRIPT-CAP allot      \ the transcript read back from its file
create OUT CAP allot
create ERR CAP allot
create SUBJECT-BUF PATH-CAP allot
create IMAGE-BUF PATH-CAP allot

variable SCRIPT-U
variable WANT-U
variable SUBJECT-U
variable IMAGE-U
variable MAIN-OWNER                   \ the owner the main thread claims with

\ With the fixture's eleven callbacks these five make sixteen entries, a
\ table-size edge.
CALLBACK: PAD-A ( -- ) ;CALLBACK
CALLBACK: PAD-B ( -- ) ;CALLBACK
CALLBACK: PAD-C ( -- ) ;CALLBACK
CALLBACK: PAD-D ( -- ) ;CALLBACK
CALLBACK: PAD-E ( -- ) ;CALLBACK

: SCRIPT$ ( -- ptr u8 n )
   SCRIPT-BUF SCRIPT-U @ ;

: SCRIPT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SCRIPT-U @ u + SCRIPT-CAP > if E-STR-CAPACITY throw then
   a SCRIPT-BUF SCRIPT-U @ + u BYTE-COPY
   u SCRIPT-U +! ;

: SCRIPT+C ( n -- ) {: c:n :}
   SCRIPT-U @ SCRIPT-CAP >= if E-STR-CAPACITY throw then
   c SCRIPT-BUF SCRIPT-U @ + c!
   1 SCRIPT-U +! ;

: SCRIPT-N ( n -- )
   SB-RESET FMT:SB-INT SB$ SCRIPT+ ;

: SAY ( ptr u8 n -- )
   SCRIPT+ LF SCRIPT+C ;

\ A captured stream, ended by a line feed unless it is empty or ends in one.
: STREAM+ ( ptr u8 n -- ) {: a u:n :}
   u 0= if exit then
   a u SCRIPT+
   a u + 1 - c@ LF <> if LF SCRIPT+C then ;

\ One fact: asserted under its own label and written as a transcript line.
: FACT ( bool ptr u8 n -- ) {: ok:bool a:ptr u:n :}
   a u T-LABEL
   ok TTRUE
   ok if s" ok   " else s" FAIL " then SCRIPT+
   a u SAY ;

: WANT$ ( -- ptr u8 n )
   WANT-BUF WANT-U @ ;

: W ( ptr u8 n -- ) {: a:ptr u:n :}
   WANT-U @ u + 1 + SCRIPT-CAP > if E-STR-CAPACITY throw then
   a WANT-BUF WANT-U @ + u BYTE-COPY
   LF WANT-BUF WANT-U @ + u + c!
   u 1 + WANT-U +! ;

: SORT-FACTS ( n -- ) {: mask:n :}
   mask 1 and 0 <> s" qsort sorts through a callback; a stack cell, a local and a loop index survive" FACT
   mask 2 and 0 <> s" a comparator nests qsort through its own slot" FACT
   mask 4 and 0 <> s" a comparator nests qsort through a second slot on the context" FACT
   mask 8 and 0 <> s" a throwing comparator answers its fallback; FAULT@ keeps the code; depth is intact" FACT
   mask $10 and 0 <> s" UNBIND inside the comparator is E-TASK-STATE and qsort completes" FACT ;

: CASE-MAIN ( -- )
   s" == the main task" SAY
   BIND-SELF SORTS SORT-FACTS ;

\ WHO is a comparator that keeps CB-OWNER of the region it runs on. qsort of a
\ two-cell row calls it on this thread, and every call claims the owner afresh,
\ so one value across a thousand sorts with a yield between them is the thread
\ pointer holding still. The cell is cleared first, so a sort that never
\ called WHO reads the free owner.
: WHO-CLAIM ( n -- n ) {: fn:n :}
   0 WHO-OWNER !
   OUTER BYTE-VIEW 2 CELL fn QSORT
   WHO-OWNER @ ;

: OWNER-STABLE? ( n -- bool ) {: fn:n :}
   fn WHO-CLAIM MAIN-OWNER !
   true OWNER-READS 0 ?do
      TASK:PAUSE
      fn WHO-CLAIM MAIN-OWNER @ = and
   loop ;

: CASE-OWNER ( -- )
   s" == the owner a thread claims with" SAY
   WHO TASK:SELF-CONTEXT ENTRY OWNER-STABLE?
      s" the main thread claims with one value, a thousand calls and yields apart" FACT
   MAIN-OWNER @ 0 <> s" which is not the free owner" FACT
   WHO UNBIND ;

: CASE-WORKERS ( -- )
   UNBIND-SELF
   s" == a worker on its own context, unbinding before it ends" SAY
   ['] WORK-UNBINDS WORKER-A TASK:ACTIVATE
   WORKER-A TASK:JOIN JOINED SORT-FACTS
   s" == a worker on its own context, ending bound" SAY
   ['] WORK-ENDS WORKER-B TASK:ACTIVATE
   WORKER-B TASK:JOIN JOINED SORT-FACTS
   [: BIND-SELF ;] catch 0= s" its rows went with it: the same slots bind to the main context" FACT
   UNBIND-SELF ;

: CALLER-RESET ( n -- ) {: want:n :}
   want CALLER-WANT !
   0 CALLER-INSIDE !  0 CALLER-MET !  0 CALLER-MISSES !
   3 0 ?do 0 CALLER-ENTERED i cells + ! loop ;

\ The x86-64 thunk has no caller row.
: CASE-CALLER ( -- )
   s" == one slot on the caller's context, two tasks inside at once" SAY
   CMP-CALLER TASK:CALLER-CONTEXT ENTRY CMP-CALLER-FN !
   2 CALLER-RESET
   ['] WORK-CALLER-A WORKER-A TASK:ACTIVATE
   ['] WORK-CALLER-B WORKER-B TASK:ACTIVATE
   WORKER-A TASK:JOIN JOINED 1 = WORKER-B TASK:JOIN JOINED 1 = and
      s" both tasks' rows came back sorted through the one pointer" FACT
   CALLER-MET @ 2 = s" both tasks were inside its comparator at once" FACT
   CALLER-MISSES @ 0=
      s" each comparison ran on the region of the task whose row it sorted" FACT
   1 CALLER-RESET
   0 CALLER-UNBIND-CODE !  1 CALLER-UNBIND-ON !
   OUTER 0 CALLER-SORT
   CALLER-UNBIND-CODE @ E-TASK-STATE = and CALLER-MISSES @ 0= and
      s" UNBIND inside its comparator is E-TASK-STATE and qsort completes" FACT
   CMP-CALLER UNBIND
   [: CMP-CALLER TASK:SELF-CONTEXT ENTRY drop ;] catch 0=
      s" UNBIND with no thread inside frees it: the slot binds to the main context" FACT
   CMP-CALLER UNBIND ;

\ Slot k's cell at offset off of its row, reached as the thunk reaches it:
\ through the table the main region's CB-ROWS publishes.
: SLOT-CELL ( n n -- ptr n ) {: k:n off:n :}
   TASK:MAIN-BASE {: base :}
   base CB-ROWS + atomic@ base FFI:>CELL - {: table:n :}
   base table + k CB-ROW-BYTES * + off + ;

\ CMP-CALLER's slot: the one idle caller row while it is bound; -1 if none is.
: CALLER-SLOT ( -- n )
   CMP-CALLER TASK:CALLER-CONTEXT ENTRY drop
   -1 CB-POOL 0 ?do
      i CB-ROW-OWNER SLOT-CELL atomic@ CB-ROW-CALLER = if drop i then
   loop
   CMP-CALLER UNBIND ;

: WORK-CALLER-BIND ( -- )
   CMP-CALLER TASK:CALLER-CONTEXT ENTRY drop 1 TASK:RETURN ;

\ A task binding a slot to the caller's context waits while a mover holds the
\ row BUSY, and binds once it is handed back.
: CASE-CALLER-HELD ( -- )
   s" == a caller bind that meets a row a mover holds" SAY
   CALLER-SLOT {: k:n :}
   k 0 >= if
      0 CB-ROW-BUSY k CB-ROW-OWNER SLOT-CELL atomic-cas 0=
   else false then
   s" CMP-CALLER's idle row is held BUSY" FACT
   k 0 < if exit then
   ['] WORK-CALLER-BIND WORKER-A TASK:ACTIVATE
   HELD-MS >MS TASK:SLEEP
   WORKER-A TASK:DONE? 0= s" the binding task waits there" FACT
   0 k CB-ROW-OWNER SLOT-CELL atomic!
   WORKER-A TASK:JOIN JOINED 1 =
      s" handed back, the row binds and the task does not throw" FACT
   k CB-ROW-OWNER SLOT-CELL atomic@ CB-ROW-CALLER =
      s" and the row is a caller row" FACT
   CMP-CALLER UNBIND ;


: CASE-ACCESSOR ( -- )
   s" == a frame accessor" SAY
   [: 0 ARG@ drop ;] catch E-FFI-CALLBACK-STATE =
      s" a frame accessor outside a callback is E-FFI-CALLBACK-STATE" FACT ;

\ Each rides INCLUDE-EVALUATE, so the refusal is a throw this file catches.
: DECL-BAD-TOKEN ( -- )
   s" CALLBACK: CBT-X1 ( q -- ) ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-BAD-PTR ( -- )
   s" CALLBACK: CBT-X2 ( ptr n -- ) ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-TWO-RESULTS ( -- )
   s" CALLBACK: CBT-X3 ( -- n n ) 0 FALLBACK ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-PTR-RESULT ( -- )
   s" CALLBACK: CBT-X4 ( -- ptr u8 ) ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-NO-FALLBACK ( -- )
   s" CALLBACK: CBT-X5 ( n -- n ) ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-WRONG-FALLBACK ( -- )
   s" CALLBACK: CBT-X6 ( n -- n ) 0.5 FFALLBACK ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-VOID-FALLBACK ( -- )
   s" CALLBACK: CBT-X7 ( n -- ) 0 FALLBACK ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-TOO-MANY-INTS ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" CALLBACK: CBT-X8 ( n n n n n n n -- ) ;CALLBACK" INCLUDE-EVALUATE
   else
      s" CALLBACK: CBT-X8 ( n n n n n n n n n -- ) ;CALLBACK" INCLUDE-EVALUATE
   then ;
: DECL-NINE-FLOATS ( -- )
   s" CALLBACK: CBT-X9 ( r r r r r r r r r -- ) ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-STRAY-CLOSE ( -- )
   s" ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-STRAY-FALLBACK ( -- )
   s" 0 FALLBACK" INCLUDE-EVALUATE ;
: DECL-NESTED ( -- )
   s" CALLBACK: CBT-XA ( -- ) CALLBACK: CBT-XB ( -- ) ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-SEVENTEENTH ( -- )
   s" CALLBACK: CBT-XC ( -- ) ;CALLBACK" INCLUDE-EVALUATE ;
\ A name of NAME-CAP (64) bytes fits; one byte more does not.
: DECL-NAME-65 ( -- )
   s" CALLBACK: CBT-NAME-65-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ( -- ) ;CALLBACK" INCLUDE-EVALUATE ;
: DECL-NAME-64 ( -- )
   s" CALLBACK: CBT-NAME-64-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ( -- ) ;CALLBACK" INCLUDE-EVALUATE ;

: CASE-DECLARATIONS ( -- )
   s" == declarations refused" SAY
   [: DECL-BAD-TOKEN ;] catch E-FFI-SYNTAX = s" an argument type outside n i32 u32 r ptr u8: E-FFI-SYNTAX" FACT
   [: DECL-BAD-PTR ;] catch E-FFI-SYNTAX = s" a pointer that is not ptr u8: E-FFI-SYNTAX" FACT
   [: DECL-TWO-RESULTS ;] catch E-FFI-SYNTAX = s" two results: E-FFI-SYNTAX" FACT
   [: DECL-PTR-RESULT ;] catch E-FFI-SYNTAX = s" a pointer result: E-FFI-SYNTAX" FACT
   [: DECL-NO-FALLBACK ;] catch E-FFI-SYNTAX = s" a value result with no fallback: E-FFI-SYNTAX" FACT
   [: DECL-WRONG-FALLBACK ;] catch E-FFI-SYNTAX = s" FFALLBACK for an integer result: E-FFI-SYNTAX" FACT
   [: DECL-VOID-FALLBACK ;] catch E-FFI-SYNTAX = s" FALLBACK with no result: E-FFI-SYNTAX" FACT
   [: DECL-TOO-MANY-INTS ;] catch E-FFI-ARITY = s" an integer argument past the registers: E-FFI-ARITY" FACT
   [: DECL-NINE-FLOATS ;] catch E-FFI-ARITY = s" a ninth float argument: E-FFI-ARITY" FACT
   [: DECL-STRAY-CLOSE ;] catch E-FFI-SYNTAX = s" ;CALLBACK with nothing open: E-FFI-SYNTAX" FACT
   [: DECL-STRAY-FALLBACK ;] catch E-FFI-SYNTAX = s" FALLBACK with nothing open: E-FFI-SYNTAX" FACT
   [: DECL-NESTED ;] catch E-FFI-SYNTAX = s" CALLBACK: inside an open declaration: E-FFI-SYNTAX" FACT
   [: DECL-SEVENTEENTH ;] catch 0=
      s" the seventeenth declaration succeeds" FACT
   [: DECL-NAME-65 ;] catch E-FFI-SYNTAX = s" a 65-byte name: E-FFI-SYNTAX" FACT
   [: DECL-NAME-64 ;] catch 0= s" a 64-byte name is accepted" FACT ;

: PAD-PAST-EDGE ( -- ) begin DECLARED POW2? while PAD repeat ;

: THROWING$ ( -- ptr u8 n )
   s" CALLBACK: CBT-RB-IN ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK  1 throw" ;

\ Non-zero cells laid over the cut, where a grown table the rollback did not
\ keep would lie.
: JUNK ( -- )
   s" create CBT-RB-JUNK" INCLUDE-EVALUATE
   4000 0 ?do 777 , loop ;

: DECLARE-LATE ( -- )
   s" : CBT-RB-ORDER ( ptr u8 ptr u8 -- n ) {: a b :} a CELL-VIEW @ b CELL-VIEW @ - ;" INCLUDE-EVALUATE
   s" CALLBACK: CBT-RB-LATE ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK" INCLUDE-EVALUATE
   s" ' CBT-RB-ORDER CBT-RB-LATE-BODY !" INCLUDE-EVALUATE ;

: SORTED-BY? ( n -- bool ) {: fn:n :}
   OUTER 0 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL fn QSORT
   OUTER 0 ROW-SORTED? ;

: BOUND$ ( -- ptr u8 n )
   s" CALLBACK: CBT-RB-BOUND ( -- ) ;CALLBACK  CBT-RB-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop  1 throw" ;

\ CBT-RB-NEXT takes slot n. A caller bind refuses a row that still names a
\ region, which a main-region bind of the region the thrown text left would accept.
: NEXT-BINDS? ( n -- bool ) {: k:n :}
   s" CALLBACK: CBT-RB-NEXT ( -- ) ;CALLBACK" INCLUDE-EVALUATE
   DECLARED k 1 + =
   s" CBT-RB-NEXT TASK:CALLER-CONTEXT FFI-CB:ENTRY drop" TEST-EVAL:RC 0= and ;

\ Only a caller bind leaves the row owner set when the text throws.
: CALLER-BOUND$ ( -- ptr u8 n )
   s" CALLBACK: CBT-RB-CALLER ( -- ) ;CALLBACK  CBT-RB-CALLER TASK:CALLER-CONTEXT FFI-CB:ENTRY drop  1 throw" ;

: SELF-BINDS? ( n -- bool ) {: k:n :}
   s" CALLBACK: CBT-RB-SELF ( -- ) ;CALLBACK" INCLUDE-EVALUATE
   DECLARED k 1 + =
   s" CBT-RB-SELF TASK:SELF-CONTEXT FFI-CB:ENTRY drop" TEST-EVAL:RC 0= and ;

\ The THROWING$ text at the edge grows the descriptor and task-row tables; C
\ then calls through the grown tables the main region publishes.
: CASE-ROLLBACK ( -- )
   s" == a callback declared in a text that throws" SAY
   PAD-PAST-EDGE DECLARED {: before:n :}
   THROWING$ TEST-EVAL:RC 1 = DECLARED before = and
      s" the rollback leaves the slot count as it was" FACT
   BOUND$ TEST-EVAL:RC 1 = before NEXT-BINDS? and
      s" the next declaration takes the slot a thrown text bound, unbound" FACT
   s" CBT-RB-NEXT FFI-CB:UNBIND" INCLUDE-EVALUATE
   DECLARED {: slot:n :}
   CALLER-BOUND$ TEST-EVAL:RC 1 = slot SELF-BINDS? and
      s" a slot a thrown text bound as the caller binds to this context" FACT
   s" CBT-RB-SELF FFI-CB:UNBIND" INCLUDE-EVALUATE
   PAD-TO-EDGE DECLARED {: edge:n :}
   CMP TASK:SELF-CONTEXT ENTRY {: early:n :}
   THROWING$ TEST-EVAL:RC 1 = DECLARED edge = and
      s" a throw after a declaration that grew the tables keeps the slot count" FACT
   JUNK
   early SORTED-BY?
      s" storage laid over the cut, a callback bound before it sorts" FACT
   DECLARE-LATE
   s" CBT-RB-LATE TASK:SELF-CONTEXT FFI-CB:ENTRY" TEST-EVAL:N SORTED-BY?
      s" a callback declared after the rollback sorts" FACT
   early SORTED-BY? s" and the earlier one still does" FACT
   s" CBT-RB-LATE FFI-CB:UNBIND" INCLUDE-EVALUATE
   CMP UNBIND ;

\ A kernel exit line ends ` lr <16 hex>`; the transcript keeps ` lr` and drops
\ the digits, which vary with the build.
: LR-LINE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 20 < if false exit then
   a u 16 - s"  lr " ENDS-WITH? 0= if false exit then
   true u u 16 - ?do a i + c@ STR-HEX-DIGIT? and loop ;

: ERR-LINE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LR-LINE? if a u 17 - SAY exit then
   a u SAY ;

\ A captured stderr as STREAM+ writes it, each line through ERR-LINE+.
: ERR-STREAM+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LF 0 begin SPLIT-NEXT while {: p:ptr pu:n next:n :}
      pu 0 <>  next u > 0=  or if p pu ERR-LINE+ then
      a u LF next
   repeat drop 2drop ;

\ The name, the exit status and both streams of one spawned process.
: TRANSCRIBE-RC ( ptr u8 n len len n -- )
   {: name:ptr nameu:n outu:len erru:len rc:n :}
   s" == child " SCRIPT+ name nameu SCRIPT+ s" : exit " SCRIPT+ rc SCRIPT-N LF SCRIPT+C
   OUT outu LEN>N STREAM+
   s" --" SAY
   ERR erru LEN>N ERR-STREAM+ ;

: TRANSCRIBE ( ptr u8 n len len PROCESS:ending -- )
   PROCESS:ENDING>RC RC>N TRANSCRIBE-RC ;

: CHILD ( ptr u8 n -- len len PROCESS:ending ) {: name:ptr nameu:n :}
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/ffi-callback-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   name nameu >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: CHILD-CASE ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu name nameu CHILD TRANSCRIBE ;

: CASE-CHILDREN ( -- )
   s" outside" CHILD-CASE
   s" sleeping" CHILD-CASE
   s" unbound" CHILD-CASE
   s" worker-stale" CHILD-CASE
   s" body-defines" CHILD-CASE
   HB-TARGET-LINUX-X86-64? 0= if s" caller-thread" CHILD-CASE then
   s" released" CHILD-CASE
   s" grown-bind" CHILD-CASE ;

\ Every comparison answers the declared fallback, so bsearch matches its first
\ probe under UNSET's 0 and no row under UNSET-ONE's 1.
: CASE-UNSET ( -- )
   s" == a body never stored" SAY
   UNSET TASK:SELF-CONTEXT ENTRY {: fn:n :}
   OUTER BYTE-VIEW OUTER BYTE-VIEW ROW-N CELL fn BSEARCH
   OUTER NULL-PTR - ROW-N 2 / cells + =
      s" bsearch through it answers the fallback: its first probe matches" FACT
   UNSET FAULT@ E-FFI-CALLBACK-STATE = s" and faults E-FFI-CALLBACK-STATE" FACT
   UNSET UNBIND
   OUTER BYTE-VIEW OUTER BYTE-VIEW ROW-N CELL UNSET-ONE TASK:SELF-CONTEXT ENTRY
   BSEARCH 0= s" a fallback of 1 reaches C too: no row matches" FACT
   UNSET-ONE UNBIND ;

: SUBJECT$ ( -- ptr u8 n ) SUBJECT-BUF SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: WRITE-SUBJECT ( -- )
   SUBJECT$ S\" require src/lib/ffi-callback.f\nCALLBACK: FILE-BOUND ( -- ) ;CALLBACK\n" FS:WRITE-ALL
   \ Numeric fallbacks grow and retire descriptors before capture. MAIN's seed
   \ entry needs a code span disjoint from the shared thunk's.
   SUBJECT$ S\" CALLBACK: FIRST ( -- n ) 456 FALLBACK ;CALLBACK\nCALLBACK: SECOND ( -- n ) 456 FALLBACK ;CALLBACK\n" FS:APPEND-FILE
   \ FILE-BOUND, the first row, is bound while the build loads; the capture
   \ drops the binding.
   SUBJECT$ S\" FILE-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop\n" FS:APPEND-FILE
   SUBJECT$ S\" require test/stripped-callback-subject.f\n: MAIN ( -- ) 0 FFI:CALLBACK-ENTRY drop STRIPPED-CALLBACK-SUBJECT:RUN ;\n" FS:APPEND-FILE ;

: BUILD-IMAGE ( -- len len PROCESS:ending )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   SUBJECT$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: RUN-IMAGE ( -- len len PROCESS:ending )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN
   OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: LINKED ( len len PROCESS:ending -- )
   PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if OUT outu LEN>N type ERR erru LEN>N type then
   rc 0= s" hb-build links test/stripped-callback-subject.f stripped" FACT ;

: STRIPPED-BODY ( -- )
   s" ffi-callback" GT-START
   s" subject.f" SUBJECT-BUF GT-PATH SUBJECT-U !
   s" application" IMAGE-BUF GT-PATH IMAGE-U !
   WRITE-SUBJECT
   BUILD-IMAGE LINKED
   s" stripped" RUN-IMAGE TRANSCRIBE ;

: CASE-STRIPPED ( -- )
   s" == a stripped image" SAY
   [: STRIPPED-BODY ;] [: GT-CLEANUP ;] finally ;

: WANT-SORTS ( -- )
   s" ok   qsort sorts through a callback; a stack cell, a local and a loop index survive" W
   s" ok   a comparator nests qsort through its own slot" W
   s" ok   a comparator nests qsort through a second slot on the context" W
   s" ok   a throwing comparator answers its fallback; FAULT@ keeps the code; depth is intact" W
   s" ok   UNBIND inside the comparator is E-TASK-STATE and qsort completes" W ;

: WANT-TASKS ( -- )
   s" == the main task" W
   WANT-SORTS
   s" == a body never stored" W
   s" ok   bsearch through it answers the fallback: its first probe matches" W
   s" ok   and faults E-FFI-CALLBACK-STATE" W
   s" ok   a fallback of 1 reaches C too: no row matches" W
   s" == the owner a thread claims with" W
   s" ok   the main thread claims with one value, a thousand calls and yields apart" W
   s" ok   which is not the free owner" W
   s" == a worker on its own context, unbinding before it ends" W
   WANT-SORTS
   s" == a worker on its own context, ending bound" W
   WANT-SORTS
   s" ok   its rows went with it: the same slots bind to the main context" W
   HB-TARGET-LINUX-X86-64? 0= if
      s" == one slot on the caller's context, two tasks inside at once" W
      s" ok   both tasks' rows came back sorted through the one pointer" W
      s" ok   both tasks were inside its comparator at once" W
      s" ok   each comparison ran on the region of the task whose row it sorted" W
      s" ok   UNBIND inside its comparator is E-TASK-STATE and qsort completes" W
      s" ok   UNBIND with no thread inside frees it: the slot binds to the main context" W
      s" == a caller bind that meets a row a mover holds" W
      s" ok   CMP-CALLER's idle row is held BUSY" W
      s" ok   the binding task waits there" W
      s" ok   handed back, the row binds and the task does not throw" W
      s" ok   and the row is a caller row" W
   then ;

: WANT-ACCESSOR ( -- )
   s" == a frame accessor" W
   s" ok   a frame accessor outside a callback is E-FFI-CALLBACK-STATE" W ;

: WANT-DECLARATIONS ( -- )
   s" == declarations refused" W
   s" ok   an argument type outside n i32 u32 r ptr u8: E-FFI-SYNTAX" W
   s" ok   a pointer that is not ptr u8: E-FFI-SYNTAX" W
   s" ok   two results: E-FFI-SYNTAX" W
   s" ok   a pointer result: E-FFI-SYNTAX" W
   s" ok   a value result with no fallback: E-FFI-SYNTAX" W
   s" ok   FFALLBACK for an integer result: E-FFI-SYNTAX" W
   s" ok   FALLBACK with no result: E-FFI-SYNTAX" W
   s" ok   an integer argument past the registers: E-FFI-ARITY" W
   s" ok   a ninth float argument: E-FFI-ARITY" W
   s" ok   ;CALLBACK with nothing open: E-FFI-SYNTAX" W
   s" ok   FALLBACK with nothing open: E-FFI-SYNTAX" W
   s" ok   CALLBACK: inside an open declaration: E-FFI-SYNTAX" W
   s" ok   the seventeenth declaration succeeds" W
   s" ok   a 65-byte name: E-FFI-SYNTAX" W
   s" ok   a 64-byte name is accepted" W ;

: WANT-ROLLBACK ( -- )
   s" == a callback declared in a text that throws" W
   s" ok   the rollback leaves the slot count as it was" W
   s" ok   the next declaration takes the slot a thrown text bound, unbound" W
   s" ok   a slot a thrown text bound as the caller binds to this context" W
   s" ok   a throw after a declaration that grew the tables keeps the slot count" W
   s" ok   storage laid over the cut, a callback bound before it sorts" W
   s" ok   a callback declared after the rollback sorts" W
   s" ok   and the earlier one still does" W ;

: WANT-CHILDREN ( -- )
   s" == child outside: exit 106" W
   s" outside: a thread is parked on its own context" W
   s" --" W
   s" E-CALLBACK habu: context is not inside a foreign call lr" W
   s" == child sleeping: exit 106" W
   s" sleeping: a thread is parked on its own context" W
   s" --" W
   s" E-CALLBACK habu: context busy on another thread lr" W
   s" == child unbound: exit 106" W
   s" unbound: sorted while bound" W
   s" --" W
   s" E-CALLBACK habu: slot is not bound lr" W
   s" == child worker-stale: exit 106" W
   s" worker-stale: the worker sorted and ended bound" W
   s" --" W
   s" E-CALLBACK habu: slot is not bound lr" W
   s" == child body-defines: exit 106" W
   s" --" W
   s" E-CALLBACK habu: the body defined lr" W
   HB-TARGET-LINUX-X86-64? 0= if
      s" == child caller-thread: exit 106" W
      s" caller-thread: the slot is bound to the caller's context" W
      s" --" W
      s" E-CALLBACK habu: thread has no region of its own lr" W
   then
   s" == child released: exit 0" W
   s" halted: the worker is parked inside its comparator" W
   s" halted: the comparator returned, qsort completed and KILL joined the worker" W
   s" killed: the worker is stopped inside its comparator" W
   s" killed: its sort completed" W
   s" killed: the worker ended at its first TASK:PAUSE after the sort" W
   s" --" W
   s" == child grown-bind: exit 0" W
   s" grown-bind: sorted" W
   s" --" W
   s" == a stripped image" W
   s" ok   hb-build links test/stripped-callback-subject.f stripped" W
   s" == child stripped: exit 0" W
   s" stripped-callback: ok" W
   s" --" W ;

: WANT-TRANSCRIPT ( -- )
   0 WANT-U !
   WANT-TASKS WANT-ACCESSOR WANT-DECLARATIONS WANT-ROLLBACK WANT-CHILDREN ;

\ The artifact as another reader will find it: read back from its file and
\ compared whole.
: TRANSCRIPT-CASE ( -- )
   s" build" FS:MAKE-DIRS
   s" build/ffi-callback-transcript.txt" SCRIPT$ FS:WRITE-ALL
   s" build/ffi-callback-transcript.txt" BACK-BUF SCRIPT-CAP FS:READ-ALL {: got:n :}
   WANT-TRANSCRIPT
   s" build/ffi-callback-transcript.txt is the transcript this file expects" T-LABEL
   BACK-BUF got WANT$ T$= ;

public

: RUN-SUITE ( -- )
   T-RESET
   0 SCRIPT-U !
   CASE-MAIN
   CASE-UNSET
   CASE-OWNER
   CASE-WORKERS
   HB-TARGET-LINUX-X86-64? 0= if CASE-CALLER CASE-CALLER-HELD then
   CASE-ACCESSOR
   CASE-DECLARATIONS
   CASE-ROLLBACK
   CASE-CHILDREN
   CASE-STRIPPED
   TRANSCRIPT-CASE
   T-REPORT ;

;using
;package

FFI-CB-TEST:RUN-SUITE
