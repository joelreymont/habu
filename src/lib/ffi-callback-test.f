\ ffi-callback-test.f - C calling back into checked Habu: qsort, bsearch and
\ pthread_create are the C callers. Each case's facts, with every child's and
\ the stripped image's exit status and streams, form a transcript written to
\ build/ffi-callback-transcript.txt, read back and compared whole. A callback
\ body that defines ends the process; that is a case of its own.
\ Run: bin/hb --load src/lib/ffi-callback-test.f
require src/core/exit-status.f
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
require test/decl-diag-capture.f

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

: SCRIPT$ ( -- ^u8 n )
   SCRIPT-BUF SCRIPT-U @ ;

: SCRIPT+ ( ^u8 n -- ) {: a:^ u:n :}
   SCRIPT-U @ u + SCRIPT-CAP > if E-STR-CAPACITY throw then
   a SCRIPT-BUF SCRIPT-U @ + u BYTE-COPY
   u SCRIPT-U +! ;

: SCRIPT+C ( n -- ) {: c:n :}
   SCRIPT-U @ SCRIPT-CAP >= if E-STR-CAPACITY throw then
   c SCRIPT-BUF SCRIPT-U @ + c!
   1 SCRIPT-U +! ;

: SCRIPT-N ( n -- )
   SB-RESET FMT:SB-INT SB$ SCRIPT+ ;

: SAY ( ^u8 n -- )
   SCRIPT+ LF SCRIPT+C ;

\ A captured stream, ended by a line feed unless it is empty or ends in one.
: STREAM+ ( ^u8 n -- ) {: a u:n :}
   u 0= if exit then
   a u SCRIPT+
   a u + 1 - c@ LF <> if LF SCRIPT+C then ;

\ One fact: asserted under its own label and written as a transcript line.
: FACT ( bool ^u8 n -- ) {: ok:bool a:^ u:n :}
   a u T-LABEL
   ok TTRUE
   ok if s" ok   " else s" FAIL " then SCRIPT+
   a u SAY ;

: WANT$ ( -- ^u8 n )
   WANT-BUF WANT-U @ ;

: W ( ^u8 n -- ) {: a:^ u:n :}
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
: SLOT-CELL ( n n -- ^n ) {: k:n off:n :}
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

\ Each rides evaluate-closed, so the rejection's stop is a throw this file
\ catches.
: DECL-BAD-TOKEN ( -- )
   s" CALLBACK: CB ( nosuch -- n ) 0 FALLBACK ;CALLBACK" evaluate-closed ;
: DECL-PTR-WORD ( -- )
   s" CALLBACK: CBT-XP ( ptr u8 -- ) ;CALLBACK" evaluate-closed ;
: DECL-BAD-PTR ( -- )
   s" CALLBACK: CBT-X2 ( ^n -- ) ;CALLBACK" evaluate-closed ;
: DECL-TWO-RESULTS ( -- )
   s" CALLBACK: CBT-X3 ( -- n n ) 0 FALLBACK ;CALLBACK" evaluate-closed ;
: DECL-PTR-RESULT ( -- )
   s" CALLBACK: CBT-X4 ( -- ^u8 ) ;CALLBACK" evaluate-closed ;
: DECL-NO-FALLBACK ( -- )
   s" CALLBACK: CBT-X5 ( n -- n ) ;CALLBACK" evaluate-closed ;
: DECL-WRONG-FALLBACK ( -- )
   s" CALLBACK: CBT-X6 ( n -- n ) 0.5 FFALLBACK ;CALLBACK" evaluate-closed ;
: DECL-VOID-FALLBACK ( -- )
   s" CALLBACK: CBT-X7 ( n -- ) 0 FALLBACK ;CALLBACK" evaluate-closed ;
: DECL-TOO-MANY-INTS ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" CALLBACK: CBT-X8 ( n n n n n n n -- ) ;CALLBACK" evaluate-closed
   else
      s" CALLBACK: CBT-X8 ( n n n n n n n n n -- ) ;CALLBACK" evaluate-closed
   then ;
: DECL-NINE-FLOATS ( -- )
   s" CALLBACK: CBT-X9 ( r r r r r r r r r -- ) ;CALLBACK" evaluate-closed ;
: DECL-STRAY-CLOSE ( -- )
   s" ;CALLBACK" evaluate-closed ;
: DECL-STRAY-FALLBACK ( -- )
   s" 0 FALLBACK" evaluate-closed ;
: DECL-NESTED ( -- )
   s" CALLBACK: CBT-XA ( -- ) CALLBACK: CBT-XB ( -- ) ;CALLBACK" evaluate-closed ;
: DECL-SEVENTEENTH ( -- )
   s" CALLBACK: CBT-XC ( -- ) ;CALLBACK" evaluate-closed ;
: DECL-COLONS ( -- )
   s" CALLBACK: A:B:C ( -- ) ;CALLBACK" evaluate-closed ;
: DECL-EMPTY-WORD ( -- )
   s" CALLBACK: A: ( -- ) ;CALLBACK" evaluate-closed ;
: DECL-EMPTY-PACKAGE ( -- )
   s" CALLBACK: :B ( -- ) ;CALLBACK" evaluate-closed ;
\ A 100-byte name, then its descriptor called by the whole name.
: DECL-NAME-100 ( -- )
   s" CALLBACK: CBT-NAME-100-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ( -- ) ;CALLBACK  CBT-NAME-100-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX FFI-CB:CLEAR" evaluate-closed ;

\ The quotation stops with the rejection status after a record of code c cu.
: REJECTS? ( [ -- ] ^u8 n -- bool ) {: q c:^ cu:n :}
   REJECTION:CAPTURE
   q catch EXIT-STATUS:REJECTED =
   REJECTION:BUFFER$ c cu CONTAINS? and
   REJECTION:BUFFER-OFF ;

\ Whether Q stops with the rejection status after a record holding HEAD and HELP.
: RECORD? ( [ -- ] ^u8 n ^u8 n -- bool )
   {: q h:^ hu:n p:^ pu:n :}
   DECL-DIAG:PROSE  q catch EXIT-STATUS:REJECTED =
   h hu DECL-DIAG:HAS? and  p pu DECL-DIAG:HAS? and
   DECL-DIAG:OFF ;

: INT-ARITY$ ( -- ^u8 n )
   HB-TARGET-LINUX-X86-64? if
      s" error[E-FFI-ARITY]: a callback takes at most 6 integer arguments"
   else
      s" error[E-FFI-ARITY]: a callback takes at most 8 integer arguments"
   then ;

: ARITY-HELP$ ( -- ^u8 n )
   s" = help: pass the rest in memory through a ^u8 argument" ;

: CASE-DECLARATIONS ( -- )
   s" == declarations rejected" SAY
   [: DECL-BAD-TOKEN ;]
      s" error[E-FFI-SYNTAX]: unknown argument type 'nosuch'"
      s" = help: an argument is n, i32, u32, r or ^u8" RECORD?
      s" an argument type outside n i32 u32 r ^u8: E-FFI-SYNTAX" FACT
   [: DECL-PTR-WORD ;]
      s" error[E-FFI-SYNTAX]: unknown argument type 'ptr'"
      s" = help: an argument is n, i32, u32, r or ^u8" RECORD?
      s" a pointer spelled ptr, not ^u8: E-FFI-SYNTAX" FACT
   [: DECL-BAD-PTR ;] s" E-FFI-SYNTAX" REJECTS? s" a pointer that is not ^u8: E-FFI-SYNTAX" FACT
   [: DECL-TWO-RESULTS ;] s" E-FFI-SYNTAX" REJECTS? s" two results: E-FFI-SYNTAX" FACT
   [: DECL-PTR-RESULT ;] s" E-FFI-SYNTAX" REJECTS? s" a pointer result: E-FFI-SYNTAX" FACT
   [: DECL-NO-FALLBACK ;] s" E-FFI-SYNTAX" REJECTS? s" a value result with no fallback: E-FFI-SYNTAX" FACT
   [: DECL-WRONG-FALLBACK ;] s" E-FFI-SYNTAX" REJECTS? s" FFALLBACK for an integer result: E-FFI-SYNTAX" FACT
   [: DECL-VOID-FALLBACK ;] s" E-FFI-SYNTAX" REJECTS? s" FALLBACK with no result: E-FFI-SYNTAX" FACT
   [: DECL-TOO-MANY-INTS ;] INT-ARITY$ ARITY-HELP$ RECORD?
      s" an integer argument past the registers: E-FFI-ARITY" FACT
   [: DECL-NINE-FLOATS ;]
      s" error[E-FFI-ARITY]: a callback takes at most 8 float arguments"
      ARITY-HELP$ RECORD?
      s" a ninth float argument: E-FFI-ARITY" FACT
   [: DECL-STRAY-CLOSE ;] s" E-FFI-SYNTAX" REJECTS? s" ;CALLBACK with nothing open: E-FFI-SYNTAX" FACT
   [: DECL-STRAY-FALLBACK ;] s" E-FFI-SYNTAX" REJECTS? s" FALLBACK with nothing open: E-FFI-SYNTAX" FACT
   [: DECL-NESTED ;]
      s" error[E-UNCLOSED]: `CALLBACK:` is still open when `CALLBACK:` opens"
      s" = help: close it with `;CALLBACK` before `CALLBACK:`" RECORD?
      s" CALLBACK: inside an open declaration: E-UNCLOSED" FACT
   [: DECL-SEVENTEENTH ;] catch 0=
      s" the seventeenth declaration succeeds" FACT
   [: DECL-COLONS ;]
      s" error[E-FFI-SYNTAX]: more than one ':' in callback name 'A:B:C'"
      s" = help: name it NAME, or PACKAGE:NAME" RECORD?
      s" a name with two ':': E-FFI-SYNTAX" FACT
   [: DECL-EMPTY-WORD ;]
      s" error[E-FFI-SYNTAX]: an empty package or word beside ':' in callback name 'A:'"
      s" = help: name it NAME, or PACKAGE:NAME" RECORD?
      s" a name ending in ':': E-FFI-SYNTAX" FACT
   [: DECL-EMPTY-PACKAGE ;]
      s" error[E-FFI-SYNTAX]: an empty package or word beside ':' in callback name ':B'"
      s" = help: name it NAME, or PACKAGE:NAME" RECORD?
      s" a name starting with ':': E-FFI-SYNTAX" FACT
   [: DECL-NAME-100 ;] catch 0= s" a 100-byte name is kept whole" FACT ;

: PAD-PAST-EDGE ( -- ) begin DECLARED POW2? while PAD repeat ;

: THROWING$ ( -- ^u8 n )
   s" CALLBACK: CBT-RB-IN ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK  1 throw" ;

\ Non-zero cells laid over the cut, where a grown table the rollback did not
\ keep would lie.
: JUNK ( -- )
   s" create CBT-RB-JUNK" evaluate-closed
   4000 0 ?do 777 , loop ;

: DECLARE-LATE ( -- )
   s" : CBT-RB-ORDER ( ^u8 ^u8 -- n ) {: a b :} a CELL-VIEW @ b CELL-VIEW @ - ;" evaluate-closed
   s" CALLBACK: CBT-RB-LATE ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK" evaluate-closed
   s" ' CBT-RB-ORDER CBT-RB-LATE-BODY !" evaluate-closed ;

: SORTED-BY? ( n -- bool ) {: fn:n :}
   OUTER 0 ROW-FILL
   OUTER BYTE-VIEW ROW-N CELL fn QSORT
   OUTER 0 ROW-SORTED? ;

: BOUND$ ( -- ^u8 n )
   s" CALLBACK: CBT-RB-BOUND ( -- ) ;CALLBACK  CBT-RB-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop  1 throw" ;

\ CBT-RB-NEXT takes slot n. A caller bind refuses a row that still names a
\ region, which a main-region bind of the region the thrown text left would accept.
: NEXT-BINDS? ( n -- bool ) {: k:n :}
   s" CALLBACK: CBT-RB-NEXT ( -- ) ;CALLBACK" evaluate-closed
   DECLARED k 1 + =
   s" CBT-RB-NEXT TASK:CALLER-CONTEXT FFI-CB:ENTRY drop" TEST-EVAL:RC 0= and ;

\ Only a caller bind leaves the row owner set when the text throws.
: CALLER-BOUND$ ( -- ^u8 n )
   s" CALLBACK: CBT-RB-CALLER ( -- ) ;CALLBACK  CBT-RB-CALLER TASK:CALLER-CONTEXT FFI-CB:ENTRY drop  1 throw" ;

: SELF-BINDS? ( n -- bool ) {: k:n :}
   s" CALLBACK: CBT-RB-SELF ( -- ) ;CALLBACK" evaluate-closed
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
   s" CBT-RB-NEXT FFI-CB:UNBIND" evaluate-closed
   DECLARED {: slot:n :}
   CALLER-BOUND$ TEST-EVAL:RC 1 = slot SELF-BINDS? and
      s" a slot a thrown text bound as the caller binds to this context" FACT
   s" CBT-RB-SELF FFI-CB:UNBIND" evaluate-closed
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
   s" CBT-RB-LATE FFI-CB:UNBIND" evaluate-closed
   CMP UNBIND ;

\ A package whose private `@`, `execute` and `CMP-BODY` would run BACKWARD in
\ place of the body declares a callback after them: its dispatch still runs the
\ body stored.
: CAPTURE$ ( -- ^u8 n )
   S\" package CBT-CAPTURE\n: FORWARD ( ^u8 ^u8 -- n ) {: a b :} a CELL-VIEW @ b CELL-VIEW @ - ;\n: BACKWARD ( ^u8 ^u8 -- n ) {: a b :} b CELL-VIEW @ a CELL-VIEW @ - ;\n: @ ( ^[ ^u8 ^u8 -- n ] -- [ ^u8 ^u8 -- n ] ) drop ['] BACKWARD ;\n: execute ( ^u8 ^u8 [ ^u8 ^u8 -- n ] -- n ) drop BACKWARD ;\nTYPED-VARIABLE CMP-BODY [ ^u8 ^u8 -- n ]\n' BACKWARD CMP-BODY !\npublic\nCALLBACK: CMP ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK\n' FORWARD CBT-CAPTURE:CMP-BODY !\n;package\n" ;

: CASE-CAPTURE ( -- )
   s" == a callback declared under a private @, execute and CMP-BODY" SAY
   [: CAPTURE$ evaluate-closed ;] catch 0= {: loaded:bool :}
   loaded s" the declaration loads" FACT
   loaded if
      s" CBT-CAPTURE:CMP TASK:SELF-CONTEXT FFI-CB:ENTRY" TEST-EVAL:N SORTED-BY?
   else false then
      s" C sorts through it by the body stored" FACT
   loaded if s" CBT-CAPTURE:CMP FFI-CB:UNBIND" evaluate-closed then ;

\ A qualified name puts the callback's words into its package's public
\ wordlist, not the current one.
: QUALIFIED$ ( -- ^u8 n )
   S\" package CBT-QUALIFIED\npublic\n: ORDER ( ^u8 ^u8 -- n ) {: a b :} a CELL-VIEW @ b CELL-VIEW @ - ;\n;package\nCALLBACK: CBT-QUALIFIED:CMP ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK\n' CBT-QUALIFIED:ORDER CBT-QUALIFIED:CMP-BODY !\n" ;

: CASE-QUALIFIED ( -- )
   s" == a callback declared by a qualified name" SAY
   [: QUALIFIED$ evaluate-closed ;] catch 0= {: loaded:bool :}
   loaded s" the declaration loads" FACT
   loaded if
      s" CBT-QUALIFIED:CMP TASK:SELF-CONTEXT FFI-CB:ENTRY" TEST-EVAL:N SORTED-BY?
   else false then
      s" C sorts through it by the body stored" FACT
   loaded if s" CBT-QUALIFIED:CMP FFI-CB:UNBIND" evaluate-closed then ;

\ A package with a part named FFI-CB declares a callback: the callback's type
\ FFI-CB:callback is still FFI-CB's.
: PART$ ( -- ^u8 n )
   S\" package CBT-PART\npackage FFI-CB\n: HELD ( -- ) ;\n;package\nCALLBACK: CMP ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK\n;package\n" ;

: CASE-PART ( -- )
   s" == a callback declared beside a part named FFI-CB" SAY
   [: PART$ evaluate-closed ;] catch 0= s" the declaration loads" FACT ;

\ A callback's record notes C's lr as `caller`, which varies with the build,
\ and ends with help the case does not pin: the transcript keeps both keys and
\ drops their values.
: ERR-LINE+ ( ^u8 n -- ) {: a:^ u:n :}
   a u s"   = caller: " STARTS-WITH? if s"   = caller:" SAY exit then
   a u s"   = help: " STARTS-WITH? if s"   = help:" SAY exit then
   a u SAY ;

\ A captured stderr as STREAM+ writes it, each line through ERR-LINE+.
: ERR-STREAM+ ( ^u8 n -- ) {: a:^ u:n :}
   a u LF 0 begin SPLIT-NEXT while {: p:^ pu:n next:n :}
      pu 0 <>  next u > 0=  or if p pu ERR-LINE+ then
      a u LF next
   repeat drop 2drop ;

\ The name, the exit status and both streams of one spawned process.
: TRANSCRIBE-RC ( ^u8 n len len n -- )
   {: name:^ nameu:n outu:len erru:len rc:n :}
   s" == child " SCRIPT+ name nameu SCRIPT+ s" : exit " SCRIPT+ rc SCRIPT-N LF SCRIPT+C
   OUT outu LEN>N STREAM+
   s" --" SAY
   ERR erru LEN>N ERR-STREAM+ ;

: TRANSCRIBE ( ^u8 n len len PROCESS:ending -- )
   PROCESS:ENDING>RC RC>N TRANSCRIBE-RC ;

: CHILD ( ^u8 n -- len len PROCESS:ending ) {: name:^ nameu:n :}
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/ffi-callback-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   name nameu >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: CHILD-CASE ( ^u8 n -- ) {: name:^ nameu:n :}
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

: SUBJECT$ ( -- ^u8 n ) SUBJECT-BUF SUBJECT-U @ ;
: IMAGE$ ( -- ^u8 n ) IMAGE-BUF IMAGE-U @ ;

: WRITE-SUBJECT ( -- )
   SUBJECT$ S\" require src/lib/ffi-callback.f\nCALLBACK: FILE-BOUND ( -- ) ;CALLBACK\n" FS:WRITE-ALL
   \ Numeric fallbacks grow and retire descriptors before capture. MAIN's seed
   \ entry needs a code span disjoint from the shared thunk's.
   SUBJECT$ S\" CALLBACK: FIRST ( -- n ) 456 FALLBACK ;CALLBACK\nCALLBACK: SECOND ( -- n ) 456 FALLBACK ;CALLBACK\n" FS:APPEND-FILE
   \ FILE-BOUND, the first row, is bound while the build loads; the capture
   \ drops the binding.
   SUBJECT$ S\" FILE-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop\n" FS:APPEND-FILE
   SUBJECT$ S\" require test/stripped-callback-subject.f\n: MAIN ( -- ) FFI:CALLBACK-ENTRY drop STRIPPED-CALLBACK-SUBJECT:RUN ;\n" FS:APPEND-FILE ;

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
   s" == declarations rejected" W
   s" ok   an argument type outside n i32 u32 r ^u8: E-FFI-SYNTAX" W
   s" ok   a pointer spelled ptr, not ^u8: E-FFI-SYNTAX" W
   s" ok   a pointer that is not ^u8: E-FFI-SYNTAX" W
   s" ok   two results: E-FFI-SYNTAX" W
   s" ok   a pointer result: E-FFI-SYNTAX" W
   s" ok   a value result with no fallback: E-FFI-SYNTAX" W
   s" ok   FFALLBACK for an integer result: E-FFI-SYNTAX" W
   s" ok   FALLBACK with no result: E-FFI-SYNTAX" W
   s" ok   an integer argument past the registers: E-FFI-ARITY" W
   s" ok   a ninth float argument: E-FFI-ARITY" W
   s" ok   ;CALLBACK with nothing open: E-FFI-SYNTAX" W
   s" ok   FALLBACK with nothing open: E-FFI-SYNTAX" W
   s" ok   CALLBACK: inside an open declaration: E-UNCLOSED" W
   s" ok   the seventeenth declaration succeeds" W
   s" ok   a name with two ':': E-FFI-SYNTAX" W
   s" ok   a name ending in ':': E-FFI-SYNTAX" W
   s" ok   a name starting with ':': E-FFI-SYNTAX" W
   s" ok   a 100-byte name is kept whole" W ;

: WANT-ROLLBACK ( -- )
   s" == a callback declared in a text that throws" W
   s" ok   the rollback leaves the slot count as it was" W
   s" ok   the next declaration takes the slot a thrown text bound, unbound" W
   s" ok   a slot a thrown text bound as the caller binds to this context" W
   s" ok   a throw after a declaration that grew the tables keeps the slot count" W
   s" ok   storage laid over the cut, a callback bound before it sorts" W
   s" ok   a callback declared after the rollback sorts" W
   s" ok   and the earlier one still does" W ;

\ The end of a callback record as ERR-LINE+ transcribes it, its blank line last.
: WANT-RECORD-END ( -- )
   s"   = caller:" W  s"   = help:" W  s" " W ;

: WANT-CAPTURE ( -- )
   s" == a callback declared under a private @, execute and CMP-BODY" W
   s" ok   the declaration loads" W
   s" ok   C sorts through it by the body stored" W ;

: WANT-QUALIFIED ( -- )
   s" == a callback declared by a qualified name" W
   s" ok   the declaration loads" W
   s" ok   C sorts through it by the body stored" W ;

: WANT-PART ( -- )
   s" == a callback declared beside a part named FFI-CB" W
   s" ok   the declaration loads" W ;

: WANT-CHILDREN ( -- )
   s" == child outside: exit 106" W
   s" outside: a thread is parked on its own context" W
   s" --" W
   s" error[E-CALLBACK]: C called the callback outside the foreign call that passed it" W
   s"   = slot: 0" W  WANT-RECORD-END
   s" == child sleeping: exit 106" W
   s" sleeping: a thread is parked on its own context" W
   s" --" W
   s" error[E-CALLBACK]: C called the callback on one thread while another thread runs in its context" W
   s"   = slot: 0" W  WANT-RECORD-END
   s" == child unbound: exit 106" W
   s" unbound: sorted while bound" W
   s" --" W
   s" error[E-CALLBACK]: C called a callback whose slot is not bound" W
   s"   = slot: 0" W  WANT-RECORD-END
   s" == child worker-stale: exit 106" W
   s" worker-stale: the worker sorted and ended bound" W
   s" --" W
   s" error[E-CALLBACK]: C called a callback whose slot is not bound" W
   s"   = slot: 0" W  WANT-RECORD-END
   s" == child body-defines: exit 106" W
   s" --" W
   s" error[E-CALLBACK]: the callback's body defined a word" W
   s"   = slot: 11" W  WANT-RECORD-END
   HB-TARGET-LINUX-X86-64? 0= if
      s" == child caller-thread: exit 106" W
      s" caller-thread: the slot is bound to the caller's context" W
      s" --" W
      s" error[E-CALLBACK]: C called a callback bound to the caller's context on a thread with no region of its own" W
      s"   = slot: 5" W  WANT-RECORD-END
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
   WANT-TASKS WANT-ACCESSOR WANT-DECLARATIONS WANT-ROLLBACK WANT-CAPTURE
   WANT-QUALIFIED WANT-PART WANT-CHILDREN ;

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
   CASE-CAPTURE
   CASE-QUALIFIED
   CASE-PART
   CASE-CHILDREN
   CASE-STRIPPED
   TRANSCRIPT-CASE
   T-REPORT ;

;using
;package

FFI-CB-TEST:RUN-SUITE
