\ ffi-callback-test.f - C calling back into checked Habu: qsort, bsearch and
\ pthread_create are the C callers. Each case's facts, with every child's and
\ the stripped image's exit status and streams, form a transcript written to
\ build/ffi-callback-transcript.txt, read back and compared whole. A callback
\ body that defines ends the process; that is a case of its own.
\ Run: bin/hb --load src/lib/ffi-callback-test.f
require src/lib/test/assert.f
require src/lib/test/eval.f
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
require tools/check-verify-core.f       \ LSP-CHECK:VERIFY-BYTES - the declaration's check
require src/lib/ffi-callback.f
require src/lib/ffi-abi.f

package FFI-CB-TEST

$3000 constant SCRIPT-CAP
$4000 constant CAP
1000 constant THREAD-READS
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
variable MAIN-THREAD                  \ the thread WHO runs on for the main task

\ With the fixture's eleven callbacks these five make sixteen entries, a
\ table-size edge.
CALLBACK: PAD-A ( -- ) ;CALLBACK
CALLBACK: PAD-B ( -- ) ;CALLBACK
CALLBACK: PAD-C ( -- ) ;CALLBACK
CALLBACK: PAD-D ( -- ) ;CALLBACK
CALLBACK: PAD-E ( -- ) ;CALLBACK

: SCRIPT$ ( -- str )
   SCRIPT-BUF SCRIPT-U @ >str ;

: SCRIPT+ ( str -- ) {: s:str :}
   SCRIPT-U @ s STR:LEN + SCRIPT-CAP > if E-STR-CAPACITY throw then
   s STR:DATA SCRIPT-BUF SCRIPT-U @ + s STR:LEN BYTE-COPY
   s STR:LEN SCRIPT-U +! ;

: SCRIPT+C ( n -- ) {: c:n :}
   SCRIPT-U @ SCRIPT-CAP >= if E-STR-CAPACITY throw then
   c SCRIPT-BUF SCRIPT-U @ + c!
   1 SCRIPT-U +! ;

: SCRIPT-N ( n -- )
   SB-RESET FMT:SB-INT SB$ SCRIPT+ ;

: SAY ( str -- )
   SCRIPT+ LF SCRIPT+C ;

\ A captured stream, ended by a line feed unless it is empty or ends in one.
: STREAM+ ( str -- ) {: s:str :}
   s STR:LEN 0= if exit then
   s SCRIPT+
   s STR:DATA s STR:LEN + 1 - c@ LF <> if LF SCRIPT+C then ;

\ One fact: asserted under its own label and written as a transcript line.
: FACT ( bool str -- ) {: ok:bool s:str :}
   s T-LABEL
   ok TTRUE
   ok if s" ok   " else s" FAIL " then SCRIPT+
   s SAY ;

: WANT$ ( -- str )
   WANT-BUF WANT-U @ >str ;

: W ( str -- ) {: s:str :}
   WANT-U @ s STR:LEN + 1 + SCRIPT-CAP > if E-STR-CAPACITY throw then
   s STR:DATA WANT-BUF WANT-U @ + s STR:LEN BYTE-COPY
   LF WANT-BUF WANT-U @ + s STR:LEN + c!
   s STR:LEN 1 + WANT-U +! ;

: SORT-FACTS ( n -- ) {: mask:n :}
   mask 1 and 0 <> s" qsort sorts through a callback; a stack cell, a local and a loop index survive" FACT
   mask 2 and 0 <> s" a comparator nests qsort through its own slot" FACT
   mask 4 and 0 <> s" a comparator nests qsort through a second slot on the context" FACT
   mask 8 and 0 <> s" a throwing comparator answers its fallback; FAULT@ keeps the code; depth is intact" FACT
   mask $10 and 0 <> s" UNBIND inside the comparator is E-TASK-STATE and qsort completes" FACT ;

: CASE-MAIN ( -- )
   s" == the main task" SAY
   BIND-SELF SORTS SORT-FACTS ;

\ WHO is a comparator that keeps pthread_self. qsort of a two-cell row calls it
\ once, so one value across a thousand sorts with a yield between them is the
\ main task's callbacks holding to one thread. The cell is cleared first, so a
\ sort that never called WHO reads 0.
: WHO-SORT ( n -- n ) {: fn:n :}
   0 WHO-THREAD !
   OUTER BYTE-VIEW 2 CELL fn QSORT
   WHO-THREAD @ ;

: THREAD-STABLE? ( n -- bool ) {: fn:n :}
   fn WHO-SORT MAIN-THREAD !
   true THREAD-READS 0 ?do
      TASK:PAUSE
      fn WHO-SORT MAIN-THREAD @ = and
   loop ;

: CASE-THREAD ( -- )
   s" == the thread a callback runs on" SAY
   WHO TASK:SELF-CONTEXT FFI-CB:ENTRY THREAD-STABLE?
      s" the main task's callbacks run on one thread, a thousand calls and yields apart" FACT
   MAIN-THREAD @ 0 <> s" which every sort called" FACT
   WHO FFI-CB:UNBIND ;

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

: CASE-CALLER ( -- )
   s" == one slot on the caller's context, two tasks inside at once" SAY
   CMP-CALLER TASK:CALLER-CONTEXT FFI-CB:ENTRY CMP-CALLER-FN !
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
   CMP-CALLER FFI-CB:UNBIND
   [: CMP-CALLER TASK:SELF-CONTEXT FFI-CB:ENTRY drop ;] catch 0=
      s" UNBIND with no thread inside frees it: the slot binds to the main context" FACT
   CMP-CALLER FFI-CB:UNBIND ;

\ Slot k's cell at offset off of its row, reached as the thunk reaches it:
\ through the table the main region's CB-ROWS publishes.
: SLOT-CELL ( n n -- ^n ) {: k:n off:n :}
   TASK:MAIN-BASE {: base :}
   base CB-ROWS + atomic@ base FFI:>CELL - {: table:n :}
   base table + k CB-ROW-BYTES * + off + ;

\ CMP-CALLER's slot: the one idle caller row while it is bound; -1 if none is.
: CALLER-SLOT ( -- n )
   CMP-CALLER TASK:CALLER-CONTEXT FFI-CB:ENTRY drop
   -1 CB-POOL 0 ?do
      i CB-ROW-OWNER SLOT-CELL atomic@ CB-ROW-CALLER = if drop i then
   loop
   CMP-CALLER FFI-CB:UNBIND ;

: WORK-CALLER-BIND ( -- )
   CMP-CALLER TASK:CALLER-CONTEXT FFI-CB:ENTRY drop 1 TASK:RETURN ;

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
   CMP-CALLER FFI-CB:UNBIND ;


false [if] \ off until dot habu-add-the-checks-a49d054d
: CASE-ACCESSOR ( -- )
   s" == a frame accessor" SAY
   [: 0 ARG@ drop ;] catch E-FFI-CALLBACK-STATE =
      s" a frame accessor outside a callback is E-FFI-CALLBACK-STATE" FACT ;
[then]

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
: REJECTS? ( [ -- ] str -- bool ) {: q c:str :}
   REJECTION:CAPTURE
   q catch 70 =
   REJECTION:BUFFER$ c STR:CONTAINS? and
   REJECTION:BUFFER-OFF ;

\ Whether Q stops with the rejection status after a record holding HEAD and HELP.
: RECORD? ( [ -- ] str str -- bool )
   {: q h:str p:str :}
   DECL-DIAG:PROSE  q catch 70 =
   h DECL-DIAG:HAS? and  p DECL-DIAG:HAS? and
   DECL-DIAG:OFF ;

: INT-ARITY$ ( -- str )
   HB-TARGET-LINUX-X86-64? if
      s" error[E-FFI-ARITY]: a callback takes at most 6 integer arguments"
   else
      s" error[E-FFI-ARITY]: a callback takes at most 8 integer arguments"
   then ;

: ARITY-HELP$ ( -- str )
   s" = help: pass the rest in memory through a ^u8 argument" ;

: CASE-DECLARATIONS ( -- )
   s" == declarations rejected" SAY
   [: DECL-BAD-TOKEN ;]
      s" error[E-FFI-SYNTAX]: unknown argument type 'nosuch'"
      s" = help: an argument is n, i32, u32, r, str or ^u8" RECORD?
      s" an argument type outside n i32 u32 r str ^u8: E-FFI-SYNTAX" FACT
   [: DECL-PTR-WORD ;]
      s" error[E-FFI-SYNTAX]: unknown argument type 'ptr'"
      s" = help: an argument is n, i32, u32, r, str or ^u8" RECORD?
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
      s" error[E-UNCLOSED]: `CALLBACK:` is still open at `CALLBACK:`"
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

: PAD-PAST-EDGE ( -- ) begin FFI-CB:DECLARED POW2? while PAD repeat ;

: THROWING$ ( -- str )
   s" CALLBACK: CBT-RB-IN ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK  1 throw" ;

\ Non-zero cells laid past the thrown text, where a grown table its storage did
\ not keep would lie.
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

: BOUND$ ( -- str )
   s" CALLBACK: CBT-RB-BOUND ( -- ) ;CALLBACK  CBT-RB-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop  1 throw" ;

\ CBT-RB-NEXT takes slot n and binds as the caller, then is unbound.
: NEXT-BINDS? ( n -- bool ) {: k:n :}
   s" CALLBACK: CBT-RB-NEXT ( -- ) ;CALLBACK" evaluate-closed
   FFI-CB:DECLARED k 1 + =
   s" CBT-RB-NEXT TASK:CALLER-CONTEXT FFI-CB:ENTRY drop" TEST-EVAL:RC 0= and
   s" CBT-RB-NEXT FFI-CB:UNBIND" evaluate-closed ;

\ A caller bind leaves the row owner set; a bind to this context rejects it.
: CALLER-BOUND$ ( -- str )
   s" CALLBACK: CBT-RB-CALLER ( -- ) ;CALLBACK  CBT-RB-CALLER TASK:CALLER-CONTEXT FFI-CB:ENTRY drop  1 throw" ;

\ CBT-RB-SELF takes slot n and binds to this context, then is unbound.
: SELF-BINDS? ( n -- bool ) {: k:n :}
   s" CALLBACK: CBT-RB-SELF ( -- ) ;CALLBACK" evaluate-closed
   FFI-CB:DECLARED k 1 + =
   s" CBT-RB-SELF TASK:SELF-CONTEXT FFI-CB:ENTRY drop" TEST-EVAL:RC 0= and
   s" CBT-RB-SELF FFI-CB:UNBIND" evaluate-closed ;

\ Slot k rejects a bind to ctx while the binding a thrown text made holds it,
\ then is unbound.
: HELD? ( n n -- bool ) {: k:n ctx:n :}
   k FFI-CB:DESCRIPTOR {: cb :}
   [: cb ctx FFI-CB:ENTRY drop ;] catch E-TASK-STATE =
   cb FFI-CB:UNBIND ;

\ The THROWING$ text at the edge grows the descriptor and task-row tables; C
\ then calls through the grown tables the main region publishes.
: CASE-THROWN ( -- )
   s" == a callback declared in a text that throws" SAY
   PAD-PAST-EDGE FFI-CB:DECLARED {: before:n :}
   THROWING$ TEST-EVAL:RC 1 = FFI-CB:DECLARED before 1 + = and
      s" a thrown text keeps the slot it declared" FACT
   BOUND$ TEST-EVAL:RC 1 = FFI-CB:DECLARED before 2 + = and {: bound:bool :}
   bound if
      before 1 + TASK:CALLER-CONTEXT HELD? before 2 + NEXT-BINDS? and
   else false then
      s" a slot a thrown text bound stays bound; the next declaration takes a new slot" FACT
   FFI-CB:DECLARED {: slot:n :}
   CALLER-BOUND$ TEST-EVAL:RC 1 = FFI-CB:DECLARED slot 1 + = and {: caller:bool :}
   caller if
      slot TASK:SELF-CONTEXT HELD? slot 1 + SELF-BINDS? and
   else false then
      s" a slot a thrown text bound as the caller stays bound as the caller" FACT
   PAD-TO-EDGE FFI-CB:DECLARED {: edge:n :}
   CMP TASK:SELF-CONTEXT FFI-CB:ENTRY {: early:n :}
   THROWING$ TEST-EVAL:RC 1 = FFI-CB:DECLARED edge 1 + = and
      s" a throw after a declaration that grew the tables keeps its slot" FACT
   JUNK
   early SORTED-BY?
      s" storage laid past the thrown text, a callback bound before it sorts" FACT
   DECLARE-LATE
   s" CBT-RB-LATE TASK:SELF-CONTEXT FFI-CB:ENTRY" TEST-EVAL:N SORTED-BY?
      s" a callback declared after the thrown text sorts" FACT
   early SORTED-BY? s" and the earlier one still does" FACT
   s" CBT-RB-LATE FFI-CB:UNBIND" evaluate-closed
   CMP FFI-CB:UNBIND ;

\ A package whose private `@`, `execute` and `CMP-BODY` would run BACKWARD in
\ place of the body declares a callback after them: its dispatch still runs the
\ body stored.
: CAPTURE$ ( -- str )
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
: QUALIFIED$ ( -- str )
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
: PART$ ( -- str )
   S\" package CBT-PART\npackage FFI-CB\n: HELD ( -- ) ;\n;package\nCALLBACK: CMP ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK\n;package\n" ;

: CASE-PART ( -- )
   s" == a callback declared beside a part named FFI-CB" SAY
   [: PART$ evaluate-closed ;] catch 0= s" the declaration loads" FACT ;

\ A body that defines a word renders, so the body cell rejects it.
: DEFINES$ ( -- str )
   S\" : CBT-DEF-ORDER ( ^u8 ^u8 -- n ) 2drop s\" variable CBT-INSIDE\" evaluate-closed 0 ;\nCALLBACK: CBT-DEF-CMP ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK\n' CBT-DEF-ORDER CBT-DEF-CMP-BODY !\n" ;

: CASE-BODY-DEFINES ( -- )
   s" == a callback body that defines a word" SAY
   [: DEFINES$ evaluate-closed ;]
      s" error[E-MISMATCH]: this quotation does more than the type that keeps it states"
      s" = help: state `renders` in the type that keeps it" RECORD?
      s" it is rejected where the body cell keeps it" FACT ;

\ A callback's record notes C's lr as `caller`, which varies with the build,
\ and ends with help the case does not pin: the transcript keeps both keys and
\ drops their values.
: ERR-LINE+ ( str -- ) {: s:str :}
   s s"   = caller: " STR:STARTS-WITH? if s"   = caller:" SAY exit then
   s s"   = help: " STR:STARTS-WITH? if s"   = help:" SAY exit then
   s SAY ;

\ A captured stderr as STREAM+ writes it, each line through ERR-LINE+.
: ERR-STREAM+ ( str -- ) {: s:str :}
   s LF 0 begin STR:SPLIT-NEXT while {: p:str next:n :}
      p STR:LEN 0 <>  next s STR:LEN > 0=  or if p ERR-LINE+ then
      s LF next
   repeat 2drop ;

\ The name, the exit status and both streams of one spawned process.
: TRANSCRIBE-RC ( str len len n -- )
   {: name:str outu:len erru:len rc:n :}
   s" == child " SCRIPT+ name SCRIPT+ s" : exit " SCRIPT+ rc SCRIPT-N LF SCRIPT+C
   OUT outu LEN>N >str STREAM+
   s" --" SAY
   ERR erru LEN>N >str ERR-STREAM+ ;

: TRANSCRIBE ( str len len PROCESS:ending -- )
   PROCESS:ENDING>RC RC>N TRANSCRIBE-RC ;

: CHILD ( str -- len len PROCESS:ending ) {: name:str :}
   PROC-ARGV-ENV-RESET
   s" --load" PROC-ARGV+
   s" test/ffi-callback-child.f" PROC-ARGV+
   s" --" PROC-ARGV+
   name PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: CHILD-CASE ( str -- ) {: name:str :}
   name name CHILD TRANSCRIBE ;

: CASE-CHILDREN ( -- )
   s" outside" CHILD-CASE
   s" sleeping" CHILD-CASE
   s" unbound" CHILD-CASE
   s" worker-stale" CHILD-CASE
   s" caller-thread" CHILD-CASE
   s" released" CHILD-CASE
   s" grown-bind" CHILD-CASE ;

\ Every comparison answers the declared fallback, so bsearch matches its first
\ probe under UNSET's 0 and no row under UNSET-ONE's 1.
: CASE-UNSET ( -- )
   s" == a body never stored" SAY
   UNSET TASK:SELF-CONTEXT FFI-CB:ENTRY {: fn:n :}
   OUTER BYTE-VIEW OUTER BYTE-VIEW ROW-N CELL fn BSEARCH
   OUTER NULL-PTR - ROW-N 2 / cells + =
      s" bsearch through it answers the fallback: its first probe matches" FACT
   UNSET FFI-CB:FAULT@ E-FFI-CALLBACK-STATE = s" and faults E-FFI-CALLBACK-STATE" FACT
   UNSET FFI-CB:UNBIND
   OUTER BYTE-VIEW OUTER BYTE-VIEW ROW-N CELL UNSET-ONE TASK:SELF-CONTEXT FFI-CB:ENTRY
   BSEARCH 0= s" a fallback of 1 reaches C too: no row matches" FACT
   UNSET-ONE FFI-CB:UNBIND ;

: SUBJECT$ ( -- str ) SUBJECT-BUF SUBJECT-U @ >str ;
: IMAGE$ ( -- str ) IMAGE-BUF IMAGE-U @ >str ;

: WRITE-SUBJECT ( -- )
   SUBJECT$ S\" require src/lib/ffi-callback.f\nrequire src/lib/ffi-abi.f\nrequire src/lib/task.f\nCALLBACK: FILE-BOUND ( -- ) ;CALLBACK\n" FS:WRITE-ALL
   \ Numeric fallbacks grow and retire descriptors before capture. MAIN's seed
   \ entry needs a code span disjoint from the shared thunk's.
   SUBJECT$ S\" CALLBACK: FIRST ( -- n ) 456 FALLBACK ;CALLBACK\nCALLBACK: SECOND ( -- n ) 456 FALLBACK ;CALLBACK\n" FS:APPEND-FILE
   \ FILE-BOUND, the first row, is bound while the build loads; the capture
   \ drops the binding.
   SUBJECT$ S\" FILE-BOUND TASK:SELF-CONTEXT FFI-CB:ENTRY drop\n" FS:APPEND-FILE
   SUBJECT$ S\" require test/stripped-callback-subject.f\n: MAIN ( -- ) FFI:CALLBACK-ENTRY drop STRIPPED-CALLBACK-SUBJECT:RUN ;\n" FS:APPEND-FILE ;

: BUILD-IMAGE ( -- len len PROCESS:ending )
   PROC-ARGV-ENV-RESET
   s" --load" PROC-ARGV+
   s" tools/hb-build.f" PROC-ARGV+
   s" --" PROC-ARGV+
   SUBJECT$ PROC-ARGV+
   s" -o" PROC-ARGV+
   IMAGE$ PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: RUN-IMAGE ( -- len len PROCESS:ending )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: LINKED ( len len PROCESS:ending -- )
   PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if OUT outu LEN>N >str type ERR erru LEN>N >str type then
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

\ The check reads a declaration, one beside a part named FFI-CB too, and its
\ words, so it goes on past their uses and rejects the definition after them.
: VERIFY$ ( -- str )
   S\" require src/lib/ffi-callback.f\n: CBV-STEP ( n -- n ) 1 + ;\nCALLBACK: CBV ( n -- n ) 0 FALLBACK ;CALLBACK\n: CBV-SET ( -- ) ['] CBV-STEP CBV-BODY ! ;\n: CBV-USE ( -- FFI-CB:callback ) CBV ;\npackage CBV-PART\npackage FFI-CB\n: HELD ( -- ) ;\n;package\nCALLBACK: CMP ( ^u8 ^u8 -- i32 ) 0 FALLBACK ;CALLBACK\n: CMP-USE ( -- ) CMP drop ;\n;package\n: CBV-BAD ( -- n ) 1 2 ;\n" ;

: REJECTED? ( LSP-CHECK:verdict -- bool )
   MATCH LSP-CHECK:verdict
      verified OF false ENDOF
      rejected OF true ENDOF
      engine-provided OF false ENDOF
      held OF false ENDOF
      incomplete OF {: status :} false ENDOF
      deferred OF false ENDOF
   ;MATCH ;

: VERIFY-BODY ( -- )
   s" ffi-callback-verify" GT-START
   s" subject.f" SUBJECT-BUF GT-PATH SUBJECT-U !
   VERIFY$ SUBJECT$ [: drop ;] PROCESS:WAIT-FOREVER true LSP-CHECK:VERIFY-BYTES
   REJECTED? {: rejected:bool :}
   LSP-CHECK:VERIFY-OUT$ {: out:str :}
   rejected  out s" E-MISMATCH" STR:CONTAINS? and
   out s" CBV-BAD" STR:CONTAINS? and
      s" it rejects the definition after the declaration: E-MISMATCH" FACT
   \ A wrong type on CBV or CBV-BODY, or a deferral, adds a record with its code.
   out S\" \"code\":" STR:COUNT-SUB 1 =
      s" it reports nothing else: it finds and types the declaration's words" FACT ;

: CASE-VERIFY ( -- )
   s" == the check of a callback declaration" SAY
   [: VERIFY-BODY ;] [: GT-CLEANUP ;] finally ;

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
   s" == the thread a callback runs on" W
   s" ok   the main task's callbacks run on one thread, a thousand calls and yields apart" W
   s" ok   which every sort called" W
   s" == a worker on its own context, unbinding before it ends" W
   WANT-SORTS
   s" == a worker on its own context, ending bound" W
   WANT-SORTS
   s" ok   its rows went with it: the same slots bind to the main context" W
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
   s" ok   and the row is a caller row" W ;

false [if] \ off until dot habu-add-the-checks-a49d054d
: WANT-ACCESSOR ( -- )
   s" == a frame accessor" W
   s" ok   a frame accessor outside a callback is E-FFI-CALLBACK-STATE" W ;
[then]

: WANT-DECLARATIONS ( -- )
   s" == declarations rejected" W
   s" ok   an argument type outside n i32 u32 r str ^u8: E-FFI-SYNTAX" W
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

: WANT-THROWN ( -- )
   s" == a callback declared in a text that throws" W
   s" ok   a thrown text keeps the slot it declared" W
   s" ok   a slot a thrown text bound stays bound; the next declaration takes a new slot" W
   s" ok   a slot a thrown text bound as the caller stays bound as the caller" W
   s" ok   a throw after a declaration that grew the tables keeps its slot" W
   s" ok   storage laid past the thrown text, a callback bound before it sorts" W
   s" ok   a callback declared after the thrown text sorts" W
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

: WANT-BODY-DEFINES ( -- )
   s" == a callback body that defines a word" W
   s" ok   it is rejected where the body cell keeps it" W ;

: WANT-VERIFY ( -- )
   s" == the check of a callback declaration" W
   s" ok   it rejects the definition after the declaration: E-MISMATCH" W
   s" ok   it reports nothing else: it finds and types the declaration's words" W ;

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
   s" == child caller-thread: exit 106" W
   s" caller-thread: the slot is bound to the caller's context" W
   s" --" W
   s" error[E-CALLBACK]: C called a callback bound to the caller's context on a thread with no region of its own" W
   s"   = slot: 5" W  WANT-RECORD-END
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
   WANT-TASKS WANT-DECLARATIONS WANT-THROWN WANT-CAPTURE
   WANT-QUALIFIED WANT-PART WANT-BODY-DEFINES WANT-VERIFY WANT-CHILDREN ;

\ The artifact as another reader will find it: read back from its file and
\ compared whole.
: TRANSCRIPT-CASE ( -- )
   s" build" FS:MAKE-DIRS
   s" build/ffi-callback-transcript.txt" SCRIPT$ FS:WRITE-ALL
   s" build/ffi-callback-transcript.txt" BACK-BUF SCRIPT-CAP FS:READ-ALL {: got:n :}
   WANT-TRANSCRIPT
   s" build/ffi-callback-transcript.txt is the transcript this file expects" T-LABEL
   BACK-BUF got >str WANT$ T$= ;

public

: RUN-SUITE ( -- )
   T-RESET
   0 SCRIPT-U !
   CASE-MAIN
   CASE-UNSET
   CASE-THREAD
   CASE-WORKERS
   CASE-CALLER
   CASE-CALLER-HELD
   CASE-DECLARATIONS
   CASE-THROWN
   CASE-CAPTURE
   CASE-QUALIFIED
   CASE-PART
   CASE-BODY-DEFINES
   CASE-VERIFY
   CASE-CHILDREN
   CASE-STRIPPED
   TRANSCRIPT-CASE
   T-REPORT ;

;package

FFI-CB-TEST:RUN-SUITE
