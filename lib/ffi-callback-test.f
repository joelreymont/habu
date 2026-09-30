\ ffi-callback-test.f - C calling back into checked Habu, through libc itself.
\
\ qsort and pthread_create are the C callers. Each case's facts go into a
\ transcript, one line each, beside the exit status and both streams of every
\ child scenario (test/ffi-callback-child.f) and of the stripped image
\ built from test/stripped-callback-subject.f. The transcript is written to
\ build/ffi-callback-transcript.txt, read back and compared whole with the one
\ this file expects.
\
\ Every word is defined before the first task starts, because Habu forbids
\ dictionary mutation while a task is live; the one definition made later is a
\ case of its own.
\ Run: bin/hb --load lib/ffi-callback-test.f
require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/test/runner.f
require lib/image-lifecycle.f
require lib/task.f
require test/ffi-callback-fixture.f

package FFI-CB-TEST
using FFI-CB

$3000 constant SCRIPT-CAP
$4000 constant CAP
60000 constant CHILD-MS               \ includes compiling the fixture in each child
600000 constant BUILD-MS
1000 constant OWNER-READS
2000 constant HAMMER-N                \ refused UNEXPOSEs the thread sorts under
200 constant HAMMER-SORTS             \ and the sorts it has to finish meanwhile
200 constant HELD-MS                  \ how long a held row keeps a call waiting
10 constant LF

create SCRIPT-BUF SCRIPT-CAP allot    \ the transcript this run captured
create WANT-BUF SCRIPT-CAP allot      \ the transcript this file expects
create BACK-BUF SCRIPT-CAP allot      \ the transcript read back from its file
create OUT CAP allot
create ERR CAP allot
create SUBJECT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
create WANT-FLOATS                    \ 1.0 .. 8.0 as IEEE 754 doubles
   $3FF0000000000000 , $4000000000000000 , $4008000000000000 , $4010000000000000 ,
   $4014000000000000 , $4018000000000000 , $401C000000000000 , $4020000000000000 ,

variable SCRIPT-U
variable WANT-U
variable SUBJECT-U
variable IMAGE-U
variable MAIN-OWNER                   \ the owner the main thread claims with
variable MAIN-SELF                    \ its pthread_self
variable OLD-CONTEXT                  \ CTX-TASK's context, kept past its UNEXPOSE
variable HAMMERS
TYPED-VARIABLE MARSHAL-RESULT r

\ The fixture's ten callbacks and these six fill the engine's sixteen stubs, so
\ the next declaration is the seventeenth.
CALLBACK: PAD-A ( -- ) ;CALLBACK
CALLBACK: PAD-B ( -- ) ;CALLBACK
CALLBACK: PAD-C ( -- ) ;CALLBACK
CALLBACK: PAD-D ( -- ) ;CALLBACK
CALLBACK: PAD-E ( -- ) ;CALLBACK
CALLBACK: PAD-F ( -- ) ;CALLBACK

\ ---- the transcript -----------------------------------------------------------
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

\ ---- the calling task ---------------------------------------------------------
: SORT-FACTS ( n -- ) {: mask:n :}
   mask 1 and 0 <> s" qsort sorts through a callback; a stack cell, a local and a loop index survive" FACT
   mask 2 and 0 <> s" a comparator nests qsort through its own slot" FACT
   mask 4 and 0 <> s" a comparator nests qsort through a second slot on the context" FACT
   mask 8 and 0 <> s" a throwing comparator answers its fallback; FAULT@ keeps the code; depth is intact" FACT
   mask $10 and 0 <> s" UNBIND inside the comparator is E-TASK-STATE and qsort completes" FACT ;

: CASE-MAIN ( -- )
   s" == the main task" SAY
   BIND-SELF SORTS SORT-FACTS ;

\ ---- C calling a stub directly --------------------------------------------------
: FLOAT-BITS ( ptr r -- n )
   BYTE-VIEW CELL-VIEW @ ;

: FLOATS-SEEN? ( -- bool )
   true 8 0 ?do i SEEN-FLOATS FLOAT-BITS WANT-FLOATS i cells + @ = and loop ;

: INTS-SEEN? ( -- bool )
   4 SEEN-INTS @ 40 =
   5 SEEN-INTS @ 50 = and
   6 SEEN-INTS @ 60 = and
   7 SEEN-INTS @ 70 = and ;

: CASE-DIRECT ( -- )
   s" == every argument register" SAY
   MARSHAL TASK:SELF-CONTEXT ENTRY MARSHAL-CALL MARSHAL-RESULT !
   0 SEEN-INTS @ $123456789ABCDEF0 = s" n arrives whole" FACT
   1 SEEN-INTS @ -2 = s" i32 is sign-extended from the low half" FACT
   2 SEEN-INTS @ $FFFFFFFD = s" u32 is the low half" FACT
   3 SEEN-INTS @ $5A = s" ptr u8 is the address C passed" FACT
   INTS-SEEN? s" x4..x7 arrive in order" FACT
   FLOATS-SEEN? s" d0..d7 arrive in order" FACT
   MARSHAL-RESULT FLOAT-BITS $4020000000000000 = s" an r result returns in d0" FACT
   MARSHAL FAULT@ 0= s" and nothing faulted" FACT
   MARSHAL UNBIND
   9 UNSET TASK:SELF-CONTEXT ENTRY CALL1 7 = s" a body never stored answers the fallback" FACT
   UNSET FAULT@ E-FFI-CALLBACK-STATE = s" and faults E-FFI-CALLBACK-STATE" FACT
   UNSET UNBIND ;

\ ---- the owner a thread claims with --------------------------------------------
\ WHO answers CB-OWNER of the region it runs on. Every call here claims it
\ afresh, so one value across a thousand calls with a yield between them is the
\ thread pointer holding still; the foreign thread's half is CASE-THREAD's.
: OWNER-STABLE? ( n -- bool ) {: fn:n :}
   0 fn CALL1 MAIN-OWNER !
   true OWNER-READS 0 ?do
      TASK:PAUSE
      0 fn CALL1 MAIN-OWNER @ = and
   loop ;

: CASE-OWNER ( -- )
   s" == the owner a thread claims with" SAY
   WHO TASK:SELF-CONTEXT ENTRY OWNER-STABLE?
      s" the main thread claims with one value, a thousand calls and yields apart" FACT
   MAIN-OWNER @ 0 <> s" which is not the free owner" FACT
   PTHREAD-SELF MAIN-SELF !
   WHO UNBIND ;

: OWNER-IDENTITY ( -- )
   THREAD-OWNER @ MAIN-OWNER @ <> s" a foreign thread claims with another value" FACT
   THREAD-OWNER @ THREAD-SELF @ - MAIN-OWNER @ MAIN-SELF @ - =
      s" each at the same offset from its own pthread_self" FACT
   THREAD-SELF @ THREAD @ = s" and pthread_self there is the pthread_t pthread_create stored" FACT ;

\ ---- workers --------------------------------------------------------------------
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

\ ---- a foreign thread on an exposed task ---------------------------------------
: IDLE-BODY ( -- ) ;

: ACTIVATE-CTX ( -- )
   ['] IDLE-BODY CTX-TASK TASK:ACTIVATE ;

\ UNEXPOSE asked again and again while the thread inside keeps nesting through
\ CMP, a lower slot naming the same context. Each refusal claims CMP's row
\ before it meets START's and hands it back, and a comparison that meets the
\ claim waits for the hand-back, so every call completes: answers the count of
\ attempts that were not E-TASK-STATE.
: HAMMER ( -- n )
   0 HAMMERS !
   0
   begin
      [: CTX-TASK TASK:UNEXPOSE ;] catch E-TASK-STATE <> if 1 + then
      1 HAMMERS +!
      HAMMERS @ HAMMER-N >= THREAD-SORTS atomic@ HAMMER-SORTS >= and
   until ;

: CASE-THREAD ( -- )
   s" == a foreign thread on an exposed task" SAY
   CMP-VIA TASK:SELF-CONTEXT ENTRY drop
   THREAD-BIND
   [: CMP-VIA CTX-TASK TASK:CONTEXT ENTRY drop ;] catch E-TASK-STATE =
      s" ENTRY of a slot bound to another context is E-TASK-STATE" FACT
   CMP-VIA UNBIND
   0 BAD !
   START-FN @ 5 THREAD-START 0= s" pthread_create takes the entry as its start routine" FACT
   WAIT-INSIDE
   [: CTX-TASK TASK:UNEXPOSE ;] catch E-TASK-STATE =
      s" UNEXPOSE with the thread parked inside is E-TASK-STATE" FACT
   [: CTX-TASK TASK:KILL ;] catch E-TASK-STATE =
      s" KILL with the thread parked inside is E-TASK-STATE" FACT
   [: ACTIVATE-CTX ;] catch E-TASK-STATE = s" ACTIVATE of an exposed task is E-TASK-STATE" FACT
   [: START UNBIND ;] catch E-TASK-STATE = s" UNBIND of the slot the thread holds is E-TASK-STATE" FACT
   1 HOLD atomic!
   1 GATE atomic!
   HAMMER 0= s" UNEXPOSE stays refused while the thread nests through a second slot, and harms no call" FACT
   0 HOLD atomic!
   THREAD-JOIN 0= s" pthread_join returns once the thread is released" FACT
   THREAD-RET @ 206 = s" its value is the callback's: the argument plus the row it sorted, nested" FACT
   BAD @ 0= s" and that row came back sorted" FACT
   OWNER-IDENTITY
   CTX-TASK TASK:CONTEXT OLD-CONTEXT !
   CTX-TASK TASK:UNEXPOSE
   [: START TASK:SELF-CONTEXT ENTRY drop ;] catch 0=
      s" UNEXPOSE after the join drops the bindings: the slot binds to the main context" FACT
   START UNBIND
   [: CMP OLD-CONTEXT @ ENTRY drop ;] catch E-TASK-STATE =
      s" ENTRY with a CONSTRUCTED task's region number is E-TASK-STATE" FACT
   CTX-TASK TASK:KILL ;

\ ---- calls that meet a row a mover holds ------------------------------------------
\ Slot k's cell at offset off of its row, reached as the thunk reaches it:
\ through the table the main region's CB-ROWS publishes.
: SLOT-CELL ( n n -- ptr n ) {: k:n off:n :}
   TASK:MAIN-BASE {: base :}
   base CB-ROWS + atomic@ base FFI:>CELL - {: table:n :}
   base table + k CB-ROW-BYTES * + off + ;

\ Moves every idle row naming context ctx 0 -> BUSY, as a bind or UNEXPOSE's
\ claim moves it, and answers them as a mask. A row a thread is inside stays.
: HOLD-ROWS ( n -- n ) {: ctx:n :}
   0 CB-POOL 0 ?do
      i CB-ROW-REGION SLOT-CELL atomic@ ctx = if
         0 CB-ROW-BUSY i CB-ROW-OWNER SLOT-CELL atomic-cas 0= if 1 i lshift or then
      then
   loop ;

: HAND-BACK ( n -- ) {: mask:n :}
   CB-POOL 0 ?do
      mask 1 i lshift and 0 <> if 0 i CB-ROW-OWNER SLOT-CELL atomic! then
   loop ;

\ A call that meets a row held BUSY waits in the thunk until the row is handed
\ back: the start routine's own entry, and then, with the thread parked inside
\ START, its sort's nested calls through CMP. An UNEXPOSE refused on START's
\ row holds CMP's, the lower slot, in just this way; on the thunk that ended
\ such a call 106, that refusal killed the thread.
: CASE-HELD ( -- )
   s" == calls that meet a row a mover holds" SAY
   THREAD-BIND
   0 BAD !
   CTX-TASK TASK:CONTEXT HOLD-ROWS {: held:n :}
   held 0 <> s" the rows naming the context are held BUSY" FACT
   START-FN @ 9 THREAD-START 0= s" pthread_create starts a thread at a held row" FACT
   HELD-MS >MS TASK:SLEEP
   INSIDE atomic@ 0= s" the thread waits there and is not inside" FACT
   held HAND-BACK
   WAIT-INSIDE
   CTX-TASK TASK:CONTEXT HOLD-ROWS {: nested:n :}
   nested 0 <> nested held <> and
      s" parked inside START, the thread keeps its row and the other is held BUSY" FACT
   1 GATE atomic!
   HELD-MS >MS TASK:SLEEP
   THREAD-SORTS atomic@ 0= s" the nested calls wait there and finish no sort" FACT
   nested HAND-BACK
   THREAD-JOIN 0= s" pthread_join returns once that row is handed back" FACT
   THREAD-RET @ 210 = s" the thread's value is its callback's" FACT
   BAD @ 0= s" and the row it sorted came back sorted" FACT
   CTX-TASK TASK:UNEXPOSE
   CTX-TASK TASK:KILL ;

\ ---- a context that outlives the task that exposed it ---------------------------
: LIVE ( -- n )
   TASK:MAIN-BASE TASKS-LIVE-CELL + atomic@ ;

: CASE-HANDOFF ( -- )
   s" == a worker exposes a task, hands it to a thread and ends" SAY
   LIVE {: base:n :}
   ['] WORK-EXPOSES WORKER-A TASK:ACTIVATE
   WORKER-A TASK:JOIN JOINED 0= s" the worker started the thread and was joined" FACT
   WAIT-INSIDE
   LIVE base 1 + = s" the exposed task still counts on the main region" FACT
   1 GATE atomic!
   THREAD-JOIN 0= s" the main task joins the thread" FACT
   THREAD-RET @ 208 = s" whose callback ran to its result" FACT
   CTX-TASK TASK:UNEXPOSE
   LIVE base = s" UNEXPOSE gives the count back" FACT
   s" variable CBT-DEFINED-AFTER" INCLUDE-EVALUATE
   s" a definition succeeds" SAY
   CTX-TASK TASK:KILL ;

\ ---- a capture ------------------------------------------------------------------
: CASE-PREPARE ( -- )
   s" == IMAGE-LIFECYCLE:PREPARE" SAY
   [: 0 ARG@ drop ;] catch E-FFI-CALLBACK-STATE =
      s" a frame accessor outside a callback is E-FFI-CALLBACK-STATE" FACT
   CMP-VIA TASK:SELF-CONTEXT ENTRY drop
   IMAGE-LIFECYCLE:PREPARE
   CTX-TASK TASK:EXPOSE
   [: CMP-VIA CTX-TASK TASK:CONTEXT ENTRY drop ;] catch 0=
      s" the capture dropped the binding: the slot binds to another context" FACT
   CTX-TASK TASK:KILL
   CMP TASK:SELF-CONTEXT ENTRY CMP-FN !
   SORT-PLAIN s" ENTRY then qsort after the capture" FACT
   CMP UNBIND ;

\ ---- declarations the declarer refuses -------------------------------------------
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
: DECL-NINE-INTS ( -- )
   s" CALLBACK: CBT-X8 ( n n n n n n n n n -- ) ;CALLBACK" INCLUDE-EVALUATE ;
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

: CASE-DECLARATIONS ( -- )
   s" == declarations refused" SAY
   [: DECL-BAD-TOKEN ;] catch E-FFI-SYNTAX = s" an argument type outside n i32 u32 r ptr u8: E-FFI-SYNTAX" FACT
   [: DECL-BAD-PTR ;] catch E-FFI-SYNTAX = s" a pointer that is not ptr u8: E-FFI-SYNTAX" FACT
   [: DECL-TWO-RESULTS ;] catch E-FFI-SYNTAX = s" two results: E-FFI-SYNTAX" FACT
   [: DECL-PTR-RESULT ;] catch E-FFI-SYNTAX = s" a pointer result: E-FFI-SYNTAX" FACT
   [: DECL-NO-FALLBACK ;] catch E-FFI-SYNTAX = s" a value result with no fallback: E-FFI-SYNTAX" FACT
   [: DECL-WRONG-FALLBACK ;] catch E-FFI-SYNTAX = s" FFALLBACK for an integer result: E-FFI-SYNTAX" FACT
   [: DECL-VOID-FALLBACK ;] catch E-FFI-SYNTAX = s" FALLBACK with no result: E-FFI-SYNTAX" FACT
   [: DECL-NINE-INTS ;] catch E-FFI-ARITY = s" an integer argument past the registers: E-FFI-ARITY" FACT
   [: DECL-NINE-FLOATS ;] catch E-FFI-ARITY = s" a ninth float argument: E-FFI-ARITY" FACT
   [: DECL-STRAY-CLOSE ;] catch E-FFI-SYNTAX = s" ;CALLBACK with nothing open: E-FFI-SYNTAX" FACT
   [: DECL-STRAY-FALLBACK ;] catch E-FFI-SYNTAX = s" FALLBACK with nothing open: E-FFI-SYNTAX" FACT
   [: DECL-NESTED ;] catch E-FFI-SYNTAX = s" CALLBACK: inside an open declaration: E-FFI-SYNTAX" FACT
   [: DECL-SEVENTEENTH ;] catch E-FFI-CALLBACK-FULL =
      s" the seventeenth declaration, with nothing left open: E-FFI-CALLBACK-FULL" FACT ;

\ ---- children ---------------------------------------------------------------------
\ The name, the exit status and both streams of one spawned process.
: TRANSCRIBE ( ptr u8 n len len outcome -- )
   PROC-OUTCOME>RC RC>N {: name:ptr nameu:n outu:len erru:len rc:n :}
   s" == child " SCRIPT+ name nameu SCRIPT+ s" : exit " SCRIPT+ rc SCRIPT-N LF SCRIPT+C
   OUT outu LEN>N STREAM+
   s" --" SAY
   ERR erru LEN>N STREAM+ ;

: CHILD ( ptr u8 n -- len len outcome ) {: name:ptr nameu:n :}
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
   s" busy" CHILD-CASE
   s" unbound" CHILD-CASE
   s" worker-stale" CHILD-CASE
   s" unexposed" CHILD-CASE
   s" defines" CHILD-CASE
   s" body-defines" CHILD-CASE
   s" halted" CHILD-CASE
   s" killed" CHILD-CASE
   s" woken" CHILD-CASE
   s" fresh" CHILD-CASE
   s" captures" CHILD-CASE ;

\ ---- a stripped image ---------------------------------------------------------------
: SUBJECT$ ( -- ptr u8 n ) SUBJECT-BUF SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: SUBJECT-SOURCE$ ( -- ptr u8 n )
   S\" require test/stripped-callback-subject.f\n: MAIN ( -- ) STRIPPED-CALLBACK-SUBJECT:RUN ;\n" ;

\ A private cache root, so hb-build links the subject on every run.
: BUILD-IMAGE ( -- len len outcome )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/hb-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   SUBJECT$ >LEN PROC-ARGV+
   s" -o" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: RUN-IMAGE ( -- len len outcome )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN
   OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME ;

: LINKED ( len len outcome -- )
   PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if OUT outu LEN>N type ERR erru LEN>N type then
   rc 0= s" hb-build links test/stripped-callback-subject.f stripped" FACT ;

: STRIPPED-BODY ( -- )
   s" ffi-callback" GT-START
   s" subject.f" SUBJECT-BUF GT-PATH SUBJECT-U !
   s" application" IMAGE-BUF GT-PATH IMAGE-U !
   SUBJECT$ SUBJECT-SOURCE$ WRITE-ALL
   BUILD-IMAGE LINKED
   s" stripped" RUN-IMAGE TRANSCRIBE ;

: CASE-STRIPPED ( -- )
   s" == a stripped image" SAY
   [: STRIPPED-BODY ;] [: GT-CLEANUP ;] finally ;

\ ---- the transcript this file expects -------------------------------------------------
: WANT-SORTS ( -- )
   s" ok   qsort sorts through a callback; a stack cell, a local and a loop index survive" W
   s" ok   a comparator nests qsort through its own slot" W
   s" ok   a comparator nests qsort through a second slot on the context" W
   s" ok   a throwing comparator answers its fallback; FAULT@ keeps the code; depth is intact" W
   s" ok   UNBIND inside the comparator is E-TASK-STATE and qsort completes" W ;

: WANT-TASKS ( -- )
   s" == the main task" W
   WANT-SORTS
   s" == every argument register" W
   s" ok   n arrives whole" W
   s" ok   i32 is sign-extended from the low half" W
   s" ok   u32 is the low half" W
   s" ok   ptr u8 is the address C passed" W
   s" ok   x4..x7 arrive in order" W
   s" ok   d0..d7 arrive in order" W
   s" ok   an r result returns in d0" W
   s" ok   and nothing faulted" W
   s" ok   a body never stored answers the fallback" W
   s" ok   and faults E-FFI-CALLBACK-STATE" W
   s" == the owner a thread claims with" W
   s" ok   the main thread claims with one value, a thousand calls and yields apart" W
   s" ok   which is not the free owner" W
   s" == a worker on its own context, unbinding before it ends" W
   WANT-SORTS
   s" == a worker on its own context, ending bound" W
   WANT-SORTS
   s" ok   its rows went with it: the same slots bind to the main context" W ;

: WANT-THREADS ( -- )
   s" == a foreign thread on an exposed task" W
   s" ok   ENTRY of a slot bound to another context is E-TASK-STATE" W
   s" ok   pthread_create takes the entry as its start routine" W
   s" ok   UNEXPOSE with the thread parked inside is E-TASK-STATE" W
   s" ok   KILL with the thread parked inside is E-TASK-STATE" W
   s" ok   ACTIVATE of an exposed task is E-TASK-STATE" W
   s" ok   UNBIND of the slot the thread holds is E-TASK-STATE" W
   s" ok   UNEXPOSE stays refused while the thread nests through a second slot, and harms no call" W
   s" ok   pthread_join returns once the thread is released" W
   s" ok   its value is the callback's: the argument plus the row it sorted, nested" W
   s" ok   and that row came back sorted" W
   s" ok   a foreign thread claims with another value" W
   s" ok   each at the same offset from its own pthread_self" W
   s" ok   and pthread_self there is the pthread_t pthread_create stored" W
   s" ok   UNEXPOSE after the join drops the bindings: the slot binds to the main context" W
   s" ok   ENTRY with a CONSTRUCTED task's region number is E-TASK-STATE" W
   s" == calls that meet a row a mover holds" W
   s" ok   the rows naming the context are held BUSY" W
   s" ok   pthread_create starts a thread at a held row" W
   s" ok   the thread waits there and is not inside" W
   s" ok   parked inside START, the thread keeps its row and the other is held BUSY" W
   s" ok   the nested calls wait there and finish no sort" W
   s" ok   pthread_join returns once that row is handed back" W
   s" ok   the thread's value is its callback's" W
   s" ok   and the row it sorted came back sorted" W
   s" == a worker exposes a task, hands it to a thread and ends" W
   s" ok   the worker started the thread and was joined" W
   s" ok   the exposed task still counts on the main region" W
   s" ok   the main task joins the thread" W
   s" ok   whose callback ran to its result" W
   s" ok   UNEXPOSE gives the count back" W
   s" a definition succeeds" W
   s" == IMAGE-LIFECYCLE:PREPARE" W
   s" ok   a frame accessor outside a callback is E-FFI-CALLBACK-STATE" W
   s" ok   the capture dropped the binding: the slot binds to another context" W
   s" ok   ENTRY then qsort after the capture" W ;

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
   s" ok   the seventeenth declaration, with nothing left open: E-FFI-CALLBACK-FULL" W ;

: WANT-CHILDREN ( -- )
   s" == child outside: exit 106" W
   s" outside: a thread is parked on its own context" W
   s" --" W
   s" hb: callback: context is not inside a foreign call" W
   s" == child sleeping: exit 106" W
   s" sleeping: a thread is parked on its own context" W
   s" --" W
   s" hb: callback: context busy on another thread" W
   s" == child busy: exit 106" W
   s" busy: the first thread is parked inside the context" W
   s" --" W
   s" hb: callback: context busy on another thread" W
   s" == child unbound: exit 106" W
   s" unbound: sorted while bound" W
   s" --" W
   s" hb: callback: slot is not bound" W
   s" == child worker-stale: exit 106" W
   s" worker-stale: the worker sorted and ended bound" W
   s" --" W
   s" hb: callback: slot is not bound" W
   s" == child unexposed: exit 106" W
   s" unexposed: the context is gone" W
   s" --" W
   s" hb: callback: slot is not bound" W
   s" == child defines: exit 79" W
   s" defines: the exposing worker was joined" W
   s" --" W
   s" == child body-defines: exit 106" W
   s" --" W
   s" hb: callback: the body defined" W
   s" == child halted: exit 0" W
   s" halted: the worker is parked inside its comparator" W
   s" halted: the comparator returned, qsort completed and KILL joined the worker" W
   s" --" W
   s" == child killed: exit 0" W
   s" killed: the worker is stopped inside its comparator" W
   s" killed: its sort completed" W
   s" killed: the worker ended at its first TASK:PAUSE after the sort" W
   s" --" W
   s" == child woken: exit 0" W
   s" woken: a thread is stopped inside its callback" W
   s" woken: WAKE of the exposed task released it to its result" W
   s" --" W
   s" == child fresh: exit 0" W
   s" fresh: a hint to a run that ended does not reach an exposure" W
   s" fresh: nor does a hint to an earlier exposure" W
   s" --" W
   s" == child captures: exit 67" W
   s" captures: a thread is parked inside" W
   s" --" W
   s" task: callback slot in flight at capture - return from the foreign call before the build or snapshot captures" W
   s" == a stripped image" W
   s" ok   hb-build links test/stripped-callback-subject.f stripped" W
   s" == child stripped: exit 0" W
   s" stripped-callback: ok" W
   s" --" W ;

: WANT-TRANSCRIPT ( -- )
   0 WANT-U !
   WANT-TASKS WANT-THREADS WANT-DECLARATIONS WANT-CHILDREN ;

\ The artifact as another reader will find it: read back from its file and
\ compared whole.
: TRANSCRIPT-CASE ( -- )
   s" build" MAKE-DIRS
   s" build/ffi-callback-transcript.txt" SCRIPT$ WRITE-ALL
   s" build/ffi-callback-transcript.txt" BACK-BUF SCRIPT-CAP READ-ALL {: got:n :}
   WANT-TRANSCRIPT
   s" build/ffi-callback-transcript.txt is the transcript this file expects" T-LABEL
   BACK-BUF got WANT$ T$= ;

public

: RUN-SUITE ( -- )
   T-RESET
   0 SCRIPT-U !
   CASE-MAIN
   CASE-DIRECT
   CASE-OWNER
   CASE-WORKERS
   CASE-THREAD
   CASE-HELD
   CASE-HANDOFF
   CASE-PREPARE
   CASE-DECLARATIONS
   CASE-CHILDREN
   CASE-STRIPPED
   TRANSCRIPT-CASE
   T-REPORT ;

;using
;package

FFI-CB-TEST:RUN-SUITE
