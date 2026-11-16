\ fault-record.f - the text of the fault records a rejection's machine code
\ writes. A fault record is the bytes package REJECTION writes for an unplaced
\ rejection of the same parts, in prose or, when the main DATA's
\ REJECTION-JSON-CELL is set, as one JSON object. A site's emitter opens the
\ record with its code and message, adds notes, then stops or keeps it; each
\ part's static text is built here twice, prose and JSON, and the run writes
\ the one the flag picks. REJECTION is Forth on a thread's VM stacks, and these
\ sites run where none can: a signal handler (no Forth word is
\ async-signal-safe), a stack past its bounds, a callback thread with no Habu
\ frame, a stop that must not be caught.
require src/core/exit-status.f
require src/lib/string.f

package FAULT-RECORD
public
256 constant FAULT-FRAME
private
256 constant FAULT-TEXT-CAP
public
\ The text an emitter lays next, then empties.
create FAULT-PROSE FAULT-TEXT-CAP allot  variable FAULT-PROSE-U
create FAULT-JSON FAULT-TEXT-CAP allot  variable FAULT-JSON-U
private
\ The part the record being emitted has open: none, its message, or a note.
0 constant FAULT-SHUT
1 constant FAULT-MESSAGE
2 constant FAULT-NOTE
variable FAULT-PART  FAULT-SHUT FAULT-PART !

: TEXT+ ( ^u8 n ^u8 ^n -- )
   {: a:^ u:n buf:^ len:^ :}
   len @ u + FAULT-TEXT-CAP > IF
      s" crash: a fault record's text is longer than FAULT-TEXT-CAP" EXIT-STATUS:INTERNAL die
   THEN
   u 0 ?do  a i + c@  buf len @ + i + c!  loop
   len @ u + len ! ;
: PROSE+ ( ^u8 n -- )  FAULT-PROSE FAULT-PROSE-U TEXT+ ;
: JSON+ ( ^u8 n -- )  FAULT-JSON FAULT-JSON-U TEXT+ ;
: BOTH+ ( ^u8 n -- ) {: a:^ u:n :}  a u PROSE+  a u JSON+ ;

\ A site's text is laid as is in a JSON string, so it holds no byte JSON escapes.
: PLAIN? ( n -- bool ) {: c:n :}
   c 32 >=  c 34 <> and  c 92 <> and ;
: CHECK-TEXT ( ^u8 n -- ) {: a:^ u:n :}
   u 0 ?do
      a i + c@ PLAIN? 0= IF
         s" crash: a fault record's text holds a byte JSON escapes" EXIT-STATUS:INTERNAL die
      THEN
   loop ;

: PART? ( n -- ) {: want:n :}
   FAULT-PART @ want <> IF
      s" crash: a fault record's parts are out of REJECTION's order" EXIT-STATUS:INTERNAL die
   THEN ;
: NOTE-PART? ( -- )
   FAULT-PART @ FAULT-SHUT = IF
      s" crash: a fault note outside a record" EXIT-STATUS:INTERNAL die
   THEN ;

public
\ A note's key, after the text before it.
: NOTE+ ( ^u8 n -- ) {: k:^ ku:n :}
   NOTE-PART?  k ku CHECK-TEXT
   S\" \n  = " PROSE+  k ku PROSE+  s" : " PROSE+
   S\" \"," JSON+
   FAULT-PART @ FAULT-MESSAGE = IF S\" \"notes\":{" JSON+ THEN
   S\" \"" JSON+  k ku JSON+  S\" \":\"" JSON+
   FAULT-NOTE FAULT-PART ! ;

\ The record's start: its code and message.
: FAULT-OPEN ( ^u8 n ^u8 n -- )
   {: code:^ cu:n msg:^ mu:n :}
   FAULT-SHUT PART?  code cu CHECK-TEXT  msg mu CHECK-TEXT
   s" error[" PROSE+  code cu PROSE+  s" ]: " PROSE+  msg mu PROSE+
   S\" {\"severity\":\"error\",\"code\":\"" JSON+  code cu JSON+
   S\" \",\"message\":\"" JSON+  msg mu JSON+
   FAULT-MESSAGE FAULT-PART ! ;

\ A note whose value is an address, `0x` and its hex digits.
: FAULT-ADDRESS ( ^u8 n -- )  NOTE+  s" 0x" BOTH+ ;

\ Note `in`, the word whose code holds an address; n is the part open before
\ it, which FAULT-AT takes when the site finds no such word.
: FAULT-IN ( -- n )
   NOTE-PART?  FAULT-PART @  s" in" NOTE+ ;

\ Note `at` and the address, from the part FAULT-IN found open.
: FAULT-AT ( n -- )  FAULT-PART !  s" at" FAULT-ADDRESS ;

\ The record's end, with its help, for a site that exits.
: FAULT-STOP ( ^u8 n -- ) {: h:^ hu:n :}
   NOTE-PART?  h hu CHECK-TEXT
   S\" \n  = help: " PROSE+  h hu PROSE+  S\" \n\n" PROSE+
   FAULT-PART @ FAULT-NOTE = IF S\" \"}" ELSE S\" \"" THEN JSON+
   S\" ,\"help\":\"" JSON+  h hu JSON+  S\" \"}\n" JSON+
   FAULT-SHUT FAULT-PART ! ;

\ The record's end, with its help, none for an empty text, for a site that
\ returns.
: FAULT-KEEP ( ^u8 n -- ) {: h:^ hu:n :}
   NOTE-PART?  h hu CHECK-TEXT
   hu 0 <> IF S\" \n  = help: " PROSE+  h hu PROSE+ THEN
   S\" \n\n" PROSE+
   FAULT-PART @ FAULT-NOTE = IF S\" \"}" ELSE S\" \"" THEN JSON+
   hu 0 <> IF S\" ,\"help\":\"" JSON+  h hu JSON+  S\" \"" JSON+ THEN
   S\" }\n" JSON+
   FAULT-SHUT FAULT-PART ! ;

\ The four stack-bounds messages and their help.
: CRS-DATA$ ( -- ^u8 n ) s" the data stack ran past its bounds" ;
: CRS-RET$ ( -- ^u8 n ) s" the return stack ran past its bounds" ;
: CRS-LOOP$ ( -- ^u8 n ) s" the loop stack ran past its bounds" ;
: CRS-MACHINE$ ( -- ^u8 n ) s" the machine stack ran past its bounds" ;
: CRS-HELP$ ( -- ^u8 n )
   s" give a recursion an exit, or keep fewer values on the stack" ;

: TASK-GUARD$ ( -- ^u8 n )
   s" ran on a task's thread, and only the main thread defines and allots" ;
: TASK-GUARD-HELP$ ( -- ^u8 n )
   s" define and allot on the main thread before the task starts" ;
: LOWER-GUARD$ ( -- ^u8 n ) s" ran while a task is live" ;
: LOWER-GUARD-HELP$ ( -- ^u8 n )
   s" wait for every task to end before forgetting definitions" ;

private
128 constant GUARD-MSG-CAP
create GUARD-MSG GUARD-MSG-CAP allot
TYPED-VARIABLE GUARD-MSG-U len
: GUARD-MSG+ ( ^u8 n -- )
   GUARD-MSG GUARD-MSG-CAP GUARD-MSG-U BUF-APPEND ;
public
\ "`<name>` <rest>", held in GUARD-MSG until the next call.
: GUARD-MSG$ ( ^u8 n ^u8 n -- ^u8 n )
   {: p:^ pu:n r:^ ru:n :}
   GUARD-MSG-U BUF-RESET
   s" `" GUARD-MSG+  p pu GUARD-MSG+  s" ` " GUARD-MSG+
   r ru GUARD-MSG+
   GUARD-MSG GUARD-MSG-U BUF-LEN@ ;

\ The callback rejections' messages and help: a thunk C entered, and an
\ outbound foreign call on a context another thread runs in.
: CB-NO-TABLE$ ( -- ^u8 n )
   s" C called a callback before any callback was bound" ;
: CB-NO-TABLE-HELP$ ( -- ^u8 n )
   s" bind it with `FFI-CB:ENTRY` before C can call it" ;
: CB-ROW-BUSY$ ( -- ^u8 n )
   s" C called the callback on a second thread while another thread runs it" ;
: CB-ROW-BUSY-HELP$ ( -- ^u8 n )
   s" bind it to `TASK:CALLER-CONTEXT` for calls from many threads at once" ;
: CB-UNBOUND$ ( -- ^u8 n ) s" C called a callback whose slot is not bound" ;
: CB-UNBOUND-HELP$ ( -- ^u8 n )
   s" keep the callback bound until the C library drops the pointer" ;
: CB-NO-REGION$ ( -- ^u8 n )
   s" C called a callback bound to the caller's context on a thread with no region of its own" ;
: CB-NO-REGION-HELP$ ( -- ^u8 n )
   s" call it only on a thread a Habu task runs" ;
: CB-OWNER-BUSY$ ( -- ^u8 n )
   s" C called the callback on one thread while another thread runs in its context" ;
: CB-OWNER-BUSY-HELP$ ( -- ^u8 n )
   s" call it only on the thread whose context it is bound to" ;
: CB-NO-FRAME$ ( -- ^u8 n )
   s" C called the callback outside the foreign call that passed it" ;
: CB-NO-FRAME-HELP$ ( -- ^u8 n )
   s" call it only while that foreign call runs" ;
: CB-NO-XT$ ( -- ^u8 n )
   s" C called a callback slot that no `CALLBACK:` declared" ;
: CB-NO-XT-HELP$ ( -- ^u8 n )
   s" pass C only the pointer `FFI-CB:ENTRY` answers" ;
: CB-DEFINED$ ( -- ^u8 n ) s" the callback's body defined a word" ;
: CB-DEFINED-HELP$ ( -- ^u8 n )
   s" define words on the main thread, outside callbacks" ;
: CB-CONTEXT-BUSY$ ( -- ^u8 n )
   s" a foreign call ran in a context another thread is running in" ;
: CB-CONTEXT-BUSY-HELP$ ( -- ^u8 n )
   s" call C from one thread at a time in a context, and bind callbacks other threads call to `TASK:CALLER-CONTEXT`" ;
;package
