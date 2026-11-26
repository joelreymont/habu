\ fault-record.f - the text of the fault records a rejection's machine code
\ writes. A fault record is the bytes package REJECTION writes for an unplaced
\ rejection of the same parts, in prose or, when the main DATA's
\ REJECTION-JSON-CELL is set, as one JSON object. A site's emitter opens the
\ record with its code and message, adds notes, then stops or keeps it, the
\ open record a linear token the checker holds to that order; each
\ part's static text is built here twice, prose and JSON, and the run writes
\ the one the flag picks. REJECTION is Forth on a thread's VM stacks, and these
\ sites run where none can: a signal handler (no Forth word is
\ async-signal-safe), a stack past its bounds, a callback thread with no Habu
\ frame, a stop that must not be caught.
require src/core/exit-status.f
require src/core/rejection.f
require src/lib/string.f
require src/lib/arena.f
require src/lib/vector.f

package FAULT-RECORD
public
256 constant FAULT-FRAME
DEFLINEAR FAULT-RECORD:record
private
\ A file link zeroes scratch cells after each piece, so the texts are made when
\ first needed and released by the emitter that lays them. They live in an arena:
\ a cross-build runs this file on its host, and the heap here is the target's.
TYPED-VARIABLE TEXTS ^ARENA:arena SCRATCH-ONLY
TYPED-VARIABLE PROSE-TEXT ^VEC:vector<u8> SCRATCH-ONLY
TYPED-VARIABLE JSON-TEXT ^VEC:vector<u8> SCRATCH-ONLY
TYPED-VARIABLE GUARD-TEXT ^VEC:vector<u8> SCRATCH-ONLY
: NEW-TEXT ( -- ^VEC:vector<u8> )
   TEXTS @ NULL-PTR = if ARENA:NEW TEXTS ! then
   TEXTS @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW ;
: PROSE ( -- ^VEC:vector<u8> )
   PROSE-TEXT @ NULL-PTR = if NEW-TEXT PROSE-TEXT ! then
   PROSE-TEXT @ ;
: JSON ( -- ^VEC:vector<u8> )
   JSON-TEXT @ NULL-PTR = if NEW-TEXT JSON-TEXT ! then
   JSON-TEXT @ ;
: GUARD ( -- ^VEC:vector<u8> )
   GUARD-TEXT @ NULL-PTR = if NEW-TEXT GUARD-TEXT ! then
   GUARD-TEXT @ ;
public
\ The text an emitter lays next, then empties.
: FAULT-PROSE$ ( -- str ) PROSE VEC:BYTES ;
: FAULT-JSON$ ( -- str ) JSON VEC:BYTES ;
: FAULT-EMPTY ( -- )
   TEXTS @ ARENA:RELEASE  NULL-PTR TEXTS !
   NULL-PTR PROSE-TEXT !  NULL-PTR JSON-TEXT !  NULL-PTR GUARD-TEXT ! ;
private
\ An open record carries its open part: its message, or a note.
1 constant PART-MESSAGE
2 constant PART-NOTE
LINEAR: MINT ( n -- FAULT-RECORD:record )
LINEAR: ERASE ( FAULT-RECORD:record -- n )
: PART ( FAULT-RECORD:record -- FAULT-RECORD:record n )
   ERASE dup MINT swap ;

: PROSE+ ( str -- )  PROSE VEC:APPEND ;
: JSON+ ( str -- )  JSON VEC:APPEND ;
: BOTH+ ( str -- ) {: s:str :}  s PROSE+  s JSON+ ;

\ The byte at the pointer, as a JSON string holds it.
: ESC-BYTE ( ^u8 -- ) {: p:^ :}
   p c@ REJECTION:JSON-ESCAPE$ {: e:str :}
   e STR:LEN 0= IF p 1 >str JSON+ ELSE e JSON+ THEN ;
\ A caller's text, as a JSON string holds it.
: ESC+ ( str -- ) {: s:str :}
   s STR:LEN 0 ?do s STR:DATA i + ESC-BYTE loop ;

\ Closes the open part's JSON string, and the notes object after a note.
: SHUT ( FAULT-RECORD:record -- )
   ERASE PART-NOTE = IF S\" \"}" ELSE S\" \"" THEN JSON+ ;

public
\ A note's key, after the text before it.
: NOTE+ ( FAULT-RECORD:record str -- FAULT-RECORD:record )
   {: k:str :}
   S\" \n  = " PROSE+  k PROSE+  s" : " PROSE+
   S\" \"," JSON+
   ERASE PART-MESSAGE = IF S\" \"notes\":{" JSON+ THEN
   S\" \"" JSON+  k ESC+  S\" \":\"" JSON+
   PART-NOTE MINT ;

\ The record's start: its code and message.
: FAULT-OPEN ( str str -- FAULT-RECORD:record )
   {: code:str msg:str :}
   s" error[" PROSE+  code PROSE+  s" ]: " PROSE+  msg PROSE+
   S\" {\"severity\":\"error\",\"code\":\"" JSON+  code ESC+
   S\" \",\"message\":\"" JSON+  msg ESC+
   PART-MESSAGE MINT ;

\ A note whose value is an address, `0x` and its hex digits.
: FAULT-ADDRESS ( FAULT-RECORD:record str -- FAULT-RECORD:record )
   NOTE+  s" 0x" BOTH+ ;

\ Note `in`, the word whose code holds an address; n is the part open before
\ it, which FAULT-AT takes when the site finds no such word.
: FAULT-IN ( FAULT-RECORD:record -- FAULT-RECORD:record n )
   PART {: was:n :}  s" in" NOTE+  was ;

\ Note `at` and the address, from the part FAULT-IN found open.
: FAULT-AT ( FAULT-RECORD:record n -- FAULT-RECORD:record )
   swap ERASE drop MINT  s" at" FAULT-ADDRESS ;

\ The record's end, with its help, for a site that exits.
: FAULT-STOP ( FAULT-RECORD:record str -- ) {: h:str :}
   S\" \n  = help: " PROSE+  h PROSE+  S\" \n\n" PROSE+
   SHUT
   S\" ,\"help\":\"" JSON+  h ESC+  S\" \"}\n" JSON+ ;

\ The record's end, with its help, none for an empty text, for a site that
\ returns.
: FAULT-KEEP ( FAULT-RECORD:record str -- ) {: h:str :}
   h STR:LEN 0 <> IF S\" \n  = help: " PROSE+  h PROSE+ THEN
   S\" \n\n" PROSE+
   SHUT
   h STR:LEN 0 <> IF S\" ,\"help\":\"" JSON+  h ESC+  S\" \"" JSON+ THEN
   S\" }\n" JSON+ ;

\ The four stack-bounds messages and their help.
: CRS-DATA$ ( -- str ) s" the data stack ran past its bounds" ;
: CRS-RET$ ( -- str ) s" the return stack ran past its bounds" ;
: CRS-LOOP$ ( -- str ) s" the loop stack ran past its bounds" ;
: CRS-MACHINE$ ( -- str ) s" the machine stack ran past its bounds" ;
: CRS-HELP$ ( -- str )
   s" give a recursion an exit, or keep fewer values on the stack" ;

: TASK-GUARD$ ( -- str )
   s" ran on a task's thread, and only the main thread defines and allots" ;
: TASK-GUARD-HELP$ ( -- str )
   s" define and allot on the main thread before the task starts" ;

\ "`<name>` <rest>", held until the record's next PIECE,.
: GUARD-MSG$ ( str str -- str )
   {: p:str r:str :}
   GUARD 0 VEC:RESIZE
   s" `" GUARD VEC:APPEND  p GUARD VEC:APPEND  s" ` " GUARD VEC:APPEND
   r GUARD VEC:APPEND
   GUARD VEC:BYTES ;

\ The callback rejections' messages and help: a thunk C entered, and an
\ outbound foreign call on a context another thread runs in.
: CB-NO-TABLE$ ( -- str )
   s" C called a callback before any callback was bound" ;
: CB-NO-TABLE-HELP$ ( -- str )
   s" bind it with `FFI-CB:ENTRY` before C can call it" ;
: CB-ROW-BUSY$ ( -- str )
   s" C called the callback on a second thread while another thread runs it" ;
: CB-ROW-BUSY-HELP$ ( -- str )
   s" bind it to `TASK:CALLER-CONTEXT` for calls from many threads at once" ;
: CB-UNBOUND$ ( -- str ) s" C called a callback whose slot is not bound" ;
: CB-UNBOUND-HELP$ ( -- str )
   s" keep the callback bound until the C library drops the pointer" ;
: CB-NO-REGION$ ( -- str )
   s" C called a callback bound to the caller's context on a thread with no region of its own" ;
: CB-NO-REGION-HELP$ ( -- str )
   s" call it only on a thread a Habu task runs" ;
: CB-OWNER-BUSY$ ( -- str )
   s" C called the callback on one thread while another thread runs in its context" ;
: CB-OWNER-BUSY-HELP$ ( -- str )
   s" call it only on the thread whose context it is bound to" ;
: CB-NO-FRAME$ ( -- str )
   s" C called the callback outside the foreign call that passed it" ;
: CB-NO-FRAME-HELP$ ( -- str )
   s" call it only while that foreign call runs" ;
: CB-NO-XT$ ( -- str )
   s" C called a callback slot that no `CALLBACK:` declared" ;
: CB-NO-XT-HELP$ ( -- str )
   s" pass C only the pointer `FFI-CB:ENTRY` answers" ;
: CB-DEFINED$ ( -- str ) s" the callback's body defined a word" ;
: CB-DEFINED-HELP$ ( -- str )
   s" define words on the main thread, outside callbacks" ;
: CB-CONTEXT-BUSY$ ( -- str )
   s" a foreign call ran in a context another thread is running in" ;
: CB-CONTEXT-BUSY-HELP$ ( -- str )
   s" call C from one thread at a time in a context, and bind callbacks other threads call to `TASK:CALLER-CONTEXT`" ;
;package
