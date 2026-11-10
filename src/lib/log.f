\ log.f - lines on fd 2 that say why a program failed, package LOG. A failure
\ caused outside the program (a provider, the network, the OS) is logged once,
\ with its text, by the library that saw it, just before it throws.
\ LINE opens a line, TEXT+, INT+ and CODE+ append to it, ;LINE writes it as
\ `<YYYY-MM-DDTHH:MM:SSZ> <level> <tag>: <text>` and LF, the stamp UTC. The tag
\ is the caller's package name in lower case; LOG does not check it.
\ TEXT+ writes a byte below $20, $7F and `\` as `\xHH`, so a line stays one
\ line and reads one way. A line fd 2 takes only in part is cut: a lone LF ends
\ it, at once or before the next line, which is lost while fd 2 refuses that LF.
\ A log call never throws for LOG's own failure: a line fd 2 refuses is lost,
\ and a refused mapping loses the line, which ;LINE replaces with NOTICE. A LINE
\ while a line is open abandons that line, as a throw does: once the new line is
\ closed, the abandoned line's remaining TEXT+ and ;LINE write nothing.
\ STORAGE CLASS. TASK-LOCAL: the line, a TASK:+USER BUF row kept across lines
\ and released once a line grows it past START-CAP; ARMED, set once this run
\ has registered the row's release; PHASE, whether a line is closed, open or
\ lost. A line logged from a TASK:AT-EXIT cleanup leaks its row: the release
\ LINE registers then joins a chain already running (src/lib/task.f
\ TASK-RUN-EXIT).
\ PROCESS-WIDE: LOCK, held for each line's write, which keeps this process's
\ lines whole (another process writing the same fd can still split a line
\ longer than PIPE_BUF); NOTICE, written under it; and TAIL, whether fd 2's
\ last line is whole or cut, read and written under it.

require src/lib/error.f
require src/lib/prelude.f
require src/lib/type/deftype.f
require src/lib/task.f
require src/lib/image-lifecycle.f
require src/lib/byte-buffer.f
require src/lib/span.f
require src/lib/date.f
require src/lib/fmt.f
require src/lib/fd.f

package LOG

private

DEFTYPE LEVEL

0 constant ERROR-N
1 constant WARN-N
$A constant LF
\ A mapping is at least a page, so lines up to 4 KiB reuse the row.
$1000 constant START-CAP
$20 constant SPACE-B
$7F constant DEL-B
$5C constant BACKSLASH

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and BUF:HDR-BYTES TASK:+USER ROW drop
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $8 TASK:+USER ARMED drop
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $8 TASK:+USER PHASE drop
TASK:FACILITY LOCK
LOCK TASK:FACILITY-INIT

\ TAIL, fd 2's last line, and its machine; each input comes under LOCK.
\ | state | a line's write: all | none             | part                   | the mending LF: moved | refused               |
\ | WHOLE | WHOLE               | WHOLE, line lost | CUT, then mend at once | -                     | -                     |
\ | CUT   | (a line is written only once whole)                              | WHOLE                 | CUT, the line is lost |
0 constant WHOLE
1 constant CUT
variable TAIL


\ PHASE's states; a fresh task's is CLOSED. Each input in each state, and the
\ state it leaves:
\   state    LINE              TEXT+ INT+ CODE+  ;LINE             refused map
\   CLOSED   opens: OPENED     nothing           nothing: CLOSED   -
\   OPENED   abandons: OPENED  appends           the row: CLOSED   LOST
\   LOST     abandons: OPENED  nothing           NOTICE: CLOSED    LOST
\ LINE leaves LOST when the row's mapping is refused; ;LINE leaves CLOSED also
\ when its write throws. An abandoned line is never written. The row's release
\ leaves CLOSED.
0 constant CLOSED
1 constant OPENED
2 constant LOST


\ The line written for a lost one: a stamp, then the text.
: NOTICE-TEXT$ ( -- ptr u8 n )  S\"  error log: out of memory, a line was lost\n" ;
DATE:TIME-LEN NOTICE-TEXT$ nip + constant NOTICE-U
NOTICE-U BUFFER: NOTICE
NOTICE-TEXT$ NOTICE DATE:TIME-LEN + swap BYTE-COPY


: ROW-RELEASE ( -- )
   0 ARMED !
   CLOSED PHASE !
   ROW BUF:DISPOSE ;


\ A task's row is released at its exit; the main task has no exit chain, and a
\ capture must not carry its row.
: REGISTER ( -- )
   TASK:SELF-N 0= if [: ROW-RELEASE ;] IMAGE-LIFECYCLE:REGISTER exit then
   [: ROW-RELEASE ;] TASK:SELF TASK:AT-EXIT ;


\ ARMED is set only after REGISTER completes, so a refused one is retried.
: ARM ( -- )
   ARMED @ 0<> if exit then
   REGISTER
   1 ARMED ! ;


\ A refused mapping loses the open line; any other code goes on.
: MAP-REFUSED ( n -- )
   {: code:n :}
   code 0= if exit then
   code E-MEM-MAP = code DYNAMIC-STORAGE:E-MAP = or if LOST PHASE ! exit then
   code throw ;


: ROW-READY ( -- )
   ROW BUF:LIVE? if ROW BUF:CLEAR exit then
   ROW START-CAP BUF:N>BLEN BUF:INIT ;


: STAMP ( -- )
   ROW DATE:TIME-LEN BUF:N>BLEN BUF:GROW SPAN:$ {: at:ptr u:n :}
   epoch-seconds at u DATE:FORMAT-EPOCH-UTC 2drop ;


: OPEN ( -- )
   ARM
   ROW-READY
   STAMP ;


: LEVEL$ ( level -- ptr u8 n )
   LEVEL>N ERROR-N = if s" error" exit then
   s" warn" ;


: ESCAPE? ( n -- bool )
   {: b:n :}
   b SPACE-B < b DEL-B = or b BACKSLASH = or ;


: HEX+ ( n -- )  s" 0123456789abcdef" drop + c@ ROW BUF:APPEND-BYTE ;


: ESCAPE+ ( n -- )
   {: b:n :}
   BACKSLASH ROW BUF:APPEND-BYTE
   [char] x ROW BUF:APPEND-BYTE
   b 4 rshift HEX+
   b $F and HEX+ ;


\ Appends at[from, to).
: RUN+ ( ptr u8 n n -- )
   {: at:ptr from:n to:n :}
   at from + to from - BUF:N>BLEN ROW BUF:APPEND-SPAN ;


\ TEXT+'s contract: the row's first growth, to hold the text unescaped, comes
\ before any byte of the text is read.
: ESCAPED+ ( ptr u8 n -- )
   {: at:ptr u:n :}
   ROW  ROW BUF:LEN@ BUF:BLEN>N u BUF:N>BLEN BUF:BLEN>N + BUF:N>BLEN  BUF:ENSURE
   0 u 0 ?do
      at i + c@ ESCAPE? if
         at over i RUN+
         at i + c@ ESCAPE+
         drop i 1+
      then
   loop
   at swap u RUN+ ;


: LF$ ( -- ptr u8 n )  S\" \n" ;


\ One lone LF to fd 2, under the caller's SIGPIPE hold. A one-byte write that
\ answers `part` counted past its byte, so the byte moved.
: MEND ( -- )
   FD:STDERR LF$ FD:TRY-WRITE-FULL MATCH FD:wrote
      all OF WHOLE TAIL ! ENDOF
      none OF ENDOF
      part OF WHOLE TAIL ! ENDOF
   ;MATCH ;


\ Mends a cut line first; true when the next line may be written.
: MENDED? ( -- bool )
   TAIL @ CUT = if MEND then
   TAIL @ WHOLE = ;


: WROTE ( FD:wrote -- )
   MATCH FD:wrote
      all OF ENDOF
      none OF ENDOF
      part OF CUT TAIL ! MEND ENDOF
   ;MATCH ;


: ROW-OUT ( -- )
   FD:STDERR
   [: MENDED? if FD:STDERR ROW BUF:SPAN$ BUF:BLEN>N FD:TRY-WRITE-FULL WROTE then ;]
   FD:WITH-NOSIGPIPE ;


: NOTICE-OUT ( -- )
   epoch-seconds NOTICE DATE:TIME-LEN DATE:FORMAT-EPOCH-UTC 2drop
   FD:STDERR [: MENDED? if FD:STDERR NOTICE NOTICE-U FD:TRY-WRITE-FULL WROTE then ;]
   FD:WITH-NOSIGPIPE ;


\ A line is lost when fd 2's SIGPIPE hold is refused; a write fd 2 refuses is
\ answered by TAIL's machine (WROTE, MEND). The flag is held only for the
\ write: other processes share fd 2's file description and its flag, and a hold
\ of theirs that overlaps this one can undo it (FD:WITH-NOSIGPIPE).
: OUT ( [ -- ] -- )
   catch {: code:n :}
   code 0= code E-PROC-OUTPUT = or if exit then
   code throw ;


: WRITE ( [ -- ] -- )
   LOCK TASK:GET
   [: OUT ;] [: LOCK TASK:RELEASE ;] finally ;


: FINISH ( -- )
   PHASE @ CLOSED = if exit then
   PHASE @ OPENED = if [: LF ROW BUF:APPEND-BYTE ;] catch MAP-REFUSED then
   PHASE @ LOST = if [: NOTICE-OUT ;] WRITE exit then
   [: ROW-OUT ;] WRITE ;


\ A task that logged one huge line keeps none of its storage.
: SHED ( -- )
   ROW BUF:LIVE? 0= if exit then
   ROW BUF:CAP@ BUF:BLEN>N START-CAP <= if exit then
   ROW BUF:DISPOSE ;

public

: ERROR ( -- level )  ERROR-N >LEVEL ;
: WARN ( -- level )  WARN-N >LEVEL ;


: TEXT+ ( ptr u8 n -- )
   PHASE @ OPENED <> if 2drop exit then
   [: 2dup ESCAPED+ ;] catch >r 2drop r> MAP-REFUSED ;


: INT+ ( n -- )  FMT:INT$ TEXT+ ;


\ A code and the name and text ERROR:CODE declared for it:
\ `-9701 MODEL:E-KEY: the provider refused the key`, the number alone when none.
: CODE+ ( n -- ) {: code:n :}
   code INT+
   code ERROR:NAME$ {: name:ptr nameu:n :}
   nameu 0= if exit then
   s"  " TEXT+ name nameu TEXT+ s" : " TEXT+ code ERROR:TEXT$ TEXT+ ;


\ A line a throw abandoned between LINE and ;LINE is never written; the next
\ LINE clears it, and the task's release frees its row.
: LINE ( level ptr u8 n -- )
   {: level:level tag:ptr tagu:n :}
   CLOSED PHASE !
   [: OPEN  OPENED PHASE ! ;] catch MAP-REFUSED
   s"  " TEXT+  level LEVEL$ TEXT+  s"  " TEXT+
   tag tagu TEXT+  s" : " TEXT+ ;


: ;LINE ( -- )
   [: FINISH ;] [: CLOSED PHASE !  SHED ;] finally ;

;package
