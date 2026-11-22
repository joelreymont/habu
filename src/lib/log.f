split-str
\ log.f - lines on fd 2 that say why a program failed, and, at info, what a
\ service did, package LOG. A failure caused outside the program (a provider,
\ the network, the OS) is logged once, with its text, by the library that saw
\ it, just before it throws.
\ LINE opens a line, TEXT+, INT+ and CODE+ append to it, ;LINE writes it as
\ `<YYYY-MM-DDTHH:MM:SSZ> <level> <tag>: <text>` and LF, the stamp UTC. The tag
\ is the caller's package name in lower case; LOG does not check it.
\ TEXT+ writes a byte below $20, $7F and `\` as `\xHH`, so a line stays one
\ line and reads one way. A line fd 2 takes only in part is cut: a lone LF ends
\ it, at once or before the next line, which is lost while fd 2 refuses that LF.
\ A log call never throws for LOG's own failure: a line fd 2 refuses is lost,
\ and a refused allocation loses the line, which ;LINE replaces with NOTICE. A
\ LINE while a line is open abandons that line, as a throw does: once the new
\ line is closed, the abandoned line's remaining TEXT+ and ;LINE write nothing.
\ STORAGE CLASS. TASK-LOCAL: the line, a per-task vector kept across lines and
\ released once a line grows it past START-CAP; ARMED, set once this run
\ has run REGISTER; PHASE, whether a line is closed, open or lost. A line
\ logged from a TASK:AT-EXIT cleanup releases its row too: the release LINE
\ registers then runs in the same ending.
\ PROCESS-WIDE: LOCK, held for each line's write, which keeps this process's
\ lines whole (another process writing the same fd can still split a line
\ longer than PIPE_BUF); NOTICE, written under it; and TAIL, whether fd 2's
\ last line is whole or cut, read and written under it.

require src/lib/error.f
require src/lib/prelude.f
require src/lib/type/deftype.f
require src/lib/task.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/span.f
require src/lib/date.f
require src/lib/fmt.f
require src/lib/fd.f

package LOG

private

DEFTYPE LEVEL

0 constant ERROR-N
1 constant WARN-N
2 constant INFO-N
$A constant LF
\ The bytes each logging task keeps between lines: the row's starting capacity
\ and the most SHED leaves it.
$1000 constant START-CAP
$20 constant SPACE-B
$7F constant DEL-B
$5C constant BACKSLASH

\ Null until a line makes the row, and again once it is released.
TASK:+USER ROW ^VEC:vector<u8>
TASK:+USER ARMED n
TASK:+USER PHASE n
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
\   state    LINE              TEXT+ INT+ CODE+  ;LINE             refused block
\   CLOSED   opens: OPENED     nothing           nothing: CLOSED   -
\   OPENED   abandons: OPENED  appends           the row: CLOSED   LOST
\   LOST     abandons: OPENED  nothing           NOTICE: CLOSED    LOST
\ LINE leaves LOST when the row's block is refused; ;LINE leaves CLOSED also
\ when its write throws. An abandoned line is never written. The row's release
\ leaves CLOSED.
0 constant CLOSED
1 constant OPENED
2 constant LOST


\ The line written for a lost one: a stamp, then the text.
: NOTICE-TEXT$ ( -- str )  S\"  error log: out of memory, a line was lost\n" ;
DATE:TIME-LEN NOTICE-TEXT$ nip + constant NOTICE-U
NOTICE-U BUFFER: NOTICE
NOTICE-TEXT$ NOTICE DATE:TIME-LEN + swap BYTE-COPY


: ROW-FREE ( -- )  ROW @  NULL-PTR ROW !  VEC:DELETE ;


: ROW-RELEASE ( -- )
   0 ARMED !
   CLOSED PHASE !
   ROW-FREE ;


\ A task's row is released at its exit; the main task has no exit chain, and its
\ row lives as long as the process.
: REGISTER ( -- )
   TASK:SELF-N 0= if exit then
   [: ROW-RELEASE ;] TASK:SELF TASK:AT-EXIT ;


\ ARMED is set only after REGISTER completes, so a refused one is retried.
: ARM ( -- )
   ARMED @ 0<> if exit then
   REGISTER
   1 ARMED ! ;


: MAP-REFUSED ( n -- )
   {: code:n :}
   code 0= if exit then
   code E-MEM-MAP = if LOST PHASE ! exit then
   code throw ;


: ROW-READY ( -- )
   ROW @ NULL-PTR <> if ROW @ 0 VEC:RESIZE exit then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ROW !
   ROW @ START-CAP VEC:RESERVE ;


: STAMP ( -- )
   ROW @ {: v:^ :}
   v VEC:COUNT {: old:n :}
   v old DATE:TIME-LEN + VEC:ENSURE
   epoch-seconds v VEC:SPAN old SPAN:SKIP SPAN:$ DATE:FORMAT-EPOCH-UTC 2drop ;


: OPEN ( -- )
   ARM
   ROW-READY
   STAMP ;


: LEVEL$ ( level -- str )
   LEVEL>N {: n:n :}
   n ERROR-N = if s" error" exit then
   n WARN-N = if s" warn" exit then
   s" info" ;


: ESCAPE? ( n -- bool )
   {: b:n :}
   b SPACE-B < b DEL-B = or b BACKSLASH = or ;


: HEX+ ( n -- )  s" 0123456789abcdef" drop + c@ ROW @ VEC:PUSH c! ;


: ESCAPE+ ( n -- )
   {: b:n :}
   BACKSLASH ROW @ VEC:PUSH c!
   [char] x ROW @ VEC:PUSH c!
   b 4 rshift HEX+
   b $F and HEX+ ;


\ Appends at[from, to).
: RUN+ ( ^u8 n n -- )
   {: at:^ from:n to:n :}
   at from + to from - ROW @ VEC:APPEND ;


\ TEXT+'s contract: the row's first growth, to hold the text unescaped, comes
\ before any byte of the text is read. The text must not lie in the row: that
\ growth and each escape's can move the block before the text is read.
: ESCAPED+ ( str -- )
   {: at:^ u:n :}
   ROW @ dup VEC:COUNT u + VEC:RESERVE
   0 u 0 ?do
      at i + c@ ESCAPE? if
         at over i RUN+
         at i + c@ ESCAPE+
         drop i 1+
      then
   loop
   at swap u RUN+ ;


: LF$ ( -- str )  S\" \n" ;


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
   [: MENDED? if FD:STDERR ROW @ VEC:SPAN SPAN:$ FD:TRY-WRITE-FULL WROTE then ;]
   FD:WITH-NOSIGPIPE ;


: NOTICE-OUT ( -- )
   epoch-seconds NOTICE DATE:TIME-LEN DATE:FORMAT-EPOCH-UTC 2drop
   FD:STDERR [: MENDED? if FD:STDERR NOTICE NOTICE-U FD:TRY-WRITE-FULL WROTE then ;]
   FD:WITH-NOSIGPIPE ;


\ A line is lost when fd 2's SIGPIPE hold is refused; a write fd 2 refuses is
\ answered by TAIL's machine (WROTE, MEND). The flag is held only for the
\ write: other processes share fd 2's file description and its flag, and a hold
\ of theirs that overlaps this one can undo it.
: OUT ( [ -- ] -- )
   catch {: code:n :}
   code 0= code E-PROC-OUTPUT = or if exit then
   code throw ;


: WRITE ( [ -- ] -- )
   LOCK TASK:GET
   [: OUT ;] [: LOCK TASK:RELEASE ;] finally ;


: FINISH ( -- )
   PHASE @ CLOSED = if exit then
   PHASE @ OPENED = if [: LF ROW @ VEC:PUSH c! ;] catch MAP-REFUSED then
   PHASE @ LOST = if [: NOTICE-OUT ;] WRITE exit then
   [: ROW-OUT ;] WRITE ;


: SHED ( -- )
   ROW @ NULL-PTR = if exit then
   ROW @ VEC:CAP START-CAP <= if exit then
   ROW-FREE ;

public

: ERROR ( -- level )  ERROR-N >LEVEL ;
: WARN ( -- level )  WARN-N >LEVEL ;
: INFO ( -- level )  INFO-N >LEVEL ;


: TEXT+ ( str -- )
   PHASE @ OPENED <> if 2drop exit then
   [: 2dup ESCAPED+ ;] catch >r 2drop r> MAP-REFUSED ;


: INT+ ( n -- )  FMT:INT$ TEXT+ ;


\ A code and the name and text declared for it:
\ `-9701 MODEL:E-KEY: the provider refused the key`, the number alone when none.
: CODE+ ( n -- ) {: code:n :}
   code INT+
   code ERROR:NAME$ {: name:^ nameu:n :}
   nameu 0= if exit then
   s"  " TEXT+ name nameu TEXT+ s" : " TEXT+ code ERROR:TEXT$ TEXT+ ;


: LINE ( level str -- )
   {: level:level tag:^ tagu:n :}
   CLOSED PHASE !
   [: OPEN  OPENED PHASE ! ;] catch MAP-REFUSED
   s"  " TEXT+  level LEVEL$ TEXT+  s"  " TEXT+
   tag tagu TEXT+  s" : " TEXT+ ;


: ;LINE ( -- )
   [: FINISH ;] [: CLOSED PHASE !  SHED ;] finally ;

;package
