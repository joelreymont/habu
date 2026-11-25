\ pty-harness.f - the pseudo-terminal harness the engine's PTY suites drive: a
\ child engine on the other side of a terminal (the pair from src/lib/pty.f),
\ read into one buffer a suite makes claims about.
\ STORAGE CLASS. PROCESS-WIDE: one child and one wait at a time; SPAWN-ON-PTY
\ refuses a second pair while one is open. The reading words take a descriptor,
\ so the same buffer also serves a suite's pipe children.

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/type/deftype.f
require src/lib/process.f
require src/lib/fd-lock.f
require src/lib/pty.f
require src/lib/time.f

package PTY-HARNESS
public

DEFTYPE WATCH                      \ a registered never-seen needle

private

10 constant POLL-MS                \ one poll blocks at most this long
60000000000 constant WAIT-NS       \ 60 s: one wait gives up after this long
$4000 constant BUF-CAP
$200 constant TAIL-KEEP            \ bytes kept when a full buffer is compacted
$10 constant WATCH-CAP             \ registered never-seen needles
$40 constant NEEDLE-CAP            \ bytes per needle
10 constant LF

create RBUF BUF-CAP allot
create PTYNAME PTY:SLAVE-PATH-CAP allot
create OUTB 1 allot                \ the one byte SEND-BYTE and SEND-LINE write
create PATH-BUF PATH-CAP allot     \ the path the spawn transaction execs
create NEEDLE WATCH-CAP NEEDLE-CAP * allot
create NEEDLE-LEN WATCH-CAP cells allot
create NEEDLE-SEEN WATCH-CAP cells allot

variable RN                        \ bytes in RBUF
variable WEND                      \ end of the window an absence claim reads; -1 with no barrier
variable MFD                       \ the master; -1 with no child
variable SFD                       \ the slave, only between opening it and the spawn
variable KID                       \ the pty child; -1 with no child
variable PATH-U
variable WATCH-N


: NEEDLE-LEN-AT ( n -- ^n ) {: w:n :}
   w cells NEEDLE-LEN + cell-view ;


: NEEDLE-SEEN-AT ( n -- ^n ) {: w:n :}
   w cells NEEDLE-SEEN + cell-view ;


: NEEDLE$ ( n -- str ) {: w:n :}
   NEEDLE w NEEDLE-CAP * +  w NEEDLE-LEN-AT @ >str ;


\ Where a needle that straddles the append could start: the new bytes plus the
\ tail of the old ones a match could have begun in.
: SCAN-START ( n n -- n ) {: old:n w:n :}
   old  w NEEDLE-LEN-AT @ 1 -  - dup 0 < if drop 0 then ;


: SCAN-ONE ( n n -- ) {: old:n w:n :}
   w NEEDLE-SEEN-AT @ 0 <> if exit then
   old w SCAN-START {: from:n :}
   RBUF from +  RN @ from - >str  w NEEDLE$ STR:CONTAINS? if 1 w NEEDLE-SEEN-AT ! then ;


\ Every watched needle is asked of the bytes appended since `old`, so the fact
\ outlives the compaction that will drop them.
: SCAN-WATCHES ( n -- ) {: old:n :}
   WATCH-N @ 0 ?do old i SCAN-ONE loop ;


\ A full buffer keeps its tail rather than dropping everything: a marker split
\ across the compaction survives whole in what is kept, and every older byte has
\ already been searched and offered to every watch. The barrier window retires
\ with the dropped bytes, so a claim behind it is refused rather than answered
\ from a buffer that lost the evidence.
: KEEP-TAIL! ( -- )
   RN @ BUF-CAP TAIL-KEEP - < if exit then
   RBUF RN @ TAIL-KEEP - +  RBUF TAIL-KEEP BYTE-COPY
   TAIL-KEEP RN !
   -1 WEND ! ;


\ The slave is opened here, never by PTY:OPEN: it must never become this
\ process's controlling terminal, and it is the child's to hold from the spawn on.
: OPEN-SLAVE ( -- )
   PTYNAME PTY:PTY-OPEN-FLAGS 0 open {: s:n :}
   s 0 < if E-PTY-OPEN throw then
   s SFD ! ;


: OPEN-MASTER-HELD ( -- )
   PTYNAME PTY:SLAVE-PATH-CAP PTY:OPEN drop PTY:MASTER>N {: m:n :}
   m MFD !
   m >FD FD:FD-CLOEXEC! ;


: OPEN-PAIR ( -- )
   [: OPEN-MASTER-HELD ;] FD-LOCK:WITH
   OPEN-SLAVE ;


: CLOSE-FD! ( ^n -- ) {: slot:^ :}
   slot @ 0 < if exit then
   slot @ close
   -1 slot ! ;

public

: BUF$ ( -- str )
   RBUF RN @ >str ;


\ Clearing retires the barrier with it: the bytes a claim would have read are
\ gone. A watch fact is not cleared - it is what survives this.
: BUF-CLEAR ( -- )
   0 RN !
   -1 WEND ! ;


private

\ Write room after the bytes, compacted first so the span is never empty; the
\ writer hands back what it wrote with TOOK.
: ROOM$ ( -- str )
   KEEP-TAIL!
   RBUF RN @ +  BUF-CAP RN @ - >str ;


\ The count is what the writer wrote into ROOM$: never negative, never past the
\ room, so the cursor never moves back or past the buffer.
: TOOK ( n -- ) {: got:n :}
   RN @ {: old:n :}
   got 0 < got BUF-CAP old - > or if E-PTY-CAPACITY throw then
   old got + RN !
   old SCAN-WATCHES ;


: READ-CHUNK ( fd -- n ) {: f:fd :}
   ROOM$ {: room:str :}
   f FD>N room STR:DATA room STR:LEN read {: got:n :}
   got 0 > if got TOOK then
   got ;

public

\ Bytes a reader this module does not own read, appended after ROOM$'s
\ compaction, which leaves room for more than TAIL-KEEP bytes; more than the
\ room holds is E-PTY-CAPACITY.
: APPEND ( str -- ) {: s:str :}
   ROOM$ {: room:str :}
   s STR:LEN room STR:LEN > if E-PTY-CAPACITY throw then
   s STR:DATA room STR:DATA s STR:LEN BYTE-COPY
   s STR:LEN TOOK ;


\ One poll and, when the descriptor has bytes, one read: above zero for bytes
\ appended, 0 for a quiet poll (an errno included), below zero once the far side
\ is gone - a ready descriptor that reads nothing is the hang-up.
: READ-STEP ( fd -- n ) {: f:fd :}
   f POLL-MS >MS PROCESS:POLL-IN COUNT>N 0 <= if 0 exit then
   f READ-CHUNK 0 > if 1 exit then
   -1 ;


\ A pipe hands over what the writer has flushed, not what it will write. Read to
\ the far end's close, so a child that answers in pieces is never truncated into
\ a wrong verdict by one short read.
: READ-TO-EOF ( fd -- ) {: f:fd :}
   begin f READ-CHUNK 0 > while repeat ;


\ A short write is an error, never a silent truncation. A negative length is
\ refused before the write, whose -1 error answer would equal a length of -1.
: WRITE-ALL ( fd str -- ) {: f:fd s:str :}
   s STR:LEN 0 < if E-PTY-CAPACITY throw then
   f FD>N s write s STR:LEN <> if E-PTY-IO throw then ;


: WRITE-BYTE ( fd n -- ) {: f:fd c:n :}
   c OUTB c!
   f OUTB 1 >str WRITE-ALL ;


: WRITE-LINE ( fd str -- ) {: f:fd s:str :}
   f s WRITE-ALL
   f LF WRITE-BYTE ;


: MASTER-FD ( -- fd )
   MFD @ >FD ;


: CHILD-PID ( -- pid )
   KID @ >PID ;


: SEND ( str -- ) {: s:str :}
   MASTER-FD s WRITE-ALL ;


: SEND-LINE ( str -- ) {: s:str :}
   MASTER-FD s WRITE-LINE ;


: SEND-BYTE ( n -- ) {: c:n :}
   MASTER-FD c WRITE-BYTE ;


: CLOSE-MASTER ( -- )
   MFD CLOSE-FD! ;


private

: DRAIN-TO-HANGUP ( -- )
   begin MASTER-FD READ-STEP 0 >= while repeat ;

public

\ Reap the pty child. A child on a terminal nobody reads blocks in write() once
\ the terminal's buffer fills, so the master is read to the hang-up before the
\ wait.
: REAP ( -- PROCESS:ending )
   CHILD-PID PID>N 0 <= if E-PTY-IO throw then
   DRAIN-TO-HANGUP
   CHILD-PID PROCESS:WAIT PROCESS:STATUS>ENDING
   -1 KID ! ;


private

\ The transaction below cannot read a local, so the path it execs travels
\ through storage.
: STORE-PATH ( str -- ) {: s:str :}
   s STR:LEN 0 < s STR:LEN PATH-CAP > or if E-PTY-CAPACITY throw then
   s STR:DATA PATH-BUF s STR:LEN BYTE-COPY
   s STR:LEN PATH-U ! ;


\ Open the pair and start the child on its slave. Every throw here leaves both
\ ends to the abort path: the child owns the slave only once the spawn returned.
: SPAWN-BUILD ( -- )
   OPEN-PAIR
   PATH-BUF PATH-U @ >str SFD @ >FD SFD @ >FD SFD @ >FD PROCESS:SPAWN-IO PID>N KID !
   KID @ 0 <= if E-PTY-IO throw then
   SFD CLOSE-FD! ;


\ Close what the build opened and forget the child it never started, so the next
\ spawn opens its own pair instead of refusing over a dead one.
: SPAWN-CLEAN ( -- )
   SFD CLOSE-FD!
   MFD CLOSE-FD!
   -1 KID ! ;

public

\ Open a pseudo-terminal pair, start the executable with the slave as its whole
\ terminal - input, output and diagnostics - and keep the master. The slave is
\ closed here: the child holds the only copies, so its exit hangs the master up.
\ A failed spawn throws what failed and leaves nothing open.
: SPAWN-ON-PTY ( str -- )
   MFD @ 0 >= if E-PTY-IO throw then
   -1 SFD !
   STORE-PATH
   [: SPAWN-BUILD ;] catch dup 0 <> if
      SPAWN-CLEAN
      throw
   then
   drop
   BUF-CLEAR ;


\ Offset of the first occurrence at or after `from` in the bytes read so far,
\ or -1.
: FIND-FROM ( n str -- n ) {: from:n s:str :}
   from 0 < from RN @ > or if -1 exit then
   RBUF from +  RN @ from - >str  s STR:FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N from + ENDOF
   ;MATCH ;


: IN-BUF? ( str -- bool ) {: s:str :}
   RBUF RN @ >str s STR:CONTAINS? ;


\ Offset of the tail at or after the END of the first head, or -1: the two
\ markers in that order, which is what tells a prompt the child printed after its
\ answer from one the line editor redrew before it. An empty head puts the tail
\ anywhere.
: FIND-AFTER ( str str -- n ) {: hs:str ts:str :}
   0 hs FIND-FROM {: at:n :}
   at 0 < if -1 exit then
   at hs STR:LEN + ts FIND-FROM ;


: AFTER? ( str str -- bool )
   FIND-AFTER 0 >= ;


private

\ Read until the quotation holds; a hang-up or the deadline answers its last
\ answer.
: WAIT-UNTIL ( [ -- bool ] -- bool )
   TIME:MONO-NS WAIT-NS + {: end:n :}
   begin
      dup execute if drop true exit then
      MASTER-FD READ-STEP 0 < if execute exit then
      TIME:MONO-NS end > if execute exit then
   again ;

public

\ The editor redraws "habu> " per key, so only a prompt after the answer shows
\ the child raw again; a ^D sent before it is read cooked and dropped as a NUL.
: WAIT-AFTER ( str str -- bool ) {: hs:str ts:str :}
   [: hs ts AFTER? ;] WAIT-UNTIL ;


\ The text anywhere: an empty head puts the tail anywhere.
: WAIT-FOR ( str -- bool ) {: s:str :}
   s" " s WAIT-AFTER ;


\ The answer is the line just before the editor redraws its prompt: neither
\ the echo nor the line's own output can end there.
: WAIT-ANSWER ( -- bool )
   s\"  ok\r\n\r\x1B[Khabu> " {: s:str :}
   [: s IN-BUF?  s\" ?\r\n\r\x1B[Khabu> " IN-BUF? or ;]
   WAIT-UNTIL
   s IN-BUF? and ;


\ Wait for a marker and close an absence claim's window at its end. The marker
\ has to be one the child prints PAST the point where the rejected text could
\ have appeared. The window retires before the wait, so a wait the child hangs
\ up on leaves none, and the claim behind it is refused as well.
: WAIT-BARRIER ( str -- bool ) {: s:str :}
   -1 WEND !
   s WAIT-FOR 0= if false exit then
   0 s FIND-FROM {: at:n :}
   at 0 < if false exit then
   at s STR:LEN + WEND !
   true ;


\ An absence claim reads the window a barrier closed, never the whole buffer, so
\ it reads the child's answer instead of a buffer that is merely still empty.
\ With no window there is no claim and the answer is false.
: WINDOW-ABSENT? ( str -- bool ) {: s:str :}
   WEND @ 0 < if false exit then
   RBUF WEND @ >str s STR:CONTAINS? 0= ;


\ Register a needle the reads from here on are asked about. The fact survives
\ every compaction and every BUF-CLEAR, which is what a negative claim over a
\ whole run needs; a match that straddles the registration point counts as seen,
\ so the fact can only refuse a claim, never grant one wrongly.
: WATCH+ ( str -- watch ) {: s:str :}
   WATCH-N @ WATCH-CAP >= if E-PTY-CAPACITY throw then
   s STR:LEN 0 <= s STR:LEN NEEDLE-CAP > or if E-PTY-CAPACITY throw then
   WATCH-N @ {: w:n :}
   s STR:DATA  NEEDLE w NEEDLE-CAP * +  s STR:LEN BYTE-COPY
   s STR:LEN w NEEDLE-LEN-AT !
   0 w NEEDLE-SEEN-AT !
   w 1 + WATCH-N !
   w >WATCH ;


: NEVER-SEEN? ( watch -- bool ) {: h:watch :}
   h WATCH>N {: w:n :}
   w 0 < w WATCH-N @ >= or if E-PTY-CAPACITY throw then
   w NEEDLE-SEEN-AT @ 0= ;


: WATCH-RESET ( -- )
   0 WATCH-N ! ;

private

\ Every descriptor cell reads -1 until something opens it, so the abort paths
\ close only what opened and SPAWN-ON-PTY can tell "no child" from fd 0.
: INIT ( -- )
   -1 MFD !
   -1 SFD !
   -1 KID !
   -1 WEND ! ;

INIT

;package
