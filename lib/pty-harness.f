\ pty-harness.f - the pseudo-terminal harness the engine's PTY suites drive.
\
\ A suite hands an executable path to SPAWN-ON-PTY, types at the master with
\ SEND / SEND-LINE / SEND-BYTE, and reads the child back through the waits
\ below. The pair itself comes from lib/pty.f, which every pty opener in the tree
\ shares; lib/process-pty-io.f supervises a linear handle over one, and this
\ module is the third shape - a child engine on the other side of a terminal,
\ read into one buffer a suite makes claims about.
\
\ STORAGE CLASS. PROCESS-WIDE. The master, the child pid, the read buffer, the
\ absence window, the wait deadline and the watch table are one set of cells, so
\ one child and one wait run at a time; SPAWN-ON-PTY refuses a second pair while
\ one is open. The reading words take a descriptor, so the same buffer also
\ serves a suite's pipe children.
\
\ READING. READ-STEP is one poll and, when the descriptor has bytes, one read. A
\ poll that reports no readiness is quiet, an errno included: the descriptors are
\ blocking, so a read no poll authorised could block past the caller's deadline.
\ A ready descriptor that reads nothing is the far side's hang-up and is
\ reported, never counted as quiet.
\
\ REAPING. A child on a terminal nobody reads blocks in write() once the
\ terminal's buffer fills, so a reap that only waits waits for a child that is
\ waiting for it: measured, 5 m 34 s in do_wait before a hand kill. REAP reads
\ the master until the child hangs up, waits out what is left of the budget on
\ the child's lifetime watch, and on expiry kills the child and names it with the
\ bytes it left unread. PROC-WAIT-RC is still the unbounded wait for callers that
\ want one.
\
\ CLAIMS. A full buffer keeps its tail, so a scan of BUF$ cannot answer a
\ question about bytes a compaction dropped. Two shapes answer a negative claim
\ honestly: WAIT-BARRIER closes a window at a marker the child printed PAST the
\ point where the rejected text would have appeared, and WINDOW-ABSENT? reads
\ only that window; WATCH+ records the fact as the bytes arrive, so a compaction
\ cannot erase it.

require lib/errors.f
require lib/string.f
require lib/type/deftype.f
require lib/process.f
require lib/pty.f

package PTY-HARNESS
public

DEFTYPE WATCH                      \ a registered never-seen needle

$4E20 constant WAIT-BUDGET-MS      \ one wait or drain runs at most this long on the clock

private

10 constant POLL-MS                \ one poll blocks at most this long
$4000 constant BUF-CAP
$200 constant TAIL-KEEP            \ bytes kept when a full buffer is compacted
$10 constant WATCH-CAP             \ registered never-seen needles
$40 constant NEEDLE-CAP            \ bytes per needle
$400 constant PATH-CAP             \ supervised executable path bytes
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
variable DEADLINE                  \ absolute monotonic end of the wait in flight
variable QUIET                     \ polls in a row that brought nothing
variable PATH-U
variable WATCH-N


: NEEDLE-LEN-AT ( n -- ptr n ) {: w:n :}
   w cells NEEDLE-LEN + cell-view ;


: NEEDLE-SEEN-AT ( n -- ptr n ) {: w:n :}
   w cells NEEDLE-SEEN + cell-view ;


: NEEDLE$ ( n -- ptr u8 n ) {: w:n :}
   NEEDLE w NEEDLE-CAP * +  w NEEDLE-LEN-AT @ ;


\ Where a needle that straddles the append could start: the new bytes plus the
\ tail of the old ones a match could have begun in.
: SCAN-START ( n n -- n ) {: old:n w:n :}
   old  w NEEDLE-LEN-AT @ 1 -  - dup 0 < if drop 0 then ;


: SCAN-ONE ( n n -- ) {: old:n w:n :}
   w NEEDLE-SEEN-AT @ 0 <> if exit then
   old w SCAN-START {: from:n :}
   RBUF from +  RN @ from -  w NEEDLE$ CONTAINS? if 1 w NEEDLE-SEEN-AT ! then ;


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


: OPEN-PAIR ( -- )
   PTYNAME PTY:SLAVE-PATH-CAP PTY:OPEN drop PTY:MASTER>N {: m:n :}
   m MFD !
   m >FD FD-CLOEXEC!
   OPEN-SLAVE ;


: CLOSE-FD! ( ptr n -- ) {: slot:ptr :}
   slot @ 0 < if exit then
   slot @ close
   -1 slot ! ;

public

: BUF$ ( -- ptr u8 n )
   RBUF RN @ ;


\ Clearing retires the barrier with it: the bytes a claim would have read are
\ gone. A watch fact is not cleared - it is what survives this.
: BUF-CLEAR ( -- )
   0 RN !
   -1 WEND ! ;


\ Write room for a reader this module does not own, compacted first so the span
\ is never empty; the reader hands back what it wrote with TOOK.
: ROOM$ ( -- ptr u8 n )
   KEEP-TAIL!
   RBUF RN @ +  BUF-CAP RN @ - ;


: TOOK ( n -- ) {: got:n :}
   RN @ {: old:n :}
   old got + RN !
   old SCAN-WATCHES ;

private

\ Append one read of fd and report it, keeping room for the next one.
: READ-CHUNK ( fd -- n ) {: f:fd :}
   ROOM$ {: room:ptr cap:n :}
   f FD>N room cap read {: got:n :}
   got 0 > if got TOOK then
   got ;

public

\ One poll and, when the descriptor has bytes, one read: above zero for bytes
\ appended, 0 for a quiet poll (an errno included), below zero once the far side
\ is gone - a ready descriptor that reads nothing is the hang-up.
: READ-STEP ( fd -- n ) {: f:fd :}
   f POLL-MS >MS POLL-IN COUNT>N 0 <= if 0 exit then
   f READ-CHUNK 0 > if 1 exit then
   -1 ;


\ A pipe hands over what the writer has flushed, not what it will write. Read to
\ the far end's close, so a child that answers in pieces is never truncated into
\ a wrong verdict by one short read.
: READ-TO-EOF ( fd -- ) {: f:fd :}
   begin f READ-CHUNK 0 > while repeat ;


: WAIT-OPEN ( n -- ) {: ms:n :}
   ms >MS PROC-DEADLINE-AT DEADLINE ! ;


: WAIT-LEFT ( -- n )
   DEADLINE @ PROC-LEFT-MS MS>N ;


\ Read fd until `quiet` polls in a row bring nothing, the far side hangs up, or
\ the wait budget passes. The deadline is absolute and is tested before each
\ poll, so a drain overruns it by at most the one poll already in flight. What
\ the drain read stays in the buffer; a caller that wants it gone clears.
: DRAIN ( fd n -- ) {: f:fd quiet:n :}
   WAIT-BUDGET-MS WAIT-OPEN
   0 QUIET !
   begin QUIET @ quiet <  WAIT-LEFT 0 >  and while
      f READ-STEP {: n:n :}
      n 0 < if exit then
      n 0 > if 0 QUIET ! else QUIET @ 1 + QUIET ! then
   repeat ;


\ A short write is an error, never a silent truncation.
: WRITE-ALL ( fd ptr u8 n -- ) {: f:fd a:ptr u:n :}
   f FD>N a u write u <> if E-PTY-IO throw then ;


: WRITE-BYTE ( fd n -- ) {: f:fd c:n :}
   c OUTB c!
   f OUTB 1 WRITE-ALL ;


: WRITE-LINE ( fd ptr u8 n -- ) {: f:fd a:ptr u:n :}
   f a u WRITE-ALL
   f LF WRITE-BYTE ;


: MASTER-FD ( -- fd )
   MFD @ >FD ;


: CHILD-PID ( -- pid )
   KID @ >PID ;


: SEND ( ptr u8 n -- ) {: a:ptr u:n :}
   MASTER-FD a u WRITE-ALL ;


: SEND-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   MASTER-FD a u WRITE-LINE ;


: SEND-BYTE ( n -- ) {: c:n :}
   MASTER-FD c WRITE-BYTE ;


: CLOSE-MASTER ( -- )
   MFD CLOSE-FD! ;


private

\ Read the master until the child hangs up or the clock runs out. A child that
\ fills the terminal blocks in write() until someone reads it, so the wait that
\ follows a reap has to be preceded by the reading, not the other way round.
\ The deadline is absolute, so this overruns it by at most the poll in flight.
: DRAIN-TO-HANGUP ( -- bool )
   begin
      WAIT-LEFT 0= if false exit then
      MASTER-FD READ-STEP 0 < if true exit then
   again ;


\ One poll-authorised read of what is still queued at the master, and whether
\ there may be more. The far side is gone by the time this runs, so a ready
\ descriptor that reads nothing ends the count instead of blocking.
: LEFTOVER-STEP ( n -- n bool )
   MASTER-FD POLL-MS >MS POLL-IN COUNT>N 0 <= if false exit then
   MASTER-FD READ-CHUNK dup 0 <= if drop false exit then
   + true ;


\ Bytes the child left at the master: what a blocked write was waiting for
\ someone to take. Only meaningful once the child is dead.
: LEFTOVER ( -- n )
   0 begin LEFTOVER-STEP 0= if exit then again ;


: KILL-REAP ( pid -- ) {: p:pid :}
   p SIGKILL PROC-KILL-RAW drop
   p PROC-WAIT-STATUS drop ;


\ True once the watch reports the exit within ms. A signal that lands mid-wait
\ restarts against the same deadline, so no signal storm shortens the budget.
: WATCH-READY? ( fd n -- bool ) {: w:fd ms:n :}
   w POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;


\ A child that has hung up is not yet a child that has exited: the terminal can
\ close before the process does. Its lifetime watch answers that question inside
\ the budget. A host that refuses a watch for a process that has ALREADY exited
\ (macOS cannot register a dead one; test/proc-watch-smoke.f pins both arms)
\ answers it the same way, because that is the case whose wait returns at once.
: EXIT-READY? ( pid n -- bool ) {: p:pid ms:n :}
   p PID>N proc-watch-open {: w:n :}
   w 0 < if true exit then
   w >FD ms WATCH-READY? {: ready:bool :}
   w close
   ready ;


\ The expiry: name the child that did not exit, kill it, and answer the killed
\ outcome, which reds a case that wanted a clean exit instead of hanging it.
: KILL-EXPIRED ( pid n -- outcome ) {: p:pid ms:n :}
   s" pty reap: no exit within ms " type ms .
   s" pty reap: killed pid " type p PID>N .
   p KILL-REAP
   OUTCOME:TIMEOUT ;

public

\ Reap a child that is no longer writing to us, bounded by ms: the exit if it
\ comes, the killed outcome if the budget runs out first. Never an unbounded
\ wait - that is PROC-WAIT-RC's contract, not this one's.
: WAIT-EXIT ( pid n -- outcome ) {: p:pid ms:n :}
   p ms EXIT-READY? 0= if p ms KILL-EXPIRED exit then
   p PROC-WAIT-OUTCOME ;

private

\ The wedge: the child still held the terminal when the clock ran out. Kill it
\ first, so the count below ends, then name the bytes it left for a reader that
\ never came.
: WEDGE-OUTCOME ( n -- outcome ) {: ms:n :}
   CHILD-PID ms KILL-EXPIRED
   s" pty reap: bytes still unread at the master " type LEFTOVER .
   -1 KID ! ;

public

\ Reap the pty child within ms. The master is read to the hang-up first, so the
\ child is never blocked writing at a terminal nobody empties, and what is left
\ of the budget waits out the exit itself.
: REAP-WITHIN ( n -- outcome ) {: ms:n :}
   CHILD-PID PID>N 0 <= if E-PTY-IO throw then
   ms WAIT-OPEN
   DRAIN-TO-HANGUP 0= if ms WEDGE-OUTCOME exit then
   CHILD-PID WAIT-LEFT WAIT-EXIT
   -1 KID ! ;


: REAP ( -- outcome )
   WAIT-BUDGET-MS REAP-WITHIN ;


private

\ The transaction below cannot read a local, so the path it execs travels
\ through storage.
: STORE-PATH ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if E-PTY-CAPACITY throw then
   a PATH-BUF u BYTE-COPY
   u PATH-U ! ;


\ Open the pair and start the child on its slave. Every throw here leaves both
\ ends to the abort path: the child owns the slave only once the spawn returned.
: SPAWN-BUILD ( -- )
   OPEN-PAIR
   PATH-BUF PATH-U @ >LEN SFD @ >FD SFD @ >FD SFD @ >FD PROC-SPAWN-IO PID>N KID !
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
: SPAWN-ON-PTY ( ptr u8 n -- )
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
: FIND-FROM ( n ptr u8 n -- n ) {: from:n a:ptr u:n :}
   from 0 < from RN @ > or if -1 exit then
   RBUF from +  RN @ from -  a u FIND-SUB MATCH option
     none OF -1 ENDOF
     some OF IDX>N from + ENDOF
   ;MATCH ;


: IN-BUF? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   RBUF RN @ a u CONTAINS? ;


\ Offset of the tail at or after the END of the first head, or -1: the two
\ markers in that order, which is what tells a prompt the child printed after its
\ answer from one the line editor redrew before it. An empty head puts the tail
\ anywhere.
: FIND-AFTER ( ptr u8 n ptr u8 n -- n ) {: ha:ptr hu:n ta:ptr tu:n :}
   0 ha hu FIND-FROM {: at:n :}
   at 0 < if -1 exit then
   at hu + ta tu FIND-FROM ;


: AFTER? ( ptr u8 n ptr u8 n -- bool )
   FIND-AFTER 0 >= ;


\ Keep reading the master until the text appears, the child hangs up, or the
\ wait budget passes. A child engine answers in as many pieces as the host's
\ scheduling chooses - its line editor redraws on every keystroke, and a loaded
\ box splits one echo across dozens of reads - so no count of polls bounds the
\ wait. A partial read is not an answer and not a failure: only the text, the
\ hang-up and the clock end the loop.
: WAIT-FOR ( ptr u8 n -- bool ) {: a:ptr u:n :}
   WAIT-BUDGET-MS WAIT-OPEN
   begin
      a u IN-BUF? if true exit then
      WAIT-LEFT 0= if false exit then
      MASTER-FD READ-STEP 0 < if a u IN-BUF? exit then
   again ;


\ Wait for a marker and close an absence claim's window at its end. The marker
\ has to be one the child prints PAST the point where the rejected text could
\ have appeared; a failed wait leaves no window, so the claim behind it is
\ refused as well.
: WAIT-BARRIER ( ptr u8 n -- bool ) {: a:ptr u:n :}
   -1 WEND !
   a u WAIT-FOR 0= if false exit then
   0 a u FIND-FROM {: at:n :}
   at 0 < if false exit then
   at u + WEND !
   true ;


\ An absence claim reads the window a barrier closed, never the whole buffer, so
\ it reads the child's answer instead of a buffer that is merely still empty.
\ With no window there is no claim and the answer is false.
: WINDOW-ABSENT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   WEND @ 0 < if false exit then
   RBUF WEND @ a u CONTAINS? 0= ;


\ Register a needle the reads from here on are asked about. The fact survives
\ every compaction and every BUF-CLEAR, which is what a negative claim over a
\ whole run needs; a match that straddles the registration point counts as seen,
\ so the fact can only refuse a claim, never grant one wrongly.
: WATCH+ ( ptr u8 n -- watch ) {: a:ptr u:n :}
   WATCH-N @ WATCH-CAP >= if E-PTY-CAPACITY throw then
   u 0= u NEEDLE-CAP > or if E-PTY-CAPACITY throw then
   WATCH-N @ {: w:n :}
   a  NEEDLE w NEEDLE-CAP * +  u BYTE-COPY
   u w NEEDLE-LEN-AT !
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
