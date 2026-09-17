\ pty-harness.f - the pseudo-terminal harness the engine's PTY suites drive.
\
\ A suite hands an executable path to SPAWN-ON-PTY, types at the master with
\ SEND / SEND-LINE / SEND-BYTE, and reads the child back through the waits
\ below. lib/pty.f opens a pair for a device peer and lib/process-pty-io.f
\ supervises a linear handle; this module is the third shape - a child engine on
\ the other side of a terminal, read into one buffer a suite makes claims about.
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

package PTY-HARNESS
public

DEFTYPE WATCH                      \ a registered never-seen needle

$4E20 constant WAIT-BUDGET-MS      \ one wait or drain runs at most this long on the clock

private

2 constant O-RDWR
$20000 constant O-NOCTTY           \ the engine's portable O_NOCTTY bit (src/os/linux/sys.f OS-OPEN-FLAGS)
O-RDWR O-NOCTTY or constant O-SLAVE   \ the slave must never become this process's controlling terminal
$40045431 constant TIOCSPTLCK-LINUX
$80045430 constant TIOCGPTN-LINUX
$20007454 constant TIOCPTYGRANT-MACOS
$20007452 constant TIOCPTYUNLK-MACOS
$40807453 constant TIOCPTYGNAME-MACOS
10 constant POLL-MS                \ one poll blocks at most this long
$4000 constant BUF-CAP
$200 constant TAIL-KEEP            \ bytes kept when a full buffer is compacted
$80 constant NAME-CAP              \ slave path bytes + NUL (Darwin fills it, Linux builds it)
$10 constant WATCH-CAP             \ registered never-seen needles
$40 constant NEEDLE-CAP            \ bytes per needle
10 constant LF

create RBUF BUF-CAP allot
create PTYNAME NAME-CAP allot
create OUTB 1 allot                \ the one byte SEND-BYTE and SEND-LINE write
create NEEDLE WATCH-CAP NEEDLE-CAP * allot
create NEEDLE-LEN WATCH-CAP cells allot
create NEEDLE-SEEN WATCH-CAP cells allot

variable RN                        \ bytes in RBUF
variable NAME-U                    \ slave path length under construction
variable WEND                      \ end of the window an absence claim reads; -1 with no barrier
variable MFD                       \ the master; -1 with no child
variable SFD                       \ the slave, only between opening it and the spawn
variable KID                       \ the pty child; -1 with no child
variable PTYNUM
variable DEADLINE                  \ absolute monotonic end of the wait in flight
variable QUIET                     \ polls in a row that brought nothing
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


: PATH-C ( n -- ) {: c:n :}
   NAME-U @ NAME-CAP >= if E-PTY-CAPACITY throw then
   c PTYNAME NAME-U @ + c!
   NAME-U @ 1 + NAME-U ! ;


: PATH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ PATH-C loop ;


: PATH-U+ ( n -- ) {: v:n :}
   v 10 >= if v 10 / recurse then
   v 10 mod 48 + PATH-C ;


: SLAVE-PATH! ( n -- ) {: num:n :}
   0 NAME-U !
   s" /dev/pts/" PATH+
   num PATH-U+
   0 PATH-C ;


: OPEN-MASTER ( -- )
   s" /dev/ptmx" >LEN PROC-PATHZ O-RDWR 0 open {: m:n :}
   m 0 < if E-PTY-OPEN throw then
   m MFD !
   m >FD FD-CLOEXEC! ;


: OPEN-SLAVE ( -- )
   PTYNAME O-SLAVE 0 open {: s:n :}
   s 0 < if E-PTY-OPEN throw then
   s SFD ! ;


: IOCTL-CK ( n -- )
   0 <> if E-PTY-IOCTL throw then ;


: OPEN-PAIR-LINUX ( -- )
   OPEN-MASTER
   0 PTYNUM !
   MFD @ TIOCSPTLCK-LINUX PTYNUM ioctl IOCTL-CK
   MFD @ TIOCGPTN-LINUX PTYNUM ioctl IOCTL-CK
   PTYNUM @ SLAVE-PATH!
   OPEN-SLAVE ;


: OPEN-PAIR-MACOS ( -- )
   OPEN-MASTER
   MFD @ TIOCPTYGRANT-MACOS NULL$ drop ioctl IOCTL-CK
   MFD @ TIOCPTYUNLK-MACOS NULL$ drop ioctl IOCTL-CK
   MFD @ TIOCPTYGNAME-MACOS PTYNAME ioctl IOCTL-CK
   OPEN-SLAVE ;


: OPEN-PAIR ( -- )
   HB-TARGET-LINUX? if OPEN-PAIR-LINUX exit then
   HB-TARGET-MACOS? if OPEN-PAIR-MACOS exit then
   E-PROC-HOST throw ;


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


\ Open a pseudo-terminal pair, start the executable with the slave as its whole
\ terminal - input, output and diagnostics - and keep the master. The slave is
\ closed here: the child holds the only copies, so its exit hangs the master up.
: SPAWN-ON-PTY ( ptr u8 n -- ) {: a:ptr u:n :}
   MFD @ 0 >= if E-PTY-IO throw then
   -1 SFD !
   OPEN-PAIR
   a u >LEN SFD @ >FD SFD @ >FD SFD @ >FD PROC-SPAWN-IO PID>N KID !
   KID @ 0 <= if E-PTY-IO throw then
   SFD CLOSE-FD!
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
