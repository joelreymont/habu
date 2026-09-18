\ genio.f - generic text I/O devices: EMIT, KEY, ACCEPT and the rest reach a
\ device through a vector table instead of the process terminal, and the
\ current task chooses which device that is.
\
\ STORAGE CLASS. TASK-LOCAL for the state an operation runs through: the
\ current input and output device indices are two per-task cells of the DATA
\ header (GENIO-ABI), and the single-byte scratch and the READ-LINE line buffer
\ are two TASK:+USER rows, so two tasks may each drive their own device. The
\ device table itself - the rows and the eight operations in them - is
\ PROCESS-WIDE and shared, which is what lets a task inherit its creator's
\ devices; installing or removing a device is not a per-task act.
\ See docs/threads.md.
\
\ THE SHAPE (docs/genio.md, docs/socket-models.md section 2). A device is eight
\ operations and one cell of its own data. The terminal is device 0 and is
\ built in; every other device takes a row of a small fixed table, so a device
\ handle is a row number and nothing that routes I/O is ever an address. The
\ current task's input and output devices are two cells of the engine's DATA
\ header (src/habu/layout.f GENIO-ABI), which is what makes them per task: the
\ engine swaps DATA for each task, lib/task.f copies them into a new task's
\ region, and the engine's own output funnel reads the output cell directly.
\
\ THE EIGHT OPERATIONS ARE SHARED CODE. Two TCP devices are the same eight
\ quotations over two different connections, and a quotation cannot capture the
\ device it belongs to, so an operation asks GENIO-ABI:ACTIVE-CELL for its own
\ row and reads its state from there. Every dispatcher below publishes that cell
\ before it executes an operation; the engine funnel publishes it too.
\
\ A WRITE OPERATION MUST NOT THROW. The engine reaches it from inside `emit`
\ and `type`, where a throw would unwind through engine internals with the
\ funnel's re-entrancy guard still set. A device that fails a write latches the
\ failure instead, and the next KEY, READ, ACCEPT or FLUSH on that device raises
\ E-GENIO-IO. Every other operation is called from ordinary Habu code and
\ reports at once.
require lib/errors.f
require lib/prelude.f
require lib/num-types.f
require lib/net/tcp4.f
require lib/type/deftype.f
require lib/image-lifecycle.f
\ A device's write row is stored through QUOTATION-STORAGE:STORE, not `xt!`:
\ see PUBLISH-WRITE below.
require src/core/quotation-storage.f

package GENIO

public

\ A device handle is its row: 0 is the terminal, 1..GENIO-ABI:DEVICES are the
\ registered devices. Like every value nominal the raw converters are not
\ validators; DEVICE, TERMINAL and TCP-DEVICE are the ways to obtain one.
DEFTYPE DEVICE

E-GENIO-OPERAND constant E-OPERAND
E-GENIO-STATE constant E-STATE
E-GENIO-IO constant E-IO
E-GENIO-FULL constant E-FULL

private

GENIO-ABI:DEVICES constant MAX-DEVICES
MAX-DEVICES 1+ constant SLOTS          \ row 0 is the terminal
0 constant TERMINAL-ROW
1 constant STDOUT-FD
0 constant STDIN-FD
$A constant LF

\ FIONREAD, the "how many bytes can I read without waiting" ioctl. The number
\ is the host's, not a wire format; Linux and macOS spell it differently.
$541B constant FIONREAD-LINUX
$4004667F constant FIONREAD-MACOS

SLOTS TYPED-BUFFER OP-EMIT   [ n -- ]
SLOTS TYPED-BUFFER OP-KEY    [ -- n ]
SLOTS TYPED-BUFFER OP-READY  [ -- bool ]
SLOTS TYPED-BUFFER OP-READ   [ ptr u8 n -- n ]
SLOTS TYPED-BUFFER OP-WRITE  [ ptr u8 n -- ]
SLOTS TYPED-BUFFER OP-ACCEPT [ ptr u8 n -- n ]
SLOTS TYPED-BUFFER OP-FLUSH  [ -- ]
SLOTS TYPED-BUFFER OP-CLOSE  [ -- ]
SLOTS TYPED-BUFFER SLOT-STATE n        \ the device's own data (a socket, an fd)
SLOTS TYPED-BUFFER SLOT-FAULT n        \ a latched write failure: 0 = healthy
SLOTS TYPED-BUFFER SLOT-GEN n          \ bumped on close, so a stale handle is caught

\ A ROW IS CLAIMED BY WINNING A COMPARE-AND-SWAP ON ITS OWN CELL, never by a
\ read-then-write another task could interleave with: a task building a device
\ on its own connection is the normal case, and two of them must not be handed
\ the same row. The states are FREE, LIVE and CLOSING, and CLOSING is why two
\ tasks closing one handle cannot both run the close operation and both bump
\ the generation -- exactly one wins the transition, the other is told the
\ handle is gone. The claim publishes the row as taken; its owner fills the
\ operations, the state and the engine's write row afterwards, before any
\ handle for it exists. Atomic cells need native alignment, and this is
\ dictionary storage shared by every task rather than the engine's per-task
\ DATA header.
0 constant FREE
1 constant LIVE
2 constant CLOSING
here data-base - negate 7 and allot
create SLOT-STATUS SLOTS cells allot

\ A HANDLE IS A ROW AND THE GENERATION THAT ROW HAD WHEN THE HANDLE WAS MINTED.
\ CLOSE returns the row to the free list and bumps its generation, so the next
\ device to take that row gets a different handle and the old one is refused
\ with E-GENIO-STATE instead of being routed to somebody else's connection.
\ The row is the low byte because SLOTS is 9; the generation is everything
\ above it, which is 2^55 closes of one row before it could repeat.
8 constant GEN-SHIFT
$FF constant ROW-MASK

variable TERMINAL-READY                \ the built-in row is installed once

\ One byte of scratch per task, for the operations that move a single character
\ and for the readable-count ioctl. Task-local for the reason TCP4's endpoint
\ storage is: two tasks may hold two devices and neither may see the other's
\ byte.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $8 TASK:+USER IO-SCRATCH drop

\ The line READ-LINE hands back, also per task: two tasks may each be reading a
\ line from their own device.
$100 constant LINE-CAP
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and LINE-CAP TASK:+USER IO-LINE drop

: SCRATCH-CELL ( -- ptr n )
   IO-SCRATCH ;

: SCRATCH ( -- ptr u8 )
   IO-SCRATCH BYTE-VIEW ;

: LINE ( -- ptr u8 )
   IO-LINE BYTE-VIEW ;

CAST: BLEN>N ( NUM:byte-len -- n )

\ ---- the engine's per-task cells --------------------------------------------

: OUT-PTR ( -- ptr n )
   data-base GENIO-ABI:OUT-CELL + ;

: IN-PTR ( -- ptr n )
   data-base GENIO-ABI:IN-CELL + ;

: ACTIVE-PTR ( -- ptr n )
   data-base GENIO-ABI:ACTIVE-CELL + ;

\ Row i-1 of the engine's write table, which the output funnel reads and
\ nothing else does. Only a registered device has one; the terminal does not.
\ The row is in the RUNNING task's DATA, which is what makes the funnel's
\ dispatch per task -- and what decides how the row is stored.
\ The cell holds the handler the funnel calls, and a cleared row is spelled
\ ZERO -- habu2.f reads the cell and a zero means "no device". Those are two
\ element types for one DATA cell, so there are two accessors, each declaring
\ what it really touches. A single `( n -- ptr a )` said neither and handed the
\ choice to whichever caller asked, which is how engine DATA minted whatever a
\ caller's signature named (dot habu-bound-ptr-arithmetic-8bf6b54a).
: ENGINE-ROW-OFF ( n -- n ) {: idx:n :}
   GENIO-ABI:WRITE-OFF idx 1- 8 * + ;

: ENGINE-ROW ( n -- ptr [ ptr u8 n -- ] )
   ENGINE-ROW-OFF data-base + ;

: ENGINE-ROW-CLEAR ( n -- )
   0 swap ENGINE-ROW-OFF data-base + ! ;

\ In the main task the row is a cell of the engine's own DATA, so it is a
\ DECLARED address that a snapshot or an AOT capture has to canonicalise, and
\ `xt!` is its declaration point. In a worker the row is a cell of that task's
\ region, which no capture ever sees and which the declared-address table
\ bounds exclude -- `xt!` there refuses with "snapshot address cell out of
\ range". QUOTATION-STORAGE:STORE is the shipped word that asks which of the
\ two a cell is and stores it the right way, and the native compiler routes
\ every proven quotation store through it for exactly this reason.
: PUBLISH-WRITE ( n -- ) {: idx:n :}
   idx OP-WRITE @ idx ENGINE-ROW QUOTATION-STORAGE:STORE ;

\ ---- rows, handles and the free list ----------------------------------------

: BOUNDED ( n -- n ) {: idx:n :}
   idx 0 < idx SLOTS >= or if E-OPERAND throw then
   idx ;

: STATUS-CELL ( n -- ptr n ) {: idx:n :}
   idx BOUNDED cells SLOT-STATUS + ;

: STATUS@ ( n -- n )
   STATUS-CELL atomic@ ;

: HANDLE ( n n -- device ) {: idx:n gen:n :}
   gen GEN-SHIFT lshift idx or >DEVICE ;

: ROW-HANDLE ( n -- device ) {: idx:n :}
   idx idx SLOT-GEN @ HANDLE ;

\ The one place a handle becomes a row. A row outside the table is an operand
\ error; a row whose generation has moved on, or which nothing occupies, is a
\ state error -- the handle named a device that is gone.
: ROW ( device -- n )
   DEVICE>N {: h:n :}
   h ROW-MASK and BOUNDED {: idx:n :}
   idx TERMINAL-ROW = if TERMINAL-ROW exit then
   idx STATUS@ LIVE <> if E-STATE throw then
   h GEN-SHIFT rshift idx SLOT-GEN @ <> if E-STATE throw then
   idx ;

\ No row free is its own failure: the table is intact and every handle in it is
\ good, the caller simply has to close a device before opening another.
: CLAIM-ROW ( -- n )
   SLOTS 1 ?do
      FREE LIVE i STATUS-CELL atomic-cas 0= if i unloop exit then
   loop
   E-FULL throw ;

\ Win the right to close before touching anything. The loser of the race is
\ told the handle is gone, which is what it is: the winner is retiring it.
: CLAIM-CLOSE ( n -- ) {: idx:n :}
   LIVE CLOSING idx STATUS-CELL atomic-cas LIVE <> if E-STATE throw then ;

\ The generation has to be visible before the row can be claimed again, so the
\ release is the last store.
: RELEASE-ROW ( n -- ) {: idx:n :}
   idx SLOT-GEN @ 1+ idx SLOT-GEN !
   FREE idx STATUS-CELL atomic! ;

: ACTIVE-ROW ( -- n )
   ACTIVE-PTR @ BOUNDED ;

\ ---- the device a running operation belongs to -------------------------------

public

\ The row an operation is running for, and the cell that row keeps its own data
\ in. A device operation reads its socket, descriptor or buffer identity here;
\ outside an operation the answer is the terminal's.
: SELF ( -- device )
   ACTIVE-ROW ROW-HANDLE ;

: SELF-STATE ( -- n )
   ACTIVE-ROW SLOT-STATE @ ;

private

: FAULT! ( n -- ) {: errno:n :}
   errno ACTIVE-ROW SLOT-FAULT ! ;

\ A latched write failure STAYS latched: a device whose write failed is broken,
\ and every later operation on it says so until the row is built again.
: FAULT-CHECK ( n -- ) {: idx:n :}
   idx SLOT-FAULT @ 0 <> if E-IO throw then ;

\ ---- the built-in terminal, device 0 ----------------------------------------
\ These wrap what EMIT, TYPE, KEY and ACCEPT did before there were devices, so
\ they name the descriptors rather than calling `emit`/`type`: those now follow
\ the CURRENT device, and asking the terminal device to write must reach the
\ terminal whatever the current device is.

: TERMINAL-WRITE ( ptr u8 n -- ) {: bytes:ptr len:n :}
   STDOUT-FD bytes len write drop ;

: TERMINAL-EMIT ( n -- ) {: c:n :}
   c SCRATCH c!
   STDOUT-FD SCRATCH 1 write drop ;

: TERMINAL-READ ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   STDIN-FD bytes cap read dup 0 < if drop E-IO throw then ;

\ KEY1 and RD-LINE are src/habu/repl.f's: the line editor IS the terminal's
\ accept, exactly as a socket device supplies its own.
: TERMINAL-KEY ( -- n )
   KEY1 ;

: TERMINAL-ACCEPT ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   RD-LINE {: line:ptr len:n :}
   len cap > if E-OPERAND throw then
   len 0 ?do line i + c@ bytes i + c! loop
   len ;

: FIONREAD ( -- n )
   HB-TARGET-LINUX? if FIONREAD-LINUX exit then
   HB-TARGET-MACOS? if FIONREAD-MACOS exit then
   E-STATE throw ;

: TERMINAL-READY? ( -- bool )
   0 SCRATCH-CELL !
   STDIN-FD FIONREAD SCRATCH ioctl 0 <> if false exit then
   SCRATCH-CELL @ 0 > ;

: TERMINAL-NOTHING ( -- ) ;

: INSTALL-TERMINAL ( -- )
   TERMINAL-READY @ 0 <> if exit then
   [: TERMINAL-EMIT ;] TERMINAL-ROW OP-EMIT !
   [: TERMINAL-KEY ;] TERMINAL-ROW OP-KEY !
   [: TERMINAL-READY? ;] TERMINAL-ROW OP-READY !
   [: TERMINAL-READ ;] TERMINAL-ROW OP-READ !
   [: TERMINAL-WRITE ;] TERMINAL-ROW OP-WRITE !
   [: TERMINAL-ACCEPT ;] TERMINAL-ROW OP-ACCEPT !
   [: TERMINAL-NOTHING ;] TERMINAL-ROW OP-FLUSH !
   [: TERMINAL-NOTHING ;] TERMINAL-ROW OP-CLOSE !
   0 TERMINAL-ROW SLOT-STATE !
   0 TERMINAL-ROW SLOT-FAULT !
   0 TERMINAL-ROW SLOT-GEN !
   LIVE TERMINAL-ROW STATUS-CELL atomic!
   1 TERMINAL-READY ! ;

\ ---- capture: a device is a process resource, not a serialisable handle ------
\ Routing goes back to the terminal and every registered row is forgotten before
\ an image is captured, so no capture carries a socket, a descriptor or a code
\ address belonging to a device that will not exist in the restored process.
\ Zero is the answer both capture paths already understand: the snapshot rebase
\ leaves a zero declared cell zero (habu2.f, "a cleared hook survives a snapshot
\ as a cleared hook") and an AOT capture records a zero target instead of
\ refusing one outside its window (aot-capture.f ACAP-TARGET-OFFSET).

: CLEAR-ROWS ( -- )
   SLOTS 1 ?do i ENGINE-ROW-CLEAR loop ;

\ Every device is forgotten, and every row's generation moves on so a handle
\ minted before the capture cannot address a row in the restored process.
: RESET-ROUTING ( -- )
   0 OUT-PTR !
   0 IN-PTR !
   0 ACTIVE-PTR !
   0 data-base GENIO-ABI:BUSY-CELL + !
   CLEAR-ROWS
   SLOTS 1 ?do
      i SLOT-GEN @ 1+ i SLOT-GEN !
      FREE i STATUS-CELL atomic!
   loop ;

variable RESET-REGISTERED

: REGISTER-RESET ( -- )
   RESET-REGISTERED @ 0 <> if exit then
   [: RESET-ROUTING ;] IMAGE-LIFECYCLE:REGISTER
   1 RESET-REGISTERED ! ;

public

\ ---- building and choosing devices -------------------------------------------

\ Build a device from its table. The operations are given in the order
\ docs/socket-models.md section 2 lists them -- emit, key, key?, read, write,
\ accept, flush, close -- and the final cell is the device's own data: a TCP4
\ connection, a descriptor, a buffer identity. The write operation is also
\ published to the engine's output funnel, which is what makes `.`, `type` and
\ the REPL's ok prompt follow this device once it is current.
: DEVICE ( [ n -- ] [ -- n ] [ -- bool ] [ ptr u8 n -- n ] [ ptr u8 n -- ] [ ptr u8 n -- n ] [ -- ] [ -- ] n -- device )
   {: qemit qkey qready qread qwrite qaccept qflush qclose state:n :}
   INSTALL-TERMINAL
   REGISTER-RESET
   CLAIM-ROW {: idx:n :}
   qemit idx OP-EMIT !
   qkey idx OP-KEY !
   qready idx OP-READY !
   qread idx OP-READ !
   qwrite idx OP-WRITE !
   qaccept idx OP-ACCEPT !
   qflush idx OP-FLUSH !
   qclose idx OP-CLOSE !
   state idx SLOT-STATE !
   0 idx SLOT-FAULT !
   idx PUBLISH-WRITE
   idx ROW-HANDLE ;

: TERMINAL ( -- device )
   INSTALL-TERMINAL
   TERMINAL-ROW ROW-HANDLE ;

private

\ The engine's cell holds the bare row, so reading it back has to re-stamp the
\ generation. A row that has since been closed answers the terminal, which is
\ the same answer the output funnel gives for its emptied row.
: CURRENT ( n -- device ) {: idx:n :}
   idx STATUS@ LIVE <> if TERMINAL-ROW ROW-HANDLE exit then
   idx ROW-HANDLE ;

public

: OUTPUT@ ( -- device )
   INSTALL-TERMINAL
   OUT-PTR @ CURRENT ;

: INPUT@ ( -- device )
   INSTALL-TERMINAL
   IN-PTR @ CURRENT ;

: OUTPUT! ( device -- )
   ROW OUT-PTR ! ;

: INPUT! ( device -- )
   ROW IN-PTR ! ;

private

\ Restoring cannot fail over the body's own result: if the body closed the
\ device the caller was on, that task goes back to the terminal, exactly as it
\ would have if the close had happened with no WITH-IO in the picture.
: RESTORE-IN ( device -- ) {: dev:device :}
   dev DEVICE>N ROW-MASK and STATUS@ LIVE <> if 0 IN-PTR ! exit then
   dev INPUT! ;

: RESTORE-OUT ( device -- ) {: dev:device :}
   dev DEVICE>N ROW-MASK and STATUS@ LIVE <> if 0 OUT-PTR ! exit then
   dev OUTPUT! ;

public

\ Run the quotation with these devices current and put the caller's back,
\ whether it returns or throws.
: WITH-IO ( device device [ -- ] -- ) {: in:device out:device body :}
   INPUT@ {: previn:device :}
   OUTPUT@ {: prevout:device :}
   in INPUT!  out OUTPUT!
   body catch {: code:n :}
   previn RESTORE-IN  prevout RESTORE-OUT
   code 0 <> if code throw then ;

private

\ ---- dispatch ---------------------------------------------------------------
\ Each dispatcher publishes the row it is about to run for and puts the
\ caller's back. A throw abandons that restore, which is harmless: every
\ dispatcher publishes the cell before anything reads it.

: BUSY-PTR ( -- ptr n )
   data-base GENIO-ABI:BUSY-CELL + ;

\ A closed row answers the terminal rather than throwing, and it has to: the
\ engine's output funnel reaches a device write from inside `emit`, where a
\ throw would unwind through engine internals, so it falls back to the terminal
\ when the row it names is empty. This layer has to agree with it, or `type`
\ and GENIO:TYPE would disagree about where a closed device's output goes.
: LIVE-ROW ( n -- n ) {: idx:n :}
   idx STATUS@ LIVE <> if TERMINAL-ROW exit then
   idx ;

: OUT-ROW ( -- n )
   INSTALL-TERMINAL
   OUT-PTR @ LIVE-ROW ;

: IN-ROW ( -- n )
   INSTALL-TERMINAL
   IN-PTR @ LIVE-ROW ;

\ ---- retiring a row ---------------------------------------------------------

\ The close operation runs for the row CLOSE published in ACTIVE-CELL, which is
\ how this reaches it with no local of its own: a quotation cannot reference
\ one (docs/forth.md), and the operation has to run under `catch`.
: RUN-CLOSE-OP ( -- )
   ACTIVE-ROW OP-CLOSE @ execute ;

\ Everything the row was, undone, ending with the release: the generation has
\ to be visible before another task can take the row.
: RETIRE-ROW ( n -- ) {: idx:n :}
   idx ENGINE-ROW-CLEAR
   OUT-PTR @ idx = if 0 OUT-PTR ! then
   IN-PTR @ idx = if 0 IN-PTR ! then
   0 idx SLOT-STATE !
   0 idx SLOT-FAULT !
   idx RELEASE-ROW ;

public

\ EMIT and TYPE raise the funnel's guard for the same reason the funnel does:
\ while a device's write runs, an `emit` or `type` INSIDE it must reach the
\ terminal rather than call that write again. They do not read the guard --
\ asking this layer for a device explicitly is not the implicit path the guard
\ protects.
: EMIT ( n -- ) {: c:n :}
   OUT-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   BUSY-PTR @ {: busy:n :}
   idx ACTIVE-PTR !  1 BUSY-PTR !
   c idx OP-EMIT @ execute
   busy BUSY-PTR !  prev ACTIVE-PTR ! ;

: TYPE ( ptr u8 n -- ) {: bytes:ptr len:n :}
   OUT-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   BUSY-PTR @ {: busy:n :}
   idx ACTIVE-PTR !  1 BUSY-PTR !
   bytes len idx OP-WRITE @ execute
   busy BUSY-PTR !  prev ACTIVE-PTR ! ;

: WRITE ( ptr u8 n -- )
   TYPE ;

: FLUSH ( -- )
   OUT-ROW {: idx:n :}
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   idx OP-FLUSH @ execute
   prev ACTIVE-PTR !
   idx FAULT-CHECK ;

: KEY ( -- n )
   IN-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   idx OP-KEY @ execute
   prev ACTIVE-PTR ! ;

: KEY? ( -- bool )
   IN-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   idx OP-READY @ execute
   prev ACTIVE-PTR ! ;

: READ ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   IN-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   bytes cap idx OP-READ @ execute
   prev ACTIVE-PTR ! ;

\ One line without its terminator. A zero length is the end of input -- the
\ terminal answers it on ^D and a stream answers it when the peer closes -- so
\ a read loop ends instead of spinning.
: ACCEPT ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   IN-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   bytes cap idx OP-ACCEPT @ execute
   prev ACTIVE-PTR ! ;

\ The REPL's line reader, routed. src/habu/repl.f's RD-LINE is the terminal
\ device's accept and stays exactly that -- the tty editor with its history and
\ cursor keys -- while any other current input device answers with its own
\ accept. Requiring this file installs it as REPL-READ, so the engine's own REPL
\ loop and every other caller of that vector read from the current device with
\ no change to repl.f: that is what makes a REPL over a connection free.
: READ-LINE ( -- ptr u8 n )
   IN-ROW TERMINAL-ROW = if RD-LINE exit then
   LINE LINE-CAP ACCEPT {: len:n :}
   LINE len ;

\ Release the device's own resource and give its row back. The handle is dead
\ afterwards: the row's generation moves on, so a later device that takes the
\ row answers a different handle and this one is refused with E-GENIO-STATE
\ rather than routed to somebody else's connection. Closing the terminal is a
\ no-op and never frees row 0. A device that was this task's current input or
\ output leaves it on the terminal.
\
\ THE ROW IS RELEASED WHETHER THE OPERATION RETURNS OR THROWS, and the error
\ is raised afterwards. A close that fails is the ordinary way a device dies --
\ TCP-CLOSE answers E-GENIO-IO when the descriptor is already gone -- and the
\ row must not be the casualty: held in CLOSING it would be neither live, so
\ every handle refuses it, nor free, so no device could take it, for the life
\ of the process. The caller still hears what went wrong; it simply no longer
\ costs a row to hear it.
: CLOSE ( device -- )
   ROW {: idx:n :}
   idx TERMINAL-ROW <> if idx CLAIM-CLOSE then
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   [: RUN-CLOSE-OP ;] catch {: code:n :}
   prev ACTIVE-PTR !
   idx TERMINAL-ROW <> if idx RETIRE-ROW then
   code 0 <> if code throw then ;

private

\ ---- the TCP connection device ----------------------------------------------
\ A stream carries bytes and has no terminal in it: emit and write send, key,
\ read and accept receive, key? is the non-blocking question TCP4 already
\ answers, and flush has nothing to do because a write is a send.

: SELF-CONNECTION ( -- TCP4:connection )
   SELF-STATE TCP4:>CONNECTION ;

: TCP-SEND ( ptr u8 n -- ) {: bytes:ptr len:n :}
   len 0 = if exit then
   SELF-CONNECTION bytes len TCP4:TRANSFER-BYTES TCP4:WRITE
   MATCH TCP4:status
      ok OF ENDOF
      failed OF TCP4:ERRNO>N FAULT! ENDOF
   ;MATCH ;

: TCP-EMIT ( n -- ) {: c:n :}
   c SCRATCH c!
   SCRATCH 1 TCP-SEND ;

\ One transfer. `closed` and `failed` are both E-GENIO-IO here: neither can
\ answer with a byte, and answering zero for ever is the spin this must not do.
: TCP-RECEIVE ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   SELF-CONNECTION bytes cap TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N ENDOF
      closed OF drop E-IO throw ENDOF
      failed OF drop E-IO throw ENDOF
   ;MATCH ;

: TCP-KEY ( -- n )
   SCRATCH 1 TCP-RECEIVE drop
   SCRATCH c@ ;

: TCP-READY? ( -- bool )
   SELF-CONNECTION TCP4:READABLE?
   MATCH TCP4:ready-result
      ready OF true ENDOF
      idle OF false ENDOF
      failed OF drop E-IO throw ENDOF
   ;MATCH ;

\ One byte, answering false at the end of the stream instead of throwing: a
\ line reader has to be able to report the last partial line.
: TCP-BYTE? ( -- bool )
   SELF-CONNECTION SCRATCH 1 TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF drop true ENDOF
      closed OF drop false ENDOF
      failed OF drop E-IO throw ENDOF
   ;MATCH ;

\ A line, without its newline. Reading one byte at a time is what keeps the
\ stream's own bytes with the stream: a device has no buffer of its own to
\ leave a half-read line in for the next reader. The running length stays on
\ the data stack because a quotation-free loop cannot rebind a local.
: TCP-ACCEPT ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   0
   begin ( len )
      TCP-BYTE? 0= if exit then
      SCRATCH c@ LF = if exit then
      dup cap >= if E-OPERAND throw then
      dup bytes swap +  SCRATCH c@ swap c!
      1+
   again ;

: TCP-CLOSE ( -- )
   SELF-CONNECTION TCP4:CLOSE
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop E-IO throw ENDOF
   ;MATCH ;

: TCP-NOTHING ( -- ) ;

public

\ A connected TCP4 stream as a device. The caller still owns the connection's
\ lifetime: GENIO:CLOSE on this device is TCP4:CLOSE on that connection.
: TCP-DEVICE ( TCP4:connection -- device )
   TCP4:CONNECTION>N {: fd:n :}
   [: TCP-EMIT ;] [: TCP-KEY ;] [: TCP-READY? ;] [: TCP-RECEIVE ;]
   [: TCP-SEND ;] [: TCP-ACCEPT ;] [: TCP-NOTHING ;] [: TCP-CLOSE ;]
   fd DEVICE ;

private

: INSTALL-REPL-READ ( -- )
   [: READ-LINE ;] is REPL-READ ;

INSTALL-REPL-READ

;package
