\ genio-core.f - the generic text I/O device table: EMIT, KEY, ACCEPT and the
\ rest reach a device through a vector table, chosen per task. It requires no
\ transport and changes nothing on load; src/lib/genio.f adds the TCP device and
\ the routed REPL reader in the same package.
\ STORAGE CLASS. TASK-LOCAL: the device indices (GENIO-ABI) and the byte scratch.
\ PROCESS-WIDE: the device table, so a task inherits its creator's devices.
require src/lib/errors.f
require src/lib/prelude.f
require src/lib/type/deftype.f
require src/lib/image-lifecycle.f
\ A device's write row is stored through QUOTATION-STORAGE:STORE, not `xt!`:
\ see PUBLISH-WRITE below.
require src/core/quotation-storage.f
require src/lib/task.f

package GENIO

public

\ A device handle is its row: 0 is the terminal, 1..GENIO-ABI:DEVICES are the
\ registered devices. Like every value nominal the raw converters are not
\ validators; DEVICE, TERMINAL and TCP-DEVICE (src/lib/genio.f) are the ways
\ to obtain one.
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

\ A row is claimed by winning a compare-and-swap on its own cell (FREE -> LIVE,
\ LIVE -> CLOSING), never by a read-then-write: two tasks building devices must
\ not get the same row, and of two tasks closing one handle exactly one runs the
\ close and bumps the generation. The claim publishes the row as taken; its
\ owner fills the operations, the state and the engine's write row before any
\ handle exists. Atomic cells need native alignment; this is dictionary storage
\ shared by every task, not the per-task DATA header.
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

: SCRATCH-CELL ( -- ptr n )
   IO-SCRATCH ;

: SCRATCH ( -- ptr u8 )
   IO-SCRATCH BYTE-VIEW ;

\ The current task's input and output devices are two cells of the engine's DATA
\ header (src/habu/layout.f GENIO-ABI): the engine swaps DATA for each task,
\ src/lib/task.f copies them into a new task's region, and the engine's output
\ funnel reads the output cell directly. A handle is a row, never an address.

: OUT-PTR ( -- ptr n )
   data-base GENIO-ABI:OUT-CELL + ;

: IN-PTR ( -- ptr n )
   data-base GENIO-ABI:IN-CELL + ;

: ACTIVE-PTR ( -- ptr n )
   data-base GENIO-ABI:ACTIVE-CELL + ;

\ Row i-1 of the engine's write table, which only the output funnel reads; the
\ terminal has none. The row is in the running task's DATA, which makes the
\ funnel's dispatch per task. The cell holds the handler the funnel calls, and a
\ cleared row is ZERO (habu2.f reads zero as no device): two element types for
\ one DATA cell, so two accessors, each declaring what it touches.
: ENGINE-ROW-OFF ( n -- n ) {: idx:n :}
   GENIO-ABI:WRITE-OFF idx 1- 8 * + ;

\ GENIO-ABI:WRITE-OFF is an image-ABI offset, so this row cannot become a
\ TYPED-VARIABLE, and no checked word may give a DATA address a quotation
\ pointee except through a private CAST:. The cast is the address computation
\ alone -- the same declaration `xt!` makes in QUOTATION-STORAGE:STORE below.
\ ENGINE-ROW-CLEAR touches the same cell as a number.
CAST: >ENGINE-ROW ( n -- ptr [ ptr u8 n -- ] )

: ENGINE-ROW ( n -- ptr [ ptr u8 n -- ] )
   ENGINE-ROW-OFF data-base BYTE-VIEW NULL-PTR BYTE-VIEW - + >ENGINE-ROW ;

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

\ A zero is no failure, and a latched failure stays (FAULT-CHECK).
: LATCH-ROW ( n n -- ) {: errno:n idx:n :}
   errno 0= if exit then
   idx SLOT-FAULT @ 0 <> if exit then
   errno idx SLOT-FAULT ! ;


public

\ The row an operation is running for and the cell holding its data. The eight
\ operations are shared code (two TCP devices are the same quotations over two
\ connections) and a quotation cannot capture its device, so an operation reads
\ its socket, descriptor or buffer identity here; every dispatcher and the
\ engine funnel publish ACTIVE-CELL before running one. Outside an operation
\ the answer is the terminal's.
: SELF ( -- device )
   ACTIVE-ROW ROW-HANDLE ;

: SELF-STATE ( -- n )
   ACTIVE-ROW SLOT-STATE @ ;

\ Latch a failed write on the running device: a write or emit operation must not
\ throw. The engine reaches a write from inside `emit` and `type`, where a throw
\ would unwind through engine internals with the funnel's re-entrancy guard set,
\ and EMIT and FORWARD-EMIT raise that guard around an emit with no catch. The
\ next KEY, READ, ACCEPT or FLUSH on the device raises E-GENIO-IO. A zero is
\ ignored, and a failure already latched is kept.
: FAULT! ( n -- )
   ACTIVE-ROW LATCH-ROW ;

private

\ A latched write failure STAYS latched: a device whose write failed is broken,
\ and every later operation on it says so until the row is built again.
: FAULT-CHECK ( n -- ) {: idx:n :}
   idx SLOT-FAULT @ 0 <> if E-IO throw then ;

\ The terminal's operations name the descriptors rather than calling `emit` and
\ `type`, which follow the current device.

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
   HB-TARGET-LINUX-KERNEL? if FIONREAD-LINUX exit then
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

\ Routing goes back to the terminal and every registered row is forgotten before
\ an image is captured, so no capture carries a socket, a descriptor or a code
\ address belonging to a device that will not exist in the restored process.
\ Zero is the answer both capture paths already understand: the snapshot rebase
\ leaves a zero declared cell zero (habu2.f, "a cleared hook survives a snapshot
\ as a cleared hook") and an AOT capture records a zero target instead of
\ refusing one outside its window (aot-capture.f ACAP-TARGET-OFFSET).

: CLEAR-ROWS ( -- )
   SLOTS 1 ?do i ENGINE-ROW-CLEAR loop ;

variable RESET-REGISTERED

\ Every device is forgotten, and every row's generation moves on so a handle
\ minted before the capture cannot address a row in the restored process.
\ PREPARE removes this one-shot hook once it returns, so the flag goes with it:
\ a capture carries the flag but no registration, and a flag left set would
\ stop the next DEVICE - in this process or the restored image - from arming
\ the next capture's reset.
: RESET-ROUTING ( -- )
   0 OUT-PTR !
   0 IN-PTR !
   0 ACTIVE-PTR !
   0 data-base GENIO-ABI:BUSY-CELL + !
   CLEAR-ROWS
   SLOTS 1 ?do
      i SLOT-GEN @ 1+ i SLOT-GEN !
      FREE i STATUS-CELL atomic!
   loop
   0 RESET-REGISTERED ! ;

: REGISTER-RESET ( -- )
   RESET-REGISTERED @ 0 <> if exit then
   [: RESET-ROUTING ;] IMAGE-LIFECYCLE:REGISTER
   1 RESET-REGISTERED ! ;

public


\ Build a device from its table. The operations are given in this order --
\ emit, key, key?, read, write, accept, flush, close -- and the final cell is the
\ device's own data: a TCP4 connection, a descriptor, a buffer identity. One task
\ builds and closes a device: its write row lives in that task's DATA. emit,
\ type, ., cr and the REPL's ok funnel through src/habu/rt.f G-OUT to the
\ current output device; diagnostics (crash reports, die, E-* lines) stay on
\ descriptor 2. The funnel falls back to descriptor 1 for a write that
\ re-entered type, a routing cell outside 1..DEVICES and an unregistered row.
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

\ The write failure the device latched, 0 when healthy.
: FAULT@ ( device -- n )
   ROW SLOT-FAULT @ ;

\ Latch a failure on the device from outside its operations, as FAULT! does
\ inside one: a device standing in front of another starts with, or catches
\ up to, that device's fault.
: LATCH ( n device -- )
   ROW LATCH-ROW ;

\ The device, or the terminal once its row was closed: the device the output
\ funnel writes to for it.
: LIVE-DEVICE ( device -- device ) {: dev:device :}
   dev DEVICE>N {: h:n :}
   h ROW-MASK and {: idx:n :}
   idx STATUS@ LIVE <> if TERMINAL-ROW ROW-HANDLE exit then
   h GEN-SHIFT rshift idx SLOT-GEN @ <> if TERMINAL-ROW ROW-HANDLE exit then
   dev ;

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

\ Each dispatcher publishes the row it is about to run for and puts the
\ caller's back, also when the operation throws. Only EMIT, WRITE, FORWARD and
\ FORWARD-EMIT skip that catch, because their operations do not throw.

: BUSY-PTR ( -- ptr n )
   data-base GENIO-ABI:BUSY-CELL + ;

\ A closed row answers the terminal rather than throwing, and it has to: the
\ engine's output funnel reaches a device write from inside `emit`, where a
\ throw would unwind through engine internals, so it falls back to the terminal
\ when the row it names is empty. This layer has to agree with it, or `type`
\ and GENIO:WRITE would disagree about where a closed device's output goes.
: LIVE-ROW ( n -- n ) {: idx:n :}
   idx STATUS@ LIVE <> if TERMINAL-ROW exit then
   idx ;

: OUT-ROW ( -- n )
   INSTALL-TERMINAL
   OUT-PTR @ LIVE-ROW ;

: IN-ROW ( -- n )
   INSTALL-TERMINAL
   IN-PTR @ LIVE-ROW ;


\ The close operation runs for the row CLOSE published in ACTIVE-CELL, which is
\ how this reaches it with no local of its own: a quotation cannot reference
\ one (docs/forth.md), and the operation has to run under `catch`.
: RUN-CLOSE-OP ( -- )
   ACTIVE-ROW OP-CLOSE @ execute ;

\ The operations that may throw run the same way, so the dispatcher puts the
\ caller's ACTIVE-CELL back before the error leaves it: an operation of the
\ caller's device that runs afterwards must still see itself as SELF.
: RUN-FLUSH-OP ( -- )
   ACTIVE-ROW OP-FLUSH @ execute ;
: RUN-KEY-OP ( n -- n )
   drop ACTIVE-ROW OP-KEY @ execute ;
: RUN-READY-OP ( bool -- bool )
   drop ACTIVE-ROW OP-READY @ execute ;
\ The buffer stays under the count read, so the catch preserves the shape.
: RUN-READ-OP ( ptr u8 n -- ptr u8 n )
   over swap ACTIVE-ROW OP-READ @ execute ;
: RUN-ACCEPT-OP ( ptr u8 n -- ptr u8 n )
   over swap ACTIVE-ROW OP-ACCEPT @ execute ;

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

\ EMIT and WRITE raise the funnel's guard for the same reason the funnel does:
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

: WRITE ( ptr u8 n -- ) {: bytes:ptr len:n :}
   OUT-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   BUSY-PTR @ {: busy:n :}
   idx ACTIVE-PTR !  1 BUSY-PTR !
   bytes len idx OP-WRITE @ execute
   busy BUSY-PTR !  prev ACTIVE-PTR ! ;

\ From inside a device's write operation, hand the bytes to another device's
\ write operation as the output funnel would, with no fault check, then latch
\ that device's fault on the running device (SELF) as FAULT! does: SELF's next
\ EMIT, WRITE or FLUSH raises what the other device's would, and a fault SELF
\ latched stays. A closed device answers the terminal. Like any write
\ operation, it does not throw.
: FORWARD ( ptr u8 n device -- ) {: bytes:ptr len:n dev:device :}
   ACTIVE-ROW {: self:n :}
   dev LIVE-DEVICE ROW {: idx:n :}
   BUSY-PTR @ {: busy:n :}
   idx ACTIVE-PTR !  1 BUSY-PTR !
   bytes len idx OP-WRITE @ execute
   busy BUSY-PTR !  self ACTIVE-PTR !
   idx SLOT-FAULT @ self LATCH-ROW ;

\ FORWARD's twin for one emit: the other device's emit operation runs, and
\ SELF latches that device's fault.
: FORWARD-EMIT ( n device -- ) {: c:n dev:device :}
   ACTIVE-ROW {: self:n :}
   dev LIVE-DEVICE ROW {: idx:n :}
   BUSY-PTR @ {: busy:n :}
   idx ACTIVE-PTR !  1 BUSY-PTR !
   c idx OP-EMIT @ execute
   busy BUSY-PTR !  self ACTIVE-PTR !
   idx SLOT-FAULT @ self LATCH-ROW ;

: FLUSH ( -- )
   OUT-ROW {: idx:n :}
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   [: RUN-FLUSH-OP ;] catch {: code:n :}
   prev ACTIVE-PTR !
   code 0<> if code throw then
   idx FAULT-CHECK ;

: KEY ( -- n )
   IN-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   0 [: RUN-KEY-OP ;] catch {: code:n :}
   prev ACTIVE-PTR !
   code 0<> if code throw then ;

: KEY? ( -- bool )
   IN-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   false [: RUN-READY-OP ;] catch {: code:n :}
   prev ACTIVE-PTR !
   code 0<> if code throw then ;

: READ ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   IN-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   bytes cap [: RUN-READ-OP ;] catch {: code:n :}
   prev ACTIVE-PTR !
   code 0<> if code throw then
   nip ;

\ One line without its terminator. A zero length is the end of input -- the
\ terminal answers it on ^D and a stream answers it when the peer closes -- so
\ a read loop ends instead of spinning. A stream fills at most the buffer and
\ leaves the rest of a longer line for the next ACCEPT, so a filled buffer means
\ the line may go on; the terminal refuses a line longer than the buffer with
\ E-OPERAND, because its editor's line cannot be read twice.
: ACCEPT ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   IN-ROW {: idx:n :}
   idx FAULT-CHECK
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   bytes cap [: RUN-ACCEPT-OP ;] catch {: code:n :}
   prev ACTIVE-PTR !
   code 0<> if code throw then
   nip ;

\ Release the device's own resource and give its row back. The handle is dead
\ afterwards: the row's generation moves on, so a later device that takes the
\ row answers a different handle and this one is refused with E-GENIO-STATE
\ rather than routed to somebody else's connection. Closing the terminal is a
\ no-op and never frees row 0. A device that was this task's current input or
\ output leaves it on the terminal.
\
\ The row is released whether the operation returns or throws, and the error is
\ raised afterwards: a failing close is the ordinary way a device dies (TCP-CLOSE
\ answers E-GENIO-IO when the descriptor is gone), and a row held in CLOSING
\ would be neither live nor free for the life of the process.
: CLOSE ( device -- )
   ROW {: idx:n :}
   idx TERMINAL-ROW <> if idx CLAIM-CLOSE then
   ACTIVE-PTR @ {: prev:n :}
   idx ACTIVE-PTR !
   [: RUN-CLOSE-OP ;] catch {: code:n :}
   prev ACTIVE-PTR !
   idx TERMINAL-ROW <> if idx RETIRE-ROW then
   code 0 <> if code throw then ;

;package
