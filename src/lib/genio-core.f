\ genio-core.f - the generic text I/O device table: EMIT, KEY, ACCEPT and the
\ rest reach a device through a vector table instead of the process terminal,
\ and the current task chooses which device that is. This is the table alone:
\ it requires no transport and changes nothing on load, so a tool can install
\ its own device. src/lib/genio.f adds the TCP device and the routed REPL
\ reader on top of it, in the same package.
\
\ STORAGE CLASS. TASK-LOCAL for the state an operation runs through: the
\ current input and output device indices are two per-task cells of the DATA
\ header (GENIO-ABI), and the single-byte scratch is a TASK:+USER row, so two
\ tasks may each drive their own device. The device table itself - the rows
\ and the eight operations in them - is PROCESS-WIDE and shared, which is what
\ lets a task inherit its creator's devices; installing or removing a device is
\ not a per-task act.
\
\ THE SHAPE (docs/design.md "Models taken from other Forths"). A device is eight
\ operations and one cell of its own data. The terminal is device 0 and is
\ built in; every other device takes a row of a small fixed table, so a device
\ handle is a row number and nothing that routes I/O is ever an address. The
\ current task's input and output devices are two cells of the engine's DATA
\ header (src/habu/layout.f GENIO-ABI), which is what makes them per task: the
\ engine swaps DATA for each task, src/lib/task.f copies them into a new task's
\ region, and the engine's own output funnel reads the output cell directly.
\
\ THE EIGHT OPERATIONS ARE SHARED CODE. Two TCP devices are the same eight
\ quotations over two different connections, and a quotation cannot capture the
\ device it belongs to, so an operation asks GENIO-ABI:ACTIVE-CELL for its own
\ row and reads its state from there. Every dispatcher below publishes that cell
\ before it executes an operation; the engine funnel publishes it too.
\
\ A WRITE OR EMIT OPERATION MUST NOT THROW. The engine reaches a write from
\ inside `emit` and `type`, where a throw would unwind through engine internals
\ with the funnel's re-entrancy guard still set. An emit is reached only from
\ EMIT and FORWARD-EMIT, which raise that guard around it with no catch, so a
\ throw would leave the guard and the active row set. A device that fails a
\ write or an emit latches the failure instead, and the next KEY, READ, ACCEPT
\ or FLUSH on that device raises E-GENIO-IO. Every other operation is called
\ from ordinary Habu code and reports at once.
\
\ HANDLES. A handle is a row and a generation: 0 is the terminal, and the
\ GENIO-ABI:DEVICES further rows are a pool. CLOSE frees the row and moves its
\ generation on, so an older handle is E-GENIO-STATE at CLOSE, OUTPUT!, INPUT!
\ and WITH-IO; a close operation that throws still frees the row, then raises.
\ Rows are claimed by compare-and-swap (FREE -> LIVE, LIVE -> CLOSING), so two
\ tasks closing one handle cannot both run its close. One task builds and
\ closes a device: its write row lives in that task's DATA.
\
\ ROUTING. emit, type, ., cr and the REPL's ok funnel through src/habu/rt.f
\ G-OUT to the current output device. Diagnostics (crash reports, die, E-*
\ lines) stay on descriptor 2. The funnel falls back to descriptor 1 for a
\ write that re-entered type, a routing cell outside 1..DEVICES and an
\ unregistered row.
\
\ IMAGES. The IMAGE-LIFECYCLE reset routes back to the terminal, frees every
\ row and moves every generation on, so no capture carries a device.
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

: SCRATCH-CELL ( -- ptr n )
   IO-SCRATCH ;

: SCRATCH ( -- ptr u8 )
   IO-SCRATCH BYTE-VIEW ;

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

\ A zero is no failure, and a latched failure stays (FAULT-CHECK).
: LATCH-ROW ( n n -- ) {: errno:n idx:n :}
   errno 0= if exit then
   idx SLOT-FAULT @ 0 <> if exit then
   errno idx SLOT-FAULT ! ;

\ ---- the device a running operation belongs to -------------------------------

public

\ The row an operation is running for, and the cell that row keeps its own data
\ in. A device operation reads its socket, descriptor or buffer identity here;
\ outside an operation the answer is the terminal's.
: SELF ( -- device )
   ACTIVE-ROW ROW-HANDLE ;

: SELF-STATE ( -- n )
   ACTIVE-ROW SLOT-STATE @ ;

\ Latch a failed write on the device whose operation is running: the write
\ operation's way to report, since it must not throw. A zero is ignored, and
\ a failure already latched is kept.
: FAULT! ( n -- )
   ACTIVE-ROW LATCH-ROW ;

private

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

\ ---- building and choosing devices -------------------------------------------

\ Build a device from its table. The operations are given in this order --
\ emit, key, key?, read, write,
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

\ ---- dispatch ---------------------------------------------------------------
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

\ ---- retiring a row ---------------------------------------------------------

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

\ FLUSH, KEY, KEY?, READ and ACCEPT put the caller's ACTIVE-CELL back whether
\ the operation returns or throws, and raise its error afterwards.
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
\ a read loop ends instead of spinning.
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

;package
