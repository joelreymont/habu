\ ws.f - RFC 6455 WebSocket connections on the HTTP/1.1 server (lib/net/http.f).
\
\ ACCEPT, inside a route handler, checks the request as an opening handshake
\ (section 4.2.1), answers 101 with Sec-WebSocket-Accept and takes the
\ request's connection over from its worker (HTTP:TAKE-OVER). The socket it
\ answers `open` with is the handler's until the handler returns: RECEIVE
\ answers one whole message, or says that the socket closed or that the wait
\ ran out, and SEND-TEXT, SEND-BINARY and CLOSE write frames. A handshake that
\ is refused is answered through the HTTP response - 400, or 426 for a version
\ other than 13 - which the worker sends once the handler returns; ACCEPT
\ answers `refused` with that status, and no socket exists for it.
\
\ RECEIVE belongs to the worker that accepted the socket. It reassembles a
\ fragmented message, answers a ping with a pong, even once this end has sent
\ its close, and a close frame with a close, and fails the connection when the
\ peer breaks the protocol: status 1002 for a frame the codec refuses
\ (lib/net/ws-frame.f) or one out of order, 1007 for text that is not UTF-8,
\ 1009 for a message past MESSAGE-MAX. A closed socket
\ reports the status its close frame carried, 1005 when the frame carried none
\ and 1006 when the stream ended without one (section 7.1.5). A message's bytes
\ are BORROWED from the socket until its next RECEIVE. Its wait is absolute,
\ however many bytes the peer keeps queued, and is waited in slices of
\ HTTP:STOP-WAIT-MS: once the server stops, RECEIVE closes the socket under
\ status 1001, going away, or loses it under 1006 when that close cannot be
\ written, and answers that.
\
\ The sending words may be called from any task. Each frame is written whole
\ under a TASK:FACILITY of its worker slot's own, so a push from another task
\ lands between the handler's frames and never inside one. A send on a socket
\ that is closing, closed or past its handler is E-WS-CLOSED, and text that is
\ not UTF-8 is E-WS-TEXT before a byte of it is written. A peer that accepts
\ no byte of a frame for STALL-MS fails the socket under status 1006, and the
\ send that stalled, like every one waiting behind it, is E-WS-CLOSED. The
\ pong and the close RECEIVE owes the peer are due STALL-MS after the worker
\ comes for the slot's lock (CLAIM): a push ahead of the answer, even one
\ whose peer keeps taking bytes, gives up then and loses the socket the same
\ way, and so does the answer's own write. Once the server stops, every write
\ on the slot - the handler's own send, another task's push, or an answer
\ RECEIVE owes - is cut short and loses the socket the same way when it gives
\ up (GIVE-UP?), within the stop's bound (lib/net/http.f STOP-BOUND-MS)
\ however many bytes the peer goes on taking.
\
\ An application's own task inside a send is joined, never killed, as
\ docs/threads.md asks of a task in TASK:WAIT: killed at the write's pause it
\ leaves the slot's lock held, so the worker's next take of the lock blocks
\ and the stop never returns.
\
\ When the handler ends, the worker lets the socket go: a socket still open is
\ closed with status 1000, or 1011 when the handler threw, in a close frame
\ due HTTP:RELEASE-MS after the worker comes for the slot's lock, and the HTTP
\ worker then closes the connection. Another task's send, held in a write when
\ the worker comes for the lock to let the socket go or to retire it, gives up
\ at that deadline: the socket is lost, the send is E-WS-CLOSED, and the
\ worker sends nothing more. A worker that a stop kills inside its handler
\ never lets its socket go: its exit retires the socket instead, sending
\ nothing, and the HTTP worker's exit closes the connection after that.
\
\ No subprotocol and no extension is negotiated, and the 101 carries no header
\ line the handler added to the response.
\
\ STORAGE CLASS. PROCESS-WIDE tables indexed by the HTTP worker's slot: a
\ socket lives in the worker that accepted it, so a slot holds one socket at a
\ time. A socket's message buffer and scratch are one mapping, made by ACCEPT
\ and given back when the worker lets the socket go or its exit retires it. A
\ slot's send lock is a TASK:FACILITY made ready while this file loads, before
\ any task is live, and never destroyed.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/span.f
require lib/task.f
require lib/utf8-scalar.f
require lib/base64.f
require lib/crypto/sha1.f
require lib/net/tcp4.f
require lib/net/http.f
require lib/net/ws-frame.f

package WS
public

\ One accepted connection: its worker's slot and the count of retirements that
\ slot had seen when it opened, so a handle kept past its socket names no later
\ one.
NEWTYPE socket 0

\ What RECEIVE answers: a whole text or binary message, its bytes and their
\ count; that the socket has closed, with the status it closed under; or that
\ the wait ran out first.
ENUM message 0
   VARIANT text FIELD chars ptr u8 FIELD count n ;VARIANT
   VARIANT binary FIELD bytes ptr u8 FIELD len n ;VARIANT
   VARIANT closed FIELD status n ;VARIANT
   VARIANT timeout ;VARIANT
;ENUM

\ What ACCEPT answers: the socket of a handshake that passed, or the HTTP
\ status, 400 or 426, a refused one is answered with once the handler returns.
ENUM accepted 0
   VARIANT open FIELD sock socket ;VARIANT
   VARIANT refused FIELD status n ;VARIANT
;ENUM

$100000 constant MESSAGE-MAX      \ the longest message RECEIVE reassembles

\ How long a peer may accept no byte of a frame written to it. One that takes
\ none for STALL-MS fails the socket: it closes under status 1006, and the send
\ and every sender waiting behind it are E-WS-CLOSED. The write notices the
\ bound when the send slice it is in ends, within TCP4:SEND-SLICE-MS and two
\ sends that never wait after it passes.
5000 constant STALL-MS

private

CAST: >SOCKET ( n -- socket )
CAST: SOCKET>N ( socket -- n )
CAST: BLEN>N ( NUM:byte-len -- n )

\ A socket's life. Every change of phase is made under the slot's lock. A
\ change another task may make - a send that loses the connection, a CLOSE -
\ stores the status first and publishes the phase with atomic!, and RECEIVE,
\ which reads both without the lock, reads the phase with atomic@ before the
\ status (docs/threads.md, Atomics). PHASE and STATUS are rows of cells whose
\ base TYPED-BUFFER aligns to a cell, so every element is 8-byte aligned.
0 constant PHASE-NONE             \ no socket: before a slot's first, after the worker lets one go or the slot is retired
1 constant PHASE-OPEN             \ frames go both ways
2 constant PHASE-CLOSING          \ this end has sent its close frame and awaits the peer's
3 constant PHASE-CLOSED           \ the closing handshake is over, or the connection failed

\ Close statuses (section 7.4.1).
1000 constant STATUS-NORMAL
1001 constant STATUS-AWAY         \ the server is stopping
1002 constant STATUS-PROTOCOL
1005 constant STATUS-NONE         \ reported for a close frame that carried no status; never sent
1006 constant STATUS-LOST         \ reported for a stream that ended with no close frame; never sent
1007 constant STATUS-DATA
1009 constant STATUS-TOO-BIG
1011 constant STATUS-INTERNAL

\ The slot is the low byte of a handle and the slot's retirement count the rest.
8 constant GEN-SHIFT
$FF constant SLOT-MASK

1000000 constant NS-PER-MS
$7FFFFFFF constant WAIT-MAX       \ the longest wait a readiness poll takes, in ms (lib/net/tcp4.f)
$2C constant COMMA
$18 constant NONCE-CHARS          \ sixteen bytes in base64
$10 constant NONCE-BYTES
$18 constant NONCE-CAP            \ what 24 characters can decode to, to the cell

\ One socket's mapping: the message being reassembled, the scratch a frame is
\ written in, a control frame's payload, the header being read, and the digest
\ behind the accept value.
$1000 constant TX-CAP             \ a frame no longer than this goes out in one write
$80 constant CONTROL-CAP          \ 125 bytes, to the cell
$10 constant HEAD-CAP             \ HEADER-MAX bytes, to the cell
MESSAGE-MAX constant TX-OFF
TX-OFF TX-CAP + constant CONTROL-OFF
CONTROL-OFF CONTROL-CAP + constant HEAD-OFF
HEAD-OFF HEAD-CAP + constant CTX-OFF
CTX-OFF SHA1:CTX-BYTES + constant DIGEST-OFF
DIGEST-OFF SHA1:DIGEST-BYTES + constant REGION-BYTES

HTTP:MAX-WORKERS TYPED-BUFFER LINK TCP4:connection
HTTP:MAX-WORKERS TYPED-BUFFER REGION SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER GEN n               \ retirements: one before each socket, one at each worker exit
HTTP:MAX-WORKERS TYPED-BUFFER PHASE n
HTTP:MAX-WORKERS TYPED-BUFFER STATUS n            \ the status a closed socket reports
HTTP:MAX-WORKERS TYPED-BUFFER OWNER n             \ the worker task that accepted it
HTTP:MAX-WORKERS TYPED-BUFFER SENT n              \ whether the send in hand was written
HTTP:MAX-WORKERS TYPED-BUFFER DUE n               \ when every write on the slot gives up, while its worker claims the lock (CLAIM); 0 otherwise
HTTP:MAX-WORKERS TYPED-BUFFER CUT-AT n            \ when a write's look first found the server stopping (GIVE-UP?); 0 until then
HTTP:MAX-WORKERS TYPED-BUFFER STALLED bool        \ set while the last slice of the write in hand found no room (PUT)
HTTP:MAX-WORKERS TYPED-BUFFER EARLY ptr u8        \ bytes the peer sent behind its handshake
HTTP:MAX-WORKERS TYPED-BUFFER EARLY-U n
HTTP:MAX-WORKERS TYPED-BUFFER KEY-AT ptr u8       \ Sec-WebSocket-Key as it was sent
HTTP:MAX-WORKERS TYPED-BUFFER KEY-U n
HTTP:MAX-WORKERS TYPED-BUFFER HEAD-U n            \ header bytes held of the frame being read
HTTP:MAX-WORKERS TYPED-BUFFER HEAD-WANT n         \ header bytes to hold before decoding again
HTTP:MAX-WORKERS TYPED-BUFFER HEAD-DONE n         \ set once the header is decoded
HTTP:MAX-WORKERS TYPED-BUFFER F-FIN n
HTTP:MAX-WORKERS TYPED-BUFFER F-WIRE n            \ the frame's opcode, as on the wire
HTTP:MAX-WORKERS TYPED-BUFFER F-LEN n
HTTP:MAX-WORKERS TYPED-BUFFER F-KEY n
HTTP:MAX-WORKERS TYPED-BUFFER PAY-U n             \ payload bytes of it read
HTTP:MAX-WORKERS TYPED-BUFFER FAILING n           \ the status a faulty frame closes under
HTTP:MAX-WORKERS TYPED-BUFFER MSG-U n             \ bytes of the message being reassembled
HTTP:MAX-WORKERS TYPED-BUFFER MSG-WIRE n          \ its opcode; 0 while there is none
HTTP:MAX-WORKERS TYPED-BUFFER MSG-READY n         \ set once its last frame has arrived

\ One send lock to a slot, as lib/net/http.f defines one task to a worker.
TASK:FACILITY LOCK-0
TASK:FACILITY LOCK-1
TASK:FACILITY LOCK-2
TASK:FACILITY LOCK-3
TASK:FACILITY LOCK-4
TASK:FACILITY LOCK-5
TASK:FACILITY LOCK-6
TASK:FACILITY LOCK-7
8 constant LOCKS

HTTP:MAX-WORKERS NONCE-CAP * SPAN-BUFFER: NONCES

\ How a read toward a frame went: the bytes are all held, the deadline passed
\ first, the stream ended or failed, or the frame breaks the protocol.
ENUM progress through late gone fault ;ENUM


\ ---- handles -----------------------------------------------------------------

\ The slot's lock, named by the word that defined it: an address stored in a
\ table would be one an image captured with it.
: LOCK ( n -- ptr n ) {: idx:n :}
   idx 0 < idx LOCKS >= or if E-WS-SOCKET throw then
   idx 0 = if LOCK-0 exit then
   idx 1 = if LOCK-1 exit then
   idx 2 = if LOCK-2 exit then
   idx 3 = if LOCK-3 exit then
   idx 4 = if LOCK-4 exit then
   idx 5 = if LOCK-5 exit then
   idx 6 = if LOCK-6 exit then
   LOCK-7 ;


\ No task is live while this file loads, so every lock is made ready here, once,
\ before any worker can take one.
: ARM-LOCKS ( -- )
   LOCKS 0 ?do i LOCK TASK:FACILITY-INIT loop ;

ARM-LOCKS


: >SLOT ( n -- n ) {: h:n :}
   h SLOT-MASK and {: idx:n :}
   idx HTTP:MAX-WORKERS >= if E-WS-SOCKET throw then
   idx ;


: HANDLE ( n -- n ) {: idx:n :}
   idx GEN @ GEN-SHIFT lshift idx or ;


\ A handle names its slot's socket while the slot has not been retired since
\ the socket opened and the worker has not let the socket go.
: CURRENT? ( n -- bool ) {: h:n :}
   h >SLOT {: idx:n :}
   h GEN-SHIFT rshift idx GEN @ = idx PHASE @ PHASE-NONE <> and ;


\ ---- the socket's buffers ----------------------------------------------------

\ Each narrows the socket's mapping. A slot with no mapping holds a span with
\ no reach, so every narrowing of it throws instead of addressing a null base.

: MSG-BUF ( n -- SPAN:span<u8> )
   REGION @ MESSAGE-MAX SPAN:TAKE ;


: TX-BUF ( n -- SPAN:span<u8> )
   REGION @ TX-OFF TX-CAP SPAN:SUB ;


: CONTROL-BUF ( n -- SPAN:span<u8> )
   REGION @ CONTROL-OFF CONTROL-CAP SPAN:SUB ;


: HEAD-BUF ( n -- SPAN:span<u8> )
   REGION @ HEAD-OFF HEAD-CAP SPAN:SUB ;


: CTX-BUF ( n -- SPAN:span<u8> )
   REGION @ CTX-OFF SHA1:CTX-BYTES SPAN:SUB ;


: DIGEST-BUF ( n -- SPAN:span<u8> )
   REGION @ DIGEST-OFF SHA1:DIGEST-BYTES SPAN:SUB ;


: NONCE-BUF ( n -- SPAN:span<u8> )
   NONCE-CAP * NONCES swap NONCE-CAP SPAN:SUB ;


\ ---- writing frames ----------------------------------------------------------

\ The one rule a write on the slot gives up by, weighed before each of its send
\ slices: its peer has accepted no byte since `since` for STALL-MS; the slot's
\ worker, come for the lock (CLAIM), is owed it by now; or the server has been
\ stopping for HTTP:RELEASE-MS. The stop is timed from the first look that
\ finds it, so every frame written while it lasts shares that one RELEASE-MS,
\ as every frame written under a claim shares its deadline: once it has
\ passed, the write in hand gives up when its slice ends and every later write
\ before its first, however many bytes the peer goes on taking.
: GIVE-UP? ( n n -- bool )
   {: since:n idx:n :}
   mono-ns since - STALL-MS NS-PER-MS * >= if true exit then
   idx DUE atomic@ {: due:n :}
   due 0 <> mono-ns due >= and if true exit then
   HTTP:STOPPING? 0= if false exit then
   idx CUT-AT @ 0= if mono-ns idx CUT-AT ! then
   mono-ns idx CUT-AT @ - HTTP:RELEASE-MS NS-PER-MS * >= ;


\ Every byte on the slot's connection in send slices, each over within
\ TCP4:SEND-SLICE-MS and two sends that never wait, or false once the connection
\ fails or the write gives up (GIVE-UP?), which it weighs before every slice. It
\ pauses after each slice that leaves bytes to send, where a halt ends the task,
\ and times progress when a slice that moved bytes returns. A slice that finds
\ no room sets STALLED, for lib/net/ws-test.f to wait on from another task; a
\ slice that moves bytes, and the write's end, clear it.
: PUT ( n ptr u8 n -- bool )
   {: idx:n a u:n :}
   u 0 <= if true exit then
   0 mono-ns                         \ the bytes written, and when the last of them moved
   begin {: done:n since:n :}
      since idx GIVE-UP? if false idx STALLED atomic! false exit then
      idx LINK @ a done + u done - TCP4:TRANSFER-BYTES TCP4:SEND-SLICE
      MATCH TCP4:slice-result
         moved OF BLEN>N done + false idx STALLED atomic! mono-ns ENDOF
         empty OF true idx STALLED atomic! done since ENDOF
         failed OF drop false idx STALLED atomic! false exit ENDOF
      ;MATCH
      over u >= if 2drop true exit then
      TASK:PAUSE
   again ;


\ One unfragmented frame as a server sends it. A frame that fits the scratch
\ goes out in one write, header and payload together; a longer payload follows
\ its header in a write of its own.
: FRAME-OUT ( n opcode ptr u8 n -- bool )
   {: idx:n op a u:n :}
   true op u 0 WS-HEADER:MAKE WS-SENDER:server idx TX-BUF ENCODE-HEADER {: size:n :}
   size u + TX-CAP <= if
      a u idx TX-BUF size SPAN:SKIP SPAN:COPY
      idx idx TX-BUF size u + SPAN:TAKE SPAN:$ PUT exit
   then
   idx idx TX-BUF size SPAN:TAKE SPAN:$ PUT 0= if false exit then
   idx a u PUT ;


\ A close frame carrying the status; an empty one for the status that stands
\ for none, and no frame for the status that stands for a lost connection.
: CLOSE-OUT ( n n -- bool )
   {: idx:n code:n :}
   code STATUS-LOST = if true exit then
   code STATUS-NONE = if 0 else CODE-BYTES then {: u:n :}
   true WS-OPCODE:close u 0 WS-HEADER:MAKE WS-SENDER:server idx TX-BUF ENCODE-HEADER {: size:n :}
   u 0 > if code >CLOSE-CODE idx TX-BUF size SPAN:SKIP CLOSE-CODE! then
   idx idx TX-BUF size u + SPAN:TAKE SPAN:$ PUT ;


\ The connection failed, or its peer stalled, under a write.
: LOST ( n -- ) {: idx:n :}
   STATUS-LOST idx STATUS !
   PHASE-CLOSED idx PHASE atomic! ;


\ ---- under the slot's lock ---------------------------------------------------

\ The two sections below run with the slot's lock held. Their operands cross
\ the catch on the stack and come back as they went, so the lock is given back
\ however the section ends.

\ Whether the slot's socket takes a frame of this opcode: an open one takes any,
\ and one that has sent its close still owes control frames, the pong a ping
\ asks for (section 5.5.2), though no more data (section 5.5.1).
: TAKES? ( n n -- bool ) {: idx:n wire:n :}
   idx PHASE @ {: phase:n :}
   phase PHASE-OPEN = if true exit then
   phase PHASE-CLOSING = wire WIRE>OPCODE CONTROL? and ;


\ A frame through a handle, if the handle's socket takes it.
: SHIP ( n n ptr u8 n -- ) {: h:n wire:n a u:n :}
   h >SLOT {: idx:n :}
   0 idx SENT !
   h CURRENT? 0= if exit then
   idx wire TAKES? 0= if exit then
   idx wire WIRE>OPCODE a u FRAME-OUT 0= if idx LOST exit then
   1 idx SENT ! ;


: SHIP-KEPT ( n n ptr u8 n -- n n ptr u8 n ) {: h:n wire:n a u:n :}
   h wire a u SHIP
   h wire a u ;


\ True when the frame was written; false when the socket does not take it.
: SEND? ( n n ptr u8 n -- bool ) {: h:n wire:n a u:n :}
   u 0 < if E-WS-LENGTH throw then
   h >SLOT {: idx:n :}
   idx LOCK TASK:GET
   h wire a u [: SHIP-KEPT ;] catch {: code:n :} drop drop drop drop
   idx SENT @ 0 <> {: written:bool :}
   idx LOCK TASK:RELEASE
   code 0 <> if code throw then
   written ;


\ The socket moves toward its end. An open socket first sends the close frame
\ the status calls for, and is lost, under 1006, when that frame cannot be
\ written. Then, by the phase asked for: none, the worker has let the socket go
\ and the handle names nothing; closing, an open socket awaits the peer's
\ close; closed, a socket not closed already reports the status.
: FINISHED ( n n n -- )
   {: h:n code:n after:n :}
   h CURRENT? 0= if exit then
   h >SLOT {: idx:n :}
   idx PHASE @ PHASE-OPEN = if
      idx code CLOSE-OUT 0= if idx LOST then
   then
   after PHASE-NONE = if PHASE-NONE idx PHASE atomic! exit then
   idx PHASE @ PHASE-CLOSED = if exit then
   after PHASE-CLOSING = if PHASE-CLOSING idx PHASE atomic! exit then
   code idx STATUS !
   PHASE-CLOSED idx PHASE atomic! ;


: FINISH-KEPT ( n n n -- n n n )
   {: h:n code:n after:n :}
   h code after FINISHED
   h code after ;


\ The section with the slot's lock, from any task.
: FINISH ( n n n -- )
   {: h:n code:n after:n :}
   h >SLOT {: idx:n :}
   idx LOCK TASK:GET
   h code after [: FINISH-KEPT ;] catch {: failed:n :} drop drop drop
   idx LOCK TASK:RELEASE
   failed 0 <> if failed throw then ;


\ The slot's worker comes for the lock, to answer its peer, to let the socket
\ go or to retire it, and is owed the lock within that bound: every write on
\ the slot until UNCLAIM gives up at that deadline (GIVE-UP?) - a push ahead
\ of the worker, however many bytes its peer goes on taking, and then the
\ worker's own - so the worker waits no longer for the lock and its own write
\ together. Only the slot's own worker claims its lock, so one cell a slot is
\ enough.
: CLAIM ( n ms -- )
   {: idx:n limit:ms :}
   limit MS>N NS-PER-MS * mono-ns + idx DUE atomic!
   idx LOCK TASK:GET ;


\ The deadline is gone before the lock is, so no later holder reads it.
: UNCLAIM ( n -- )
   {: idx:n :}
   0 idx DUE atomic!
   idx LOCK TASK:RELEASE ;


\ The section under the worker's claim on the lock.
: FINISH-CLAIMED ( n n n ms -- )
   {: idx:n code:n after:n limit:ms :}
   idx limit CLAIM
   idx HANDLE code after [: FINISH-KEPT ;] catch {: failed:n :} drop drop drop
   idx UNCLAIM
   failed 0 <> if failed throw then ;


\ ---- reading frames ----------------------------------------------------------

\ When a wait of that long ends. One no readiness poll could be asked for is
\ refused here, before anything is read.
: DEADLINE ( ms -- n ) {: limit:ms :}
   limit MS>N 0 < limit MS>N WAIT-MAX > or if E-WS-WAIT throw then
   limit MS>N NS-PER-MS * mono-ns + ;


\ What is left of the deadline, as the timed poll takes it: a deadline already
\ passed asks the question once without waiting.
: REMAINING ( n -- ms ) {: deadline:n :}
   deadline mono-ns - {: left:n :}
   left 0 <= if 0 >MS exit then
   left NS-PER-MS / >MS ;


\ When the slice of the wait now beginning ends: at the deadline, or
\ HTTP:STOP-WAIT-MS from now if that comes first, so the wait reads
\ HTTP:STOPPING? again at least that often.
: SLICE-END ( n -- n ) {: deadline:n :}
   mono-ns HTTP:STOP-WAIT-MS NS-PER-MS * + deadline min ;


: READABLE? ( n n -- bool ) {: idx:n deadline:n :}
   idx LINK @ deadline REMAINING TCP4:READABLE-WITHIN?
   MATCH TCP4:ready-result
      ready OF true ENDOF
      idle OF false ENDOF
      failed OF drop true ENDOF
   ;MATCH ;


\ The bytes the peer sent behind its handshake are the stream's first.
: TAKE-EARLY ( n SPAN:span<u8> -- n ) {: idx:n dst :}
   dst SPAN:LEN idx EARLY-U @ min {: take:n :}
   idx EARLY @ take dst SPAN:COPY
   idx EARLY @ take + idx EARLY !
   idx EARLY-U @ take - idx EARLY-U !
   take ;


\ Bytes into the span, as many as are there and no more than it holds: their
\ count, 0 when the deadline passed first, -1 when the stream ended or failed.
: PULL ( n SPAN:span<u8> n -- n ) {: idx:n dst deadline:n :}
   idx EARLY-U @ 0 > if idx dst TAKE-EARLY exit then
   idx deadline READABLE? 0= if 0 exit then
   idx LINK @ dst SPAN:$ TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N ENDOF
      closed OF drop -1 ENDOF
      failed OF drop -1 ENDOF
   ;MATCH ;


: UNREAD ( n -- progress ) {: got:n :}
   got 0= if construct progress late exit then
   construct progress gone ;


: THROUGH? ( progress -- bool )
   MATCH progress
      through OF true ENDOF
      late OF false ENDOF
      gone OF false ENDOF
      fault OF false ENDOF
   ;MATCH ;


: FRAME-RESET ( n -- ) {: idx:n :}
   0 idx HEAD-U !
   2 idx HEAD-WANT !
   0 idx HEAD-DONE !
   0 idx PAY-U ! ;


: KEEP-FRAME ( header n n -- ) {: h size:n idx:n :}
   h WS-HEADER:UNMAKE {: fin:bool op len:n key:n :}
   fin if 1 else 0 then idx F-FIN !
   op OPCODE>WIRE idx F-WIRE !
   len idx F-LEN !
   key idx F-KEY !
   1 idx HEAD-DONE ! ;


\ What the bytes held say: the count the header needs, or the header itself.
\ The codec throws what breaks the protocol, so this runs under a catch.
: DECODE-HELD ( n -- n ) {: idx:n :}
   idx HEAD-BUF SPAN:$ drop idx HEAD-U @ WS-SENDER:client MESSAGE-MAX DECODE-HEADER
   MATCH decoded
      need OF idx HEAD-WANT ! ENDOF
      frame OF idx KEEP-FRAME ENDOF
   ;MATCH
   idx ;


: FRAME-FAULT? ( n -- bool ) {: code:n :}
   code E-WS-RSV <= code E-WS-MASK >= and ;


: CONTROL-FRAME? ( n -- bool )
   F-WIRE @ WIRE>OPCODE CONTROL? ;


\ 0 for a frame that may come now, else the status it fails with. A message's
\ first frame names its kind and the rest continue it (section 5.4), and a
\ message has MESSAGE-MAX bytes of room.
: ORDER-STATUS ( n -- n ) {: idx:n :}
   idx CONTROL-FRAME? if 0 exit then
   idx F-WIRE @ 0= idx MSG-WIRE @ 0 <> xor if STATUS-PROTOCOL exit then
   idx MSG-U @ idx F-LEN @ + MESSAGE-MAX > if STATUS-TOO-BIG exit then
   0 ;


\ 0 while the header bytes held break no rule, else the status they fail with.
\ A throw that is not a frame's fault is not this word's to answer.
: HEADER-STATUS ( n -- n ) {: idx:n :}
   idx [: DECODE-HELD ;] catch {: code:n :} drop
   code 0 <> if
      code FRAME-FAULT? 0= if code throw then
      code E-WS-TOO-BIG = if STATUS-TOO-BIG exit then
      STATUS-PROTOCOL exit
   then
   idx HEAD-DONE @ 0= if 0 exit then
   idx ORDER-STATUS ;


\ The header is decoded after every read, so a fault shows in the fewest bytes
\ and a header cut short by the deadline is taken up where it stopped.
: READ-HEADER ( n n -- progress ) {: idx:n deadline:n :}
   begin
      idx HEAD-DONE @ 0 <> if construct progress through exit then
      idx idx HEAD-BUF idx HEAD-U @ idx HEAD-WANT @ idx HEAD-U @ - SPAN:SUB deadline PULL
      {: got:n :}
      got 0 <= if got UNREAD exit then
      got idx HEAD-U +!
      idx HEADER-STATUS {: status:n :}
      status 0 <> if status idx FAILING ! construct progress fault exit then
   again ;


\ Where the frame in hand keeps its payload: a control frame's in the control
\ buffer, a data frame's behind what its message already holds.
: PAYLOAD ( n -- SPAN:span<u8> ) {: idx:n :}
   idx CONTROL-FRAME? if idx CONTROL-BUF idx F-LEN @ SPAN:TAKE exit then
   idx MSG-BUF idx MSG-U @ idx F-LEN @ SPAN:SUB ;


\ The payload as it comes, read once whatever the deadline and again only
\ while the deadline has not passed, so bytes that keep arriving hold the read
\ no longer than bytes that stop: a payload cut short by the deadline is taken
\ up where it stopped.
: READ-PAYLOAD ( n n -- progress )
   {: idx:n deadline:n :}
   begin
      idx PAY-U @ idx F-LEN @ >= if construct progress through exit then
      idx idx PAYLOAD idx PAY-U @ SPAN:SKIP deadline PULL {: got:n :}
      got 0 <= if got UNREAD exit then
      got idx PAY-U +!
      idx PAY-U @ idx F-LEN @ < mono-ns deadline > and if construct progress late exit then
   again ;


\ One whole frame, its payload unmasked where it lies. The payload is unmasked
\ in one piece, so the key starts at its first byte.
: FRAME-IN ( n n -- progress ) {: idx:n deadline:n :}
   idx deadline READ-HEADER {: head:progress :}
   head THROUGH? 0= if head exit then
   idx deadline READ-PAYLOAD {: body:progress :}
   body THROUGH? 0= if body exit then
   idx F-KEY @ idx PAYLOAD MASK
   construct progress through ;


\ ---- what a frame means ------------------------------------------------------

: UTF8? ( ptr u8 n -- bool ) {: a u:n :}
   0 begin dup u < while
      a u rot UTF8:NEXT
      MATCH UTF8:scalar-step
         scalar OF nip ENDOF
         raw-byte OF 2drop false exit ENDOF
      ;MATCH
   repeat
   drop true ;


\ The socket closes under this status, which goes to the peer in a close frame,
\ due STALL-MS after the worker comes for the lock, when this end has not sent
\ one.
: SHUT ( n n -- )
   {: idx:n code:n :}
   idx code PHASE-CLOSED STALL-MS >MS FINISH-CLAIMED ;


\ The server is stopping, so the socket closes under status 1001, going away
\ (section 7.4.1): an open socket says so in a close frame due HTTP:RELEASE-MS
\ after the worker comes for the lock, as when it lets the socket go, and a
\ closing one, which has sent its close, sends nothing more. A push holding
\ the lock gives up by then and the socket is lost under 1006, sending
\ nothing, as it is when the close frame cannot be written.
: GOING-AWAY ( n -- )
   {: idx:n :}
   idx STATUS-AWAY PHASE-CLOSED HTTP:RELEASE-MS >MS FINISH-CLAIMED ;


: MESSAGE$ ( n -- ptr u8 n ) {: idx:n :}
   idx MSG-BUF idx MSG-U @ SPAN:TAKE SPAN:$ ;


: TEXT? ( n -- bool )
   WS-OPCODE:text OPCODE>WIRE = ;


\ A data frame joins its message and the last one completes it. Text is checked
\ whole, because a scalar may straddle two fragments.
: DATA-IN ( n -- ) {: idx:n :}
   idx F-LEN @ idx MSG-U +!
   idx F-WIRE @ 0 <> if idx F-WIRE @ idx MSG-WIRE ! then
   idx F-FIN @ 0= if exit then
   idx MSG-WIRE @ TEXT? if
      idx MESSAGE$ UTF8? 0= if idx STATUS-DATA SHUT exit then
   then
   1 idx MSG-READY ! ;


\ The pong carries the ping's payload and is due STALL-MS after the worker
\ comes for the lock. A socket that has sent its close still owes it; a
\ closed one owes none.
: PING-IN ( n -- )
   {: idx:n :}
   idx STALL-MS >MS CLAIM
   idx HANDLE WS-OPCODE:pong OPCODE>WIRE idx CONTROL-BUF idx F-LEN @ SPAN:TAKE SPAN:$
   [: SHIP-KEPT ;] catch {: failed:n :} drop drop drop drop
   idx UNCLAIM
   failed 0 <> if failed throw then ;


\ The statuses a close frame may carry (section 7.4): those RFC 6455 and the
\ registry define, 1000 to 1014 without the three that are never sent, and the
\ ranges left to libraries and applications, 3000 to 4999.
: WIRE-STATUS? ( n -- bool ) {: code:n :}
   code 3000 >= code 4999 <= and if true exit then
   code 1000 < code 1014 > or if false exit then
   code 1004 < code 1006 > or ;


\ The status the close frame in hand closes the socket under: the one it
\ carries, 1005 when it carries none, and the fault's own status when the
\ payload is half a status, a status no frame may carry, or a reason that is
\ not UTF-8.
: CLOSE-STATUS ( n -- n ) {: idx:n :}
   idx F-LEN @ {: u:n :}
   u 0= if STATUS-NONE exit then
   u CODE-BYTES < if STATUS-PROTOCOL exit then
   idx CONTROL-BUF SPAN:$ drop CODE-BYTES BE@ {: code:n :}
   code WIRE-STATUS? 0= if STATUS-PROTOCOL exit then
   idx CONTROL-BUF CODE-BYTES u CODE-BYTES - SPAN:SUB SPAN:$ UTF8? 0= if STATUS-DATA exit then
   code ;


: TURN ( n -- ) {: idx:n :}
   idx F-WIRE @ WIRE>OPCODE
   MATCH opcode
      continuation OF idx DATA-IN ENDOF
      text OF idx DATA-IN ENDOF
      binary OF idx DATA-IN ENDOF
      close OF idx idx CLOSE-STATUS SHUT ENDOF
      ping OF idx PING-IN ENDOF
      pong OF ENDOF
   ;MATCH
   idx FRAME-RESET ;


\ One slice of the wait: a frame read and acted on, the slice run out with any
\ frame cut short kept for the next, or the socket shut for a stream that ended
\ or a frame that broke the protocol.
: STEP ( n n -- ) {: idx:n deadline:n :}
   idx deadline SLICE-END FRAME-IN
   MATCH progress
      through OF idx TURN ENDOF
      late OF ENDOF
      gone OF idx STATUS-LOST SHUT ENDOF
      fault OF idx idx FAILING @ SHUT ENDOF
   ;MATCH ;


: DELIVER ( n -- message ) {: idx:n :}
   idx MESSAGE$ {: a u:n :}
   idx MSG-WIRE @ {: wire:n :}
   0 idx MSG-U !
   0 idx MSG-WIRE !
   0 idx MSG-READY !
   wire TEXT? if a u WS-MESSAGE:text exit then
   a u WS-MESSAGE:binary ;


\ Only the worker that accepted a socket receives on it, and only until it has
\ let the socket go.
: OWNED ( socket -- n ) {: sock :}
   sock SOCKET>N {: h:n :}
   h CURRENT? 0= if E-WS-SOCKET throw then
   h >SLOT {: idx:n :}
   idx OWNER @ TASK:SELF-N <> if E-WS-SOCKET throw then
   idx ;


\ RECEIVE's wait, to the deadline, for the slot's socket.
: NEXT-MESSAGE ( n n -- message ) {: idx:n deadline:n :}
   false                                 \ no frame asked for yet
   begin {: asked:bool :}
      idx PHASE atomic@ PHASE-CLOSED = if idx STATUS @ WS-MESSAGE:closed exit then
      idx MSG-READY @ 0 <> if idx DELIVER exit then
      HTTP:STOPPING? if idx GOING-AWAY idx STATUS @ WS-MESSAGE:closed exit then
      asked mono-ns deadline > and if WS-MESSAGE:timeout exit then
      idx deadline STEP
      true
   again ;


\ ---- the handshake -----------------------------------------------------------

: LISTED? ( ptr u8 n ptr u8 n n -- bool ) {: list lu:n token tu:n start:n :}
   list lu COMMA start SPLIT-NEXT {: fa:ptr fu:n next:n found:bool :}
   found 0= if false exit then
   fa fu TRIM token tu STR=CI if true exit then
   next start <= if false exit then
   list lu token tu next RECURSE ;


\ The value of a header the request carries exactly once.
: SOLE ( HTTP:request ptr u8 n -- ptr u8 n bool ) {: asked:HTTP:request name nu:n :}
   asked name nu HTTP:HEADER-COUNT-OF 1 <> if name 0 false exit then
   asked name nu HTTP:HEADER-OF$ ;


: LISTS? ( HTTP:request ptr u8 n ptr u8 n -- bool )
   {: asked:HTTP:request name nu:n token tu:n :}
   asked name nu SOLE {: va vu:n found:bool :}
   found 0= if false exit then
   va vu token tu 0 LISTED? ;


\ The count of bytes the key decodes to.
: DECODE-NONCE ( n -- n )
   {: idx:n :}
   idx KEY-AT @ idx KEY-U @ idx NONCE-BUF BASE64:DECODE ;


\ The key is the base64 of sixteen bytes (section 4.2.1), and is kept as it
\ was sent for the accept value. A throw that is not base64's own refusal is
\ not this word's to answer.
: KEY-OK? ( HTTP:request n -- bool )
   {: asked:HTTP:request idx:n :}
   asked s" sec-websocket-key" SOLE {: va vu:n found:bool :}
   found 0= if false exit then
   vu NONCE-CHARS <> if false exit then
   va idx KEY-AT !
   vu idx KEY-U !
   idx [: DECODE-NONCE ;] catch {: got code:n :}
   code 0= if got NONCE-BYTES = exit then
   code E-BASE64-FIRST > code E-BASE64-LAST < or if code throw then
   false ;


\ An HTTP/1.1 GET (section 4.2.1, item 1). Item 2, exactly one valid Host, the
\ server checks before any handler runs (lib/net/http-request.f HOST-OK?).
: GET-OK? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked HTTP:METHOD$ s" GET" STR= 0= if false exit then
   asked HTTP:VERSION$ s" HTTP/1.1" STR= ;


\ 0 for an opening handshake this server accepts, else the HTTP status that
\ refuses it: 426 when all that is wrong is the version (section 4.4).
: VERDICT ( HTTP:request n -- n ) {: asked:HTTP:request idx:n :}
   asked GET-OK? 0= if 400 exit then
   asked s" upgrade" s" websocket" LISTS? 0= if 400 exit then
   asked s" connection" s" upgrade" LISTS? 0= if 400 exit then
   asked idx KEY-OK? 0= if 400 exit then
   asked s" sec-websocket-version" SOLE {: va vu:n found:bool :}
   found 0= if 426 exit then
   va vu s" 13" STR= 0= if 426 exit then
   0 ;


: REFUSE ( HTTP:response n -- ) {: answer:HTTP:response status:n :}
   status 426 = if
      answer s" Upgrade" s" websocket" HTTP:HEADER!
      answer s" Sec-WebSocket-Version" s" 13" HTTP:HEADER!
      answer 426 s" upgrade_required" s" this server speaks WebSocket version 13" HTTP:ERROR!
      exit
   then
   answer 400 s" bad_handshake" s" the request is not a WebSocket opening handshake" HTTP:ERROR! ;


: GUID$ ( -- ptr u8 n )
   s" 258EAFA5-E914-47DA-95CA-C5AB0DC85B11" ;


\ The accept value into the span (section 4.2.2): the base64 of the SHA-1 of
\ the key as it was sent and the protocol's GUID.
: ACCEPT-VALUE ( n SPAN:span<u8> -- n ) {: idx:n out :}
   idx CTX-BUF SHA1:START
   idx CTX-BUF idx KEY-AT @ idx KEY-U @ SHA1:FEED
   idx CTX-BUF GUID$ SHA1:FEED
   idx CTX-BUF SHA1:FINISH idx DIGEST-BUF SHA1:DIGEST!
   idx DIGEST-BUF SPAN:$ out BASE64:ENCODE ;


: HEAD+ ( n ptr u8 n n -- n ) {: at:n src len:n idx:n :}
   src len idx TX-BUF at SPAN:SKIP SPAN:COPY
   at len + ;


\ The 101 answer in the scratch, and its length.
: SWITCHING ( n -- n ) {: idx:n :}
   0 S\" HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n" idx HEAD+
   S\" Connection: Upgrade\r\nSec-WebSocket-Accept: " idx HEAD+ {: at:n :}
   idx idx TX-BUF at SPAN:SKIP ACCEPT-VALUE at +
   S\" \r\n\r\n" idx HEAD+ ;


\ ---- a socket's birth and end ------------------------------------------------

\ The slot's socket, if it has one, is gone: the count moves, so a handle from
\ before names nothing from here on, whatever the slot holds next. Runs with
\ the slot's lock held.
: RETIRED ( n -- ) {: idx:n :}
   idx GEN @ 1+ idx GEN !
   PHASE-NONE idx PHASE ! ;


: RETIRE ( n -- ) {: idx:n :}
   idx LOCK TASK:GET
   idx RETIRED
   idx LOCK TASK:RELEASE ;


\ A socket is published under its slot's lock, in the phase and with the
\ status given. The slot was retired before anything was kept for the socket,
\ so a handle an earlier socket left behind never reads a slot half written.
: BEGET ( n n n -- socket ) {: idx:n phase:n status:n :}
   idx LOCK TASK:GET
   status idx STATUS !
   TASK:SELF-N idx OWNER !
   phase idx PHASE !
   idx LOCK TASK:RELEASE
   idx HANDLE >SOCKET ;


\ The mapping is given back, and a span with no reach is left in its place.
: DROP-REGION ( n -- ) {: idx:n :}
   idx REGION @ SPAN:LEN 0= if exit then
   idx REGION @ MEM:FREE-SPAN
   idx REGION @ 0 SPAN:TAKE idx REGION ! ;


\ The worker lets its socket go, handed the code the handler threw: a socket
\ still open is closed normally, or as an internal error when the handler
\ threw, in a goodbye due HTTP:RELEASE-MS after the worker comes for the lock,
\ which a send held in a write gives up by as well, and the mapping is given
\ back.
: LET-GO ( n -- )
   {: code:n :}
   HTTP:WORKER-SLOT {: idx:n :}
   code 0= if STATUS-NORMAL else STATUS-INTERNAL then {: status:n :}
   idx status PHASE-NONE HTTP:RELEASE-MS >MS FINISH-CLAIMED
   idx DROP-REGION ;


\ Nagle's algorithm off, so a frame leaves at once instead of waiting for the
\ peer to acknowledge the one before it.
: NAGLE-OFF? ( TCP4:connection -- bool )
   true TCP4:NODELAY!
   MATCH TCP4:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ The connection sends without delay and has been answered 101, or it refused
\ one or the other.
: SWITCHED? ( n -- bool )
   {: idx:n :}
   idx LINK @ NAGLE-OFF? 0= if false exit then
   idx idx TX-BUF idx SWITCHING SPAN:TAKE SPAN:$ PUT ;


\ The connection is taken before anything is kept for it, so a request that
\ was already taken changes nothing. The slot is retired next, so a handle its
\ last socket left names nothing by the time the new connection is kept. The
\ 101 is written before the socket is open to any other task; a connection
\ that refuses TCP_NODELAY, or fails under the 101, is a socket born closed.
: OPEN-SOCKET ( HTTP:request n -- socket ) {: asked:HTTP:request idx:n :}
   asked [: LET-GO ;] HTTP:TAKE-OVER {: conn:TCP4:connection early eu:n :}
   idx RETIRE
   conn idx LINK !
   early idx EARLY !
   eu idx EARLY-U !
   REGION-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN idx REGION !
   idx FRAME-RESET
   0 idx MSG-U !
   0 idx MSG-WIRE !
   0 idx MSG-READY !
   0 idx CUT-AT !
   false idx STALLED atomic!
   idx SWITCHED? if idx PHASE-OPEN 0 BEGET exit then
   idx PHASE-CLOSED STATUS-LOST BEGET ;


: SWEEP-KEPT ( n -- n ) {: idx:n :}
   idx RETIRED
   idx DROP-REGION
   idx ;


\ Every worker's exit runs this, however the worker ended. One that a stop
\ killed inside its handler never let its socket go, so the socket is retired
\ and its mapping given back here, sending nothing; the HTTP worker's exit then
\ closes the connection. The worker claims the lock before the sweep, so
\ nothing is retired under another task's send: one held in a write gives the
\ lock up as it does for the goodbye (CLAIM). A stop cuts every write on the
\ slot, which gives up within the stop's bound (GIVE-UP?, lib/net/http.f
\ STOP-BOUND-MS). A worker the stop kills inside its own write, past that
\ bound, ends at the write's pause without unwinding its catches, its slot's
\ lock still held; taking the lock again is nothing new for its owner.
: RELEASE-SLOT ( -- )
   HTTP:WORKER-SLOT {: idx:n :}
   idx HTTP:RELEASE-MS >MS CLAIM
   idx [: SWEEP-KEPT ;] catch {: code:n :} drop
   idx UNCLAIM
   code 0 <> if code throw then ;


\ No server is running while this file loads, so the hook goes in here.
: HOOK-EXIT ( -- )
   [: RELEASE-SLOT ;] HTTP:ON-WORKER-EXIT ;

HOOK-EXIT


public

\ Inside a route handler: the request is checked as an opening handshake, that
\ is an HTTP/1.1 GET whose Upgrade names websocket, whose Connection names
\ Upgrade, whose Sec-WebSocket-Key is the base64 of sixteen bytes and whose
\ Sec-WebSocket-Version is 13, each of those four arriving once; the server
\ refuses an HTTP/1.1 request without exactly one valid Host before any
\ handler runs. A handshake that passes is answered 101 at once, and `open`
\ carries the socket the handler owns until it returns; when the 101 cannot
\ be written, the socket is closed from birth under status 1006 and its first
\ RECEIVE says so. One that does not pass is answered 400, or 426 naming
\ version 13, through the response once the handler returns, and `refused`
\ carries that status: no socket exists for it. Origin is the handler's to
\ check before it accepts.
: ACCEPT ( HTTP:request HTTP:response -- accepted )
   {: asked:HTTP:request answer:HTTP:response :}
   HTTP:WORKER-SLOT {: idx:n :}
   asked idx VERDICT {: status:n :}
   status 0 <> if
      answer status REFUSE
      status WS-ACCEPTED:refused exit
   then
   asked idx OPEN-SOCKET WS-ACCEPTED:open ;


\ The next whole message, waiting that long for it. `text` and `binary` carry
\ the message's bytes, borrowed until the socket's next RECEIVE; `closed`
\ carries the status the socket closed under, and is every later answer too;
\ `timeout` leaves a frame that was cut short to be taken up by the next call.
\ Only the worker that accepted the socket may call it: anything else is
\ E-WS-SOCKET. A wait below zero is E-WS-WAIT.
\
\ The wait is absolute: past it no frame is begun but the first a call asks
\ for, and a payload still arriving is left after the read in hand for the
\ next call, so a wait of zero asks once and a peer that keeps bytes queued or
\ coming cannot hold the call. It runs in slices of at most HTTP:STOP-WAIT-MS,
\ each begun by reading HTTP:STOPPING? and each ending mid-frame the same way:
\ once the server stops, an open or closing socket is closed under 1001
\ (GOING-AWAY), or lost under 1006 when that close cannot be written, and the
\ call answers that.
\ The frame read last is acted on even past the wait: the pong a ping is owed,
\ and the close that answers a close, a fault or bad text, are due STALL-MS
\ after the worker comes for the slot's lock (CLAIM). So outside a stop the
\ call ends at most STALL-MS and one send slice past its wait - at most
\ TCP4:SEND-SLICE-MS and two sends that never wait - whether the answer stalls
\ or a push it waits behind is stalled or still moving; a push that gives up
\ loses the socket, and the answer is not written. While the server stops,
\ every write the call makes or waits behind is cut short, so the call answers
\ within a taker's share of the stop's bound (GIVE-UP?, lib/net/http.f
\ STOP-BOUND-MS).
: RECEIVE ( socket ms -- message ) {: sock limit:ms :}
   limit DEADLINE {: deadline:n :}
   sock OWNED deadline NEXT-MESSAGE ;


\ One text message in one frame, from any task. Bytes that are not UTF-8 are
\ E-WS-TEXT, refused before the slot's lock is taken, since a text frame
\ carries only UTF-8 (section 5.6). E-WS-CLOSED when the socket is not open
\ to it.
: SEND-TEXT ( socket ptr u8 n -- ) {: sock a u:n :}
   a u UTF8? 0= if E-WS-TEXT throw then
   sock SOCKET>N WS-OPCODE:text OPCODE>WIRE a u SEND? 0= if E-WS-CLOSED throw then ;


: SEND-BINARY ( socket ptr u8 n -- ) {: sock a u:n :}
   sock SOCKET>N WS-OPCODE:binary OPCODE>WIRE a u SEND? 0= if E-WS-CLOSED throw then ;


\ Starts the closing handshake under that status, from any task: the close
\ frame is sent, nothing may be sent behind it, and the handler goes on
\ receiving until the peer's close arrives. A socket already closing, closed or
\ past its handler is left as it is. A status no frame may carry is E-WS-CODE.
: CLOSE ( socket n -- )
   {: sock code:n :}
   code WIRE-STATUS? 0= if E-WS-CODE throw then
   sock SOCKET>N code PHASE-CLOSING FINISH ;

;package
