split-str
\ httpc.f - one HTTP/1.1 client connection to one origin: dialing by name, a
\ request written once, the answer read in pieces by its framing, and the
\ connection kept for the next request when the answer allows it, over
\ `http://` or `https://`. No redirects, cookies, content decoding or retries.
\
\ Contract. One task uses a conn at a time, and every wait is a poll(2) on the
\ calling task, bounded by DEADLINES!. HTTP/1.1 only. A request's bytes are
\ written at most once: HTTPC never resends, so a request lost on a reused
\ connection is the caller's to classify. No proxy support: the proxy
\ variables are ignored. No HEAD requests, whose answers would misframe.
\ Linux only. A record is OPENed when fresh or for another origin; a CLOSEd
\ conn keeps its origin, trust and room and dials again at its next SEND. Each
\ exchange is REQUEST, HEADER+ and DEADLINES! as needed, one SEND, then NEXT
\ until it answers `response` or `failed`; a NEXT before the first SEND or
\ after CLOSE answers `failed` with fault `send`. One task at a time uses HTTPC
\ over https, as TLS's contract requires: the TLS library keeps process-wide
\ state and no lock.
\ STORAGE CLASS. CALLER-OWNED: the conn record, the origin, trust and room
\ spans OPEN borrows until the record is dropped or OPENed again, SEND's body
\ until SEND returns and NEXT's span until NEXT returns. The record holds its
\ TLS session inline, so it stays at one address while it holds a connection.
\ HTTPC allocates nothing; mbedTLS allocates its own buffers from a session's
\ START to its STOP.
\
\ The lifecycle is the LIFECYCLE table. SEND runs it from `starting`, NEXT from
\ where the last run rested (`heading` after `sent`, `reading` after
\ `partway`; a rest that ended the exchange answers again). Every fault hangs
\ up the connection before the machine rests in `down`, so the fault is kept
\ for the first NEXT. A held connection is probed with one non-blocking peek
\ before reuse: anything but "no byte and no close" drops it before a request
\ byte is written. It is reused only after an answer framed by status, length
\ or chunks was read whole, on HTTP/1.1, with no `Connection: close` and no
\ byte left past it. The room holds the request head while it is built and
\ sent, then the answer head; bytes read past the head are handed out before
\ the wire. The chunk decoder keeps only the bytes left in the current chunk:
\ at zero it reads a size line, and a chunk's data read whole is followed at
\ once by its CRLF. The answer's head, its interim heads together, each
\ chunk-size line and its trailer section are each bounded by the room, past
\ which NEXT fails `oversize`. Each head's first line is judged as it arrives:
\ bytes held that cannot begin `HTTP/1.x NNN` and a space or CR fail
\ `malformed` before any further read, so a close or a full room after them
\ is not `cut` or `oversize`; a whole line whose status is outside 100 to
\ 599 fails `malformed` too. Dialing tries every address
\ getaddrinfo answers, in order, each with the time left divided by the
\ addresses left.
\ A request marked malformed or oversize while it was built fails at
\ `starting`, with nothing dialed or written, and drops any connection the
\ record held.
\
\ An https origin adds TLS over the connected socket. Its greeting STARTs the
\ conn's own TLS session with the host and the trust file, then steps the
\ handshake, waiting on the socket as each step asks within the deadlines: a
\ certificate that does not verify, or a trust file that cannot be read, is
\ `trust` with the verify flags or mbedTLS's code as CAUSE; any other failure
\ is `handshake`. Every receive and write then goes through the session, and
\ a write asked to wait is made again with the same bytes, as TLS requires.
\ Only the peer's close_notify is its end of stream: a transport that closes
\ without one is `cut` at any point of an answer, a body read to the close
\ included, since a stream cut at a record boundary would otherwise end that
\ body short and whole. A connection is reused only when its session holds
\ no byte READ has not handed out (TLS's HELD?): a poll of the socket cannot
\ see bytes mbedTLS has read into the session, and the next answer would be
\ read from them. The probe stays a peek at the socket, since nothing reads
\ the session between that check and the probe, so a TLS record held on the
\ socket makes the connection stale too. Hanging up STOPs a session that
\ started before the socket is closed.
HB-TARGET-LINUX-KERNEL? ?TARGET
require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/le.f
require src/lib/fd.f
require src/lib/ffi-abi.f
require src/lib/adt/option.f
require src/lib/fsm.f
require src/lib/net/tls.f

package HTTPC
public

\ Why an exchange failed; LOST? says which faults may succeed if tried again.
ENUM fault resolve connect handshake trust send receive cut timeout
   malformed oversize ;ENUM

\ One NEXT's answer: `chunk` has the status and the body bytes now in the
\ span, `response` the status once the body has ended.
SUMTYPE chunk-result
   VARIANT chunk n len ;VARIANT
   VARIANT response n ;VARIANT
   VARIANT failed fault ;VARIANT
;SUMTYPE

private

ENUM phase starting probing resolving connecting greeting sending heading
   reading settling sent partway idle closed down ;ENUM
ENUM held none idle busy ;ENUM
ENUM intake marked none idle busy ;ENUM
ENUM probe quiet stale ;ENUM
ENUM lookup found failed ;ENUM
ENUM dial up next failed ;ENUM
ENUM greet up failed ;ENUM
\ How one TLS step of the greeting ended: `done`, `waited` once the socket is
\ ready as a want asked, or `failed` with the fault kept.
ENUM shake done waited failed ;ENUM
ENUM delivery sent failed ;ENUM
ENUM head-read interim head failed ;ENUM
ENUM piece some end failed ;ENUM
ENUM reuse keep shut ;ENUM
ENUM framing empty sized chunked closing ;ENUM

\ What a receive answered: bytes, the peer's end of stream, or a fault kept.
SUMTYPE take
   VARIANT got n ;VARIANT
   VARIANT eof ;VARIANT
   VARIANT broke ;VARIANT
;SUMTYPE

public

\ found, opt and opt-len are raw bytes the OS writes: getaddrinfo's result
\ pointer, and getsockopt's value and length.
STRUCTURE conn OPAQUE DERIVE addr
   FIELD fd n
   FIELD held held
   FIELD resume phase
   FIELD answer chunk-result
   FIELD fault fault
   FIELD cause n
   FIELD authority ^u8
   FIELD authority-len n
   FIELD host-len n
   FIELD secure bool
   FIELD trust ^u8
   FIELD trust-len n
   FIELD room ^u8
   FIELD room-len n
   FIELD used n
   FIELD mark option<fault>
   FIELD get-method bool
   FIELD total n
   FIELD stall n
   FIELD ends n
   FIELD list n
   FIELD addr n
   FIELD addrs-left n
   FIELD body ^u8
   FIELD body-len n
   FIELD filled n
   FIELD head-len n
   FIELD taken n
   FIELD interims n
   FIELD status n
   FIELD minor n
   FIELD chunked bool
   FIELD length option<n>
   FIELD close-asked bool
   FIELD framing framing
   FIELD left n
   FIELD span ^u8
   FIELD span-len n
   FIELD got n
   BUFFER node 256
   BUFFER service 8
   BUFFER hints 48
   FIELD pollfd FD:pollfd
   FIELD byte u8
   BUFFER found 8
   BUFFER opt 4
   BUFFER opt-len 4
   FIELD tls TLS:session
;STRUCTURE

private

253 constant HOST-MOST             \ RFC 1035's longest name text
5 constant PORT-DIGITS
65535 constant PORT-MOST
15 constant SIZE-DIGITS            \ hex digits a positive chunk size may have
$80801 constant SOCKET-FLAGS       \ SOCK_STREAM | SOCK_NONBLOCK | SOCK_CLOEXEC
$4000 constant MSG-NOSIGNAL
$42 constant PEEK-FLAGS            \ MSG_DONTWAIT | MSG_PEEK
$01 constant POLLIN
$04 constant POLLOUT
1 constant SOL-SOCKET
4 constant SO-ERROR
4 constant EINTR
11 constant EAGAIN
115 constant EINPROGRESS
\ glibc's struct addrinfo on a 64-bit Linux target.
4 constant AI-FAMILY
8 constant AI-SOCKTYPE
12 constant AI-PROTOCOL
16 constant AI-ADDRLEN
24 constant AI-ADDR
40 constant AI-NEXT
48 constant AI-BYTES
1 constant SOCK-STREAM
$7FFFFFFF constant MAX-TIMEOUT     \ poll(2) takes an int of milliseconds.
1000000 constant NS-PER-MS
$7FFFFFFFFFFFFFFF constant NO-DEADLINE   \ a mono-ns deadline that never passes.

\ An addrinfo node's address, which getaddrinfo answers as a cell.
CAST: N>BYTES ( n -- ^u8 )
\ The conn's TLS session as bytes, which OPEN zeroes.
CAST: SESSION>BYTES ( ^TLS:session -- ^u8 )

PROCESS-SYMBOLS

FUNCTION: LOOKUP-CALL getaddrinfo ( ^u8 ^u8 ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: FREE-CALL freeaddrinfo ( n -- ) ;FUNCTION
FUNCTION: SOCKET-CALL socket ( n n n -- i32 ) ;FUNCTION
FUNCTION: CONNECT-CALL connect ( n n n -- i32 ) ;FUNCTION
FUNCTION: OPTION-CALL getsockopt ( n n n ^u8 ^u8 -- i32 ) ;FUNCTION
FUNCTION: SEND-CALL send ( n str n -- n ) ;FUNCTION
FUNCTION: RECEIVE-CALL recv ( n ^u8 n n -- n ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION


: NODE ( ^conn -- ^u8 ) CONN-NODE SPAN:$ drop ;
: SERVICE ( ^conn -- ^u8 ) CONN-SERVICE SPAN:$ drop ;
: HINTS ( ^conn -- ^u8 ) CONN-HINTS SPAN:$ drop ;
: ROOM ( ^conn -- ^u8 ) CONN-ROOM @ ;


: FAIL ( ^conn fault n -- ) {: c f:fault e:n :}
   f c CONN-FAULT !  e c CONN-CAUSE ! ;

: CUT ( ^conn -- ) construct fault cut 0 FAIL ;
: MALFORMED ( ^conn -- ) construct fault malformed 0 FAIL ;
: TIMED-OUT ( ^conn -- ) construct fault timeout 0 FAIL ;
: OVERSIZED ( ^conn -- ) construct fault oversize 0 FAIL ;


\ STOPs the TLS session of an https conn, which holds nothing unless GREET
\ STARTed it on the socket, then closes the socket the record holds, if any.
: HANG-UP ( ^conn -- ) {: c :}
   c CONN-SECURE @ if c CONN-TLS TLS:STOP then
   c CONN-FD @ 0 >= if c CONN-FD @ CLOSE-CALL drop then
   -1 c CONN-FD ! ;

\ The conn's TLS session zeroed, which TLS reads as one holding nothing.
: CLEAR-TLS ( ^conn -- ) {: c :}
   c CONN-TLS SESSION>BYTES {: a:^ :}
   [type] TLS:session width>n 0 ?do 0 a i + c! loop ;


\ ---- Waiting

: STALL-END ( ^conn -- n ) {: c :}
   c CONN-STALL @ 0= if NO-DEADLINE exit then
   c CONN-STALL @ NS-PER-MS * mono-ns + ;

\ When a single wait begins: the stall from now, within the exchange's total.
: WAIT-END ( ^conn -- n ) dup STALL-END swap CONN-ENDS @ min ;

\ Milliseconds left before a mono-ns deadline, rounded up so a poll cannot end
\ before it; -1, no timeout, for none.
: TIMEOUT-MS ( n -- n ) {: deadline:n :}
   deadline NO-DEADLINE = if -1 exit then
   deadline mono-ns - {: left:n :}
   left 0 <= if 0 exit then
   left NS-PER-MS 1- + NS-PER-MS / MAX-TIMEOUT min ;

\ One poll(2) of the socket: how many are ready, 0 after the timeout, or the
\ errno negated.
: POLL-ONCE ( ^conn n n -- n ) {: c events:n ms:n :}
   c CONN-FD @ >FD events FD:>POLLFD c CONN-POLLFD !
   c CONN-POLLFD 1 ms poll ;

\ True once the socket is ready for the events; a signal restarts the wait
\ with what is left of it. False with the fault kept: timeout once the
\ deadline passes, or WHY with poll's errno.
: AWAIT-BY ( ^conn n n fault -- bool ) {: c events:n deadline:n why:fault :}
   begin
      c events deadline TIMEOUT-MS POLL-ONCE {: rc:n :}
      rc 0 > if true exit then
      rc 0= if c TIMED-OUT false exit then
      rc negate EINTR <> if c why rc negate FAIL false exit then
   again ;

: AWAIT ( ^conn n fault -- bool ) {: c events:n why:fault :}
   c events c WAIT-END why AWAIT-BY ;


\ ---- Moving bytes

\ After a refused send or receive: true to try again, after a wait for the
\ events when the socket was not ready; else the fault WHY is kept.
: RETRY? ( ^conn n n fault -- bool ) {: c e:n events:n why:fault :}
   e EINTR = if true exit then
   e EAGAIN = if c events why AWAIT exit then
   c why e FAIL false ;

\ One receive from the socket, waiting while there is nothing to receive yet.
: SOCKET-RECEIVE ( ^conn ^u8 n -- take ) {: c a:^ u:n :}
   begin
      c CONN-FD @ a u 0 RECEIVE-CALL {: rc:n :}
      rc 0 > if rc construct take got exit then
      rc 0= if construct take eof exit then
      c FFI:ERRNO POLLIN construct fault receive RETRY? 0= if
         construct take broke exit
      then
   again ;

\ Writes the whole span to the socket; false with the fault kept.
: SOCKET-WRITE ( ^conn str -- bool ) {: c a:^ u:n :}
   0 begin dup u < while
      {: off:n :}
      c CONN-FD @ a off + u off - MSG-NOSIGNAL SEND-CALL {: rc:n :}
      rc 0 >= if
         off rc +
      else
         c FFI:ERRNO POLLOUT construct fault send RETRY? 0= if false exit then
         off
      then
   repeat drop true ;

\ After a TLS want: true once the socket is ready for the events, else the
\ fault or `timeout` is kept.
: RECEIVE-WAIT? ( ^conn n -- bool ) construct fault receive AWAIT ;
: SEND-WAIT? ( ^conn n -- bool ) construct fault send AWAIT ;

: RECEIVE-FAILED ( ^conn n -- ) {: c e:n :}
   c construct fault receive e FAIL ;

: SEND-FAILED ( ^conn n -- ) {: c e:n :}
   c construct fault send e FAIL ;

\ One receive through the TLS session, waiting as it asks. Its `closed`, the
\ peer's close_notify, is the peer's end; `truncated` is `cut`.
: TLS-RECEIVE ( ^conn ^u8 n -- take ) {: c a:^ u:n :}
   begin
      c CONN-TLS a u TLS:READ MATCH TLS:tls-io
         moved OF LEN>N construct take got exit ENDOF
         want-read OF c POLLIN RECEIVE-WAIT? ENDOF
         want-write OF c POLLOUT RECEIVE-WAIT? ENDOF
         closed OF construct take eof exit ENDOF
         truncated OF c CUT false ENDOF
         failed OF c swap RECEIVE-FAILED false ENDOF
      ;MATCH
   0= until construct take broke ;

\ Writes the whole span through the TLS session; after a want the rest of
\ the span is written again, the same bytes and length. False with the
\ fault kept; a peer that has closed fails the write.
: TLS-WRITE ( ^conn str -- bool ) {: c a:^ u:n :}
   0 begin dup u < while
      {: off:n :}
      c CONN-TLS a off + u off - TLS:WRITE MATCH TLS:tls-io
         moved OF LEN>N off + true ENDOF
         want-read OF off c POLLIN SEND-WAIT? ENDOF
         want-write OF off c POLLOUT SEND-WAIT? ENDOF
         closed OF off c 0 SEND-FAILED false ENDOF
         truncated OF off c 0 SEND-FAILED false ENDOF
         failed OF off c rot SEND-FAILED false ENDOF
      ;MATCH
      0= if drop false exit then
   repeat drop true ;

\ One receive over the conn's socket or its TLS session; `timeout`, with
\ nothing read, once the exchange's total deadline has passed, so a peer
\ that always has a byte ready cannot outlast it.
: RECEIVE ( ^conn ^u8 n -- take ) {: c a:^ u:n :}
   mono-ns c CONN-ENDS @ >= if c TIMED-OUT construct take broke exit then
   c CONN-SECURE @ if c a u TLS-RECEIVE else c a u SOCKET-RECEIVE then ;

\ Writes the whole span over the conn's socket or its TLS session; false
\ with the fault kept.
: WRITE-ALL ( ^conn str -- bool ) {: c a:^ u:n :}
   c CONN-SECURE @ if c a u TLS-WRITE else c a u SOCKET-WRITE then ;

\ Answer bytes not handed out yet that were read past the head.
: EXTRA ( ^conn -- n ) {: c :}
   c CONN-FILLED @ c CONN-HEAD-LEN @ - c CONN-TAKEN @ - ;

\ The one source of answer bytes: those read past the head first, then the
\ wire.
: PULL ( ^conn ^u8 n -- take ) {: c a:^ u:n :}
   c EXTRA {: x:n :}
   x 0= if c a u RECEIVE exit then
   x u min {: k:n :}
   c ROOM c CONN-HEAD-LEN @ + c CONN-TAKEN @ + a k BYTE-COPY
   k c CONN-TAKEN +!
   k construct take got ;

\ One answer byte into the connection's byte; false with the fault kept, a close
\ being `cut`.
: BYTE? ( ^conn -- bool ) {: c :}
   c c CONN-BYTE 1 PULL MATCH take
      got OF drop true ENDOF
      eof OF c CUT false ENDOF
      broke OF false ENDOF
   ;MATCH ;

: LAST-BYTE ( ^conn -- n ) CONN-BYTE c@ ;


\ ---- Building the request head

: MARKED? ( ^conn -- bool )
   CONN-MARK @ MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ The first mark stands: SEND fails with it and writes nothing.
: MARK ( ^conn fault -- ) {: c f:fault :}
   c MARKED? if exit then
   f OPTION:SOME c CONN-MARK ! ;

: APPEND ( ^conn str -- ) {: c a:^ u:n :}
   c MARKED? if exit then
   c CONN-USED @ u + c CONN-ROOM-LEN @ > if
      c construct fault oversize MARK exit
   then
   a c ROOM c CONN-USED @ + u BYTE-COPY
   u c CONN-USED +! ;

: CHAR+ ( ^conn n -- ) {: c b:n :}
   b c CONN-BYTE c!  c c CONN-BYTE 1 APPEND ;

: CRLF+ ( ^conn -- ) {: c :} c 13 CHAR+ c 10 CHAR+ ;

: DIGITS+ ( ^conn n -- ) {: c v:n :}
   v 9 > if c v 10 / RECURSE then
   c v 10 mod [char] 0 + CHAR+ ;

: HAS? ( str n -- bool )
   INDEX-OF MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

: BREAK? ( str -- bool ) {: a:^ u:n :}
   a u 13 HAS? a u 10 HAS? or ;

\ Whether a request line's method or target holds CR, LF or SP.
: SPLITS? ( str -- bool ) {: a:^ u:n :}
   a u BREAK? a u 32 HAS? or ;


\ ---- The origin

: HOST-BYTE? ( n -- bool ) {: b:n :}
   b 33 < b 126 > or if false exit then
   s" /?#@[]" b INDEX-OF MATCH option
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH ;

: HOST-BYTES? ( str -- bool ) {: a:^ u:n :}
   u 0 ?do a i + c@ HOST-BYTE? 0= if false unloop exit then loop
   true ;

\ The host as getaddrinfo takes it, NUL-terminated in the record.
: HOST? ( ^conn str -- bool ) {: c a:^ u:n :}
   u 0= u HOST-MOST > or if false exit then
   a u HOST-BYTES? 0= if false exit then
   a c NODE u BYTE-COPY  0 c NODE u + c!
   u c CONN-HOST-LEN !
   true ;

\ The port's digits as getaddrinfo's service, NUL-terminated in the record.
: PORT? ( ^conn str -- bool ) {: c a:^ u:n :}
   u 0= u PORT-DIGITS > or if false exit then
   a u STR-PARSE-POS MATCH option
      none OF false exit ENDOF
      some OF dup 1 < swap PORT-MOST > or if false exit then ENDOF
   ;MATCH
   a c SERVICE u BYTE-COPY  0 c SERVICE u + c!
   true ;

: HOST-PORT? ( ^conn str n -- bool ) {: c a:^ u:n k:n :}
   c a k HOST?  c a k + 1+ u k - 1- PORT? and ;

\ The host and its port, else the scheme's port P.
: AUTHORITY? ( ^conn str str -- bool )
   {: c a:^ u:n p:^ pu:n :}
   a c CONN-AUTHORITY !  u c CONN-AUTHORITY-LEN !
   a u [char] : INDEX-OF MATCH option
      none OF c a u HOST? c p pu PORT? and ENDOF
      some OF IDX>N {: k:n :} c a u k HOST-PORT? ENDOF
   ;MATCH ;

\ Whether the span starts with the scheme prefix P, in any case.
: SCHEME? ( str str -- bool ) {: a:^ u:n p:^ pu:n :}
   u pu < if false exit then
   a pu p pu STR=CI ;

: ORIGIN? ( ^conn str -- bool ) {: c a:^ u:n :}
   a u s" http://" SCHEME? if
      false c CONN-SECURE !
      c a 7 + u 7 - s" 80" AUTHORITY? exit
   then
   a u s" https://" SCHEME? if
      true c CONN-SECURE !
      c a 8 + u 8 - s" 443" AUTHORITY? exit
   then
   false ;


\ ---- Senses and actions of the lifecycle

\ A marked request, before what the record holds.
: INTAKE ( ^conn -- intake ) {: c :}
   c MARKED? if construct intake marked exit then
   c CONN-HELD @ MATCH held
      none OF construct intake none ENDOF
      idle OF construct intake idle ENDOF
      busy OF construct intake busy ENDOF
   ;MATCH ;

\ Drops any held connection and keeps the mark's fault.
: REFUSE ( ^conn -- ) {: c :}
   c HANG-UP
   c CONN-MARK @ MATCH option
      some OF c swap 0 FAIL ENDOF
      none OF ENDOF
   ;MATCH ;

\ A held connection is quiet when a peek finds no byte and no close.
: PROBE ( ^conn -- probe ) {: c :}
   c CONN-FD @ c CONN-BYTE 1 PEEK-FLAGS RECEIVE-CALL 0 < FFI:ERRNO EAGAIN = and
   if construct probe quiet else construct probe stale then ;

: HINTS! ( ^conn -- ) {: c :}
   AI-BYTES 0 do 0 c HINTS i + c! loop
   SOCK-STREAM c HINTS AI-SOCKTYPE + LE:U32! ;

: COUNT-ADDRS ( n -- n )
   0 swap begin dup 0<> while
      swap 1+ swap N>BYTES AI-NEXT + LE:U64@
   repeat drop ;

\ Every address of the host, of either family, for a stream socket.
: RESOLVE ( ^conn -- lookup ) {: c :}
   c HINTS!
   c NODE c SERVICE c HINTS c CONN-FOUND SPAN:$ drop LOOKUP-CALL {: rc:n :}
   rc 0<> if c construct fault resolve rc FAIL construct lookup failed exit then
   c CONN-FOUND SPAN:$ drop LE:U64@ {: first:n :}
   first c CONN-LIST !  first c CONN-ADDR !
   first COUNT-ADDRS c CONN-ADDRS-LEFT !
   construct lookup found ;

: UNLIST ( ^conn -- ) {: c :}
   c CONN-LIST @ 0<> if c CONN-LIST @ FREE-CALL then
   0 c CONN-LIST !  0 c CONN-ADDR ! ;

\ The deadline of this address's attempt: its share of the time left, within
\ a single wait's.
: ATTEMPT-END ( ^conn -- n ) {: c :}
   c WAIT-END {: wait:n :}
   c CONN-ENDS @ NO-DEADLINE = if wait exit then
   c CONN-ENDS @ mono-ns - c CONN-ADDRS-LEFT @ / mono-ns + wait min ;

: SOCKET-ERROR ( ^conn -- n ) {: c :}
   4 c CONN-OPT-LEN SPAN:$ drop LE:U32!
   c CONN-FD @ SOL-SOCKET SO-ERROR
   c CONN-OPT SPAN:$ drop c CONN-OPT-LEN SPAN:$ drop OPTION-CALL
   0 < if FFI:ERRNO exit then
   c CONN-OPT SPAN:$ drop LE:U32@ ;

\ Connects the record's socket to the address by the deadline; false with
\ the fault kept.
: CONNECTED? ( ^conn ^u8 n -- bool ) {: c ai:^ deadline:n :}
   c CONN-FD @ ai AI-ADDR + LE:U64@ ai AI-ADDRLEN + LE:U32@ CONNECT-CALL
   0= if true exit then
   FFI:ERRNO {: e:n :}
   e EINPROGRESS <> e EINTR <> and if
      c construct fault connect e FAIL false exit
   then
   c POLLOUT deadline construct fault connect AWAIT-BY 0= if false exit then
   c SOCKET-ERROR {: err:n :}
   err 0= if true exit then
   c construct fault connect err FAIL false ;

\ One non-blocking, close-on-exec socket for the address, connected; false
\ with the fault kept and nothing held.
: ATTEMPT? ( ^conn ^u8 n -- bool ) {: c ai:^ deadline:n :}
   ai AI-FAMILY + LE:U32@ SOCKET-FLAGS ai AI-PROTOCOL + LE:U32@ SOCKET-CALL
   {: fd:n :}
   fd 0 < if c construct fault connect FFI:ERRNO FAIL false exit then
   fd c CONN-FD !
   c ai deadline CONNECTED? if true exit then
   fd CLOSE-CALL drop  -1 c CONN-FD !  false ;

\ The next address in its share of the time left; the list is freed once an
\ address connects or none is left.
: DIAL ( ^conn -- dial ) {: c :}
   c CONN-ADDR @ N>BYTES {: ai:^ :}
   c ATTEMPT-END {: deadline:n :}
   ai AI-NEXT + LE:U64@ c CONN-ADDR !
   -1 c CONN-ADDRS-LEFT +!
   c ai deadline ATTEMPT? if c UNLIST construct dial up exit then
   c CONN-ADDR @ 0<> if construct dial next exit then
   c UNLIST construct dial failed ;

\ After a handshake step's want: `waited` once the socket is ready, else
\ `failed` with the fault kept.
: HANDSHAKE-WAIT ( ^conn n -- shake )
   construct fault handshake AWAIT
   if construct shake waited else construct shake failed then ;

: TRUST-FAILED ( ^conn n -- shake ) {: c e:n :}
   c construct fault trust e FAIL  construct shake failed ;

: HANDSHAKE-FAILED ( ^conn n -- shake ) {: c e:n :}
   c construct fault handshake e FAIL  construct shake failed ;

\ A TLS step's answer: `distrusted` is fault trust and `failed` fault
\ handshake, with mbedTLS's flags or code as CAUSE.
: SHAKEN ( ^conn TLS:tls-step -- shake )
   {: c s:TLS:tls-step :}
   s MATCH TLS:tls-step
      done OF construct shake done ENDOF
      want-read OF c POLLIN HANDSHAKE-WAIT ENDOF
      want-write OF c POLLOUT HANDSHAKE-WAIT ENDOF
      distrusted OF c swap TRUST-FAILED ENDOF
      failed OF c swap HANDSHAKE-FAILED ENDOF
   ;MATCH ;

\ STARTs the conn's session on its socket for the host and the trust file.
: TLS-START ( ^conn -- shake ) {: c :}
   c c CONN-TLS c CONN-FD @ >FD c NODE c CONN-HOST-LEN @
   c CONN-TRUST @ c CONN-TRUST-LEN @ TLS:START SHAKEN ;

\ Steps the handshake to its end, waiting as each step asks.
: SHAKE ( ^conn -- greet ) {: c :}
   begin
      c c CONN-TLS TLS:HANDSHAKE SHAKEN MATCH shake
         done OF construct greet up exit ENDOF
         waited OF ENDOF
         failed OF construct greet failed exit ENDOF
      ;MATCH
   again ;

\ A plain origin has no handshake; an https one STARTs its session, then
\ shakes hands within the deadlines.
: GREET ( ^conn -- greet ) {: c :}
   c CONN-SECURE @ 0= if construct greet up exit then
   c TLS-START MATCH shake
      done OF c SHAKE ENDOF
      waited OF c SHAKE ENDOF
      failed OF construct greet failed ENDOF
   ;MATCH ;

\ The head and the body, each written once.
: DELIVER ( ^conn -- delivery ) {: c :}
   c c ROOM c CONN-USED @ WRITE-ALL
   if c c CONN-BODY @ c CONN-BODY-LEN @ WRITE-ALL else false then
   if construct delivery sent else construct delivery failed then ;


\ ---- The answer head

\ Whether the room holds the blank line after the head, searched from FROM;
\ the head's length is kept when it does.
: HEAD-END? ( ^conn n -- bool ) {: c from:n :}
   c ROOM c CONN-FILLED @ S\" \r\n\r\n" from FIND-SUB-FROM MATCH option
      none OF false ENDOF
      some OF IDX>N 4 + c CONN-HEAD-LEN ! true ENDOF
   ;MATCH ;

\ More of the head into the room; `oversize` once the room is full.
: MORE-HEAD? ( ^conn -- bool ) {: c :}
   c CONN-FILLED @ c CONN-ROOM-LEN @ >= if c OVERSIZED false exit then
   c c ROOM c CONN-FILLED @ + c CONN-ROOM-LEN @ c CONN-FILLED @ - RECEIVE
   MATCH take
      got OF c CONN-FILLED +! true ENDOF
      eof OF c CUT false ENDOF
      broke OF false ENDOF
   ;MATCH ;

: FILL-HEAD ( ^conn -- bool ) {: c :}
   0 begin c over HEAD-END? 0= while
      drop c CONN-FILLED @ 3 - 0 max
      c MORE-HEAD? 0= if drop false exit then
   repeat drop true ;

\ Where the first CRLF held at or after FROM begins; -1 while none is.
: LINE-AT ( ^conn n -- n ) {: c from:n :}
   c ROOM c CONN-FILLED @ S\" \r\n" from FIND-SUB-FROM MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Whether byte b may stand at index i of a status line's fixed part:
\ `HTTP/1.`, a digit, a space, three digits, then a space or CR.
: LEAD-BYTE? ( n n -- bool ) {: b:n i:n :}
   i 7 < if s" HTTP/1." drop i + c@ b = exit then
   i 7 = if b STR-DIGIT? exit then
   i 8 = if b 32 = exit then
   i 12 < if b STR-DIGIT? exit then
   b 32 = b 13 = or ;

\ Whether the bytes held can begin a status line.
: LEAD? ( ^conn -- bool ) {: c :}
   c ROOM {: r:^ :}
   true c CONN-FILLED @ 13 min 0 ?do r i + c@ i LEAD-BYTE? and loop ;

\ The first line into the room, judged as each receive adds to it: bytes
\ held that cannot begin a status line fail `malformed` before any
\ further read.
: FILL-LINE ( ^conn -- bool ) {: c :}
   0 begin
      c LEAD? 0= if drop c MALFORMED false exit then
      c over LINE-AT 0 <
   while
      drop c CONN-FILLED @ 1- 0 max
      c MORE-HEAD? 0= if drop false exit then
   repeat drop true ;

\ The first line, held whole with LEAD? true; keeps the status and x. A
\ line with no CRLF held, or a status outside 100 to 599, is refused.
: STATUS? ( ^conn -- bool ) {: c :}
   c ROOM {: r:^ :}
   c 0 LINE-AT 12 < if false exit then
   r 7 + c@ STR-DIGIT-VALUE c CONN-MINOR !
   r 9 + 3 STR-PARSE-POS MATCH option
      none OF false ENDOF
      some OF {: s:n :}
         s 100 >= s 599 <= and dup if s c CONN-STATUS ! then ENDOF
   ;MATCH ;

: INTERIM? ( ^conn -- bool ) CONN-STATUS @ dup 100 >= swap 200 < and ;

\ Drops an interim head, moving the bytes read past it to the room's start;
\ `oversize` once the interim heads of one answer together outgrow the room.
: DISCARD? ( ^conn -- bool ) {: c :}
   c CONN-HEAD-LEN @ {: h:n :}
   c CONN-INTERIMS @ h + {: shed:n :}
   shed c CONN-ROOM-LEN @ > if c OVERSIZED false exit then
   shed c CONN-INTERIMS !
   c ROOM h + c ROOM c CONN-FILLED @ h - BYTE-COPY
   h negate c CONN-FILLED +!
   0 c CONN-HEAD-LEN !
   true ;

: CHUNKED? ( str -- bool ) {: a:^ u:n :}
   u 7 < if false exit then
   a u + 7 - 7 s" chunked" STR=CI ;

\ Whether `close` is one of a Connection field's comma-separated options.
: CLOSE-TOKEN? ( str -- bool ) {: a:^ u:n :}
   0 begin
      {: start:n :}
      a u [char] , start SPLIT-NEXT
   while
      {: t:^ tu:n next:n :}
      t tu TRIM s" close" STR=CI if true exit then
      next
   repeat 2drop drop false ;

\ A Content-Length that is not a number, or two that differ, is malformed.
: LENGTH? ( ^conn str -- bool ) {: c a:^ u:n :}
   a u STR-PARSE-POS MATCH option
      none OF false ENDOF
      some OF {: v:n :}
         c CONN-LENGTH @ MATCH option
            none OF true ENDOF
            some OF v = ENDOF
         ;MATCH
         v OPTION:SOME c CONN-LENGTH !
      ENDOF
   ;MATCH ;

: FIELD? ( ^conn str str -- bool ) {: c k:^ ku:n v:^ vu:n :}
   k ku s" Content-Length" STR=CI if c v vu LENGTH? exit then
   k ku s" Transfer-Encoding" STR=CI if
      v vu CHUNKED? c CONN-CHUNKED ! true exit
   then
   k ku s" Connection" STR=CI if
      v vu CLOSE-TOKEN? if true c CONN-CLOSE-ASKED ! then
   then
   true ;

\ A line without a colon names no field this reads.
: FIELD-LINE? ( ^conn str -- bool ) {: c a:^ u:n :}
   a u [char] : INDEX-OF MATCH option
      none OF true ENDOF
      some OF IDX>N {: k:n :} c a k a k + 1+ u k - 1- TRIM FIELD? ENDOF
   ;MATCH ;

: FIELDS-RESET ( ^conn -- ) {: c :}
   false c CONN-CHUNKED !  OPTION:NONE c CONN-LENGTH !
   false c CONN-CLOSE-ASKED ! ;

\ The line ending at or after FROM in the head, as its end; the head always
\ ends in one.
: LINE-END ( ^conn n -- n ) {: c from:n :}
   c ROOM c CONN-HEAD-LEN @ S\" \r\n" from FIND-SUB-FROM MATCH option
      none OF c CONN-HEAD-LEN @ ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Reads the field lines after the status line up to the blank line.
: FIELDS? ( ^conn -- bool ) {: c :}
   c FIELDS-RESET
   c 0 LINE-END 2 +
   begin
      {: from:n :}
      c from LINE-END {: to:n :}
      to from = if true exit then
      c c ROOM from + to from - FIELD-LINE? 0= if false exit then
      to 2 +
   again ;

\ Reads a head, its first line judged before the rest is read; an interim
\ one is discarded.
: READ-HEAD ( ^conn -- head-read ) {: c :}
   c FILL-LINE 0= if construct head-read failed exit then
   c STATUS? 0= if c MALFORMED construct head-read failed exit then
   c FILL-HEAD 0= if construct head-read failed exit then
   c INTERIM? if
      c DISCARD? 0= if construct head-read failed exit then
      construct head-read interim exit
   then
   c FIELDS? 0= if c MALFORMED construct head-read failed exit then
   construct head-read head ;

: BODILESS? ( ^conn -- bool ) CONN-STATUS @ dup 204 = swap 304 = or ;

\ How the body ends: no body for 204 and 304, else chunks, else a length,
\ else the close.
: FRAME ( ^conn -- ) {: c :}
   0 c CONN-LEFT !
   c BODILESS? if construct framing empty c CONN-FRAMING ! exit then
   c CONN-CHUNKED @ if construct framing chunked c CONN-FRAMING ! exit then
   c CONN-LENGTH @ MATCH option
      some OF c CONN-LEFT ! construct framing sized ENDOF
      none OF construct framing closing ENDOF
   ;MATCH c CONN-FRAMING ! ;


\ ---- The body

\ At most MOST answer bytes into NEXT's span.
: INTO-SPAN ( ^conn n -- take ) {: c most:n :}
   c c CONN-SPAN @ c CONN-SPAN-LEN @ most min PULL ;

: SIZED-PIECE ( ^conn -- piece ) {: c :}
   c CONN-LEFT @ 0= if construct piece end exit then
   c c CONN-LEFT @ INTO-SPAN MATCH take
      got OF dup c CONN-GOT !  negate c CONN-LEFT +!  construct piece some ENDOF
      eof OF c CUT construct piece failed ENDOF
      broke OF construct piece failed ENDOF
   ;MATCH ;

: CLOSING-PIECE ( ^conn -- piece ) {: c :}
   c c CONN-SPAN-LEN @ INTO-SPAN MATCH take
      got OF c CONN-GOT !  construct piece some ENDOF
      eof OF construct piece end ENDOF
      broke OF construct piece failed ENDOF
   ;MATCH ;

\ One more answer byte of a line or section of N bytes so far, which may
\ be as long as the room; false with the fault kept, `oversize` past it.
: BYTE-IN? ( ^conn n -- bool ) {: c n:n :}
   n c CONN-ROOM-LEN @ >= if c OVERSIZED false exit then
   c BYTE? ;

\ Reads through the line's LF, N bytes of the line or its section read so
\ far; answers the count through the LF.
: TO-LF ( ^conn n -- n bool ) {: c n:n :}
   n begin c LAST-BYTE 10 <> while
      c over BYTE-IN? 0= if false exit then
      1+
   repeat true ;

\ The byte after a chunk size: the line's CR, or an extension's start.
: SIZE-END? ( n -- bool ) {: b:n :}
   b 13 = b 59 = or b 32 = or b 9 = or ;

\ A chunk's size line, its size kept as the bytes left; an empty or too long
\ size, or one followed by anything but an extension or the line's end, is
\ malformed; a line longer than the room, its extension included, is
\ oversize.
: SIZE-LINE? ( ^conn -- bool ) {: c :}
   0 0 begin
      c BYTE? 0= if 2drop false exit then
      c LAST-BYTE STR-HEX-DIGIT?
   while
      1+ dup SIZE-DIGITS > if 2drop c MALFORMED false exit then
      swap 4 lshift c LAST-BYTE STR-HEX-DIGIT-VALUE or swap
   repeat
   {: size:n k:n :}
   k 0= c LAST-BYTE SIZE-END? 0= or if c MALFORMED false exit then
   size c CONN-LEFT !
   c k 1+ TO-LF nip ;

\ The LF after a CR just read.
: CRLF-TAIL? ( ^conn -- bool ) {: c :}
   c BYTE? 0= if false exit then
   c LAST-BYTE 10 <> if c MALFORMED false exit then
   true ;

\ The CRLF that ends a chunk's data.
: CHUNK-END? ( ^conn -- bool ) {: c :}
   c BYTE? 0= if false exit then
   c LAST-BYTE 13 <> if c MALFORMED false exit then
   c CRLF-TAIL? ;

\ The trailer lines after the last chunk, through the blank line; a
\ section longer than the room is oversize.
: TRAILERS? ( ^conn -- bool ) {: c :}
   0 begin
      c over BYTE-IN? 0= if drop false exit then
      1+
      c LAST-BYTE 13 = if
         c CONN-ROOM-LEN @ < if c CRLF-TAIL? exit then
         c OVERSIZED false exit
      then
      c swap TO-LF 0= if drop false exit then
   again ;

: CHUNK-DATA ( ^conn -- piece ) {: c :}
   c c CONN-LEFT @ INTO-SPAN MATCH take
      got OF dup c CONN-GOT !  negate c CONN-LEFT +! ENDOF
      eof OF c CUT construct piece failed exit ENDOF
      broke OF construct piece failed exit ENDOF
   ;MATCH
   c CONN-LEFT @ 0= if c CHUNK-END? 0= if construct piece failed exit then then
   construct piece some ;

: CHUNK-PIECE ( ^conn -- piece ) {: c :}
   c CONN-LEFT @ 0= if
      c SIZE-LINE? 0= if construct piece failed exit then
      c CONN-LEFT @ 0= if
         c TRAILERS? if construct piece end exit then
         construct piece failed exit
      then
   then
   c CHUNK-DATA ;

\ One piece of the body by its framing.
: READ-PIECE ( ^conn -- piece ) {: c :}
   c CONN-FRAMING @ MATCH framing
      empty OF construct piece end ENDOF
      sized OF c SIZED-PIECE ENDOF
      chunked OF c CHUNK-PIECE ENDOF
      closing OF c CLOSING-PIECE ENDOF
   ;MATCH ;

: FRAMED? ( ^conn -- bool )
   CONN-FRAMING @ MATCH framing
      empty OF true ENDOF
      sized OF true ENDOF
      chunked OF true ENDOF
      closing OF false ENDOF
   ;MATCH ;

\ Whether an https conn's session holds bytes past what was read from it.
: TLS-HELD? ( ^conn -- bool ) {: c :}
   c CONN-SECURE @ if c CONN-TLS TLS:HELD? else false then ;

\ Kept after a framed answer read whole, on HTTP/1.1, with no `Connection:
\ close` and no byte read past it, in the room or held in the TLS session.
: REUSE ( ^conn -- reuse ) {: c :}
   c FRAMED? c CONN-MINOR @ 1 = and c CONN-CLOSE-ASKED @ 0= and c EXTRA 0= and
   c TLS-HELD? 0= and
   if construct reuse keep else construct reuse shut then ;


\ ---- Resting states

: REST ( ^conn held phase -- ) {: c h:held p:phase :}
   h c CONN-HELD !  p c CONN-RESUME ! ;

: SENT-REST ( ^conn -- ) {: c :}
   c construct held busy construct phase heading REST
   0 c CONN-FILLED !  0 c CONN-HEAD-LEN !  0 c CONN-TAKEN !
   0 c CONN-INTERIMS ! ;

: PARTWAY-REST ( ^conn -- ) {: c :}
   c construct held busy construct phase reading REST
   c CONN-STATUS @ c CONN-GOT @ >LEN construct chunk-result chunk
   c CONN-ANSWER ! ;

: ENDED ( ^conn -- ) {: c :}
   c CONN-STATUS @ construct chunk-result response c CONN-ANSWER ! ;

: IDLE-REST ( ^conn -- ) {: c :}
   c construct held idle construct phase idle REST  c ENDED ;

: CLOSED-REST ( ^conn -- ) {: c :}
   c construct held none construct phase closed REST  c ENDED ;

: DOWN-REST ( ^conn -- ) {: c :}
   c construct held none construct phase down REST
   c CONN-FAULT @ construct chunk-result failed c CONN-ANSWER ! ;


FSM:DEFINE LIFECYCLE ( ^conn ) phase intake
   IN starting  INTAKE ( intake )
      marked   REFUSE   >down
      none     -        >resolving
      idle     -        >probing
      busy     HANG-UP  >resolving
   IN probing  PROBE ( probe )
      quiet    -        >sending
      stale    HANG-UP  >resolving
   IN resolving  RESOLVE ( lookup )
      found    -        >connecting
      failed   -        >down
   IN connecting  DIAL ( dial )
      up       -        >greeting
      next     -        >connecting
      failed   -        >down
   IN greeting  GREET ( greet )
      up       -        >sending
      failed   HANG-UP  >down
   IN sending  DELIVER ( delivery )
      sent     -        >sent
      failed   HANG-UP  >down
   IN heading  READ-HEAD ( head-read )
      interim  -        >heading
      head     FRAME    >reading
      failed   HANG-UP  >down
   IN reading  READ-PIECE ( piece )
      some     -        >partway
      end      -        >settling
      failed   HANG-UP  >down
   IN settling  REUSE ( reuse )
      keep     -        >idle
      shut     HANG-UP  >closed
   FINAL sent     SENT-REST
   FINAL partway  PARTWAY-REST
   FINAL idle     IDLE-REST
   FINAL closed   CLOSED-REST
   FINAL down     DOWN-REST
;DEFINE


: ENDS! ( ^conn -- ) {: c :}
   c CONN-TOTAL @ 0= if NO-DEADLINE c CONN-ENDS ! exit then
   c CONN-TOTAL @ NS-PER-MS * mono-ns + c CONN-ENDS ! ;

\ Hangs up, frees the address list and rests `down`, holding nothing, with
\ fault `send` and no cause: a NEXT before the next SEND fails `send`.
: UNSENT ( ^conn -- ) {: c :}
   c HANG-UP  c UNLIST
   c construct held none construct phase down REST
   c construct fault send 0 FAIL ;

\ Runs LIFECYCLE from the phase. A throw out of a sense runs UNSENT before it
\ goes on, so no socket or address list outlives it.
: DRIVE ( ^conn phase -- ) {: c p:phase :}
   [: c p LIFECYCLE ;] catch {: rc:n :}
   rc 0<> if c UNSENT rc throw then ;

public

\ Origin `http://host[:port]` (port 80 by default) or `https://host[:port]`
\ (port 443), trust file and room. Parses the origin and does no I/O; false
\ for an origin it cannot read. An https connection trusts the certificates
\ of the trust file, which a plain one never reads. The record holds no
\ connection after it.
: OPEN ( ^conn str str ^u8 n -- bool )
   {: c o:^ ou:n t:^ tu:n r:^ ru:n :}
   c CLEAR-TLS
   -1 c CONN-FD !  0 c CONN-LIST !  c UNSENT
   t c CONN-TRUST !  tu c CONN-TRUST-LEN !
   r c CONN-ROOM !  ru c CONN-ROOM-LEN !
   0 c CONN-USED !  0 c CONN-HEAD-LEN !  OPTION:NONE c CONN-MARK !
   c o ou ORIGIN? ;

\ Method and target. Starts a request head in the room, the request line and
\ `Host`, and clears the deadlines. A CR, LF or SP in either marks the request
\ malformed.
: REQUEST ( ^conn str str -- ) {: c m:^ mu:n t:^ tu:n :}
   0 c CONN-USED !  0 c CONN-HEAD-LEN !  OPTION:NONE c CONN-MARK !
   0 c CONN-TOTAL !  0 c CONN-STALL !
   m mu s" GET" STR= c CONN-GET-METHOD !
   m mu SPLITS? t tu SPLITS? or if c construct fault malformed MARK then
   c m mu APPEND  c 32 CHAR+  c t tu APPEND  c s"  HTTP/1.1" APPEND  c CRLF+
   c s" Host: " APPEND  c c CONN-AUTHORITY @ c CONN-AUTHORITY-LEN @ APPEND
   c CRLF+ ;

\ One header line, without its CRLF. A line break in it, or a head that
\ outgrows the room, marks the request so SEND fails it without touching the
\ wire.
: HEADER+ ( ^conn str -- ) {: c a:^ u:n :}
   a u BREAK? if c construct fault malformed MARK exit then
   c a u APPEND  c CRLF+ ;

\ Total milliseconds for the whole exchange, from SEND to the last NEXT, and
\ stall milliseconds for any single wait; 0 means none.
: DEADLINES! ( ^conn n n -- ) {: c total:n stall:n :}
   total c CONN-TOTAL !  stall c CONN-STALL ! ;

\ The body span is borrowed until SEND returns. A `Content-Length` frames it
\ in every request but a GET with no body, which has none. Probes or dials,
\ then writes head and body once. A fault is kept for the first NEXT.
: SEND ( ^conn str -- ) {: c a:^ u:n :}
   a c CONN-BODY !  u c CONN-BODY-LEN !
   c CONN-GET-METHOD @ u 0= and 0= if
      c s" Content-Length: " APPEND  c u DIGITS+  c CRLF+
   then
   c CRLF+
   c ENDS!
   c construct phase starting DRIVE ;

\ The span holds at least one byte. The first call reads the head. Each call
\ answers one piece of the body in the span, or the end, or the fault.
: NEXT ( ^conn ^u8 len -- chunk-result ) {: c a:^ u:len :}
   a c CONN-SPAN !  u LEN>N c CONN-SPAN-LEN !
   c c CONN-RESUME @ DRIVE
   c CONN-ANSWER @ ;

\ The answer's head, status line through the blank line, in the room; valid
\ until the next REQUEST.
: HEAD$ ( ^conn -- str ) dup ROOM swap CONN-HEAD-LEN @ ;

\ Drops the connection in any state; the record keeps its origin, trust and
\ room, and its next SEND dials again.
: CLOSE ( ^conn -- ) UNSENT ;

\ Whether no whole answer arrived from a peer that may answer later.
: LOST? ( fault -- bool )
   MATCH fault
      resolve OF true ENDOF
      connect OF true ENDOF
      handshake OF true ENDOF
      trust OF false ENDOF
      send OF true ENDOF
      receive OF true ENDOF
      cut OF true ENDOF
      timeout OF true ENDOF
      malformed OF false ENDOF
      oversize OF false ENDOF
   ;MATCH ;

: FAULT$ ( fault -- str )
   MATCH fault
      resolve OF s" the host name did not resolve" ENDOF
      connect OF s" no address of the host took the connection" ENDOF
      handshake OF s" the TLS handshake failed" ENDOF
      trust OF
         s" the server's certificate did not verify, or the trust file could not be read"
      ENDOF
      send OF s" the request was not written" ENDOF
      receive OF s" reading the answer failed" ENDOF
      cut OF s" the server closed the connection before the answer ended" ENDOF
      timeout OF s" a deadline passed" ENDOF
      malformed OF s" the request or the answer is not well-formed HTTP" ENDOF
      oversize OF
         s" a head, interim heads, a chunk-size line or trailers exceed the room"
      ENDOF
   ;MATCH ;

\ The errno, getaddrinfo code, mbedTLS code or verify flags behind the last
\ fault; 0 for none.
: CAUSE ( ^conn -- n ) CONN-CAUSE @ ;

;package
