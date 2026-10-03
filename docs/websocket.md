# WebSocket

Habu's RFC 6455 support is a server: a route handler of the
[HTTP/1.1 server](http.md) accepts the upgrade and then owns the connection as
a socket. It is built from the bytes up, and everything under the connection
words is pure byte work that needs no socket:

| file | package | what it does |
| --- | --- | --- |
| `lib/net/ws.f` | `WS` | accepts the handshake inside an HTTP handler, receives whole messages, sends and closes |
| `lib/net/ws-frame.f` | `WS` | encodes and decodes frame headers, masks payloads, writes close codes |
| `lib/crypto/sha1.f` | `SHA1` | the digest behind `Sec-WebSocket-Accept` ([crypto](crypto.md#sha-1-for-the-websocket-handshake)) |
| `lib/base64.f` | `BASE64` | the encoding of the key and of the accept value ([stdlib](stdlib.md#base64)) |

Requiring `lib/net/ws.f` loads all of them and the HTTP server. No libcrypto is
involved.

## Connections

```forth
WS:ACCEPT ( HTTP:request HTTP:response -- WS:accepted )
WS:RECEIVE ( WS:socket ms -- WS:message )
WS:SEND-TEXT ( WS:socket ptr u8 n -- )
WS:SEND-BINARY ( WS:socket ptr u8 n -- )
WS:CLOSE ( WS:socket n -- )
WS:MESSAGE-MAX ( -- n )
WS:STALL-MS ( -- n )
```

```forth
: BACK ( ptr u8 n WS:socket -- ) {: a u:n sock:WS:socket :}
   sock a u WS:SEND-TEXT ;

: TALK ( WS:socket -- ) {: sock:WS:socket :}
   begin
      sock 1000 >MS WS:RECEIVE
      MATCH WS:message
         text OF sock BACK ENDOF
         binary OF 2drop ENDOF
         closed OF drop exit ENDOF
         timeout OF ENDOF
      ;MATCH
   again ;

: CHAT ( HTTP:request HTTP:response -- )
   WS:ACCEPT
   MATCH WS:accepted
      open OF TALK ENDOF
      refused OF drop ENDOF
   ;MATCH ;

: ROUTES ( -- )
   s" GET" s" /chat" [: CHAT ;] HTTP:ROUTE ;
```

A socket lives in the HTTP worker whose handler accepted it, for as long as
that handler runs: the worker serves nothing else meanwhile, so a server holds
at most `HTTP:MAX-WORKERS` sockets, and the handler returns when the socket
closes or its application stops serving. Once `HTTP:STOP` begins, `RECEIVE`
closes the socket under status 1001, or 1006 when that close frame cannot be
written, and answers `closed` (below), so a handler that returns on `closed`
ends within the stop's bound. A worker still inside a handler past that bound
is killed ([http.md](http.md#lifecycle)), and the killed worker's exit closes
its connection.

### The handshake

`ACCEPT` checks the request as an opening handshake (section 4.2.1): an
`HTTP/1.1` GET whose `Upgrade` lists `websocket` and whose `Connection` lists
`Upgrade`, in any case, whose `Sec-WebSocket-Key` is the base64 of sixteen
bytes and whose `Sec-WebSocket-Version` is `13`, each of the four arriving
exactly once (the server has already answered 400 to an HTTP/1.1 request
without exactly one valid `Host`, before any handler runs;
[http.md](http.md#refusals-and-faults)).

- A handshake that passes is answered at once with `101 Switching Protocols`,
  `Upgrade: websocket`, `Connection: Upgrade` and `Sec-WebSocket-Accept`, and
  the connection is taken from the worker with
  [`HTTP:TAKE-OVER`](http.md#taking-the-connection-over). `TCP_NODELAY` is set
  on the connection before the 101 goes out, so a small frame is sent without
  waiting for the peer to acknowledge the one before it. The 101 carries
  nothing the handler added to the response. `ACCEPT` answers `open` with the
  socket. A connection that refuses `TCP_NODELAY`, or fails under the 101,
  gives a socket closed from birth under status 1006, and the handler's first
  `RECEIVE` answers `closed`.
- One that does not is answered through the response once the handler returns:
  `400 bad_handshake`, or `426 upgrade_required` with `Upgrade: websocket` and
  `Sec-WebSocket-Version: 13` when only the version is wrong (section 4.4),
  rendered by the server's error hook like any other error answer. `ACCEPT`
  answers `refused` with that status, 400 or 426, and makes no socket, so
  nothing can be sent for a refused handshake.

`Origin` is the handler's to check before it accepts. No subprotocol and no
extension is negotiated.

### Receiving

`RECEIVE` waits up to the milliseconds given for the next whole message and
answers a `WS:message`. The wait is absolute: a peer that keeps frames queued,
or keeps a long frame's bytes coming, cannot hold the call past it; a frame
still arriving when the wait ends is taken up by the next call. A wait of zero
asks once without waiting; one below zero, or past the 2^31 - 1 ms a readiness
poll takes, is `E-WS-WAIT`. Answering a control frame can hold the call past
its wait by up to `WS:STALL-MS` and one `TCP4:SEND-SLICE-MS`: the answer is due
`WS:STALL-MS` after the worker comes for the send lock, and another task's
push ahead of it, stalled or still moving, gives up at that deadline, losing
the socket under 1006 with the answer unwritten.

`RECEIVE` reads `HTTP:STOPPING?` at least every `HTTP:STOP-WAIT-MS`. Once the
server stops, it sends an open socket a close frame with status 1001 (going
away, section 7.4.1), written under `HTTP:RELEASE-MS`, and answers `closed`
with 1001, or with 1006 when that frame cannot be written; a socket already
closing is sent nothing more.

| arm | carries | when |
| --- | --- | --- |
| `text` | `ptr u8 n` | a text message, checked as UTF-8 once it is whole |
| `binary` | `ptr u8 n` | a binary message |
| `closed` | the status `n` | the socket has closed; every later call answers the same |
| `timeout` | nothing | the wait ran out; a frame cut short is taken up by the next call |

A message's bytes are borrowed from the socket until its next `RECEIVE`. Only
the worker that accepted a socket receives on it: from any other task, and
through a handle kept past its handler, `RECEIVE` is `E-WS-SOCKET`.

Inside it, `RECEIVE` reassembles a fragmented message, answers a ping with a
pong carrying the same payload, also while the closing handshake runs
(section 5.5.2), lets a pong pass, and answers the peer's close
frame with a close frame carrying the same status. A peer that breaks the
protocol is sent a close frame and the socket closes under its status:

| status | the peer sent |
| --- | --- |
| 1002 | a frame the codec refuses (below): unmasked, a control frame that is fragmented or past 125 bytes, a reserved bit or opcode, a length in a longer form than it needs |
| 1002 | a continuation of no message, or a new text or binary frame inside a fragmented one |
| 1002 | a close frame with half a status, or with one no frame may carry |
| 1007 | text, or a close reason, that is not UTF-8 |
| 1009 | a frame, or fragments together, past `MESSAGE-MAX` (1 MiB) |

The `closed` arm's status is the one the socket closed under: what the peer's
close frame carried, 1005 when it carried none, 1006 when the stream ended with
no close frame (section 7.1.5) or a close frame this end owed could not be
written, 1001 when the server stopped, and the status above when this end
failed the connection. A close frame may carry 1000 to 1014 except 1004, 1005
and 1006, and 3000 to 4999 (section 7.4).

### Sending and closing

`SEND-TEXT` and `SEND-BINARY` write one unfragmented frame. Text that is not
UTF-8 is `E-WS-TEXT` before anything is written (section 5.6). No word sends a
ping or a fragmented message: no caller needs one, and the RFC requires
neither. `CLOSE` starts the closing handshake
under the status given: the handler goes on receiving until `closed` answers
with the peer's status. A status no frame may carry is `E-WS-CODE`, and closing
a socket that is already closing, closed or past its handler changes nothing.

The three may be called from any task. Every frame is written whole under a
`TASK:FACILITY` of its worker slot's own, so a push from another task lands
between the handler's frames and never inside one. A send on a socket that is closing,
closed or past its handler, and one the connection fails under, is
`E-WS-CLOSED`: a pushing task learns that way that its socket is gone. A write
waits while the peer does not read, and the socket's other senders wait behind
it. A peer that accepts no byte for
`WS:STALL-MS` (5000 ms) fails the socket: it closes under status 1006, and the
stalled send and every send waiting behind it are `E-WS-CLOSED`. The write
notices when the send slice in hand ends, within `TCP4:SEND-SLICE-MS` and two
sends that never wait after the bound passes.

When the handler ends, the worker lets the socket go before it closes the
connection: a socket still open is sent a close frame with status 1000, or 1011
when the handler threw, and that frame is due `HTTP:RELEASE-MS` after the
worker comes for the send lock, rather than `WS:STALL-MS`. The worker does not
wait out another task's push to say goodbye or to answer its peer. While the
worker claims the send lock, every write on the slot gives up at the claim's
deadline: `WS:STALL-MS` after the claim for a pong, a close answering the
peer's or one failing the connection, and `HTTP:RELEASE-MS` for the goodbye,
the stop's 1001 close and the retirement. Once the server stops, every write on
the slot gives up `HTTP:RELEASE-MS` after the first look that finds it
stopping. Either way it gives up however many bytes the peer takes
(`lib/net/ws.f` `GIVE-UP?`); a write looks before each send slice. A write that
gives up loses the socket under 1006, whether it is another task's push, the
worker's own answer to a control frame or its goodbye. Only a send reports that
as `E-WS-CLOSED`: the worker's own writes throw nothing, and a `RECEIVE` whose
write gave up answers `closed` with 1006.
A worker that a stop kills inside its handler never lets its socket go: its
exit retires the socket, sending nothing, and the HTTP worker's exit then
closes the connection. Killed inside a write, it gives its slot's lock back as
it ends, so a send waiting behind that write, or made later through its
handle, is `E-WS-CLOSED`. The slot's count of retirements moves before its next
socket opens, so a handle kept from an earlier socket never names a later one:
a send through it is `E-WS-CLOSED`.

An application's own task inside a send is joined, never killed, by the rule
[threads](threads.md#semaphores) gives a task blocked in `TASK:WAIT`: killed
at the write's pause, it would leave the slot's lock held, so the worker's next
take of it would block and the stop would never return.

### Storage

The tables are process-wide and indexed by the HTTP worker's slot. A socket's
message buffer and scratch are one mapping, made by `ACCEPT` and given back
when the worker lets the socket go or its exit retires the socket. A slot's
send lock is a `TASK:FACILITY` made ready while `lib/net/ws.f` loads, before
any task is live, and never destroyed. A slot's due time is set while its
worker claims that lock, to answer its peer, close, let the socket go or retire
it, and cleared before the worker gives the lock back; every write on the slot
gives up once it passes. Its stop time, taken by the first look that finds the
server stopping and cleared when the slot's next socket opens, starts the one
`HTTP:RELEASE-MS` every write on the slot shares once the server stops.

## The frame codec

```forth
WS:ENCODE-HEADER ( WS:header WS:sender SPAN:span<u8> -- n )
WS:DECODE-HEADER ( ptr u8 n WS:sender n -- WS:decoded )
WS:MASK ( n SPAN:span<u8> -- )
WS:CLOSE-CODE! ( WS:close-code SPAN:span<u8> -- )
WS:HEADER-MAX ( -- n )
WS:CLOSE-NORMAL WS:CLOSE-PROTOCOL WS:CLOSE-INVALID-DATA WS:CLOSE-TOO-BIG ( -- WS:close-code )
```

- `WS:opcode` is `continuation text binary close ping pong`, the six opcodes
  RFC 6455 defines, on the wire 0, 1, 2, 8, 9 and $A. `close`, `ping` and `pong`
  are the control frames. Make one with `WS-OPCODE:text` and dispatch on one
  with `MATCH WS:opcode`.
- `WS:sender` is `client` or `server`. A client masks every frame it sends and a
  server masks none (section 5.1), so the sender decides the mask bit.
- `WS:header` holds `fin` (a bool, set on a message's last frame), `op`, `len`
  (the payload's length in bytes) and `key` (the masking key, 0 on an unmasked
  frame): `WS-HEADER:MAKE ( bool WS:opcode n n -- WS:header )`.
- `WS:decoded` is `need` with a byte count, or `frame` with a header and the
  count of bytes the header took.
- `WS:close-code` is a close status (section 7.4.1): 1000 normal, 1002 protocol
  error, 1007 invalid payload data, 1009 too big.

### Decoding

`DECODE-HEADER` takes the bytes held from the start of a frame, the sender they
came from, and the longest payload the caller accepts. While too few bytes are
held it answers `need` with the count to hold before calling again: 2 until the
second byte arrives, then the whole header's size, 2 to 14. Once the header is
held it answers `frame`: the payload starts that many bytes in and is the
header's `len` bytes long, and a client's payload is unmasked with `WS:MASK`
under the header's key.

It refuses as soon as the bytes held show a fault, without waiting for the rest
of the header. Each code names the frame's fault, and RFC 6455 says which status
closes the connection for it:

| code | the frame | close with |
| --- | --- | --- |
| `E-WS-RSV` -9330 | sets a reserved bit, and no extension is negotiated | 1002 |
| `E-WS-OPCODE` -9331 | carries a reserved opcode | 1002 |
| `E-WS-CONTROL` -9332 | is a control frame that is fragmented or longer than 125 bytes | 1002 |
| `E-WS-LENGTH` -9333 | has an extended length with its high bit set, or in a longer form than it needs (section 5.2's MUST) | 1002 |
| `E-WS-TOO-BIG` -9334 | carries a payload longer than the caller's bound | 1009 |
| `E-WS-MASK` -9335 | is a client's unmasked frame or a server's masked one | 1002 |

A negative byte count is `E-SPAN-LENGTH`.

### Encoding

`ENCODE-HEADER` writes a header as its sender sends it and answers the size it
wrote: the length in its shortest form, and the mask bit and key only for a
client. So every header it writes decodes to the header it was given, and every
header `DECODE-HEADER` accepts encodes back to the same bytes. It refuses a
fragmented control frame or one longer than 125 bytes (`E-WS-CONTROL`), a
negative length (`E-WS-LENGTH`), a nonzero key on a server's header or a
client's key outside 32 bits (`E-WS-MASK`), and a span shorter than the header
(`E-SPAN-CAPACITY`), each before it writes a byte. `WS:HEADER-MAX`, 14 bytes,
holds any header. The payload follows the header, and a client masks it with
`WS:MASK` under the key it wrote.

### Masking and close codes

`MASK` XORs each byte of the span with the key's byte at the same offset mod 4,
the key's high byte first (section 5.3), so masking twice restores the bytes.
The span's first byte is taken as the payload's first, so a payload masked in
pieces is split at multiples of four bytes. A key outside 32 bits is
`E-WS-MASK`.

`CLOSE-CODE!` writes a status as the two big-endian bytes that open a close
frame's payload. A span shorter than two bytes is `E-SPAN-CAPACITY`.

## The handshake's accept value

A server answers `Sec-WebSocket-Key` with `Sec-WebSocket-Accept`: the base64 of
the SHA-1 of the key followed by `258EAFA5-E914-47DA-95CA-C5AB0DC85B11`
(section 1.3). With a context span `CTX` of `SHA1:CTX-BYTES`, a digest span `DG`
of `SHA1:DIGEST-BYTES` and an output span `OUT`:

```forth
CTX SHA1:START
CTX key-ptr key-len SHA1:FEED
CTX s" 258EAFA5-E914-47DA-95CA-C5AB0DC85B11" SHA1:FEED
CTX SHA1:FINISH DG SHA1:DIGEST!
DG SPAN:$ OUT BASE64:ENCODE
```

`lib/base64-test.f` derives the RFC's `s3pPLMBiTxaQ9kYGzzhZRbK+xOo=` this way.

## Errors

Each file has its own block in `lib/errors.f`. The codec's is -9330..-9339
(`E-WS-FIRST`, `E-WS-LAST`): it uses -9330..-9335 (above) and keeps the rest in
reserve. The connection words' is -9410..-9419 (`E-WS-CONN-FIRST`,
`E-WS-CONN-LAST`):

| code | meaning |
| --- | --- |
| `E-WS-CLOSED` -9410 | a send on a socket that is closing, closed or past its handler, or whose connection failed, or accepted no byte for `WS:STALL-MS`, under the send, or that gave up at the worker's claim on the send lock or at the server's stop |
| `E-WS-CODE` -9411 | `CLOSE` with a status no frame may carry |
| `E-WS-SOCKET` -9412 | `RECEIVE` through a handle that is not the calling worker's own live socket |
| `E-WS-WAIT` -9413 | a `RECEIVE` wait below zero, or longer than a readiness poll takes |
| `E-WS-TEXT` -9414 | `SEND-TEXT` of bytes that are not UTF-8 |

A send of a negative length is the codec's `E-WS-LENGTH`.

## Tests

`bin/hb --load lib/net/ws-test.f`, SUITE `websocket`, runs a Habu client over
TCP4 against a server on a loopback port. The client builds and reads its
frames with the codec and sends the malformed ones as raw bytes. It shakes
hands with section 1.3's key and reads back `s3pPLMBiTxaQ9kYGzzhZRbK+xOo=`;
has every malformed handshake refused, among them an HTTP/1.0 request, the
handler seeing each refusal's status, and ones with no `Host`, two, an empty
one or one with a space, which the server refuses before the handler runs;
echoes text, and binary at each length form's edges (125, 126, 65535, 65536
and 70000 bytes); sends fragments with a ping between them and a scalar
straddling two; goes quiet across the handler's wait and cuts a frame in two
around it; puts a frame behind the handshake; provokes each failure status,
with the message bound met exactly and passed by one; closes from either end,
by the handler returning and by its throwing, and answers a ping while the
closing handshake runs; refuses a text send of bytes that are not UTF-8,
writing nothing; and reads 400 frames from a second task and 400 from the
handler sent at once, every one whole. It sends through a handle kept past its
socket and through one kept past a worker a stop killed, an application's exit
hook throwing before WS's own in that worker's exit; stops reading while its
pings are answered until a pong stalls, so the socket fails under 1006 no
sooner than `WS:STALL-MS` after the worker came for the lock to write it, and a
send waiting behind the stalled pong is `E-WS-CLOSED`. A handler returns while
another task's push to a peer that does not read is stalled: the stop that
follows kills no task and returns within http-test's ceiling, and the push is
`E-WS-CLOSED`. A ping arriving while another task's push to a client that keeps
taking bytes holds the lock loses the socket under 1006 within the wait,
`WS:STALL-MS` and one send slice; the push is `E-WS-CLOSED` and the stop that
follows kills no task. A stop cuts the handler's own push, to a client that
reads nothing and to one that keeps taking bytes past the stop's ceiling alike:
the push is `E-WS-CLOSED` within the stop's bound and no task is killed. After
the first, a send through the handle that handler kept is `E-WS-CLOSED` on the
restarted server. A burst of 4000 pings sent in one write cannot hold a 50 ms
`RECEIVE` past its wait, and a `RECEIVE` that waits not at all answers
`timeout` mid-frame though the rest of the frame waits in the socket, the next
taking the message whole. A stop closes a handler's idle `RECEIVE` under 1001,
the client reading the close frame and then the end of the stream; it ends the
worker without a kill when a ping or a close arrives behind a stalled push and
when the worker's own pong stalls; and when the stop's cut, started by the
handler's own send, has run out, the 1001 close cannot be written and the
socket closes under 1006, the client reading the text and then the end of the
stream. The frames the client sends and reads one at a time are lines of
`build/ws-transcript.txt`, the push's frames a line of counts and the burst's
none; the file is read back and compared whole.

`bin/hb --load lib/net/ws-frame-test.f`, SUITE `websocket-frame`, writes and
reads section 5.7's example frames byte for byte and unmasks its masked
"Hello". It takes each length form to its edges (125, 126, 65535, 65536 and
2^63 - 1), round-trips headers of every opcode from both senders, answers
`need` for partial headers, and makes every refusal from the fewest bytes that
show it, with the payload bound met exactly and exceeded by one in each length
form.
