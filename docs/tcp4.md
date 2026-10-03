# IPv4 TCP sockets

[`lib/net/tcp4.f`](../lib/net/tcp4.f) provides generic IPv4 stream socket I/O.
The current implementation supports Habu on Linux and macOS AArch64. It
contains no application protocol, framing, name resolution, or connection
policy. Other operating systems are rejected before opening a socket. The
readiness questions (`PENDING?`, `READABLE?` and their `-WITHIN?` forms) run
on the AIO loop ([aio.md](aio.md)), so a program that asks one starts the loop
with `AIO:START` first.

## Values and operations

`TCP4:ADDRESS` validates a numeric IPv4 address in `0..0xFFFFFFFF` and returns
the nominal `TCP4:address`; for example, `$7F000001 TCP4:ADDRESS` represents
`127.0.0.1`. Dotted-decimal and host-name parsing are not part of this module.
`TCP4:PORT` validates `0..65535` and returns `TCP4:port`.
`TRANSFER-BYTES` validates a non-negative length and returns `NUM:byte-len`.
`READ`, `READ-EXACT` and `WRITE` refuse a span longer than `0x7FFFF000`, the
most Linux moves in one transfer, and `SEND-SLICE` hands the OS at most that
much of a longer span.
Addresses and ports are converted to network byte order only at the foreign
boundary. A listening socket and a connected stream are **different types**:
`TCP4:listener` accepts, `TCP4:connection` carries bytes, and neither stands in
for the other. `TCP4:errno` preserves the positive host error number. Raw
nominal conversion words are not validators.

| Operation | Inputs | Result |
| --- | --- | --- |
| `BIND` | Local address, local port | `bind-result`: `bound listener` or `failed errno` |
| `LISTEN` | Listener, backlog | `status`: `ok` or `failed errno` |
| `LOCAL` | Listener | `endpoint-result`: `endpoint address port` or `failed errno` |
| `PENDING?` | Listener | `ready-result`, described below |
| `PENDING-WITHIN?` | Listener, deadline in milliseconds | `ready-result`, described below |
| `ACCEPT` | Listener | `accept-result`: `accepted connection address port` or `failed errno` |
| `CONNECT` | Remote address, remote port | `connect-result`: `connected connection` or `failed errno` |
| `READABLE?` | Connection | `ready-result`, described below |
| `READABLE-WITHIN?` | Connection, deadline in milliseconds | `ready-result`, described below |
| `READ` | Connection, writable byte span | `read-result`, described below |
| `READ-EXACT` | Connection, writable byte span | `read-result`, described below |
| `SEND-SLICE` | Connection, borrowed byte span | `slice-result`: `moved byte-len`, `empty` or `failed errno` |
| `WRITE` | Connection, borrowed byte span | `status`: `ok` or `failed errno` |
| `NODELAY!` | Connection, bool | `status`: `ok` or `failed errno` |
| `UNREAD` | Connection | `queue-result`: `queued byte-len` or `failed errno` |
| `UNSENT` | Connection | `queue-result`: `queued byte-len` or `failed errno` |
| `SHUTDOWN` | Connection, direction | `status`: `ok` or `failed errno` |
| `CLOSE` | Connection | `status`: `ok` or `failed errno` |
| `CLOSE-LISTENER` | Listener | `status`: `ok` or `failed errno` |
| `SOCKET` | — | `socket-result`: `opened connection` or `failed errno` |
| `LISTENER-FD` | Listener | `fd` |
| `CONNECTION-FD` | Connection | `fd` |

Byte spans are `ptr u8 NUM:byte-len`. Sockets are created with close-on-exec
set atomically. `ACCEPT`, `CONNECT`, `READ`, `READ-EXACT` and `WRITE` wait for
the stream; `PENDING?` and `READABLE?` are the non-blocking questions to ask
first, and their `-WITHIN?` forms wait a bounded time for the answer.

Every connection this module makes is non-blocking before the caller has it:
`ACCEPT`'s and `SOCKET`'s from the start, `CONNECT`'s once its blocking connect
has answered. A listener stays blocking. No send or receive waits in the
kernel: `READ` and `READ-EXACT` wait for data in poll(2) on the task's own
thread, with no deadline, as a blocking receive would. `SEND-SLICE` sends once,
waits at most `SEND-SLICE-MS` (100 ms) on the task's own thread for room, and
sends once more, so a slice returns within `SEND-SLICE-MS` and two sends that
never wait, whatever the peer does; `empty` says no byte found room. The bound
covers the connections this module makes; `>CONNECTION` sets no flag, so a
descriptor made elsewhere needs its owner to set `O_NONBLOCK`. `WRITE` loops
over slices and pauses its task after every slice that leaves bytes to send,
whether it moved some or none, so it still returns only when every byte is
accepted, but a halt ends the task within one slice however slowly the peer
reads. `NODELAY!` turns Nagle's algorithm off or on for a connection. The
non-blocking path has not been run on Linux.

`UNREAD` and `UNSENT` count a connection's two queues as the OS holds them,
without moving a byte. `UNREAD` answers the bytes the peer has sent that this
end has not read yet (`FIONREAD`); `UNSENT` answers the bytes this end has
written that the connection still holds, unsent or sent and not yet
acknowledged (Darwin's `SO_NWRITE`, Linux's `SIOCOUTQ`). On Linux a FIN that
`SHUTDOWN` sent counts as one byte until it is acknowledged, because `SIOCOUTQ`
subtracts sequence numbers. A byte leaves the writer's `UNSENT` only once the
peer has it, so while the peer reads nothing the writer's `UNSENT` and the
peer's `UNREAD` together hold every byte written, counted twice at most until
its acknowledgement arrives. Neither count has been run on Linux.

Address zero binds every local IPv4 interface; port zero
requests an ephemeral port, obtainable with `LOCAL`. A failed `BIND` or
`CONNECT` closes the newly created socket and retains the original failure's
errno, so a `failed` result never leaks a descriptor.

`SOCKET`, `LISTENER-FD` and `CONNECTION-FD` are the descriptor seam an
asynchronous caller needs and this module's own blocking surface does not.
`SOCKET` answers an AF_INET stream socket with the same flags `BIND` and
`CONNECT` ask for, bound to nothing and connected to nothing, as a
`TCP4:connection` because that is what it becomes and what `CLOSE` ends. The two
`-FD` words read the descriptor out of a handle without giving it away: the
handle still owns it, and `CLOSE` or `CLOSE-LISTENER` is still what closes it.
They exist so a listener can be handed to `AIO:ACCEPT` and a fresh socket to
`AIO:CONNECT` ([aio.md](aio.md)).

`LISTEN` takes a backlog of `1..4096` (Linux `SOMAXCONN`), the queue of
connections completed but not yet accepted; the kernel may cap it lower.
`ACCEPT` answers the connection **and** the peer's address and port, and the
accepted connection is owned and closed separately from its listener.

`READ` performs one transfer of `1..0x7FFFF000` bytes and returns as soon as the
stream answers, so a short read is normal. `READ-EXACT` waits until the whole
span is filled. Both share one result, which requires an exhaustive match:

| Variant | Payload | Meaning |
| --- | --- | --- |
| `data` | Byte length | Bytes copied into the supplied span: `1..capacity` for `READ`, the whole span for `READ-EXACT` |
| `closed` | Byte length | The peer ended the stream; the payload is the bytes delivered into the span first, always zero for `READ` |
| `failed` | Errno | OS receive failure |

An end of stream is not an error and not an empty message: a stream has no
message boundaries, so `closed` is the only way a transfer answers zero bytes.
The `closed` length of a partial `READ-EXACT` is a readable prefix of the span.

`WRITE` returns `ok` only when the OS accepted every byte, which is not
delivery. A `failed` write may already have transmitted part of the span; the
stream is then unusable and the caller closes it. An empty span is `ok` and
sends nothing. Writing to a closed peer fails with `EPIPE` and never raises
`SIGPIPE`, because every send carries `MSG_NOSIGNAL`.

`PENDING?` and `READABLE?` ask the same question of a listener and a connection
without waiting, and `PENDING-WITHIN?` and `READABLE-WITHIN?` ask it with a
deadline:

| Variant | Meaning |
| --- | --- |
| `ready` | `ACCEPT` / `READ` will answer at once |
| `idle` | Nothing is waiting; for the timed forms, the deadline passed first |
| `failed` | Errno the loop reported for the readiness operation |

`ready` covers the end of stream and a failed connection as well as data, since
those also answer immediately; the following `READ` reports which it was.

**All four questions run on the AIO loop** ([aio.md](aio.md)): each one submits
a single `AIO:POLL` for the descriptor with the caller's timeout as the
poll's own linked deadline and awaits it, so a task waiting for its peer costs
no CPU and no thread of its own — only its own socket and its own task-local
endpoint row, and other tasks keep running. The program starts the loop:
`AIO:START` must have run before the first wait, and a wait without it is
`E-AIO-STATE`; there is no fallback to `poll(2)`. Because the deadline belongs
to the operation the kernel is running, a signal no longer cuts the wait short
and nothing has to be resumed. A timeout of zero asks the question with a
zero-length deadline and is exactly `READABLE?` / `PENDING?`; a timeout below
zero or above `0x7FFFFFFF` throws `E-OPERAND` before any descriptor is touched.

`SHUTDOWN` half-closes a live stream in one direction or both and leaves the
descriptor open until `CLOSE`. The direction is the `TCP4:direction` enum built
by `RECEIVING`, `SENDING` or `BOTH`; `SENDING` is the one that tells the peer
the stream has ended while still reading its reply.

An interrupted `ACCEPT`, `READ`, `READ-EXACT` or `WRITE` retries against
the same descriptor; a readiness wait has nothing to retry, because the loop
absorbs the signal. An interrupted `CONNECT` does **not**: Linux
[completes that connection asynchronously](https://man7.org/linux/man-pages/man2/connect.2.html),
so it is reported as `failed` with `EINTR` and its socket is closed.

The caller owns each listener and connection and closes it once after its last
use. Do not reuse a handle after `CLOSE`, including an interrupted close: Linux
[releases the descriptor before reporting such errors](https://man7.org/linux/man-pages/man2/close.2.html).
Handle lifetime is a caller obligation, not a linear ownership proof.

## Foreign boundary and errors

The API and control flow are checked Habu. Fourteen exact libc schemas reach
the bounded FFI as `FUNCTION:` declarations, each stating the C function's own
effect: `socket`, `bind`, `listen`, `accept4` and Darwin's `accept`, `connect`,
`getsockname`, `recv`, `send`, `setsockopt`, `getsockopt`, `ioctl`,
`shutdown` and `close`; errno is package FFI's binding, shared by every
consumer. `ioctl` is declared variadic past its request, which Darwin passes
on the stack. Argument preparation and result normalization are checked
helpers, and the module carries no `TRUSTED:` body at all. Writable extents are
explicit. C `int` returns are
normalized from 32 bits; `ssize_t` results retain the host's 64 bits. The
16-byte `sockaddr_in` and four-byte `socklen_t` layouts
come from this platform's libc headers, not an application wire format.
`accept4` carries `SOCK_CLOEXEC` so an accepted connection is never inherited
across an exec.

Temporary endpoint storage is task-local, as are the existing FFI argument
tables. Each declared symbol resolves on its first call through
[`RTLD_DEFAULT`](https://man7.org/linux/man-pages/man3/dlsym.3.html) and is cached
by package FFI for every later caller. The native executable already depends on
libc; these addresses are borrowed from the process, without acquiring a library
reference.
Separate tasks may use separate sockets and buffers concurrently, which is what
lets one task block in `ACCEPT` while another connects; calls must not nest
within one task. Sharing one connection between readers requires the
application's own coordination. Input/output storage must remain valid for each
call. Loading the module opens no sockets.

Invalid operands throw `E-OPERAND` (`-9180`). An unsupported OS throws
`E-PLATFORM` (`-9181`) and an unexpected foreign result `E-RESULT` (`-9183`). A
symbol that will not resolve is package FFI's named failure, `E-FFI-DLSYM`
(`-3502`), raised at the first call that needs it; `E-SYMBOL` (`-9182`) is
retired with this module's own resolution. Ordinary OS operation failures are
result variants, not those exceptions.

Package FFI registers cache cleanup with
[`IMAGE-LIFECYCLE`](../lib/image-lifecycle.f) when the first declaration is
made, before any symbol is resolved, and a failed resolution retains that
registration for a later retry. Quiescent image preparation clears every cached
function address, so the next call resolves again. The module acquires no
dynamic-library reference to release. Callers must
close their descriptors and stop concurrent use before capture; live descriptors
are process resources, not serializable handles.

## Checks

```sh
bin/hb --load lib/net/tcp4-test.f
```

The suite holds both peers in one process: the main task binds an ephemeral
loopback port, listens, starts a listener task that blocks in `ACCEPT`, then
connects to it. It starts the AIO loop before its first case and stops it after
its last. Its assertions cover the echo round trip through `WRITE`,
`READ-EXACT` and the peer endpoint `ACCEPT` reports, a server half-close read
back as the end of stream, an idle listener and an idle stream answering `idle`
before a waiting connection and a sent request answer `ready`, a partial `READ`,
a refused connect reported as `failed` with `ECONNREFUSED`, a read through a
closed connection reported as `failed` with `EBADF`, a live stream half-closed
in each direction and in both, and out-of-range port, address, transfer,
backlog, capacity and timeout operands rejected before any socket call. It needs
neither external network access nor root, and binds only `127.0.0.1` on
kernel-chosen ports.

The timed waits are measured against the monotonic clock: a peer task that
writes after 50 ms answers `ready` within a 500 ms deadline after a wait of at
least 40 ms, a silent peer and a quiet listener answer `idle` after a 100 ms
deadline having waited at least 90 ms, a peer that closes answers `ready` and
reads back as the end of stream, and a pending connect answers `ready` on the
listener. A wait that ignored its timeout would return early and fail those
lower bounds, which is what separates a parked wait from a busy loop.

Two cases pin the loop itself. A task parked in a `READABLE-WITHIN?` on a silent
connection is counted in `/proc/self/task`: three threads where one ran before
the suite's first task — the base, the parked task, and the loop's one — so a
wait costs no thread of its own, and the peer's later write is what answers that
parked wait. A `READABLE-WITHIN?` with the loop stopped throws `E-AIO-STATE`
rather than falling back to `poll(2)`.

The non-blocking connections keep the blocking contracts: a `READ` waits out a
peer that writes after 50 ms and then reads its half-close as the end of
stream, and `WRITE` to a peer that stopped reading ends at a halt between
slices, as does one to a reader that takes bytes every 10 ms, within
`SEND-SLICE-MS` and the suite's 1000 ms slack of the halt. A slice to a peer
that reads as it comes moves exactly the bytes that arrive; once both loopback
buffers are full a slice answers `empty` after its wait; a reset connection's
slices fail with `EPIPE` or `ECONNRESET`, and the full pipe's two counts, the
writer's `UNSENT` and the peer's `UNREAD`, add up to every byte the slices
moved, part of it still unsent. The peer's `UNREAD` is exactly the bytes
waiting for it, less what a read takes; the writer's `UNSENT` empties once the
peer acknowledges them; both answer `EBADF` on a closed connection. Against a
reader that takes 16 KiB every 10 ms and answers each read, so the room it makes
is acknowledged at once, each of up to eight slices of a 16 MiB flood, each
handed at least 1 MiB, returns within `SEND-SLICE-MS` plus the suite's 1000 ms
scheduler slack; the line `sipped slices, ms:` prints their durations.

`E-PLATFORM`, `E-FFI-DLSYM` and `E-RESULT` have no case here: each needs a host
this build does not run on, a process without libc, or a kernel returning a
non-IPv4 endpoint.

These checks establish host behavior; they do not establish sustained
throughput, behavior on a lossy or congested network, connection counts beyond a
handful, or support for another host ABI.
