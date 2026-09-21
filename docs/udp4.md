# IPv4 UDP sockets

[`lib/net/udp4.f`](../lib/net/udp4.f) provides generic IPv4 UDP socket I/O.
The current implementation supports Habu on Linux AArch64 with glibc. It
contains no application protocol, packet sequencing, device addresses, or
capture policy. Other operating systems are rejected before opening a socket.
A `RECEIVE` that has to wait does so on the AIO loop ([aio.md](aio.md)), so a
program that receives with a non-zero timeout starts the loop with
`AIO:LOOP-START` first.

## Values and operations

`UDP4:ADDRESS` validates a numeric IPv4 address in `0..0xFFFFFFFF` and returns
the nominal `UDP4:address`; for example, `$7F000001 UDP4:ADDRESS` represents
`127.0.0.1`. `UDP4:ADDRESS$` parses a borrowed `ptr u8 n` string in strict
dotted-decimal form: exactly four decimal octets, without signs, surrounding
whitespace, or redundant leading zeros. It returns the same address type.
`UDP4:PORT` validates `0..65535` and returns `UDP4:port`.
`PAYLOAD-BYTES` validates `0..65507` and returns `NUM:byte-len`.
Addresses and ports are converted to network byte order only at the foreign
boundary. A `UDP4:socket` is a distinct handle type; `UDP4:errno` preserves
the positive Linux error number. Raw nominal conversion words are not validators.

| Operation | Inputs | Result |
| --- | --- | --- |
| `BIND` | Local address, local port | `open-result`: `opened socket` or `failed errno` |
| `LOCAL` | Socket | `endpoint-result`: `endpoint address port` or `failed errno` |
| `SEND` | Socket, destination address, destination port, borrowed byte span | `status`: `ok` or `failed errno` |
| `RECEIVE` | Socket, writable byte span, `ms` timeout | `receive-result`, described below |
| `CLOSE` | Socket | `status`: `ok` or `failed errno` |

Byte spans are `ptr u8 NUM:byte-len`. `BIND` creates a nonblocking socket
with close-on-exec set atomically. Address zero means all local IPv4 interfaces;
port zero requests an ephemeral port, obtainable with `LOCAL`. A bind failure
closes the newly created socket and retains the original failure's errno.

`SEND` accepts empty datagrams. Success means the OS accepted the entire
datagram, with no guarantee of delivery. Errors such as interruption or a full
send queue reach the caller; retry policy belongs to the application.

`RECEIVE` requires writable capacity `1..65507` and timeout `0..2147483647` ms.
Zero timeout performs an immediate receive attempt. An empty incoming datagram
is valid, distinct from timeout. The result requires an exhaustive match:

| Variant | Payload | Meaning |
| --- | --- | --- |
| `packet` | Byte length, source address, source port | Entire datagram copied into the supplied buffer |
| `truncated` | Original byte length, source address, source port | Only capacity bytes copied; remaining bytes discarded |
| `timeout` | None | No datagram obtained before the receive wait expired |
| `failed` | Errno | OS receive failure, or the errno the loop reported for the wait |

Lengths in `truncated` must **not** be used as readable buffer lengths. The
receiver uses Linux `MSG_TRUNC` to retain the original datagram size.
The socket is nonblocking, so `RECEIVE` tries `recvfrom` first and waits only
when there is nothing queued: it submits one `AIO:POLL-ADD` for the
milliseconds left on its absolute deadline and awaits it, so a receiving task
costs no thread of its own. A wait with no loop running is `E-AIO-STATE`; a
zero timeout never reaches the loop, since one immediate try is the whole call.
Interrupted receives and readiness races retry against that monotonic deadline;
they do not restart the full timeout. Source filtering and packet
reordering are application responsibilities. See the Linux
[`recvfrom` contract](https://man7.org/linux/man-pages/man2/recv.2.html).

The caller owns each opened socket and closes it once after its last use.
Do not reuse the handle after `CLOSE`, including an interrupted close: Linux
[releases the descriptor before reporting such errors](https://man7.org/linux/man-pages/man2/close.2.html).
Handle lifetime is a caller obligation, not a linear ownership proof.

## Foreign boundary and errors

The API and control flow are checked Habu. Six exact libc schemas reach the
bounded FFI as `FUNCTION:` declarations, each stating the C function's own
effect: `socket`, `bind`, `getsockname`, `sendto`, `recvfrom` and `close`;
errno is package
FFI's binding, shared by every consumer. Argument preparation and result
normalization are checked helpers, and the module carries no `TRUSTED:` body at
all. Writable extents are explicit. C `int` returns are
normalized from 32 bits; `ssize_t` results retain the host's 64 bits. The
16-byte `sockaddr_in` and four-byte `socklen_t` layouts
come from this platform's libc headers, not an application wire format.

Temporary endpoint storage is task-local, as are the existing FFI argument
tables. Each declared symbol resolves on its first call through
[`RTLD_DEFAULT`](https://man7.org/linux/man-pages/man3/dlsym.3.html) and is cached
by package FFI for every later caller. The native executable already depends on
libc; these addresses are borrowed from the process, without acquiring a library
reference.
Separate tasks may use separate sockets and buffers concurrently; calls must not nest within one task. Sharing a socket
between receivers requires the application's own coordination. Input/output
storage must remain valid for each call. Loading the module opens no sockets.

Invalid operands throw `E-OPERAND` (`-9100`). An unsupported OS throws
`E-PLATFORM` (`-9101`) and an unexpected foreign result `E-RESULT` (`-9103`). A
symbol that will not resolve is package FFI's named failure, `E-FFI-DLSYM`
(`-3502`), raised at the first call that needs it; `E-SYMBOL` (`-9102`) is
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

A fresh-process image regression remains required before shipping saved
applications containing this library: the suites below prove host behaviour,
not image capture.

## Checks

```sh
bin/hb --load lib/net/udp4-test.f
python3 test/net/udp4.py
```

The Habu suite holds both peers in one process: two sockets bound to ephemeral
`127.0.0.1` ports, with the AIO loop started before its first case and stopped
after its last. Its 58 assertions cover a 64-byte datagram delivered with its
length, its bytes and its sender's address and port; an idle socket answering
`timeout` having waited out a 30 ms deadline against the monotonic clock; a zero
timeout answering `timeout` at once and answering a queued datagram without
waiting; a 64-byte datagram received into a 16-byte capacity reported as
`truncated` carrying 64 with the first bytes delivered; a `RECEIVE` parked in the
loop from a task and answered by the main thread's later `SEND`; and a `RECEIVE`
that must wait with the loop stopped refused as `E-AIO-STATE`.

The Python harness runs Habu's supported source-list loader and an independent
Python
UDP peer on localhost. Its 36 native cases cover empty and maximum-size
datagrams in both directions, exact source endpoints and lengths, truncation
with buffer guards, immediate and timed empty receives, nonblocking/close-on-exec
flags, failed bind cleanup/error reporting, operations after close, numeric
limits, strict dotted-decimal parsing, concurrent first use and reception on separate sockets, and rejected
nominal substitutions. It needs neither external network
access nor root. Scratch files stay under ignored `tmp/test-udp4/`.

These checks establish host behavior; they do not establish sustained network
throughput, behavior under real packet loss, or support for another host ABI.
