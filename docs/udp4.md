# IPv4 UDP sockets

[`lib/net/udp4.f`](../lib/net/udp4.f) provides generic IPv4 UDP socket I/O.
The current implementation supports Habu on Linux AArch64 with glibc. It
contains no application protocol, packet sequencing, device addresses, or
capture policy. Other operating systems are rejected before opening a socket.

## Values and operations

`UDP4:ADDRESS` validates a numeric IPv4 address in `0..0xFFFFFFFF` and returns
the nominal `UDP4:address`; for example, `$7F000001 UDP4:ADDRESS` represents
`127.0.0.1`. `UDP4:ADDRESS$` parses a borrowed `ptr u8 n` string in strict
dotted-decimal form: exactly four decimal octets, without signs, surrounding
whitespace, or redundant leading zeros. It returns the same address type.
`UDP4:PORT` validates `0..65535` and returns `UDP4:port`.
`PAYLOAD-BYTES` validates `0..65507` and returns `CAD-NUM:byte-len`.
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

Byte spans are `ptr u8 CAD-NUM:byte-len`. `BIND` creates a nonblocking socket
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
| `failed` | Errno | OS receive or poll failure |

Lengths in `truncated` must **not** be used as readable buffer lengths. The
receiver uses Linux `MSG_TRUNC` to retain the original datagram size.
Interrupted receives/polls and readiness races retry against a monotonic
deadline; they do not restart the full timeout. Source filtering and packet
reordering are application responsibilities. See the Linux
[`recvfrom` contract](https://man7.org/linux/man-pages/man2/recv.2.html).

The caller owns each opened socket and closes it once after its last use.
Do not reuse the handle after `CLOSE`, including an interrupted close: Linux
[releases the descriptor before reporting such errors](https://man7.org/linux/man-pages/man2/close.2.html).
Handle lifetime is a caller obligation, not a linear ownership proof.

## Foreign boundary and errors

The API and control flow are checked Habu. Eight small private `TRUSTED:`
definitions describe exact libc schemas through the existing bounded FFI:
`socket`, `bind`, `getsockname`, `sendto`, `recvfrom`, `poll`, `close`, and
`__errno_location`. Argument preparation and result normalization are checked helpers; each trusted
body contains only its fixed foreign call. Writable extents are explicit. C `int` returns are
normalized from 32 bits; `ssize_t` results retain the host's 64 bits. The
16-byte `sockaddr_in`, four-byte `socklen_t`, and eight-byte `pollfd` layouts
come from this platform's libc headers, not an application wire format.

Temporary endpoint/poll storage is task-local, as are the existing FFI argument
tables. A synchronized first call resolves process-owned libc symbols and
publishes them for other callers. Separate tasks may use separate sockets and
buffers concurrently; calls must not nest within one task. Sharing a socket
between receivers requires the application's own coordination. Input/output
storage must remain valid for each call. Loading the module opens no sockets.

Invalid operands throw `E-OPERAND` (`-9100`). Unsupported OS, failed library or
symbol resolution, and an unexpected foreign result throw `E-PLATFORM`
(`-9101`), `E-SYMBOL` (`-9102`), and `E-RESULT` (`-9103`). Ordinary OS operation
failures are result variants, not those exceptions.

The source-loaded host path is tested. Saving an application image after using
this library is not yet supported: cached libc addresses belong to the writing
process and need reset through the shared image lifecycle. That integration
and a fresh-process image regression remain required before shipping saved
applications containing this library. Live descriptors also remain process
resources; they are not serializable handles.

## Checks

```sh
python3 test/net/udp4.py
```

The harness runs Habu's supported source-list loader and an independent Python
UDP peer on localhost. Its 36 native cases cover empty and maximum-size
datagrams in both directions, exact source endpoints and lengths, truncation
with buffer guards, immediate and timed empty receives, nonblocking/close-on-exec
flags, failed bind cleanup/error reporting, operations after close, numeric
limits, strict dotted-decimal parsing, concurrent first use and reception on separate sockets, and rejected
nominal substitutions. It needs neither external network
access nor root. Scratch files stay under ignored `tmp/test-udp4/`.

These checks establish host behavior; they do not establish sustained network
throughput, behavior under real packet loss, or support for another host ABI.
