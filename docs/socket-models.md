# Socket models in SwiftForth and VFX Forth

How two hosted Forth systems expose TCP, restated in our words so Habu's
network packages can take proven verbs and one important structural idea:
sockets as generic I/O devices. See [tasking-models.md](tasking-models.md)
for the sources and their licences.

Sources, local copies as of 2026-09-16:

- SwiftForth 4.1.10: `lib/options/tcp.f` (78 lines) over the socket words the
  kernel imports; `lib/samples/echo-server.f`.
- VFX Forth 64 for Linux: `Lib/Lin64/Genio/SocketIo.fth` (942 lines); manual
  `Doc/VfxLin64.pdf` chapter 7 "Generic IO" (p. 79) and chapter 24 "Supported
  shared libraries" (p. 269); `Lib/SharedLibs/libcurl`.
- SwiftX ARM evaluation: `src/vio.f`, the vectored terminal I/O of the embedded
  target (see [tasking-models.md](tasking-models.md) section 2.1).
- Habu: [udp4.md](udp4.md), `lib/net/udp4.f`.

## 1. SwiftForth: a current socket and six verbs

`SOCKET: name` defines a socket record; `/TCP-SOCKET` initialises it, and
the words below act on the current socket (`>SOCKET`), which holds a server
descriptor and a connection descriptor. Errors throw with the failing word's
name and `errno`.

| Word | Effect | Meaning |
| --- | --- | --- |
| `SERVER` | `( port -- )` | address the socket as a server on a port |
| `/BIND` | `( -- )` | bind |
| `/LISTEN` | `( -- )` | listen with a default backlog of 5 |
| `/ACCEPT` | `( -- )` | block until a connection arrives; sets the connection descriptor |
| `/CONNECT` | `( -- )` | connect as a client |
| `TCP-READ` | `( addr u1 -- u2 )` | one `recv`, partial read |
| `TCP-READX` | `( addr u -- )` | read exactly `u` bytes |
| `TCP-WRITE` | `( addr u -- )` | send all bytes |

The echo server is fifteen lines: listen, then `BEGIN /ACCEPT ['] service
CATCH DROP AGAIN` inside one task. That is the whole server shape: one
accept loop, one catch per connection.

## 2. VFX Forth: sockets as generic I/O devices

VFX declares the BSD calls (`socket`, `bind`, `listen`, `accept` as
`SACCEPT`, `connect`, `send`, `recv`, `poll`, `getaddrinfo`, ...) through its
shared-library interface, then offers two layers.

Plain helpers:

| Word | Effect | Meaning |
| --- | --- | --- |
| `TCPConnect` | `( c-addr u port -- socket ior )` | resolve a host name, connect |
| `UDPConnect` | `( c-addr u port -- socket ior )` | |
| `readsock`, `writesock` | `( c-addr u hsock -- len ior )` | one transfer |
| `sockReadLen` | `( c-addr len hsock -- ior )` | read exactly |
| `pollsock` | `( hsock -- #bytes or -1 )` | readable count without blocking |
| `closesocket` | | |

The device layer: `SocketDev: name` creates a device record ("sid"). Opening
it takes a `/SDopen` structure (address family, type, protocol, `sockaddr_in`)
and a mode: `sd_socket` (raw), `sd_connect` (resolves the name, connects) or
`sd_listen` (binds, listens with `SOMAXCONN`). The device's vector table
implements the generic operations every device has: `sd-emit`, `sd-key`,
`sd-key?`, `sd-peek`, `sd-read`, `sd-readex`, `sd-write`, `sd-accept` (line
input with backspace handling), `sd-flush`, `sd-close`, `sd-ioctl`,
`sd-init`, `sd-term`. Output is buffered (1440 bytes) and flushed on
`sd-flush` and `sd-cr`. Once open, `sid dup op-handle ! ip-handle !` makes
the socket the current task's input and output device: `EMIT`, `TYPE`,
`KEY`, `ACCEPT` and therefore the whole text interpreter work over the
connection without any change. A REPL over TCP costs nothing extra. Serial
ports are devices of the same kind (`Genio/Serial.fth`).

For HTTP and TLS VFX binds `libcurl` rather than writing a client; `zlib` and
`libiconv` are bound the same way.

## 3. Habu today

`lib/net/udp4.f` only: typed address, port and socket handles, result ADTs
carrying `errno`, Linux aarch64 with glibc. No TCP, no device abstraction:
`EMIT`, `TYPE` and `KEY` are bound to the process terminal.

## 4. What to adopt

| Gap | Model to follow | Dot |
| --- | --- | --- |
| No TCP | SwiftForth's verbs (`BIND`, `LISTEN`, `ACCEPT`, `CONNECT`, `READ`, `READ-EXACT`, `WRITE`, `CLOSE`) in `UDP4`'s typed style, plus VFX's non-blocking readable check | `habu-add-tcp-sockets-fb1d351e` |
| Text I/O bound to the terminal | VFX generic devices: a device record with the operations above, per-task current input and output device, terminal first, TCP second; the REPL becomes remote for free | `habu-route-text-i-1fbcb2ba`, done: [genio.md](genio.md) |
| HTTP client | `libcurl` through the FFI, as VFX does; server-side HTTP/1.1 is a small layer over `TCP4` once it exists | after the two above |
| TLS | terminate at a proxy for servers; `libcurl` or OpenSSL through the FFI for clients | with HTTP |

Serial links are the same device kind, which is what makes one program
structure serve the server and the microcontroller. SwiftX already does the embedded
half: its terminal words dispatch through per-task user-variable vectors
(`'EMIT`, `'KEY`, `'ACCEPT`, ...) in 57 lines, so the Habu device record must
stay small enough to be that table on a microcontroller.
