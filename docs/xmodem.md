# XMODEM packets and serial transfers

[lib/xmodem.f](../lib/xmodem.f) implements the XMODEM packet codec.
[lib/serial-xmodem.f](../lib/serial-xmodem.f) transfers bytes over an already-open
[raw serial stream](serial.md). Both are checked Habu. The codec has no I/O;
the transfer library adds no foreign boundary beyond `SERIAL`.

## Packet codec

`XMODEM` uses nominal `sequence`, `payload-size`, and `check-kind` roles:

- `SEQUENCE ( n -- sequence )` accepts 0 through 255.
- `BLOCK ( n -- payload-size )` accepts 128 or 1024 payload bytes.
- `CRC ( -- check-kind )` and `SUM ( -- check-kind )` select the two-byte
  CRC or one-byte arithmetic checksum.
- `BYTES ( n -- CAD-NUM:byte-len )` validates a nonnegative span length.
- `PACKET-BYTES ( payload-size check-kind -- CAD-NUM:byte-len )` includes
  the three header bytes and checksum trailer.
- `ENCODE ( ptr u8 CAD-NUM:byte-len sequence payload-size check-kind ptr a -- )`
  replaces an initialized output `BUF` with one packet. The borrowed input
  must be disjoint from the output buffer and no longer than the selected
  payload size. Empty input is allowed; all unused payload bytes are `0x1A`.
  Invalid operands leave the buffer unchanged.
- `DECODE ( ptr u8 CAD-NUM:byte-len check-kind -- ptr u8 CAD-NUM:byte-len sequence )`
  checks a complete packet, then borrows its full payload and returns its
  sequence number. It validates SOH/STX, exact length, complemented sequence,
  and the negotiated checksum. It does not consume EOT or other controls.

CRC uses polynomial `0x1021`, initial value zero, no reflection or final XOR,
and transmits the high byte first. The arithmetic checksum is the payload sum
modulo 256. The codec can describe either trailer with either payload size;
the serial sender selects 128-byte packets when a peer requests checksum mode.

`SOH`, `STX`, `EOT`, `ACK`, `NAK`, `CAN`, and `REQUEST-CRC` expose control-byte
values. `E-OPERAND` (-9120) rejects invalid configuration or input lengths;
`E-FRAME` (-9121) rejects malformed packets. Buffer errors retain their own
codes. Generated nominal conversions do not validate values; operations
recheck their operands.

## Serial session

`SERIAL-XMODEM` uses one caller-owned session header:

```forth
require lib/serial-xmodem.f
create TRANSFER SERIAL-XMODEM:SESSION-BYTES allot
```

`INIT ( SERIAL:handle ms ms ptr a -- )` initializes fresh, zero-initialized storage
of `SESSION-BYTES` bytes. It borrows the open handle and allocates a private
packet buffer. The first wait argument controls waiting for an ACK or the next
packet header. Choose it to allow the entire packet to reach
the peer at the configured baud rate. For example, a 1024-byte packet at
115200 baud takes about 90 ms on an 8N1 link before peer processing. A slower
link needs a longer control wait, as does a peer that processes a command before
starting its reply. The second wait controls receive inactivity within a frame,
including the quiet interval used when draining a rejected frame. It must allow
normal inter-byte gaps and be shorter than the peer's retransmission interval.
These waits are separate because a peer's EOT acknowledgment can take longer
than the gap it permits during packet recovery. Both waits must be 1 through
`INT32_MAX` milliseconds.

`DISPOSE ( ptr a -- )` releases the packet buffer and is idempotent. It does
not close the borrowed serial handle. Dispose the session, then close the
handle exactly once in the caller's cleanup path.

- `SEND ( ptr u8 CAD-NUM:byte-len XMODEM:payload-size ms ptr a -- transfer-result )`
  sends a borrowed source span. The requested payload size applies when the
  receiver requests CRC using `C`; a `NAK` handshake selects checksum and
  128-byte payloads. An exact packet multiple does not cause an extra padding
  packet. Empty input sends EOT after negotiation.
- `RECEIVE ( CAD-NUM:byte-len ptr a ms ptr a -- transfer-result )` takes a
  maximum byte count, an initialized destination `BUF`, an overall timeout,
  and the session. It clears the destination before starting and appends
  accepted packets. The destination must be disjoint from the session and its
  private storage. The limit includes complete padded packets.

The timeout on each operation is a fresh **overall transfer deadline**, from
1 through `INT32_MAX` milliseconds. Negotiation, retries, incoming noise,
partial frames, and receive draining all share that deadline. The shorter
per-session waits control individual control waits and receive inactivity. Neither
extends the overall deadline.

Each transfer begins at sequence 1 and wraps through 255 to 0. The sender
requires ACK for each packet and for EOT, resending an identical packet after
NAK or a missing ACK. The receiver accepts both payload sizes, ACKs a repeated
previous sequence without appending it again, and rejects an unexpected
sequence. It requests CRC three times before trying checksum when no packet
has started; once a packet starts, the negotiated check kind remains fixed.
Rejected or incomplete frames are drained to an inactivity boundary before
NAK. Ten consecutive failed attempts end the transfer; a newly accepted packet
resets the receiver's retry count.

Two consecutive CAN bytes at a control boundary cancel the transfer and are
acknowledged. A single CAN does not cancel. On other failures, the library
attempts to send three CAN bytes without waiting beyond the deadline.

`transfer-result` variants are:

| Variant | Meaning |
| --- | --- |
| `completed byte-len` | SEND: original source bytes, after EOT ACK. RECEIVE: full padded payload bytes, after acknowledging EOT. |
| `timeout` | Overall transfer deadline expired. |
| `closed` | The serial stream closed. |
| `failed SERIAL:errno` | A serial operation failed; the original positive errno is retained. |
| `cancelled` | The peer sent two CAN bytes. |
| `retry-limit` | Ten consecutive failed attempts. |
| `capacity` | The next complete payload would exceed the receiver's limit. |

Invalid wait, timeout, or handle representation throws `E-OPERAND` (-9122);
codec operand and buffer-state/allocation errors retain their original codes.
Failure may leave an accepted prefix in the receive buffer. Treat the transfer
as complete only on `completed`. XMODEM does not carry an exact binary length,
so padding is never stripped. The application must interpret its own length
field or other framing and check its own command response.

A session can be reused for successive SEND and RECEIVE operations, each with
fresh sequence, negotiation, progress, and deadline state. After failure the
caller must establish that the peer is ready for the next transfer. Distinct
tasks can use distinct sessions, buffers, and serial handles concurrently.
Concurrent use of one session or multiple consumers of one stream is not
supported. Source/destination storage must remain live for the whole call.

## Verification and image lifetime

Run through the native supported loader:

```sh
python3 test/xmodem.py
python3 test/serial-xmodem.py
```

The packet tests compare independent Python packets and `binascii.crc_hqx`
results. The serial tests use real kernel pseudoterminals and an independent
peer, covering data, retries, partial input, EOT acknowledgment, cancellation,
timeouts under incoming traffic, capacity, sequence wraparound, and session
reuse. They do not exercise a physical device.

On 2026-09-10, the codec passed **57 native checks**, and serial transfers
passed **45**, including separate control/inactivity waits and two concurrent
Habu tasks reusing independent sessions and ports. The global error-code lint
also passed with no findings. These are focused library checks; they do not
establish a rebuilt compiler or a full engine-suite result.

The tested execution path is normal source loading. The underlying serial
library's [saved-image lifecycle limitation](serial.md) still applies.
Application-owned live handles and initialized sessions must not be captured
as reusable resources for a different process.

The packet and transfer rules follow the
[XMODEM/YMODEM Protocol Reference](https://techheap.packetizer.com/communications/modems/xmodem-ymodem_reference.html),
including Ward Christensen's XMODEM description, John Byrns's CRC specification,
and Chuck Forsberg's 1K/abort extensions. This library implements single-file
XMODEM, not YMODEM batch metadata or streaming YMODEM-g.
