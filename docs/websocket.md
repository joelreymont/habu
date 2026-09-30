# WebSocket

Habu's RFC 6455 support is built from the bytes up. What exists is pure byte
work and needs no socket or HTTP server:

| file | package | what it does |
| --- | --- | --- |
| `lib/net/ws-frame.f` | `WS` | encodes and decodes frame headers, masks payloads, writes close codes |
| `lib/crypto/sha1.f` | `SHA1` | the digest behind `Sec-WebSocket-Accept` ([crypto](crypto.md#sha-1-for-the-websocket-handshake)) |
| `lib/base64.f` | `BASE64` | the encoding of the key and of the accept value ([stdlib](stdlib.md#base64)) |

The words that own a connection (the upgrade handshake, receiving whole
messages, answering pings, closing) are not written yet; they are the codec's
first caller.

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

The block -9330..-9339 (`E-WS-FIRST`, `E-WS-LAST` in `lib/errors.f`) belongs to
the WebSocket words. The codec holds -9330..-9335, and the rest is left for the
connection words.

## Tests

`bin/hb --load lib/net/ws-frame-test.f`, SUITE `websocket-frame`, writes and
reads section 5.7's example frames byte for byte and unmasks its masked
"Hello". It takes each length form to its edges (125, 126, 65535, 65536 and
2^63 - 1), round-trips headers of every opcode from both senders, answers
`need` for partial headers, and makes every refusal from the fewest bytes that
show it, with the payload bound met exactly and exceeded by one in each length
form.
