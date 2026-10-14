# Server-Sent Events framing

`src/lib/net/event-stream.f` is a checked, incremental framing decoder for
byte chunks received by a caller such as Maki's Responses stream. It has no
socket, reconnect policy, or event queue. The framing rules follow the
[HTML Server-Sent Events parsing algorithm, section 9.2.6](https://html.spec.whatwg.org/multipage/server-sent-events.html#parsing-an-event-stream).

Allocate `SSE:STORAGE-BYTES` at a cell-aligned address, initially zeroed, and
keep it exclusive until `SSE:CLOSE`. `SSE:OPEN ( ptr n n -- SSE:decoder )` checks
the storage size and returns a nominal handle. `SSE:FEED` takes the handle, a
`ptr u8 n` chunk, an opaque caller context pointer, and a checked quotation
`[ ptr a ptr u8 n ptr u8 n -- ]`. The quotation receives the context pointer,
event type, and data as
borrowed spans, valid only during that call. The default type is `message`.
`SSE:FINISH ( SSE:decoder -- )` ends input and discards an event without a
terminating blank line; later `FEED` throws `SSE:E-STATE`. `SSE:CLOSE` releases
the decoder's growable buffers, including after a callback throw, and permits
reopening the same storage.

`LF`, `CR`, and `CRLF` delimit lines across chunk boundaries. One leading UTF-8
BOM is stripped even when split across chunks. A field ends at its first colon;
one following space is removed. `data` lines join with LF, minus the final LF
at dispatch; `event` replaces the type. A blank line dispatches only when a
data field appeared, including an empty `data:` or `data` field. Comments,
unknown fields, `id`, and `retry` are ignored. Valid UTF-8, including NUL, is
passed byte for byte. Invalid UTF-8 in a completed event throws `SSE:E-UTF8`
before its callback. A callback throw propagates and leaves the decoder in a
failed state; close it without feeding again.

`SSE:E-STORAGE` reports invalid storage or size, `SSE:E-SOURCE` reports a
negative byte count or null nonempty source, and `SSE:E-STATE` reports an
invalid lifecycle operation. Buffer allocation errors propagate from `BUF`.

The focused native check is:

```sh
bin/hb --load src/lib/net/event-stream-test.f
```
