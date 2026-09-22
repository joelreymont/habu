# HTTPS client over libcurl

[`lib/net/curl.f`](../lib/net/curl.f) binds libcurl's easy interface through the
`FUNCTION:` declarer so Habu can speak HTTP and HTTPS. It is Habu on Linux
AArch64 with glibc and `libcurl.so.4`; every other platform is rejected before a
handle exists. VFX Forth takes the same route — bind libcurl rather than write a
client and a TLS stack ([socket-models.md](socket-models.md)) — and TLS,
redirects, proxies, compression and the system CA bundle come with it.

The package carries no application protocol: no retry policy, no authentication,
no JSON, no URL construction. It moves one request and one response.

## Values and operations

`CURL:handle` is the easy handle, `CURL:code` a `CURLcode`, and
`CURL:http-status` a response status line's code. All three are nominal cell
types, so a curl code cannot be passed where a status is wanted. Their raw
conversions (`CURL:HANDLE>N`, `CURL:>HANDLE`, …) are boundaries, not validators.
Byte spans are `ptr u8 n`; a response capacity is `ptr u8 len`.

| Operation | Inputs | Result |
| --- | --- | --- |
| `INIT` | | `init-result`: `ready handle` or `failed code` |
| `URL!` | Handle, borrowed URL text | `status`: `ok` or `failed code` |
| `METHOD!` | Handle, borrowed method text | `status` |
| `HEADER+` | Handle, borrowed `Name: value` line | `status` |
| `BODY!` | Handle, borrowed byte span | `status` |
| `COOKIE-FILE!` | Handle, borrowed path | `status` |
| `COOKIE-JAR!` | Handle, borrowed path | `status` |
| `TIMEOUT!` | Handle, `ms` | `status` |
| `LOW-SPEED!` | Handle, bytes a second, seconds | `status` |
| `FOLLOW!` | Handle, `bool` | `status` |
| `PERFORM` | Handle, writable byte span | `fetch-result`, described below |
| `CLEANUP` | Handle | |

Every input span is borrowed for its own call: libcurl copies the URL, the
method, each header line, both cookie paths and the request body before the
setter returns, so nothing has to stay alive until `PERFORM`.

`INIT` runs `curl_global_init` once per process, turns off libcurl's use of
signals (`CURLOPT_NOSIGNAL`), restricts the schemes to HTTP and HTTPS, and
empties the handle's private slot. The restriction covers both the URL the
caller sets and whatever a redirect names, so a `file://` or `ftp://` URL out of
a scraped page cannot reach the filesystem or another service through a handle:
it fails with `CURLE_UNSUPPORTED_PROTOCOL` (1) before anything is opened.
A handle that cannot be created or configured is cleaned up before `INIT`
answers, so a failure never leaves one allocated. An image capture clears the
initialised flag, because a restored image is a different process.

`METHOD!` sets the request verb. Without it libcurl sends `GET`, or `POST` once
`BODY!` has been used; `METHOD!` overrides the verb and changes nothing else.

`HEADER+` appends one line to the handle's header list. The list head lives in
the handle's own `CURLOPT_PRIVATE` slot, so `CLEANUP` frees exactly the list that
handle owns and no package-side table tracks it. An append that cannot allocate
answers `failed` with `CURLE_OUT_OF_MEMORY` and leaves the previous list intact.

`COOKIE-FILE!` names a Netscape or Set-Cookie format file read at request time
and starts libcurl's cookie engine; an empty path starts the engine with no
stored cookies. `COOKIE-JAR!` names the file the whole cookie store is written to
when the handle is cleaned up.

`TIMEOUT!` bounds the WHOLE transfer, not one read. Zero removes the bound.

`LOW-SPEED!` is the other rule, a stall test rather than a ceiling: the transfer
is ended only while it stays below a rate, in bytes a second, for a number of
seconds (`CURLOPT_LOW_SPEED_LIMIT` and `CURLOPT_LOW_SPEED_TIME`). A document that
arrives slowly but never stops therefore finishes, however large it is, while a
peer that stops sending is dropped after the window instead of after the whole
transfer's limit. The two are independent: a handle may carry both, and zero in
either low-speed value removes the test. Both values are C longs, `0..2147483647`,
and the rate is set first: a failure there is answered before the window is
touched. A transfer ended by the stall test fails with
`CURLE_OPERATION_TIMEDOUT` (28), the code `TIMEOUT!` also yields.

## Performing a request

`PERFORM` writes into a caller-owned span and requires capacity `1..67108864`.
Habu cannot hand libcurl a callback into checked code, and none is needed:
libcurl's default write callback is `fwrite`, so `CURLOPT_WRITEDATA` is given an
`open_memstream` stream and `memcpy` moves the finished bytes into the caller's
span. The stream and its buffer are released on every branch, and the handle
never keeps a pointer to a closed stream.

The result requires an exhaustive match:

| Variant | Payload | Meaning |
| --- | --- | --- |
| `response` | `http-status len` | The whole body, `len` bytes, is in the caller's span |
| `truncated` | `http-status len` | The body was `len` bytes and the first `capacity` were copied |
| `failed` | `code` | The transfer failed; the `CURLcode` says why |

`truncated` carries the WHOLE body's length while only capacity bytes were
copied, so a short read is always visible: a caller that ignores the distinction
still cannot mistake a partial body for a complete one. `failed` covers every
transport outcome, including `CURLE_OPERATION_TIMEDOUT` (28) when `TIMEOUT!`
expires and `CURLE_COULDNT_CONNECT` (7) when nothing answers. An HTTP error
status is NOT a failure: a 404 is a `response` carrying 404.

`CLEANUP` destroys the handle and frees its header list; neither is ever left
allocated. The handle is dead afterwards and must not be used again.

## Many transfers on one task

`PERFORM` holds its task for the whole transfer, so ten transfers that way cost
ten threads parked in libcurl. The words below are the other shape: ONE
package-owned task drives every transfer, and a task that starts one is free
until it asks for the answer.

| Operation | Inputs | Result |
| --- | --- | --- |
| `LOOP-START` | | `status` |
| `START` | Handle, writable byte span | `status` |
| `AWAIT` | Handle | `fetch-result` |
| `CANCEL` | Handle | |
| `LOOP-STOP` | | |

The loop task parks in [AIO](aio.md), not in libcurl. One turn is: take the
first AIO ticket of the loop's group that ended; drain the wake pipe and scan
the transfer records if that was the wake ticket, forget the timer if it was the
timer, free an fd's slot if it was a poll; call `curl_multi_perform` and take
every `CURLMSG_DONE` from `curl_multi_info_read`; then ask `curl_multi_fdset`
which descriptors libcurl is waiting on and `curl_multi_timeout` how long it may
wait, and rebuild the tickets from those answers. A descriptor still wanted with
the mask its ticket holds keeps that ticket; one whose mask changed or that left
the sets is `AIO:CANCEL`led and armed again if it comes back; the exception set
is asked for as readable and writable together. The multi timeout becomes one
`AIO:TIMEOUT` ticket — 100 ms when libcurl answers -1, never more than 1000 ms,
and never less than 1 — re-armed when it fires and replaced when libcurl asks
for a shorter one. libcurl's other event shape,
`curl_multi_socket_action` with `CURLMOPT_SOCKETFUNCTION` and
`CURLMOPT_TIMERFUNCTION`, is callbacks, and Habu hands libcurl no callback — the
same rule `PERFORM`'s memstream answers.

`AIO:LOOP-START` is the program's job and has to have happened first: a
`LOOP-START` with no AIO loop running answers AIO's own `E-AIO-STATE`. The wake
pipe is what `START`, `CANCEL` and `LOOP-STOP` write one byte to after they have
marked a record; the loop holds one `AIO:POLL-ADD` on its read end, and a byte
that arrives between the read and the scan costs one extra turn, never a missed
record.

Every per-handle option — `URL!`, `METHOD!`, `HEADER+`, `BODY!`,
`COOKIE-FILE!`, `COOKIE-JAR!`, `TIMEOUT!`, `LOW-SPEED!`, `FOLLOW!` and the
scheme restriction — is set before `START` and holds for that transfer alone, so
one transfer's timeout, stall or failure ends that transfer and nothing else.

From `START` until `AWAIT` answers, the handle and the span belong to the loop:
no other task may touch either, and nothing outside the loop asks libcurl about
the handle. That is why a waiter finds its own transfer by scanning the
package's records for its handle rather than calling `curl_easy_getinfo` on a
handle the loop may be driving — `CURLOPT_PRIVATE` already carries that handle's
header list. A second `START` on a handle already in flight is `CURL:E-STATE`,
and so are `AWAIT` and `CANCEL` on a handle with none, and an `AWAIT` from a
task that did not start it.

`AWAIT` answers the fetch-result `PERFORM` would have answered, with the same
`response`, `truncated` and `failed` meanings, and it is what gives the transfer
record back: every started transfer is awaited exactly once, cancelled ones
included. The wait is a `TASK:STOP` loop, so it costs no CPU and the main thread
may wait too. A `TASK:HALT` while it waits ends the calling task at its next
`TASK:PAUSE`, and the record is abandoned first: the loop takes the handle out,
drops the body and frees the record without waking anybody, which is what keeps
the loop from waking a TCB the join has released. No `TASK:AT-EXIT` is
registered — the abandon in that wait is what a halted waiter needs.

`CANCEL` ends one early — the loop takes the handle out of the multi handle,
drops whatever body it had collected and answers the waiter `failed` with
`CURLE_ABORTED_BY_CALLBACK` (42) — and a transfer that finished first keeps its
result, so a cancel that loses that race changes nothing.

`LOOP-STOP` refuses with `CURL:E-STATE` while any record is not free, exactly as
`AIO:LOOP-STOP` refuses a ring the kernel still owns: await or cancel-and-await
everything first. It then wakes the loop, joins it, destroys the multi handle
and closes the pipe, and rethrows whatever ended the loop. Every ticket the loop
submitted is cancelled and awaited before it returns, so the AIO loop can be
stopped after it. A second `LOOP-START`, and a `LOOP-STOP` with no loop running,
are `CURL:E-STATE`. After a stop the loop can be started again.

The table holds `MAX-TRANSFERS` (32) transfers at once and a `START` past that
is `CURL:E-CAPACITY`. Thirty-two rather than the sixty-four a record table alone
would allow: one AIO group holds `AIO:GROUP-MAX` (64) tickets, two of them are
the wake pipe's and the timer's, and a transfer can hold more than one
descriptor while it resolves and connects. When the sets still want more tickets
than the group has room for, the descriptors left over are not armed that turn
and the timer is capped at 100 ms instead, so the loop asks again: a bounded
delay, never a hang. An fd at or above `FD_SETSIZE` cannot appear in an fd_set
at all, so the sets also bound the loop at 1024 descriptors and that ceiling is
libcurl's own.

A multi call that refuses the handle it was given is a contract violation: the
loop ends every transfer in flight as `failed` with 42 so no owner is left
parked, gives its tickets back, and `CURL:E-RESULT` is rethrown from
`LOOP-STOP`.

## Failures

`CURL:E-PLATFORM` rejects a non-Linux target before anything is allocated.
`CURL:E-STATE` rejects a handle that was never opened. A handle used after
`CLEANUP` is a use-after-free the nominal type does not prevent: the value is
still nonzero, so treat `CLEANUP` as the end of that handle's life.
`CURL:E-OPERAND` rejects a bad argument: a capacity, body size, timeout or
low-speed value outside its range, and a URL, method, path or header line
containing a NUL, which would otherwise be silently cut short at the C string
boundary.
`CURL:E-RESULT` reports a libcurl contract violation, such as a status outside
`0..999` after a successful transfer, or a multi call that refuses the handle it
was given. `CURL:E-CAPACITY` rejects a `START` with no transfer record free. A
missing libcurl symbol is package FFI's `E-FFI-DLSYM`, named where the first
call stands.

## Declarations

Every foreign function is declared with `FUNCTION:` (see `lib/ffi-abi.f`).
`CURL*`, `curl_slist*` and `FILE*` are opaque — nothing in the package
dereferences one — so they are declared `n`: AAPCS64 passes a pointer and an
integer in the same register, and a cell is what the nominal handle types wrap.
Only a span Habu or the callee really reads or writes is declared `ptr u8`, which
is what the bounded call guards.

`curl_easy_setopt` and `curl_easy_getinfo` are variadic. On Linux AAPCS64 a
variadic integer or pointer argument uses the same register as a fixed one, so
one declaration per ARGUMENT SHAPE is exact, and the two shapes
(`SETOPT-NUM ( n n n -- n )` and `SETOPT-SPAN ( n n ptr u8 -- n )`) share the
symbol under different Habu names. `INIT`'s platform gate is what keeps that
true: Apple's ARM64 variant passes variadic arguments on the stack instead.

Option numbers are written as curl.h writes them — the option type's base plus
the option's own number — so each one is checkable against
`/usr/include/curl/curl.h` on sight. The scheme restriction uses the bitmask
options `CURLOPT_PROTOCOLS` (181) and `CURLOPT_REDIR_PROTOCOLS` (182), which are
deprecated in favour of `CURLOPT_PROTOCOLS_STR` (318) and
`CURLOPT_REDIR_PROTOCOLS_STR` (319) but still honoured and carry no version
floor; the string form is the replacement once the libcurl floor is 7.85.

## Tests

`lib/net/curl-test.f` holds both peers in one process: a server task binds a
[TCP4](tcp4.md) listener on a free loopback port, publishes the port in a shared
cell and accepts one connection at a time, speaking enough HTTP/1.0 to answer
the cases, while the main task drives package CURL against it. It needs nothing
on PATH and opens no outward connection.

The cases are a GET with the fixture's exact bytes and its `Content-Length`, a
404 for an unknown path, a 304 the server gives only when it parsed
`If-Modified-Since` out of the request, a POST with headers and a body and a
custom `DELETE` (both 501), a `Set-Cookie` that round trips through the jar file
and comes back on a second request, a truncated response, a path the server
accepts and never answers so only `TIMEOUT!` can end it, a path it serves in
timed chunks — slow, about 640 bytes a second, but never stalled — where a
whole-transfer ceiling loses the body while `LOW-SPEED!` under that rate
finishes it whole and the same limit alone still ends the stalled path, a handle
with no URL, a `file://` URL to a readable file that is refused with its bytes
never reaching the buffer, and the refusals: a dead handle, a NUL inside a URL,
a low-speed rate below zero and a window past the C long ceiling, against a
handle that takes both the disabling pair and a real one.

The loop's own cases follow, with `AIO:LOOP-START` run first because a live task
forbids compilation: a `START` before `LOOP-START` and a second `LOOP-START`
refused with `E-STATE`; the same request answered identically by `PERFORM` and
by `START`/`AWAIT`, whole and against a span too short for it; thirty-two
transfers in flight at once, each with its own span, where the thread count read
from `/proc/self/task` during the run is the count before plus two — the CURL
loop and the AIO loop — and a thirty-third `START` is `E-CAPACITY`; one transfer
the server never answers ending as 28 under its own `TIMEOUT!` while three
others on the same loop answer 200; a transfer cancelled while the server holds
it open, which answers `failed` with 42; a transfer another task may not await,
a loop that refuses to stop while it is in flight, and a second `AWAIT` of it
refused; a submitter task halted while parked in `AWAIT`, after which the loop
stops once it has taken the abandoned record back; and the loop started again,
carrying one more transfer. A last case checks the server task itself served
every request and reported no fault.

Setting `HABU_NET_TESTS=1` adds one request to `https://example.com`, which is
the only case that leaves the machine and the only one that exercises TLS and
the system CA bundle; the gate runs without it.
