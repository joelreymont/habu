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

## Failures

`CURL:E-PLATFORM` rejects a non-Linux target before anything is allocated.
`CURL:E-STATE` rejects a handle that was never opened. A handle used after
`CLEANUP` is a use-after-free the nominal type does not prevent: the value is
still nonzero, so treat `CLEANUP` as the end of that handle's life.
`CURL:E-OPERAND` rejects a bad argument: a capacity, body size or timeout
outside its range, and a URL, method, path or header line containing a NUL,
which would otherwise be silently cut short at the C string boundary.
`CURL:E-RESULT` reports a libcurl contract violation, such as a status outside
`0..999` after a successful transfer. A missing libcurl symbol
is package FFI's `E-FFI-DLSYM`, named where the first call stands.

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
accepts and never answers so only `TIMEOUT!` can end it, a handle with no URL, a
`file://` URL to a readable file that is refused with its bytes never reaching
the buffer, and the two refusals. A last case checks the server task itself
served every request and reported no fault.

Setting `HABU_NET_TESTS=1` adds one request to `https://example.com`, which is
the only case that leaves the machine and the only one that exercises TLS and
the system CA bundle; the gate runs without it.
