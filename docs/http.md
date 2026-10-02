# HTTP/1.1 server

[`lib/net/http.f`](../lib/net/http.f) is package `HTTP`, an HTTP/1.1 server on
[`lib/net/tcp4.f`](tcp4.md), [`lib/aio.f`](aio.md), `lib/task.f` and
`lib/queue.f`. One listener task accepts on an IPv4 address; a fixed pool of
request workers takes connections from a bounded queue; each request runs one
handler inside a catch boundary, so a handler that throws is answered 500 and
its worker takes the next connection. It runs wherever TCP4 and AIO do (Linux
and macOS AArch64). The package is five more files, each required by the next:

| file | holds |
| --- | --- |
| `lib/net/http-arena.f` | the per-worker slots, the `request` and `response` handles, the limits, the error names |
| `lib/net/http-request.f` | the request parser and the request accessors |
| `lib/net/http-response.f` | the response words, `JSON!` and `ERROR!` |
| `lib/net/http-router.f` | routes, bound segments, 404 and 405 |
| `lib/net/http-static.f` | one directory tree served from memory |

Requiring `lib/net/http.f` loads all of them.

## Lifecycle

Everything a task runs is defined, and everything the server reads is
installed, before `START`: Habu forbids dictionary mutation while a task is
live ([threads.md](threads.md)), and a hook added to a running pool would be
missed by every worker already started. Installing a hook or a rule while the
server runs is `E-STATE`.

```forth
AIO:START
HTTP:ROUTES-RESET
s" GET" s" /api/ping" [: PING ;] HTTP:ROUTE
s" /srv/site" HTTP:STATIC-ROOT
$7F000001 0 2 500 HTTP:START      \ address, port (0: ephemeral), workers, idle ms
HTTP:PORT .                       \ the port the listener bound
\ ... serve ...
HTTP:STOP
AIO:STOP
```

| word | effect | meaning |
| --- | --- | --- |
| `START` | `( n n n n -- )` | bind address and port, start that many workers (1..`MAX-WORKERS`), wait for all worker start hooks, then start the listener; a connection silent for the idle milliseconds is closed |
| `STOP` | `( -- )` | ask every task to end, wait for them to the stop bound, kill any still inside its body, give everything back |
| `RUNNING?` | `( -- bool )` | during startup and until `STOP`, or false after a failed `START` |
| `PORT`, `ADDRESS` | `( -- n )` | what the listener actually bound |
| `ENDED-TASKS`, `TASK-TOTAL` | `( -- n )` | tasks that reached the end of their own body, of the listener plus the workers |
| `KILLED-TASKS` | `( -- n )` | tasks the last `STOP` had to kill past its bound; 0 is a server whose every task ended itself |

The AIO loop is the caller's to start: every wait the listener and the workers
make is an AIO submission, so `START` with no loop running is `E-AIO-STATE`,
thrown to the caller rather than inside a task nobody reads.
If a worker start hook throws, `START` throws an actual hook error after every
started worker has run its exit hooks and been joined. The listener is closed,
the server's resources are released, and `RUNNING?` is false; the same address
and port can be started again. No request is accepted during worker startup.

`STOP` waits `IDLE-MS` + 200 ms of lingering + 200 ms of slack (at least the
listener's 200 ms accept wait plus slack). A worker parked inside a handler past
that bound is killed with the rest of its body unrun; `KILLED-TASKS` counts it.
Every task counts itself out as it leaves its body and wakes the stopper, so a
stop lasts as long as its slowest task takes to notice, not its whole bound.

The pool is a fixed eight worker TCBs (`MAX-WORKERS`), each with one slot: a
1 MiB input body, a 1 MiB generated body, a header table and the scratch one
request needs, carved from one mapping per worker at `START`.

## Handlers

```forth
: ITEM ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   asked s" id" HTTP:SEGMENT-OF$ {: ida:ptr idu:n found:bool :}
   answer 200 HTTP:STATUS!
   answer [: ITEM-BODY ;] HTTP:JSON! ;

s" GET" s" /items/{id}" [: ITEM ;] HTTP:ROUTE
```

`ROUTE ( ptr u8 n ptr u8 n [ request response -- ] -- )` registers a method, a
pattern and a handler; the strings must outlive the server, which a literal
does. A pattern segment `{name}` matches one nonempty path segment and binds
its percent-decoded text; any other segment matches itself. The first route
whose pattern and method both match answers. A path some pattern matches under
another method is 405 with `Allow` listing the methods that do match; a path
no pattern matches goes to the static tree, then 404. `ROUTES-RESET` empties
the table (up to 32 routes).

A handle names a worker's slot and the generation it had when the request
began, so a handle kept past its request is `E-HANDLE`.

| request word | effect |
| --- | --- |
| `METHOD$`, `PATH$`, `QUERY$`, `BODY$` | `( request -- ptr u8 n )` |
| `HEADER-OF$` | `( request ptr u8 n -- ptr u8 n bool )`, name case-insensitive, first match |
| `HEADER-COUNT-OF` | `( request ptr u8 n -- n )`: trust a header only when exactly one arrived |
| `SEGMENT-OF$` | `( request ptr u8 n -- ptr u8 n bool )`, the decoded text `{name}` bound |
| `BODY-READER` | `( request -- JR:reader )`, a [json-read](json.md) reader over the body on the slot's own storage; the caller closes it |

| response word | effect |
| --- | --- |
| `STATUS!` | `( response n -- )`; 200 until set |
| `HEADER!` | `( response ptr u8 n ptr u8 n -- )`, one header line, copied |
| `BODY!` | `( response ptr u8 n -- )`, a BORROWED body that must outlive the send |
| `FILE!` | `( response fd n -- )`, send `n` bytes of an open descriptor the response now owns |
| `JSON!` | `( response [ ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer ] -- )`, the body a quotation writes through the worker's own writer, with `Content-Type: application/json` |
| `ERROR!` | `( response n ptr u8 n ptr u8 n -- )`, status, code and message, rendered by the installed error hook |
| `REQUEST-ID$` | `( response -- ptr u8 n )`, the id this request is reported under |

A body larger than the generated-body buffer is `E-RESPONSE`, never a truncated
body; a handler that lets it through is answered 500 `response_too_large`.
The server adds `Connection` and `Content-Length`; 204 and 304 carry no body,
and a HEAD request gets the head a GET would.

`WORKER-SLOT ( -- n )` is the running worker's slot, bounded by
`MAX-WORKERS`: ordinary storage is one copy for the process, so a handler that
keeps per-request state indexes its own rows by it. A quotation cannot read an
enclosing local, which is how values reach a `JSON!` body.

## Refusals and faults

| status | code | when |
| --- | --- | --- |
| 400 | `bad_request` | the request line, a header, the length or the chunk framing does not parse, or the version is not HTTP/1.1 or HTTP/1.0 |
| 400 | `bad_path` | a static path that climbs with `..` |
| 404 | `not_found` | no route and no static file answers the path |
| 405 | `method_not_allowed` | a route matches the path under another method; `Allow` names them |
| 413 | `body_too_large` | a body past `BODY-CAP` |
| 431 | `headers_too_large` | a header block past `HEAD-CAP`, or more than `MAX-HEADERS` headers |
| 500 | `internal` | the handler threw; the throw code goes to stderr with the request id |
| 500 | `response_too_large` | the handler's JSON body did not fit |
| 501 | `not_implemented` | a transfer coding other than `chunked` |

Every refusal closes the connection, half-closing first and reading what the
peer is still sending for up to 200 ms, so a peer mid-upload still receives its
answer. A request id is `r<slot>-<count>`: the worker and how many requests it
has answered, which no two live requests share.

`ON-ERROR ( [ response n ptr u8 n ptr u8 n -- ] -- )` installs how every error
answer is rendered, the server's own refusals and every `ERROR!` a handler
calls. The default is one line of text,

```
not_found: no route answers this path (request r0-9)
```

with `Content-Type: text/plain; charset=utf-8`. A hook sets the status and the
body through the response words and quotes `REQUEST-ID$`; the JSON renderer in
`lib/net/http-test.f` is the worked example. For a refusal or a fault the hook
runs outside the handler's boundary, under the worker's own catch: a hook that
throws there has its throw code reported on stderr with the request id, and the
client gets the plain-text default with code `error_hook` and the status it
was owed. The worker then answers as usual and takes the next connection.

## Worker hooks

`ON-WORKER-START ( [ -- ] -- )` and `ON-WORKER-EXIT ( [ -- ] -- )` register up
to eight hooks each, for a resource that belongs to the worker task itself (a
database connection per worker, say). Start hooks run in each worker, in
registration order, after it has claimed its slot and before its first
connection; exit hooks run in reverse through `TASK:AT-EXIT`, so a worker that
throws gives its resources back as surely as one that is stopped. A start hook
that throws fails `START` for the whole pool. Other workers finish their start
hooks and run their exit hooks before `START` returns the error. Hooks stay
registered for every later server in the process.

## Static files

`STATIC-ROOT ( ptr u8 n -- )` reads a whole directory tree into memory before
`START` (up to 64 files of up to 4 MiB). `STOP` releases the tree. A refused
`START` argument or context, or a listener-open failure, leaves it loaded.
Failure opening worker resources or starting tasks releases it; load it again
with `STATIC-ROOT` before retrying.
`STATIC-COUNT` is the number of files read. A GET or HEAD no
route answers is served from the tree, `/` as `/index.html`, with a content type
from the extension, an `ETag` over the bytes, and `304 Not Modified` for a
matching `If-None-Match` (a list, or `*`).

Two installed rules are the caller's policy:

| word | rule effect | default |
| --- | --- | --- |
| `CACHE-RULE!` | `[ ptr u8 n -- ptr u8 n ]`: a served path to its `Cache-Control` value | `no-cache` for every file |
| `FALLBACK-RULE!` | `[ ptr u8 n -- ptr u8 n ]`: a path that names no file to the path that answers it; empty is none | no fallback |

A single-page application caches its hashed asset directory for a year and
answers its client routes with its one page:

```forth
: SPA-CACHE ( ptr u8 n -- ptr u8 n ) {: path:ptr len:n :}
   path len s" /_app/immutable/" STARTS-WITH?
   if s" public, max-age=31536000, immutable" exit then
   s" no-cache" ;

[: SPA-CACHE ;] HTTP:CACHE-RULE!
```

## Limits

| constant | value | bound |
| --- | --- | --- |
| `MAX-WORKERS` | 8 | request workers |
| `MAX-HEADERS` | 64 | headers in one request |
| `MAX-SEGMENTS` | 8 | bound segments in one route |
| `HEAD-CAP` | 7 KiB | request line and headers (431 past it) |
| `BODY-CAP` | 1 MiB | request body (413 past it) |
| `HDR-CAP` | 2 KiB | response header lines a handler adds |
| `JSON-CAP` | 1 MiB | a generated body: `JSON!` or the default error text |

## Errors

The block is `-9340..-9349` in `lib/errors.f` (`E-HTTP-FIRST`..`E-HTTP-LAST`),
published in the package under short names:

| name | code | meaning |
| --- | --- | --- |
| `HTTP:E-STATE` | -9340 | `START` on a running server, `STOP` on a stopped one, a hook installed while one runs, a second `STATIC-ROOT` |
| `HTTP:E-HANDLE` | -9341 | a handle past its request, or one over another worker's slot |
| `HTTP:E-WORKERS` | -9342 | a worker count outside 1..`MAX-WORKERS`, or an idle deadline under 1 ms |
| `HTTP:E-CAPACITY` | -9343 | more hooks, headers or bound segments than a table holds, or a negative body length |
| `HTTP:E-ROUTE` | -9344 | more routes than the table holds, an empty method, a pattern not starting with `/` |
| `HTTP:E-STATIC` | -9345 | a static root that is not a directory, or a tree larger than its tables |
| `HTTP:E-SOCKET` | -9346 | the listener could not be bound, put to listen or asked its address |
| `HTTP:E-RESPONSE` | -9347 | a JSON body larger than the generated-body buffer |

## Test

`bin/hb --load lib/net/http-test.f` (gate row `http`) serves a fixture tree and
a route table on a loopback port. CURL fetches JSON routes, posts bodies, and
revalidates a static file with its ETag; raw TCP4 sends what CURL cannot: a bad
request line, an oversized header block and body, a gzip transfer coding,
chunked and pipelined requests, and an idle connection. It checks a throwing
handler answered 500 by a worker that serves on, the hook order, the installed
error, cache and fallback policy, a stop that kills a parked worker, and a start
hook that ends its worker. The status line and headers of every raw case are
written to `build/http-transcript.txt`, read back and compared whole; the server
that answers them runs one worker, so the request ids and every length are the
same on every run.
