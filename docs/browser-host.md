# Browser host for Wasm modules

`lib/browser/` runs a module built by `tools/wasm-build.f` in a browser: the
host moves bytes, draws triangles and shows text, and the module decides what
to ask for and what to draw. Its first caller is Maki's viewer. The page, the
module and the module's data are served by Habu's HTTP server
([http.md](http.md)).

## The module

The module imports nothing and exports `memory`, `run() -> i32`,
`throw-code() -> i64`, `out-base() -> i32` and `out-len() -> i32`
([wasm-backend.md](wasm-backend.md) §17.6). Its memory's minimum is the pages
holding its image and its maximum memory32's 65536, so the host can grow it and
place received bytes past the image, where the module's checked loads reach
them. The host and the module take turns through memory.

## Turns

Every field is an i64, little-endian. A turn writes one event, calls `run()`
and handles the records the run left in `[out-base, out-base + out-len)`, in
order.

The event is 40 bytes at the address the module names in `hello`:

| offset | field |
| --- | --- |
| +0 | kind: 0 start, 1 bytes, 2 pointer, 3 answered, 4 typed, 5 moved, 6 released |
| +8, +16, +24, +32 | a, b, c, d |

- bytes: a is where the bytes of the resource the module fetched start and b
  their length; c and d are the canvas drawing buffer's width and height in
  device pixels.
- pointer: a pointer pressed on the canvas. a and b are x and y in device
  pixels from the canvas's top-left; c and d as for bytes.
- moved and released: the pressed pointer moved, or released, with a, b, c
  and d as for pointer. The canvas captures the pointer it was pressed with, so
  they come even while it is outside the canvas, where x or y is negative or
  past the drawing buffer's size.
- answered: a is where the bytes the server answered a post with start, b
  their length and c the answer's HTTP status; d is 0.
- typed: a is where the UTF-8 bytes of the text entered in the page's field
  start and b their length; c and d as for bytes.

A record is 32 bytes, `kind a b c`:

| kind | record | meaning |
| --- | --- | --- |
| 0 | hello | a is the event's address |
| 1 | fetch | a and b are a path's address and length: fetch it relative to the page and answer with a bytes event |
| 2 | draw | a is b triangles' corners (f32 x y z, three per triangle) and c a matrix (16 f32, column-major) to clip space: x and y in [-1, 1], z in [0, 1], 0 nearest |
| 3 | text | a and b are UTF-8 text's address and length: show it |
| 4 | post | a and b are a span's address and length, and the span's first c bytes are a path: post the rest, the body, to the path relative to the page and answer with an answered event |

The rules:

- The first `run()` comes before any event address is known, so the host writes
  no event for it. The module's event block is zero, which reads as start, and
  that run answers `hello`. The host writes events only after `hello`.
- Fetch and post records are the run's requests. As the run's records are
  read, the host decodes every request's path and copies every post's body,
  since a later run may rewrite the storage they name and growth detaches it.
  A post whose c is negative or past its span is refused.
- Once the run's other records are handled, the turn makes its requests one at
  a time, in order. For each it waits for the answer, grows memory by the
  pages the answer needs, copies it past the old end and runs the module with
  a bytes or answered event naming it. The requests that run makes come before
  the turn's next one.
- Every input is taken at once: the host writes its event, runs the module and
  reads the records without yielding to anything else, so runs never overlap
  and none is dropped. While a turn waits for an answer, such as a post that
  waits minutes for a review, a pointer, a move, a release or typed text runs
  its own turn, with its own requests. So other turns may run between a
  request and the run of its answer. Answers to different turns' requests run
  in the order they arrive and name no request (answered's d is 0), so a
  module with requests from two turns outstanding tells their answers apart
  by their bodies.
- A post is answered for every HTTP status. A fetch whose status is not 2xx
  fails, as does a request whose connection fails.
- A failure ends its turn: the turn makes no more requests, and the failure is
  shown. Other turns go on.
- Records are read whole, and every address and length is checked against
  memory where it is used.
- A nonzero status from `run()` is shown as `throw <code>`, its throw code. A
  trap ends the instance and is shown too: every later run is refused, and so
  is placing an answer or typed text, so an answer that comes after the trap
  neither runs the module nor grows its memory. A failure to fetch or post, to
  grow memory or to run is shown.
- Growth detaches the old `ArrayBuffer`, so the host takes every view from
  `memory.buffer` where it uses it (WebAssembly JavaScript Interface,
  [memories](https://www.w3.org/TR/wasm-js-api-2/#memories)).

The module maps clip space to pixels as x = (ndc.x + 1) * width / 2 and
y = (1 - ndc.y) * height / 2, origin top-left, pixel centres at +0.5.

## The files

| file | holds |
| --- | --- |
| `lib/browser/turn.js` | the protocol alone: instantiate, write events, run, read records, take requests, grow memory and copy answers; `start()` runs the first turn, and `pointer(x, y)`, `moved(x, y)`, `released(x, y)` and `typed(text)` an input's; shared by both hosts |
| `lib/browser/host.js` | the browser host, on the main thread: WebGPU draws each draw record with a depth test and no culling, lit flat by the triangle's normal from the screen derivatives of its position; text sets the text element. A `pointerdown` on the canvas captures the pointer and sends a pointer, and while the canvas holds the capture each `pointermove` sends moved and the `pointerup` or `pointercancel` released: `pointerup`'s `buttons` is 0, so the capture, not the buttons, marks a drag. Each sends its position in the canvas's displayed rectangle scaled to the drawing buffer, whose size is the canvas's CSS size times `devicePixelRatio`. Enter in the field submits its form, which sends the field's text as typed and clears the field at once |
| `lib/browser/index.html` | the page: a canvas, a text field in a form, a text element and `host.js`; it loads the module from `/module.wasm`, and the field is disabled until the host listens to it |
| `lib/browser/host-cli.mjs` | the bun runner for tests, below; it proves no rendering |
| `lib/browser/host.f` | package `BROWSER-HOST`, which serves the page |

`host-cli.mjs` runs start and then each step in order, each once every run the
steps before it allowed has run:

```sh
bun lib/browser/host-cli.mjs <module.wasm> [--size W H] [step]...
```

| step | does |
| --- | --- |
| `--pointer X Y`, `--move X Y`, `--release X Y` | a pointer pressed, moved or released at X, Y |
| `--typed TEXT` | TEXT entered in the field |
| `--answer STATUS FILE` | answers the oldest waiting request with STATUS and FILE's bytes; a fetch answered outside 200-299 fails |
| `--fail` | fails the oldest waiting request, as a lost connection does |

The drawing buffer is 640 by 480 unless `--size` says otherwise. The runner
prints one line per record, a request's when the host makes it: `hello`,
`fetch <path>`, `post <path> <length> <body in hex>`, `draw <count>` and
`text <string>`; at the end, `outstanding fetch <path>` or
`outstanding post <path>` for each request no step answered. Each failure is
printed on stderr as it happens and the steps go on; the runner exits 1 if any
happened, else 0.

## Serving the page

`lib/browser/host.f` reads the page's three files as it loads, from the tree
that resolved it, into dictionary buffers. An application image built from a
program that requires it therefore carries the page: such an image cannot find
Habu's tree at run time ([forth.md](forth.md) **Files**). `BROWSER-HOST:ROUTES
( -- )` registers `GET /`, `/host.js` and `/turn.js` with their content types.
The caller registers its own routes, `/module.wasm` and every path its module
fetches or posts to, and starts the server:

```forth
: SERVE ( -- )
   AIO:START
   HTTP:ROUTES-RESET
   BROWSER-HOST:ROUTES
   s" GET" s" /module.wasm" [: MODULE-FILE ;] HTTP:ROUTE   \ application/wasm
   s" GET" s" /scene" [: SCENE-FILE ;] HTTP:ROUTE
   s" POST" s" /edit" [: EDIT ;] HTTP:ROUTE
   $7F000001 0 2 500 HTTP:START
   \ ... serve ...
   HTTP:STOP
   AIO:STOP ;
```

## Tests

`test/browser/echo-test.f` builds `test/browser/echo.f` into a module and runs
it under `host-cli.mjs`. Start answers `hello`, `fetch fixture` and the text
`loading` in the buffer the bytes run writes its own text to, so that text is
shown before the fetch is answered. The bytes answer the text of a checksum the
module computes over the copy in its memory; a pointer answers `x,y`, a move
`moved x,y` and a release `released x,y`. One run answers the fetch with a
small file; the other with a file larger than the module's whole memory, so
memory grows past what the module had.

Typed text shows itself and posts twice, both posts naming one buffer that
every typed run rewrites; an answered event shows the status, then the body,
and a 200 also fetches. The cases:

- held: while a post waits, a pointer, a move, a release and more typed text
  each answer, and the later text's post is made at once; the held post's 200
  then answers its status, its body and its fetch.
- copied: while the first post waits, more typed text rewrites the buffer the
  waiting second post names and grows memory; that post still reaches the
  server with its own path and body.
- chained: a post's 200 fetches, and that fetch is made and answered before
  the turn's second post, whose 422 is shown.
- trapped: typed text that traps while start's fetch waits ends the instance:
  the fetch's answer, a pointer and more typed text are refused, and nothing
  more comes from the module.
- failed: a 404 fetch, a failed post and a post whose path is longer than its
  span each end their turn, and a pointer still answers.

It needs bun and wasm-tools, so it runs in the wasm device check,
`bin/hb --load test/wasm/device.f` ([bootstrap.md](bootstrap.md)), not the
ordinary gate.

`lib/browser/host-test.f` (gate row `browser-host`) starts Habu's HTTP server
with `BROWSER-HOST:ROUTES` on a loopback port, and CURL checks that `/`,
`/host.js` and `/turn.js` answer 200 with their content types and the bytes of
their files.

WebGPU drawing, the pointer's capture and position during a drag, and the
field are checked by hand through a caller's page in a browser.
