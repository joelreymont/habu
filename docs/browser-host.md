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
| +0 | kind: 0 start, 1 bytes, 2 pointer |
| +8, +16, +24, +32 | a, b, c, d |

- bytes: a is where the bytes of the resource the module fetched start and b
  their length; c and d are the canvas drawing buffer's width and height in
  device pixels.
- pointer: a and b are x and y in device pixels from the canvas's top-left;
  c and d as for bytes.

A record is 32 bytes, `kind a b c`:

| kind | record | meaning |
| --- | --- | --- |
| 0 | hello | a is the event's address |
| 1 | fetch | a and b are a path's address and length: fetch it relative to the page and answer with a bytes event |
| 2 | draw | a is b triangles' corners (f32 x y z, three per triangle) and c a matrix (16 f32, column-major) to clip space: x and y in [-1, 1], z in [0, 1], 0 nearest |
| 3 | text | a and b are UTF-8 text's address and length: show it |

The rules:

- The first `run()` comes before any event address is known, so the host writes
  no event for it. The module's event block is zero, which reads as start, and
  that run answers `hello`. The host writes events only after `hello`.
- A fetch goes on with the same turn once the run's other records are
  handled, since the next run rewrites the storage they name: the host fetches
  the path, grows memory by the pages the bytes need, copies them past the old
  end and runs the module again with a bytes event naming them.
- One turn is active at a time, its fetches and their bytes runs included. A
  pointer that arrives during a turn, such as a click during the first fetch,
  is dropped.
- Records are read whole, and every address and length is checked against
  memory where it is used.
- A nonzero status from `run()` is shown as `throw <code>`, its throw code. A
  trap ends the instance and is shown too. A failure to fetch, to grow memory
  or to run is shown.
- Growth detaches the old `ArrayBuffer`, so the host takes every view from
  `memory.buffer` where it uses it (WebAssembly JavaScript Interface,
  [memories](https://www.w3.org/TR/wasm-js-api-2/#memories)).

The module maps clip space to pixels as x = (ndc.x + 1) * width / 2 and
y = (1 - ndc.y) * height / 2, origin top-left, pixel centres at +0.5.

## The files

| file | holds |
| --- | --- |
| `lib/browser/turn.js` | the protocol alone: instantiate, write events, run, read records, grow memory and copy bytes; `start()` runs the first turn and `pointer(x, y)` a pointer's; shared by both hosts |
| `lib/browser/host.js` | the browser host, on the main thread: WebGPU draws each draw record with a depth test and no culling, lit flat by the triangle's normal from the screen derivatives of its position; text sets the text element; a `pointerdown` on the canvas sends a pointer, its position in the canvas's displayed rectangle scaled to the drawing buffer, whose size is the canvas's CSS size times `devicePixelRatio` |
| `lib/browser/index.html` | the page: a canvas, a text element and `host.js`; it loads the module from `/module.wasm` |
| `lib/browser/host-cli.mjs` | the bun runner for tests: `bun lib/browser/host-cli.mjs <module.wasm> <bytes-file> [--size W H] [--pointer X Y]...` runs start, answers every fetch with the file, sends each pointer and prints one line per record (`hello`, `fetch <path>`, `draw <count>`, `text <string>`); it exits 0, or 1 with the failure on stderr. It proves no rendering |
| `lib/browser/host.f` | package `BROWSER-HOST`, which serves the page |

## Serving the page

`lib/browser/host.f` reads the page's three files as it loads, from the tree
that resolved it, into dictionary buffers. An application image built from a
program that requires it therefore carries the page: such an image cannot find
Habu's tree at run time ([forth.md](forth.md) **Files**). `BROWSER-HOST:ROUTES
( -- )` registers `GET /`, `/host.js` and `/turn.js` with their content types.
The caller registers its own routes, `/module.wasm` and every path its module
fetches, and starts the server:

```forth
: SERVE ( -- )
   AIO:START
   HTTP:ROUTES-RESET
   BROWSER-HOST:ROUTES
   s" GET" s" /module.wasm" [: MODULE-FILE ;] HTTP:ROUTE   \ application/wasm
   s" GET" s" /scene" [: SCENE-FILE ;] HTTP:ROUTE
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
module computes over the copy in its memory, and each pointer answers `x,y`.
One run answers the fetch with a small file; the other with a file larger than
the module's whole memory, so memory grows past what the module had. It needs
bun and wasm-tools, so it runs in the wasm device check,
`bin/hb --load test/wasm/device.f` ([bootstrap.md](bootstrap.md)), not the
ordinary gate.

`lib/browser/host-test.f` (gate row `browser-host`) starts Habu's HTTP server
with `BROWSER-HOST:ROUTES` on a loopback port, and CURL checks that `/`,
`/host.js` and `/turn.js` answer 200 with their content types and the bytes of
their files.

WebGPU drawing and clicks are checked by hand through a caller's page in a
browser.
