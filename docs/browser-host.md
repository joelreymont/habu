# Browser host for Wasm modules

`src/lib/browser/` runs a module built by `tools/wasm-build.f` in a browser: the
host moves bytes, draws triangles, and shows a nested tree and text. The module
decides what to ask for and what to draw. Its first caller is Maki's viewer.
The page, the module and the module's data are served by Habu's HTTP server
([http.md](http.md)).

## The module

The module imports nothing and exports `memory`, `run() -> i32`,
`throw-code() -> i64`, `out-base() -> i32` and `out-len() -> i32`
([wasm-backend.md](wasm-backend.md) §17.6). Its memory's minimum is the pages
holding its image and its maximum memory32's 65536, so the host can grow it and
place received bytes past the image, where the module's checked loads reach
them. The module grows it too, for its dynamic buffers, whose pages are never
the host's ([wasm-backend.md](wasm-backend.md) §17.5). The host and the module
take turns through memory.

## Turns

Every field is an i64, little-endian. A turn writes one event, calls `run()`
and handles the records the run left in `[out-base, out-base + out-len)`, in
order.

The event is 56 bytes at the address the module names in `hello`:

| offset | field |
| --- | --- |
| +0 | kind: 0 start, 1 bytes, 2 pointer, 3 answered, 4 typed, 5 moved, 6 released, 7 wheel, 8 key, 9 resize, 10 control |
| +8, +16, +24, +32 | a, b, c, d |
| +40 | buttons: the pointer's `buttons` bitset; for a tagged answered event, the drawing-buffer width; 0 otherwise and for pointer release |
| +48 | modifier bitset: Shift 1, Control 2, Alt 4, Meta 8; for a tagged answered event, the drawing-buffer height; 0 when irrelevant |

- bytes: a is where the bytes of the resource the module fetched start and b
  their length; c and d are the canvas drawing buffer's width and height in
  device pixels.
- pointer: a pointer pressed on the canvas. a and b are x and y in device
  pixels from the canvas's top-left; c and d as for bytes. Buttons and
  modifiers are sampled for each pointer event.
- moved and released: the pressed pointer moved, or released, with a, b, c
  and d as for pointer. The canvas captures the pointer it was pressed with, so
  they come even while it is outside the canvas, where x or y is negative or
  past the drawing buffer's size.
- answered: a is where the response bytes start, b their length and c the HTTP
  status, or 0 for a kind 7 request's transport failure. For a kind 7 request,
  d is its exact positive signed 64-bit request ID and the last two fields are
  the drawing-buffer width and height at answer time. For a legacy post, d and
  the last two fields are 0.
- typed: a is where the UTF-8 bytes of the text entered in the page's field
  start and b their length; c and d as for bytes.
- wheel: a and b are signed horizontal and vertical deltas in integer device
  pixels. The browser host converts DOM pixel, line and page wheel units using
  the canvas's displayed size and drawing-buffer scale. c and d are its size.
- key: a and b name the UTF-8 bytes of `KeyboardEvent.key`, delivered only
  while the canvas has keyboard focus. c and d are the drawing-buffer size.
- resize: a and b are 0, c and d are the current drawing-buffer size.
- control: a is a positive transient key of a tree row the user selected, b is
  0, and c and d are the drawing-buffer size. The module owns the persistent
  identity behind a row; host keys are only controls in the current tree.

The host overwrites all 56 bytes on every event. Fields that do not apply to
an event are zero. Modules must reserve the full event block; a 40-byte event
block is no longer supported.

A record is 32 bytes, `kind a b c`:

| kind | record | meaning |
| --- | --- | --- |
| 0 | hello | a is the event's address |
| 1 | fetch | a and b are a path's address and length: fetch it relative to the page and answer with a bytes event |
| 2 | draw | a is b triangles' corners (f32 x y z, three per triangle) and c a matrix (16 f32, column-major) to clip space: x and y in [-1, 1], z in [0, 1], 0 nearest |
| 3 | text | a and b are UTF-8 text's address and length: show it |
| 4 | post | a and b are a span's address and length, and the span's first c bytes are a path: post the rest, the body, to the path relative to the page and answer with an answered event |
| 5 | tree | a and b name strict UTF-8 JSON rows, and c is the selected row key or 0 |
| 6 | highlight | a names one u8 mask value per triangle, b is the count, and c is 0; 0 means unselected and 1 selected |
| 7 | request | a and b name a span beginning with little-endian u64 method and path byte lengths, followed by the method's ASCII bytes, the path's strict UTF-8 bytes, and the arbitrary binary body; c is a positive signed 64-bit request ID |

The request span has at least 16 bytes. Its method and path are nonempty, the
method is an HTTP token, and both lengths must fit the span without wrapping;
the remaining bytes are the body. The host copies and decodes all three before
it starts asynchronous work. GET, POST and PUT serve current callers, and the
host uses `fetch` for other valid method tokens. A GET or HEAD body is refused
by the turn protocol before dispatch. Body bytes are sent only when present,
with the same binary content-type behavior as legacy posts. Every HTTP status,
including a GET 404, reaches an answered event. A tagged request's transport
rejection reaches an answered event with status 0 and UTF-8 bytes of
`String(error)` as its body. Status 0 is reserved for transport failure. The
request ID stays a BigInt in the host, so IDs above JavaScript's safe integer
range arrive unchanged.

Tree JSON is an array of `{ "key": number, "parent": number, "label": string }`
rows. A key is a unique positive JavaScript safe integer. A parent is 0 for a
root or an earlier row's key. The selected key must occur in the array unless
it is 0. The protocol boundary rejects malformed JSON, invalid UTF-8, keys,
parent order and selection before the browser host sees the tree. The host
uses text nodes and native buttons for expansion and selection; it retains
expansion and focus when the module sends the same tree with a new selection.
These row keys are not persistent design identity.

A highlight mask is checked and copied synchronously before the next record
can run. It remains in effect for subsequent draws until another highlight
record replaces it. Each draw must have the mask's triangle count. Before the
first highlight, draws use an unselected mask. A highlight does not repaint:
the module issues a draw when it wants the new colors visible. Zero triangles
are valid.

The rules:

- The first `run()` comes before any event address is known, so the host writes
  no event for it. The module's event block is zero, which reads as start, and
  that run answers `hello`. The host writes events only after `hello`.
- Fetch, post and request records are the run's requests. As the run's records are
  read, the host decodes every request's path and copies every body's bytes,
  since a later run may rewrite the storage they name and growth detaches it.
  A post whose c is negative or past its span is refused. A request with an
  invalid ID, header, length, method token, UTF-8 path or GET/HEAD body is
  refused.
- Once the run's other records are handled, all its tagged requests start
  independently, even when legacy records lie between them. Their answers run
  in arrival order, each with its own ID and the drawing-buffer size sampled
  when its answer arrives. Legacy fetch and post records wait for one another
  in order; a reply's own requests complete before the next legacy record.
  For each answer, the host grows memory by the pages it needs, copies it past
  the old end and runs the module with an event naming it.
- Every input is taken at once: the host writes its event, runs the module and
  reads the records without yielding to anything else, so runs never overlap
  and none is dropped. While a turn waits for an answer, such as a post that
  waits minutes for a review, pointer, wheel, key, resize, control or typed text
  runs its own turn, with its own requests. So other turns may run between a
  request and the run of its answer. Answers to different turns' requests run
  in the order they arrive. Tagged requests carry their ID in answered's d;
  legacy post answers keep d at 0 and must be distinguished by their bodies.
- A post or request is answered for every HTTP status. A fetch whose status is
  not 2xx fails. A tagged request's transport failure is answered with status
  0, so its module can clear its pending operation and continue.
- Tagged requests from one run settle independently, as does that run's legacy
  sequence. A failed tagged reply does not cancel sibling requests or the
  separate legacy sequence. A host using request records must provide a
  synchronous `error(error)` callback; the browser displays and logs it, and
  the CLI reports it. The callback receives the original first reply error as
  soon as it occurs, even while siblings remain pending. Each further reply
  error from that turn calls it again with an `AggregateError` holding the
  original errors in arrival order and a message containing every error's
  string. Transport rejection alone is an answered status 0, not a callback
  error. A missing callback is refused before dispatching that run's requests.
- Within the legacy sequence, a failed fetch or post stops later legacy records
  and rejects the turn's Promise. Input-run and record-validation failures also
  reject that Promise. Asynchronous tagged reply failures use `error(error)`;
  their visibility does not wait for other requests or depend on a caller
  awaiting the turn's Promise. Other input turns can still run unless the
  instance trapped; a trap prevents later answers from being placed or run.
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
| `src/lib/browser/turn.js` | the protocol alone: instantiate, write 56-byte events, validate and read records, take requests, grow memory and copy answers; shared by both hosts |
| `src/lib/browser/host.js` | the browser host, on the main thread: WebGPU draws lit triangles with a depth test and a selected-triangle color, and resizes its depth texture with the canvas. It presents the validated tree with keyboard-accessible native buttons, preserves expansion on identical tree updates, and sends selected row keys as control events. Canvas pointer capture delivers drag and release, including cancellation; wheel deltas are normalized to device pixels, canvas keydown sends `KeyboardEvent.key`, and `ResizeObserver` sends size changes. The canvas suppresses its context menu and wheel scrolling. Enter in the separate field submits typed text |
| `src/lib/browser/index.html` | the page: a scene tree to the left of a focusable canvas, text field in a form, text element and `host.js`; it loads `/module.wasm`, and the field is disabled until the host listens to it |
| `src/lib/browser/host-cli.mjs` | the bun runner for tests, below; it proves no rendering |
| `src/lib/browser/host.f` | package `BROWSER-HOST`, which serves the page |

`host-cli.mjs` runs start and then each step in order, each once every run the
steps before it allowed has run:

```sh
bun src/lib/browser/host-cli.mjs <module.wasm> [--size W H] [step]...
```

| step | does |
| --- | --- |
| `--pointer X Y BUTTONS MODS`, `--move X Y BUTTONS MODS`, `--release X Y BUTTONS MODS` | a pointer pressed, moved or released at X, Y |
| `--wheel DX DY MODS` | a signed wheel delta already in device pixels |
| `--key TEXT MODS` | a canvas keydown's `KeyboardEvent.key` text |
| `--resize W H` | changes the runner's drawing-buffer size and sends resize |
| `--control KEY` | selects a transient tree row key |
| `--typed TEXT` | TEXT entered in the field |
| `--answer STATUS FILE` | answers the oldest waiting request with STATUS and FILE's bytes; a legacy fetch answered outside 200-299 fails |
| `--answer-id ID STATUS FILE` | answers the waiting tagged request with ID; replies may overtake earlier ones |
| `--fail` | fails the oldest waiting request, as a lost connection does; a tagged request delivers status 0 |

The drawing buffer is 640 by 480 unless `--size` says otherwise. The runner
prints one line per record, a request's when the host makes it: `hello`,
`fetch <path>`, `post <path> <length> <body in hex>`,
`request <method> <path> <ID> <length> <body in hex>`,
`tree <selected> <JSON>`, `highlight <count> <mask in hex>`,
`draw <count> <mask in hex>` and `text <string>`; the mask is omitted for a
zero-triangle draw or highlight. At the end, `outstanding fetch <path>`,
`outstanding post <path>` or `outstanding request <method> <path> <ID>` appears
for each request no step answered. Each failure is
printed on stderr as it happens and the steps go on; the runner exits 1 if any
happened, else 0.

## Serving the page

`src/lib/browser/host.f` reads the page's three files as it loads, from the tree
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

`test/browser/storage-test.f` builds `test/browser/storage.f`, whose dynamic
buffer grows, is released and is reserved again around the pages the host adds
for a fetch larger than the module's image. The module throws unless the buffer
lies past the host's bytes each time it is reserved after the fetch, kept its
bytes as it grew and gets its first pages back cleared; its text is the
checksum of the host's bytes, so a host page the module wrote changes it. It
runs in the wasm device check too.

`test/browser/navigation-test.f` builds `test/browser/navigation.f` and runs it
through the same Wasm host runner. It checks pointer buttons and modifiers,
signed wheel deltas, UTF-8 key text, resize and control events; a nested tree
with changed selection; default, selected, repeated and zero-triangle draws;
and rejection of malformed tree and highlight records. The fixture also draws
two triangles for browser inspection, one highlighted and one unselected.

`test/browser/request-test.f` builds and validates the checked Wasm fixture
`test/browser/request.f` and runs `test/browser/request-peer.mjs`. The peer
drives two live turns whose tagged replies
arrive in reverse order, checks exact IDs above 2^53, binary bodies and copied
request data after memory growth, then exercises rejected records and the CLI's
FIFO replies. It verifies immediate tagged reply reporting while a sibling
waits, original error identity, aggregate error text, and the CLI's failure
exit. A failed legacy sequence stops only its own later records. It also
checks the old zero fields for fetch and post replies.

`src/lib/browser/host-test.f` (gate row `browser-host`) starts Habu's HTTP server
with `BROWSER-HOST:ROUTES` on a loopback port, and CURL checks that `/`,
`/host.js` and `/turn.js` answer 200 with their content types and the bytes of
their files.

WebGPU drawing, canvas input, tree controls and form typing also need a browser
check: the CLI executes the protocol but has no DOM or GPU. With the navigation
fixture served as `/module.wasm`, Chrome shows nested Assembly → Part → Face
rows and distinct orange and gray triangles. Selecting a row sends control;
collapsing and expanding Part uses native buttons; entering `tree` changes the
selected row without losing expansion. A right-button Shift press/release
reports buttons 2 and modifier 1, canvas Shift+ArrowLeft reports key
`ArrowLeft` with modifier 1, a line-mode wheel event with deltas (2, -3)
reports device-pixel deltas (32, -48), and changing the window size sends a
resize event. Entering `redraw` after resize renders both triangles again.
Entering `zero` clears the canvas with no WebGPU error.
