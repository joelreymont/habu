// turn.js - the turn protocol between a host and a Habu Wasm module. The
// browser host (host.js) and the bun runner (host-cli.mjs) share it; it knows
// no DOM, GPU, network or file system.
//
// The module (src/arch/wasm/link.f) imports nothing and exports memory, run,
// throw-code, out-base, out-len and reserve. The module owns every byte of its
// memory, which may grow to memory32's 65536 pages. For each delivery the host
// calls reserve(length), which answers the address of fresh pages, at least
// one, or 0 when it refuses; the host copies the bytes there and runs the event
// that names them. The module frees them (src/lib/browser/event.f) once it no
// longer needs them. An empty delivery reserves nothing and is named 0, 0.
//
// An event is 56 bytes at the address hello names: kind, a, b, c, d, buttons,
// mods. Kinds: 0 start, 1 bytes, 2 pointer, 3 answered, 4 typed, 5 moved,
// 6 released, 7 wheel, 8 key, 9 resize, 10 control, 11 load, 12 focus,
// 13 highlight, 14 notified, 15 tick, 16 file, 17 chunk. c and d are the
// drawing buffer's width and height in device pixels except for answered, file
// and chunk.
//   bytes, typed, key: a, b name the fetched bytes, the field's text or
//     KeyboardEvent.key (sent only while the canvas has focus)
//   pointer, moved, released: a, b are x, y from the canvas's top-left; the
//     canvas captures the pointer, so either may be negative or past the buffer
//   answered: a, b the response, c the status, d the request id, buttons and
//     mods the buffer size when the answer arrived; a tagged request's
//     transport failure is status 0 with String(error) as its body; a legacy
//     post's d, buttons and mods are 0. For an rpc, a, b are the JSON of the
//     response's result, c 1, or of its error, c 2; a transport failure is c 0
//     with String(error)
//   wheel: a, b signed device-pixel deltas; resize: a, b are 0
//   control: a is a tree row's key, transient across sessions and never a
//     persistent identity; b is 1 when the person opens a row whose more is
//     true, which the module answers by sending the tree again with that row's
//     children, else 0, a selection
//   load: a, b name the scene's bytes
//   focus, highlight: a, b name packed 16-byte identities, b a multiple of 16;
//     b 0 clears. What a scene or an identity means is the module's.
//   notified: a, b a u64 method length, the method, then the params JSON, all
//     UTF-8; one per host notification
//   tick: a the token of the after record that asked for it
//   file: a, b the file's UTF-8 name, c its size in bytes, d the host's
//     positive file number, valid while the module lives; one per file picked
//     or dropped
//   chunk: a, b the bytes read, c the file number, d the offset; buttons 1, or
//     0 when the read failed, with a, b String(error)
// buttons is the pointer's bitset; mods are Shift 1, Control 2, Alt 4, Meta 8.
// The host writes all 56 bytes on every event; a field that does not apply is 0.
//
// A record is 32 bytes, kind a b c:
//   0 hello: a is the event's address
//   1 fetch: a, b a path fetched relative to the page; outside 2xx it fails
//   2 draw: a, b instance rows, b rows of 64 bytes: twelve f32, a column-major
//     3x4 transform from the mesh's u16 grid to world, then u32s: a held mesh
//     handle, the instance's id, flags (bit 0 every face selected, the others
//     0) and the highlight word where the instance's face bits start, or
//     0xFFFFFFFF for none; a row's offset plus the words its mesh's faces need
//     (record 13) is at most the highlight's word count, and that sum is the
//     words the row needs. c a column-major f32 4x4 matrix to clip space (x, y
//     in [-1, 1], z in [0, 1], 0 nearest). A draw of 0 rows clears, names no
//     handle and needs no words; a draw removes a shown SVG. A host may draw
//     the last draw's rows again on its own, so records 6 and 14 keep them
//     valid until the next draw
//   3 text: a, b UTF-8 text
//   4 post: span a, b whose first c bytes are the path and the rest the body
//   5 tree: a, b strict UTF-8 JSON rows {key, parent, label} with an optional
//     boolean more, true when the row has children and none of them are among
//     the rows (absent is false); c the selected key or 0; keys are unique
//     positive safe integers, a parent is 0 or an earlier row's key whose more
//     is not true; a row the module sends again keeps its key from one tree to
//     the next
//   6 highlight: a the address of b u32 words of face bits, c 0: bit i of word
//     w is face 32w + i from an instance's offset. It holds for every later
//     draw; before the first there are 0 words. b is at least the most words
//     a row of the last draw needs, so a module sends a longer highlight
//     before the draw that needs it and a shorter one after
//   7 request: span a, b of u64 method and path lengths, the method token, the
//     UTF-8 path and the body (none for GET or HEAD); c a positive i64 id
//   8 svg: a, b the image bytes, copied before host.svg runs
//   9 input: a is 0 or 1, disabling or enabling the typed-text field
//   10 rpc: span a, b of a u64 method length, the nonempty UTF-8 method, then
//     the params, UTF-8 JSON text of an object; c a positive i64 id
//   11 after: a ms, at least 0, b a token; one tick after ms
//   12 read: a a file number a file event delivered, b an offset, c a length of
//     at least 1, with b + c at most the file's size; each gets its own chunk
//   13 mesh: a, b a span of a u32 vertex count V, 1 to 65,536, a u32 triangle
//     count T, V vertices of four u16 (x, y, z on the mesh's grid and the
//     vertex's face) and T triangles of three u16 indices below V, so b is
//     8 + 8V + 6T; c a handle not held, 1 to 0xFFFFFFFF, as a draw row's u32
//     names it. Its faces need floor(F / 32) + 1 highlight words, F the
//     highest face of its vertices. The host copies the mesh and holds it by
//     c until an unmesh
//   14 unmesh: a a held handle that no row of the last draw names, b and c 0,
//     so a module draws without a mesh before it unmeshes it; the handle may
//     be used again after
// A post or request is answered for every HTTP status. Request, rpc and read
// records are tagged. A host that takes tagged records, or fetches and posts
// from a run that throws, provides error(error): the first reply failure of a
// turn reaches it at once, each further one as an AggregateError of all of them.
// The module maps clip space to pixels as x = (ndc.x + 1) * width / 2 and
// y = (1 - ndc.y) * height / 2, origin top-left.
//
// The module imports nothing. A turn writes one event at the address the
// module named in hello, calls run() and handles the records the run left in
// [out-base, out-base + out-len), in order. The first run has no event: the
// module's event block is zero, which reads as start. Fetch, post, request, rpc
// and read records are requests: as the records are read, each is decoded and
// each body copied, since later runs rewrite the storage they name and growth
// detaches it. Once the run's other records are handled, tagged requests from
// that run start independently; legacy fetches and posts still wait in order, including
// the requests made by their replies. An answer is copied into pages reserved
// through the module, and the module runs again with an event naming them.
// Reserve, copy and run are one synchronous stretch: the module imports nothing,
// so no other turn runs between them, and host.size() is the only call there.
// Runs are synchronous and never overlap, but other turns
// run while a request waits. A run that
// traps ends the instance, as end() does: nothing runs, and memory does not
// grow, after it.
//
// Every field is an i64, little-endian, and a BigInt here. Growth detaches the
// old ArrayBuffer, so every view is taken from memory.buffer where it is used:
// https://www.w3.org/TR/wasm-js-api-2/#memories

const EVENT_BYTES = 56n;
const RECORD_BYTES = 32;
const MATRIX_BYTES = 64n; // 16 f32, column-major
const ROW_BYTES = 64; // an instance: twelve f32, u32 handle, id, flags, offset
const NO_OFFSET = 0xFFFFFFFF; // an instance row that reads no highlight words
const MAX_HANDLE = 0xFFFFFFFFn; // a draw row's u32 names a mesh handle
const MAX_VERTICES = 65536; // a mesh's u16 indices name at most this many
const MAX_KEY = BigInt(Number.MAX_SAFE_INTEGER);
// A tree row's keys, sorted: more is optional.
const TREE_KEYS = new Set(["key,label,parent", "key,label,more,parent"]);

// Events; start, kind 0, is the zero event block the first run reads.
const BYTES = 1n;
const POINTER = 2n;
const ANSWERED = 3n;
const TYPED = 4n;
const MOVED = 5n;
const RELEASED = 6n;
const WHEEL = 7n;
const KEY = 8n;
const RESIZE = 9n;
const CONTROL = 10n;
const LOAD = 11n;
const FOCUS = 12n;
const HIGHLIGHT_EVENT = 13n; // HIGHLIGHT names the record
const NOTIFIED = 14n;
const TICK = 15n;
const FILE = 16n;
const CHUNK = 17n;
const ID_BYTES = 16;

// Records.
const HELLO = 0n;
const FETCH = 1n;
const DRAW = 2n;
const TEXT = 3n;
const POST = 4n;
const TREE = 5n;
const HIGHLIGHT = 6n;
const REQUEST = 7n;
const SVG = 8n;
const INPUT = 9n;
const RPC = 10n;
const AFTER = 11n;
const READ = 12n;
const MESH = 13n;
const UNMESH = 14n;
const TAGGED = new Set([REQUEST, RPC, READ]);

const utf8 = new TextDecoder("utf-8", { fatal: true });
// A request's leading BOM bytes are content, including at the start of a path.
const requestUtf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const encoder = new TextEncoder();

// An rpc response's JSON text as its status, 1 for a result and 2 for an
// error, and that member's value; any other text throws.
function outcome(text) {
  const response = JSON.parse(text);
  const keys = response !== null && typeof response === "object" && !Array.isArray(response) ?
    Object.keys(response) : [];
  if (keys.length === 1 && keys[0] === "result") return [1n, response.result];
  if (keys.length === 1 && keys[0] === "error") return [2n, response.error];
  throw new TypeError("an rpc response must be {\"result\":…} or {\"error\":…}");
}

export class Module {
  // host: hello(), fetch(path) -> Promise<Uint8Array>, post(path, body) ->
  // Promise<{ status, bytes }>, request(method, path, body, id) -> the same;
  // id is an optional diagnostic argument for the CLI.
  // rpc(method, params, id) -> Promise of the response's JSON text,
  // {"result":…} or {"error":…}; params is the record's JSON text.
  // after(ms, token) runs tick(token) once ms have passed. read(file, offset,
  // length) -> Promise<Uint8Array> of the numbered file's bytes. Also
  // mesh(handle, vertexCount, triangleCount, vertices, triangles) with copied
  // Uint16Arrays, unmesh(handle), draw(rows, count, matrix) with the rows'
  // bytes copied, highlight(words) with a copied Uint32Array, text(string),
  // tree(rows, selected), svg(copiedBytes), input(enabled), and size() ->
  // [width, height] of the drawing buffer.
  static async load(bytes, host) {
    const { instance } = await WebAssembly.instantiate(bytes);
    return new Module(instance.exports, host);
  }

  constructor(exports, host) {
    this.x = exports;
    this.host = host;
    this.event = null; // the address hello named
    this.ended = null; // why the instance ended: a trap or end()
    this.meshes = new Map(); // a held mesh handle -> the words its faces need
    this.words = 0; // the last highlight's word count, 0 before the first
    this.drawn = new Set(); // the handles the last draw's rows name
    this.need = 0; // the most words a row of the last draw needs
    this.files = new Map(); // a delivered file number -> its size, for reads
  }

  // The first turn.
  start() {
    return this.turn(null);
  }

  // Turns for a pointer pressed, moved and released at x, y in the drawing
  // buffer's pixels.
  pointer(x, y, buttons, mods) {
    return this.turn([POINTER, x, y, ...this.host.size(), buttons, mods]);
  }

  moved(x, y, buttons, mods) {
    return this.turn([MOVED, x, y, ...this.host.size(), buttons, mods]);
  }

  released(x, y, buttons, mods) {
    return this.turn([RELEASED, x, y, ...this.host.size(), buttons, mods]);
  }

  wheel(dx, dy, mods) {
    return this.turn([WHEEL, dx, dy, ...this.host.size(), 0, mods]);
  }

  async key(text, mods) {
    const bytes = encoder.encode(text);
    await this.turn([KEY, this.place(bytes), bytes.length, ...this.host.size(), 0, mods]);
  }

  resize() {
    return this.turn([RESIZE, 0, 0, ...this.host.size(), 0, 0]);
  }

  // A tree row selected, or opened when opened is true.
  control(key, opened = false) {
    const id = BigInt(key);
    if (id <= 0n || id > MAX_KEY) throw new RangeError(`control key ${id} is not a positive safe integer`);
    if (typeof opened !== "boolean")
      throw new TypeError("control opened must be a boolean");
    return this.turn([CONTROL, id, opened ? 1 : 0, ...this.host.size(), 0, 0]);
  }

  // A turn for a scene's bytes.
  async load(scene) {
    await this.turn([LOAD, this.place(scene), scene.length, ...this.host.size(), 0, 0]);
  }

  // Turns for packed 16-byte identities to focus or highlight; none clears.
  focus(ids) {
    return this.identities(FOCUS, ids);
  }

  highlight(ids) {
    return this.identities(HIGHLIGHT_EVENT, ids);
  }

  async identities(kind, ids) {
    if (ids.length % ID_BYTES !== 0)
      throw new RangeError(`${ids.length} bytes are not whole ${ID_BYTES}-byte identities`);
    await this.turn([kind, this.place(ids), ids.length, ...this.host.size(), 0, 0]);
  }

  // A turn for typed text, its UTF-8 bytes.
  async typed(text) {
    const bytes = encoder.encode(text);
    await this.turn([TYPED, this.place(bytes), bytes.length, ...this.host.size(), 0, 0]);
  }

  // A turn for a host notification: its method and its params JSON text.
  async notified(method, params) {
    const name = encoder.encode(method);
    const text = encoder.encode(params);
    const bytes = new Uint8Array(8 + name.length + text.length);
    new DataView(bytes.buffer).setBigUint64(0, BigInt(name.length), true);
    bytes.set(name, 8);
    bytes.set(text, 8 + name.length);
    await this.turn([NOTIFIED, this.place(bytes), bytes.length, ...this.host.size(), 0, 0]);
  }

  // The turn an after record's ms asked for.
  tick(token) {
    return this.turn([TICK, token, 0, ...this.host.size(), 0, 0]);
  }

  // A turn for a file the host numbered; read records may then name it.
  async file(name, size, number) {
    const n = BigInt(number);
    if (n <= 0n) throw new RangeError(`a file number must be positive, got ${n}`);
    const bytes = encoder.encode(name);
    this.files.set(n, BigInt(size));
    await this.turn([FILE, this.place(bytes), bytes.length, size, n, 0, 0]);
  }

  // The event's run, then its requests. All tagged requests start independently;
  // legacy requests keep their order and wait for their own recursive replies.
  // A run's throw is passed on at once: its requests go on, and a legacy
  // request that fails later reports to host.error.
  async turn(event) {
    const [requests, thrown] = this.run(event);
    if (requests.some((request) => TAGGED.has(request.kind)) && typeof this.host.error !== "function")
      throw new TypeError("host.error(error) is required for tagged replies");
    const errors = [];
    const report = (error) => {
      errors.push(error);
      this.host.error(errors.length === 1 ? error :
        new AggregateError([...errors], errors.map(String).join("; ")));
    };
    const tagged = requests.filter((request) => TAGGED.has(request.kind))
      .map((request) => this.answer(request).catch(report));
    const legacy = (async () => {
      for (const request of requests)
        if (!TAGGED.has(request.kind)) await this.answer(request);
    })();
    if (thrown) {
      legacy.catch((error) => {
        if (typeof this.host.error !== "function") throw error;
        report(error);
      });
      throw thrown;
    }
    await Promise.all([...tagged, legacy]);
  }

  async answer({ kind, path, body, method, params, id, file, offset, length }) {
    if (kind === FETCH) {
      const bytes = await this.host.fetch(path);
      await this.turn([BYTES, this.place(bytes), bytes.length, ...this.host.size(), 0, 0]);
    } else if (kind === REQUEST) {
      let response;
      try {
        response = await this.host.request(method, path, body, id);
      } catch (error) {
        response = { status: 0, bytes: encoder.encode(String(error)) };
      }
      const { status, bytes } = response;
      await this.turn([ANSWERED, this.place(bytes), bytes.length, status, id, ...this.host.size()]);
    } else if (kind === RPC) {
      let status, value;
      try {
        [status, value] = outcome(await this.host.rpc(method, params, id));
      } catch (error) {
        [status, value] = [0n, String(error)];
      }
      const bytes = encoder.encode(status === 0n ? value : JSON.stringify(value));
      await this.turn([ANSWERED, this.place(bytes), bytes.length, status, id, ...this.host.size()]);
    } else if (kind === READ) {
      let read, bytes;
      try {
        bytes = await this.host.read(file, offset, length);
        if (BigInt(bytes.length) !== length)
          throw new RangeError(`file ${file} gave ${bytes.length} bytes at ${offset}, not ${length}`);
        read = 1n;
      } catch (error) {
        [read, bytes] = [0n, encoder.encode(String(error))];
      }
      await this.turn([CHUNK, this.place(bytes), bytes.length, file, offset, read, 0]);
    } else {
      const { status, bytes } = await this.host.post(path, body);
      await this.turn([ANSWERED, this.place(bytes), bytes.length, status, 0, 0, 0]);
    }
  }

  // Ends the instance for its host: a reply that arrives later is refused.
  end() {
    this.ended ??= "the host ended the module";
  }

  // Refuses the instance a trap or end() ended.
  live() {
    if (this.ended) throw new Error(this.ended);
  }

  // Writes the event and runs the module; handles the records but the
  // requests, and answers [requests, the run's throw or null]. A run that throws
  // has still made its records: the module waits on every request it wrote.
  run(event) {
    this.live();
    if (event) this.write(event);
    let status;
    try {
      status = this.x.run();
    } catch (e) {
      this.ended = `the module trapped and has ended: ${e}`;
      throw e;
    }
    const thrown = status !== 0 ? new Error(`throw ${this.x["throw-code"]()}`) : null;
    const requests = [];
    for (const [kind, a, b, c] of this.records()) {
      if (kind === FETCH) requests.push({ kind, path: utf8.decode(this.span(a, b)) });
      else if (kind === POST) requests.push(this.post(a, b, c));
      else if (kind === REQUEST) requests.push(this.request(a, b, c));
      else if (kind === RPC) requests.push(this.rpc(a, b, c));
      else if (kind === READ) requests.push(this.read(a, b, c));
      else this.record(kind, a, b, c);
    }
    return [requests, thrown];
  }

  // Memory's bytes [a, a + n), refused unless they lie inside it.
  span(a, n) {
    const size = BigInt(this.x.memory.buffer.byteLength);
    if (a < 0n || n < 0n || a + n > size)
      throw new RangeError(`bytes [${a}, ${a + n}) lie outside the module's ${size} bytes of memory`);
    return new Uint8Array(this.x.memory.buffer, Number(a), Number(n));
  }

  // A post's path, the first c of its span's b bytes, and a copy of the rest,
  // its body.
  post(a, b, c) {
    if (c < 0n || c > b) throw new RangeError(`a post's path of ${c} bytes in its span of ${b}`);
    const s = this.span(a, b);
    return { kind: POST, path: utf8.decode(s.subarray(0, Number(c))), body: s.slice(Number(c)) };
  }

  // A tagged HTTP request. The 16-byte header gives unsigned byte lengths;
  // compare them as BigInts before narrowing to offsets in the checked span.
  request(a, b, id) {
    if (id <= 0n) throw new RangeError(`a request needs a positive request ID, got ${id}`);
    const s = this.span(a, b);
    if (b < 16n) throw new RangeError(`request header needs 16 bytes, got ${b}`);
    const v = new DataView(s.buffer, s.byteOffset, s.byteLength);
    const methodLen = v.getBigUint64(0, true);
    const pathLen = v.getBigUint64(8, true);
    if (methodLen === 0n || methodLen > b - 16n || pathLen > b - 16n - methodLen)
      throw new RangeError(`request lengths ${methodLen} and ${pathLen} exceed span ${b}`);
    if (pathLen === 0n) throw new RangeError("request needs a nonempty path");
    const methodEnd = 16 + Number(methodLen);
    const pathEnd = methodEnd + Number(pathLen);
    const method = requestUtf8.decode(s.subarray(16, methodEnd));
    if (!/^[!#$%&'*+.^_`|~0-9A-Za-z-]+$/.test(method))
      throw new Error("request method is not an HTTP method token");
    const path = requestUtf8.decode(s.subarray(methodEnd, pathEnd));
    const body = s.slice(pathEnd);
    if ((method.toUpperCase() === "GET" || method.toUpperCase() === "HEAD") && body.length)
      throw new TypeError(`${method} ${path}: a request body is not allowed`);
    return { kind: REQUEST, method, path, body, id };
  }

  // A tagged JSON-RPC request: the u64 method length, the method, then the
  // params, whose JSON value must be an object.
  rpc(a, b, id) {
    if (id <= 0n) throw new RangeError(`an rpc needs a positive id, got ${id}`);
    const s = this.span(a, b);
    if (b < 8n) throw new RangeError(`an rpc's method length needs 8 bytes, got ${b}`);
    const methodLen = new DataView(s.buffer, s.byteOffset, s.byteLength).getBigUint64(0, true);
    if (methodLen === 0n || methodLen > b - 8n)
      throw new RangeError(`an rpc's method of ${methodLen} bytes in its span of ${b}`);
    const methodEnd = 8 + Number(methodLen);
    const method = requestUtf8.decode(s.subarray(8, methodEnd));
    const params = requestUtf8.decode(s.subarray(methodEnd));
    let value;
    try {
      value = JSON.parse(params);
    } catch (e) {
      throw new Error("rpc params JSON is invalid", { cause: e });
    }
    if (value === null || typeof value !== "object" || Array.isArray(value))
      throw new Error("rpc params JSON must be an object");
    return { kind: RPC, method, params, id };
  }

  // A tagged read of a delivered file's bytes [offset, offset + length).
  read(file, offset, length) {
    const size = this.files.get(file);
    if (size === undefined) throw new RangeError(`a read of file ${file}, which no file event delivered`);
    if (length < 1n || offset < 0n || offset + length > size)
      throw new RangeError(`a read of ${length} bytes at ${offset} outside file ${file}'s ${size}`);
    return { kind: READ, file, offset, length };
  }

  write(event) {
    if (this.event === null) throw new Error("an event before the module's hello");
    if (event.length !== 7) throw new Error(`an event has ${event.length} fields, expected 7`);
    const s = this.span(this.event, EVENT_BYTES);
    const v = new DataView(s.buffer, s.byteOffset, s.byteLength);
    event.forEach((f, i) => v.setBigInt64(8 * i, BigInt(f), true));
  }

  // The run's records, each read whole before any is handled.
  records() {
    const base = BigInt(this.x["out-base"]() >>> 0);
    const len = BigInt(this.x["out-len"]() >>> 0);
    if (len % BigInt(RECORD_BYTES) !== 0n) throw new Error(`out-len ${len} is not whole records`);
    const s = this.span(base, len);
    const v = new DataView(s.buffer, s.byteOffset, s.byteLength);
    const out = [];
    for (let at = 0; at < s.byteLength; at += RECORD_BYTES)
      out.push([0, 8, 16, 24].map((k) => v.getBigInt64(at + k, true)));
    return out;
  }

  // Every record but a request, an rpc or a read.
  record(kind, a, b, c) {
    switch (kind) {
      case HELLO:
        this.event = a;
        this.host.hello();
        return;
      case DRAW:
        this.draw(a, b, c);
        return;
      case TEXT:
        this.host.text(utf8.decode(this.span(a, b)));
        return;
      case TREE:
        this.tree(a, b, c);
        return;
      case HIGHLIGHT:
        this.faces(a, b, c);
        return;
      case MESH:
        this.mesh(a, b, c);
        return;
      case UNMESH:
        this.unmesh(a, b, c);
        return;
      case SVG:
        if (c !== 0n) throw new Error("svg c must be zero");
        if (b <= 0n) throw new RangeError("svg needs a positive byte length");
        this.host.svg(this.span(a, b).slice());
        return;
      case INPUT:
        if (b !== 0n || c !== 0n) throw new Error("input b and c must be zero");
        if (a !== 0n && a !== 1n) throw new Error("input a must be 0 or 1");
        this.host.input(a === 1n);
        return;
      case AFTER:
        if (a < 0n) throw new RangeError(`an after of ${a} ms`);
        this.host.after(a, b);
        return;
      default:
        throw new Error(`a record of kind ${kind}, which names no record`);
    }
  }

  tree(a, b, c) {
    const bytes = this.span(a, b);
    let source;
    try {
      source = utf8.decode(bytes);
    } catch (e) {
      throw new Error("tree UTF-8 is invalid", { cause: e });
    }
    let rows;
    try {
      rows = JSON.parse(source);
    } catch (e) {
      throw new Error("tree JSON is invalid", { cause: e });
    }
    if (!Array.isArray(rows)) throw new Error("tree JSON must be an array");
    const keys = new Set();
    const more = new Set(); // keys of rows whose more is true
    for (const row of rows) {
      if (row === null || typeof row !== "object" || Array.isArray(row) ||
          !TREE_KEYS.has(Object.keys(row).sort().join(",")) ||
          !Number.isSafeInteger(row.key) || row.key <= 0 ||
          !Number.isSafeInteger(row.parent) || row.parent < 0 ||
          typeof row.label !== "string")
        throw new Error("tree row must have a positive safe key, earlier parent, and string label");
      if ("more" in row && typeof row.more !== "boolean")
        throw new Error(`tree row ${row.key} more must be a boolean`);
      if (keys.has(row.key)) throw new Error(`duplicate tree key ${row.key}`);
      if (row.parent !== 0 && !keys.has(row.parent))
        throw new Error(`tree parent ${row.parent} must be an earlier row key`);
      if (more.has(row.parent))
        throw new Error(`tree row ${row.key}'s parent ${row.parent} has more`);
      keys.add(row.key);
      if (row.more) more.add(row.key);
    }
    if (c < 0n || c > MAX_KEY || (c !== 0n && !keys.has(Number(c))))
      throw new Error(`selected tree key ${c} is missing`);
    this.host.tree(rows, Number(c));
  }

  // A highlight's b u32 words, copied; b covers the last draw's rows, and
  // later draws' rows must need at most b words.
  faces(a, b, c) {
    if (c !== 0n) throw new Error("highlight c must be zero");
    if (b < BigInt(this.need))
      throw new RangeError(`a highlight of ${b} words, fewer than the ` +
        `${this.need} the last draw's rows need`);
    const words = new Uint32Array(this.span(a, b * 4n).slice().buffer);
    this.host.highlight(words);
    this.words = words.length;
  }

  // A mesh, copied, that the host holds by handle c until an unmesh, with the
  // words its faces need. Typed arrays read the platform's byte order,
  // little-endian wherever this runs.
  mesh(a, b, c) {
    if (c <= 0n || c > MAX_HANDLE)
      throw new RangeError(`a mesh handle ${c} is not 1 to ${MAX_HANDLE}`);
    const handle = Number(c);
    if (this.meshes.has(handle))
      throw new Error(`a mesh of handle ${handle}, which is already held`);
    if (b < 8n) throw new RangeError(`a mesh's counts need 8 bytes, got ${b}`);
    const span = this.span(a, b);
    const counts = new DataView(span.buffer, span.byteOffset, 8);
    const vertexCount = counts.getUint32(0, true);
    const triangleCount = counts.getUint32(4, true);
    if (vertexCount === 0 || vertexCount > MAX_VERTICES)
      throw new RangeError(`a mesh of ${vertexCount} vertices, ` +
        `not 1 to ${MAX_VERTICES}`);
    const size = 8n + 8n * BigInt(vertexCount) + 6n * BigInt(triangleCount);
    if (b !== size)
      throw new RangeError(`a mesh of ${vertexCount} vertices and ` +
        `${triangleCount} triangles is ${size} bytes, not ${b}`);
    const bytes = span.slice();
    const vertices = new Uint16Array(bytes.buffer, 8, 4 * vertexCount);
    const triangles = new Uint16Array(bytes.buffer, 8 + 8 * vertexCount,
      3 * triangleCount);
    for (const index of triangles)
      if (index >= vertexCount)
        throw new RangeError(`a mesh index ${index} is not below its ` +
          `${vertexCount} vertices`);
    let face = 0;
    for (let k = 3; k < vertices.length; k += 4)
      face = Math.max(face, vertices[k]);
    this.host.mesh(handle, vertexCount, triangleCount, vertices, triangles);
    this.meshes.set(handle, Math.floor(face / 32) + 1);
  }

  unmesh(a, b, c) {
    if (b !== 0n || c !== 0n) throw new Error("unmesh b and c must be zero");
    if (a <= 0n || a > MAX_KEY || !this.meshes.has(Number(a)))
      throw new Error(`an unmesh of handle ${a}, which is not held`);
    if (this.drawn.has(Number(a)))
      throw new Error(`an unmesh of handle ${a}, which the last draw's ` +
        "rows name");
    this.host.unmesh(Number(a));
    this.meshes.delete(Number(a));
  }

  // b instance rows of held meshes, copied, drawn through matrix c; keeps
  // the handles the rows name and the most words a row needs.
  draw(a, b, c) {
    const rows = this.span(a, b * BigInt(ROW_BYTES)).slice();
    const matrix = this.span(c, MATRIX_BYTES);
    const v = new DataView(rows.buffer);
    const drawn = new Set();
    let need = 0;
    for (let at = 0; at < rows.length; at += ROW_BYTES) {
      const handle = v.getUint32(at + 48, true);
      const flags = v.getUint32(at + 56, true);
      const offset = v.getUint32(at + 60, true);
      if (!this.meshes.has(handle))
        throw new Error(`a draw of handle ${handle}, which is not held`);
      if (flags > 1)
        throw new Error(`a draw's flags ${flags} set a bit other than bit 0`);
      if (offset !== NO_OFFSET) {
        const end = offset + this.meshes.get(handle);
        if (end > this.words)
          throw new RangeError(`a draw row at offset ${offset} needs ${end} ` +
            `highlight words; the highlight has ${this.words}`);
        need = Math.max(need, end);
      }
      drawn.add(handle);
    }
    this.host.draw(rows, Number(b), matrix);
    this.drawn = drawn;
    this.need = need;
  }

  // Copies a delivery into pages the module reserves, which it then owns, and
  // answers their address; refuses one the module cannot reserve.
  place(bytes) {
    this.live();
    if (bytes.length === 0) return 0n;
    const at = this.x.reserve(BigInt(bytes.length));
    if (at === 0n) throw new RangeError(`the module cannot reserve ${bytes.length} bytes`);
    new Uint8Array(this.x.memory.buffer, Number(at), bytes.length).set(bytes);
    return at;
  }
}
