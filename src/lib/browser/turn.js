// turn.js - the turn protocol between a host and a Habu Wasm module
// (docs/browser-host.md). The browser host (host.js) and the bun runner
// (host-cli.mjs) share it; it knows no DOM, GPU, network or file system.
//
// The module imports nothing. A turn writes one event at the address the
// module named in hello, calls run() and handles the records the run left in
// [out-base, out-base + out-len), in order. The first run has no event: the
// module's event block is zero, which reads as start. Fetch, post and request
// records are requests: as the records are read, each path is decoded and each
// body copied, since later runs rewrite the storage they name and growth detaches
// it. Once the run's other records are handled, tagged requests from that run
// start independently; legacy fetches and posts still wait in order, including
// the requests made by their replies. An answer is copied into host-owned
// pages, growing memory only when it does not fit in their unused tail. The
// module runs again with an event naming those bytes.
// Runs are synchronous and never overlap, but other turns
// run while a request waits. A run that
// traps ends the instance: nothing runs, and memory does not grow, after it.
//
// Every field is an i64, little-endian, and a BigInt here. Growth detaches the
// old ArrayBuffer, so every view is taken from memory.buffer where it is used:
// https://www.w3.org/TR/wasm-js-api-2/#memories

const PAGE_BYTES = 65536;
const EVENT_BYTES = 56n;
const RECORD_BYTES = 32;
const CORNER_BYTES = 36n; // a triangle's three corners of f32 x y z
const MATRIX_BYTES = 64n; // 16 f32, column-major
const MAX_KEY = BigInt(Number.MAX_SAFE_INTEGER);

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

// Records.
const HELLO = 0n;
const FETCH = 1n;
const DRAW = 2n;
const TEXT = 3n;
const POST = 4n;
const TREE = 5n;
const HIGHLIGHT = 6n;
const REQUEST = 7n;

const utf8 = new TextDecoder("utf-8", { fatal: true });
// A request's leading BOM bytes are content, including at the start of a path.
const requestUtf8 = new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const encoder = new TextEncoder();

export class Module {
  // host: hello(), fetch(path) -> Promise<Uint8Array>, post(path, body) ->
  // Promise<{ status, bytes }>, request(method, path, body, id) -> the same;
  // id is an optional diagnostic argument for the CLI. A host that handles
  // REQUEST also provides error(error) for asynchronous reply failures.
  // Also draw(corners, count, matrix, mask), text(string), tree(rows, selected),
  // highlight(mask), and size() -> [width, height] of the drawing buffer.
  static async load(bytes, host) {
    const { instance } = await WebAssembly.instantiate(bytes);
    return new Module(instance.exports, host);
  }

  constructor(exports, host) {
    this.x = exports;
    this.host = host;
    this.event = null; // the address hello named
    this.ended = null; // the trap that ended the instance
    this.mask = null; // no highlight record yet: draw with an unselected mask
    this.inputNext = 0; // next free byte in the host's last grown chunk
    this.inputLimit = 0;
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

  control(key) {
    const id = BigInt(key);
    if (id <= 0n || id > MAX_KEY) throw new RangeError(`control key ${id} is not a positive safe integer`);
    return this.turn([CONTROL, id, 0, ...this.host.size(), 0, 0]);
  }

  // A turn for typed text, its UTF-8 bytes placed as an answer's are.
  async typed(text) {
    const bytes = encoder.encode(text);
    await this.turn([TYPED, this.place(bytes), bytes.length, ...this.host.size(), 0, 0]);
  }

  // The event's run, then its requests. All tagged requests start independently;
  // legacy requests keep their order and wait for their own recursive replies.
  async turn(event) {
    const requests = this.run(event);
    if (requests.some((request) => request.kind === REQUEST) && typeof this.host.error !== "function")
      throw new TypeError("host.error(error) is required for REQUEST replies");
    const errors = [];
    const tagged = requests.filter((request) => request.kind === REQUEST)
      .map((request) => this.answer(request).catch((error) => {
        errors.push(error);
        this.host.error(errors.length === 1 ? error :
          new AggregateError([...errors], errors.map(String).join("; ")));
      }));
    const legacy = (async () => {
      for (const request of requests)
        if (request.kind !== REQUEST) await this.answer(request);
    })();
    await Promise.all([...tagged, legacy]);
  }

  async answer({ kind, path, body, method, id }) {
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
    } else {
      const { status, bytes } = await this.host.post(path, body);
      await this.turn([ANSWERED, this.place(bytes), bytes.length, status, 0, 0, 0]);
    }
  }

  // Refuses the instance a trap ended.
  live() {
    if (this.ended) throw new Error(`the module trapped and has ended: ${this.ended}`);
  }

  // Writes the event and runs the module; handles the records but the
  // requests, which it answers.
  run(event) {
    this.live();
    if (event) this.write(event);
    let status;
    try {
      status = this.x.run();
    } catch (e) {
      this.ended = e;
      throw e;
    }
    if (status !== 0) throw new Error(`throw ${this.x["throw-code"]()}`);
    const requests = [];
    for (const [kind, a, b, c] of this.records()) {
      if (kind === FETCH) requests.push({ kind, path: utf8.decode(this.span(a, b)) });
      else if (kind === POST) requests.push(this.post(a, b, c));
      else if (kind === REQUEST) requests.push(this.request(a, b, c));
      else this.record(kind, a, b, c);
    }
    return requests;
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

  // Every record but a request.
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
        this.highlight(a, b, c);
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
    for (const row of rows) {
      if (row === null || typeof row !== "object" || Array.isArray(row) ||
          Object.keys(row).sort().join(",") !== "key,label,parent" ||
          !Number.isSafeInteger(row.key) || row.key <= 0 ||
          !Number.isSafeInteger(row.parent) || row.parent < 0 ||
          typeof row.label !== "string")
        throw new Error("tree row must have a positive safe key, earlier parent, and string label");
      if (keys.has(row.key)) throw new Error(`duplicate tree key ${row.key}`);
      if (row.parent !== 0 && !keys.has(row.parent))
        throw new Error(`tree parent ${row.parent} must be an earlier row key`);
      keys.add(row.key);
    }
    if (c < 0n || c > MAX_KEY || (c !== 0n && !keys.has(Number(c))))
      throw new Error(`selected tree key ${c} is missing`);
    this.host.tree(rows, Number(c));
  }

  highlight(a, b, c) {
    if (c !== 0n) throw new Error("highlight c must be zero");
    const mask = this.span(a, b);
    if (mask.some((value) => value > 1)) throw new Error("highlight mask contains a value other than 0 or 1");
    this.mask = mask.slice();
    this.host.highlight(this.mask);
  }

  draw(a, b, c) {
    const corners = this.span(a, b * CORNER_BYTES);
    const matrix = this.span(c, MATRIX_BYTES);
    const count = Number(b);
    const mask = this.mask ?? new Uint8Array(count);
    if (mask.length !== count)
      throw new Error(`highlight count ${mask.length} does not match draw count ${count}`);
    this.host.draw(corners, count, matrix, mask);
  }

  // Keep host inputs in pages grown by the host, including after the module
  // grows memory beyond them. Every placed span remains live for the instance.
  place(bytes) {
    this.live();
    const m = this.x.memory;
    if (bytes.length === 0) return BigInt(m.buffer.byteLength);
    if (bytes.length > this.inputLimit - this.inputNext) {
      const at = m.buffer.byteLength;
      const pages = Math.ceil(bytes.length / PAGE_BYTES);
      m.grow(pages);
      this.inputNext = at;
      this.inputLimit = at + pages * PAGE_BYTES;
    }
    const at = this.inputNext;
    new Uint8Array(m.buffer, at, bytes.length).set(bytes);
    this.inputNext += bytes.length;
    return BigInt(at);
  }
}
