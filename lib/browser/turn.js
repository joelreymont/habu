// turn.js - the turn protocol between a host and a Habu Wasm module
// (docs/browser-host.md). The browser host (host.js) and the bun runner
// (host-cli.mjs) share it; it knows no DOM, GPU, network or file system.
//
// The module imports nothing. A turn writes one event at the address the
// module named in hello, calls run() and handles the records the run left in
// [out-base, out-base + out-len), in order. The first run has no event: the
// module's event block is zero, which reads as start. Fetch and post records
// are requests: as the records are read, each path is decoded and each post
// body copied, since later runs rewrite the storage they name and growth
// detaches it. Once the run's other records are handled, the turn makes its
// requests one at a time; memory grows by the pages each answer needs, the
// answer is copied past the old end, and the module runs again with an event
// naming it, whose own requests go before the next. A run is synchronous, so
// runs never overlap, but other turns run while a request waits. A run that
// traps ends the instance: nothing runs, and memory does not grow, after it.
//
// Every field is an i64, little-endian, and a BigInt here. Growth detaches the
// old ArrayBuffer, so every view is taken from memory.buffer where it is used:
// https://www.w3.org/TR/wasm-js-api-2/#memories

const PAGE_BYTES = 65536;
const EVENT_BYTES = 40n;
const RECORD_BYTES = 32;
const CORNER_BYTES = 36n; // a triangle's three corners of f32 x y z
const MATRIX_BYTES = 64n; // 16 f32, column-major

// Events; start, kind 0, is the zero event block the first run reads.
const BYTES = 1n;
const POINTER = 2n;
const ANSWERED = 3n;
const TYPED = 4n;
const MOVED = 5n;
const RELEASED = 6n;

// Records.
const HELLO = 0n;
const FETCH = 1n;
const DRAW = 2n;
const TEXT = 3n;
const POST = 4n;

const utf8 = new TextDecoder("utf-8", { fatal: true });
const encoder = new TextEncoder();

export class Module {
  // host: hello(), fetch(path) -> Promise<Uint8Array>, post(path, body) ->
  // Promise<{ status, bytes }> for every HTTP status, draw(corners, count,
  // matrix), text(string) and size() -> [width, height] of the drawing buffer.
  static async load(bytes, host) {
    const { instance } = await WebAssembly.instantiate(bytes);
    return new Module(instance.exports, host);
  }

  constructor(exports, host) {
    this.x = exports;
    this.host = host;
    this.event = null; // the address hello named
    this.ended = null; // the trap that ended the instance
  }

  // The first turn.
  start() {
    return this.turn(null);
  }

  // Turns for a pointer pressed, moved and released at x, y in the drawing
  // buffer's pixels.
  pointer(x, y) {
    return this.turn([POINTER, x, y, ...this.host.size()]);
  }

  moved(x, y) {
    return this.turn([MOVED, x, y, ...this.host.size()]);
  }

  released(x, y) {
    return this.turn([RELEASED, x, y, ...this.host.size()]);
  }

  // A turn for typed text, its UTF-8 bytes placed as an answer's are.
  async typed(text) {
    const bytes = encoder.encode(text);
    await this.turn([TYPED, this.place(bytes), bytes.length, ...this.host.size()]);
  }

  // The event's run, then each request it made, in order.
  async turn(event) {
    for (const request of this.run(event)) await this.answer(request);
  }

  async answer({ kind, path, body }) {
    if (kind === FETCH) {
      const bytes = await this.host.fetch(path);
      await this.turn([BYTES, this.place(bytes), bytes.length, ...this.host.size()]);
    } else {
      const { status, bytes } = await this.host.post(path, body);
      await this.turn([ANSWERED, this.place(bytes), bytes.length, status, 0]);
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

  write(event) {
    if (this.event === null) throw new Error("an event before the module's hello");
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
        this.host.draw(this.span(a, b * CORNER_BYTES), Number(b), this.span(c, MATRIX_BYTES));
        return;
      case TEXT:
        this.host.text(utf8.decode(this.span(a, b)));
        return;
      default:
        throw new Error(`a record of kind ${kind}, which names no record`);
    }
  }

  // The bytes copied past memory's old end, which grows by the pages they
  // need; answers where they start. A trap's instance is refused first.
  place(bytes) {
    this.live();
    const m = this.x.memory;
    const at = m.buffer.byteLength;
    m.grow(Math.ceil(bytes.length / PAGE_BYTES));
    new Uint8Array(m.buffer, at, bytes.length).set(bytes);
    return BigInt(at);
  }
}
