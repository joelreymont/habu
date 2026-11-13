// host-cli.mjs - the browser host's turns (turn.js) under bun, for tests. It
// proves no rendering:
//
//    bun host-cli.mjs <module.wasm> [--size W H] [--mcp URL TOKEN-FILE] [step]...
//
// Runs the start turn, then each step in order, once every run the steps
// before it let happen has run. With --mcp, rpcs go to URL as host.js sends
// them, with TOKEN-FILE's text, trailing whitespace trimmed, as a bearer token,
// and afters fire on real time; a step is done once no relay is outstanding.
// The steps:
//
//    --pointer X Y BUTTONS MODS, --move X Y BUTTONS MODS,
//    --release X Y BUTTONS MODS
//                          a pointer pressed, moved or released at X, Y
//    --wheel DX DY MODS    signed device-pixel wheel deltas
//    --key TEXT MODS       KeyboardEvent.key on the focused canvas
//    --resize W H          change the drawing buffer and send a resize event
//    --control KEY         select a transient tree row
//    --open KEY            open a transient tree row whose more is true
//    --load FILE           FILE's bytes as a scene
//    --focus HEX, --highlight HEX
//                          focus or highlight HEX's packed 16-byte identities,
//                          32 hex digits each; "" clears
//    --typed TEXT          TEXT typed
//    --repeat-typed TEXT N  type TEXT N times in the same module
//    --memory              print final Wasm memory capacity in bytes
//    --answer STATUS FILE  answers the oldest waiting request with STATUS and
//                          FILE's bytes; a fetch answered outside 200-299
//                          fails, as in the browser
//    --answer-id ID STATUS FILE
//                          answers the waiting tagged request with ID, as a
//                          reply that overtakes earlier ones does
//    --fail                fails the oldest waiting request, as a lost
//                          connection does
//    --notify METHOD FILE  a host notification of METHOD whose params are
//                          FILE's JSON text
//    --tick                the tick of the oldest after no --tick has fired
//    --file NAME PATH      a file named NAME of PATH's bytes, numbered from 1;
//                          reads of it are answered from those bytes
//    --answer-rpc ID FILE  answers the waiting rpc with ID with FILE's JSON
//                          text, {"result":…} or {"error":…}
//    --fail-rpc ID         fails the waiting rpc with ID, as a lost parent does
//    --picked INSTANCE FACE DEPTH
//                          answers the oldest waiting pick with INSTANCE, -1
//                          to 0xFFFFFFFF, FACE, 0 to 0xFFFF, and DEPTH, a
//                          decimal whose f32 is finite, sent as its f32 bits;
//                          a value outside these is a usage error, and none
//                          waiting a failure
//    --occluded HEX        answers the oldest waiting occlude with HEX's bytes,
//                          one per box; none waiting, or a byte count other
//                          than its boxes, is a failure
//    --until TEXT MS       waits until a line printed after it began contains
//                          TEXT; MS ms without one is a failure
//
// --tick, --answer-rpc and --fail-rpc are usage errors with --mcp, and --until
// without it.
// The drawing buffer is 640 by 480 unless --size says otherwise. Prints one
// line per record, a request's when the host makes it: hello, fetch <path>,
// post <path> <length> <body in hex>, request <method> <path> <id> <length>
// <hex>, rpc <method> <id> <params JSON>, after <ms> <token>, read <file>
// <offset> <length>, pick <x> <y> <id>, occlude <boxes> <id>, tree <selected>
// <JSON>, mesh <handle> <V> <T> <origin x> <origin y> <origin z>, unmesh
// <handle>, highlight <words> <words in
// hex, little-endian>, draw <count> then each row's
// <handle>/<id>/<flags>/<offset or ->/<rrggbb>/<metallic>/<roughness>, the
// last two the u16s in decimal, ground <nx> <ny> <nz> <d> or ground none, svg
// <length> <bytes in hex>, input <0|1>, text <string>; a highlight or draw of
// none prints only its count. At the end,
// outstanding <fetch|post> <path>, outstanding request <method> <path> <id>,
// outstanding rpc <method> <id>, outstanding pick <id> or outstanding occlude
// <id>. A --tick with no after
// waiting, an --answer-rpc or --fail-rpc of an id no waiting rpc holds, or a
// relay that fails or passes RELAY_MS, is a failure.
// It
// draws nothing. Each failure is printed on stderr as it happens, such as
// `throw <code>` for a run that answered a nonzero status, and the steps go
// on; it exits 1 if any happened, else 0.
import { readFileSync } from "node:fs";
import { Module } from "./turn.js";
import { mcpRpc } from "./mcp.js";

const USAGE =
  "usage: bun host-cli.mjs <module.wasm> [--size W H] [--mcp URL TOKEN-FILE] " +
  "[--pointer X Y BUTTONS MODS | --move X Y BUTTONS MODS | --release X Y BUTTONS MODS | " +
  "--wheel DX DY MODS | --key TEXT MODS | --resize W H | --control KEY | " +
  "--open KEY | " +
  "--load FILE | --focus HEX | --highlight HEX | " +
  "--typed TEXT | --repeat-typed TEXT N | --memory | " +
  "--answer STATUS FILE | --answer-id ID STATUS FILE | --fail | " +
  "--notify METHOD FILE | --tick | --file NAME PATH | --answer-rpc ID FILE | --fail-rpc ID | " +
  "--picked INSTANCE FACE DEPTH | --occluded HEX | --until TEXT MS]...";

// The time a relayed rpc may take before it fails.
const RELAY_MS = 60000;

let failed = false;

function report(e) {
  console.error(e.message);
  failed = true;
}

// The requests the host made that no step has answered, oldest first.
const waiting = [];

function request(kind, path, method, id) {
  return new Promise((resolve, reject) => waiting.push({ kind, path, method, id, resolve, reject }));
}

function oldest(flag) {
  if (waiting.length === 0) throw new Error(`${flag}: no request is outstanding`);
  return waiting.shift();
}

function byId(flag, id) {
  const at = waiting.findIndex((r) => r.kind === "request" && r.id === id);
  if (at < 0) throw new Error(`${flag}: no request ${id} is outstanding`);
  return waiting.splice(at, 1)[0];
}

// The rpcs, picks and occludes the host made that no step has answered and
// the tokens of the afters no --tick has fired, each oldest first; each
// --file's bytes by number.
const calls = [];
const picks = [];
const occludes = [];
const ticks = [];
const files = new Map();

function call(flag, id) {
  const at = calls.findIndex((r) => r.id === id);
  if (at < 0) throw new Error(`${flag}: no rpc ${id} is outstanding`);
  return calls.splice(at, 1)[0];
}

// Under --mcp, the relays not yet settled and the timers of the afters not
// yet fired.
const relays = new Set();
const timers = new Set();

// The waiting --until, if any: its text and how it is done.
let until = null;

// Prints a record line and wakes the waiting --until whose text it contains.
function print(line) {
  console.log(line);
  if (until !== null && line.includes(until.text)) until.done();
}

// Answers only once every microtask has run, so every run a step let happen.
function settled() {
  return new Promise((resolve) => setImmediate(resolve));
}

// Answers once every run has run and no relay is outstanding.
async function quiet() {
  await settled();
  while (relays.size > 0) {
    await Promise.allSettled([...relays]);
    await settled();
  }
}

// A step the loop awaits: its function's promise is its wait.
function waits(step) {
  step.waits = true;
  return step;
}

// The module's path, the drawing buffer's size and the steps, each a function
// of the module.
function parse(args) {
  if (args.length < 1) throw new Error(USAGE);
  const state = { size: [640n, 480n], memory: false, mcp: null };
  const steps = [];
  const seen = new Set();
  let i = 1;
  const take = (n) => {
    if (i + n > args.length) throw new Error(USAGE);
    i += n;
    return args.slice(i - n, i);
  };
  while (i < args.length) {
    const flag = args[i++];
    seen.add(flag);
    switch (flag) {
      case "--size":
        state.size = take(2).map(BigInt);
        break;
      case "--mcp": {
        const [url, file] = take(2);
        state.mcp = { url, token: readFileSync(file, "utf8").trimEnd() };
        break;
      }
      case "--pointer": {
        const [x, y, buttons, mods] = take(4).map(BigInt);
        steps.push((m) => m.pointer(x, y, buttons, mods).catch(report));
        break;
      }
      case "--move": {
        const [x, y, buttons, mods] = take(4).map(BigInt);
        steps.push((m) => m.moved(x, y, buttons, mods).catch(report));
        break;
      }
      case "--release": {
        const [x, y, buttons, mods] = take(4).map(BigInt);
        steps.push((m) => m.released(x, y, buttons, mods).catch(report));
        break;
      }
      case "--wheel": {
        const [dx, dy, mods] = take(3).map(BigInt);
        steps.push((m) => m.wheel(dx, dy, mods).catch(report));
        break;
      }
      case "--key": {
        const [key, mods] = take(2);
        steps.push((m) => m.key(key, BigInt(mods)).catch(report));
        break;
      }
      case "--resize": {
        const next = take(2).map(BigInt);
        steps.push((m) => {
          state.size = next;
          m.resize().catch(report);
        });
        break;
      }
      case "--control": {
        const [key] = take(1).map(BigInt);
        steps.push((m) => m.control(key).catch(report));
        break;
      }
      case "--open": {
        const [key] = take(1).map(BigInt);
        steps.push((m) => m.control(key, true).catch(report));
        break;
      }
      case "--load": {
        const [file] = take(1);
        steps.push((m) => m.load(readFileSync(file)).catch(report));
        break;
      }
      case "--focus":
      case "--highlight": {
        const [hex] = take(1);
        if (!/^(?:[0-9a-fA-F]{32})*$/.test(hex)) throw new Error(USAGE);
        const ids = Buffer.from(hex, "hex");
        steps.push((m) => (flag === "--focus" ? m.focus(ids) : m.highlight(ids)).catch(report));
        break;
      }
      case "--typed": {
        const [text] = take(1);
        steps.push((m) => m.typed(text).catch(report));
        break;
      }
      case "--repeat-typed": {
        const [text, times] = take(2);
        const count = Number(times);
        if (!Number.isSafeInteger(count) || count < 0) throw new Error(USAGE);
        for (let n = 0; n < count; n++) steps.push((m) => m.typed(text).catch(report));
        break;
      }
      case "--memory":
        state.memory = true;
        break;
      case "--answer": {
        const [status, file] = take(2);
        const code = Number(BigInt(status));
        steps.push(() => {
          const bytes = readFileSync(file);
          oldest(flag).resolve({ status: code, bytes });
        });
        break;
      }
      case "--answer-id": {
        const [id, status, file] = take(3);
        const key = BigInt(id);
        const code = Number(BigInt(status));
        steps.push(() => {
          const bytes = readFileSync(file);
          byId(flag, key).resolve({ status: code, bytes });
        });
        break;
      }
      case "--fail":
        steps.push(() => {
          const r = oldest(flag);
          r.reject(new Error(`${r.path}: failed`));
        });
        break;
      case "--notify": {
        const [method, file] = take(2);
        steps.push((m) => m.notified(method, readFileSync(file, "utf8")).catch(report));
        break;
      }
      case "--tick":
        steps.push((m) => {
          if (ticks.length === 0) throw new Error(`${flag}: no after is waiting`);
          m.tick(ticks.shift()).catch(report);
        });
        break;
      case "--file": {
        const [name, path] = take(2);
        steps.push((m) => {
          const bytes = readFileSync(path);
          const number = files.size + 1;
          files.set(number, bytes);
          m.file(name, bytes.length, number).catch(report);
        });
        break;
      }
      case "--answer-rpc": {
        const [id, file] = take(2);
        const key = BigInt(id);
        steps.push(() => call(flag, key).resolve(readFileSync(file, "utf8")));
        break;
      }
      case "--fail-rpc": {
        const key = BigInt(take(1)[0]);
        steps.push(() => call(flag, key).reject(new Error(`rpc ${key}: failed`)));
        break;
      }
      case "--picked": {
        const [instance, face, depth] = take(3);
        const whole = (text, low, high) =>
          /^-?[0-9]+$/.test(text) && BigInt(text) >= low && BigInt(text) <= high;
        const z = Math.fround(Number(depth));
        if (!whole(instance, -1n, 0xFFFFFFFFn) || !whole(face, 0n, 0xFFFFn) ||
            !/^[-+]?(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:[eE][-+]?[0-9]+)?$/.test(depth) ||
            !Number.isFinite(z))
          throw new Error(USAGE);
        const bits = new DataView(new ArrayBuffer(4));
        bits.setFloat32(0, z);
        const answer = [Number(instance), Number(face), bits.getUint32(0)];
        steps.push(() => {
          if (picks.length === 0) throw new Error(`${flag}: no pick is waiting`);
          picks.shift().resolve(answer);
        });
        break;
      }
      case "--occluded": {
        const [hex] = take(1);
        if (!/^(?:[0-9a-fA-F]{2})*$/.test(hex)) throw new Error(USAGE);
        const bytes = Uint8Array.from(Buffer.from(hex, "hex"));
        steps.push(() => {
          if (occludes.length === 0) throw new Error(`${flag}: no occlude is outstanding`);
          const { id, count, resolve } = occludes[0];
          if (bytes.length !== count)
            throw new Error(`${flag}: ${bytes.length} bytes for occlude ${id}'s ${count} boxes`);
          occludes.shift();
          resolve(bytes);
        });
        break;
      }
      case "--until": {
        const [text, limit] = take(2);
        const ms = Number(limit);
        if (!Number.isSafeInteger(ms) || ms < 0) throw new Error(USAGE);
        steps.push(waits(() => new Promise((resolve) => {
          const timer = setTimeout(() => {
            until = null;
            report(new Error(`${flag} ${text}: not printed within ${ms} ms`));
            resolve();
          }, ms);
          until = {
            text,
            done: () => {
              clearTimeout(timer);
              until = null;
              resolve();
            },
          };
        })));
        break;
      }
      default:
        throw new Error(USAGE);
    }
  }
  const scripted = ["--tick", "--answer-rpc", "--fail-rpc"].some((f) => seen.has(f));
  if (state.mcp !== null ? scripted : seen.has("--until")) throw new Error(USAGE);
  return { wasm: args[0], state, steps };
}

try {
  const { wasm, state, steps } = parse(process.argv.slice(2));
  const relay = state.mcp === null ? null : mcpRpc(state.mcp.url, state.mcp.token, RELAY_MS);
  const module = await Module.load(readFileSync(wasm), {
    hello: () => print("hello"),
    fetch: (path) => {
      print(`fetch ${path}`);
      return request("fetch", path).then(({ status, bytes }) => {
        if (status < 200 || status > 299) throw new Error(`${path}: ${status}`);
        return bytes;
      });
    },
    post: (path, body) => {
      print(`post ${path} ${body.length} ${Buffer.from(body).toString("hex")}`);
      return request("post", path);
    },
    request: (method, path, body, id) => {
      print(`request ${method} ${path} ${id} ${body.length} ${Buffer.from(body).toString("hex")}`);
      return request("request", path, method, id);
    },
    rpc: (method, params, id) => {
      print(`rpc ${method} ${id} ${params}`);
      if (relay === null) return new Promise((resolve, reject) => calls.push({ id, method, resolve, reject }));
      const answer = relay(method, params).catch((e) => {
        report(new Error(`rpc ${id}: ${e.message}`));
        throw e;
      });
      const settle = () => relays.delete(answer);
      relays.add(answer);
      answer.then(settle, settle);
      return answer;
    },
    after: (ms, token) => {
      print(`after ${ms} ${token}`);
      if (relay === null) {
        ticks.push(token);
        return;
      }
      const timer = setTimeout(() => {
        timers.delete(timer);
        module.tick(token).catch(report);
      }, Number(ms));
      timers.add(timer);
    },
    read: (file, offset, length) => {
      print(`read ${file} ${offset} ${length}`);
      return Promise.resolve(files.get(Number(file)).subarray(Number(offset), Number(offset + length)));
    },
    pick: (x, y, id) => {
      print(`pick ${x} ${y} ${id}`);
      return new Promise((resolve) => picks.push({ id, resolve }));
    },
    occlude: (boxes, id) => {
      const count = boxes.length / 6;
      print(`occlude ${count} ${id}`);
      return new Promise((resolve) => occludes.push({ id, count, resolve }));
    },
    error: report,
    mesh: (handle, vertexCount, triangleCount, origin) =>
      print(`mesh ${handle} ${vertexCount} ${triangleCount} ${origin.join(" ")}`),

    unmesh: (handle) => print(`unmesh ${handle}`),
    draw: (rows, count) => {
      const v = new DataView(rows.buffer, rows.byteOffset, rows.byteLength);
      let line = `draw ${count}`;
      for (let at = 0; at < rows.length; at += 72) {
        const [handle, id, flags, offset] =
          [48, 52, 56, 60].map((k) => v.getUint32(at + k, true));
        const from = offset === 0xFFFFFFFF ? "-" : offset;
        const rgb = Buffer.from(rows.subarray(at + 64, at + 67)).toString("hex");
        const [metallic, roughness] =
          [68, 70].map((k) => v.getUint16(at + k, true));
        line += ` ${handle}/${id}/${flags}/${from}/${rgb}/${metallic}/${roughness}`;
      }
      print(line);
    },
    tree: (rows, selected) => print(`tree ${selected} ${JSON.stringify(rows)}`),
    highlight: (words) => {
      const hex = Buffer.from(words.buffer, words.byteOffset,
        words.byteLength).toString("hex");
      print(`highlight ${words.length}${words.length ? ` ${hex}` : ""}`);
    },
    ground: (plane) => print(`ground ${plane ? plane.join(" ") : "none"}`),
    svg: (bytes) => print(`svg ${bytes.length} ${Buffer.from(bytes).toString("hex")}`),
    input: (enabled) => print(`input ${enabled ? 1 : 0}`),
    text: (s) => print(`text ${s}`),
    size: () => state.size,
  });
  module.start().catch(report);
  await quiet();
  for (const step of steps) {
    let wait;
    try {
      wait = step(module);
    } catch (e) {
      report(e);
    }
    if (step.waits) await wait;
    await quiet();
  }
  for (const timer of timers) clearTimeout(timer);
  timers.clear();
  for (const { kind, path, method, id } of waiting)
    console.log(kind === "request" ? `outstanding request ${method} ${path} ${id}` : `outstanding ${kind} ${path}`);
  for (const { id, method } of calls) console.log(`outstanding rpc ${method} ${id}`);
  for (const { id } of picks) console.log(`outstanding pick ${id}`);
  for (const { id } of occludes) console.log(`outstanding occlude ${id}`);
  if (state.memory) console.log(`memory ${module.x.memory.buffer.byteLength}`);
} catch (e) {
  report(e);
}
process.exitCode = failed ? 1 : 0;
