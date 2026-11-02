// host-cli.mjs - the browser host's turns (turn.js) under bun, for tests. It
// proves no rendering:
//
//    bun host-cli.mjs <module.wasm> [--size W H] [step]...
//
// Runs the start turn, then each step in order, once every run the steps
// before it let happen has run:
//
//    --pointer X Y BUTTONS MODS, --move X Y BUTTONS MODS,
//    --release X Y BUTTONS MODS
//                          a pointer pressed, moved or released at X, Y
//    --wheel DX DY MODS    signed device-pixel wheel deltas
//    --key TEXT MODS       KeyboardEvent.key on the focused canvas
//    --resize W H          change the drawing buffer and send a resize event
//    --control KEY         select a transient tree row
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
//
// The drawing buffer is 640 by 480 unless --size says otherwise. Prints one
// line per record, a request's when the host makes it: hello, fetch <path>,
// post <path> <length> <body in hex>, request <method> <path> <id> <length>
// <hex>, rpc <method> <id> <params JSON>, after <ms> <token>, read <file>
// <offset> <length>, tree <selected> <JSON>, highlight <count> <mask in hex>,
// draw <count> <mask in hex>, svg <length> <bytes in hex>, input <0|1>, text
// <string>. At the end, outstanding <fetch|post> <path>, outstanding request
// <method> <path> <id> or outstanding rpc <method> <id>. A --tick with no after
// waiting, or an --answer-rpc or --fail-rpc of an id no waiting rpc holds, is
// a failure.
// It
// draws nothing. Each failure is printed on stderr as it happens, such as
// `throw <code>` for a run that answered a nonzero status, and the steps go
// on; it exits 1 if any happened, else 0.
import { readFileSync } from "node:fs";
import { Module } from "./turn.js";

const USAGE =
  "usage: bun host-cli.mjs <module.wasm> [--size W H] " +
  "[--pointer X Y BUTTONS MODS | --move X Y BUTTONS MODS | --release X Y BUTTONS MODS | " +
  "--wheel DX DY MODS | --key TEXT MODS | --resize W H | --control KEY | " +
  "--load FILE | --focus HEX | --highlight HEX | " +
  "--typed TEXT | --repeat-typed TEXT N | --memory | " +
  "--answer STATUS FILE | --answer-id ID STATUS FILE | --fail | " +
  "--notify METHOD FILE | --tick | --file NAME PATH | --answer-rpc ID FILE | --fail-rpc ID]...";

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

// The rpcs the host made that no step has answered and the tokens of the
// afters no --tick has fired, each oldest first; each --file's bytes by number.
const calls = [];
const ticks = [];
const files = new Map();

function call(flag, id) {
  const at = calls.findIndex((r) => r.id === id);
  if (at < 0) throw new Error(`${flag}: no rpc ${id} is outstanding`);
  return calls.splice(at, 1)[0];
}

// Answers only once every microtask has run, so every run a step let happen.
function settled() {
  return new Promise((resolve) => setImmediate(resolve));
}

// The module's path, the drawing buffer's size and the steps, each a function
// of the module.
function parse(args) {
  if (args.length < 1) throw new Error(USAGE);
  const state = { size: [640n, 480n], memory: false };
  const steps = [];
  let i = 1;
  const take = (n) => {
    if (i + n > args.length) throw new Error(USAGE);
    i += n;
    return args.slice(i - n, i);
  };
  while (i < args.length) {
    const flag = args[i++];
    switch (flag) {
      case "--size":
        state.size = take(2).map(BigInt);
        break;
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
      default:
        throw new Error(USAGE);
    }
  }
  return { wasm: args[0], state, steps };
}

try {
  const { wasm, state, steps } = parse(process.argv.slice(2));
  const module = await Module.load(readFileSync(wasm), {
    hello: () => console.log("hello"),
    fetch: (path) => {
      console.log(`fetch ${path}`);
      return request("fetch", path).then(({ status, bytes }) => {
        if (status < 200 || status > 299) throw new Error(`${path}: ${status}`);
        return bytes;
      });
    },
    post: (path, body) => {
      console.log(`post ${path} ${body.length} ${Buffer.from(body).toString("hex")}`);
      return request("post", path);
    },
    request: (method, path, body, id) => {
      console.log(`request ${method} ${path} ${id} ${body.length} ${Buffer.from(body).toString("hex")}`);
      return request("request", path, method, id);
    },
    rpc: (method, params, id) => {
      console.log(`rpc ${method} ${id} ${params}`);
      return new Promise((resolve, reject) => calls.push({ id, method, resolve, reject }));
    },
    after: (ms, token) => {
      console.log(`after ${ms} ${token}`);
      ticks.push(token);
    },
    read: (file, offset, length) => {
      console.log(`read ${file} ${offset} ${length}`);
      return Promise.resolve(files.get(Number(file)).subarray(Number(offset), Number(offset + length)));
    },
    error: report,
    draw: (corners, count, matrix, mask) =>
      console.log(`draw ${count}${count ? ` ${Buffer.from(mask).toString("hex")}` : ""}`),
    tree: (rows, selected) => console.log(`tree ${selected} ${JSON.stringify(rows)}`),
    highlight: (mask) =>
      console.log(`highlight ${mask.length}${mask.length ? ` ${Buffer.from(mask).toString("hex")}` : ""}`),
    svg: (bytes) => console.log(`svg ${bytes.length} ${Buffer.from(bytes).toString("hex")}`),
    input: (enabled) => console.log(`input ${enabled ? 1 : 0}`),
    text: (s) => console.log(`text ${s}`),
    size: () => state.size,
  });
  module.start().catch(report);
  await settled();
  for (const step of steps) {
    try {
      step(module);
    } catch (e) {
      report(e);
    }
    await settled();
  }
  for (const { kind, path, method, id } of waiting)
    console.log(kind === "request" ? `outstanding request ${method} ${path} ${id}` : `outstanding ${kind} ${path}`);
  for (const { id, method } of calls) console.log(`outstanding rpc ${method} ${id}`);
  if (state.memory) console.log(`memory ${module.x.memory.buffer.byteLength}`);
} catch (e) {
  report(e);
}
process.exitCode = failed ? 1 : 0;
