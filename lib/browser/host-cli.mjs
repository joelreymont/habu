// host-cli.mjs - the browser host's turns under bun, for tests
// (docs/browser-host.md):
//
//    bun host-cli.mjs <module.wasm> [--size W H] [step]...
//
// Runs the start turn, then each step in order, once every run the steps
// before it let happen has run:
//
//    --pointer X Y, --move X Y, --release X Y
//                          a pointer pressed, moved or released at X, Y
//    --typed TEXT          TEXT typed
//    --answer STATUS FILE  answers the oldest waiting request with STATUS and
//                          FILE's bytes; a fetch answered outside 200-299
//                          fails, as in the browser
//    --fail                fails the oldest waiting request, as a lost
//                          connection does
//
// The drawing buffer is 640 by 480 unless --size says otherwise. Prints one
// line per record, a request's when the host makes it: hello, fetch <path>,
// post <path> <length> <body in hex>, draw <count>, text <string>; and at the
// end, outstanding <fetch|post> <path> for each request still waiting. It
// draws nothing. Each failure is printed on stderr as it happens, such as
// `throw <code>` for a run that answered a nonzero status, and the steps go
// on; it exits 1 if any happened, else 0.
import { readFileSync } from "node:fs";
import { Module } from "./turn.js";

const USAGE =
  "usage: bun host-cli.mjs <module.wasm> [--size W H] " +
  "[--pointer X Y | --move X Y | --release X Y | --typed TEXT | --answer STATUS FILE | --fail]...";

let failed = false;

function report(e) {
  console.error(e.message);
  failed = true;
}

// The requests the host made that no step has answered, oldest first.
const waiting = [];

function request(kind, path) {
  return new Promise((resolve, reject) => waiting.push({ kind, path, resolve, reject }));
}

function oldest(flag) {
  if (waiting.length === 0) throw new Error(`${flag}: no request is outstanding`);
  return waiting.shift();
}

// Answers only once every microtask has run, so every run a step let happen.
function settled() {
  return new Promise((resolve) => setImmediate(resolve));
}

// The module's path, the drawing buffer's size and the steps, each a function
// of the module.
function parse(args) {
  if (args.length < 1) throw new Error(USAGE);
  let size = [640n, 480n];
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
        size = take(2).map(BigInt);
        break;
      case "--pointer": {
        const [x, y] = take(2).map(BigInt);
        steps.push((m) => m.pointer(x, y).catch(report));
        break;
      }
      case "--move": {
        const [x, y] = take(2).map(BigInt);
        steps.push((m) => m.moved(x, y).catch(report));
        break;
      }
      case "--release": {
        const [x, y] = take(2).map(BigInt);
        steps.push((m) => m.released(x, y).catch(report));
        break;
      }
      case "--typed": {
        const [text] = take(1);
        steps.push((m) => m.typed(text).catch(report));
        break;
      }
      case "--answer": {
        const [status, file] = take(2);
        const code = Number(BigInt(status));
        steps.push(() => {
          const bytes = readFileSync(file);
          oldest(flag).resolve({ status: code, bytes });
        });
        break;
      }
      case "--fail":
        steps.push(() => {
          const r = oldest(flag);
          r.reject(new Error(`${r.path}: failed`));
        });
        break;
      default:
        throw new Error(USAGE);
    }
  }
  return { wasm: args[0], size, steps };
}

try {
  const { wasm, size, steps } = parse(process.argv.slice(2));
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
    draw: (corners, count) => console.log(`draw ${count}`),
    text: (s) => console.log(`text ${s}`),
    size: () => size,
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
  for (const { kind, path } of waiting) console.log(`outstanding ${kind} ${path}`);
} catch (e) {
  report(e);
}
process.exitCode = failed ? 1 : 0;
