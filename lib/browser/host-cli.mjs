// host-cli.mjs - the browser host's turns under bun, for tests
// (docs/browser-host.md):
//
//    bun host-cli.mjs <module.wasm> <bytes-file> [--size W H] [--pointer X Y]...
//
// Runs the start turn, answers every fetch with the bytes file, then sends
// each pointer in order with the drawing buffer's size, 640 by 480 unless
// --size says otherwise. Prints one line per record: hello, fetch <path>,
// draw <count>, text <string>. It draws nothing. Exits 0, or 1 with the
// failure on stderr: `throw <code>` for a run that answered a nonzero status.
import { readFileSync } from "node:fs";
import { Module } from "./turn.js";

const USAGE = "usage: bun host-cli.mjs <module.wasm> <bytes-file> [--size W H] [--pointer X Y]...";

function parse(args) {
  if (args.length < 2 || (args.length - 2) % 3 !== 0) throw new Error(USAGE);
  const [wasm, file] = args;
  let size = [640n, 480n];
  const pointers = [];
  for (let i = 2; i < args.length; i += 3) {
    const pair = [BigInt(args[i + 1]), BigInt(args[i + 2])];
    if (args[i] === "--size") size = pair;
    else if (args[i] === "--pointer") pointers.push(pair);
    else throw new Error(USAGE);
  }
  return { wasm, file, size, pointers };
}

try {
  const { wasm, file, size, pointers } = parse(process.argv.slice(2));
  const module = await Module.load(readFileSync(wasm), {
    hello: () => console.log("hello"),
    fetch: async (path) => {
      console.log(`fetch ${path}`);
      return readFileSync(file);
    },
    draw: (corners, count) => console.log(`draw ${count}`),
    text: (s) => console.log(`text ${s}`),
    size: () => size,
  });
  await module.start();
  for (const [x, y] of pointers) await module.pointer(x, y);
} catch (e) {
  console.error(e.message);
  process.exitCode = 1;
}
