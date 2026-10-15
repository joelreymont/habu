// data-peer.mjs - a foreign host for test/wasm/data-test.f that observes and
// decides nothing. It instantiates the module at the path argument with no
// imports, writes its memory to the before path, grows memory one page and
// stores the value argument at the offset argument in that page, calls run,
// writes the OUT region to stdout and the memory to the after path. Exits 0 or
// 1 by run's status, with the throw code on stderr for 1, and 2 on a trap with
// the trap on stderr, as run.mjs does.
import { readFileSync, writeFileSync } from "node:fs";

const [path, before, after, offset, value] = process.argv.slice(2);
const x = (await WebAssembly.instantiate(readFileSync(path))).instance.exports;
writeFileSync(before, new Uint8Array(x.memory.buffer));
const hostPage = x.memory.grow(1) * 65536;
new Uint8Array(x.memory.buffer)[hostPage + Number(offset)] = Number(value);
try {
  process.exitCode = x.run() === 0 ? 0 : 1;
} catch (e) {
  console.error(String(e));
  process.exitCode = 2;
}
process.stdout.write(new Uint8Array(x.memory.buffer, x["out-base"](), x["out-len"]()));
if (process.exitCode === 1) console.error(`throw ${x["throw-code"]()}`);
writeFileSync(after, new Uint8Array(x.memory.buffer));
