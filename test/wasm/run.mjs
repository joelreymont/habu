// run.mjs - the wasm device check's runner (test/wasm/harness.f): instantiates
// the module at the path argument with no imports, calls run and writes the OUT
// region to stdout. Exits 0 or 1 by run's status, 1 with `throw <i64>` on
// stderr; 2 on a trap; 3 on a compile or link error.
import { readFileSync } from "node:fs";

const bytes = readFileSync(process.argv[2]);
let x;
try {
  x = (await WebAssembly.instantiate(bytes)).instance.exports;
} catch (e) {
  console.error(String(e));
  process.exitCode = 3;
}
if (x) {
  try {
    process.exitCode = x.run() === 0 ? 0 : 1;
  } catch (e) {
    console.error(String(e));
    process.exitCode = 2;
  }
  process.stdout.write(new Uint8Array(x.memory.buffer, x["out-base"](), x["out-len"]()));
  if (process.exitCode === 1) console.error(`throw ${x["throw-code"]()}`);
}
