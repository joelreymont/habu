// A foreign peer for the checked data-region program. The source's string
// owner occupies the captured image's first page.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";

const [path, taskBaseArg, dataBytesArg, imageBaseArg, stackBaseArg, stackBytesArg, baseCellArg, capCellArg] = process.argv.slice(2);
const [taskBase, dataBytes, imageBase, stackBase, stackBytes, baseCell, capCell] =
  [taskBaseArg, dataBytesArg, imageBaseArg, stackBaseArg, stackBytesArg, baseCellArg, capCellArg].map(Number);
const { instance } = await WebAssembly.instantiate(readFileSync(path));
const x = instance.exports;
const memory = x.memory;
const ownerBytes = 65536;
const before = Uint8Array.from(new Uint8Array(memory.buffer, imageBase, ownerBytes));
const hostPage = memory.grow(1) * 65536;
new Uint8Array(memory.buffer)[hostPage + 7] = 173;
assert.equal(x.run(), 0);
assert.deepEqual(new Uint8Array(memory.buffer, imageBase, ownerBytes), before);
const view = new DataView(memory.buffer);
assert.equal(view.getBigUint64(taskBase + baseCell, true), BigInt(stackBase));
assert.equal(view.getBigUint64(taskBase + capCell, true), BigInt(stackBytes));
assert.equal(taskBase, stackBase + stackBytes);
assert.equal(imageBase, taskBase + dataBytes);
assert.ok(dataBytes >= capCell + 8);
assert.equal(new Uint8Array(memory.buffer)[hostPage + 7], 173);
const output = new TextDecoder().decode(new Uint8Array(memory.buffer, x["out-base"](), x["out-len"]()));
assert.match(output, /^stack depth ok\n/);
assert.match(output, /static owner intact\n137\n/);
assert.match(output, /bounds ok\n/);
const lines = output.trimEnd().split("\n");
const dynamicBase = Number(lines.at(-1));
assert.ok(dynamicBase >= hostPage + 65536);
assert.equal(new Uint8Array(memory.buffer)[dynamicBase + 204799], 137);
console.log("task DATA, owner image, stack header and host growth ok");
