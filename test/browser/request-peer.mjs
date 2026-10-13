// Browser device peer: run the checked Wasm fixture through the real turn
// protocol, including concurrent browser turns whose requests finish in reverse
// order. Build test/browser/request.f with tools/wasm-build.f first.
import assert from "node:assert/strict";
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { Module } from "../../src/lib/browser/turn.js";

const wasm = process.argv[2];
if (!wasm) throw new Error("usage: bun test/browser/request-peer.mjs <module.wasm>");
const image = readFileSync(wasm);
const texts = [];
const waiting = [];
let size = [640n, 480n];
const host = {
  hello() {},
  request(method, path, body) {
    return new Promise((resolve) => waiting.push({ method, path, body, resolve }));
  },
  text(value) { texts.push(value); },
  size() { return size; },
};

const module = await Module.load(image, host);
await module.start();
const first = module.typed("request");
const second = module.typed("get");
assert.equal(waiting.length, 2);
assert.deepEqual(waiting.map(({ method, path }) => [method, path]),
  [["POST", "/state"], ["GET", "/projects"]]);
assert.deepEqual([...waiting[0].body], [0, 255]);
assert.deepEqual([...waiting[1].body], []);

waiting[1].resolve({ status: 404, bytes: new Uint8Array([3]) });
await second;
assert.deepEqual(texts, ["9007199254740994 404 640,480 3,"]);
assert.deepEqual([...waiting[0].body], [0, 255]);
size = [100n, 80n];
waiting[0].resolve({ status: 201, bytes: new Uint8Array([0, 255]) });
await first;
assert.deepEqual(texts, [
  "9007199254740994 404 640,480 3,",
  "9007199254740993 201 100,80 0,255,",
]);
const put = module.typed("put");
assert.deepEqual(waiting.at(-1).method, "PUT");
assert.deepEqual(waiting.at(-1).path, "/view");
assert.deepEqual([...waiting.at(-1).body], [0, 255]);
waiting.at(-1).resolve({ status: 204, bytes: new Uint8Array() });
await put;
assert.equal(texts.at(-1), "9007199254740995 204 100,80 ");

const bomPath = module.typed("bom path");
assert.equal(waiting.at(-1).method, "POST");
assert.equal(waiting.at(-1).path, "\uFEFF/state");
waiting.at(-1).resolve({ status: 200, bytes: new Uint8Array() });
await bomPath;

for (const [command, message] of [
  ["short header", /request header/],
  ["outside", /outside the module/],
  ["huge length", /request lengths/],
  ["bad length", /request lengths/],
  ["bad id", /positive request ID/],
  ["negative id", /positive request ID/],
  ["empty method", /request lengths/],
  ["bad token", /HTTP method token/],
  ["bad newline", /HTTP method token/],
  ["bad utf8", /Invalid byte sequence|encoded data was not valid/],
  ["empty path", /nonempty path/],
  ["bom method", /HTTP method token/],
  ["bom only method", /HTTP method token/],
]) {
  const test = await Module.load(image, host);
  await test.start();
  const count = waiting.length;
  const turn = test.typed(command);
  assert.equal(waiting.length, count, `${command} reached the HTTP host`);
  await assert.rejects(turn, message, command);
}

const dir = mkdtempSync(join(tmpdir(), "browser-request-"));
const answer = join(dir, "answer.bin");
writeFileSync(answer, new Uint8Array([0, 255]));
const run = Bun.spawnSync([
  process.execPath, "src/lib/browser/host-cli.mjs", wasm,
  "--typed", "request", "--answer", "409", answer,
]);
assert.equal(run.exitCode, 0, new TextDecoder().decode(run.stderr));
assert.equal(new TextDecoder().decode(run.stdout),
  "hello\nrequest POST /state 9007199254740993 2 00ff\n" +
  "text 9007199254740993 409 640,480 0,255,\n");
const ordered = Bun.spawnSync([
  process.execPath, "src/lib/browser/host-cli.mjs", wasm,
  "--typed", "two", "--answer", "200", answer, "--answer", "201", answer,
]);
assert.equal(ordered.exitCode, 0, new TextDecoder().decode(ordered.stderr));
assert.equal(new TextDecoder().decode(ordered.stdout),
  "hello\nrequest POST /state 9007199254740993 2 00ff\n" +
  "text 9007199254740993 200 640,480 0,255,\n" +
  "request POST /state 9007199254740994 2 00ff\n" +
  "text 9007199254740994 201 640,480 0,255,\n");
const legacyPost = Bun.spawnSync([
  process.execPath, "src/lib/browser/host-cli.mjs", wasm,
  "--typed", "legacy post", "--answer", "200", answer,
]);
assert.equal(legacyPost.exitCode, 0, new TextDecoder().decode(legacyPost.stderr));
assert.equal(new TextDecoder().decode(legacyPost.stdout),
  "hello\npost /state 2 00ff\ntext 0 200 0,0 0,255,\n");
const legacyFetch = Bun.spawnSync([
  process.execPath, "src/lib/browser/host-cli.mjs", wasm,
  "--typed", "legacy fetch", "--answer", "200", answer,
]);
assert.equal(legacyFetch.exitCode, 0, new TextDecoder().decode(legacyFetch.stderr));
assert.equal(new TextDecoder().decode(legacyFetch.stdout),
  "hello\nfetch /state\ntext 0,0\n");
const failed = Bun.spawnSync([
  process.execPath, "src/lib/browser/host-cli.mjs", wasm,
  "--typed", "request", "--fail",
]);
assert.equal(failed.exitCode, 1);
assert.equal(new TextDecoder().decode(failed.stdout),
  "hello\nrequest POST /state 9007199254740993 2 00ff\n");
assert.equal(new TextDecoder().decode(failed.stderr), "/state: failed\n");
console.log(`request peer passed; CLI answer in ${dir}`);
