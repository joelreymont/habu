// Browser device peer: run the checked Wasm fixture through the real turn
// protocol, including requests from one run and from concurrent browser turns
// that finish in reverse order. Build test/browser/request.f first.
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
const posts = [];
const fetches = [];
const reported = [];
let textFailure = null;
let reportFailure = null;
let size = [640n, 480n];
const host = {
  hello() {},
  fetch(path) {
    return new Promise((resolve, reject) => fetches.push({ path, resolve, reject }));
  },
  post(path, body) {
    return new Promise((resolve, reject) => posts.push({ path, body, resolve, reject }));
  },
  request(method, path, body, id) {
    return new Promise((resolve, reject) => waiting.push({ method, path, body, id, resolve, reject }));
  },
  error(error) {
    if (reportFailure) throw reportFailure;
    reported.push(error);
  },
  text(value) {
    if (textFailure) textFailure(value);
    texts.push(value);
  },
  size() { return size; },
};

const parallelModule = await Module.load(image, host);
await parallelModule.start();
const parallel = parallelModule.typed("parallel");
assert.equal(waiting.length, 2, "both requests from one run must start before either answers");
assert.deepEqual(waiting.map(({ method, path, id }) => [method, path, id]), [
  ["POST", "/state", 9007199254740993n],
  ["GET", "/projects", 9007199254740994n],
]);
assert.deepEqual([...waiting[0].body], [0, 255]);
assert.deepEqual([...waiting[1].body], []);
waiting[1].resolve({ status: 404, bytes: new Uint8Array([3]) });
await new Promise((resolve) => setImmediate(resolve));
assert.deepEqual(texts, ["9007199254740994 404 640,480 3,"]);
assert.deepEqual([...waiting[0].body], [0, 255], "pending body survives reply memory growth");
size = [100n, 80n];
waiting[0].reject(new Error("lost"));
await parallel;
const lost = [...new TextEncoder().encode("Error: lost")].map((b) => `${b},`).join("");
assert.equal(texts[1], `9007199254740993 0 100,80 ${lost}`);
const fresh = parallelModule.typed("put");
assert.equal(waiting.at(-1).path, "/view");
waiting.at(-1).resolve({ status: 204, bytes: new Uint8Array() });
await fresh;
assert.equal(texts.at(-1), "9007199254740995 204 100,80 ");

const trapped = await Module.load(image, host);
await trapped.start();
const pending = trapped.typed("request");
const firstPending = waiting.at(-1);
await assert.rejects(trapped.typed("trap"), /Unreachable|memory access out of bounds/);
const sizeBefore = trapped.x.memory.buffer.byteLength;
const textCount = texts.length;
firstPending.resolve({ status: 200, bytes: new Uint8Array([1]) });
await pending;
assert.match(String(reported.at(-1)), /module trapped and has ended/);
await new Promise((resolve) => setImmediate(resolve));
assert.equal(trapped.x.memory.buffer.byteLength, sizeBefore);
assert.equal(texts.length, textCount);
waiting.length = 0;
texts.length = 0;
reported.length = 0;
size = [640n, 480n];

const earlyTrap = await Module.load(image, host);
await earlyTrap.start();
const earlyTurn = earlyTrap.typed("parallel");
const held = waiting.at(-2);
waiting.at(-1).resolve({ status: 471, bytes: new Uint8Array() });
await new Promise((resolve) => setImmediate(resolve));
assert.equal(reported.length, 1, "a trapped reply must be visible while its sibling waits");
assert.ok(reported[0] instanceof WebAssembly.RuntimeError);
assert.match(String(reported[0]), /Unreachable|memory access out of bounds/);
const beforeHeld = earlyTrap.x.memory.buffer.byteLength;
const beforeTexts = texts.length;
held.resolve({ status: 200, bytes: new Uint8Array([1]) });
await earlyTurn;
assert.equal(earlyTrap.x.memory.buffer.byteLength, beforeHeld);
assert.equal(texts.length, beforeTexts);
assert.equal(reported.length, 2);
assert.ok(reported[1] instanceof AggregateError);
assert.strictEqual(reported[1].errors[0], reported[0]);
waiting.length = 0;
reported.length = 0;

const loneReply = await Module.load(image, host);
await loneReply.start();
const loneTurn = loneReply.typed("request");
const loneError = new Error("one host callback");
textFailure = () => { throw loneError; };
waiting.at(-1).resolve({ status: 200, bytes: new Uint8Array() });
await new Promise((resolve) => setImmediate(resolve));
assert.equal(reported.length, 1);
assert.strictEqual(reported[0], loneError);
await loneTurn;
textFailure = null;
waiting.length = 0;
reported.length = 0;

const replyFailure = await Module.load(image, host);
await replyFailure.start();
const failedReplies = replyFailure.typed("parallel");
const invalid = waiting.at(-2);
const trap = waiting.at(-1);
invalid.resolve({ status: 470, bytes: new Uint8Array() });
await new Promise((resolve) => setImmediate(resolve));
assert.equal(reported.length, 1);
assert.match(reported[0].message, /record of kind 99/);
trap.resolve({ status: 471, bytes: new Uint8Array() });
await failedReplies;
assert.equal(reported.length, 2);
assert.ok(reported[1] instanceof AggregateError);
assert.equal(reported[1].errors.length, 2);
assert.strictEqual(reported[1].errors[0], reported[0]);
assert.ok(reported[1].errors[1] instanceof WebAssembly.RuntimeError);
// host.js displays String(error), so both reasons must survive that rendering.
assert.match(String(reported[1]), /record of kind 99/);
assert.match(String(reported[1]), /(?:Unreachable|memory access out of bounds)/);
assert.ok(replyFailure.ended instanceof WebAssembly.RuntimeError);
waiting.length = 0;
reported.length = 0;

const callbackFailure = await Module.load(image, host);
await callbackFailure.start();
const failedCallbacks = callbackFailure.typed("parallel");
const callbackErrors = [new Error("first host callback"), new Error("second host callback")];
textFailure = (value) => {
  if (value.startsWith("9007199254740993")) throw callbackErrors[0];
  if (value.startsWith("9007199254740994")) throw callbackErrors[1];
};
waiting.at(-2).resolve({ status: 200, bytes: new Uint8Array() });
await new Promise((resolve) => setImmediate(resolve));
assert.strictEqual(reported[0], callbackErrors[0]);
waiting.at(-1).resolve({ status: 200, bytes: new Uint8Array() });
await failedCallbacks;
assert.ok(reported[1] instanceof AggregateError);
assert.deepEqual(reported[1].errors, callbackErrors);
assert.match(String(reported[1]), /first host callback/);
assert.match(String(reported[1]), /second host callback/);
textFailure = null;
reported.length = 0;
const recoveredCallback = callbackFailure.typed("put");
waiting.at(-1).resolve({ status: 204, bytes: new Uint8Array() });
await recoveredCallback;
waiting.length = 0;
texts.length = 0;

const refusedReporter = await Module.load(image, host);
await refusedReporter.start();
const reporterTurn = refusedReporter.typed("request");
reportFailure = new Error("reporter failed");
waiting.at(-1).resolve({ status: 470, bytes: new Uint8Array() });
await assert.rejects(reporterTurn, (error) => error === reportFailure);
reportFailure = null;
waiting.length = 0;

const noReporter = await Module.load(image, { ...host, error: undefined });
await noReporter.start();
const waitingBefore = waiting.length;
await assert.rejects(noReporter.typed("request"), /host.error.*REQUEST/);
assert.equal(waiting.length, waitingBefore);

const legacyWithoutReporter = await Module.load(image, { ...host, error: undefined });
await legacyWithoutReporter.start();
const oldTurn = legacyWithoutReporter.typed("legacy post");
posts.at(-1).resolve({ status: 200, bytes: new Uint8Array() });
await oldTurn;
posts.length = 0;

const legacyFailure = await Module.load(image, host);
await legacyFailure.start();
const stoppedLegacy = legacyFailure.typed("legacy two");
const legacyError = new Error("legacy post failed");
posts.at(-1).reject(legacyError);
await assert.rejects(stoppedLegacy, (error) => error === legacyError);
assert.equal(fetches.length, 0, "a failed legacy reply stops the remaining legacy sequence");
posts.length = 0;

const interleaved = await Module.load(image, host);
await interleaved.start();
const mixed = interleaved.typed("interleaved");
assert.equal(waiting.length, 2, "a legacy record cannot hold a later tagged request");
assert.equal(posts.length, 1);
assert.deepEqual([posts[0].path, [...posts[0].body]], ["/legacy", [42]]);
waiting[1].resolve({ status: 200, bytes: new Uint8Array([1]) });
waiting[0].resolve({ status: 200, bytes: new Uint8Array([2]) });
posts[0].resolve({ status: 200, bytes: new Uint8Array([3]) });
await mixed;
waiting.length = 0;
posts.length = 0;
texts.length = 0;

const mixedFailure = await Module.load(image, host);
await mixedFailure.start();
const mixedTurn = mixedFailure.typed("interleaved");
waiting.at(-2).resolve({ status: 470, bytes: new Uint8Array() });
waiting.at(-1).resolve({ status: 200, bytes: new Uint8Array() });
posts.at(-1).resolve({ status: 202, bytes: new Uint8Array([3]) });
await new Promise((resolve) => setImmediate(resolve));
assert.equal(posts.at(-1).path, "/followup", "tagged failure cannot cancel legacy followup");
posts.at(-1).resolve({ status: 200, bytes: new Uint8Array([4]) });
await mixedTurn;
assert.match(reported.at(-1).message, /record of kind 99/);
reported.length = 0;
const recoveredMixed = mixedFailure.typed("put");
waiting.at(-1).resolve({ status: 204, bytes: new Uint8Array() });
await recoveredMixed;
waiting.length = 0;
posts.length = 0;
texts.length = 0;

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
  ["get body", /request body is not allowed/],
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
  "request POST /state 9007199254740994 2 00ff\n" +
  "text 9007199254740993 200 640,480 0,255,\n" +
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
const legacyOrder = Bun.spawnSync([
  process.execPath, "src/lib/browser/host-cli.mjs", wasm,
  "--typed", "legacy two", "--answer", "202", answer,
  "--answer", "203", answer, "--answer", "200", answer,
]);
assert.equal(legacyOrder.exitCode, 0, new TextDecoder().decode(legacyOrder.stderr));
assert.equal(new TextDecoder().decode(legacyOrder.stdout),
  "hello\npost /state 2 00ff\n" +
  "text 0 202 0,0 0,255,\npost /followup 1 2a\n" +
  "text 0 203 0,0 0,255,\nfetch /state\ntext 0,0\n");
const failed = Bun.spawnSync([
  process.execPath, "src/lib/browser/host-cli.mjs", wasm,
  "--typed", "request", "--fail",
]);
assert.equal(failed.exitCode, 0, new TextDecoder().decode(failed.stderr));
assert.equal(new TextDecoder().decode(failed.stdout),
  "hello\nrequest POST /state 9007199254740993 2 00ff\n" +
  `text 9007199254740993 0 640,480 ${[...new TextEncoder().encode("Error: /state: failed")].map((b) => `${b},`).join("")}\n`);
const failedReply = Bun.spawnSync([
  process.execPath, "src/lib/browser/host-cli.mjs", wasm,
  "--typed", "two", "--answer", "470", answer, "--answer", "471", answer,
]);
assert.equal(failedReply.exitCode, 1);
const stderr = new TextDecoder().decode(failedReply.stderr);
assert.match(stderr, /record of kind 99/);
assert.match(stderr, /Unreachable|memory access out of bounds/);
console.log(`request peer passed; CLI answer in ${dir}`);
