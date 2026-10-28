// A foreign Wasm/browser peer, not a Habu build driver. The checked display.f
// fixture drives the production turn protocol, CLI and (with --chrome) DOM.
import assert from "node:assert/strict";
import { readFileSync, mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { Module } from "../../src/lib/browser/turn.js";

const [file, output, flag, chrome] = process.argv.slice(2);
if (!file || !output || (flag && (flag !== "--chrome" || !chrome)))
  throw new Error("usage: bun test/browser/display-peer.mjs MODULE OUTPUT [--chrome EXECUTABLE]");
const bytes = readFileSync(file);
mkdirSync(output, { recursive: true });
const records = [];
const host = {
  hello() {}, size: () => [640, 480],
  svg: (bytes) => records.push(["svg", bytes]),
  input: (enabled) => records.push(["input", enabled]),
  draw: () => records.push(["draw"]),
  tree() {}, text: (text) => records.push(["text", text]),
};
let module = await Module.load(bytes, host);
await module.start();
assert.deepEqual(records.map(([kind]) => kind), ["input", "svg"]);
assert.equal(records[0][1], false);
const copied = records[1][1];
const original = copied.slice();
// A later run can reuse its source buffer; growth also detaches memory views.
await module.typed("blue");
new Uint8Array(module.x.memory.buffer).fill(0);
module.x.memory.grow(1);
assert.deepEqual(copied, original, "SVG bytes must survive source mutation and memory growth");
module = await Module.load(bytes, host);
await module.start();
await module.typed("on");
assert.deepEqual(records.at(-1), ["input", true]);
await module.typed("off");
assert.deepEqual(records.at(-1), ["input", false]);
for (const [command, reason] of [
  ["empty", /positive/], ["negative", /positive/],
  ["outside", /outside the module/], ["past", /outside the module/],
  ["svg-reserved", /svg c must be zero/],
  ["flag", /input a must be 0 or 1/], ["negative-flag", /input a must be 0 or 1/],
  ["input-b", /input b and c must be zero/], ["input-c", /input b and c must be zero/],
]) {
  const count = records.length;
  await assert.rejects(module.typed(command), reason, command);
  assert.equal(records.length, count, `${command} must not reach the host`);
}
await assert.rejects(module.focus(new Uint8Array(17)), /whole 16-byte identities/, "a partial identity");
// A delivery the module cannot reserve, memory32's whole 4 GiB, is refused by
// reserve before memory grows, and the instance keeps running.
const memory = () => module.x.memory.buffer.byteLength;
let before = memory();
await assert.rejects(module.load(new Uint8Array(2 ** 32)),
  (e) => e instanceof RangeError && /cannot reserve 4294967296 bytes/.test(e.message), "an unreservable delivery");
assert.equal(memory(), before, "a refused delivery must not grow memory");
// reserve answers 0 for an empty, negative, past-memory32 or past-bound count
// and leaves memory as it was.
for (const n of [0n, -1n, 2n ** 32n, 2n ** 32n + 1n]) assert.equal(module.x.reserve(n), 0n, `reserve(${n})`);
assert.equal(memory(), before, "a refused reserve must not grow memory");
await module.load(Uint8Array.of(0, 255, 16));
assert.deepEqual(records.at(-1), ["text", "load 00ff10"], "a later delivery after a refused one");
// A run that throws still frees its delivery: echoing 32 identities passes
// ECHO's buffer, so each of these turns throws -1.
await module.focus(new Uint8Array(16));
before = memory();
for (let i = 0; i < 5; i++) await assert.rejects(module.focus(new Uint8Array(512)), /^Error: throw -1$/, "an echo past its buffer");
assert.equal(memory(), before, "turns that throw must free their deliveries");
const scene = join(output, "scene.bin");
writeFileSync(scene, Uint8Array.of(0, 255, 16));
const cli = Bun.spawn([process.execPath, "src/lib/browser/host-cli.mjs", file, "--typed", "on", "--typed", "zero",
  "--load", scene, "--focus", "00".repeat(15) + "Ff", "--highlight", ""], { stdout: "pipe", stderr: "pipe" });
const [stdout, stderr, code] = await Promise.all([
  new Response(cli.stdout).text(), new Response(cli.stderr).text(), cli.exited,
]);
writeFileSync(join(output, "cli.txt"), stdout);
assert.equal(code, 0, stderr);
assert.equal(stdout, `hello\ninput 0\nsvg ${original.length} ${Buffer.from(original).toString("hex")}\ninput 1\ndraw 0\n` +
  `text load 00ff10\ntext focus ${"00".repeat(15)}ff\ntext highlight \n`);
console.log("SVG/input protocol, independent copy, rejected records, identities, refused delivery and CLI passed");

if (chrome) await browser();

async function browser() {
  const root = resolve("src/lib/browser");
  const requests = [];
  // /slow answers only once the test releases it.
  let releaseSlow;
  const slow = new Promise((resolve) => { releaseSlow = resolve; });
  // One shared server for every DOM case; fixture data changes through turns.
  const server = Bun.serve({ hostname: "127.0.0.1", port: 0, fetch(request) {
    const path = new URL(request.url).pathname;
    requests.push(path);
    if (path === "/slow") return slow.then(() => new Response("late"));
    if (path === "/module.wasm") return new Response(bytes, { headers: { "Content-Type": "application/wasm" } });
    if (path === "/") {
      const page = readFileSync(join(root, "index.html"), "utf8").replace('src="host.js"', 'src="peer-boot.js"');
      return new Response(page, { headers: { "Content-Type": "text/html" } });
    }
    if (path === "/peer-boot.js") return new Response(`
      import { Module } from '/turn.js';
      const load = Module.load;
      Module.load = async (...args) => window.subject = await load.call(Module, ...args);
      const create = URL.createObjectURL;
      const revoke = URL.revokeObjectURL;
      window.created = []; window.revoked = [];
      URL.createObjectURL = (blob) => { const url = create.call(URL, blob); created.push([url, blob.type]); return url; };
      URL.revokeObjectURL = (url) => { revoked.push(url); revoke.call(URL, url); };
      await import('/host.js');
    `, { headers: { "Content-Type": "text/javascript" } });
    if (path === "/mount") return new Response(
      '<!doctype html><canvas tabindex="0" style="width:320px;height:240px"></canvas><script type="module" src="/mount-boot.js"></script>',
      { headers: { "Content-Type": "text/html" } });
    // Counts each instance's turns and keeps each WebGPU device, so a disposed
    // viewport's listeners and device can be checked. breakPipeline makes the
    // next device's pipeline throw; ignoreAbort fetches without the signal.
    if (path === "/mount-boot.js") return new Response(`
      import { Module } from '/turn.js';
      import { mount } from '/viewport.js';
      window.modules = []; window.devices = []; window.records = [];
      const load = Module.load;
      Module.load = async (...args) => { const m = await load.call(Module, ...args); modules.push(m); m.turns = 0; return m; };
      const turn = Module.prototype.turn;
      Module.prototype.turn = function (event) { this.turns++; return turn.call(this, event); };
      const request = GPUAdapter.prototype.requestDevice;
      GPUAdapter.prototype.requestDevice = async function (...args) { const d = await request.apply(this, args);
        devices.push(d); d.lost.then((info) => { d.reason = info.reason; });
        if (window.breakPipeline) { breakPipeline = false; d.createRenderPipeline = () => { throw new Error('pipeline refused'); }; }
        return d; };
      window.canvas = document.querySelector('canvas');
      window.mountOne = () => mountOn(canvas);
      window.mountOn = (target) => mount(target, { module: '/module.wasm',
        fetch: (url, init) => fetch(url, window.ignoreAbort ? { ...init, signal: undefined } : init),
        onTree: (rows, selected) => records.push(['tree', rows, selected]),
        onText: (text, error) => records.push(['text', text, error]),
        onSvg: (bytes) => records.push(['svg', bytes && bytes.length]),
        onInput: (enabled) => records.push(['input', enabled]) });
      window.ready = true;
    `, { headers: { "Content-Type": "text/javascript" } });
    if (path === "/host.js" || path === "/viewport.js" || path === "/turn.js")
      return new Response(Bun.file(join(root, path.slice(1))), { headers: { "Content-Type": "text/javascript" } });
    return new Response("not found", { status: 404 });
  } });
  const profile = mkdtempSync(join(tmpdir(), "habu-display-chrome-"));
  const child = Bun.spawn([chrome, "--headless=new", "--no-first-run", "--no-default-browser-check",
    "--enable-unsafe-webgpu", "--use-angle=swiftshader", "--disable-gpu-sandbox",
    "--remote-debugging-port=0", `--user-data-dir=${profile}`, "about:blank"],
    { detached: true, stdout: "ignore", stderr: Bun.file(join(output, "chrome.log")) });
  let socket;
  try {
    const active = join(profile, "DevToolsActivePort");
    await until(() => Bun.file(active).exists());
    const port = readFileSync(active, "utf8").split("\n")[0];
    const target = await (await fetch(`http://127.0.0.1:${port}/json/new?about:blank`, { method: "PUT" })).json();
    socket = new WebSocket(target.webSocketDebuggerUrl);
    await new Promise((resolve, reject) => { socket.onopen = resolve; socket.onerror = reject; });
    let id = 0;
    const pending = new Map();
    socket.onmessage = ({ data }) => {
      const result = JSON.parse(data);
      if (result.id) { const { resolve, reject } = pending.get(result.id); pending.delete(result.id);
        result.error ? reject(new Error(JSON.stringify(result.error))) : resolve(result.result); }
    };
    const call = (method, params = {}) => new Promise((resolve, reject) => {
      pending.set(++id, { resolve, reject }); socket.send(JSON.stringify({ id, method, params }));
    });
    const evaluate = async (expression) => {
      const result = await call("Runtime.evaluate", { expression, awaitPromise: true, returnByValue: true });
      if (result.exceptionDetails) throw new Error(JSON.stringify(result.exceptionDetails));
      return result.result.value;
    };
    const command = (text) => evaluate(`subject.typed(${JSON.stringify(text)})`);
    await call("Page.enable");
    await call("Page.navigate", { url: `http://127.0.0.1:${server.port}/` });
    await until(() => evaluate("!!window.subject && !!document.querySelector('img')?.complete"))
      .catch(async (error) => { throw new Error(await evaluate("document.querySelector('#text')?.textContent"), { cause: error }); });
    const state = () => evaluate(`(() => {
      const canvas = document.querySelector('canvas'), image = document.querySelector('img');
      const r = canvas.getBoundingClientRect();
      return { disabled: document.querySelector('input').disabled, canvas: [r.width, r.height, canvas.width, canvas.height],
        visible: getComputedStyle(canvas).visibility, image: image?.src, natural: image && [image.naturalWidth, image.naturalHeight],
        fit: image && getComputedStyle(image).objectFit, text: document.querySelector('#text').textContent,
        error: document.querySelector('#text').className, created, revoked, script: window.svgScript ?? 0 };
    })()`);
    const start = await state();
    assert.equal(start.disabled, true, "startup must preserve input(false)");
    assert.deepEqual(start.natural, [400, 100]);
    assert.equal(start.visible, "hidden");
    assert.equal(start.fit, "contain");
    assert.equal(start.created[0][1], "image/svg+xml");
    assert.equal(start.script, 0);
    const pixel = await evaluate(`(() => { const c = document.createElement('canvas'); c.width=400; c.height=100;
      const ctx=c.getContext('2d'); ctx.drawImage(document.querySelector('img'),0,0);
      return Array.from(ctx.getImageData(200,50,1,1).data); })()`);
    assert.deepEqual(pixel, [214, 40, 40, 255], "the real browser must decode and render the SVG");
    await evaluate("window.oldImage=document.querySelector('img'); window.oldError=oldImage.onerror; window.oldLoad=oldImage.onload");
    await command("blue");
    await until(() => evaluate("document.querySelector('img')?.naturalHeight === 400"));
    const blue = await state();
    assert.deepEqual(blue.canvas, start.canvas, "SVG replacement must retain canvas layout and buffer size");
    assert.deepEqual(blue.revoked, [start.image]);
    const portrait = await call("Page.captureScreenshot", { format: "png" });
    writeFileSync(join(output, "portrait.png"), Buffer.from(portrait.data, "base64"));
    await evaluate("oldError?.call(oldImage, new Event('error')); oldLoad?.call(oldImage, new Event('load'))");
    assert.equal((await state()).error, "", "stale callbacks must not report an error");
    await command("text"); await command("tree");
    assert.equal((await state()).image, blue.image, "text and tree records must leave the viewport alone");
    await command("off");
    await evaluate("document.querySelector('input').value='red'; document.querySelector('form').dispatchEvent(new Event('submit',{cancelable:true}))");
    assert.equal((await state()).image, blue.image, "disabled form must not dispatch a typed turn");
    await command("on");
    assert.equal((await state()).disabled, false);
    await evaluate("document.querySelector('input').value='red'; document.querySelector('form').requestSubmit()");
    await until(() => evaluate("document.querySelector('img')?.naturalWidth === 400"));
    assert.equal(await evaluate("document.querySelector('input').value"), "");
    await command("zero");
    const zero = await state();
    assert.equal(zero.image, undefined); assert.equal(zero.visible, "visible");
    assert.deepEqual(zero.canvas, start.canvas);
    await evaluate("oldError?.call(oldImage,new Event('error'))");
    assert.equal((await state()).error, "");
    await command("red"); await command("draw");
    assert.equal((await state()).image, undefined, "nonempty draws also clear SVG");
    await command("broken");
    await until(() => evaluate("document.querySelector('#text').className === 'error'"));
    assert.match((await state()).text, /SVG image/);
    await command("red");
    await until(() => evaluate("document.querySelector('img')?.naturalWidth === 400"));
    await evaluate("document.querySelector('img').click()");
    assert.equal((await state()).script, 0);
    assert.equal(requests.includes("/svg-external"), false, "SVG image must not fetch external references");
    assert.equal(requests.includes("/svg-link"), false, "SVG image must not navigate links");
    const final = await state();
    assert.deepEqual(final.revoked, final.created.slice(0, -1).map(([url]) => url), "every replaced image URL must be revoked");
    await command("text");
    const shot = await call("Page.captureScreenshot", { format: "png" });
    writeFileSync(join(output, "display.png"), Buffer.from(shot.data, "base64"));
    writeFileSync(join(output, "dom.json"), JSON.stringify(await state(), null, 2));
    console.log("Headless Chrome: inert SVG rendering, replacement, draw clearing, stale callbacks, errors and input control passed");
    await mounted();

    // viewport.js alone on a page that owns only a canvas: mount, turns,
    // dispose, and a second mount of the same canvas.
    async function mounted() {
      await call("Page.navigate", { url: `http://127.0.0.1:${server.port}/mount` });
      await until(() => evaluate("window.ready === true"));
      const svgLength = original.length;
      await evaluate("(async () => { window.first = await mountOne(); })()");
      assert.deepEqual(await evaluate("records"), [["input", true], ["input", false], ["svg", svgLength]],
        "mount enables input, then the start turn's records arrive");
      assert.equal(await evaluate("canvas.style.visibility"), "hidden");
      await evaluate("first.post({ kind: 'typed', text: 'draw' })");
      await evaluate("first.post({ kind: 'typed', text: 'text' })");
      assert.deepEqual((await evaluate("records")).slice(3), [["svg", null], ["text", "still here", false]],
        "a draw removes the shown SVG");
      assert.equal(await evaluate("canvas.style.visibility"), "visible");
      // Scenes and identities reach the module as load, focus and highlight
      // events; display.f echoes each one's bytes in a text record.
      const ids = ["0123456789abcdefFEDCBA9876543210", "00".repeat(15) + "ff"];
      await evaluate("first.load(new TextEncoder().encode('scene'))");
      await evaluate("first.load(Uint8Array.of(0, 255).buffer)");
      await evaluate(`first.focus(${JSON.stringify(ids)})`);
      await evaluate("first.highlight([])");
      assert.deepEqual((await evaluate("records")).slice(5), [
        ["text", "load 7363656e65", false], ["text", "load 00ff", false],
        ["text", `focus ${ids.join("").toLowerCase()}`, false], ["text", "highlight ", false],
      ], "load, focus and highlight carry their bytes to the module");
      const turns = await evaluate("modules[0].turns");
      for (const bad of ["['0123']", `['${"g".repeat(32)}']`, `['${ids[1]}', 7]`, `[, '${ids[1]}']`, "'" + ids[1] + "'"])
        assert.match(await evaluate(`(() => { try { first.highlight(${bad}); } catch (e) { return String(e); } })()`),
          /TypeError: identit/, bad);
      assert.equal(await evaluate("modules[0].turns"), turns, "a malformed identity list runs no turn");
      const key = "(canvas.dispatchEvent(new KeyboardEvent('keydown', { key: 'a' })), modules[0].turns)";
      const before = await evaluate("modules[0].turns");
      assert.equal(await evaluate(key), before + 1, "a key on the mounted canvas runs a turn");
      const listeners = async (expression) => {
        const { result } = await call("Runtime.evaluate", { expression });
        return (await call("DOMDebugger.getEventListeners", { objectId: result.objectId })).listeners.length;
      };
      assert.ok(await listeners("canvas") > 0 && await listeners("window") > 0, "a mounted viewport listens");
      await evaluate("first.dispose()");
      await until(() => evaluate("devices[0].reason !== undefined"));
      assert.equal(await evaluate("devices[0].reason"), "destroyed", "dispose releases the GPU device");
      assert.deepEqual([await listeners("canvas"), await listeners("window")], [0, 0], "dispose removes every listener");
      assert.equal(await evaluate(key), before + 1, "a key on a disposed canvas runs no turn");
      assert.equal(await evaluate("canvas.dispatchEvent(new PointerEvent('pointerdown', { pointerId: 1 })), modules[0].turns"),
        before + 1, "a pointer on a disposed canvas runs no turn");
      assert.match(await evaluate("(() => { try { first.post({ kind: 'typed', text: 'draw' }); } catch (e) { return String(e); } })()"),
        /disposed/);
      assert.equal((await evaluate("records")).length, 9, "no callback runs after dispose");
      await evaluate("(async () => { window.second = await mountOne(); await second.post({ kind: 'typed', text: 'draw' }); })()");
      assert.deepEqual((await evaluate("records")).slice(9),
        [["input", true], ["input", false], ["svg", svgLength], ["svg", null]], "a second mount runs a fresh module");
      assert.equal(await evaluate("modules.length === 2 && devices.length === 2 && modules[1].turns"), 3,
        "the second mount has its own module and device, and ran start, resize and draw");
      await evaluate("second.dispose()");
      await until(() => evaluate("devices[1].reason === 'destroyed'"));

      // A canvas holding another context is refused before any device exists;
      // a setup step that throws once the device exists destroys it.
      const owned = await evaluate("devices.length");
      assert.equal(await evaluate(`(async () => { const c = document.createElement('canvas'); c.getContext('2d');
        try { await mountOn(c); return 'mounted'; } catch (e) { return String(e); } })()`),
        "Error: the canvas already has a context other than WebGPU");
      assert.equal(await evaluate("devices.length"), owned, "a refused canvas requests no device");
      assert.equal(await evaluate("(async () => { window.breakPipeline = true; try { await mountOne(); return 'mounted'; } catch (e) { return String(e); } })()"),
        "Error: pipeline refused");
      await until(() => evaluate(`devices[${owned}].reason === 'destroyed'`))
        .catch(() => assert.fail("a mount that fails after requesting its device destroys it"));

      // A fetch outstanding at dispose fails into no callback, and every
      // disposed viewport's module is then collected.
      const sent = () => requests.filter((path) => path === "/slow").length;
      await evaluate("(async () => { window.third = await mountOne(); })()");
      const heard = (await evaluate("records")).length;
      await evaluate("third.post({ kind: 'typed', text: 'fetch' }), 0");
      await until(() => sent() === 1);
      await evaluate(`third.dispose(); delete window.first; delete window.second; delete window.third;
        window.collected = modules.map((m) => new WeakRef(m)); modules.length = 0`);
      await until(async () => { await call("HeapProfiler.collectGarbage"); return evaluate("collected.every((w) => !w.deref())"); })
        .catch(async () => assert.fail(`disposed modules alive: ${await evaluate("JSON.stringify(collected.map((w) => !!w.deref()))")}`));
      assert.equal((await evaluate("records")).length, heard, "an aborted fetch reaches no callback");

      // A reply that arrives after dispose runs no turn.
      await evaluate("(async () => { window.ignoreAbort = true; window.fourth = await mountOne(); })()");
      await evaluate("window.late = fourth.post({ kind: 'typed', text: 'fetch' }), 0");
      await until(() => sent() === 2);
      const ran = await evaluate("fourth.dispose(), modules[0].turns");
      releaseSlow();
      await evaluate("late");
      assert.equal(await evaluate("modules[0].turns"), ran, "a late reply runs no turn");
      assert.equal((await evaluate("records")).length, heard + 3, "a late reply reaches no callback");
      console.log("Headless Chrome: viewport mount, turns, load, focus, highlight, dispose, remount, refused mounts and disposed replies passed");
    }
  } finally {
    releaseSlow(); socket?.close(); await stopGroup(child.pid); await child.exited; server.stop(true);
    const cleanup = Bun.spawn(["trash", profile], { stdout: "ignore", stderr: "inherit" });
    assert.equal(await cleanup.exited, 0, "Chrome profile cleanup");
  }
}

// Chrome leads its own process group: a renderer can outlive the browser
// process, so teardown signals the group and waits until no member is left.
async function stopGroup(group) {
  for (const signal of ["SIGTERM", "SIGKILL"]) {
    if (!signalGroup(group, signal)) return;
    const limit = performance.now() + 5000;
    while (performance.now() < limit) {
      if (!signalGroup(group, 0)) return;
      await Bun.sleep(25);
    }
  }
  throw new Error(`Chrome process group ${group} survived SIGKILL`);
}

function signalGroup(group, signal) {
  try { process.kill(-group, signal); return true; }
  catch (error) { if (error.code === "ESRCH") return false; throw error; }
}

async function until(check) {
  const limit = performance.now() + 15000;
  while (performance.now() < limit) { if (await check()) return; await Bun.sleep(25); }
  throw new Error("browser condition timed out");
}
