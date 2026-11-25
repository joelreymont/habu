// A foreign Wasm/browser peer, not a Habu build driver. The checked display.f
// fixture drives the production turn protocol, CLI and (with --chrome) DOM.
// With --chrome the page is served under an MCP Apps host's image policy,
// img-src 'self' data:, so its SVG renders only from a data: URL; that SVG
// must run no script, fetch no external reference and follow no link.
// A mounted viewport's resize turn carries the page's devicePixelRatio in
// thousandths as its a, Math.round(devicePixelRatio * 1000) in the page.
import assert from "node:assert/strict";
import { readFileSync, mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { Module } from "../../src/lib/browser/turn.js";
import { devtoolsPort, launch, stopGroup, until } from "./chrome.mjs";

const [file, output, flag, chrome] = process.argv.slice(2);
if (!file || !output || (flag && (flag !== "--chrome" || !chrome)))
  throw new Error("usage: bun test/browser/display-peer.mjs MODULE OUTPUT [--chrome EXECUTABLE]");
const bytes = readFileSync(file);
mkdirSync(output, { recursive: true });
const records = [];
const host = {
  hello() {}, size: () => [640, 480], ratio: () => 1000,
  svg: (bytes) => records.push(["svg", bytes]),
  input: (enabled) => records.push(["input", enabled]),
  draw: () => records.push(["draw"]),
  ground: (plane) => records.push(["ground", plane]),
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
  ["occlude", /occlude 7 before any draw/],
  ["ground-c", /ground c must be zero/], ["ground-b", /a ground of 12 bytes, not 0 or 40/],
  ["ground-outside", /outside the module/], ["ground-nan", /not all finite/],
  ["ground-flat", /normal is \(0, 0, 0\)/], ["ground-far", /is not a finite f32/],
  ["ground-box", /lowest -0\.5, -0\.5, 2 above its highest 0\.5, 0\.5, 1/],
]) {
  const count = records.length;
  await assert.rejects(module.typed(command), reason, command);
  assert.equal(records.length, count, `${command} must not reach the host`);
}
await assert.rejects(module.focus(new Uint8Array(17)), /whole 16-byte identities/, "a partial identity");
// A delivery the module cannot reserve, memory32's whole 4 GiB, is rejected by
// reserve before memory grows, and the instance keeps running.
const memory = () => module.x.memory.buffer.byteLength;
let before = memory();
await assert.rejects(module.load(new Uint8Array(2 ** 32)),
  (e) => e instanceof RangeError && /cannot reserve 4294967296 bytes/.test(e.message), "an unreservable delivery");
assert.equal(memory(), before, "a rejected delivery must not grow memory");
// reserve answers 0 for an empty, negative, past-memory32 or past-bound count
// and leaves memory as it was.
for (const n of [0n, -1n, 2n ** 32n, 2n ** 32n + 1n]) assert.equal(module.x.reserve(n), 0n, `reserve(${n})`);
assert.equal(memory(), before, "a rejected reserve must not grow memory");
await module.load(Uint8Array.of(0, 255, 16));
assert.deepEqual(records.at(-1), ["text", "load 00ff10"], "a later delivery after a rejected one");
// Echoing 32 identities passes ECHO's buffer, so each of these turns throws -1
// and rejects with that throw's record above throw 85, the module's code for
// an uncaught throw; the run still frees its delivery.
const thrown = "error[E-THROW]: nothing caught a throw of code -1\n  = code: -1\n" +
  "  = help: catch the code where the program can go on, or remove its cause\n\n";
await module.focus(new Uint8Array(16));
before = memory();
for (let i = 0; i < 5; i++)
  await assert.rejects(module.focus(new Uint8Array(512)),
    (e) => e.constructor === Error && e.message === `${thrown}throw 85`, "an echo past its buffer");
assert.equal(memory(), before, "turns that throw must free their deliveries");
const logged = [];
const consoleError = console.error;
console.error = (...args) => logged.push(args);
try {
  await module.typed("warn");
} finally {
  console.error = consoleError;
}
assert.deepEqual(logged, [["warned"]], "a run that returns sends its error text to console.error");
await assert.rejects(module.typed("warn-throw"),
  (e) => e.constructor === Error && e.message === "warned\nthrow 93", "error text above a throw's code");
for (const [command, text] of [["die", "warned"], ["die-255", "\uFFFDboom"]]) {
  module = await Module.load(bytes, host);
  await module.start();
  await assert.rejects(module.typed(command), (e) => e.cause instanceof WebAssembly.RuntimeError &&
    e.message === `${text}\n${e.cause.message}`, command);
  assert.match(module.ended, /^the module trapped and has ended: Error: /, command);
  assert.throws(() => module.run(), (e) => e.message === module.ended, `${command}: a run after the trap`);
}
const scene = join(output, "scene.bin");
writeFileSync(scene, Uint8Array.of(0, 255, 16));
const cli = Bun.spawn([process.execPath, "src/lib/browser/host-cli.mjs", file, "--typed", "on", "--typed", "zero",
  "--typed", "draw", "--typed", "unmesh", "--typed", "seam", "--typed", "ground", "--typed", "ground-none",
  "--load", scene,
  "--focus", "00".repeat(15) + "Ff",
  "--highlight", ""], { stdout: "pipe", stderr: "pipe" });
const [stdout, stderr, code] = await Promise.all([
  new Response(cli.stdout).text(), new Response(cli.stderr).text(), cli.exited,
]);
writeFileSync(join(output, "cli.txt"), stdout);
assert.equal(code, 0, stderr);
assert.equal(stdout, `hello\ninput 0\nsvg ${original.length} ${Buffer.from(original).toString("hex")}\ninput 1\ndraw 0\n` +
  "mesh 1 6 2 0 0 0\nmesh 2 3 1 0 0 0\nmesh 3 3 1 0 0 0\nhighlight 2 0200000004000000\n" +
  "draw 5 1/1/0/0/ff7314/0/32768 2/2/0/0/30a040/65535/16384 " +
  "1/3/1/-/ffffff/0/0 1/4/0/-/2060c0/0/65535 3/5/0/-/30a040/0/0\n" +
  "draw 1 2/2/0/0/30a040/65535/16384\nunmesh 1\n" +
  "draw 1 2/2/0/0/30a040/65535/16384\n" +
  "mesh 4 4 2 100000 200000 300000\nmesh 5 4 2 139000 190000 299000\n" +
  "draw 2 4/8/0/-/ff7314/0/32768 5/9/0/-/ff7314/0/32768\n" +
  "ground 0 0 1 0 -0.5 -0.5 0 0.5 0.5 1\nground none\n" +
  `text load 00ff10\ntext focus ${"00".repeat(15)}ff\ntext highlight \n`);
console.log("SVG/input protocol, independent copy, rejected records, identities, rejected delivery, " +
  "error text, mesh origins and CLI passed");

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
      // An MCP Apps host's image policy: an SVG shown from any other source,
      // a blob: URL included, is blocked and fails the SVG cases.
      return new Response(page, { headers: { "Content-Type": "text/html",
        "Content-Security-Policy": "img-src 'self' data:" } });
    }
    if (path === "/peer-boot.js") return new Response(`
      import { Module } from '/turn.js';
      const load = Module.load;
      Module.load = async (...args) => window.subject = await load.call(Module, ...args);
      await import('/host.js');
    `, { headers: { "Content-Type": "text/javascript" } });
    if (path === "/mount") return new Response(
      '<!doctype html><canvas tabindex="0" style="width:320px;height:240px"></canvas><script type="module" src="/mount-boot.js"></script>',
      { headers: { "Content-Type": "text/html" } });
    // Counts each instance's turns, keeps each resize turn's a in ratios and
    // each WebGPU device, so a disposed viewport's listeners and device can be
    // checked. Keeps each matchMedia list, since emulating another ratio fires
    // no change event. breakPipeline makes the next device's pipeline throw;
    // ignoreAbort fetches without the signal.
    if (path === "/mount-boot.js") return new Response(`
      import { Module } from '/turn.js';
      import { mount } from '/viewport.js';
      window.modules = []; window.devices = []; window.records = []; window.lists = [];
      const media = window.matchMedia;
      window.matchMedia = (query) => { const list = media.call(window, query); lists.push(list); return list; };
      const load = Module.load;
      Module.load = async (...args) => { const m = await load.call(Module, ...args); modules.push(m); m.turns = 0; m.ratios = [];
        return m; };
      const turn = Module.prototype.turn;
      Module.prototype.turn = function (event) { this.turns++;
        if (event && event[0] === 9n) this.ratios.push(Number(event[1])); return turn.call(this, event); };
      const request = GPUAdapter.prototype.requestDevice;
      GPUAdapter.prototype.requestDevice = async function (...args) { const d = await request.apply(this, args);
        devices.push(d); d.lost.then((info) => { d.reason = info.reason; });
        if (window.breakPipeline) { breakPipeline = false; d.createRenderPipeline = () => { throw new Error('pipeline rejected'); }; }
        return d; };
      window.canvas = document.querySelector('canvas');
      window.mountOne = () => mountOn(canvas);
      window.mountOn = (target) => mount(target, { Module, module: '/module.wasm',
        fetch: (url, init) => fetch(url, window.ignoreAbort ? { ...init, signal: undefined } : init),
        onTree: (rows, selected) => records.push(['tree', rows, selected]),
        onText: (text, error) => records.push(['text', text, error]),
        onSvg: (bytes) => records.push(['svg', bytes && bytes.length]),
        onInput: (enabled) => records.push(['input', enabled]) });
      window.ready = true;
    `, { headers: { "Content-Type": "text/javascript" } });
    if (path === "/host.js" || path === "/viewport.js" || path === "/turn.js" || path === "/page.js" ||
        path === "/mcp.js")
      return new Response(Bun.file(join(root, path.slice(1))), { headers: { "Content-Type": "text/javascript" } });
    return new Response("not found", { status: 404 });
  } });
  const profile = mkdtempSync(join(tmpdir(), "habu-display-chrome-"));
  const child = launch(chrome, profile, join(output, "chrome.log"));
  let socket, failure;
  try {
    const port = await devtoolsPort(join(profile, "DevToolsActivePort"));
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
        error: document.querySelector('#text').className, script: window.svgScript ?? 0 };
    })()`);
    // The screenshot's RGB at each clip-space [x, y] of the canvas, as the
    // page decodes it.
    const pixels = async (points) => {
      const { data } = await call("Page.captureScreenshot", { format: "png" });
      return evaluate(`(async () => {
        const image = new Image();
        image.src = "data:image/png;base64,${data}";
        await image.decode();
        const r = document.querySelector("canvas").getBoundingClientRect();
        const scale = image.naturalWidth / innerWidth;
        const c = document.createElement("canvas");
        [c.width, c.height] = [image.naturalWidth, image.naturalHeight];
        const context = c.getContext("2d");
        context.drawImage(image, 0, 0);
        return ${JSON.stringify(points)}.map(([x, y]) => [
          (r.left + (x + 1) * r.width / 2) * scale,
          (r.top + (1 - y) * r.height / 2) * scale,
        ]).map(([x, y]) => Array.from(context.getImageData(
          Math.floor(x), Math.floor(y), 1, 1).data.slice(0, 3)));
      })()`);
    };
    // display.f's draw: the left square's face 0 and face 1 centres, the
    // triangle's centre, the right square's centre, the background above
    // the squares, right of the triangle, the upper square's face 0 centre
    // and the flat triangle's centre.
    const points = [[-0.6333, -0.1333], [-0.3667, 0.1333], [-0.6333, 0.7667],
      [0.5, 0], [0, 0.7], [0.3667, 0.7667], [-0.6333, -0.9333]];
    const background = [255, 255, 255];
    const start = await state();
    assert.equal(start.disabled, true, "startup must preserve input(false)");
    assert.deepEqual(start.natural, [400, 100]);
    assert.equal(start.visible, "hidden");
    assert.equal(start.fit, "contain");
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
    const portrait = await call("Page.captureScreenshot", { format: "png" });
    writeFileSync(join(output, "portrait.png"), Buffer.from(portrait.data, "base64"));
    await evaluate("oldError?.call(oldImage, new Event('error')); oldLoad?.call(oldImage, new Event('load'))");
    assert.equal((await state()).error, "", "stale callbacks must not report an error");
    await command("text"); await command("tree");
    assert.equal((await state()).image, blue.image, "text and tree records must leave the viewport alone");
    // A row whose more is true opens from its toggle: the module sees b = 1
    // and answers with the row's child Bracket and Bracket's child Bolt. The
    // rebuilt tree keeps the row open and its toggle focused, and Bracket,
    // whose child arrived with it, starts open.
    await command("more");
    const toggle = (label) =>
      `document.querySelector('#tree button[aria-label$=" ${label}"]')`;
    const shelf = toggle("Shelf");
    const expanded = (label) =>
      evaluate(`${toggle(label)}.getAttribute('aria-expanded')`);
    assert.equal(await expanded("Shelf"), "false",
      "a new row with more starts closed");
    await evaluate(`${shelf}.focus(); ${shelf}.click()`);
    await until(() => evaluate(
      "document.querySelector('#tree').textContent.includes('Bolt')"));
    assert.equal(
      await evaluate("document.querySelector('#text').textContent"), "opened",
      "the opening reaches the module with b = 1");
    assert.equal(await expanded("Shelf"), "true",
      "an opened row stays open across a changed tree");
    assert.equal(await evaluate(`document.activeElement === ${shelf}`), true,
      "the opened row's toggle keeps focus across the rebuild");
    assert.equal(await expanded("Bracket"), "true",
      "a new row whose children arrive with it starts open");
    await command("off");
    await evaluate("document.querySelector('input').value='red'; document.querySelector('form').dispatchEvent(new Event('submit',{cancelable:true}))");
    assert.equal((await state()).image, blue.image, "disabled form must not dispatch a typed turn");
    await command("on");
    assert.equal((await state()).disabled, false);
    await evaluate("document.querySelector('input').value='red'; document.querySelector('form').requestSubmit()");
    await until(() => evaluate("document.querySelector('img')?.naturalWidth === 400"));
    assert.equal(await evaluate("document.querySelector('input').value"), "");
    await command("draw");
    const drawn = await state();
    assert.equal(drawn.image, undefined, "nonempty draws also clear SVG");
    assert.equal(drawn.visible, "visible");
    assert.deepEqual(drawn.canvas, start.canvas);
    const [face0, face1, triangle, right, above, upper, flat] =
      await pixels(points);
    const apart = (a, b, by) => a.some((v, k) => Math.abs(v - b[k]) > by);
    assert.ok(face0[0] > face0[1] && face0[1] > face0[2] &&
      Math.max(...face0) > 60 && Math.min(...face0) < 200,
      `the orange square's face 0 keeps its hue, neither black nor washed ` +
      `out, got ${face0}`);
    assert.ok(flat[1] > flat[0] && flat[1] > flat[2],
      `the green flat triangle stays green, got ${flat}`);
    assert.ok(apart(face1, face0, 40),
      `the marked face 1 ${face1} reads apart from the unmarked face 0 ${face0}`);
    assert.ok(right[2] > right[1] && right[1] > right[0],
      `the selected white square takes the mark's blue, got ${right}`);
    assert.ok(triangle[2] > triangle[1] && triangle[1] > triangle[0],
      `the triangle's face 34, highlighted in word 1, takes the mark's blue, got ${triangle}`);
    assert.deepEqual(above, background,
      `the background is white, got ${above}`);
    // A row at y -0.8 across the flat triangle's slanted edge x + y = -1.3,
    // one point per canvas pixel: 4x multisampling blends an edge pixel.
    const step = 2 / drawn.canvas[2];
    const row = Array.from({ length: Math.ceil(0.2 / step) + 1 },
      (_, k) => [-0.6 + k * step, -0.8]);
    const edge = (await pixels(row)).find((c) => c.every((v, k) =>
      Math.min(flat[k], 255) + 10 <= v && v <= Math.max(flat[k], 255) - 10));
    assert.ok(edge, `a pixel on the flat triangle's edge lies between ` +
      `${flat} and the background`);
    // display.f's metal: the same draw with the orange square metallic.
    await command("metal");
    const [metal] = await pixels(points);
    assert.ok(apart(metal, face0, 30) && metal[0] > metal[1] &&
      metal[1] > metal[2],
      `the metal orange face 0 ${metal} reads apart from the dielectric ` +
      `${face0} and keeps its hue`);
    console.log(`Headless Chrome pixels: orange face 0 ${face0}, blue ` +
      `${upper}, marked orange face 1 ${face1}, marked metal triangle ` +
      `${triangle}, marked white ${right}, background ${above}, flat ` +
      `triangle ${flat}, its edge ${edge}, metal orange face 0 ${metal}`);
    // display.f's occlude against that draw: the box behind the left square
    // answers 0, the box below every instance 1.
    await command("occlude");
    const occluded = (await state()).text;
    assert.equal(occluded, "occluded 0001 7",
      `the hidden box fails and the open one passes, got ${occluded}`);
    // A box from past the near plane to behind the left square passes, and
    // so does one whose front face lies on the square's depth.
    await command("edges");
    const edges = (await state()).text;
    assert.equal(edges, "occluded 0101 8",
      `the box across the near plane and the flush box pass, got ${edges}`);
    // 4097 boxes, all below every instance but the last, behind the left
    // square.
    await command("many");
    const many = (await state()).text;
    assert.equal(many, "occluded 0100 4096 9",
      `4096 open boxes pass and the hidden last one fails, got ${many}`);
    console.log(`Headless Chrome occlude: ${occluded}, ${edges}, ${many}`);
    await evaluate(`(() => { window.texts = []; window.text = subject.host.text;
      subject.host.text = (s) => { texts.push(s); text(s); }; })()`);
    await command("pick");
    const picks = await evaluate("subject.host.text = text, texts");
    const [half, one] = [0x3F000000, 0x3F800000];
    assert.deepEqual(picks.toSorted(), [`picked 1 1 ${half} 1`,
      `picked 3 0 ${half} 2`, `picked -1 0 ${one} 3`,
      `picked -1 0 ${one} 4`].toSorted(),
      "each pick answers its pixel's instance, face and depth, or none; " +
      `the page's text, where a failed pick goes: ${(await state()).text}`);
    // z 0.5 through the matrix's 2z + 0.5 over w 2 is depth 0.75.
    await command("nearer");
    const nearer = (await state()).text;
    assert.equal(nearer, `picked 6 1 ${0x3F400000} 5`,
      `a pick through a moved matrix with w 2 answers the nearer instance, ` +
      `its face and its depth, got ${nearer}`);
    console.log(`Headless Chrome picks: ${picks.join("; ")}; ${nearer}`);
    // Mesh 1 drawn by no row, then unmeshed: only the triangle stays.
    await command("unmesh");
    const [left0, left1, alone, right1, over, top] = await pixels(points);
    const unmeshed = [left0, left1, right1, over, top];
    assert.deepEqual(unmeshed, unmeshed.map(() => background),
      `mesh 1's pixels are background, got ${JSON.stringify(unmeshed)}`);
    assert.deepEqual(alone, triangle,
      `the triangle is still drawn, got ${alone}`);
    console.log(`Headless Chrome pixels after unmesh: triangle ${alone}, ` +
      `mesh 1's and background ${JSON.stringify(unmeshed)}`);
    // A draw of 0 rows after a shown SVG shows the canvas, cleared.
    await command("red"); await command("zero");
    const zero = await state();
    assert.equal(zero.image, undefined); assert.equal(zero.visible, "visible");
    assert.deepEqual(zero.canvas, start.canvas);
    const cleared = await pixels(points);
    assert.deepEqual(cleared, points.map(() => background),
      `a draw of 0 rows clears to white, got ${JSON.stringify(cleared)}`);
    console.log(`Headless Chrome pixels after 0 rows: ` +
      JSON.stringify(cleared));
    // Nothing drawn hides nothing.
    await command("occlude");
    const open = (await state()).text;
    assert.equal(open, "occluded 0101 7",
      `after a draw of 0 rows both boxes pass, got ${open}`);
    console.log(`Headless Chrome occlude after 0 rows: ${open}`);
    await shadowed();
    await traced();
    await volumed();
    await seamed();
    await evaluate("oldError?.call(oldImage,new Event('error'))");
    assert.equal((await state()).error, "");
    await command("broken");
    await until(() => evaluate("document.querySelector('#text').className === 'error'"));
    assert.match((await state()).text, /SVG image/);
    await command("red");
    await until(() => evaluate("document.querySelector('img')?.naturalWidth === 400"));
    await evaluate("document.querySelector('img').click()");
    assert.equal((await state()).script, 0);
    assert.equal(requests.includes("/svg-external"), false, "SVG image must not fetch external references");
    assert.equal(requests.includes("/svg-link"), false, "SVG image must not navigate links");
    await command("text");
    const shot = await call("Page.captureScreenshot", { format: "png" });
    writeFileSync(join(output, "display.png"), Buffer.from(shot.data, "base64"));
    writeFileSync(join(output, "dom.json"), JSON.stringify(await state(), null, 2));
    console.log("Headless Chrome: inert SVG rendering, replacement, " +
      "draw clearing, stale callbacks, errors, input control and tree " +
      "opening passed");
    await mounted();

    // display.f's ground z = 0, +z up, under an orange unit box on it at x
    // and y -0.5 to 0.5, mesh 100 drawn by the host straight from the page,
    // seen straight down through x / 2, y / 2. The key, from the studio's
    // front left, casts the box's shadow up and right on screen.
    // boxes(m, list, flags) draws mesh 100's box as each of list's [lowest x,
    // y, z, highest x, y, z], a row each, with the rows' flags; box(m, k,
    // flags) draws the unit box scaled by k about the world's origin.
    async function shadowed() {
      const unitBox = [-0.5, -0.5, 0, 0.5, 0.5, 1];
      await evaluate(`(() => {
        // +-x, +-y and +-z as octahedral i16s.
        const octahedral = [[[-32767, 0], [32767, 0]], [[0, -32767], [0, 32767]],
          [[32767, 32767], [0, 0]]];
        const vertices = new Int16Array(24 * 6), triangles = new Uint16Array(36);
        for (let f = 0; f < 6; f++) {
          const [j, s] = [f >> 1, f & 1];
          [[0, 0], [1, 0], [1, 1], [0, 1]].forEach(([p, q], k) => {
            const g = [0, 0, 0];
            [g[j], g[(j + 1) % 3], g[(j + 2) % 3]] = [s, p, q];
            vertices.set([...g, 0, ...octahedral[j][s]], 6 * (4 * f + k));
          });
          triangles.set([0, 1, 2, 0, 2, 3].map((k) => 4 * f + k), 6 * f);
        }
        subject.host.mesh(100, 24, 12, [0, 0, 0], new Uint16Array(vertices.buffer), triangles);
        window.boxes = (m, list, flags = 0) => {
          const rows = list.map((b) => {
            const row = new DataView(new ArrayBuffer(72));
            [b[3] - b[0], 0, 0, 0, b[4] - b[1], 0, 0, 0, b[5] - b[2], b[0], b[1], b[2]]
              .forEach((v, j) => row.setFloat32(4 * j, v, true));
            row.setUint32(48, 100, true); row.setUint32(56, flags, true);
            row.setUint32(60, 0xFFFFFFFF, true);
            [0xff, 0x73, 0x14].forEach((v, j) => row.setUint8(64 + j, v));
            row.setUint16(70, 32768, true);
            return new Uint8Array(row.buffer);
          });
          subject.host.draw(Uint8Array.from(rows.flatMap((r) => Array.from(r))),
            rows.length, Float32Array.from(m));
        };
        window.box = (m, k = 1, flags = 0) =>
          boxes(m, [${JSON.stringify(unitBox)}.map((v) => k * v)], flags);
      })()`);
      const down = [0.5, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, -0.25, 0, 0, 0, 0.5, 1];
      // down orbited 180° about +z, and turned 180° about x to look up.
      const orbited = [-0.5, 0, 0, 0, 0, -0.5, 0, 0, 0, 0, -0.25, 0, 0, 0, 0.5, 1];
      const below = [0.5, 0, 0, 0, 0, -0.5, 0, 0, 0, 0, 0.25, 0, 0, 0, 0.5, 1];
      // In clip space: the floor in the box's shadow, its mirror through the
      // box's centre, a floor point far from both and the box's top.
      const points = [[0.2, 0.4], [-0.2, -0.4], [-0.5, -0.5], [0, 0]];
      // The shade is 190 by the studio's terms.
      const grey = (c) => c.every((v) => v >= 175 && v <= 205) &&
        Math.max(...c) - Math.min(...c) <= 20;
      await command("ground");
      await evaluate(`box(${JSON.stringify(down)})`);
      const shot = await call("Page.captureScreenshot", { format: "png" });
      writeFileSync(join(output, "shadow.png"), Buffer.from(shot.data, "base64"));
      const [shade, mirror, far, top] = await pixels(points);
      assert.ok(grey(shade), `the floor away from the key is in neutral grey shadow, got ${shade}`);
      assert.deepEqual(mirror, background, `its mirror on the key's side is white, got ${mirror}`);
      assert.deepEqual(far, background, `the floor far from the box is white, got ${far}`);
      assert.ok(top[0] > top[1] && top[1] > top[2], `the box keeps its orange, got ${top}`);
      const picked = await evaluate(`(() => {
        const [x, y] = ${JSON.stringify(points[0])}, c = document.querySelector("canvas");
        return subject.host.pick(BigInt(Math.floor((x + 1) * c.width / 2)),
          BigInt(Math.floor((1 - y) * c.height / 2))); })()`);
      assert.deepEqual(picked, [-1, 0, 0x3F800000], `a pick on the shadow answers none, got ${picked}`);
      // The canvas's pixel row through the shadow's floor point: its greys are
      // the traced rays', and from its darkest pixel out to each end the
      // penumbra brightens without a band or a step. A brightening V dips by
      // at most a grey: shade holds it in 255ths and the canvas in greys.
      const width = await evaluate(`document.querySelector("canvas").width`);
      const places = Array.from({ length: width }, (_, k) =>
        [(2 * k + 1) / width - 1, points[0][1]]);
      const row = await pixels(places);
      const rowRays = shares(places.map(([x, y]) => [2 * x, 2 * y]), [unitBox], [1, 0, 0]);
      const wrong = off(row, rowRays);
      assert.equal(wrong.length, 0, `the row's greys are the rays' ${wrong.map((k) =>
        greyed(rowRays[k]))}, got ${wrong.map((k) => row[k])} at pixels ${wrong}`);
      const darkest = row.reduce((d, c, k) => (c[0] < row[d][0] ? k : d), 0);
      let rise = 0, dip = 0;
      for (const step of [-1, 1])
        for (let k = darkest; k !== (step < 0 ? 0 : width - 1); k += step) {
          const [c, next] = [row[k], row[k + step]];
          rise = Math.max(rise, next[0] - c[0]);
          dip = Math.max(dip, c[0] - next[0]);
          assert.ok(next.every((v, j) => v >= c[j] - 1 && v - c[j] <= 6),
            `along the row out from pixel ${darkest}, pixel ${k} ${c} then ${next}`);
        }
      // The turntable keeps the key's place on screen as the camera orbits.
      await evaluate(`box(${JSON.stringify(orbited)})`);
      const [turned, turnedMirror] = await pixels(points);
      assert.ok(grey(turned) && Math.abs(turned[0] - shade[0]) <= 20,
        `orbited 180°, the shadow stays on the same screen side, got ${turned}`);
      assert.deepEqual(turnedMirror, background, `orbited, the mirror stays white, got ${turnedMirror}`);
      // The box, the casters' box and the camera scaled by 1e-12 draw the same
      // picture.
      const small = down.map((v, k) => (k < 12 ? v * 1e12 : v));
      await evaluate(`subject.host.ground(Float32Array.of(0, 0, 1, 0),
        Float32Array.from([-0.5, -0.5, 0, 0.5, 0.5, 1], (v) => v * 1e-12));
        box(${JSON.stringify(small)}, 1e-12)`);
      const [tiny] = await pixels(points);
      assert.ok(grey(tiny), `the box at 1e-12 scale casts the shadow, got ${tiny}`);
      await command("ground");
      // display.f's key against the box at half scale inside the casters'
      // box, the key's rays rising toward -x and -y: the camera sees the box
      // behind the caster, the key does not, both see the box beside it, and
      // the box across the light's near plane passes.
      await evaluate(`box(${JSON.stringify(down)}, 0.5)`);
      await command("key");
      const sights = (await state()).text;
      assert.equal(sights, "occluded 010303 10",
        `the key test fails behind the caster and passes beside it and around it, got ${sights}`);
      // Tilted 45° about x, the camera's right is down's, so the turntable
      // keeps the key where it was; were the studio the view's own frame, the
      // key would stand nearly overhead and leave this floor point, x 0.6 and
      // y 1, white.
      const h = Math.SQRT1_2;
      const tilted = [0.5, 0, 0, 0, 0, 0.5 * h, 0.2 * h, 0, 0, 0.5 * h, -0.2 * h, 0, 0, 0, 0.5, 1];
      await evaluate(`box(${JSON.stringify(tilted)})`);
      const [leaning] = await pixels([[0.3, 0.5 * h]]);
      assert.ok(grey(leaning), `tilted 45°, the floor behind the box is in shadow, got ${leaning}`);
      // From below, the floor point shadowed from above is at y -0.4.
      await evaluate(`box(${JSON.stringify(below)})`);
      const [under] = await pixels([[0.2, -0.4]]);
      assert.deepEqual(under, background, `a camera below the ground shows no shadow, got ${under}`);
      await command("key");
      const beneath = (await state()).text;
      assert.equal(beneath, "occluded 010101 10", `a draw of no shadow passes no key test, got ${beneath}`);
      // zoomed looks down at world x 0.15..0.65 and y 0.55..1.05, past the
      // box's y 0.5: its centre is the floor point shade read, which the box
      // outside the view shades as a cast-only row and which is white with
      // no row.
      const zoomed = [4, 0, 0, 0, 0, 4, 0, 0, 0, 0, -0.25, 0, -1.6, -3.2, 0.5, 1];
      await evaluate(`subject.host.draw(new Uint8Array(0), 0, Float32Array.from(${JSON.stringify(zoomed)}))`);
      const [bare] = await pixels([[0, 0]]);
      assert.deepEqual(bare, background, `the floor in view with no row is white, got ${bare}`);
      await evaluate(`box(${JSON.stringify(zoomed)}, 1, 2)`);
      const [outside] = await pixels([[0, 0]]);
      assert.ok(grey(outside), `a cast-only box outside the view shades the floor in view, got ${outside}`);
      // The cast-only box in view casts the shadow the drawn box casts and
      // leaves its own pixels as the ground's, a neutral grey or white, which
      // a pick passes through.
      await evaluate(`box(${JSON.stringify(down)}, 1, 2)`);
      const [castShade, , , castTop] = await pixels(points);
      assert.deepEqual(castShade, shade, `a cast-only box casts the drawn box's shadow, got ${castShade}`);
      assert.ok(castTop.every((v) => v === castTop[0]),
        `a cast-only box leaves the ground's neutral pixels, got ${castTop}`);
      const castPick = await evaluate(`(() => { const c = document.querySelector("canvas");
        return subject.host.pick(BigInt(Math.floor(c.width / 2)), BigInt(Math.floor(c.height / 2))); })()`);
      assert.deepEqual(castPick, [-1, 0, 0x3F800000], `a pick on a cast-only box answers none, got ${castPick}`);
      await command("ground-none");
      await command("ground");
      await command("key");
      const remade = (await state()).text;
      assert.equal(remade, "occluded 030303 10", `a shadow map made since the draw hides no box, got ${remade}`);
      await command("ground-none");
      await evaluate(`box(${JSON.stringify(down)})`);
      const [none] = await pixels(points);
      assert.deepEqual(none, background, `with no ground the floor is white, got ${none}`);
      console.log(`Headless Chrome ground: shadow ${shade}, mirror ${mirror}, far ${far}, ` +
        `box ${top}, row darkest ${row[darkest]} rising at most ${rise} and dipping ${dip}, orbited ${turned}, ` +
        `1e-12 scale ${tiny}, key ${sights}, tilted ${leaning}, ` +
        `from below ${under}, outside the view bare ${bare} and cast-only ${outside}, ` +
        `cast-only in view shade ${castShade} top ${castTop} pick ${castPick}, no ground ${none}`);
    }

    // The share V of the key's softbox the ground z = 0 sees at each [x, y]
    // past boxes, where the studio's right is right: 60 x 36 rays over the
    // softbox's half extents 0.45 and 0.27 about the key's direction.
    function shares(points, boxes, right) {
      const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
      const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0]];
      const unit = (a) => a.map((v) => v / Math.hypot(...a));
      const key = unit([-0.4, 0.6, 0.7]), across = unit(cross([0, 1, 0], key));
      const above = cross(key, across), up = [0, 0, 1], back = cross(right, up);
      const rays = [];
      for (let i = 0; i < 60; i++) for (let j = 0; j < 36; j++) {
        const [u, v] = [0.45 * ((2 * i + 1) / 60 - 1), 0.27 * ((2 * j + 1) / 36 - 1)];
        const d = key.map((k, n) => k + u * across[n] + v * above[n]);
        rays.push([0, 1, 2].map((n) => dot(d, [right[n], up[n], back[n]])));
      }
      const hits = (p, d, b) => {
        let [near, far] = [0, Infinity];
        for (let k = 0; k < 3; k++) {
          const [s, t] = [(b[k] - p[k]) / d[k], (b[3 + k] - p[k]) / d[k]];
          [near, far] = [Math.max(near, Math.min(s, t)), Math.min(far, Math.max(s, t))];
        }
        return near < far;
      };
      return points.map(([x, y]) =>
        1 - rays.filter((d) => boxes.some((b) => hits([x, y, 0], d, b))).length / rays.length);
    }

    // The grey, 0 to 255 unrounded, of ground that sees v of the softbox, as
    // viewport.js's shadow shades it: env straight up is 0.6, outside both
    // softboxes, and the key's irradiance there 3 times its direction's up.
    function greyed(v) {
      const key = 3 * 0.6 / Math.sqrt(1.01) / Math.PI;
      const c = (0.6 + key * v) / (0.6 + key);
      return 255 * (1.055 * c ** (1 / 2.4) - 0.055);
    }

    // The colours that are not the grey of their share v, 0.1 either way and
    // then one grey: each one's index.
    function off(colours, vs) {
      return vs.flatMap((v, k) => (colours[k].every((c) =>
        c >= greyed(Math.max(v - 0.1, 0)) - 1 && c <= greyed(Math.min(v + 0.1, 1)) + 1) ? [] : [k]));
    }

    // Box A 40 mm a side at the origin and box B 80 x 80 x 200 at x 38, y
    // -232, on the ground z = 0, drawn from the page and seen straight down,
    // the screen's right along x + y and 270 world units to clip's 1 up the
    // screen, so the key's rays rise toward -0.2631 x + 0.9648 y. Over a grid
    // of ground points at least 6 from both feet, the greys are the traced
    // rays', and at 27.98, 67.67, where rays see 0.371 of the softbox past A
    // and B, the grey is 217 within its 6 band.
    async function traced() {
      const A = [0, 0, 0, 40, 40, 40], B = [38, -232, 0, 118, -152, 200];
      const h = Math.SQRT1_2, point = [27.98, 67.67];
      const [, , width, height] = (await state()).canvas;
      const [sx, sy] = [height / width / 270, 1 / 270];
      const view = [h * sx, -h * sy, 0, 0, h * sx, h * sy, 0, 0, 0, 0, -1 / 400, 0,
        0, 0, 0.75, 1];
      const world = ([x, y]) => [h * (x / sx - y / sy), h * (x / sx + y / sy)];
      const places = [];
      for (let y = 2; y < height; y += 5) for (let x = 2; x < width; x += 5)
        places.push([(2 * x + 1) / width - 1, 1 - (2 * y + 1) / height]);
      const seen = [];
      for (const boxes of [[A, B], [A]]) {
        const all = [0, 1, 2].map((k) => Math.min(...boxes.map((b) => b[k])))
          .concat([3, 4, 5].map((k) => Math.max(...boxes.map((b) => b[k]))));
        await evaluate(`subject.host.ground(Float32Array.of(0, 0, 1, 0),
          Float32Array.from(${JSON.stringify(all)}));
          boxes(${JSON.stringify(view)}, ${JSON.stringify(boxes)})`);
        const here = places.filter((p) => { const [x, y] = world(p); return boxes.every((b) =>
          Math.hypot(Math.max(b[0] - x, x - b[3], 0), Math.max(b[1] - y, y - b[4], 0)) >= 6); });
        const rays = shares(here.map(world), boxes, [h, h, 0]);
        const colours = await pixels(here);
        const wrong = off(colours, rays);
        const shaded = rays.filter((v) => v < 1).length;
        assert.ok(shaded > 100 && wrong.length === 0, `under ${boxes.length} boxes the ` +
          `ground's greys are the rays' at ${shaded} shaded points, got ${wrong.length} wrong, first ${wrong.slice(0, 8).map((k) =>
          `${colours[k]} at ${world(here[k])} for ${rays[k]}`).join("; ")}`);
        const [at] = await pixels([[h * sx * (point[0] + point[1]), h * sy * (point[1] - point[0])]]);
        if (boxes.length === 2)
          assert.ok(at.every((v) => v >= 211 && v <= 224), `under A and B the grey at ` +
            `27.98, 67.67 is the rays' ${greyed(shares([point], boxes, [h, h, 0])[0])}, got ${at}`);
        seen.push(`${boxes.length} boxes ${shaded} shaded points, ${at} at 27.98, 67.67`);
      }
      const shot = await call("Page.captureScreenshot", { format: "png" });
      writeFileSync(join(output, "traced.png"), Buffer.from(shot.data, "base64"));
      await command("ground-none");
      await evaluate(`subject.host.draw(new Uint8Array(0), 0, Float32Array.from(${JSON.stringify(view)}));
        subject.host.unmesh(100)`);
      console.log(`Headless Chrome traced rays: the ground's greys are the rays', ${seen.join("; ")}`);
    }

    // Runs run, a function's source, on the page, and answers the last Light
    // uniform the host wrote, the width of each shadow map it made, the
    // plane of the last ground it was given and the page's text.
    function lit(run) {
      return evaluate(`(async () => {
        const write = GPUQueue.prototype.writeBuffer, make = GPUDevice.prototype.createTexture;
        const ground = subject.host.ground, light = [], maps = [];
        let plane = null;
        GPUQueue.prototype.writeBuffer = function (buffer, offset, data, ...rest) {
          if (buffer.size === 160 && buffer.usage & GPUBufferUsage.UNIFORM) light.push(Array.from(data));
          return write.call(this, buffer, offset, data, ...rest);
        };
        GPUDevice.prototype.createTexture = function (...args) {
          const texture = make.apply(this, args);
          if (texture.format === "depth32float") maps.push(texture.width);
          return texture;
        };
        subject.host.ground = (held, box) => { plane = held && Array.from(held); return ground(held, box); };
        try { await (${run})(); } finally {
          GPUQueue.prototype.writeBuffer = write; GPUDevice.prototype.createTexture = make;
          subject.host.ground = ground;
        }
        return { light: light.at(-1), maps, plane, text: document.querySelector("#text").textContent };
      })()`);
    }

    // The key's x and y axes, M's centre and its half sides in each, from the
    // Light uniform's first two rows, key x and y over a half side less the
    // centre's.
    function keyed(light) {
      const half = [0, 1].map((r) => 1 / Math.hypot(...[0, 4, 8].map((k) => light[k + r])));
      const axes = [0, 1].map((r) => [0, 4, 8].map((k) => light[k + r] * half[r]));
      const centre = [0, 1].map((r) => -light[12 + r] * half[r]);
      return { axes, centre, half };
    }

    // display.f's volume and volume-skewed: each sends a ground and draws 0
    // rows, and answers the planes and pixel of the key's volume
    // src/lib/browser/studio.f's KEY-VOLUME finds. The host fits its shadow
    // map to its own statement of that volume, viewport.js's keyVolume, so
    // each of the module's five planes is the host's: the ground the host
    // holds, then key x at least M's lowest, at most its highest, then key y
    // the same, M from the Light uniform; and its pixel is M's larger side
    // over the map's side, 1024 on every canvas, studio.f's SHADOW-SIDE. A
    // normal's components agree to 1e-6 of its length 1, the ground's offset
    // to 1e-6 of itself, a side's offset to 1e-6 of M's larger bound in that
    // axis and the pixel to 1e-6 of itself: the uniform is f32.
    async function volumed() {
      const float = (hex) => { const v = new DataView(new ArrayBuffer(8));
        v.setBigUint64(0, BigInt(`0x${hex}`)); return v.getFloat64(0); };
      const close = (a, b, scale) => Math.abs(a - b) <= 1e-6 * scale;
      const seen = [];
      for (const name of ["volume", "volume-skewed"]) {
        await command("ground-none");
        const { light, maps, plane, text } = await lit(`() => subject.typed(${JSON.stringify(name)})`);
        const [word, count, ...numbers] = text.split(" ");
        assert.equal(word, "volume", `${name} answers its volume, got ${text}`);
        assert.equal(count, "5", `${name}'s volume has five planes, got ${text}`);
        assert.equal(numbers.length, 21, `${name} answers 20 plane numbers and a pixel, got ${text}`);
        const module = numbers.map(float);
        assert.ok(light && maps.length === 1 && maps[0] === 1024 && plane,
          `${name} draws a shadow on one map 1024 square, got ` +
          `${light ? "a" : "no"} Light uniform, maps ${maps} and ground ${plane}`);
        const { axes, centre, half } = keyed(light);
        const n = Math.hypot(...plane.slice(0, 3));
        const side = (k, s) => [...axes[k].map((v) => s * v), -s * (centre[k] - s * half[k])];
        const host = [plane.map((v) => v / n), side(0, 1), side(0, -1), side(1, 1), side(1, -1)];
        const bound = [0, 1].map((k) => Math.abs(centre[k]) + half[k]);
        const scales = [Math.abs(host[0][3]), bound[0], bound[0], bound[1], bound[1]];
        host.forEach((h, p) => h.forEach((v, j) => {
          const m = module[4 * p + j];
          assert.ok(close(m, v, j < 3 ? 1 : Math.max(scales[p], Math.abs(m))),
            `${name}'s plane ${p} number ${j} is the host's ${v}, got ${m}`);
        }));
        const pixel = 2 * Math.max(...half) / maps[0];
        assert.ok(close(module[20], pixel, pixel), `${name}'s pixel is the host's ${pixel}, got ${module[20]}`);
        seen.push(`${name} M x ${centre[0] - half[0]}..${centre[0] + half[0]} ` +
          `y ${centre[1] - half[1]}..${centre[1] + half[1]} pixel ${module[20]}`);
      }
      await command("ground-none");
      console.log(`Headless Chrome key volume: the module's planes and pixel are the host's, ${seen.join("; ")}`);
    }

    // display.f's seam: meshes 4 and 5, one orange plane split along a shared
    // edge, reach its grid positions by different origins. Under the rotated
    // transform the plane's colour is uniform (one normal, an orthographic
    // matrix), so every canvas pixel along the edge, from 10% to 90% of it,
    // has that colour: a gap would blend in the white background. The edge's
    // grid y is +-20000 about the transform's centre, so its ends are
    // +-20000 2^-15 times R's y column (0.48, 0.6, 0.64), x and y by the
    // matrix's 0.6. The plane's colour is read 6 pixels to each side. Those
    // samples all lie within the box top shadowed draws, a uniform orange
    // square of NDC half side 0.25, so the plane is also read at grid x
    // 110000 and 170000 on the centre's y, 10000 inside each mesh's far side:
    // -+30000 2^-15 times R's x column (0.8, 0, -0.6), x by the matrix's 0.6,
    // NDC x -+0.44, y 0, outside that square.
    async function seamed() {
      await command("seam");
      const [, , width, height] = (await state()).canvas;
      const half = 0.6 * 20000 / 32768, end = [0.48 * half, 0.6 * half];
      const along = [end[0] * width / 2, -end[1] * height / 2];
      const length = Math.hypot(...along);
      const off = [-along[1] / length * 12 / width, -along[0] / length * 12 / height];
      const n = Math.ceil(0.8 * length);
      const edge = Array.from({ length: n + 1 }, (_, k) => -0.8 + 1.6 * k / n)
        .map((t) => [t * end[0], t * end[1]]);
      const sides = edge.flatMap(([x, y]) => [[x + off[0], y + off[1]], [x - off[0], y - off[1]]]);
      const inside = [-1, 1].map((s) => [s * 0.6 * 0.8 * 30000 / 32768, 0]);
      const seen = await pixels([...edge, ...sides, ...inside]);
      const [line, beside, within] = [seen.slice(0, edge.length),
        seen.slice(edge.length, edge.length + sides.length), seen.slice(edge.length + sides.length)];
      const plane = beside[0];
      const near = (a, b) => a.every((v, k) => Math.abs(v - b[k]) <= 2);
      assert.ok(plane[0] > plane[1] && plane[1] > plane[2],
        `the plane beside the edge is orange, got ${plane}`);
      within.forEach((c, k) => assert.ok(near(c, plane),
        `mesh ${4 + k} at NDC ${inside[k]} is the plane's ${plane}, got ${c}`));
      beside.forEach((c, k) => assert.ok(near(c, plane),
        `the plane ${k & 1 ? "after" : "before"} the edge at sample ${k >> 1} is ${plane}, got ${c}`));
      line.forEach((c, k) => assert.ok(near(c, plane),
        `the shared edge's pixel ${k} of ${n + 1} is the plane's ${plane}, got ${c}`));
      console.log(`Headless Chrome seam: ${n + 1} pixels along the edge of meshes with ` +
        `different origins, each the plane's ${plane}`);
    }

    // viewport.js alone on a page that owns only a canvas: mount, turns,

    // dispose, and a second mount of the same canvas.
    async function mounted() {
      await call("Page.navigate", { url: `http://127.0.0.1:${server.port}/mount` });
      await until(() => evaluate("window.ready === true"));
      const svgLength = original.length;
      // Under a ratio other than headless Chrome's 1, so a host answering a
      // constant 1000 fails. The ratio is cleared only once the viewport is
      // disposed: a viewport still mounted would resize at its next frame.
      const scale = (ratio) => call("Emulation.setDeviceMetricsOverride",
        { width: 0, height: 0, deviceScaleFactor: ratio, mobile: false });
      await scale(2);
      await evaluate("(async () => { window.first = await mountOne(); })()");
      assert.deepEqual(await evaluate("records"), [["input", true], ["input", false], ["svg", svgLength]],
        "mount enables input, then the start turn's records arrive");
      await until(() => evaluate("modules[0].ratios.length > 0"));
      assert.deepEqual(await evaluate("modules[0].ratios"), [2000],
        "the mount's resize carries devicePixelRatio 2 in thousandths");
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
      await call("Emulation.clearDeviceMetricsOverride");
      assert.deepEqual([await listeners("canvas"), await listeners("window"), await listeners("lists.at(-1)")], [0, 0, 0],
        "dispose removes every listener");
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
      // A ratio change reaches the module through the mount's matchMedia watch.
      assert.deepEqual(await evaluate("modules[1].ratios"), [1000]);
      await scale(2);
      await evaluate("lists.at(-1)?.dispatchEvent(new Event('change')), 0");
      await until(() => evaluate("modules[1].ratios.length === 2"))
        .catch(() => assert.fail("a devicePixelRatio change sends a resize turn"));
      assert.deepEqual(await evaluate("modules[1].ratios"), [1000, 2000],
        "a devicePixelRatio change to 2 sends a resize turn whose a is 2000");
      // A ratio change that keeps the drawing buffer's size, as browser zoom
      // on a canvas that fills its frame does, reaches the re-armed watch.
      await scale(2.001);
      await evaluate("lists.at(-1)?.dispatchEvent(new Event('change')), 0");
      await until(() => evaluate("modules[1].ratios.length === 3"))
        .catch(() => assert.fail("a devicePixelRatio change that keeps the buffer's size sends a resize turn"));
      assert.deepEqual(await evaluate("[modules[1].ratios, canvas.width, canvas.height]"), [[1000, 2000, 2001], 640, 480],
        "a devicePixelRatio change that keeps the buffer's size sends a resize turn and resizes no buffer");
      await evaluate("second.dispose()");
      await until(() => evaluate("devices[1].reason === 'destroyed'"));
      await call("Emulation.clearDeviceMetricsOverride");

      // A canvas holding another context is rejected before any device exists;
      // a setup step that throws once the device exists destroys it.
      const owned = await evaluate("devices.length");
      assert.equal(await evaluate(`(async () => { const c = document.createElement('canvas'); c.getContext('2d');
        try { await mountOn(c); return 'mounted'; } catch (e) { return String(e); } })()`),
        "Error: the canvas already has a context other than WebGPU");
      assert.equal(await evaluate("devices.length"), owned, "a rejected canvas requests no device");
      assert.equal(await evaluate("(async () => { window.breakPipeline = true; try { await mountOne(); return 'mounted'; } catch (e) { return String(e); } })()"),
        "Error: pipeline rejected");
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
      console.log("Headless Chrome: viewport mount, turns, load, focus, highlight, dispose, remount, rejected mounts and disposed replies passed");
    }
  } catch (error) {
    failure = error; throw error;
  } finally {
    try {
      releaseSlow();
      try { await stopGroup(child, socket); await child.exited; } finally {
        socket?.close();
        server.stop(true);
        rmSync(profile, { recursive: true });
      }
    } catch (error) {
      if (!failure) throw error;
      console.error("Chrome teardown after the failed check:", error);
    }
  }
}
