// A foreign Wasm/browser peer, not a Habu build driver. The checked display.f
// fixture drives the production turn protocol, CLI and (with --chrome) DOM.
import assert from "node:assert/strict";
import { readFileSync, mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
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
  ["occlude", /occlude 7 before any draw/],
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
  "--typed", "draw", "--typed", "unmesh", "--load", scene,
  "--focus", "00".repeat(15) + "Ff",
  "--highlight", ""], { stdout: "pipe", stderr: "pipe" });
const [stdout, stderr, code] = await Promise.all([
  new Response(cli.stdout).text(), new Response(cli.stderr).text(), cli.exited,
]);
writeFileSync(join(output, "cli.txt"), stdout);
assert.equal(code, 0, stderr);
assert.equal(stdout, `hello\ninput 0\nsvg ${original.length} ${Buffer.from(original).toString("hex")}\ninput 1\ndraw 0\n` +
  "mesh 1 6 2\nmesh 2 3 1\nmesh 3 3 1\nhighlight 2 0200000004000000\n" +
  "draw 5 1/1/0/0/ff7314/0/32768 2/2/0/0/30a040/65535/16384 " +
  "1/3/1/-/ffffff/0/0 1/4/0/-/2060c0/0/65535 3/5/0/-/30a040/0/0\n" +
  "draw 1 2/2/0/0/30a040/65535/16384\nunmesh 1\n" +
  "draw 1 2/2/0/0/30a040/65535/16384\n" +
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
  const child = Bun.spawn([chrome, "--headless=new", "--no-first-run", "--no-default-browser-check",
    "--enable-unsafe-webgpu", "--use-angle=swiftshader", "--disable-gpu-sandbox",
    "--remote-debugging-port=0", `--user-data-dir=${profile}`, "about:blank"],
    { detached: true, stdout: "ignore", stderr: Bun.file(join(output, "chrome.log")) });
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
        error: document.querySelector('#text').className, created, revoked, script: window.svgScript ?? 0 };
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
    const [orange, green, white, cobalt] = [[255, 115, 20], [48, 160, 64],
      [255, 255, 255], [32, 96, 192]];
    // The expected bytes: the paint, mixed 60% toward the mark when
    // selected, times the light of normal n.
    const light = (n) => 0.3 + 0.7 * Math.abs(
      (0.3 * n[0] + 0.5 * n[1] + 0.8 * n[2]) / Math.hypot(0.3, 0.5, 0.8));
    const shade = (paint, selected, n) => {
      let c = paint.map((v) => v / 255);
      if (selected) {
        const y = [0.2126, 0.7152, 0.0722]
          .reduce((sum, w, k) => sum + w * c[k], 0);
        const mark = y > 0.5 ? [0.05, 0.15, 0.6] : [0.6, 0.9, 1.0];
        c = c.map((v, k) => 0.4 * v + 0.6 * mark[k]);
      }
      return c.map((v) => Math.round(255 * v * light(n)));
    };
    const near = (got, want) => got.every((v, k) => Math.abs(v - want[k]) <= 2);
    const z = [0, 0, 1], tilted = [0, 1, -3].map((v) => v / Math.hypot(1, 3));
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
    assert.ok(near(face0, shade(orange, false, z)),
      `the orange square's face 0 is orange times its light, got ${face0}`);
    assert.ok(near(upper, shade(cobalt, false, z)),
      `the blue square, mesh 1 again, is blue times its light, got ${upper}`);
    assert.ok(near(face1, shade(orange, true, z)),
      `the orange square's highlighted face 1 is marked, got ${face1}`);
    for (const plain of [shade(orange, false, z), shade(white, false, z)])
      assert.ok(face1.some((v, k) => Math.abs(v - plain[k]) > 40),
        `the marked face ${face1} reads apart from ${plain}`);
    assert.ok(near(triangle, shade(green, true, tilted)),
      `the triangle's face 34, highlighted in word 1, is marked and lit ` +
      `by its normal through the inverse transpose, got ${triangle}`);
    assert.ok(near(flat, shade(green, false, z)),
      `the flat triangle is lit by its own normal, got ${flat}`);
    assert.ok(near(right, shade(white, true, z)),
      `the selected white square is marked, got ${right}`);
    assert.deepEqual(above, background,
      `the background is white, got ${above}`);
    console.log(`Headless Chrome pixels: orange face 0 ${face0}, blue ` +
      `${upper}, marked orange face 1 ${face1}, marked triangle ` +
      `${triangle}, marked white ${right}, background ${above}, flat ` +
      `triangle ${flat}`);
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
    const final = await state();
    assert.deepEqual(final.revoked, final.created.slice(0, -1).map(([url]) => url), "every replaced image URL must be revoked");
    await command("text");
    const shot = await call("Page.captureScreenshot", { format: "png" });
    writeFileSync(join(output, "display.png"), Buffer.from(shot.data, "base64"));
    writeFileSync(join(output, "dom.json"), JSON.stringify(await state(), null, 2));
    console.log("Headless Chrome: inert SVG rendering, replacement, " +
      "draw clearing, stale callbacks, errors, input control and tree " +
      "opening passed");
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
  } catch (error) {
    failure = error; throw error;
  } finally {
    try {
      releaseSlow();
      try { await stopGroup(child.pid, socket); await child.exited; } finally {
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

// Chrome removes its code-sign clone only when it closes itself, so teardown
// asks it to close and waits; it signals the group only if no socket opened.
async function stopGroup(group, socket) {
  if (socket?.readyState === WebSocket.OPEN) {
    socket.send(JSON.stringify({ id: 0, method: "Browser.close" }));
    while (signalGroup(group, 0)) await Bun.sleep(25);
    return;
  }
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

// Whether a member is left. Darwin answers EPERM, not ESRCH, while the group's
// last members have exited but are not yet reaped: members remain.
function signalGroup(group, signal) {
  try { process.kill(-group, signal); return true; }
  catch (error) { if (error.code === "ESRCH") return false; if (error.code === "EPERM") return true; throw error; }
}

async function until(check) {
  const limit = performance.now() + 15000;
  while (performance.now() < limit) { if (await check()) return; await Bun.sleep(25); }
  throw new Error("browser condition timed out");
}

// Chrome creates DevToolsActivePort before it writes the port into it, so the
// port is taken only from a whole first line.
async function devtoolsPort(active) {
  let port;
  await until(() => {
    try { port = /^(\d+)\n/.exec(readFileSync(active, "utf8"))?.[1]; }
    catch (error) { if (error.code !== "ENOENT") throw error; }
    return port !== undefined;
  });
  return port;
}
