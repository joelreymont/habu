// A foreign browser peer standing in for ChatGPT's MCP Apps host and for the
// server's view socket, not a Habu build driver. It serves a parent page that
// iframes PAGE, a page composed by MCP-APP:PAGE+, under ChatGPT's view CSP with
// the socket's origin as a connectDomains entry, and plays the host bridge in
// the parent: it answers the view's requests from a table, view_code with the
// code.json beside PAGE, MCP-APP:CODE+ of test/browser/app.f's module, and
// view_socket with the url of the peer's /view, records each message, and
// sends a tool-input and a tool-result notification before it answers
// ui/initialize, so the view holds both until its start turn. /view is a
// WebSocket endpoint: it sends view/code {code} first, answers each tools/call
// as the table does, and on the test's cue pushes a notification, view/alive
// among them, or closes the socket; for a session told to, it closes each of
// its first sockets right after it opens, or answers view_code with a tool
// error.
// Headless Chrome then checks the handshake, whose ui/initialize declares the
// inline and fullscreen display modes and which asks for no display mode, the
// view_code call, view.js's full-screen request after it and the
// view_socket ask, that both notifications reach the module in the order
// sent, that the tree gives way to the canvas in the 507 px frame, opened over
// it by its toggle with a row pressed while it was closed in view, and is
// docked at 900 px, that each button of the module's toolbar reaches the
// module as a key turn, that the zoom buttons show only over a drawing, where
// "+" scales it by 1.25, "f" fits it and a drag pans it with no key turn sent,
// and once the drawing is gone the toolbar's "+" sends its key again, that a message from a window
// other than the parent is ignored, a tools/call round trip over the socket,
// the model context, an after tick, a
// response holding both a result and an error, a dropped file read in two
// pieces, teardown, a second frame torn down before its ui/initialize is
// answered, a third torn down while its view_code request is pending, a
// fourth whose view_code answer names another page id, a fifth whose started
// view restarts with a second view_code result, handing the second view what
// it carried, and a sixth whose restart's result names another page id. In a
// seventh frame, app.js runs the server's view.js and socket.js: its first
// start asks for full screen after view_code, a tool call is answered over
// the socket while the host sees only view_code and
// view_socket, neither view/code nor a pushed view/alive reaches the module
// and a pushed notification does, view/resume is kept and answered {}, a
// close answers the outstanding call -32000, the binary message the server
// sent for it reaching no module, and the view reopens after its
// backoff, a view/code naming other code fetches view_code over the socket,
// then notifies the old module view/restart and, once that turn has settled,
// restarts the view through app.js, carrying the resume the module told in it
// and the session, the new canvas has the focus the old one had, the
// restarted module gets a call's binary messages after its response's JSON,
// in the order sent, a file dropped on the restarted view reaches its module
// once and no listener of the old view throws, and a close under 4403 makes
// the view ask view_socket again before it reopens, and the restarted view
// asks for no display mode. An eighth frame's sockets are closed
// right after they open: the delays between its reopens double, and its
// first view/alive resets them to 1 s. A ninth frame, once running, is pushed
// a view/code naming other code, and its view_code over the socket is
// answered with a tool error: the view runs on and answers its module's next
// call. A tenth frame's host context lists only the inline display mode and
// its view_socket is answered with a tool error: its view asks for no display
// mode, its module's tools/call is answered -32000 with that error's text,
// and it makes no socket. DIR holds
// data.bin, the file dropped, and receives chrome.log;
// TITLE is the page's unescaped title.
import assert from "node:assert/strict";
import { readFileSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { devtoolsPort, launch, stopGroup, until } from "./chrome.mjs";

const [file, output, title, flag, chrome] = process.argv.slice(2);
if (!file || !output || !title || flag !== "--chrome" || !chrome)
  throw new Error("usage: bun test/browser/app-peer.mjs PAGE DIR TITLE --chrome EXECUTABLE");
const page = readFileSync(file);
const meta = JSON.parse(readFileSync(join(dirname(file), "code.json"), "utf8"));
const CODE_B = "b".repeat(64);
const code = JSON.stringify(meta);
// ChatGPT's view CSP, the socket's origin added as connectDomains adds it.
const csp = (host) => `script-src 'self' 'wasm-unsafe-eval' 'unsafe-inline' 'unsafe-eval' blob:; worker-src blob:; connect-src 'self' ws://${host}; img-src 'self' data:`;
const INPUT = { arguments: { name: "Zürich" } };
const PARAMS = { structuredContent: { name: "Zürich ✓" } };
const STRANGER = { structuredContent: { name: "stranger" } };
const LATER = { structuredContent: { name: "after the stranger" } };
const TOOL_RESULT = { content: [{ type: "text", text: "project" }] };
const NO_SOCKET = "the server has no view socket";
const SOCKET_ERROR = { content: [{ type: "text", text: NO_SOCKET }], isError: true };
const CODE_ERROR = { content: [{ type: "text", text: "the view's code could not be read" }], isError: true };
const OLDER = `This view's page is older than ${title}'s server: ${title}'s app must be refreshed where it was installed; then open this view again.`;
const OLDER_CONTEXT = { content: [{ type: "text", text: `${title}'s view could not start: ${OLDER}`,
  annotations: { audience: ["assistant"] } }] };
// The restarting frames' view.js files: the first records what start was
// handed, asks for view_code again and restarts with its result, carrying
// CARRIED; the second shows that it started and what it was handed.
const CARRIED = { from: "first" };
const FIRST = `export async function start(host) {
  const first = window.first = { meta: host.meta, carried: host.carried, disposed: false };
  host.request("tools/call", { name: "view_code", arguments: {} })
    .then((r) => host.restart(r.result, ${JSON.stringify(CARRIED)}));
  return { notify() {}, dispose() { first.disposed = true; } };
}`;
const SECOND = `export async function start({ carried }) {
  document.body.innerHTML = '<p id="text"></p>';
  document.querySelector("#text").textContent = "second view " + JSON.stringify(carried);
  return { notify() {}, dispose() {} };
}`;
const SECOND_SHOWN = `second view ${JSON.stringify(CARRIED)}`;
const base64 = (text) => Buffer.from(text).toString("base64");

// The parent: every message from the frame lands in window.log; requests are
// answered from the table, view_code with code and view_socket with the url of
// the frame's session, whose ask names the session's count of asks. Before it
// answers ui/initialize it starts recording into window.shown each text an
// element #text in the frame's body is given, view.html's included once it
// replaces the body, and sends the two notifications. stranger posts a message
// to the frame from a new sibling frame, so its source is not the frame's
// parent. While both is set, the next ui/update-model-context is answered with
// a result and an error.
const parent = `<!doctype html><meta charset="utf-8"><iframe src="/app.html" style="width:507px;height:700px"></iframe>
<script>
const frame = document.querySelector("iframe");
const answers = {
  "ui/initialize": { protocolVersion: "2026-01-26", hostInfo: { name: "app-peer", version: "1" },
    hostCapabilities: {}, hostContext: {} },
  "ui/request-display-mode": { mode: "fullscreen" },
  "tools/call": ${JSON.stringify(TOOL_RESULT)},
  "ui/update-model-context": {},
};
const code = ${code};
const asks = {};
const answer = ({ method, params }, session = "main") =>
  method === "tools/call" && params.name === "view_code" ? { content: [], _meta: code } :
  method === "tools/call" && params.name === "view_socket" ?
    { content: [], _meta: { url: "ws://" + location.host + "/view?session=" + session +
      "&ask=" + (asks[session] = (asks[session] ?? 0) + 1), seconds: 86400 } } :
  answers[method];
// Records into into each text an element #text in doc's body is given.
const record = (doc, into) => new MutationObserver((records) => {
  for (const r of records) if (r.target.id === "text") for (const n of r.addedNodes) into.push(n.textContent);
}).observe(doc.body, { childList: true, subtree: true });
window.log = [];
window.replies = [];
window.shown = [];
window.both = false;
window.send = (message) => frame.contentWindow.postMessage({ jsonrpc: "2.0", ...message }, "*");
window.stranger = (message) => {
  const other = document.body.appendChild(document.createElement("iframe"));
  other.contentWindow.Function("w", "m", "w.postMessage(m, '*')")(frame.contentWindow, { jsonrpc: "2.0", ...message });
};
window.addEventListener("message", ({ source, data }) => {
  if (source !== frame.contentWindow) return;
  if (!("method" in data)) { replies.push(data); return; }
  log.push(data);
  if (data.method === "ui/initialize") {
    record(frame.contentDocument, shown);
    send({ method: "ui/notifications/tool-input", params: ${JSON.stringify(INPUT)} });
    send({ method: "ui/notifications/tool-result", params: ${JSON.stringify(PARAMS)} });
  }
  if (both && data.method === "ui/update-model-context") { both = false; send({ id: data.id, result: answers[data.method], error: { code: -32603, message: "both" } }); }
  else if ("id" in data) send({ id: data.id, result: answer(data) });
});
// addTorn adds a page frame and answers its requests from the table, but to
// a request of method it sends a teardown, then answer, then a ping whose
// reply marks that the answer was handled. The messages the frame
// posts land in window[name].log, its replies in window[name].replies. second
// is torn down on ui/initialize, pending on view_code, answered with another
// page id.
window.addTorn = (name, method, answer) => {
  const torn = window[name] = { log: [], replies: [] };
  const other = document.body.appendChild(document.createElement("iframe"));
  other.src = "/app.html";
  const send = (message) => other.contentWindow.postMessage({ jsonrpc: "2.0", ...message }, "*");
  window.addEventListener("message", ({ source, data }) => {
    if (source !== other.contentWindow) return;
    if (!("method" in data)) { torn.replies.push(data); return; }
    torn.log.push(data);
    if (!("id" in data)) return;
    if (data.method !== method) { send({ id: data.id, result: answers[data.method] }); return; }
    send({ id: 1, method: "ui/resource-teardown", params: {} });
    send({ id: data.id, result: answer });
    send({ id: 2, method: "ping" });
  });
};
// addOlder adds a page frame whose view_code answer names another page id;
// the messages it posts land in older.log.
window.older = { log: [], frame: null };
window.addOlder = () => {
  const other = older.frame = document.body.appendChild(document.createElement("iframe"));
  other.src = "/app.html";
  window.addEventListener("message", ({ source, data }) => {
    if (source !== other.contentWindow || !("method" in data)) return;
    older.log.push(data);
    if (!("id" in data)) return;
    const result = data.method === "tools/call" ? { content: [], _meta: { ...code, page: "0".repeat(64) } } : answers[data.method];
    other.contentWindow.postMessage({ jsonrpc: "2.0", id: data.id, result }, "*");
  });
};
// addRestart adds a page frame whose first view_code answer is code with
// FIRST as its view.js, and whose second is next; the messages it posts land
// in window[name].log.
const coded = (view, meta) => ({ content: [], _meta: { ...code, ...meta, files: { ...code.files, "view.js": view } } });
window.firstCode = coded(${JSON.stringify(base64(FIRST))}, {});
window.secondCode = coded(${JSON.stringify(base64(SECOND))}, {});
window.olderCode = coded(${JSON.stringify(base64(SECOND))}, { page: "0".repeat(64) });
window.addRestart = (name, next) => {
  const other = document.body.appendChild(document.createElement("iframe"));
  const restarted = window[name] = { log: [], frame: other };
  let calls = 0;
  other.src = "/app.html";
  window.addEventListener("message", ({ source, data }) => {
    if (source !== other.contentWindow || !("method" in data)) return;
    restarted.log.push(data);
    if (!("id" in data)) return;
    const result = data.method !== "tools/call" ? answers[data.method] : calls++ === 0 ? firstCode : next;
    other.contentWindow.postMessage({ jsonrpc: "2.0", id: data.id, result }, "*");
  });
};
// addLive adds a page frame answered as the table does, view_socket for the
// session name, or with failure when it is given, and ui/initialize with
// context as its hostContext when it is given; the messages it posts land
// in window[name].log, each text its #text is given in window[name].shown,
// and each error its window reports in window[name].errors.
window.addLive = (name, failure, context) => {
  const other = document.body.appendChild(document.createElement("iframe"));
  const live = window[name] = { log: [], shown: [], errors: [], frame: other };
  other.src = "/app.html";
  window.addEventListener("message", ({ source, data }) => {
    if (source !== other.contentWindow || !("method" in data)) return;
    live.log.push(data);
    if (data.method === "ui/initialize") {
      record(other.contentDocument, live.shown);
      other.contentWindow.addEventListener("error", (e) => live.errors.push(e.message));
    }
    if (!("id" in data)) return;
    const result = failure && data.method === "tools/call" && data.params.name === "view_socket" ?
      failure : context && data.method === "ui/initialize" ? { ...answers[data.method], hostContext: context } :
      answer(data, name);
    other.contentWindow.postMessage({ jsonrpc: "2.0", id: data.id, result }, "*");
  });
};
</script>`;

// The view socket's side: the count of requests to /view, and per session its
// messages, its open socket, its opens, the ask its last open's url names, the
// times of its last open and of the last close the peer made, the code
// view/code names, whether the next tools/call closes the socket
// unanswered, and the binary messages it sends before its answer; for the
// sessions named, how many more sockets close right after
// they open, the ms from each close the peer made to the next open, and the
// tool error view_code is answered with.
const view = { requests: 0, log: { main: [], live: [], flaky: [], erring: [] }, sockets: {},
  opens: { main: 0, live: 0, flaky: 0, erring: 0 }, asks: {}, openedAt: {}, closedAt: {},
  codes: { main: meta.code, live: meta.code, flaky: meta.code, erring: meta.code },
  hold: { main: false, live: false, flaky: false, erring: false },
  binary: { main: [], live: [], flaky: [], erring: [] },
  shut: { flaky: 2 }, reopens: { flaky: [] }, codeErrors: { erring: CODE_ERROR } };
const drop = (session) => {
  view.closedAt[session] = performance.now();
  view.sockets[session].close(4000, "dropped");
};
const push = (session, method, params) =>
  view.sockets[session].send(JSON.stringify({ jsonrpc: "2.0", method, params }));
const websocket = {
  open(ws) {
    const { session, ask } = ws.data;
    view.sockets[session] = ws;
    view.opens[session]++;
    view.asks[session] = ask;
    view.openedAt[session] = performance.now();
    if (view.reopens[session] && session in view.closedAt)
      view.reopens[session].push(view.openedAt[session] - view.closedAt[session]);
    push(session, "view/code", { code: view.codes[session] });
    if (view.shut[session] > 0) { view.shut[session]--; drop(session); }
  },
  message(ws, text) {
    const { session } = ws.data;
    const m = JSON.parse(text);
    view.log[session].push(m);
    if (m.method !== "tools/call") {
      ws.send(JSON.stringify({ jsonrpc: "2.0", id: m.id, error: { code: -32601, message: "Method not found" } }));
      return;
    }
    for (const piece of view.binary[session].splice(0)) ws.send(piece);
    if (view.hold[session]) { view.hold[session] = false; drop(session); return; }
    const result = m.params.name !== "view_code" ? TOOL_RESULT :
      view.codeErrors[session] ?? { content: [], _meta: { ...meta, code: view.codes[session] } };
    ws.send(JSON.stringify({ jsonrpc: "2.0", id: m.id, result }));
  },
  close(ws) {
    if (view.sockets[ws.data.session] === ws) delete view.sockets[ws.data.session];
  },
};

const server = Bun.serve({ hostname: "127.0.0.1", port: 0, websocket, fetch(request, server) {
  const url = new URL(request.url);
  const path = url.pathname;
  const html = (body) => new Response(body, { headers: { "Content-Type": "text/html; charset=utf-8",
    "Content-Security-Policy": csp(url.host) } });
  if (path === "/") return new Response(parent, { headers: { "Content-Type": "text/html; charset=utf-8" } });
  if (path === "/app.html") return html(page);
  if (path === "/view") {
    view.requests++;
    const session = url.searchParams.get("session");
    if (!Object.hasOwn(view.codes, session)) return new Response("unknown session", { status: 403 });
    if (server.upgrade(request, { data: { session, ask: url.searchParams.get("ask") } })) return undefined;
    return new Response("not a WebSocket request", { status: 400 });
  }
  return new Response("not found", { status: 404 });
} });
const profile = mkdtempSync(join(tmpdir(), "habu-app-chrome-"));
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
  const text = () => evaluate("frames[0]?.document.querySelector('#text')?.textContent ?? null");
  const shows = (want) => until(async () => await text() === want)
    .catch(async (error) => { throw new Error(`#text is ${JSON.stringify(await text())}, not ${JSON.stringify(want)}`, { cause: error }); });
  const submit = (words) => evaluate(`(() => { const f = frames[0].document.querySelector('form input');
    f.value = ${JSON.stringify(words)}; f.form.requestSubmit(); })()`);
  const typed = async (words, want) => { await submit(words); await shows(want); };
  await call("Page.enable");
  await call("Page.navigate", { url: `http://127.0.0.1:${server.port}/` });
  // Page.navigate answers once the navigation commits, before the page's
  // script has defined the globals read below.
  await until(async () => await evaluate("typeof shown") === "object");

  // The host's notifications; the socket's reach the module beside them.
  const notes = async () => (await evaluate("shown")).filter((t) => t.startsWith("notified ui/"));
  const input = `notified ui/notifications/tool-input ${JSON.stringify(INPUT)}`;
  const result = `notified ui/notifications/tool-result ${JSON.stringify(PARAMS)}`;
  await until(async () => (await notes()).length === 2);
  assert.deepEqual(await notes(), [input, result], "the held notifications reach the module in the order sent");
  const methods = () => evaluate("log.map((m) => m.method)");
  const handshake = ["ui/initialize", "ui/notifications/initialized", "tools/call"];
  const viewStart = ["ui/request-display-mode", "tools/call"];
  assert.deepEqual(await methods(), [...handshake, ...viewStart],
    "the handshake: initialize, then initialized, then view_code, then view.js's display-mode request, then view_socket");
  const [init, , coded, mode, asked] = await evaluate("log");
  assert.deepEqual(init.params, { appInfo: { name: title, version: "1" },
    appCapabilities: { availableDisplayModes: ["inline", "fullscreen"] }, protocolVersion: "2026-01-26" });
  assert.deepEqual(mode.params, { mode: "fullscreen" });
  assert.deepEqual(coded.params, { name: "view_code", arguments: {} });
  assert.deepEqual(asked.params, { name: "view_socket", arguments: {} });
  console.log(`Headless Chrome: handshake ${(await methods()).join(", ")}`);
  // The socket's view/open lands before the checks below read #text.
  const opened = "notified view/open {}";
  await until(async () => (await evaluate("shown")).includes(opened));
  // #text keeps its height whatever it holds, so the canvas keeps its size.
  const heights = await evaluate(`(() => { const d = frames[0].document, t = d.querySelector('#text');
    const sized = (s) => { t.textContent = s; return d.querySelector('canvas').clientHeight; };
    const hs = ["", "one", "1\\n2\\n3\\n4\\n5"].map(sized); return hs; })()`);
  assert.ok(heights[0] > 0 && heights.every((h) => h === heights[0]),
    `the canvas keeps its height with an empty #text, one line and five: ${heights.join(", ")}`);

  // Under 720 px the tree gives way: the canvas spans the frame, and the
  // toggle lays the tree over the canvas's left without resizing it. At 720 px
  // and wider the tree is docked and the toggle hidden.
  const layout = () => evaluate(`(() => { const w = frames[0], d = w.document, side = d.querySelector('#side');
    const toggle = d.querySelector('#tree-toggle'), c = d.querySelector('canvas').getBoundingClientRect();
    const s = side.getBoundingClientRect(), t = toggle?.getBoundingClientRect();
    const shown = side.checkVisibility({ visibilityProperty: true });
    return { frame: d.documentElement.clientWidth, side: shown ? 'visible' : 'hidden', sideLeft: s.left,
      sideWidth: s.width, left: c.left, width: c.width, height: c.height,
      toggle: toggle && w.getComputedStyle(toggle).display, expanded: toggle?.getAttribute('aria-expanded'),
      hit: !!toggle && d.elementFromPoint(t.left + t.width / 2, t.top + t.height / 2) === toggle }; })()`);
  const clickToggle = () => evaluate("frames[0].document.querySelector('#tree-toggle').click()");
  const narrow = await layout();
  assert.equal(narrow.frame, 507);
  assert.equal(narrow.side, "hidden", "in a 507 px frame the tree is hidden");
  assert.ok(narrow.left === 0 && narrow.width === narrow.frame,
    `in a 507 px frame the canvas spans it: ${JSON.stringify(narrow)}`);
  assert.ok(narrow.toggle !== "none" && narrow.expanded === "false", `the tree's toggle is shown, closed: ${JSON.stringify(narrow)}`);
  await clickToggle();
  const unfolded = await layout();
  assert.ok(unfolded.side === "visible" && unfolded.sideLeft === 0 && unfolded.sideWidth >= 240 && unfolded.expanded === "true",
    `the toggle opens the tree at the frame's left: ${JSON.stringify(unfolded)}`);
  assert.deepEqual([unfolded.left, unfolded.width, unfolded.height], [narrow.left, narrow.width, narrow.height],
    "the open tree lies over the canvas, which keeps its size");
  assert.ok(unfolded.hit, `the toggle lies above the open tree, so a click closes it: ${JSON.stringify(unfolded)}`);
  await clickToggle();
  const closed = await layout();
  assert.ok(closed.side === "hidden" && closed.expanded === "false", `the toggle closes the tree: ${JSON.stringify(closed)}`);
  // A row pressed while the tree is closed is scrolled into view, so it shows
  // once the toggle opens the tree.
  const pressed = "frames[0].document.querySelector('#tree button[aria-pressed=\"true\"]')";
  await submit("tree");
  await until(async () => await evaluate(`${pressed}?.textContent ?? null`) === "40");
  await clickToggle();
  const seen = await evaluate(`(() => { const side = frames[0].document.querySelector('#side');
    const r = ${pressed}.getBoundingClientRect(), b = side.getBoundingClientRect(), top = b.top + side.clientTop;
    return { scrolled: side.scrollTop, top: r.top, bottom: r.bottom, lo: top, hi: top + side.clientHeight }; })()`);
  assert.ok(seen.scrolled > 0 && seen.top >= seen.lo && seen.bottom <= seen.hi,
    `a row pressed below the closed tree's fold is in view once it opens: ${JSON.stringify(seen)}`);
  await clickToggle();
  const widen = (px) => evaluate(`document.querySelector('iframe').style.width = '${px}px'`);
  await widen(900);
  await until(async () => (await layout()).frame === 900);
  const wide = await layout();
  assert.ok(wide.side === "visible" && wide.toggle === "none", `in a 900 px frame the tree is docked and the toggle hidden: ${JSON.stringify(wide)}`);
  assert.ok(wide.left >= wide.sideWidth && wide.left + wide.width === wide.frame,
    `in a 900 px frame the canvas sits beside the tree: ${JSON.stringify(wide)}`);
  await widen(507);
  await until(async () => (await layout()).width === narrow.width);
  console.log(`Headless Chrome: a 507 px frame's canvas is ${narrow.width} px wide and its toggled tree lies over it; a 900 px frame docks the tree beside a ${wide.width} px canvas`);

  // The module's toolbar gives the keys: each button's press reaches the
  // module as a key turn of its row's key. #keys, the page's own zoom of a
  // drawing, shows only while one stands in the canvas's place: over it "+"
  // scales it by 1.25 and "f" fits it, a drag pans it, and no key turn
  // reaches the module.
  await submit("toolbar");
  const toolButtons = "frames[0].document.querySelectorAll('#toolbar button')";
  await until(() => evaluate(`${toolButtons}.length === 3`));
  const keys = ["+", "-", "f"];
  assert.deepEqual(await evaluate(`[...${toolButtons}].map((b) => b.getAttribute('aria-label'))`),
    ["Zoom in", "Zoom out", "Fit"]);
  for (const [i, key] of keys.entries()) {
    await evaluate(`${toolButtons}[${i}].click()`);
    await shows(`key ${key} 0`);
  }
  const keyButtons = "frames[0].document.querySelectorAll('#keys button[data-key]')";
  const keysShown = () => evaluate(`[...${keyButtons}].map((b) => b.getBoundingClientRect().width > 0)`);
  assert.deepEqual(await keysShown(), [false, false, false], "with no drawing the zoom buttons are hidden");
  const image = "frames[0].document.querySelector('#viewport img')";
  const since = (await evaluate("shown")).length;
  await submit("svg");
  await until(() => evaluate(`!!${image}?.complete`));
  assert.deepEqual(await keysShown(), [true, true, true], "a drawing shows the zoom buttons");
  const box = () => evaluate(`(() => { const r = ${image}.getBoundingClientRect();
    return { left: r.left, top: r.top, width: r.width, height: r.height }; })()`);
  const fitted = await box();
  const near = (a, b) => Math.abs(a - b) < 1e-3;
  await evaluate(`${keyButtons}[0].click()`);
  const zoomed = await box();
  assert.ok(near(zoomed.width, fitted.width * 1.25) && near(zoomed.height, fitted.height * 1.25) &&
    near(zoomed.left + zoomed.width / 2, fitted.left + fitted.width / 2) &&
    near(zoomed.top + zoomed.height / 2, fitted.top + fitted.height / 2),
    `"+" scales the drawing by 1.25 about the frame's centre: ${JSON.stringify([fitted, zoomed])}`);
  await evaluate(`${keyButtons}[2].click()`);
  assert.deepEqual(await box(), fitted, `"f" fits the drawing again`);
  // A drag in the parent's coordinates, from the drawing's centre.
  const at = await evaluate(`(() => { const i = document.querySelector('iframe'), f = i.getBoundingClientRect();
    return { x: f.left + i.clientLeft + ${fitted.left} + ${fitted.width} / 2,
      y: f.top + i.clientTop + ${fitted.top} + ${fitted.height} / 2 }; })()`);
  at.x = Math.round(at.x); at.y = Math.round(at.y);
  const mouse = (type, x, y) => call("Input.dispatchMouseEvent", { type, x, y, button: "left", buttons: type === "mouseReleased" ? 0 : 1, clickCount: 1 });
  await mouse("mousePressed", at.x, at.y);
  await mouse("mouseMoved", at.x + 30, at.y + 20);
  await mouse("mouseReleased", at.x + 30, at.y + 20);
  const dragged = await box();
  assert.deepEqual(dragged, { ...fitted, left: fitted.left + 30, top: fitted.top + 20 }, "a drag pans the drawing");
  // A turn after the clicks and the drag answers after any key turn they sent.
  await typed("after", "tick 7");
  assert.deepEqual((await evaluate("shown")).slice(since).filter((t) => t.startsWith("key ")), [],
    "no key turn reaches the module from the zoom buttons");
  await submit("draw");
  await until(async () => !(await evaluate(`!!${image}`)));
  assert.deepEqual(await keysShown(), [false, false, false], "the drawing's removal hides the zoom buttons");
  await evaluate(`${toolButtons}[0].click()`); await shows("key + 0");
  console.log(`Headless Chrome: the toolbar's buttons send the keys ${keys.join(" ")}; the zoom buttons show only over a drawing, where "+" scales it by 1.25, "f" fits it and a drag pans it, and no key turn is sent; after the drawing goes the toolbar's "+" sends its key again`);

  const later = `notified ui/notifications/tool-result ${JSON.stringify(LATER)}`;
  await evaluate(`stranger({ method: "ui/notifications/tool-result", params: ${JSON.stringify(STRANGER)} });
    send({ method: "ui/notifications/tool-result", params: ${JSON.stringify(LATER)} })`);
  await shows(later);
  assert.deepEqual(await notes(), [input, result, later], "a message from a window other than the parent is ignored");

  await typed("rpc", `answered 1 1 ${JSON.stringify(TOOL_RESULT)}`);
  assert.deepEqual(view.log.main.at(-1).params, { name: "get_project" }, "the module's tools/call goes over the socket");

  await typed("context", "answered 1 3 {}");
  const context = (await evaluate("log")).at(-1);
  assert.equal(context.method, "ui/update-model-context");
  assert.deepEqual(context.params, { content: [{ type: "text", text: "seen" }] });

  await typed("after", "tick 7");

  await evaluate("both = true");
  await submit("context");
  await until(async () => (await text()).startsWith("answered 0"));
  assert.match(await text(), /^answered 0 3 Error: \d+: the message is not a JSON-RPC response$/,
    "a response holding both a result and an error is a transport failure");

  // The page has no file picker: a file reaches the module dropped on the frame.
  const data = resolve(output, "data.bin");
  const drag = { items: [], files: [data], dragOperationsMask: 1 };
  for (const type of ["dragEnter", "dragOver", "drop"])
    await call("Input.dispatchDragEvent", { type, x: 100, y: 100, data: drag });
  await shows(`file data.bin ${readFileSync(data).length} 1`);
  await typed("read start", "chunk 1 0 1 0,1,2,");
  await typed("read end", "chunk 1 7 1 253,254,255,");

  await evaluate("send({ id: 99, method: 'ui/resource-teardown', params: {} })");
  await until(() => evaluate("replies.some((r) => r.id === 99)"));
  assert.deepEqual(await evaluate("replies.find((r) => r.id === 99)"), { jsonrpc: "2.0", id: 99, result: {} });

  await evaluate(`addTorn("second", "ui/initialize", answers["ui/initialize"])`);
  await until(() => evaluate("second.replies.some((r) => r.id === 2)"));
  assert.deepEqual(await evaluate("second.replies"), [{ jsonrpc: "2.0", id: 1, result: {} },
    { jsonrpc: "2.0", id: 2, error: { code: -32601, message: "Method not found" } }]);
  assert.deepEqual(await evaluate("second.log.map((m) => m.method)"), ["ui/initialize"],
    "a frame torn down before its initialize is answered posts nothing after the answer");

  await evaluate(`addTorn("pending", "tools/call", { content: [], _meta: { ...code, page: "0".repeat(64) } })`);
  await until(() => evaluate("pending.replies.some((r) => r.id === 2)"));
  assert.deepEqual(await evaluate("pending.replies"), [{ jsonrpc: "2.0", id: 1, result: {} },
    { jsonrpc: "2.0", id: 2, error: { code: -32601, message: "Method not found" } }]);
  assert.deepEqual(await evaluate("pending.log.map((m) => m.method)"), handshake,
    "a frame torn down while its view_code request is pending posts nothing after the teardown's answer");

  await evaluate("addOlder()");
  const olderText = () => evaluate("older.frame.contentDocument?.querySelector('#text')?.textContent ?? null");
  await until(async () => await olderText() === OLDER && (await evaluate("older.log.length")) > handshake.length)
    .catch(async (error) => { throw new Error(`the older frame's #text is ${JSON.stringify(await olderText())}`, { cause: error }); });
  const olderLog = await evaluate("older.log");
  assert.deepEqual(olderLog.map((m) => m.method), [...handshake, "ui/update-model-context"],
    "an older page asks for its code, then tells the model once");
  assert.deepEqual(olderLog.at(-1).params, OLDER_CONTEXT);
  console.log(`Headless Chrome: an older page shows ${JSON.stringify(OLDER)}`);

  const view_code = { name: "view_code", arguments: {} };
  const restartedText = (name) => evaluate(`${name}.frame.contentDocument?.querySelector('#text')?.textContent ?? null`);
  const restartedShows = (name, want) => until(async () => await restartedText(name) === want)
    .catch(async (error) => { throw new Error(`the ${name} frame's #text is ${JSON.stringify(await restartedText(name))}, not ${JSON.stringify(want)}`, { cause: error }); });
  await evaluate("addRestart('restarted', secondCode)");
  await restartedShows("restarted", SECOND_SHOWN);
  const first = await evaluate("restarted.frame.contentWindow.first");
  assert.deepEqual(first.meta, await evaluate("firstCode._meta"), "the first view is handed view_code's whole _meta");
  assert.equal(first.carried, null, "the first view is handed carried null");
  assert.equal(first.disposed, true, "restart disposes the first view");
  assert.deepEqual((await evaluate("restarted.log")).filter((m) => m.method === "tools/call").map((m) => m.params),
    [view_code, view_code], "the page asks for view_code once and the first view once more");
  console.log(`Headless Chrome: a restarted view shows ${JSON.stringify(SECOND_SHOWN)}`);

  await evaluate("addRestart('restartedOlder', olderCode)");
  await restartedShows("restartedOlder", OLDER);
  await until(async () => (await evaluate("restartedOlder.log")).some((m) => m.method === "ui/update-model-context"));
  const restartedOlderLog = await evaluate("restartedOlder.log");
  assert.deepEqual(restartedOlderLog.map((m) => m.method), [...handshake, "tools/call", "ui/update-model-context"],
    "a restart onto an older page tells the model once");
  assert.deepEqual(restartedOlderLog.at(-1).params, OLDER_CONTEXT);
  assert.equal(await evaluate("restartedOlder.frame.contentWindow.first.disposed"), true,
    "a restart onto an older page disposes the first view");
  console.log(`Headless Chrome: a restart onto an older page shows ${JSON.stringify(OLDER)}`);

  // The frames that run the server's view.js and socket.js through app.js.
  const liveText = (name) => evaluate(`${name}.frame.contentDocument?.querySelector('#text')?.textContent ?? null`);
  const liveShows = (name, want) => until(async () => await liveText(name) === want).catch(async (error) => {
    throw new Error(`the ${name} frame's #text is ${JSON.stringify(await liveText(name))}, not ${JSON.stringify(want)}; ` +
      `it showed ${JSON.stringify(await evaluate(`${name}.shown`))}`, { cause: error });
  });
  const liveTyped = async (name, words, want) => {
    await evaluate(`(() => { const f = ${name}.frame.contentDocument.querySelector('form input');
      f.value = ${JSON.stringify(words)}; f.form.requestSubmit(); })()`);
    await liveShows(name, want);
  };
  const liveShown = () => evaluate("live.shown");
  const hostTools = (name) => evaluate(`${name}.log.filter((m) => m.method === 'tools/call').map((m) => m.params.name)`);
  const told = (name) => evaluate(`${name}.log.filter((m) => m.method === 'ui/update-model-context')`);
  const rpcAnswer = `answered 1 1 ${JSON.stringify(TOOL_RESULT)}`;

  await evaluate("addLive('live')");
  await until(async () => (await liveShown()).includes(opened)).catch(async (error) => {
    throw new Error(`the live frame showed ${JSON.stringify(await liveShown())}; it told ${JSON.stringify(await told("live"))}`, { cause: error });
  });
  await liveTyped("live", "rpc", rpcAnswer);
  assert.deepEqual(view.log.live.map((m) => [m.method, m.params]), [["tools/call", { name: "get_project" }]]);
  assert.deepEqual(await hostTools("live"), ["view_code", "view_socket"], "no tools/call but view_code and view_socket reaches the host");
  assert.deepEqual(await liveShown(), [opened, rpcAnswer], "view/code, the socket's first message, does not reach the module");
  const liveMethods = () => evaluate("live.log.map((m) => m.method)");
  assert.deepEqual(await liveMethods(), [...handshake, ...viewStart],
    "the page asks for no display mode; view.js's first start asks for full screen after view_code");
  assert.deepEqual((await evaluate("live.log")).find((m) => m.method === "ui/request-display-mode").params, { mode: "fullscreen" });
  console.log(`Headless Chrome: the live frame posts ${(await liveMethods()).join(", ")}`);
  console.log("Headless Chrome: socket: a tool call is answered over the socket; the host's only tools/calls are view_code and view_socket; view/code does not reach the module");

  const changed = { project: "p1" };
  const changedNote = `notified maki/changed ${JSON.stringify(changed)}`;
  push("live", "view/alive", {});
  push("live", "maki/changed", changed);
  await liveShows("live", changedNote);
  assert.deepEqual(await liveShown(), [opened, rpcAnswer, changedNote], "view/alive does not reach the module");
  console.log(`Headless Chrome: socket: a pushed view/alive does not reach the module; the pushed maki/changed ${JSON.stringify(changed)} does`);

  const posted = (await evaluate("live.log")).length;
  await liveTyped("live", "resume", "answered 1 4 {}");
  assert.equal(view.log.live.length, 1, "view/resume does not reach the socket");
  assert.equal((await evaluate("live.log")).length, posted, "view/resume does not reach the host");
  console.log("Headless Chrome: socket: view/resume is kept and answered {} at once");

  view.binary.live = [Buffer.from("kept")];
  view.hold.live = true;
  const closedError = { code: -32000, message: "the view's socket closed" };
  await liveTyped("live", "rpc", `answered 2 1 ${JSON.stringify(closedError)}`);
  await until(async () => (await liveShown()).filter((t) => t === opened).length === 2);
  assert.equal(view.opens.live, 2);
  const gap = view.openedAt.live - view.closedAt.live;
  assert.ok(gap >= 1000, `the view reopened ${gap} ms after the close, inside its 1 s backoff`);
  assert.deepEqual(await hostTools("live"), ["view_code", "view_socket"], "a reopen within the session asks the host nothing");
  console.log(`Headless Chrome: socket: a close answers the outstanding call ${JSON.stringify(closedError)}, and the binary message the server sent for it reaches no module; the view reopens ${Math.round(gap)} ms later and the module gets view/open again`);

  view.codes.live = CODE_B;
  const before = (await liveShown()).length;
  const canvasFocused = () => evaluate("live.frame.contentDocument.activeElement?.localName === 'canvas'");
  await evaluate("live.frame.contentDocument.querySelector('canvas').focus()");
  assert.equal(await canvasFocused(), true, "the canvas has focus before the restart");
  drop("live");
  // The old module, told view/restart once view_code is answered, tells
  // view/resume {"project":"p2"} over the p1 it told before; its turn settles
  // with that rpc's answer, and the new module receives p2.
  const restarting = "notified view/restart {}";
  const resumed = `notified view/resume ${JSON.stringify({ project: "p2" })}`;
  const after = [opened, restarting, "answered 1 5 {}", resumed, opened];
  await until(async () => (await liveShown()).length >= before + after.length).catch(async (error) => {
    throw new Error(`the live frame showed ${JSON.stringify((await liveShown()).slice(before))}; it told ${JSON.stringify(await told("live"))}`, { cause: error });
  });
  assert.deepEqual((await liveShown()).slice(before), after,
    "the old module sees the reopen, then view/restart and its resume's answer; the new one its resume, then its own socket's open");
  assert.equal(await canvasFocused(), true, "the restarted view's canvas has the focus the old one had");
  assert.deepEqual(view.log.live.at(-1), { jsonrpc: "2.0", id: 3, method: "tools/call", params: view_code },
    "view_code is fetched over the socket");
  // The new view's socket brought view/code before this call's answer.
  await liveTyped("live", "rpc", rpcAnswer);
  assert.deepEqual(view.log.live.filter((m) => m.params?.name === "view_code").length, 1,
    "the restarted view, given the code it runs, does not restart again");
  assert.equal(view.opens.live, 4);
  assert.equal(view.asks.live, "1", "the restarted view's socket reuses the carried session");
  assert.deepEqual(await hostTools("live"), ["view_code", "view_socket"], "the restart asks the host nothing");
  assert.deepEqual(await told("live"), [], "the restart does not fail");
  assert.equal((await liveMethods()).filter((m) => m === "ui/request-display-mode").length, 1,
    "the restarted view does not ask for full screen again");
  console.log("Headless Chrome: socket: view/code naming other code fetches view_code over the socket and app.js restarts the view; the old module is notified view/restart {} first, and the new module receives the view/resume {\"project\":\"p2\"} it told then; its socket reuses the session, and its canvas has the focus the old one had");

  view.binary.live = [Buffer.from("ab"), Buffer.from("cd")];
  await liveTyped("live", "rpc", `${rpcAnswer} abcd`);
  console.log("Headless Chrome: socket: the module gets the server's binary messages for a call after its response's JSON, in the order sent");

  // A drop on the restarted page reaches only the new view's listener; an old
  // view's would hand the file to its disposed viewport, which throws.
  const bytes = [...readFileSync(data)];
  const droppedText = `file data.bin ${bytes.length} 1`;
  await evaluate(`(() => { const w = live.frame.contentWindow, t = new w.DataTransfer();
    t.items.add(new w.File([new w.Uint8Array(${JSON.stringify(bytes)})], "data.bin"));
    w.document.body.dispatchEvent(new w.DragEvent("drop", { dataTransfer: t, bubbles: true, cancelable: true })); })()`);
  await liveShows("live", droppedText);
  assert.equal((await liveShown()).filter((t) => t === droppedText).length, 1, "the dropped file reaches the module once");
  assert.deepEqual(await evaluate("live.errors"), [], "no listener of the old view handles the drop");
  console.log(`Headless Chrome: socket: after a restart, a dropped file reaches the module once (${droppedText}) and the old view's listeners are gone`);

  view.sockets.live.close(4403, "expired");
  await until(() => view.opens.live === 5);
  assert.equal(view.asks.live, "2", "the view asks view_socket again before it reopens");
  assert.deepEqual(await hostTools("live"), ["view_code", "view_socket", "view_socket"]);
  console.log("Headless Chrome: socket: a close under 4403 makes the view ask view_socket again before it reopens");

  await evaluate("addLive('flaky')");
  await until(() => view.opens.flaky === 3);
  const [firstGap, secondGap] = view.reopens.flaky.map(Math.round);
  assert.ok(firstGap >= 1000 && secondGap >= 2000,
    `a socket closed right after it opens reopens after ${firstGap} ms, then ${secondGap} ms, not 1 s then 2 s`);
  push("flaky", "view/alive", {});
  drop("flaky");
  await until(() => view.opens.flaky === 4);
  const provedGap = Math.round(view.reopens.flaky[2]);
  assert.ok(provedGap >= 1000 && provedGap < 4000, `a socket that sent view/alive reopens after ${provedGap} ms, not 1 s`);
  await evaluate("flaky.frame.remove()");
  console.log(`Headless Chrome: socket: sockets closed right after they open reopen after ${firstGap} ms, then ${secondGap} ms; after a view/alive, ${provedGap} ms`);

  await evaluate("addLive('erring')");
  // view/code naming other code reaches the view once it runs, as after a
  // release; before, app.js's restart would ignore the fetch's result.
  await until(async () => (await evaluate("erring.shown")).includes(opened));
  push("erring", "view/code", { code: CODE_B });
  await until(() => view.log.erring.some((m) => m.params?.name === "view_code"));
  await liveTyped("erring", "rpc", rpcAnswer).catch(async (error) => {
    throw new Error(`the view did not run on: it told ${JSON.stringify(await told("erring"))}`, { cause: error });
  });
  assert.deepEqual(view.log.erring.map((m) => m.params?.name), ["view_code", "get_project"],
    "view_code's tool error over the socket is not retried until the next socket's view/code");
  assert.deepEqual(await told("erring"), [], "view_code's tool error does not fail the view");
  await evaluate("erring.frame.remove()");
  console.log(`Headless Chrome: socket: view_code over the socket answered with a tool error ${JSON.stringify(CODE_ERROR)} leaves the view running; its next call is answered ${rpcAnswer}`);

  const requests = view.requests;
  await evaluate(`addLive("failing", ${JSON.stringify(SOCKET_ERROR)}, { availableDisplayModes: ["inline"] })`);
  await until(() => evaluate("failing.frame.contentDocument?.querySelector('form input')?.disabled === false"));
  const failed = { code: -32000, message: NO_SOCKET };
  await liveTyped("failing", "rpc", `answered 2 1 ${JSON.stringify(failed)}`);
  assert.deepEqual(await hostTools("failing"), ["view_code", "view_socket"], "the module's tools/call does not reach the host");
  assert.equal(view.requests, requests, "the view makes no socket");
  assert.deepEqual(await evaluate("failing.log.map((m) => m.method)"), [...handshake, "tools/call"],
    "a view whose host lists its modes without fullscreen asks for no display mode");
  console.log("Headless Chrome: a host whose context lists only the inline display mode is asked for none");
  console.log(`Headless Chrome: socket: view_socket answered with a tool error answers the module's tools/call ${JSON.stringify(failed)}, and the view makes no socket`);
  console.log("Headless Chrome: MCP App held notifications in order, a stranger ignored, tools/call over the socket, model context, after, a malformed response, file reads, teardown, an early teardown, a teardown during view_code, an older page, a restart, a restart onto an older page and the view's socket passed, its backoff, a view_code tool error and a restart's listeners among them");
} catch (error) {
  failure = error; throw error;
} finally {
  try {
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
