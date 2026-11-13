// A foreign browser peer standing in for ChatGPT's MCP Apps host, not a Habu
// build driver. It serves a parent page that iframes PAGE, a page composed by
// MCP-APP:PAGE+ from test/browser/app.f's module, under ChatGPT's view CSP,
// and plays the host bridge in the parent: it answers the view's requests from
// a table, records each message, and sends a tool-input and a tool-result
// notification before it answers ui/initialize, so the view holds both until
// its start turn. Headless Chrome then checks the handshake, that both
// notifications reach the module in the order sent, that a message from a
// window other than the parent is ignored, a tools/call round trip, the model
// context, an after tick, a response holding both a result and an error, a
// picked file read in two pieces, teardown, and a second frame torn down before
// its ui/initialize is answered. DIR holds data.bin, the file picked, and
// receives chrome.log; TITLE is the page's unescaped title.
import assert from "node:assert/strict";
import { readFileSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { devtoolsPort, launch, stopGroup, until } from "./chrome.mjs";

const [file, output, title, flag, chrome] = process.argv.slice(2);
if (!file || !output || !title || flag !== "--chrome" || !chrome)
  throw new Error("usage: bun test/browser/app-peer.mjs PAGE DIR TITLE --chrome EXECUTABLE");
const page = readFileSync(file);
const CSP = "script-src 'self' 'wasm-unsafe-eval' 'unsafe-inline' 'unsafe-eval' blob:; worker-src blob:; connect-src 'self'";
const INPUT = { arguments: { name: "Zürich" } };
const PARAMS = { structuredContent: { name: "Zürich ✓" } };
const STRANGER = { structuredContent: { name: "stranger" } };
const LATER = { structuredContent: { name: "after the stranger" } };
const TOOL_RESULT = { content: [{ type: "text", text: "project" }] };

// The parent: every message from the frame lands in window.log; requests are
// answered from the table. Before it answers ui/initialize it starts
// recording into window.shown each text the frame's #text is given, and sends
// the two notifications. stranger posts a message to the frame from a new
// sibling frame, so its source is not the frame's parent. While both is set,
// the next tools/call is answered with a result and an error.
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
    new MutationObserver((records) => {
      for (const r of records) for (const n of r.addedNodes) shown.push(n.textContent);
    }).observe(frame.contentDocument.querySelector("#text"), { childList: true });
    send({ method: "ui/notifications/tool-input", params: ${JSON.stringify(INPUT)} });
    send({ method: "ui/notifications/tool-result", params: ${JSON.stringify(PARAMS)} });
  }
  if (both && data.method === "tools/call") { both = false; send({ id: data.id, result: answers[data.method], error: { code: -32603, message: "both" } }); }
  else if ("id" in data) send({ id: data.id, result: answers[data.method] });
});
// addSecond adds a page frame and, on its ui/initialize, sends it a teardown,
// then the answer, then a ping whose reply marks that the answer was handled.
// The messages it posts land in second.log, its replies in second.replies.
window.second = { log: [], replies: [] };
window.addSecond = () => {
  const other = document.body.appendChild(document.createElement("iframe"));
  other.src = "/app.html";
  const send = (message) => other.contentWindow.postMessage({ jsonrpc: "2.0", ...message }, "*");
  window.addEventListener("message", ({ source, data }) => {
    if (source !== other.contentWindow) return;
    if (!("method" in data)) { second.replies.push(data); return; }
    second.log.push(data);
    if (data.method !== "ui/initialize") return;
    send({ id: 1, method: "ui/resource-teardown", params: {} });
    send({ id: data.id, result: answers["ui/initialize"] });
    send({ id: 2, method: "ping" });
  });
};
</script>`;

const server = Bun.serve({ hostname: "127.0.0.1", port: 0, fetch(request) {
  const path = new URL(request.url).pathname;
  if (path === "/") return new Response(parent, { headers: { "Content-Type": "text/html; charset=utf-8" } });
  if (path === "/app.html")
    return new Response(page, { headers: { "Content-Type": "text/html; charset=utf-8", "Content-Security-Policy": CSP } });
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

  const notes = async () => (await evaluate("shown")).filter((t) => t.startsWith("notified "));
  const input = `notified ui/notifications/tool-input ${JSON.stringify(INPUT)}`;
  const result = `notified ui/notifications/tool-result ${JSON.stringify(PARAMS)}`;
  await until(async () => (await notes()).length === 2);
  assert.deepEqual(await notes(), [input, result], "the held notifications reach the module in the order sent");
  const methods = () => evaluate("log.map((m) => m.method)");
  assert.deepEqual(await methods(), ["ui/initialize", "ui/notifications/initialized", "ui/request-display-mode"],
    "the handshake: initialize, then initialized, then the display-mode request");
  const [init, , mode] = await evaluate("log");
  assert.deepEqual(init.params, { appInfo: { name: title, version: "1" }, appCapabilities: {}, protocolVersion: "2026-01-26" });
  assert.deepEqual(mode.params, { mode: "fullscreen" });
  console.log(`Headless Chrome: handshake ${(await methods()).join(", ")}`);

  const later = `notified ui/notifications/tool-result ${JSON.stringify(LATER)}`;
  await evaluate(`stranger({ method: "ui/notifications/tool-result", params: ${JSON.stringify(STRANGER)} });
    send({ method: "ui/notifications/tool-result", params: ${JSON.stringify(LATER)} })`);
  await shows(later);
  assert.deepEqual(await notes(), [input, result, later], "a message from a window other than the parent is ignored");

  await typed("rpc", `answered 1 1 ${JSON.stringify(TOOL_RESULT)}`);
  const tool = (await evaluate("log")).at(-1);
  assert.equal(tool.method, "tools/call");
  assert.deepEqual(tool.params, { name: "get_project" });

  await typed("context", "answered 1 3 {}");
  const context = (await evaluate("log")).at(-1);
  assert.equal(context.method, "ui/update-model-context");
  assert.deepEqual(context.params, { content: [{ type: "text", text: "seen" }] });

  await typed("after", "tick 7");

  await evaluate("both = true");
  await submit("rpc");
  await until(async () => (await text()).startsWith("answered "));
  assert.match(await text(), /^answered 0 1 Error: \d+: the message is not a JSON-RPC response$/,
    "a response holding both a result and an error is a transport failure");

  const data = resolve(output, "data.bin");
  const picker = await call("Runtime.evaluate", { expression: "frames[0].document.querySelector('#file')" });
  await call("DOM.enable");
  await call("DOM.setFileInputFiles", { files: [data], objectId: picker.result.objectId });
  await shows(`file data.bin ${readFileSync(data).length} 1`);
  await typed("read start", "chunk 1 0 1 0,1,2,");
  await typed("read end", "chunk 1 7 1 253,254,255,");

  await evaluate("send({ id: 99, method: 'ui/resource-teardown', params: {} })");
  await until(() => evaluate("replies.some((r) => r.id === 99)"));
  assert.deepEqual(await evaluate("replies.find((r) => r.id === 99)"), { jsonrpc: "2.0", id: 99, result: {} });

  await evaluate("addSecond()");
  await until(() => evaluate("second.replies.some((r) => r.id === 2)"));
  assert.deepEqual(await evaluate("second.replies"), [{ jsonrpc: "2.0", id: 1, result: {} },
    { jsonrpc: "2.0", id: 2, error: { code: -32601, message: "Method not found" } }]);
  assert.deepEqual(await evaluate("second.log.map((m) => m.method)"), ["ui/initialize"],
    "a frame torn down before its initialize is answered posts nothing after the answer");
  console.log("Headless Chrome: MCP App held notifications in order, a stranger ignored, tools/call, model context, after, a malformed response, file reads, teardown and an early teardown passed");
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
