// app.js - an MCP App's view (ext-apps, protocol "2026-01-26") of a Habu Wasm
// module, started by app.html's bootstrap with the page's id. A host keeps
// this page, so it holds only what does not change with a release; the view
// comes from the server each time it starts. It speaks JSON-RPC 2.0 with the
// host over window.parent.postMessage and takes messages only from
// window.parent:
//
//   start  ui/initialize {appInfo, appCapabilities, protocolVersion}; on its
//          result the notification ui/notifications/initialized, then
//          ui/request-display-mode {mode: "fullscreen"}, then tools/call
//          {name: "view_code", arguments: {}}, whose result's _meta holds the
//          server's page id and the view's code, then view.js's start
//   code   _meta {page, files}, files each base64 by name; view.js is
//          imported from a blob: URL; a page id that is not this page's ends
//          start: the page is older than its server
//   view   view.js exports start(host), host {files, request}: files
//          is the _meta's files, request(method, params) resolves with the
//          response message as request below does; start resolves with
//          {notify(method, paramsJSON), dispose()}
//   failure
//          a failure of loading or mounting (the call's error, a missing
//          file, an import's or view.js's start's failure, the older page)
//          is told to the model once, as ui/update-model-context of one text
//          block for the assistant, then shown in #text; a failure after
//          ui/resource-teardown is neither told nor shown
//   host notifications
//          each goes to notify(method, paramsJSON), in order, held until
//          view.js's start has resolved, and dropped once start has failed
//          or the view is torn down
//   ui/resource-teardown
//          disposes the started view, then answers {}; any other host
//          request is answered -32601
//
// Start is a machine over phase, whose states are
//   starting  waiting for ui/initialize's result
//   loading   the view_code call and view.js's import
//   mounting  waiting for view.js's start
//   running   started; notifications are delivered
//   failed    start failed: shown, and told to the model when loading or mounting failed
//   torn      ui/resource-teardown arrived
// starting goes to loading on initialize's result; loading to mounting once
// the code is checked and view.js imported; mounting to running when start
// resolves, which delivers the held notifications. loading or mounting goes
// to failed on any failure; a failure in starting goes to failed and is only
// shown, by the bootstrap. Any state goes to torn on ui/resource-teardown. Each step after
// an await runs only while the phase is still the one it set, and a start
// that resolves after torn is disposed at once.
const PROTOCOL = "2026-01-26";
const VERSION = "1";
const NOT_FOUND = -32601;
const CODE_TOOL = "view_code";

let lastId = 0;
const pending = new Map(); // a request's id -> the resolve and reject of its response

function post(message) {
  window.parent.postMessage({ jsonrpc: "2.0", ...message }, "*");
}

// Resolves with the response message, whether it holds a result or an error;
// rejects a response that holds neither or both.
function request(method, params) {
  const id = ++lastId;
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    post({ id, method, params });
  });
}

async function call(method, params) {
  const r = await request(method, params);
  if ("error" in r) throw new Error(`${method}: ${JSON.stringify(r.error)}`);
  return r.result;
}

const shown = document.querySelector("#text"); // app.html's own, where a failure is shown

let phase = "starting";
let view = null; // view.js's started view, while running
const held = []; // host notifications that arrived before start resolved

// JSON-RPC 2.0 omits params when there are none: delivered as {}.
function deliver({ method, params }) {
  view.notify(method, JSON.stringify(params ?? {}));
}

function teardown() {
  if (phase === "running") view.dispose();
  view = null;
  held.length = 0;
  phase = "torn";
}

function received({ source, data: m }) {
  if (source !== window.parent || m === null || typeof m !== "object" || m.jsonrpc !== "2.0") return;
  if (typeof m.method === "string" && "id" in m) {
    if (m.method === "ui/resource-teardown") {
      teardown();
      post({ id: m.id, result: {} });
    } else {
      post({ id: m.id, error: { code: NOT_FOUND, message: "Method not found" } });
    }
  } else if (typeof m.method === "string") {
    if (phase === "running") deliver(m);
    else if (phase !== "failed" && phase !== "torn") held.push(m);
  } else if (pending.has(m.id)) {
    const { resolve, reject } = pending.get(m.id);
    pending.delete(m.id);
    if (("result" in m) === ("error" in m)) reject(new Error(`${m.id}: the message is not a JSON-RPC response`));
    else resolve(m);
  }
}

async function load(bytes) {
  const url = URL.createObjectURL(new Blob([bytes], { type: "text/javascript" }));
  try { return await import(url); } finally { URL.revokeObjectURL(url); }
}

// A failure of loading or mounting: told to the model, then shown in #text.
function failed(title, text) {
  call("ui/update-model-context", { content: [{ type: "text",
    text: `${title}'s view could not start: ${text}`, annotations: { audience: ["assistant"] } }] })
    .catch((e) => console.error(e));
  document.body.replaceChildren(shown);
  shown.textContent = text;
  shown.className = "error";
}

// The server's page id is not this page's: its text is shown as it is.
class OlderPage extends Error {}

// view_code's result r, checked for this page: its files and view.js's bytes.
function code(r, id, title) {
  if (r.isError)
    throw new Error(`${CODE_TOOL}: ${(r.content ?? []).filter((c) => c.type === "text").map((c) => c.text).join(" ")}`);
  if (typeof r._meta?.page !== "string")
    throw new Error(`${CODE_TOOL}: the result's _meta lacks page`);
  if (r._meta.page !== id)
    throw new OlderPage(`This view's page is older than ${title}'s server: refresh the ${title} app, then open this view again.`);
  const { files } = r._meta;
  const coded = files?.["view.js"];
  if (typeof coded !== "string") throw new Error(`${CODE_TOOL}: the result's _meta lacks view.js`);
  return { files, bytes: Uint8Array.fromBase64(coded) };
}

// loading, then mounting, then running.
async function run(id, title) {
  post({ method: "ui/notifications/initialized" });
  call("ui/request-display-mode", { mode: "fullscreen" }).catch((e) => console.error(e));
  const r = await call("tools/call", { name: CODE_TOOL, arguments: {} });
  if (phase !== "loading") return;
  const { files, bytes } = code(r, id, title);
  const module = await load(bytes);
  if (phase !== "loading") return;
  phase = "mounting";
  const started = await module.start({ files, request });
  if (phase !== "mounting") {
    started.dispose();
    return;
  }
  phase = "running";
  view = started;
  for (const m of held.splice(0)) deliver(m);
}

export async function start(id) {
  window.addEventListener("message", received);
  const title = document.querySelector("title").textContent;
  try {
    await call("ui/initialize", {
      appInfo: { name: title, version: VERSION },
      appCapabilities: {},
      protocolVersion: PROTOCOL,
    });
    if (phase !== "starting") return;
    phase = "loading";
    await run(id, title);
  } catch (e) {
    if (phase === "starting") { phase = "failed"; held.length = 0; throw e; } // the bootstrap shows it
    if (phase !== "loading" && phase !== "mounting") return;
    phase = "failed";
    held.length = 0;
    console.error(e);
    failed(title, e instanceof OlderPage ? e.message : String(e));
  }
}
