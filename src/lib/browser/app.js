// app.js - an MCP App's view (ext-apps, protocol "2026-01-26") of a Habu Wasm
// module, started by app.html's bootstrap with turn.js's Module, viewport.js's
// mount, page.js's exports and the module's bytes. It speaks JSON-RPC 2.0 with
// the host over window.parent.postMessage and takes messages only from
// window.parent:
//
//   start  ui/initialize {appInfo, appCapabilities, protocolVersion}; on its
//          result the notification ui/notifications/initialized, then
//          ui/request-display-mode {mode: "fullscreen"}, then mount; a teardown
//          before that result ends start there
//   rpc    a module's rpc record is a request under this file's own id; its
//          response's result or error answers the record; a response that
//          holds neither or both rejects it
//   host notifications
//          each goes to view.notify(method, paramsJSON), in order, held until
//          mount has resolved, so after the start turn has begun
//   ui/resource-teardown
//          disposes the viewport, then answers {}; any other host request is
//          answered -32601
//
// page.js shows what the module sends and gives it the field's text and each
// file picked or dropped anywhere on the page. The module is the one path the
// viewport's fetch answers; every other path rejects.
const PROTOCOL = "2026-01-26";
const VERSION = "1";
const MODULE = "module.wasm";
const NOT_FOUND = -32601;

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

async function rpc(method, params) {
  const r = await request(method, JSON.parse(params));
  return JSON.stringify("result" in r ? { result: r.result } : { error: r.error });
}

let view = null;
let torn = false;
const held = []; // host notifications that arrived before mount resolved

// JSON-RPC 2.0 omits params when there are none: delivered as {}.
function deliver({ method, params }) {
  view.notify(method, JSON.stringify(params ?? {}));
}

function teardown() {
  torn = true;
  view?.dispose();
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
    if (torn) return;
    if (view) deliver(m);
    else held.push(m);
  } else if (pending.has(m.id)) {
    const { resolve, reject } = pending.get(m.id);
    pending.delete(m.id);
    if (("result" in m) === ("error" in m)) reject(new Error(`${m.id}: the message is not a JSON-RPC response`));
    else resolve(m);
  }
}

export async function start(Module, mount, { canvas, show, enable, sceneTree, svg, listen }, bytes) {
  window.addEventListener("message", received);
  await call("ui/initialize", {
    appInfo: { name: document.querySelector("title").textContent, version: VERSION },
    appCapabilities: {},
    protocolVersion: PROTOCOL,
  });
  if (torn) return;
  post({ method: "ui/notifications/initialized" });
  call("ui/request-display-mode", { mode: "fullscreen" }).catch((e) => console.error(e));
  const mounted = await mount(canvas, {
    Module,
    module: MODULE,
    fetch: async (url) => {
      if (url !== MODULE) throw new Error(`${url}: the page holds only ${MODULE}`);
      return new Response(bytes, { headers: { "Content-Type": "application/wasm" } });
    },
    rpc,
    onTree: sceneTree((key, opened) =>
      mounted.post({ kind: "control", key, opened })),
    onText: show,
    onSvg: svg,
    onInput: enable,
  });
  if (torn) {
    mounted.dispose();
    return;
  }
  view = mounted;
  for (const m of held.splice(0)) deliver(m);
  listen(view);
}
