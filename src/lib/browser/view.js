// view.js - the part of an MCP App's view that changes with a release, which
// app.js imports from the server's code and starts. start(host), host {meta,
// files, request, restart, carried}, puts view.html's style and markup in place
// of the page's body, imports turn.js, viewport.js, page.js and socket.js from
// blob: URLs after it, since page.js reads the document as it loads, opens the
// view's socket (socket.js), mounts the module with an rpc bridge, and listens
// to the page's field and drops. It resolves with the viewport, whose dispose
// also ends the socket and removes the page's listeners, so a restart leaves
// none of this view's.
//
//   rpc    a module's tools/call rpc goes over the socket (socket.call), its
//          view/resume P is kept by the socket and answered {} at once, and
//          every other method is host.request(method, params); the response's
//          result or error answers the record
//   notify each server notification but view/code and view/alive, which are
//          the shell's own, and view/open each time the socket opens, reaches
//          the module as the viewport's notify, the path host
//          notifications take, held until the module is mounted; when carried
//          is set, the module first receives view/resume with its resume
//   code   view/code whose code is not meta.code: tools/call view_code {} over
//          the socket, then host.restart(result, socket.carried()), one fetch
//          at a time; a failed fetch, an error or a tool error (isError), is
//          logged and the view runs on, so the next socket's view/code tries
//          again
//
// page.js shows what the module sends and gives it the field's text and each
// file dropped anywhere on the page. The module is the one path the
// viewport's fetch answers; every other path rejects.
const MODULE = "module.wasm";
const CODE_TOOL = "view_code";
const SCRIPTS = ["turn.js", "viewport.js", "page.js", "socket.js"];

// A file of view_code's _meta as bytes.
function file(files, name) {
  const coded = files?.[name];
  if (typeof coded !== "string") throw new Error(`${CODE_TOOL}: the result's _meta lacks ${name}`);
  return Uint8Array.fromBase64(coded);
}

async function load(bytes) {
  const url = URL.createObjectURL(new Blob([bytes], { type: "text/javascript" }));
  try { return await import(url); } finally { URL.revokeObjectURL(url); }
}

export async function start({ meta, files, request, restart, carried }) {
  const bytes = file(files, "module");
  const scripts = SCRIPTS.map((name) => file(files, name));
  document.body.innerHTML = new TextDecoder().decode(file(files, "view.html"));
  const [{ Module }, { mount }, page, { open }] = await Promise.all(scripts.map(load));
  const { canvas, show, enable, sceneTree, svg, listen } = page;
  let mounted = null;
  let held = []; // the socket's notifications before the module is mounted, then null
  let fetching = false;
  let ended = false;
  const notify = (method, paramsJSON) => {
    if (held) held.push([method, paramsJSON]);
    else mounted.notify(method, paramsJSON);
  };
  const onCode = (code) => {
    if (fetching || ended || code === meta.code) return;
    fetching = true;
    socket.call("tools/call", { name: CODE_TOOL, arguments: {} }).then((r) => {
      if ("error" in r) throw new Error(`${CODE_TOOL}: ${JSON.stringify(r.error)}`);
      if (r.result?.isError)
        throw new Error(`${CODE_TOOL}: ${(r.result.content ?? []).filter((c) => c.type === "text").map((c) => c.text).join(" ")}`);
      if (!ended) restart(r.result, socket.carried());
    }).catch((e) => console.error(e)).finally(() => { fetching = false; });
  };
  const socket = open({ request, notify, onCode, carried });
  const rpc = async (method, params) => {
    const p = JSON.parse(params);
    if (method === "view/resume") {
      socket.resume(p);
      return JSON.stringify({ result: {} });
    }
    const r = await (method === "tools/call" ? socket.call(method, p) : request(method, p));
    return JSON.stringify("result" in r ? { result: r.result } : { error: r.error });
  };
  try {
    mounted = await mount(canvas, {
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
  } catch (e) {
    socket.dispose();
    throw e;
  }
  if (carried?.resume != null) mounted.notify("view/resume", JSON.stringify(carried.resume));
  for (const [method, paramsJSON] of held) mounted.notify(method, paramsJSON);
  held = null;
  const listening = new AbortController();
  listen(mounted, listening.signal);
  return {
    ...mounted,
    dispose() {
      ended = true;
      listening.abort();
      socket.dispose();
      mounted.dispose();
    },
  };
}
