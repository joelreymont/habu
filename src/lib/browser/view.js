// view.js - the part of an MCP App's view that changes with a release, which
// app.js imports from the server's code and starts. start puts view.html's
// style and markup in place of the page's body, imports turn.js, viewport.js
// and page.js from blob: URLs after it, since page.js reads the document as it
// loads, mounts the module with an rpc bridge over the host's requests, and
// listens to the page's field and drops.
//
//   rpc    a module's rpc record is host.request(method, params); its
//          response's result or error answers the record
//
// page.js shows what the module sends and gives it the field's text and each
// file dropped anywhere on the page. The module is the one path the
// viewport's fetch answers; every other path rejects.
const MODULE = "module.wasm";
const CODE_TOOL = "view_code";
const SCRIPTS = ["turn.js", "viewport.js", "page.js"];

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

export async function start({ files, request }) {
  const bytes = file(files, "module");
  const scripts = SCRIPTS.map((name) => file(files, name));
  document.body.innerHTML = new TextDecoder().decode(file(files, "view.html"));
  const [{ Module }, { mount }, page] = await Promise.all(scripts.map(load));
  const { canvas, show, enable, sceneTree, svg, listen } = page;
  const rpc = async (method, params) => {
    const r = await request(method, JSON.parse(params));
    return JSON.stringify("result" in r ? { result: r.result } : { error: r.error });
  };
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
  listen(mounted);
  return mounted;
}
