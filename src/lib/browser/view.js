// view.js - the part of an MCP App's view that changes with a release, which
// app.js imports from the server's code and starts. start(host), host {meta,
// files, request, restart, carried, initialized}, asks for full screen at its
// first start (mode below), puts view.html's style and markup in place of the
// page's body, imports turn.js, viewport.js, page.js and socket.js from blob:
// URLs after it, since page.js reads the document as it loads, opens the view's
// socket (socket.js), mounts the module with an rpc bridge, and listens to the
// page's field, key buttons and drops and to its tree toggle, which in a
// narrow frame opens and closes the tree over the canvas (view.html). It
// resolves with the viewport, whose dispose also ends the socket and removes
// the page's listeners, so a restart leaves none of this view's.
//
//   rpc    a module's tools/call rpc goes over the socket (socket.call), its
//          view/resume P is kept by the socket and answered {} at once, and
//          every other method is host.request(method, params); the response's
//          result or error answers the record
//   notify each server notification but view/code and view/alive, which are
//          the shell's own, and view/open each time the socket opens, reaches
//          the module as the viewport's notify, the path host
//          notifications take, held until the module is mounted; when carried
//          is set, the module first receives view/resume with its resume; a
//          mounted module receives view/restart {} before a restart (code
//          below)
//   code   view/code whose code is not meta.code restarts the view from
//          view_code's result, by the restart machine below
//   mode   ui/request-display-mode {mode: "fullscreen"} through host.request,
//          once, at the first start; a restart keeps the mode the view is in.
//          It is not asked when initialized.hostContext.availableDisplayModes
//          is a list without "fullscreen"; with no such list there is nothing
//          to check against, and it is asked. A failure is logged
//   focus  a restart keeps keyboard focus on the canvas or an element with an
//          id: when one of them has it as the restart's start replaces the
//          body, the new body's canvas, or its element with that id, takes
//          it, with preventScroll, once the module is mounted, unless focus
//          left the view's window meanwhile or an element of the new body
//          has it by then; focus on an element without an id (a key button,
//          the field, a tree row) is not kept
//
// The restart is a machine over phase, step(input, value) its one entry. The
// states are
//   idle      no restart is under way
//   fetching  tools/call view_code {} is on the socket
//   asking    the mounted module's view/restart {} turn has not settled; the
//             restart waits for that turn and every request it makes, so a
//             module answers view/restart with view/resume alone
//   ended     view.js disposed the view
// and the inputs
//   code      view/code's code
//   fetched   view_code's result
//   failed    the call's failure, an error or a tool error (isError)
//   told      the view/restart turn settled; notify never rejects, as viewport.js reports a turn's failure through onText
//   dispose   view.js's dispose
// The table:
//   idle      code: none when it is meta.code, else fetching
//             dispose: ended
//   fetching  fetched: asking, notifying view/restart {}, or told at once when
//             no module is mounted
//             failed: logged, idle; the view runs on, so the next socket's
//             view/code tries again
//             dispose: ended
//   asking    told: idle, then host.restart(result, socket.carried()), so what
//             the module told with view/resume in that turn is what the next
//             view receives; app.js disposes this view in it, ending it,
//             unless app.js is not running
//             dispose: ended
//   ended     every input none
// code is none in fetching and asking, so one fetch runs at a time; every other
// pairing in the table is one that cannot arrive, named none.
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

// The element that has focus, as a selector: "canvas" for the canvas, else
// its id; null when no element of the view has it or it has no id.
function focused() {
  const e = document.activeElement;
  if (e === null || e === document.body) return null;
  if (e.localName === "canvas") return "canvas";
  return e.id === "" ? null : `#${CSS.escape(e.id)}`;
}

async function load(bytes) {
  const url = URL.createObjectURL(new Blob([bytes], { type: "text/javascript" }));
  try { return await import(url); } finally { URL.revokeObjectURL(url); }
}

// Asks for full screen unless the host's modes are known and lack it.
function fullscreen(request, initialized) {
  const modes = initialized?.hostContext?.availableDisplayModes;
  if (Array.isArray(modes) && !modes.includes("fullscreen")) return;
  request("ui/request-display-mode", { mode: "fullscreen" }).then((r) => {
    if ("error" in r) throw new Error(`ui/request-display-mode: ${JSON.stringify(r.error)}`);
  }).catch((e) => console.error(e));
}

export async function start({ meta, files, request, restart, carried, initialized }) {
  if (carried === null) fullscreen(request, initialized);
  const bytes = file(files, "module");
  const scripts = SCRIPTS.map((name) => file(files, name));
  const focus = carried === null ? null : focused();
  document.body.innerHTML = new TextDecoder().decode(file(files, "view.html"));
  let left = false; // focus left the view's window after the body was replaced
  const leave = () => { left = true; };
  window.addEventListener("blur", leave);
  try {
    const [{ Module }, { mount }, page, { open }] = await Promise.all(scripts.map(load));
    const { canvas, show, enable, sceneTree, svg, listen } = page;
    let mounted = null;
    let held = []; // the socket's notifications before the module is mounted, then null
    const notify = (method, paramsJSON) => {
      if (held) held.push([method, paramsJSON]);
      else mounted.notify(method, paramsJSON);
    };

    // The restart machine (header).
    let phase = "idle";
    const step = (input, value) => restarts[phase][input](value);
    const fetching = () => {
      phase = "fetching";
      socket.call("tools/call", { name: CODE_TOOL, arguments: {} }).then((r) => {
        if ("error" in r) throw new Error(`${CODE_TOOL}: ${JSON.stringify(r.error)}`);
        if (r.result?.isError)
          throw new Error(`${CODE_TOOL}: ${(r.result.content ?? []).filter((c) => c.type === "text").map((c) => c.text).join(" ")}`);
        return r.result;
      }).then((result) => step("fetched", result), (e) => step("failed", e));
    };
    const asking = (result) => {
      phase = "asking";
      if (mounted === null) step("told", result);
      else mounted.notify("view/restart", "{}").then(() => step("told", result));
    };
    const told = (result) => {
      phase = "idle";
      restart(result, socket.carried());
    };
    const failed = (e) => {
      console.error(e);
      phase = "idle";
    };
    const ended = () => { phase = "ended"; };
    const none = () => {};
    // phase -> input -> transition(value); every input in every phase.
    const restarts = {
      idle: {
        code: (code) => { if (code !== meta.code) fetching(); },
        fetched: none, failed: none, told: none,
        dispose: ended,
      },
      fetching: {
        code: none,
        fetched: asking,
        failed,
        told: none,
        dispose: ended,
      },
      asking: { code: none, fetched: none, failed: none, told, dispose: ended },
      ended: { code: none, fetched: none, failed: none, told: none, dispose: none },
    };

    const socket = open({ request, notify, onCode: (code) => step("code", code), carried });
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
    const toggle = document.querySelector("#tree-toggle");
    const side = document.querySelector("#side");
    toggle.addEventListener("click", () => {
      toggle.setAttribute("aria-expanded", String(side.classList.toggle("open")));
    }, { signal: listening.signal });
    if (focus !== null && !left && document.activeElement === document.body)
      document.querySelector(focus)?.focus({ preventScroll: true });
    return {
      ...mounted,
      dispose() {
        step("dispose");
        listening.abort();
        socket.dispose();
        mounted.dispose();
      },
    };
  } finally {
    window.removeEventListener("blur", leave);
  }
}
