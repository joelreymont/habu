// host.js - the browser host page: index.html's canvas runs /module.wasm
// through viewport.js, which draws with WebGPU and sends canvas input, and
// page.js shows what the module sends, its toolbar included, and gives it the
// field's text, each toolbar press, value and shortcut and each file dropped,
// and mcp.js sends its tool calls to the page origin's
// MCP endpoint. A failure to start or run is shown in text. Only a browser
// checks this file: host-cli.mjs runs the same turns with no DOM or GPU.
import { Module } from "./turn.js";
import { mcpRpc } from "./mcp.js";
import { mount } from "./viewport.js";
import { canvas, show, fail, enable, sceneTree, toolbar, svg, labels, listen } from "./page.js";

const rpc = mcpRpc("/mcp");

async function main() {
  const view = await mount(canvas, {
    Module,
    module: "/module.wasm",
    fetch: (url, init) => fetch(url, init),
    rpc,
    onTree: sceneTree((key, opened) =>
      view.post({ kind: "control", key, opened })),
    onToolbar: toolbar((event) => view.post(event)),
    onText: show,
    onSvg: svg,
    onInput: enable,
    onLabels: labels,
  });
  listen(view);
}

main().catch(fail);
