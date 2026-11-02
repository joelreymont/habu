// host.js - the browser host page: index.html's canvas runs /module.wasm
// through viewport.js, which draws with WebGPU and sends canvas input, and
// page.js shows what the module sends and gives it the field's text and each
// file picked or dropped. A failure to start or run is shown in text. Only a
// browser checks this file: host-cli.mjs runs the same turns with no DOM or
// GPU.
import { Module } from "./turn.js";
import { mount } from "./viewport.js";
import { canvas, show, fail, enable, sceneTree, svg, listen } from "./page.js";

let rpcId = 0;

// A tool name as an Mcp-Name value: as it is when every byte is visible ASCII,
// else =?base64?...?= of its UTF-8 bytes.
function mcpName(name) {
  return /^[!-~]*$/.test(name) ? name : `=?base64?${new TextEncoder().encode(name).toBase64()}?=`;
}

// A module's rpc: tools/call goes to the page origin's MCP endpoint as a
// JSON-RPC 2.0 request; the page answers the rest itself. A JSON-RPC response
// to it answers with its result or error whatever the HTTP status; any other
// body rejects.
async function rpc(method, params) {
  if (method === "ui/update-model-context") return '{"result":{}}';
  if (method !== "tools/call") return '{"error":{"code":-32601,"message":"Method not found"}}';
  const id = ++rpcId;
  const call = JSON.parse(params);
  call._meta = {
    ...call._meta,
    "io.modelcontextprotocol/protocolVersion": "2026-07-28",
    "io.modelcontextprotocol/clientCapabilities": {},
  };
  // MCP 2026-07-28's HTTP transport requires both Accept types, the version
  // header, Mcp-Method naming the method and, for a tools/call, Mcp-Name
  // naming params.name; params._meta repeats the version beside the client's
  // capabilities.
  const headers = {
    "Content-Type": "application/json",
    Accept: "application/json, text/event-stream",
    "MCP-Protocol-Version": "2026-07-28",
    "Mcp-Method": "tools/call",
  };
  if (typeof call.name === "string") headers["Mcp-Name"] = mcpName(call.name);
  const r = await fetch("/mcp", {
    method: "POST",
    headers,
    body: JSON.stringify({ jsonrpc: "2.0", id, method: "tools/call", params: call }),
  });
  const reply = await r.json().catch(() => null);
  if (reply === null || typeof reply !== "object" || reply.jsonrpc !== "2.0" || reply.id !== id ||
      ("result" in reply) === ("error" in reply))
    throw new Error(`/mcp: ${r.status}: the body is not a JSON-RPC response`);
  return JSON.stringify("result" in reply ? { result: reply.result } : { error: reply.error });
}

async function main() {
  const view = await mount(canvas, {
    Module,
    module: "/module.wasm",
    fetch: (url, init) => fetch(url, init),
    rpc,
    onTree: sceneTree((key) => view.post({ kind: "control", key })),
    onText: show,
    onSvg: svg,
    onInput: enable,
  });
  listen(view);
}

main().catch(fail);
