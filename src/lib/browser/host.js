// host.js - the browser host page: index.html's canvas runs /module.wasm
// through viewport.js, which draws with WebGPU and sends canvas input; this
// file shows the module's SVG images, text and tree, and sends text from the
// field and selected tree rows, and gives it each file picked or dropped. A
// failure to start or run is shown in text.
//
// SVG is shown through an img element, whose processing mode disables scripts,
// interaction and external references; supplied markup is never inserted into
// the document. Only a browser checks this file: host-cli.mjs runs the same
// turns with no DOM or GPU.
import { Module } from "./turn.js";
import { mount } from "./viewport.js";

const canvas = document.querySelector("canvas");
const field = document.querySelector("form input");
const picker = document.querySelector("#file");
const shown = document.querySelector("#text");
const tree = document.querySelector("#tree");
const frame = document.querySelector("#viewport");

function show(s, error) {
  shown.textContent = s;
  shown.className = error ? "error" : "";
}

function fail(e) {
  show(String(e), true);
  console.error(e);
}

// Native buttons remain keyboard reachable. Rows retain their expansion and
// focus when the module sends the same tree again with a different selection.
function sceneTree(onSelect) {
  let snapshot = null;
  let selected = 0;
  let buttons = new Map();

  const select = (key) => {
    buttons.get(selected)?.setAttribute("aria-pressed", "false");
    selected = key;
    buttons.get(key)?.setAttribute("aria-pressed", "true");
  };

  return (rows, key) => {
    const next = JSON.stringify(rows.map(({ key, parent, label }) => [key, parent, label]));
    if (next !== snapshot) {
      const children = new Map([[0, []]]);
      for (const row of rows) {
        if (!children.has(row.key)) children.set(row.key, []);
        children.get(row.parent).push(row);
      }
      const nextButtons = new Map();
      const build = (parent) => {
        const list = document.createElement("ul");
        for (const row of children.get(parent)) {
          const item = document.createElement("li");
          const line = document.createElement("div");
          line.className = "tree-row";
          const descendants = children.get(row.key);
          if (descendants.length) {
            const branch = build(row.key);
            const toggle = document.createElement("button");
            toggle.type = "button";
            toggle.textContent = "▾";
            toggle.setAttribute("aria-label", `Collapse ${row.label}`);
            toggle.setAttribute("aria-expanded", "true");
            toggle.addEventListener("click", () => {
              const open = branch.hidden;
              branch.hidden = !open;
              toggle.textContent = open ? "▾" : "▸";
              toggle.setAttribute("aria-label", `${open ? "Collapse" : "Expand"} ${row.label}`);
              toggle.setAttribute("aria-expanded", String(open));
            });
            line.append(toggle);
            item.append(line, branch);
          } else {
            const spacer = document.createElement("span");
            spacer.className = "tree-spacer";
            line.append(spacer);
            item.append(line);
          }
          const button = document.createElement("button");
          button.type = "button";
          button.textContent = row.label;
          button.addEventListener("click", () => onSelect(row.key));
          line.append(button);
          nextButtons.set(row.key, button);
          list.append(item);
        }
        return list;
      };
      tree.replaceChildren(build(0));
      buttons = nextButtons;
      snapshot = next;
    }
    select(key);
  };
}

let image = null;

function clearImage() {
  if (!image) return;
  const old = image;
  image = null;
  old.remove();
  URL.revokeObjectURL(old.src);
}

function svg(bytes) {
  clearImage();
  if (!bytes) return;
  const next = document.createElement("img");
  next.alt = "SVG image";
  next.onerror = () => {
    if (image === next) fail(new Error("SVG image could not be decoded"));
  };
  image = next;
  // An img uses SVG's secure image mode: no scripts, external resources or
  // document interaction. Never insert the supplied markup into the DOM.
  next.src = URL.createObjectURL(new Blob([bytes], { type: "image/svg+xml" }));
  frame.append(next);
}

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
    onInput: (enabled) => { field.disabled = !enabled; },
  });
  // Enter submits the field's form: its text is sent and the field cleared at
  // once, though the turn may wait minutes for an answer. Submission, not a
  // key, sends it, since the Enter that ends an input method's composition
  // submits nothing. The field stays disabled until the module starts, as an
  // Enter with no listener would submit the page.
  field.form.addEventListener("submit", (e) => {
    e.preventDefault();
    if (field.disabled) return;
    const text = field.value;
    field.value = "";
    view.post({ kind: "typed", text });
  });
  picker.addEventListener("change", () => {
    for (const file of picker.files) view.file(file);
    picker.value = "";
  });
  document.addEventListener("dragover", (e) => e.preventDefault());
  document.addEventListener("drop", (e) => {
    e.preventDefault();
    for (const file of e.dataTransfer.files) view.file(file);
  });
}

main().catch(fail);
