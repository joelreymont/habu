// host.js - the browser host page: index.html's canvas runs /module.wasm
// through viewport.js, which draws with WebGPU and sends canvas input; this
// file shows the module's SVG images, text and tree, and sends text from the
// field and selected tree rows. A failure to start or run is shown in text.
//
// SVG is shown through an img element, whose processing mode disables scripts,
// interaction and external references; supplied markup is never inserted into
// the document. Only a browser checks this file: host-cli.mjs runs the same
// turns with no DOM or GPU.
import { mount } from "./viewport.js";

const canvas = document.querySelector("canvas");
const field = document.querySelector("input");
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

async function main() {
  const view = await mount(canvas, {
    module: "/module.wasm",
    fetch: (url, init) => fetch(url, init),
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
}

main().catch(fail);
