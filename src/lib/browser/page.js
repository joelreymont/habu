// page.js - a host page's view of a mounted module, shared by host.js
// (index.html) and app.js (app.html): both pages hold a canvas, a #tree, a
// #text, a form's input and a #file picker under a #viewport. This file shows
// the module's text, tree and SVG images, enables its field, and sends the
// view the field's text and each file picked or dropped on the page. It
// imports nothing, so app.html can load it from a blob: URL.
//
// SVG is shown through an img element, whose processing mode disables scripts,
// interaction and external references; supplied markup is never inserted into
// the document.

export const canvas = document.querySelector("canvas");
const field = document.querySelector("form input");
const picker = document.querySelector("#file");
const shown = document.querySelector("#text");
const tree = document.querySelector("#tree");
const frame = document.querySelector("#viewport");

export function show(s, error) {
  shown.textContent = s;
  shown.className = error ? "error" : "";
}

export function fail(e) {
  show(String(e), true);
  console.error(e);
}

export function enable(enabled) {
  field.disabled = !enabled;
}

// Native buttons remain keyboard reachable. A tree sent again with only its
// selection changed keeps its buttons; a changed tree is rebuilt. A rebuild
// keeps each row's expansion by key, a row the module sends again keeping its
// key: a row open before stays open, and a new row starts open exactly when
// its children have been sent. When focus was on a row's toggle or label, the
// same control of the row with the same key has it after the rebuild, where
// the row still has that control. A row whose more is true shows a toggle with
// no children; opening it calls onControl(key, true), once per opening, and
// the module answers with its children. A selection calls
// onControl(key, false).
export function sceneTree(onControl) {
  let snapshot = null;
  let selected = 0;
  let buttons = new Map(); // a row key -> its label button
  let toggles = new Map(); // a row key -> its toggle
  let expanded = new Map(); // a row key -> whether its children are shown

  const select = (key) => {
    buttons.get(selected)?.setAttribute("aria-pressed", "false");
    selected = key;
    buttons.get(key)?.setAttribute("aria-pressed", "true");
  };

  // The toggle of a row shown with branch, its children's list.
  const toggle = (row, descendants, branch, open) => {
    const button = document.createElement("button");
    button.type = "button";
    const expand = (open) => {
      branch.hidden = !open;
      button.textContent = open ? "▾" : "▸";
      const verb = open ? "Collapse" : "Expand";
      button.setAttribute("aria-label", `${verb} ${row.label}`);
      button.setAttribute("aria-expanded", String(open));
      expanded.set(row.key, open);
    };
    expand(open);
    button.addEventListener("click", () => {
      expand(branch.hidden);
      if (!branch.hidden && row.more && !descendants.length)
        onControl(row.key, true);
    });
    return button;
  };

  // The key of the row whose control in controls has focus, or undefined.
  const focused = (controls) => {
    for (const [key, button] of controls)
      if (button === document.activeElement) return key;
  };

  return (rows, key) => {
    const next = JSON.stringify(rows.map(({ key, parent, label, more }) =>
      [key, parent, label, more === true]));
    if (next !== snapshot) {
      const children = new Map([[0, []]]);
      for (const row of rows) {
        if (!children.has(row.key)) children.set(row.key, []);
        children.get(row.parent).push(row);
      }
      const label = focused(buttons);
      const opener = focused(toggles);
      const known = expanded;
      expanded = new Map();
      const nextButtons = new Map();
      const nextToggles = new Map();
      const build = (parent) => {
        const list = document.createElement("ul");
        for (const row of children.get(parent)) {
          const item = document.createElement("li");
          const line = document.createElement("div");
          line.className = "tree-row";
          const descendants = children.get(row.key);
          if (descendants.length || row.more) {
            const branch = build(row.key);
            const open = known.get(row.key) ?? descendants.length > 0;
            const button = toggle(row, descendants, branch, open);
            nextToggles.set(row.key, button);
            line.append(button);
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
          button.addEventListener("click", () => onControl(row.key, false));
          line.append(button);
          nextButtons.set(row.key, button);
          list.append(item);
        }
        return list;
      };
      tree.replaceChildren(build(0));
      buttons = nextButtons;
      toggles = nextToggles;
      snapshot = next;
      (label === undefined ? toggles.get(opener) : buttons.get(label))?.focus();
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

export function svg(bytes) {
  clearImage();
  if (!bytes) return;
  const next = document.createElement("img");
  next.alt = "SVG image";
  next.onerror = () => {
    if (image === next) fail(new Error("SVG image could not be decoded"));
  };
  image = next;
  next.src = URL.createObjectURL(new Blob([bytes], { type: "image/svg+xml" }));
  frame.append(next);
}

// Enter submits the field's form: its text is sent and the field cleared at
// once, though the turn may wait minutes for an answer. Submission, not a key,
// sends it, since the Enter that ends an input method's composition submits
// nothing. The field stays disabled until the module starts, as an Enter with
// no listener would submit the page.
export function listen(view) {
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
