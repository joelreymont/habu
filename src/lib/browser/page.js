// page.js - a host page's view of a mounted module, shared by host.js
// (index.html) and view.js (view.html): both pages hold a canvas under a
// #viewport, a #tree, a #text and a form's input, and no file picker; view.html
// also holds buttons under #viewport whose data-key names a key. This file
// shows the module's text, tree and SVG images, zooms and pans a shown image,
// enables its field, and sends the view the field's text, each key button's
// key while no image shows and each file dropped on the page. It imports nothing, so view.js can import it from a blob: URL.
//
// SVG is shown through an img element, whose processing mode disables scripts,
// interaction and external references; supplied markup is never inserted into
// the document.

export const canvas = document.querySelector("canvas");
const field = document.querySelector("form input");
const shown = document.querySelector("#text");
const tree = document.querySelector("#tree");
const frame = document.querySelector("#viewport");
const keys = frame.querySelectorAll("button[data-key]");

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

// The nearest ancestor of el that scrolls its content, or null.
const scroller = (el) => {
  const scrolls = (o) => o === "auto" || o === "scroll";
  for (let up = el.parentElement; up; up = up.parentElement) {
    const { overflowX, overflowY } = getComputedStyle(up);
    if ((scrolls(overflowY) && up.scrollHeight > up.clientHeight) ||
        (scrolls(overflowX) && up.scrollWidth > up.clientWidth)) return up;
  }
  return null;
};

// How far a scroller's start edge moves so that a..b lies within its visible
// lo..hi, keeping a in view when a..b is the longer: 0 when it lies within.
const nearest = (a, b, lo, hi) => (a < lo ? a - lo : b > hi ? Math.min(b - hi, a - lo) : 0);

// el scrolled into view within its scroller's visible area only, never the
// page or the page that holds this one.
const reveal = (el) => {
  const box = scroller(el);
  if (!box) return;
  const r = el.getBoundingClientRect(), b = box.getBoundingClientRect();
  const top = b.top + box.clientTop, left = b.left + box.clientLeft;
  box.scrollTop += nearest(r.top, r.bottom, top, top + box.clientHeight);
  box.scrollLeft += nearest(r.left, r.right, left, left + box.clientWidth);
};

// Native buttons remain keyboard reachable. A tree sent again with only its
// selection changed keeps its buttons; a changed tree is rebuilt. A rebuild
// keeps each row's expansion by key, a row the module sends again keeping its
// key: a row open before stays open, and a new row starts open as its open
// member says, else exactly when its children have been sent. When focus was on a row's toggle or label, the
// same control of the row with the same key has it after the rebuild, where
// the row still has that control. A row whose more is true shows a toggle with
// no children; opening it calls onControl(key, true), once per opening, and
// the module answers with its children. A selection calls
// onControl(key, false). A pressed key that differs from the one pressed before
// opens every ancestor of its row, kept open across rebuilds as any opened row
// is, and scrolls the row's label into view within the tree's scroller, never
// the page; the same pressed key sent again changes nothing, so an ancestor
// closed since stays closed.
export function sceneTree(onControl) {
  let snapshot = null;
  let selected = 0;
  let buttons = new Map(); // a row key -> its label button
  let toggles = new Map(); // a row key -> its toggle
  let opens = new Map(); // a row key -> the function that opens or closes it
  let parents = new Map(); // a built row's key -> its parent's key
  let expanded = new Map(); // a row key -> whether its children are shown

  const select = (key) => {
    buttons.get(selected)?.setAttribute("aria-pressed", "false");
    const moved = key !== selected;
    selected = key;
    const button = buttons.get(key);
    button?.setAttribute("aria-pressed", "true");
    if (!moved || !button) return;
    // Parents hold only rows built from the root, so this walk ends there.
    for (let up = parents.get(key); up; up = parents.get(up))
      opens.get(up)(true);
    reveal(button);
  };

  // The toggle of a row shown with branch, its children's list, and the
  // function that opens or closes it.
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
    return [button, expand];
  };

  // The key of the row whose control in controls has focus, or undefined.
  const focused = (controls) => {
    for (const [key, button] of controls)
      if (button === document.activeElement) return key;
  };

  return (rows, key) => {
    const next = JSON.stringify(rows.map(({ key, parent, label, more, open }) =>
      [key, parent, label, more === true, open ?? null]));
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
      const nextOpens = new Map();
      const nextParents = new Map();
      const build = (parent) => {
        const list = document.createElement("ul");
        for (const row of children.get(parent)) {
          const item = document.createElement("li");
          const line = document.createElement("div");
          line.className = "tree-row";
          const descendants = children.get(row.key);
          nextParents.set(row.key, parent);
          if (descendants.length || row.more) {
            const branch = build(row.key);
            const open = known.get(row.key) ?? row.open ?? descendants.length > 0;
            const [button, expand] = toggle(row, descendants, branch, open);
            nextToggles.set(row.key, button);
            nextOpens.set(row.key, expand);
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
      opens = nextOpens;
      parents = nextParents;
      snapshot = next;
      (label === undefined ? toggles.get(opener) : buttons.get(label))?.focus();
    }
    select(key);
  };
}

let image = null;
// The shown image's view: the frame's point p shows the image's fitted point
// (p - [dx, dy]) / scale, scale lying in 1..32.
let scale = 1, dx = 0, dy = 0;
let drag = null; // the panning pointer's id and last client point, or null

const place = () => {
  image.style.transform = `translate(${dx}px, ${dy}px) scale(${scale})`;
};

// The image scaled by k about the frame's point (x, y), its scale held in
// 1..32, so that point stays where it is.
const zoom = (k, x, y) => {
  const next = Math.min(32, Math.max(1, scale * k));
  dx = x - (x - dx) * next / scale;
  dy = y - (y - dy) * next / scale;
  scale = next;
  place();
};

// A key button's key acting on the shown image.
const act = (key) => {
  const x = frame.clientWidth / 2, y = frame.clientHeight / 2;
  if (key === "+") zoom(1.25, x, y);
  else if (key === "-") zoom(1 / 1.25, x, y);
  else if (key === "f") { scale = 1; dx = 0; dy = 0; place(); }
};

function clearImage() {
  if (!image) return;
  const old = image;
  image = null;
  drag = null;
  old.remove();
}

// A shown image stands in the canvas's place, fitted to the frame; null
// removes it. While it shows, the key buttons act on it instead of sending
// their keys (listen): "+" scales it by 1.25 and "-" by 1/1.25 about the
// frame's centre, and "f" fits it again. The wheel over it scales it by 1.1 per
// 100 CSS pixels of delta about the pointer, and a primary-button drag pans it.
// Its scale stays within 1..32. A CSS transform on the img, which #viewport
// clips, does this; the module is not told.
export function svg(bytes) {
  clearImage();
  if (!bytes) return;
  const next = document.createElement("img");
  next.alt = "SVG image";
  next.onerror = () => {
    if (image === next) fail(new Error("SVG image could not be decoded"));
  };
  image = next;
  scale = 1; dx = 0; dy = 0;
  // A native image drag would take the pointer from the pan.
  next.draggable = false;
  next.style.transformOrigin = "0 0";
  next.style.touchAction = "none";
  place();
  next.addEventListener("wheel", (e) => {
    e.preventDefault();
    const r = frame.getBoundingClientRect();
    // deltaY in CSS pixels, read as viewport.js reads a wheel: a line is the
    // img's line height, else 16 px, and a page is the frame's height.
    const line = parseFloat(getComputedStyle(next).lineHeight) || 16;
    const unit = e.deltaMode === 2 ? r.height : e.deltaMode === 1 ? line : 1;
    zoom(1.1 ** (-e.deltaY * unit / 100), e.clientX - r.left - frame.clientLeft, e.clientY - r.top - frame.clientTop);
  }, { passive: false });
  next.addEventListener("pointerdown", (e) => {
    if (e.button !== 0) return;
    next.setPointerCapture(e.pointerId);
    drag = { id: e.pointerId, x: e.clientX, y: e.clientY };
  });
  next.addEventListener("pointermove", (e) => {
    if (drag?.id !== e.pointerId) return;
    dx += e.clientX - drag.x;
    dy += e.clientY - drag.y;
    drag.x = e.clientX;
    drag.y = e.clientY;
    place();
  });
  // Capture ends with the pointer's release or cancellation.
  next.addEventListener("lostpointercapture", (e) => {
    if (drag?.id === e.pointerId) drag = null;
  });
  // A data: URL, the one image source beyond 'self' that an MCP Apps host's
  // img-src grants (ext-apps 2026-01-26, Security); blob: images are rejected.
  next.src = `data:image/svg+xml;base64,${bytes.toBase64()}`;
  // Before the key buttons, which then lie over it.
  canvas.after(next);
}

// Enter submits the field's form: its text is sent and the field cleared at
// once, though the turn may wait minutes for an answer. Submission, not a key,
// sends it, since the Enter that ends an input method's composition submits
// nothing. The field stays disabled until the module starts, as an Enter with
// no listener would submit the page. A click on a key button acts on the shown
// image (svg), or with no image sends its data-key as a key turn. signal, when
// given, removes the listeners once it aborts.
export function listen(view, signal) {
  for (const button of keys)
    button.addEventListener("click", () => {
      const key = button.dataset.key;
      if (image) act(key);
      else view.post({ kind: "key", key });
    }, { signal });
  field.form.addEventListener("submit", (e) => {
    e.preventDefault();
    if (field.disabled) return;
    const text = field.value;
    field.value = "";
    view.post({ kind: "typed", text });
  }, { signal });
  document.addEventListener("dragover", (e) => e.preventDefault(), { signal });
  document.addEventListener("drop", (e) => {
    e.preventDefault();
    for (const file of e.dataTransfer.files) view.file(file);
  }, { signal });
}
