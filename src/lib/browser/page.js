// page.js - a host page's view of a mounted module, shared by host.js
// (index.html) and view.js (view.html): both pages hold a canvas and a
// #toolbar under a #viewport, a #tree, a #text and a form's input, and no file
// picker; view.html also holds #keys, buttons under #viewport whose data-key
// names a zoom of a shown image. This file shows the module's text, tree,
// toolbar, labels and SVG images, zooms and pans a shown image, enables its
// field, and sends the view the field's text, each toolbar press, value and
// shortcut, and each file dropped on the page. It imports nothing, so view.js
// can import it from a blob: URL.
//
// SVG is shown through an img element, whose processing mode disables scripts,
// interaction and external references; supplied markup is never inserted into
// the document: a toolbar icon is a path element made here whose d the host
// checked as path data (turn.js record 18).

export const canvas = document.querySelector("canvas");
const field = document.querySelector("form input");
const shown = document.querySelector("#text");
const tree = document.querySelector("#tree");
const frame = document.querySelector("#viewport");
const bar = document.querySelector("#toolbar");
const zooms = document.querySelector("#keys");
const keys = frame.querySelectorAll("#keys button[data-key]");

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
// the row still has that control. A row whose dim is true has class dim on its
// line and is otherwise as any row. A row whose more is true shows a toggle with
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
    const next = JSON.stringify(rows.map(({ key, parent, label, more, open, dim }) =>
      [key, parent, label, more === true, open ?? null, dim === true]));
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
          line.className = row.dim ? "tree-row dim" : "tree-row";
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

const SVG_NS = "http://www.w3.org/2000/svg";
// The help button's icon: a question mark in a circle.
const HELP_ICON = "M12 22a10 10 0 1 0 0-20a10 10 0 1 0 0 20M9.1 9a3 3 0 0 1 5.8 1c0 2-3 3-3 3M12 17h.01";

// An icon of path data d at 16 CSS px in a 24 x 24 box, stroked in
// currentColor at width 2 with round caps and joins and no fill.
const icon = (d) => {
  const svg = document.createElementNS(SVG_NS, "svg");
  for (const [name, value] of [["viewBox", "0 0 24 24"], ["width", "16"], ["height", "16"],
    ["fill", "none"], ["stroke", "currentColor"], ["stroke-width", "2"],
    ["stroke-linecap", "round"], ["stroke-linejoin", "round"], ["aria-hidden", "true"]])
    svg.setAttribute(name, value);
  const path = document.createElementNS(SVG_NS, "path");
  path.setAttribute("d", d);
  svg.append(path);
  return svg;
};

// A row's face: its icon, else its label as text.
const face = (row) => (row.icon === undefined ? document.createTextNode(row.label) : icon(row.icon));

// "label (shortcut)", or the label when the row has no shortcut.
const tip = (row) => (row.shortcut === undefined ? row.label : `${row.label} (${row.shortcut})`);

// pressed as aria-pressed, which a row that does not state it lacks.
const markPressed = (el, row) => {
  if (typeof row.pressed === "boolean") el.setAttribute("aria-pressed", String(row.pressed));
  else el.removeAttribute("aria-pressed");
};

// Whether el takes typed text, so that a key pressed in it is the text's.
const UNTYPED = new Set(["button", "checkbox", "color", "file", "image", "radio", "range", "reset", "submit"]);
const typing = (el) => el instanceof Element && (el.localName === "textarea" || el.isContentEditable ||
  (el.localName === "input" && !UNTYPED.has(el.type)));

// The page's toolbar state, which listen's key handling reads: the rows last
// stated, the renderer's onEvent, and the open help screen, or null.
let stated = [];
let press = null;
let help = null;

const shortcuts = () => stated.filter((row) => row.shortcut !== undefined);

// The help screen, a modal dialog; closing it, by Escape, ? or its button,
// removes it and returns focus to the element that had it.
const openHelp = () => {
  if (help) return;
  const before = document.activeElement;
  help = document.createElement("dialog");
  help.className = "help";
  help.setAttribute("aria-label", "Keyboard shortcuts");
  // The help screen's content: every row with a shortcut by group, in the
  // toolbar's order, each with its icon, label and key.
  const groups = new Map();
  for (const row of shortcuts()) {
    const name = row.group ?? "";
    if (!groups.has(name)) groups.set(name, []);
    groups.get(name).push(row);
  }
  const title = document.createElement("h2");
  title.textContent = "Keyboard shortcuts";
  const parts = [title];
  for (const [name, rows] of groups) {
    const section = document.createElement("section");
    if (name !== "") {
      const heading = document.createElement("h3");
      heading.textContent = name;
      section.append(heading);
    }
    const list = document.createElement("dl");
    for (const row of rows) {
      const term = document.createElement("dt");
      const key = document.createElement("kbd");
      key.textContent = row.shortcut;
      term.append(key);
      const what = document.createElement("dd");
      if (row.icon !== undefined) what.append(icon(row.icon));
      what.append(row.label);
      list.append(term, what);
    }
    section.append(list);
    parts.push(section);
  }
  const close = document.createElement("button");
  close.type = "button";
  close.textContent = "Close";
  close.addEventListener("click", () => help.close());
  parts.push(close);
  help.replaceChildren(...parts);
  help.addEventListener("close", () => {
    help.remove();
    help = null;
    before?.focus?.({ preventScroll: true });
  });
  document.body.append(help);
  help.showModal();
};

// A key pressed anywhere in the page, seen before its target sees it. A key
// the focused control uses is that control's alone: any key in a field that
// takes typed text (typing), so typing in a text row's field never fires a
// shortcut, and a range's moving keys (RANGE_KEYS) in a range, whose other
// keys still reach the shortcuts. While the help screen is open, ? and
// Escape close it and reach nothing else. Elsewhere, ? opens it while a row
// has a shortcut, and away from the canvas, which sends its own keys to the
// module, a key with no Control, Alt or Meta held that is a row's shortcut
// presses that row.
const RANGE_KEYS = new Set(["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown", "Home", "End", "PageUp", "PageDown"]);
const keyed = (e) => {
  if (e.isComposing) return;
  if (e.target instanceof HTMLInputElement && e.target.type === "range" && RANGE_KEYS.has(e.key)) return;
  if (help) {
    if (e.key === "?" || e.key === "Escape") {
      e.preventDefault();
      e.stopPropagation();
      help.close();
    }
    return;
  }
  if (typing(e.target)) return;
  if (e.key === "?" && shortcuts().length) {
    e.preventDefault();
    e.stopPropagation();
    openHelp();
    return;
  }
  if (e.target === canvas || e.ctrlKey || e.altKey || e.metaKey) return;
  const row = stated.find((row) => row.shortcut === e.key);
  if (!row) return;
  e.preventDefault();
  press?.({ kind: "key", key: row.key });
};

// The renderer of the toolbar record's rows (turn.js record 18) into
// #toolbar, to which [] leaves nothing. Consecutive rows of one group form a
// cluster, a role="group" named by the group, which the stylesheet draws as
// its own rounded pill. A button or item shows its icon, else its label, with
// the tooltip "label (shortcut)" or "label", the label as its accessible name,
// its shortcut as aria-keyshortcuts and a stated pressed as aria-pressed; a
// press calls onEvent({ kind: "key", key }). Consecutive items are one menu
// button, a popover placed below the button's right edge as it opens, whose
// face is its pressed item's, else its first item's, and whose tooltip and
// name are the group; the menu lists each item's icon, label and shortcut. A
// range is an input type=range, beside its icon or else its label, calling
// onEvent({ kind: "set", key, text }) with its value on each input; a text
// row an input type=search, its icon leading inside the field when it has
// one and its label the placeholder and accessible name, calling it on
// change. While any row has a shortcut the toolbar ends with the page's
// own help button, which opens the help screen. Rows that differ from the
// last ones in more than pressed and value rebuild the toolbar, the control
// of the row with the focused one's key taking focus; others update it in
// place, so a dragged range and a focused button stay as they are, and a
// range's or text's value is set only when it differs from the value last
// stated. The page never flips pressed or keeps a value of its own: the
// module states each.
export function toolbar(onEvent) {
  press = onEvent;
  let snapshot = null;
  let controls = new Map(); // a row key -> its control: a button, an item's button or an input
  let menus = new Map(); // an item's key -> its menu's button and face function
  let values = new Map(); // a range's or text's key -> the value last stated
  let helpButton = null;

  const button = (row, cls) => {
    const el = document.createElement("button");
    el.type = "button";
    el.className = cls;
    el.title = tip(row);
    el.setAttribute("aria-label", row.label);
    if (row.shortcut !== undefined) el.setAttribute("aria-keyshortcuts", row.shortcut);
    markPressed(el, row);
    return el;
  };

  const tool = (row) => {
    const el = button(row, "tool");
    el.append(face(row));
    el.addEventListener("click", () => onEvent({ kind: "key", key: row.key }));
    return el;
  };

  // The menu button of consecutive items and its face function.
  const menu = (items) => {
    const group = items[0].group;
    const opener = document.createElement("button");
    opener.type = "button";
    opener.className = "tool";
    opener.title = group;
    opener.setAttribute("aria-label", group);
    opener.setAttribute("aria-expanded", "false");
    const list = document.createElement("div");
    list.className = "menu";
    list.popover = "auto";
    list.setAttribute("aria-label", group);
    opener.popoverTargetElement = list;
    list.addEventListener("beforetoggle", (e) => {
      if (e.newState !== "open") return;
      const r = opener.getBoundingClientRect();
      list.style.top = `${r.bottom + 4}px`;
      list.style.right = `${document.documentElement.clientWidth - r.right}px`;
    });
    list.addEventListener("toggle", (e) => opener.setAttribute("aria-expanded", String(e.newState === "open")));
    for (const row of items) {
      const el = button(row, "menu-item");
      const mark = document.createElement("span");
      mark.className = "menu-icon";
      if (row.icon !== undefined) mark.append(icon(row.icon));
      const label = document.createElement("span");
      label.className = "menu-label";
      label.textContent = row.label;
      el.append(mark, label);
      if (row.shortcut !== undefined) {
        const key = document.createElement("kbd");
        key.textContent = row.shortcut;
        el.append(key);
      }
      el.addEventListener("click", () => {
        list.hidePopover();
        onEvent({ kind: "key", key: row.key });
      });
      controls.set(row.key, el);
    }
    list.append(...items.map((row) => controls.get(row.key)));
    // The face of the pressed item, else the first item's; the group's name
    // when that item has no icon.
    const show = (rows) => {
      const top = rows.find((row) => row.pressed === true) ?? rows[0];
      opener.replaceChildren(top.icon === undefined ? document.createTextNode(group) : icon(top.icon));
    };
    show(items);
    // One owner for every item, so an update shows the menu's face once.
    const owner = { opener, show, keys: items.map((row) => row.key) };
    for (const row of items) menus.set(row.key, owner);
    return [opener, list];
  };

  const input = (row) => {
    const wrap = document.createElement("label");
    wrap.title = row.label;
    const el = document.createElement("input");
    el.setAttribute("aria-label", row.label);
    if (row.kind === "range") {
      wrap.className = "tool-input";
      wrap.append(face(row));
      el.type = "range";
      el.min = String(row.min);
      el.max = String(row.max);
      el.value = String(row.value);
      el.addEventListener("input", () => onEvent({ kind: "set", key: row.key, text: el.value }));
    } else {
      wrap.className = "tool-search";
      if (row.icon !== undefined) wrap.append(icon(row.icon));
      el.type = "search";
      el.placeholder = row.label;
      el.value = row.value ?? "";
      el.addEventListener("change", () => onEvent({ kind: "set", key: row.key, text: el.value }));
    }
    values.set(row.key, row.value);
    controls.set(row.key, el);
    wrap.append(el);
    return wrap;
  };

  const build = (rows) => {
    const at = document.activeElement;
    let focused;
    for (const [key, el] of controls) if (el === at) focused = key;
    for (const [key, { opener }] of menus) if (opener === at) focused = key;
    const onHelp = helpButton !== null && helpButton === at;
    controls = new Map();
    menus = new Map();
    values = new Map();
    helpButton = null;
    const clusters = [];
    for (let i = 0; i < rows.length;) {
      const group = rows[i].group;
      const cluster = document.createElement("div");
      cluster.className = "cluster";
      if (group !== undefined) {
        cluster.setAttribute("role", "group");
        cluster.setAttribute("aria-label", group);
      }
      for (; i < rows.length && rows[i].group === group;) {
        const row = rows[i];
        if (row.kind === "item") {
          const items = [];
          for (; i < rows.length && rows[i].group === group && rows[i].kind === "item"; i++) items.push(rows[i]);
          cluster.append(...menu(items));
          continue;
        }
        if (row.kind === "range" || row.kind === "text") cluster.append(input(row));
        else {
          const el = tool(row);
          controls.set(row.key, el);
          cluster.append(el);
        }
        i++;
      }
      clusters.push(cluster);
    }
    if (rows.some((row) => row.shortcut !== undefined)) {
      const cluster = document.createElement("div");
      cluster.className = "cluster";
      helpButton = button({ label: "Keyboard shortcuts", shortcut: "?" }, "tool");
      helpButton.append(icon(HELP_ICON));
      helpButton.addEventListener("click", openHelp);
      cluster.append(helpButton);
      clusters.push(cluster);
    }
    bar.replaceChildren(...clusters);
    // An item's focus goes to its menu's button, as the rebuilt menu is closed.
    (onHelp ? helpButton : menus.get(focused)?.opener ?? controls.get(focused))?.focus();
  };

  // pressed and value stated again, each control kept.
  const update = (rows) => {
    const byKey = new Map(rows.map((row) => [row.key, row]));
    const shownMenus = new Set();
    for (const row of rows) {
      const el = controls.get(row.key);
      if (row.kind === "range" || row.kind === "text") {
        if (row.value !== values.get(row.key)) {
          el.value = row.kind === "range" ? String(row.value) : row.value ?? "";
          values.set(row.key, row.value);
        }
        continue;
      }
      markPressed(el, row);
      const owner = menus.get(row.key);
      if (owner && !shownMenus.has(owner)) {
        shownMenus.add(owner);
        owner.show(owner.keys.map((key) => byKey.get(key)));
      }
    }
  };

  return (rows) => {
    stated = rows;
    const next = JSON.stringify(rows.map(({ pressed, value, ...rest }) => rest));
    if (next !== snapshot) {
      build(rows);
      snapshot = next;
    } else update(rows);
  };
}

let marks = []; // the shown labels' elements

// Each placed label (viewport.js onLabels), {text, x, y} in CSS pixels from
// the canvas's top-left, as a div.label over the canvas whose bottom centre
// is at that point, as the page's .label rule places it; the labels shown
// before are removed, and [] removes them all. A label holds its text as
// textContent and takes no pointer.
export function labels(placed) {
  for (const mark of marks) mark.remove();
  marks = placed.map(({ text, x, y }) => {
    const mark = document.createElement("div");
    mark.className = "label";
    mark.textContent = text;
    mark.style.left = `${canvas.offsetLeft + x}px`;
    mark.style.top = `${canvas.offsetTop + y}px`;
    return mark;
  });
  // Right after the canvas, before #toolbar and #keys, which then lie over them.
  canvas.after(...marks);
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

// A #keys button's key acting on the shown image.
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
  if (zooms) zooms.hidden = true;
}

// A shown image stands in the canvas's place, fitted to the frame; null
// removes it. #keys is shown only while it shows, and its buttons act on it
// (listen), never sending a key: "+" scales it by 1.25 and "-" by 1/1.25
// about the frame's centre, and "f" fits it again. The wheel over it scales it by 1.1 per
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
  // Before #toolbar and #keys, which then lie over it.
  canvas.after(next);
  if (zooms) zooms.hidden = false;
}

// Enter submits the field's form: its text is sent and the field cleared at
// once, though the turn may wait minutes for an answer. Submission, not a key,
// sends it, since the Enter that ends an input method's composition submits
// nothing. The field stays disabled until the module starts, as an Enter with
// no listener would submit the page. A click on a #keys button acts on the
// shown image (svg). A key pressed in the page goes first to the toolbar's
// shortcuts and help screen (keyed). signal, when given, removes the
// listeners once it aborts.
export function listen(view, signal) {
  for (const button of keys)
    button.addEventListener("click", () => {
      if (image) act(button.dataset.key);
    }, { signal });
  document.addEventListener("keydown", keyed, { capture: true, signal });
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
