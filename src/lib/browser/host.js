// host.js - the browser host (docs/browser-host.md): runs /module.wasm's turns
// through turn.js on the main thread, draws its triangles with WebGPU, shows
// its text and tree, and sends canvas input and the text entered in the field.
// A failure to start WebGPU, request, grow memory or run is shown in text.
import { Module } from "./turn.js";

const canvas = document.querySelector("canvas");
const field = document.querySelector("input");
const shown = document.querySelector("#text");
const tree = document.querySelector("#tree");

function show(s, error) {
  shown.textContent = s;
  shown.className = error ? "error" : "";
}

function fail(e) {
  show(String(e), true);
  console.error(e);
}

// Clip space from the matrix; each fragment lit by its triangle's normal, from
// the screen derivatives of the position, so no normals are sent.
const SHADER = `
@group(0) @binding(0) var<uniform> clip: mat4x4<f32>;

struct Corner {
  @builtin(position) at: vec4<f32>,
  @location(0) p: vec3<f32>,
  @location(1) @interpolate(flat) selected: u32,
}

@vertex fn vs(@location(0) p: vec3<f32>, @location(1) selected: u32) -> Corner {
  return Corner(clip * vec4<f32>(p, 1.0), p, selected);
}

@fragment fn fs(c: Corner) -> @location(0) vec4<f32> {
  let n = normalize(cross(dpdx(c.p), dpdy(c.p)));
  let lit = 0.3 + 0.7 * abs(dot(n, normalize(vec3<f32>(0.3, 0.5, 0.8))));
  let color = select(vec3<f32>(1.0), vec3<f32>(1.0, 0.45, 0.08), c.selected == 1u);
  return vec4<f32>(color * lit, 1.0);
}
`;

// WebGPU on the canvas; answers the draw a draw record calls.
async function gpu() {
  if (!navigator.gpu) throw new Error("this browser has no WebGPU");
  const adapter = await navigator.gpu.requestAdapter();
  if (!adapter) throw new Error("WebGPU found no adapter");
  const device = await adapter.requestDevice();
  device.lost.then((info) => fail(`WebGPU device lost: ${info.message}`));
  device.addEventListener("uncapturederror", (e) => fail(`WebGPU: ${e.error.message}`));
  const format = navigator.gpu.getPreferredCanvasFormat();
  const context = canvas.getContext("webgpu");
  context.configure({ device, format, alphaMode: "opaque" });
  const module = device.createShaderModule({ code: SHADER });
  const pipeline = device.createRenderPipeline({
    layout: "auto",
    vertex: {
      module,
      entryPoint: "vs",
      buffers: [
        { arrayStride: 12, attributes: [{ shaderLocation: 0, offset: 0, format: "float32x3" }] },
        { arrayStride: 4, attributes: [{ shaderLocation: 1, offset: 0, format: "uint32" }] },
      ],
    },
    fragment: { module, entryPoint: "fs", targets: [{ format }] },
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth24plus", depthWriteEnabled: true, depthCompare: "less" },
  });
  const matrix = device.createBuffer({ size: 64, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
  const bind = device.createBindGroup({
    layout: pipeline.getBindGroupLayout(0),
    entries: [{ binding: 0, resource: { buffer: matrix } }],
  });
  const depthTexture = () => device.createTexture({
    size: [canvas.width, canvas.height], format: "depth24plus",
    usage: GPUTextureUsage.RENDER_ATTACHMENT,
  });
  let depth = depthTexture();
  let corners = null;
  let selected = null;

  const resize = () => {
    depth.destroy();
    depth = depthTexture();
  };

  const draw = (bytes, count, clip, mask) => {
    const cornerSize = Math.max(4, bytes.byteLength);
    if (!corners || corners.size < cornerSize) {
      corners?.destroy();
      corners = device.createBuffer({ size: cornerSize, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST });
    }
    const flags = new Uint32Array(count * 3);
    for (let i = 0; i < count; i++) flags.fill(mask[i], i * 3, i * 3 + 3);
    const selectedSize = Math.max(4, flags.byteLength);
    if (!selected || selected.size < selectedSize) {
      selected?.destroy();
      selected = device.createBuffer({ size: selectedSize, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST });
    }
    if (bytes.byteLength) device.queue.writeBuffer(corners, 0, bytes);
    if (flags.byteLength) device.queue.writeBuffer(selected, 0, flags);
    device.queue.writeBuffer(matrix, 0, clip);
    const encoder = device.createCommandEncoder();
    const pass = encoder.beginRenderPass({
      colorAttachments: [{
        view: context.getCurrentTexture().createView(),
        clearValue: [1, 1, 1, 1],
        loadOp: "clear",
        storeOp: "store",
      }],
      depthStencilAttachment: { view: depth.createView(), depthClearValue: 1, depthLoadOp: "clear", depthStoreOp: "discard" },
    });
    pass.setPipeline(pipeline);
    pass.setBindGroup(0, bind);
    pass.setVertexBuffer(0, corners);
    pass.setVertexBuffer(1, selected);
    pass.draw(count * 3);
    pass.end();
    device.queue.submit([encoder.finish()]);
  };
  return { draw, resize };
}

// A pointer's position in the rectangle the canvas is displayed in, scaled to
// its drawing buffer.
function at(e) {
  const r = canvas.getBoundingClientRect();
  return [
    Math.floor(((e.clientX - r.left) * canvas.width) / r.width),
    Math.floor(((e.clientY - r.top) * canvas.height) / r.height),
  ];
}

function modifiers(e) {
  return (e.shiftKey ? 1 : 0) | (e.ctrlKey ? 2 : 0) |
    (e.altKey ? 4 : 0) | (e.metaKey ? 8 : 0);
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

function drawingSize() {
  const box = canvas.getBoundingClientRect();
  return [
    Math.max(1, Math.round(box.width * devicePixelRatio)),
    Math.max(1, Math.round(box.height * devicePixelRatio)),
  ];
}

async function main() {
  [canvas.width, canvas.height] = drawingSize();
  const renderer = await gpu();
  const answer = await fetch("/module.wasm");
  if (!answer.ok) throw new Error(`/module.wasm: ${answer.status} ${answer.statusText}`);
  const updateTree = sceneTree((key) => module.control(key).catch(fail));
  const module = await Module.load(await answer.arrayBuffer(), {
    hello: () => {},
    fetch: async (path) => {
      const r = await fetch(path);
      if (!r.ok) throw new Error(`${path}: ${r.status} ${r.statusText}`);
      return new Uint8Array(await r.arrayBuffer());
    },
    // Answers every status: the module reads it.
    post: async (path, body) => {
      const r = await fetch(path, { method: "POST", body });
      return { status: r.status, bytes: new Uint8Array(await r.arrayBuffer()) };
    },
    request: async (method, path, body) => {
      const init = { method };
      if (body.length) init.body = body;
      const r = await fetch(path, init);
      return { status: r.status, bytes: new Uint8Array(await r.arrayBuffer()) };
    },
    error: fail,
    draw: renderer.draw,
    text: (s) => show(s, false),
    tree: updateTree,
    highlight: () => {},
    size: () => [canvas.width, canvas.height],
  });
  // A press captures its pointer, so the drag's moves and its release reach
  // the canvas even outside it. pointerup's buttons is 0, so the capture, not
  // the buttons, says the pointer is dragging.
  const active = new Set();
  canvas.addEventListener("pointerdown", (e) => {
    canvas.focus({ preventScroll: true });
    canvas.setPointerCapture(e.pointerId);
    active.add(e.pointerId);
    module.pointer(...at(e), e.buttons, modifiers(e)).catch(fail);
  });
  canvas.addEventListener("pointermove", (e) => {
    if (active.has(e.pointerId)) module.moved(...at(e), e.buttons, modifiers(e)).catch(fail);
  });
  const release = (e) => {
    if (active.delete(e.pointerId)) module.released(...at(e), 0, modifiers(e)).catch(fail);
  };
  canvas.addEventListener("pointerup", release);
  canvas.addEventListener("pointercancel", release);
  canvas.addEventListener("lostpointercapture", release);
  canvas.addEventListener("contextmenu", (e) => e.preventDefault());
  canvas.addEventListener("wheel", (e) => {
    e.preventDefault();
    const box = canvas.getBoundingClientRect();
    const line = parseFloat(getComputedStyle(canvas).lineHeight) || 16;
    const xUnit = e.deltaMode === 2 ? box.width : e.deltaMode === 1 ? line : 1;
    const yUnit = e.deltaMode === 2 ? box.height : e.deltaMode === 1 ? line : 1;
    module.wheel(
      Math.round(e.deltaX * xUnit * canvas.width / box.width),
      Math.round(e.deltaY * yUnit * canvas.height / box.height),
      modifiers(e),
    ).catch(fail);
  }, { passive: false });
  canvas.addEventListener("keydown", (e) => {
    if (!e.isComposing) module.key(e.key, modifiers(e)).catch(fail);
  });
  // Enter submits the field's form: its text is sent and the field cleared at
  // once, though the turn may wait minutes for an answer. Submission, not a
  // key, sends it, since the Enter that ends an input method's composition
  // submits nothing. The field is disabled until here, as an Enter with no
  // listener would submit the page.
  field.form.addEventListener("submit", (e) => {
    e.preventDefault();
    const text = field.value;
    field.value = "";
    module.typed(text).catch(fail);
  });
  field.disabled = false;
  const started = module.start();
  const resize = () => {
    const [width, height] = drawingSize();
    if (canvas.width === width && canvas.height === height) return false;
    canvas.width = width;
    canvas.height = height;
    renderer.resize();
    module.resize().catch(fail);
    return true;
  };
  new ResizeObserver(resize).observe(canvas);
  window.addEventListener("resize", resize);
  if (!resize()) module.resize().catch(fail);
  await started;
}

main().catch(fail);
