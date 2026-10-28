// viewport.js - the browser viewer as a component. mount runs a Habu Wasm
// module's turns (turn.js) on the main thread, draws its triangles with WebGPU
// on the caller's canvas, and sends the canvas's pointer, wheel, key and size
// to the module. The caller owns the page: nothing here looks up the DOM, and
// every other record reaches the caller through a callback.
//
//   mount(canvas, { module, fetch, onTree, onText, onSvg, onInput })
//     -> Promise<viewport>
//   module                 the module's URL, fetched with fetch
//   fetch(url, init)       window.fetch's contract; the module, and every
//                          fetch, post and request record, go through it
//   onTree(rows, selected) a tree record's rows and selected key (turn.js)
//   onText(text, error)    a text record, error false; a failure after mount
//                          resolved, error true (it is also logged)
//   onSvg(bytes)           an svg record's copied bytes, shown in place of the
//                          canvas, whose visibility it hides; null when a draw
//                          removes the shown image
//   onInput(enabled)       true as the module starts, then each input record
//
//   viewport.post(event)   { kind: "typed", text } or { kind: "control", key }:
//                          the field's submitted text or a tree row's key; the
//                          promise settles when the turn has, its failure
//                          already reported through onText
//   viewport.load(scene)   a load turn (turn.js) of the scene's bytes, an
//                          ArrayBuffer or Uint8Array; settles as post does
//   viewport.focus(ids), viewport.highlight(ids)
//                          a focus or highlight turn of ids, an array of
//                          strings of 32 hex digits whose pairs are an
//                          identity's 16 bytes in order; [] clears. A
//                          malformed id throws before any turn; settles as
//                          post does
//   viewport.dispose()     removes the listeners, aborts the outstanding
//                          fetches, releases the GPU device and ends the
//                          module; no callback runs after it, a later reply
//                          runs no turn, and the canvas can be mounted again
//
// mount resolves once the start turn has begun; it rejects, having released
// what it took, if WebGPU, the module's fetch or its instantiation fails.
import { Module } from "./turn.js";

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

// WebGPU on the canvas: draw answers a draw record, release destroys the
// device and everything allocated from it. A canvas holding another context is
// refused before a device exists; a later setup failure releases the device.
async function gpu(canvas, fail) {
  if (!navigator.gpu) throw new Error("this browser has no WebGPU");
  const context = canvas.getContext("webgpu");
  if (!context) throw new Error("the canvas already has a context other than WebGPU");
  const adapter = await navigator.gpu.requestAdapter();
  if (!adapter) throw new Error("WebGPU found no adapter");
  const device = await adapter.requestDevice();
  const release = () => {
    context.unconfigure();
    device.destroy();
  };
  try {
    return setup(canvas, context, device, release, fail);
  } catch (e) {
    release();
    throw e;
  }
}

function setup(canvas, context, device, release, fail) {
  device.lost.then((info) => fail(`WebGPU device lost: ${info.message}`));
  device.addEventListener("uncapturederror", (e) => fail(`WebGPU: ${e.error.message}`));
  const format = navigator.gpu.getPreferredCanvasFormat();
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

  return { draw, resize, release };
}

function modifiers(e) {
  return (e.shiftKey ? 1 : 0) | (e.ctrlKey ? 2 : 0) |
    (e.altKey ? 4 : 0) | (e.metaKey ? 8 : 0);
}

// The canvas's displayed box in device pixels.
function drawingSize(canvas) {
  const box = canvas.getBoundingClientRect();
  return [
    Math.max(1, Math.round(box.width * devicePixelRatio)),
    Math.max(1, Math.round(box.height * devicePixelRatio)),
  ];
}

// A pointer's position in the rectangle the canvas is displayed in, scaled to
// its drawing buffer.
function at(canvas, e) {
  const r = canvas.getBoundingClientRect();
  return [
    Math.floor(((e.clientX - r.left) * canvas.width) / r.width),
    Math.floor(((e.clientY - r.top) * canvas.height) / r.height),
  ];
}

const ID_HEX = /^[0-9a-fA-F]{32}$/;

// The identities, 32 hex digits each, as the packed 16-byte list turn.js sends.
function packed(ids) {
  if (!Array.isArray(ids)) throw new TypeError("identities must be an array of 32-hex-digit strings");
  const bytes = new Uint8Array(ids.length * 16);
  // By index, so a hole reads as undefined and is refused.
  for (let i = 0; i < ids.length; i++) {
    const id = ids[i];
    if (typeof id !== "string" || !ID_HEX.test(id))
      throw new TypeError(`identity ${i} is not 32 hex digits: ${typeof id === "string" ? JSON.stringify(id) : typeof id}`);
    for (let k = 0; k < 16; k++) bytes[i * 16 + k] = parseInt(id.slice(2 * k, 2 * k + 2), 16);
  }
  return bytes;
}

function sceneBytes(scene) {
  if (scene instanceof ArrayBuffer) return new Uint8Array(scene);
  if (scene instanceof Uint8Array) return scene;
  throw new TypeError("a scene is an ArrayBuffer or a Uint8Array");
}

export async function mount(canvas, { module: url, fetch: load, onTree, onText, onSvg, onInput }) {
  // One signal aborts the outstanding fetches and removes every listener.
  const ending = new AbortController();
  const { signal } = ending;
  let disposed = false;
  let renderer = null;
  let observer = null;
  let module = null;

  const fail = (e) => {
    if (disposed) return;
    onText(String(e), true);
    console.error(e);
  };
  const live = (fn) => (...args) => { if (!disposed) fn(...args); };
  const answer = async (path, init) => {
    const r = await load(path, { ...init, signal });
    return { status: r.status, bytes: new Uint8Array(await r.arrayBuffer()) };
  };

  const dispose = () => {
    if (disposed) return;
    disposed = true;
    ending.abort();
    observer?.disconnect();
    renderer?.release();
    module?.end();
    canvas.style.visibility = "";
    renderer = null;
    module = null;
  };

  try {
    [canvas.width, canvas.height] = drawingSize(canvas);
    renderer = await gpu(canvas, fail);
    const r = await load(url, { signal });
    if (!r.ok) throw new Error(`${url}: ${r.status} ${r.statusText}`);
    let shown = false;
    module = await Module.load(await r.arrayBuffer(), {
      hello: () => {},
      fetch: async (path) => {
        const r = await load(path, { signal });
        if (!r.ok) throw new Error(`${path}: ${r.status} ${r.statusText}`);
        return new Uint8Array(await r.arrayBuffer());
      },
      // Answers every status: the module reads it.
      post: (path, body) => answer(path, { method: "POST", body }),
      request: (method, path, body) => answer(path, body.length ? { method, body } : { method }),
      error: fail,
      draw: live((...args) => {
        if (shown) {
          shown = false;
          // Visibility keeps the canvas's layout and drawing-buffer size stable.
          canvas.style.visibility = "visible";
          onSvg(null);
        }
        renderer.draw(...args);
      }),
      svg: live((bytes) => {
        shown = true;
        canvas.style.visibility = "hidden";
        onSvg(bytes);
      }),
      input: live(onInput),
      text: live((s) => onText(s, false)),
      tree: live(onTree),
      highlight: () => {},
      size: () => [canvas.width, canvas.height],
    });
  } catch (e) {
    dispose();
    throw e;
  }

  const send = async (turn) => {
    try {
      await turn();
    } catch (e) {
      fail(e);
    }
  };
  const on = (type, fn, options = {}) => canvas.addEventListener(type, fn, { ...options, signal });

  // A press captures its pointer, so the drag's moves and its release reach
  // the canvas even outside it. pointerup's buttons is 0, so the capture, not
  // the buttons, says the pointer is dragging.
  const active = new Set();
  on("pointerdown", (e) => {
    canvas.focus({ preventScroll: true });
    canvas.setPointerCapture(e.pointerId);
    active.add(e.pointerId);
    send(() => module.pointer(...at(canvas, e), e.buttons, modifiers(e)));
  });
  on("pointermove", (e) => {
    if (active.has(e.pointerId)) send(() => module.moved(...at(canvas, e), e.buttons, modifiers(e)));
  });
  const release = (e) => {
    if (active.delete(e.pointerId)) send(() => module.released(...at(canvas, e), 0, modifiers(e)));
  };
  on("pointerup", release);
  on("pointercancel", release);
  on("lostpointercapture", release);
  on("contextmenu", (e) => e.preventDefault());
  on("wheel", (e) => {
    e.preventDefault();
    const box = canvas.getBoundingClientRect();
    const line = parseFloat(getComputedStyle(canvas).lineHeight) || 16;
    const xUnit = e.deltaMode === 2 ? box.width : e.deltaMode === 1 ? line : 1;
    const yUnit = e.deltaMode === 2 ? box.height : e.deltaMode === 1 ? line : 1;
    send(() => module.wheel(
      Math.round(e.deltaX * xUnit * canvas.width / box.width),
      Math.round(e.deltaY * yUnit * canvas.height / box.height),
      modifiers(e),
    ));
  }, { passive: false });
  on("keydown", (e) => {
    if (!e.isComposing) send(() => module.key(e.key, modifiers(e)));
  });

  // Enabled only before start: its first records may immediately disable it.
  onInput(true);
  send(() => module.start());
  const resize = () => {
    const [width, height] = drawingSize(canvas);
    if (canvas.width === width && canvas.height === height) return false;
    canvas.width = width;
    canvas.height = height;
    renderer.resize();
    send(() => module.resize());
    return true;
  };
  observer = new ResizeObserver(resize);
  observer.observe(canvas);
  window.addEventListener("resize", resize, { signal });
  if (!resize()) send(() => module.resize());

  const open = () => {
    if (disposed) throw new Error("the viewport is disposed");
  };
  return {
    post(event) {
      open();
      switch (event.kind) {
        case "typed": return send(() => module.typed(event.text));
        case "control": return send(() => module.control(event.key));
        default: throw new TypeError(`unknown viewport event ${event.kind}`);
      }
    },
    load(scene) {
      open();
      const bytes = sceneBytes(scene);
      return send(() => module.load(bytes));
    },
    focus(ids) {
      open();
      const bytes = packed(ids);
      return send(() => module.focus(bytes));
    },
    highlight(ids) {
      open();
      const bytes = packed(ids);
      return send(() => module.highlight(bytes));
    },
    dispose,
  };
}
