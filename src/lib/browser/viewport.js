// viewport.js - the browser viewer as a component. mount runs a Habu Wasm
// module's turns (turn.js) on the main thread, holds its meshes on the GPU
// and draws their instances with WebGPU on the caller's canvas, and sends the
// canvas's pointer, wheel, key and size to the module. The caller owns the
// page: nothing here looks up the DOM, and every other record reaches the
// caller through a callback.
//
//   mount(canvas, { Module, module, fetch, rpc, onTree, onText, onSvg, onInput })
//     -> Promise<viewport>
//   Module                 turn.js's Module class, which the caller imports
//   module                 the module's URL, fetched with fetch
//   fetch(url, init)       window.fetch's contract; the module, and every
//                          fetch, post and request record, go through it
//   rpc(method, paramsJSON) an rpc record's method and params JSON text ->
//                          Promise of the response's JSON text, {"result":…}
//                          or {"error":…}; a rejection is a transport failure
//   onTree(rows, selected) a tree record's rows and selected key (turn.js)
//   onText(text, error)    a text record, error false; a failure after mount
//                          resolved, error true (it is also logged)
//   onSvg(bytes)           an svg record's copied bytes, shown in place of the
//                          canvas, whose visibility it hides; null when a draw
//                          removes the shown image
//   onInput(enabled)       true as the module starts, then each input record
//
//   viewport.post(event)   { kind: "typed", text } or { kind: "control", key,
//                          opened }: the field's submitted text, or a tree
//                          row's key, selected or, when opened is true, opened
//                          (turn.js's control event); the promise settles
//                          when the turn has, its failure already reported
//                          through onText
//   viewport.load(scene)   a load turn (turn.js) of the scene's bytes, an
//                          ArrayBuffer or Uint8Array; settles as post does
//   viewport.focus(ids), viewport.highlight(ids)
//                          a focus or highlight turn of ids, an array of
//                          strings of 32 hex digits whose pairs are an
//                          identity's 16 bytes in order; [] clears. A
//                          malformed id throws before any turn; settles as
//                          post does
//   viewport.notify(method, paramsJSON)
//                          a notified turn of a host notification; settles as
//                          post does
//   viewport.file(file)    a file turn of a File, numbered from 1 and kept for
//                          the module's reads; settles as post does
//   viewport.dispose()     removes the listeners, clears the after timers,
//                          aborts the outstanding fetches, releases the GPU
//                          device and ends the module; no callback runs after
//                          it, a later reply runs no turn, and the canvas can
//                          be mounted again
//
// An after record's tick is sent once its ms have passed while the document is
// visible, or else at the next visibilitychange to visible. A read record reads
// File.slice(offset, offset + length) of its numbered File.
//
// mount resolves once the start turn has begun; it rejects, having released
// what it took, if WebGPU, the module's fetch or its instantiation fails.

// A vertex is a held mesh's four u16: x, y and z on the mesh's grid, which
// the instance's 3x4 transform (four float32x3 columns) takes to world, then
// clip from the matrix; and its face. A fragment is selected when the
// instance's flags bit 0 is set or, with an offset not 0xFFFFFFFF, bit
// face % 32 of highlight word offset + face / 32 is set. Each fragment is lit
// by its triangle's normal, from the screen derivatives of the world
// position, so no normals are sent. box draws one occlude box per instance
// as the 12 triangles of its faces: vertex k takes corner corners[k], whose
// bit j picks the highest coordinate j in place of the lowest. A box whose
// corners' clip z lie on both sides of 0, the near plane, draws the
// full-viewport quad at depth 0 from vertices 0 to 5 in place of its faces,
// which that plane can cut away while the box's inside is in view.
const SHADER = `
@group(0) @binding(0) var<uniform> clip: mat4x4<f32>;
@group(0) @binding(1) var<storage, read> words: array<u32>;

struct Vertex {
  @location(0) grid: vec4<u32>,
  @location(1) c0: vec3<f32>,
  @location(2) c1: vec3<f32>,
  @location(3) c2: vec3<f32>,
  @location(4) c3: vec3<f32>,
  @location(5) bits: vec2<u32>, // the instance's flags and offset
}

struct Corner {
  @builtin(position) at: vec4<f32>,
  @location(0) p: vec3<f32>,
  @location(1) @interpolate(flat) face: u32,
  @location(2) @interpolate(flat) bits: vec2<u32>,
}

@vertex fn vs(v: Vertex) -> Corner {
  let g = vec3<f32>(v.grid.xyz);
  let p = v.c0 * g.x + v.c1 * g.y + v.c2 * g.z + v.c3;
  return Corner(clip * vec4<f32>(p, 1.0), p, v.grid.w, v.bits);
}

fn selected(face: u32, bits: vec2<u32>) -> bool {
  if ((bits.x & 1u) != 0u) { return true; }
  if (bits.y == 0xFFFFFFFFu) { return false; }
  return ((words[bits.y + face / 32u] >> (face % 32u)) & 1u) != 0u;
}

@fragment fn fs(c: Corner) -> @location(0) vec4<f32> {
  let n = normalize(cross(dpdx(c.p), dpdy(c.p)));
  let lit = 0.3 + 0.7 * abs(dot(n, normalize(vec3<f32>(0.3, 0.5, 0.8))));
  let orange = vec3<f32>(1.0, 0.45, 0.08);
  let color = select(vec3<f32>(1.0), orange, selected(c.face, c.bits));
  return vec4<f32>(color * lit, 1.0);
}

fn corner(c: u32, lo: vec3<f32>, hi: vec3<f32>) -> vec4<f32> {
  let high = vec3<bool>((c & 1u) != 0u, (c & 2u) != 0u, (c & 4u) != 0u);
  return clip * vec4<f32>(select(lo, hi, high), 1.0);
}

@vertex fn box(@builtin(vertex_index) k: u32, @location(0) lo: vec3<f32>,
    @location(1) hi: vec3<f32>) -> @builtin(position) vec4<f32> {
  var near = 0u;
  for (var c = 0u; c < 8u; c++) {
    if (corner(c, lo, hi).z < 0.0) { near++; }
  }
  if (near != 0u && near != 8u) {
    var quad = array<vec2<f32>, 6>(vec2<f32>(-1.0, -1.0), vec2<f32>(1.0, -1.0),
      vec2<f32>(1.0, 1.0), vec2<f32>(-1.0, -1.0), vec2<f32>(1.0, 1.0),
      vec2<f32>(-1.0, 1.0));
    return vec4<f32>(select(vec2<f32>(0.0), quad[min(k, 5u)], k < 6u), 0.0, 1.0);
  }
  var corners = array<u32, 36>(0u, 2u, 6u, 0u, 6u, 4u, 1u, 3u, 7u, 1u, 7u, 5u,
    0u, 1u, 5u, 0u, 5u, 4u, 2u, 3u, 7u, 2u, 7u, 6u,
    0u, 1u, 3u, 0u, 3u, 2u, 4u, 5u, 7u, 4u, 7u, 6u);
  return corner(corners[k], lo, hi);
}
`;

const ROW = 64; // an instance row: twelve f32, u32 handle, id, flags, offset
const QUERIES = 4096; // WebGPU's most queries in one query set

// WebGPU on the canvas: setup's renderer, whose release destroys the device
// and everything allocated from it. A canvas holding another context is
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

// The rows regrouped by handle, each handle in the order it first appears,
// and the [handle, first, count] runs over them.
function grouped(rows, count) {
  const v = new DataView(rows.buffer, rows.byteOffset, rows.byteLength);
  const groups = new Map();
  for (let i = 0; i < count; i++) {
    const handle = v.getUint32(i * ROW + 48, true);
    if (!groups.has(handle)) groups.set(handle, []);
    groups.get(handle).push(i);
  }
  const ordered = new Uint8Array(count * ROW);
  const runs = [];
  let at = 0;
  for (const [handle, list] of groups) {
    runs.push([handle, at, list.length]);
    for (const i of list)
      ordered.set(rows.subarray(i * ROW, (i + 1) * ROW), ROW * at++);
  }
  return [ordered, runs];
}

// The renderer holds each mesh by handle as a vertex buffer of its u16x4
// vertices and an index buffer of its u16 triangles; draw writes the rows to
// an instance buffer and issues one indexed, instanced draw per handle, and
// highlight writes the words to a storage buffer. Each buffer is padded to
// the 4 bytes WebGPU's sizes and writes need, and the instance and highlight
// buffers grow when a write needs more. The depth texture keeps the last
// draw's depth, cleared to 1 when made, and the matrix buffer its matrix;
// occlude draws its boxes against them in passes of at most QUERIES occlusion
// queries, one per box, and reads the resolved sample counts back through
// buffers of its own, released once read.
function setup(canvas, context, device, release, fail) {
  device.lost.then((info) => fail(`WebGPU device lost: ${info.message}`));
  device.addEventListener("uncapturederror", (e) => fail(`WebGPU: ${e.error.message}`));
  const format = navigator.gpu.getPreferredCanvasFormat();
  context.configure({ device, format, alphaMode: "opaque" });
  const module = device.createShaderModule({ code: SHADER });
  const column = (k) =>
    ({ shaderLocation: 1 + k, offset: 12 * k, format: "float32x3" });
  const pipeline = device.createRenderPipeline({
    layout: "auto",
    vertex: {
      module,
      entryPoint: "vs",
      buffers: [
        { arrayStride: 8, attributes: [
          { shaderLocation: 0, offset: 0, format: "uint16x4" }] },
        { arrayStride: ROW, stepMode: "instance", attributes: [
          column(0), column(1), column(2), column(3),
          { shaderLocation: 5, offset: 56, format: "uint32x2" }] },
      ],
    },
    fragment: { module, entryPoint: "fs", targets: [{ format }] },
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth24plus", depthWriteEnabled: true, depthCompare: "less" },
  });
  const occluder = device.createRenderPipeline({
    layout: "auto",
    vertex: {
      module,
      entryPoint: "box",
      buffers: [{ arrayStride: 24, stepMode: "instance", attributes: [
        { shaderLocation: 0, offset: 0, format: "float32x3" },
        { shaderLocation: 1, offset: 12, format: "float32x3" }] }],
    },
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth24plus", depthWriteEnabled: false, depthCompare: "less-equal" },
  });
  const { UNIFORM, STORAGE, VERTEX, INDEX, COPY_DST, COPY_SRC, QUERY_RESOLVE, MAP_READ } =
    GPUBufferUsage;
  const matrix = device.createBuffer({ size: 64, usage: UNIFORM | COPY_DST });
  const boxBind = device.createBindGroup({
    layout: occluder.getBindGroupLayout(0),
    entries: [{ binding: 0, resource: { buffer: matrix } }],
  });
  const depthTexture = () => {
    const texture = device.createTexture({
      size: [canvas.width, canvas.height], format: "depth24plus",
      usage: GPUTextureUsage.RENDER_ATTACHMENT,
    });
    const encoder = device.createCommandEncoder();
    encoder.beginRenderPass({ colorAttachments: [], depthStencilAttachment: {
      view: texture.createView(), depthClearValue: 1, depthLoadOp: "clear", depthStoreOp: "store",
    } }).end();
    device.queue.submit([encoder.finish()]);
    return texture;
  };
  let depth = depthTexture();
  const meshes = new Map(); // a held handle -> { vertices, indices, count }
  let instances = null;
  let words = null;
  let bind = null;

  // buffer, or a buffer of at least size bytes in its place.
  const grown = (buffer, size, usage) => {
    if (buffer && buffer.size >= size) return buffer;
    buffer?.destroy();
    size = Math.max(size, 2 * (buffer?.size ?? 0));
    return device.createBuffer({ size, usage });
  };
  const highlight = (bits) => {
    const before = words;
    words = grown(words, Math.max(4, bits.byteLength), STORAGE | COPY_DST);
    if (bits.byteLength) device.queue.writeBuffer(words, 0, bits);
    if (words === before) return;
    bind = device.createBindGroup({
      layout: pipeline.getBindGroupLayout(0),
      entries: [{ binding: 0, resource: { buffer: matrix } },
        { binding: 1, resource: { buffer: words } }],
    });
  };
  highlight(new Uint32Array(0));

  // A buffer holding the u16s, its size rounded up to 4 bytes, at least 4.
  const filled = (u16s, usage) => {
    const size = Math.max(4, Math.ceil(u16s.byteLength / 4) * 4);
    const buffer = device.createBuffer({ size, usage, mappedAtCreation: true });
    new Uint16Array(buffer.getMappedRange()).set(u16s);
    buffer.unmap();
    return buffer;
  };
  // A failed mesh holds nothing: the vertex buffer goes if the index
  // buffer's creation throws.
  const mesh = (handle, vertexCount, triangleCount, vertices, triangles) => {
    const held = filled(vertices, VERTEX);
    let indices;
    try {
      indices = filled(triangles, INDEX);
    } catch (e) {
      held.destroy();
      throw e;
    }
    meshes.set(handle, { vertices: held, indices, count: 3 * triangleCount });
  };
  const unmesh = (handle) => {
    const { vertices, indices } = meshes.get(handle);
    vertices.destroy();
    indices.destroy();
    meshes.delete(handle);
  };

  const resize = () => {
    depth.destroy();
    depth = depthTexture();
  };

  // rows, count instance rows of held meshes (turn.js); 0 rows clears.
  const draw = (rows, count, clip) => {
    const [ordered, runs] = grouped(rows, count);
    if (count) {
      instances = grown(instances, ordered.byteLength, VERTEX | COPY_DST);
      device.queue.writeBuffer(instances, 0, ordered);
    }
    device.queue.writeBuffer(matrix, 0, clip);
    const encoder = device.createCommandEncoder();
    const pass = encoder.beginRenderPass({
      colorAttachments: [{
        view: context.getCurrentTexture().createView(),
        clearValue: [1, 1, 1, 1],
        loadOp: "clear",
        storeOp: "store",
      }],
      depthStencilAttachment: { view: depth.createView(), depthClearValue: 1, depthLoadOp: "clear", depthStoreOp: "store" },
    });
    pass.setPipeline(pipeline);
    pass.setBindGroup(0, bind);
    if (count) pass.setVertexBuffer(1, instances);
    for (const [handle, first, n] of runs) {
      const { vertices, indices, count: indexCount } =
        meshes.get(handle);
      pass.setVertexBuffer(0, vertices);
      pass.setIndexBuffer(indices, "uint16");
      pass.drawIndexed(indexCount, n, 0, 0, first);
    }
    pass.end();
    device.queue.submit([encoder.finish()]);
  };

  // boxes, six f32 each, lowest corner then highest -> a byte per box, 1 when
  // any of its samples passed the last draw's depth.
  const occlude = async (boxes) => {
    const count = boxes.length / 6;
    if (count === 0) return new Uint8Array(0);
    const held = device.createBuffer({ size: boxes.byteLength, usage: VERTEX, mappedAtCreation: true });
    new Float32Array(held.getMappedRange()).set(boxes);
    held.unmap();
    const resolved = device.createBuffer({ size: 8 * count, usage: QUERY_RESOLVE | COPY_SRC });
    const read = device.createBuffer({ size: 8 * count, usage: MAP_READ | COPY_DST });
    const sets = [];
    try {
      const encoder = device.createCommandEncoder();
      for (let first = 0; first < count; first += QUERIES) {
        const n = Math.min(QUERIES, count - first);
        const set = device.createQuerySet({ type: "occlusion", count: n });
        sets.push(set);
        const pass = encoder.beginRenderPass({
          colorAttachments: [],
          depthStencilAttachment: { view: depth.createView(), depthReadOnly: true },
          occlusionQuerySet: set,
        });
        pass.setPipeline(occluder);
        pass.setBindGroup(0, boxBind);
        pass.setVertexBuffer(0, held);
        for (let i = 0; i < n; i++) {
          pass.beginOcclusionQuery(i);
          pass.draw(36, 1, 0, first + i);
          pass.endOcclusionQuery();
        }
        pass.end();
        // first is a multiple of QUERIES, so the offset is 256-aligned.
        encoder.resolveQuerySet(set, 0, n, resolved, 8 * first);
      }
      encoder.copyBufferToBuffer(resolved, 0, read, 0, 8 * count);
      device.queue.submit([encoder.finish()]);
      await read.mapAsync(GPUMapMode.READ);
      return Uint8Array.from(new BigUint64Array(read.getMappedRange()), (s) => (s > 0n ? 1 : 0));
    } finally {
      for (const resource of [held, resolved, read, ...sets]) resource.destroy();
    }
  };

  return { mesh, unmesh, highlight, draw, occlude, resize, release };
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

// setTimeout's largest delay; a longer after waits in steps of it.
const MAX_DELAY = 2 ** 31 - 1;

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

export async function mount(canvas, { Module, module: url, fetch: load, rpc, onTree, onText, onSvg, onInput }) {
  // One signal aborts the outstanding fetches and removes every listener.
  const ending = new AbortController();
  const { signal } = ending;
  let disposed = false;
  let renderer = null;
  let observer = null;
  let module = null;
  const timers = new Set();
  const due = []; // the tokens whose ms passed while the document was hidden
  const files = new Map(); // a file number -> its File

  const fail = (e) => {
    if (disposed) return;
    onText(String(e), true);
    console.error(e);
  };
  const live = (fn) => (...args) => (disposed ? undefined : fn(...args));
  const answer = async (path, init) => {
    const r = await load(path, { ...init, signal });
    return { status: r.status, bytes: new Uint8Array(await r.arrayBuffer()) };
  };

  const dispose = () => {
    if (disposed) return;
    disposed = true;
    ending.abort();
    for (const timer of timers) clearTimeout(timer);
    timers.clear();
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
      rpc: (method, params) => rpc(method, params),
      after: (ms, token) => wait(Number(ms), token),
      read: async (file, offset, length) =>
        new Uint8Array(await files.get(Number(file)).slice(Number(offset), Number(offset + length)).arrayBuffer()),
      error: fail,
      mesh: live((...args) => renderer.mesh(...args)),
      unmesh: live((handle) => renderer.unmesh(handle)),
      highlight: live((words) => renderer.highlight(words)),
      occlude: live((boxes) => renderer.occlude(boxes)),
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

  const tick = (token) => send(() => module.tick(token));
  function wait(ms, token) {
    const step = Math.min(ms, MAX_DELAY);
    const timer = setTimeout(() => {
      timers.delete(timer);
      if (ms > step) wait(ms - step, token);
      else if (document.visibilityState === "visible") tick(token);
      else due.push(token);
    }, step);
    timers.add(timer);
  }
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "visible") for (const token of due.splice(0)) tick(token);
  }, { signal });

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
        case "control":
          return send(() => module.control(event.key, event.opened));
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
    notify(method, paramsJSON) {
      open();
      return send(() => module.notified(method, paramsJSON));
    },
    file(file) {
      open();
      if (!(file instanceof File)) throw new TypeError("viewport.file takes a File");
      const number = files.size + 1;
      files.set(number, file);
      return send(() => module.file(file.name, file.size, number));
    },
    dispose,
  };
}
