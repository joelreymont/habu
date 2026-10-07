// host.js - the browser host (docs/browser-host.md): runs /module.wasm's turns
// through turn.js on the main thread, draws its triangles with WebGPU, shows
// its text and sends it each pointerdown on the canvas. A failure to start
// WebGPU, to fetch, to grow memory or to run is shown in the text element.
import { Module } from "./turn.js";

const canvas = document.querySelector("canvas");
const shown = document.querySelector("#text");

function show(s, error) {
  shown.textContent = s;
  shown.className = error ? "error" : "";
}

function fail(e) {
  show(String(e), true);
}

// Clip space from the matrix; each fragment lit by its triangle's normal, from
// the screen derivatives of the position, so no normals are sent.
const SHADER = `
@group(0) @binding(0) var<uniform> clip: mat4x4<f32>;

struct Corner {
  @builtin(position) at: vec4<f32>,
  @location(0) p: vec3<f32>,
}

@vertex fn vs(@location(0) p: vec3<f32>) -> Corner {
  return Corner(clip * vec4<f32>(p, 1.0), p);
}

@fragment fn fs(c: Corner) -> @location(0) vec4<f32> {
  let n = normalize(cross(dpdx(c.p), dpdy(c.p)));
  let lit = 0.3 + 0.7 * abs(dot(n, normalize(vec3<f32>(0.3, 0.5, 0.8))));
  return vec4<f32>(lit, lit, lit, 1.0);
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
      buffers: [{ arrayStride: 12, attributes: [{ shaderLocation: 0, offset: 0, format: "float32x3" }] }],
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
  const depth = device.createTexture({
    size: [canvas.width, canvas.height],
    format: "depth24plus",
    usage: GPUTextureUsage.RENDER_ATTACHMENT,
  });
  let corners = null;

  return (bytes, count, clip) => {
    if (!corners || corners.size < bytes.byteLength) {
      corners?.destroy();
      corners = device.createBuffer({ size: bytes.byteLength, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST });
    }
    device.queue.writeBuffer(corners, 0, bytes);
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
    pass.draw(count * 3);
    pass.end();
    device.queue.submit([encoder.finish()]);
  };
}

async function main() {
  const box = canvas.getBoundingClientRect();
  canvas.width = Math.max(1, Math.round(box.width * devicePixelRatio));
  canvas.height = Math.max(1, Math.round(box.height * devicePixelRatio));
  const draw = await gpu();
  const answer = await fetch("/module.wasm");
  if (!answer.ok) throw new Error(`/module.wasm: ${answer.status} ${answer.statusText}`);
  const module = await Module.load(await answer.arrayBuffer(), {
    hello: () => {},
    fetch: async (path) => {
      const r = await fetch(path);
      if (!r.ok) throw new Error(`${path}: ${r.status} ${r.statusText}`);
      return new Uint8Array(await r.arrayBuffer());
    },
    draw,
    text: (s) => show(s, false),
    size: () => [canvas.width, canvas.height],
  });
  // The rectangle the canvas is displayed in, scaled to its drawing buffer.
  canvas.addEventListener("pointerdown", (e) => {
    const r = canvas.getBoundingClientRect();
    const x = Math.floor(((e.clientX - r.left) * canvas.width) / r.width);
    const y = Math.floor(((e.clientY - r.top) * canvas.height) / r.height);
    module.pointer(x, y).catch(fail);
  });
  await module.start();
}

main().catch(fail);
