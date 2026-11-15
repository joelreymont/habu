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

// A vertex is a held mesh's four u16, x, y and z on the mesh's grid, which
// the instance's 3x4 transform (four float32x3 columns) takes to world, then
// clip from the matrix, and its face; then its normal, two snorm16 of the
// octahedral encoding, which the cofactor matrix of the transform's 3x3, the
// inverse transpose times its determinant, takes to world: the sign of the
// determinant keeps a mirroring transform's normal outward, and a normal the
// cofactor matrix maps to zero, one perpendicular to the grid axis a flat 3x3
// collapses, falls back to the triangle's own normal. A face is its
// triangle's first vertex's. A fragment is selected when the instance's flags
// bit 0 is set or, with an offset not 0xFFFFFFFF, bit face % 32 of highlight
// word offset + face / 32 is set; a selected fragment's paint is mixed toward
// a mark chosen by the paint's Rec. 709 luma, of its encoded bytes. fs then
// shades in linear light: the paint, decoded from sRGB, is a glTF
// metallic-roughness surface lit by a studio, env, and a directional key
// light along env's key softbox, tone-mapped by Khronos PBR Neutral and
// encoded to sRGB, the only encoding, since the preferred canvas format is
// never an -srgb one. The studio and key light are fixed in view's right, up
// and back, which studio() sets: with no ground they are the view's own, so
// the studio turns with the camera; with a ground, up is the ground's normal
// and right and back turn about it with the camera, a turntable. V points
// from the fragment toward view's eye, a point when its w is 1 and a
// direction when 0. The normal is turned toward V: meshes are drawn without
// culling and may be open. hit, after vs, marks the nearest fragment in
// pick's one texel, with 1 in its fourth channel to tell a hit from the clear.
// The key casts the instances' soft shadow on the ground: caster draws them
// into a depth map by light.clip, whose x and y run along the key softbox's
// tangents and whose depth runs away from the key; rows and columns sum the
// map's blockers into summed-area tables, counts and heights; and ground
// draws the catcher, the quad light.corners on the plane, whose fragment,
// shadow, finds by PCSS over those tables the share V of the softbox it sees
// past the casters. The ground is not in the map: the plane's depth at light
// x, y is light.ground.x x + light.ground.y y + light.ground.z,
// light.reach.xy is the softbox's half extents in light x and y per unit of
// depth, and light.reach.z the tallest caster's height above the plane in
// units of depth. A caster below the plane neither shades it nor hides one
// above it from the key. The catcher reads white times
// (env(up, 1) + key V) / (env(up, 1) + key), key the key's irradiance on the
// ground, in linear light and not tone-mapped, so an unshadowed ground reads
// white as the clear does.
// box draws one occlude box per instance as the 12 triangles of its faces:
// vertex k takes corner corners[k], whose bit j picks the highest coordinate j
// in place of the lowest. A box whose corners' clip z lie on both sides of 0,
// the near plane, draws the full-viewport quad at depth 0 from vertices 0 to
// 5 in place of its faces, which that plane can cut away while the box's
// inside is in view.
//
// KEY is the key light's direction and KEY_BOX its softbox's half extents,
// per unit along KEY, in studio coordinates. SHADOW_SIDE is the shadow map's
// largest side, the most at which 255 side² still fits a u32 table's sums;
// WebGPU guarantees textures that wide.
const KEY = [-0.4, 0.6, 0.7];
const KEY_BOX = [0.45, 0.27];
const SHADOW_SIDE = Math.floor(Math.sqrt(2 ** 32 / 255));
const SHADER = `
@group(0) @binding(0) var<uniform> clip: mat4x4<f32>;
@group(0) @binding(1) var<storage, read> words: array<u32>;

struct View {
  eye: vec4<f32>,
  right: vec4<f32>,
  up: vec4<f32>,
  back: vec4<f32>,
}

@group(0) @binding(2) var<uniform> view: View;

struct Light {
  clip: mat4x4<f32>,
  corners: array<vec4<f32>, 4>,
  ground: vec4<f32>,
  reach: vec4<f32>,
}

@group(0) @binding(3) var<uniform> light: Light;
@group(0) @binding(4) var depths: texture_depth_2d;
@group(0) @binding(5) var counts: texture_storage_2d<r32uint, read_write>;
@group(0) @binding(6) var heights: texture_storage_2d<r32uint, read_write>;
@group(0) @binding(7) var countSums: texture_2d<u32>;
@group(0) @binding(8) var heightSums: texture_2d<u32>;

const PI = 3.14159265358979;

struct Vertex {
  @location(0) grid: vec4<u32>,
  @location(1) c0: vec3<f32>,
  @location(2) c1: vec3<f32>,
  @location(3) c2: vec3<f32>,
  @location(4) c3: vec3<f32>,
  @location(5) row: vec3<u32>, // the instance's id, flags and offset
  @location(6) octahedral: vec2<f32>,
  @location(7) paint: vec4<f32>, // r, g, b and the 0 byte
  @location(8) surface: vec2<f32>, // metallic, roughness
  @location(9) origin: vec3<u32>, // the mesh's origin (record 13)
}

// @invariant: vs, the pick's vs and caster place a vertex at the same clip
// position for the same grid position, columns and matrix.
struct Corner {
  @builtin(position) @invariant at: vec4<f32>,
  @location(0) n: vec3<f32>,
  @location(1) @interpolate(flat) face: u32,
  @location(2) @interpolate(flat) bits: vec2<u32>,
  @location(3) @interpolate(flat) paint: vec3<f32>,
  @location(4) @interpolate(flat) surface: vec2<f32>,
  @location(5) world: vec3<f32>,
  @location(6) @interpolate(flat) id: u32,
}

fn unfold(e: vec2<f32>) -> vec3<f32> {
  var n = vec3<f32>(e, 1.0 - abs(e.x) - abs(e.y));
  let t = max(-n.z, 0.0);
  n.x += select(t, -t, n.x >= 0.0);
  n.y += select(t, -t, n.y >= 0.0);
  return normalize(n);
}

// The vertex in world: its grid position, origin plus u16s added in u32 and
// taken to f32 once, so a position two meshes reach by different origins
// has the same bits, through the row's columns.
fn placed(v: Vertex) -> vec3<f32> {
  let g = vec3<f32>(v.grid.xyz + v.origin);
  return v.c0 * g.x + v.c1 * g.y + v.c2 * g.z + v.c3;
}

@vertex fn vs(v: Vertex) -> Corner {
  let p = placed(v);
  let cofactor = mat3x3<f32>(cross(v.c1, v.c2), cross(v.c2, v.c0),
    cross(v.c0, v.c1));
  let side = select(1.0, -1.0, dot(v.c0, cofactor[0]) < 0.0);
  let w = side * (cofactor * unfold(v.octahedral));
  let n = select(vec3<f32>(0.0), normalize(w), dot(w, w) > 0.0);
  return Corner(clip * vec4<f32>(p, 1.0), n, v.grid.w, v.row.yz,
    v.paint.rgb, v.surface, p, v.row.x);
}

fn selected(face: u32, bits: vec2<u32>) -> bool {
  if ((bits.x & 1u) != 0u) { return true; }
  if (bits.y == 0xFFFFFFFFu) { return false; }
  return ((words[bits.y + face / 32u] >> (face % 32u)) & 1u) != 0u;
}

fn mark(paint: vec3<f32>) -> vec3<f32> {
  let luminance = dot(paint, vec3<f32>(0.2126, 0.7152, 0.0722));
  let tint = select(vec3<f32>(0.6, 0.9, 1.0), vec3<f32>(0.05, 0.15, 0.6),
    luminance > 0.5);
  return mix(paint, tint, 0.6);
}

// The sRGB EOTF.
fn linear(c: vec3<f32>) -> vec3<f32> {
  return select(pow((c + 0.055) / 1.055, vec3<f32>(2.4)), c / 12.92,
    c <= vec3<f32>(0.04045));
}

// The sRGB OETF.
fn encoded(c: vec3<f32>) -> vec3<f32> {
  return select(1.055 * pow(c, vec3<f32>(1.0 / 2.4)) - 0.055, 12.92 * c,
    c <= vec3<f32>(0.0031308));
}

// Khronos PBR Neutral, as its reference code.
fn neutral(color: vec3<f32>) -> vec3<f32> {
  let start = 0.76;
  let desaturation = 0.15;
  let x = min(color.r, min(color.g, color.b));
  let offset = select(0.04, x - 6.25 * x * x, x < 0.08);
  let c = color - offset;
  let peak = max(c.r, max(c.g, c.b));
  if (peak < start) { return c; }
  let d = 1.0 - start;
  let top = 1.0 - d * d / (peak + d - start);
  let g = 1.0 - 1.0 / (desaturation * (peak - top) + 1.0);
  return mix(c * (top / peak), vec3<f32>(top), g);
}

// A softbox of half extents h centred on direction c, seen along d: tangent
// coordinates dot(d, t) / dot(d, c), t horizontal and vertical across c,
// each edge softened over s.
fn softbox(d: vec3<f32>, c: vec3<f32>, h: vec2<f32>, s: f32) -> f32 {
  let facing = dot(d, c);
  if (facing <= 0.0) { return 0.0; }
  let across = normalize(cross(vec3<f32>(0.0, 1.0, 0.0), c));
  let x = abs(vec2<f32>(dot(d, across), dot(d, cross(c, across)))) / facing;
  let e = vec2<f32>(1.0) - smoothstep(h - s, h + s, x);
  return e.x * e.y;
}

fn keyward() -> vec3<f32> {
  return normalize(vec3<f32>(${KEY.join(", ")}));
}

// The studio's linear radiance along view direction d, its highlights
// softened by perceptual roughness.
fn env(d: vec3<f32>, rough: f32) -> vec3<f32> {
  let s = 0.02 + 0.5 * rough;
  let key = softbox(d, keyward(), vec2<f32>(${KEY_BOX.join(", ")}), s);
  let rim = softbox(d, normalize(vec3<f32>(0.8, 0.3, -0.5)),
    vec2<f32>(0.09, 0.7), s);
  return vec3<f32>(mix(0.05, 0.6, smoothstep(-0.3, 0.6, d.y)) + 6.0 * key +
    3.0 * rim);
}

// The split-sum scale and bias of F0, Karis's analytic approximation.
fn split(rough: f32, nv: f32) -> vec2<f32> {
  let r = rough * vec4<f32>(-1.0, -0.0275, -0.572, 0.022) +
    vec4<f32>(1.0, 0.0425, 1.04, -0.04);
  let a004 = min(r.x * r.x, exp2(-9.28 * nv)) * r.x + r.y;
  return vec2<f32>(-1.04, 1.04) * a004 + r.zw;
}

// The key light, irradiance 3: Lambert, GGX D, height-correlated Smith V and
// Schlick F.
fn keyed(n: vec3<f32>, v: vec3<f32>, f0: vec3<f32>, diffuse: vec3<f32>,
    alpha: f32) -> vec3<f32> {
  let l = keyward();
  let nl = dot(n, l);
  if (nl <= 0.0) { return vec3<f32>(0.0); }
  let h = normalize(l + v);
  let nh = max(dot(n, h), 0.0);
  let nv = max(dot(n, v), 1e-4);
  let a2 = alpha * alpha;
  let q = nh * nh * (a2 - 1.0) + 1.0;
  let d = a2 / (PI * q * q);
  let vis = 0.5 / (nl * sqrt(nv * nv * (1.0 - a2) + a2) +
    nv * sqrt(nl * nl * (1.0 - a2) + a2));
  let f = f0 + (1.0 - f0) * pow(clamp(1.0 - dot(v, h), 0.0, 1.0), 5.0);
  return ((1.0 - f) * diffuse / PI + f * d * vis) * 3.0 * nl;
}

@fragment fn fs(c: Corner) -> @location(0) vec4<f32> {
  let flat = normalize(cross(dpdx(c.world), dpdy(c.world)));
  let w = select(normalize(c.n), flat, dot(c.n, c.n) == 0.0);
  let frame = transpose(mat3x3<f32>(view.right.xyz, view.up.xyz,
    view.back.xyz));
  let v = frame * normalize(view.eye.xyz - c.world * view.eye.w);
  let n = select(frame * w, -(frame * w), dot(frame * w, v) < 0.0);
  let paint = select(c.paint, mark(c.paint), selected(c.face, c.bits));
  let base = linear(paint);
  let metallic = c.surface.x;
  let rough = clamp(c.surface.y, 0.045, 1.0);
  let f0 = mix(vec3<f32>(0.04), base, metallic);
  let diffuse = base * (1.0 - metallic);
  let ab = split(rough, max(dot(n, v), 1e-4));
  let light = diffuse * env(n, 1.0) +
    env(reflect(-v, n), rough) * (f0 * ab.x + ab.y) +
    keyed(n, v, f0, diffuse, rough * rough);
  return vec4<f32>(encoded(neutral(light)), 1.0);
}

@fragment fn hit(c: Corner) -> @location(0) vec4<u32> {
  return vec4<u32>(c.id, c.face, bitcast<u32>(c.at.z), 1u);
}

@vertex fn caster(v: Vertex) -> @builtin(position) @invariant vec4<f32> {
  return light.clip * vec4<f32>(placed(v), 1.0);
}

struct Floor {
  @builtin(position) at: vec4<f32>,
  @location(0) seen: vec3<f32>, // light-space x, y and z
}

// The catcher at depth 0.5, where the pipeline's compare always passes.
@vertex fn ground(@builtin(vertex_index) k: u32) -> Floor {
  var order = array<u32, 6>(0u, 1u, 2u, 0u, 2u, 3u);
  let p = light.corners[order[k]];
  let c = clip * p;
  return Floor(vec4<f32>(c.xy, 0.5 * c.w, c.w), (light.clip * p).xyz);
}

// Light-space x, y, the map's clip space, as a texture coordinate.
fn mapped(q: vec2<f32>) -> vec2<f32> {
  return vec2<f32>(0.5 + 0.5 * q.x, 0.5 - 0.5 * q.y);
}

fn plane(q: vec2<f32>) -> f32 {
  return dot(light.ground.xy, q) + light.ground.z;
}

// Map texel p's blocker count, 1 when its nearest caster lies above the
// plane, and that caster's height above the plane along the key in 255ths of
// light.reach.z; 0, 0 when none does. A cleared texel, 1, holds no caster:
// the casters' depths lie inside 0 to 1.
fn blocker(p: vec2<u32>, side: u32) -> vec2<u32> {
  let q = (vec2<f32>(p) + 0.5) / f32(side) * vec2<f32>(2.0, -2.0) +
    vec2<f32>(-1.0, 1.0);
  let z = plane(q);
  let b = textureLoad(depths, vec2<i32>(p), 0);
  if (b >= min(z, 1.0)) { return vec2<u32>(0u); }
  return vec2<u32>(1u, u32(clamp(round((z - b) / light.reach.z * 255.0), 0.0,
    255.0)));
}

var<workgroup> scan: array<vec2<u32>, 256>;

// The inclusive prefix sum of value over the workgroup's invocations, by
// Hillis-Steele, then the sum of all.
fn prefix(t: u32, value: vec2<u32>) -> vec4<u32> {
  scan[t] = value;
  for (var d = 1u; d < 256u; d *= 2u) {
    workgroupBarrier();
    var before = vec2<u32>(0u);
    if (t >= d) { before = scan[t - d]; }
    workgroupBarrier();
    scan[t] += before;
  }
  workgroupBarrier();
  let sums = vec4<u32>(scan[t], scan[255]);
  workgroupBarrier();
  return sums;
}

// rows turns each map row's blockers into their running sums along x, in 256
// texel chunks; columns then sums those along y in place, so a texel holds
// the sums over the texels at or before it in both. The sums are u32 and
// cannot overflow: a count is at most side², a height sum at most 255 side²,
// and SHADOW_SIDE keeps that below 2³².
@compute @workgroup_size(256) fn rows(@builtin(workgroup_id) g: vec3<u32>,
    @builtin(local_invocation_index) t: u32) {
  let side = textureDimensions(depths).x;
  var carry = vec2<u32>(0u);
  for (var x0 = 0u; x0 < side; x0 += 256u) {
    let p = vec2<u32>(x0 + t, g.x);
    var value = vec2<u32>(0u);
    if (p.x < side) { value = blocker(p, side); }
    let sums = prefix(t, value);
    if (p.x < side) {
      textureStore(counts, p, vec4<u32>(carry.x + sums.x, 0u, 0u, 0u));
      textureStore(heights, p, vec4<u32>(carry.y + sums.y, 0u, 0u, 0u));
    }
    carry += sums.zw;
  }
}

@compute @workgroup_size(256) fn columns(@builtin(workgroup_id) g: vec3<u32>,
    @builtin(local_invocation_index) t: u32) {
  let side = textureDimensions(counts).y;
  var carry = vec2<u32>(0u);
  for (var y0 = 0u; y0 < side; y0 += 256u) {
    let p = vec2<u32>(g.x, y0 + t);
    var value = vec2<u32>(0u);
    if (p.y < side) {
      value = vec2<u32>(textureLoad(counts, p).x, textureLoad(heights, p).x);
    }
    let sums = prefix(t, value);
    if (p.y < side) {
      textureStore(counts, p, vec4<u32>(carry.x + sums.x, 0u, 0u, 0u));
      textureStore(heights, p, vec4<u32>(carry.y + sums.y, 0u, 0u, 0u));
    }
    carry += sums.zw;
  }
}

// The blockers' count and height sum over map texels x < c.x and y < c.y.
fn summed(c: vec2<i32>) -> vec2<u32> {
  if (c.x == 0 || c.y == 0) { return vec2<u32>(0u); }
  return vec2<u32>(textureLoad(countSums, c - 1, 0).x,
    textureLoad(heightSums, c - 1, 0).x);
}

// The blockers' count and height sum over the rectangle lo to hi of map
// texel coordinates, texel i covering i to i + 1, as an exact box filter: a
// sum from the origin to a point is bilinear between whole coordinates, so
// each corner's is the bilinear blend of the four summed there. The result
// is the blend's 16 terms, each the exact u32 sum over a rectangle between
// the corners' whole coordinates, so no large total is rounded; such a
// rectangle runs backwards, and its sum is negative, when one of its sides
// lies inside one texel's width. The map's outside holds no blocker.
fn boxed(lo: vec2<f32>, hi: vec2<f32>) -> vec2<f32> {
  let size = vec2<i32>(textureDimensions(countSums));
  let a = clamp(lo, vec2<f32>(0.0), vec2<f32>(size));
  let b = clamp(hi, vec2<f32>(0.0), vec2<f32>(size));
  let i0 = min(vec2<i32>(floor(a)), size - 1);
  let i1 = min(vec2<i32>(floor(b)), size - 1);
  let f0 = a - vec2<f32>(i0);
  let f1 = b - vec2<f32>(i1);
  // at[4 y + x]: x and y index i0, i0 + 1, i1 and i1 + 1.
  var at: array<vec2<u32>, 16>;
  for (var k = 0; k < 16; k++) {
    let j = vec2<i32>(k % 4, k / 4);
    at[k] = summed(select(i0, i1, j >= vec2<i32>(2)) + j % 2);
  }
  var sum = vec2<f32>(0.0);
  for (var k = 0; k < 16; k++) {
    // k's bits pick lo's lattice point in x and y, then hi's.
    let l = vec2<i32>(k & 1, (k >> 1) & 1);
    let h = vec2<i32>((k >> 2) & 1, (k >> 3) & 1);
    let w = select(1.0 - f0, f0, l == vec2<i32>(1)) *
      select(1.0 - f1, f1, h == vec2<i32>(1));
    let x = vec2<i32>(l.x, 2 + h.x);
    let y = vec2<i32>(l.y, 2 + h.y);
    let g = at[4 * y.y + x.y] - at[4 * y.y + x.x] - at[4 * y.x + x.y] +
      at[4 * y.x + x.x];
    let back = i1 + h < i0 + l;
    let s = select(vec2<f32>(g), -vec2<f32>(vec2<u32>(0u) - g),
      back.x != back.y);
    sum += w.x * w.y * s;
  }
  return sum;
}

// q's rectangle of half extents e in light x and y, as map texel
// coordinates, its lowest corner then its highest.
fn texels(q: vec2<f32>, e: vec2<f32>) -> mat2x2<f32> {
  let side = f32(textureDimensions(countSums).x);
  return mat2x2<f32>(mapped(q + vec2<f32>(-e.x, e.y)) * side,
    mapped(q + vec2<f32>(e.x, -e.y)) * side);
}

// The search region is the softbox as the fragment sees it at the tallest
// caster's height; the blockers' mean height there sets the penumbra
// rectangle, the softbox as the fragment sees it at that height and at least
// a texel wide, and V is 1 less the share of it the blockers cover. No
// blocker in the search region leaves V 1.
@fragment fn shadow(f: Floor) -> @location(0) vec4<f32> {
  let side = f32(textureDimensions(countSums).x);
  let search = texels(f.seen.xy, light.reach.z * light.reach.xy);
  let found = boxed(search[0], search[1]);
  var v = 1.0;
  if (found.x > 0.0) {
    let height = found.y / found.x / 255.0 * light.reach.z;
    let r = texels(f.seen.xy, max(height * light.reach.xy, vec2<f32>(1.0 / side)));
    let e = r[1] - r[0];
    v = clamp(1.0 - boxed(r[0], r[1]).x / (e.x * e.y), 0.0, 1.0);
  }
  let ambient = env(vec3<f32>(0.0, 1.0, 0.0), 1.0).x;
  let key = 3.0 * keyward().y / PI;
  return vec4<f32>(encoded(vec3<f32>((ambient + key * v) / (ambient + key))),
    1.0);
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

// A pick that finds no instance; 0x3F800000 is 1.0's bits.
const NONE = [-1, 0, 0x3F800000];

const ROW = 72; // bytes in an instance row
const PLACED = ROW + 12; // a row with its mesh's origin, as grouped writes it
const QUERIES = 4096; // WebGPU's most queries in one query set

// The rows of the inverse of m, a column-major 4x4, in binary64 by
// Gauss-Jordan elimination with partial pivoting; null when m is singular.
function inverse(m) {
  const a = [0, 1, 2, 3].map((r) => [0, 1, 2, 3].map((c) => m[4 * c + r])
    .concat([0, 1, 2, 3].map((c) => (c === r ? 1 : 0))));
  for (let c = 0; c < 4; c++) {
    let p = c;
    for (let r = c + 1; r < 4; r++)
      if (Math.abs(a[r][c]) > Math.abs(a[p][c])) p = r;
    if (a[p][c] === 0) return null;
    [a[c], a[p]] = [a[p], a[c]];
    const pivot = a[c][c];
    for (let j = 0; j < 8; j++) a[c][j] /= pivot;
    for (let r = 0; r < 4; r++) {
      const f = a[r][c];
      if (r !== c) for (let j = 0; j < 8; j++) a[r][j] -= f * a[c][j];
    }
  }
  return a.map((row) => row.slice(4));
}

const dot3 = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
const unit = (a) => { const l = Math.hypot(...a); return a.map((v) => v / l); };

// SHADER's view of clip matrix m, column-major: eye, right, up and back, 16
// f32s, or null when m is singular or the view comes out not finite. eye is
// m⁻¹ (0, 0, 1, 0), the centre of projection (E, 1), or the direction (d, 0)
// when its w is 0, as an affine m's is exactly, negated if need be so that
// the way toward it lowers NDC depth; back is V at the unprojected NDC
// centre, right the unprojected (1, 0, 0.5) less that centre, orthogonal to
// back, and up the unprojected (0, 1, 0.5) less that centre, orthogonal to
// back and right: no cross product, whose sign would follow the world's
// handedness.
function viewer(m) {
  const inv = inverse(m);
  if (!inv) return null;
  const apply = (p) => inv.map((row) => row.reduce((s, v, k) => s + v * p[k], 0));
  const unproject = (x, y, z) => {
    const [a, b, c, w] = apply([x, y, z, 1]);
    return [a / w, b / w, c / w];
  };
  const minus = (a, b) => a.map((v, k) => v - b[k]);
  const h = apply([0, 0, 1, 0]);
  const length = Math.hypot(h[0], h[1], h[2]);
  let eye = h[3] !== 0
    ? [h[0] / h[3], h[1] / h[3], h[2] / h[3], 1]
    : [h[0] / length, h[1] / length, h[2] / length, 0];
  const centre = unproject(0, 0, 0.5);
  const toward = () => minus(eye.slice(0, 3), centre.map((v) => v * eye[3]));
  if (dot3(toward(), minus(unproject(0, 0, 0.25), unproject(0, 0, 0.75))) < 0)
    eye = eye.map((v) => -v);
  const back = unit(toward());
  const along = (a, b) => b.map((v) => v * dot3(a, b));
  const across = minus(unproject(1, 0, 0.5), centre);
  const right = unit(minus(across, along(across, back)));
  const above = minus(unproject(0, 1, 0.5), centre);
  const up = unit(minus(minus(above, along(above, back)), along(above, right)));
  const view = Float32Array.of(...eye, ...right, 0, ...up, 0, ...back, 0);
  return view.every(Number.isFinite) ? view : null;
}

const cross3 = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
  a[0] * b[1] - a[1] * b[0]];
const scaled = (a, k) => a.map((v) => v * k);
const plus = (...vs) => [0, 1, 2].map((k) => vs.reduce((s, v) => s + v[k], 0));

// The studio's view, viewer's view with the studio's axes, in binary64: the
// view's own with no plane; with a ground plane, up is its normal n, right r
// the view's right laid on the plane, or, where that right lies along n, s
// times n cross the view's back laid on it, and back s r cross n, s the sign
// of the view's right · (up × back), so the studio keeps the view's
// handedness. A right laid shorter than 1e-6 lies along n: an f32 matrix
// rolled 90° leaves about 4e-8.
function studio(view, plane) {
  if (!plane) return view.slice();
  const [right, up, back] = [4, 8, 12].map((k) => Array.from(view.subarray(k, k + 3)));
  const n = unit(Array.from(plane.subarray(0, 3)));
  const laid = (a) => plus(a, scaled(n, -dot3(a, n)));
  const s = Math.sign(dot3(right, cross3(up, back)));
  const along = laid(right);
  const r = Math.hypot(...along) >= 1e-6 ? unit(along)
    : scaled(unit(cross3(n, laid(back))), s);
  return Float32Array.of(...view.subarray(0, 4), ...r, 0, ...n, 0,
    ...scaled(cross3(r, n), s), 0);
}

// The key light's world axes from the studio's view and the ground plane: x
// and y the key softbox's tangents, z toward the key, as shader's softbox
// takes them in studio coordinates; and the plane's unit normal n and d.
function keyAxes(axes, plane) {
  const c = unit(KEY);
  const across = unit(cross3([0, 1, 0], c));
  const [right, up, back] = [4, 8, 12].map((k) => Array.from(axes.subarray(k, k + 3)));
  const world = (a) => plus(scaled(right, a[0]), scaled(up, a[1]), scaled(back, a[2]));
  const length = Math.hypot(plane[0], plane[1], plane[2]);
  return { x: world(across), y: world(cross3(c, across)), z: world(c),
    n: Array.from(plane.subarray(0, 3), (v) => v / length), d: plane[3] / length };
}

// The Light uniform, 40 f32s, for the casters fit found with the key's axes.
// The map's x and y cover the casters; its depth runs from the nearest
// caster, 0, to the farthest, 1, 1% to spare at each end, so a cleared 1
// holds none. The catcher grows the casters' rectangle on each side by
// h D / (1 - h |slope|), the farthest past them a point of the plane can
// see a caster against the softbox: h the softbox's half extent, D the tallest caster's
// distance to the plane along the key and slope the plane's rise along the
// key per unit along that axis. The growth is finite, since the key stands
// 36.6° above the ground, more than the softbox's half-angle atan(0.45) =
// 24.2°. An axis in which the casters have no extent, all of them a point or
// a sheet seen edge-on, takes a spare of a millionth of their largest extent,
// so the fit keeps the casters' own scale whatever the scene's units.
// Casters with no extent at all draw no shadow: fit answers null for them.
function lit(fitted, a) {
  const { lo, hi, near, far, tall } = fitted;
  const nx = dot3(a.n, a.x), ny = dot3(a.n, a.y), nz = dot3(a.n, a.z);
  const spare = Math.max(hi[0] - lo[0], hi[1] - lo[1], near - far) * 1e-6;
  const [cx, cy] = [(lo[0] + hi[0]) / 2, (lo[1] + hi[1]) / 2];
  const ex = Math.max((hi[0] - lo[0]) / 2, spare);
  const ey = Math.max((hi[1] - lo[1]) / 2, spare);
  const pad = Math.max(near - far, spare) / 100;
  const top = near + pad;
  const span = near - far + 2 * pad;
  // The clip matrix's rows: x, y, depth and w.
  const rows = [[...scaled(a.x, 1 / ex), -cx / ex], [...scaled(a.y, 1 / ey), -cy / ey],
    [...scaled(a.z, -1 / span), top / span], [0, 0, 0, 1]];
  const D = tall / nz;
  const gx = KEY_BOX[0] * D / (1 - KEY_BOX[0] * Math.abs(nx / nz));
  const gy = KEY_BOX[1] * D / (1 - KEY_BOX[1] * Math.abs(ny / nz));
  const corner = (x, y) => [...plus(scaled(a.x, x), scaled(a.y, y),
    scaled(a.z, -(a.d + x * nx + y * ny) / nz)), 1];
  const out = new Float32Array(40);
  for (let c = 0; c < 4; c++) for (let r = 0; r < 4; r++) out[4 * c + r] = rows[r][c];
  out.set([...corner(lo[0] - gx, lo[1] - gy), ...corner(hi[0] + gx, lo[1] - gy),
    ...corner(hi[0] + gx, hi[1] + gy), ...corner(lo[0] - gx, hi[1] + gy)], 16);
  out.set([ex * nx / (nz * span), ey * ny / (nz * span),
    (top + (a.d + cx * nx + cy * ny) / nz) / span, 0], 32);
  out.set([span * KEY_BOX[0] / ex, span * KEY_BOX[1] / ey, D / span, 0], 36);
  return out;
}

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
// each followed by origin(handle)'s three u32s in PLACED bytes, and the
// [handle, first, count] runs over them.
function grouped(rows, count, origin) {
  const v = new DataView(rows.buffer, rows.byteOffset, rows.byteLength);
  const groups = new Map();
  for (let i = 0; i < count; i++) {
    const handle = v.getUint32(i * ROW + 48, true);
    if (!groups.has(handle)) groups.set(handle, []);
    groups.get(handle).push(i);
  }
  const ordered = new Uint8Array(count * PLACED);
  const out = new DataView(ordered.buffer);
  const runs = [];
  let at = 0;
  for (const [handle, list] of groups) {
    runs.push([handle, at, list.length]);
    const o = origin(handle);
    for (const i of list) {
      ordered.set(rows.subarray(i * ROW, (i + 1) * ROW), PLACED * at);
      o.forEach((v, k) => out.setUint32(PLACED * at + ROW + 4 * k, v, true));
      at++;
    }
  }
  return [ordered, runs];
}

// The renderer holds each mesh by handle as a vertex buffer of its 12-byte
// vertices and an index buffer of its u16 triangles, and its origin; draw
// writes the rows, each with its mesh's origin, to an instance buffer and
// issues one indexed, instanced draw per handle, and
// highlight writes the words to a storage buffer. Each buffer is padded to
// the 4 bytes WebGPU's sizes and writes need, and the instance and highlight
// buffers grow when a write needs more. draw samples 4 times per pixel: into
// a 4x colour texture resolved into the canvas's, then discarded, against a
// 4x depth texture. The depth texture keeps the last draw's depth, cleared
// to 1 when made, the matrix buffer its matrix and the view buffer studio's
// view of the ground held then and of the last view viewer gave, the identity
// matrix's before any. While a ground is held, a square depth32float shadow
// map whose side is the drawing buffer's larger side, at most SHADOW_SIDE,
// exists with its two r32uint tables, and a draw whose eye is above the plane
// and some of whose instances rise above it first draws them into the map
// along the key, sums the tables from it, then draws the catcher first in its
// main pass. occlude draws its boxes
// against them in passes of at most QUERIES occlusion queries, one per box,
// and reads the resolved sample counts back through buffers of its own,
// released once read, so a box passes when any sample does. pick,
// single-sample, submits its probe draw of the last draw's runs before it
// returns, so queue order puts it after that draw and before any later write,
// and maps the pixel by the drawing buffer's size when it is called, the size
// pointer events give. A buffer destroyed while a submitted pick uses it lives
// until the pick's work completes (WebGPU's destroy).
function setup(canvas, context, device, release, fail) {
  device.lost.then((info) => fail(`WebGPU device lost: ${info.message}`));
  device.addEventListener("uncapturederror", (e) => fail(`WebGPU: ${e.error.message}`));
  const format = navigator.gpu.getPreferredCanvasFormat();
  context.configure({ device, format, alphaMode: "opaque" });
  const module = device.createShaderModule({ code: SHADER });
  const column = (k) =>
    ({ shaderLocation: 1 + k, offset: 12 * k, format: "float32x3" });
  const layouts = [
    { arrayStride: 12, attributes: [
      { shaderLocation: 0, offset: 0, format: "uint16x4" },
      { shaderLocation: 6, offset: 8, format: "snorm16x2" }] },
    { arrayStride: PLACED, stepMode: "instance", attributes: [
      column(0), column(1), column(2), column(3),
      { shaderLocation: 5, offset: 52, format: "uint32x3" },
      { shaderLocation: 7, offset: 64, format: "unorm8x4" },
      { shaderLocation: 8, offset: 68, format: "unorm16x2" },
      { shaderLocation: 9, offset: ROW, format: "uint32x3" }] },
  ];
  const pipeline = device.createRenderPipeline({
    layout: "auto",
    vertex: { module, entryPoint: "vs", buffers: layouts },
    fragment: { module, entryPoint: "fs", targets: [{ format }] },
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth24plus", depthWriteEnabled: true, depthCompare: "less" },
    multisample: { count: 4 },
  });
  const prober = device.createRenderPipeline({
    layout: "auto",
    vertex: { module, entryPoint: "vs", buffers: layouts },
    fragment: { module, entryPoint: "hit", targets: [{ format: "rgba32uint" }] },
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
    multisample: { count: 4 },
  });
  const caster = device.createRenderPipeline({
    layout: "auto",
    vertex: { module, entryPoint: "caster", buffers: layouts },
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth32float", depthWriteEnabled: true, depthCompare: "less" },
  });
  const catcher = device.createRenderPipeline({
    layout: "auto",
    vertex: { module, entryPoint: "ground" },
    fragment: { module, entryPoint: "shadow", targets: [{ format }] },
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth24plus", depthWriteEnabled: false, depthCompare: "always" },
    multisample: { count: 4 },
  });
  const rowScan = device.createComputePipeline({ layout: "auto",
    compute: { module, entryPoint: "rows" } });
  const columnScan = device.createComputePipeline({ layout: "auto",
    compute: { module, entryPoint: "columns" } });
  const { UNIFORM, STORAGE, VERTEX, INDEX, COPY_DST, COPY_SRC, QUERY_RESOLVE, MAP_READ } =
    GPUBufferUsage;
  const matrix = device.createBuffer({ size: 64, usage: UNIFORM | COPY_DST });
  // The last view viewer gave, kept for a draw whose matrix is singular.
  let seen = viewer(Float32Array.of(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1));
  const view = device.createBuffer({ size: 64, usage: UNIFORM | COPY_DST,
    mappedAtCreation: true });
  new Float32Array(view.getMappedRange()).set(seen);
  view.unmap();
  const boxBind = device.createBindGroup({
    layout: occluder.getBindGroupLayout(0),
    entries: [{ binding: 0, resource: { buffer: matrix } }],
  });
  const light = device.createBuffer({ size: 160, usage: UNIFORM | COPY_DST });
  const casterBind = device.createBindGroup({
    layout: caster.getBindGroupLayout(0),
    entries: [{ binding: 3, resource: { buffer: light } }],
  });
  let plane = null; // the ground held, or null
  // The shadow map, its tables and the bind groups of rows, columns and the
  // catcher, while a ground is held.
  let shadows = null;
  const shadowMap = () => {
    const side = Math.min(Math.max(canvas.width, canvas.height), SHADOW_SIDE);
    const { RENDER_ATTACHMENT, TEXTURE_BINDING, STORAGE_BINDING } = GPUTextureUsage;
    const map = device.createTexture({ size: [side, side], format: "depth32float",
      usage: RENDER_ATTACHMENT | TEXTURE_BINDING });
    const table = () => device.createTexture({ size: [side, side], format: "r32uint",
      usage: STORAGE_BINDING | TEXTURE_BINDING });
    const [counts, heights] = [table(), table()];
    const bound = (user, entries) => device.createBindGroup({
      layout: user.getBindGroupLayout(0),
      entries: entries.map(([binding, resource]) => ({ binding, resource })),
    });
    shadows = { map, counts, heights,
      rows: bound(rowScan, [[3, { buffer: light }], [4, map.createView()],
        [5, counts.createView()], [6, heights.createView()]]),
      columns: bound(columnScan, [[5, counts.createView()], [6, heights.createView()]]),
      catcher: bound(catcher, [[0, { buffer: matrix }], [3, { buffer: light }],
        [7, counts.createView()], [8, heights.createView()]]) };
  };
  const unshadow = () => {
    for (const texture of [shadows.map, shadows.counts, shadows.heights]) texture.destroy();
    shadows = null;
  };
  const probeMatrix = device.createBuffer({ size: 64, usage: UNIFORM | COPY_DST });
  const probeBind = device.createBindGroup({
    layout: prober.getBindGroupLayout(0),
    entries: [{ binding: 0, resource: { buffer: probeMatrix } }],
  });
  const texel = device.createTexture({ size: [1, 1], format: "rgba32uint",
    usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.COPY_SRC });
  const texelDepth = device.createTexture({ size: [1, 1], format: "depth24plus",
    usage: GPUTextureUsage.RENDER_ATTACHMENT });
  const depthTexture = () => {
    const texture = device.createTexture({
      size: [canvas.width, canvas.height], format: "depth24plus", sampleCount: 4,
      usage: GPUTextureUsage.RENDER_ATTACHMENT,
    });
    const encoder = device.createCommandEncoder();
    encoder.beginRenderPass({ colorAttachments: [], depthStencilAttachment: {
      view: texture.createView(), depthClearValue: 1, depthLoadOp: "clear", depthStoreOp: "store",
    } }).end();
    device.queue.submit([encoder.finish()]);
    return texture;
  };
  const colorTexture = () => device.createTexture({
    size: [canvas.width, canvas.height], format, sampleCount: 4,
    usage: GPUTextureUsage.RENDER_ATTACHMENT,
  });
  let depth = depthTexture();
  let color = colorTexture();
  // A held handle -> { vertices, indices, count, origin, lo, hi }, lo and hi
  // the corners of the box of the grid positions (origin plus u16s) of the
  // vertices its triangles name, null for a mesh of no triangles: record 13
  // allows vertices no triangle names.
  const meshes = new Map();
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
        { binding: 1, resource: { buffer: words } },
        { binding: 2, resource: { buffer: view } }],
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
  const mesh = (handle, vertexCount, triangleCount, origin, vertices,
      triangles) => {
    const held = filled(vertices, VERTEX);
    let indices;
    try {
      indices = filled(triangles, INDEX);
    } catch (e) {
      held.destroy();
      throw e;
    }
    let lo = null, hi = null;
    for (const k of triangles) {
      const g = Array.from(vertices.subarray(6 * k, 6 * k + 3),
        (v, j) => v + origin[j]);
      lo = lo ? lo.map((v, j) => Math.min(v, g[j])) : g;
      hi = hi ? hi.map((v, j) => Math.max(v, g[j])) : g;
    }
    meshes.set(handle, { vertices: held, indices, count: 3 * triangleCount,
      origin: origin.slice(), lo, hi });
  };
  const unmesh = (handle) => {
    const { vertices, indices } = meshes.get(handle);
    vertices.destroy();
    indices.destroy();
    meshes.delete(handle);
  };

  const resize = () => {
    depth.destroy();
    color.destroy();
    depth = depthTexture();
    color = colorTexture();
    if (shadows) {
      unshadow();
      shadowMap();
    }
  };

  const ground = (held) => {
    plane = held && held.slice();
    if (plane && !shadows) shadowMap();
    if (!plane && shadows) unshadow();
  };

  // The casters among the count rows of ordered, by their meshes' grid boxes
  // taken to world, in the key's axes: lo and hi the x, y rectangle covering
  // them, near and far their greatest and least z, tall the most any rises
  // above the plane; null when none does or when they have no extent in any
  // of x, y and z. A row wholly on or below the plane, or of a mesh of no
  // triangles, casts nothing.
  const fit = (ordered, count, axes) => {
    const v = new DataView(ordered.buffer, ordered.byteOffset, ordered.byteLength);
    const lo = [Infinity, Infinity], hi = [-Infinity, -Infinity];
    let near = -Infinity, far = Infinity, tall = 0;
    for (let i = 0; i < count; i++) {
      const c = Array.from({ length: 12 }, (_, k) => v.getFloat32(i * PLACED + 4 * k, true));
      const { lo: g0, hi: g1 } = meshes.get(v.getUint32(i * PLACED + 48, true));
      if (!g0) continue;
      const corners = [];
      for (let k = 0; k < 8; k++) {
        const g = [0, 1, 2].map((j) => ((k >> j) & 1 ? g1[j] : g0[j]));
        corners.push([0, 1, 2].map((j) => c[j] * g[0] + c[3 + j] * g[1] +
          c[6 + j] * g[2] + c[9 + j]));
      }
      const rise = Math.max(...corners.map((p) => dot3(axes.n, p) + axes.d));
      if (!(rise > 0)) continue;
      tall = Math.max(tall, rise);
      for (const p of corners) {
        const [x, y, z] = [dot3(axes.x, p), dot3(axes.y, p), dot3(axes.z, p)];
        lo[0] = Math.min(lo[0], x); hi[0] = Math.max(hi[0], x);
        lo[1] = Math.min(lo[1], y); hi[1] = Math.max(hi[1], y);
        near = Math.max(near, z); far = Math.min(far, z);
      }
    }
    const wide = Math.max(hi[0] - lo[0], hi[1] - lo[1], near - far);
    return tall > 0 && wide > 0 ? { lo, hi, near, far, tall } : null;
  };

  // The last draw's runs and its matrix, copied, as a pick reads them; null
  // before the first.
  let last = null;

  // rows, count instance rows of held meshes (turn.js); 0 rows clears.
  const draw = (rows, count, clip) => {
    const [ordered, runs] = grouped(rows, count, (h) => meshes.get(h).origin);

    last = { runs, clip: new Float32Array(clip.slice().buffer) };
    if (count) {
      instances = grown(instances, ordered.byteLength, VERTEX | COPY_DST);
      device.queue.writeBuffer(instances, 0, ordered);
    }
    device.queue.writeBuffer(matrix, 0, clip);
    seen = viewer(last.clip) ?? seen;
    const axes = studio(seen, plane);
    device.queue.writeBuffer(view, 0, axes);
    const key = plane && keyAxes(axes, plane);
    const above = key && dot3(key.n, seen) + key.d * seen[3] > 0;
    const cast = above ? fit(ordered, count, key) : null;
    const encoder = device.createCommandEncoder();
    if (cast) {
      device.queue.writeBuffer(light, 0, lit(cast, key));
      const shade = encoder.beginRenderPass({ colorAttachments: [],
        depthStencilAttachment: { view: shadows.map.createView(), depthClearValue: 1,
          depthLoadOp: "clear", depthStoreOp: "store" } });
      shade.setPipeline(caster);
      shade.setBindGroup(0, casterBind);
      shade.setVertexBuffer(1, instances);
      for (const [handle, first, n] of runs) {
        const { vertices, indices, count: indexCount } = meshes.get(handle);
        shade.setVertexBuffer(0, vertices);
        shade.setIndexBuffer(indices, "uint16");
        shade.drawIndexed(indexCount, n, 0, 0, first);
      }
      shade.end();
      const sum = encoder.beginComputePass();
      sum.setPipeline(rowScan);
      sum.setBindGroup(0, shadows.rows);
      sum.dispatchWorkgroups(shadows.map.height);
      sum.setPipeline(columnScan);
      sum.setBindGroup(0, shadows.columns);
      sum.dispatchWorkgroups(shadows.map.width);
      sum.end();
    }
    const pass = encoder.beginRenderPass({
      colorAttachments: [{
        view: color.createView(),
        resolveTarget: context.getCurrentTexture().createView(),
        clearValue: [1, 1, 1, 1],
        loadOp: "clear",
        storeOp: "discard",
      }],
      depthStencilAttachment: { view: depth.createView(), depthClearValue: 1, depthLoadOp: "clear", depthStoreOp: "store" },
    });
    if (cast) {
      pass.setPipeline(catcher);
      pass.setBindGroup(0, shadows.catcher);
      pass.draw(6);
    }
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

  // The last draw's matrix composed with the map of pixel px, py of a W by H
  // buffer onto a 1x1 target, the pixel's centre onto the texel's.
  const probed = (px, py, W, H) => {
    const out = new Float32Array(16);
    for (let k = 0; k < 16; k += 4) {
      const [x, y, z, w] = last.clip.subarray(k, k + 4);
      out.set([W * x + (W - 2 * px - 1) * w, H * y + (1 - H + 2 * py) * w,
        z, w], k);
    }
    return out;
  };

  // [instance, face, depthBits] at pixel x, y (BigInts) of the last draw.
  const pick = async (x, y) => {
    const [width, height] = [canvas.width, canvas.height];
    if (!last?.runs.length || x < 0n || y < 0n || x >= BigInt(width) ||
        y >= BigInt(height))
      return NONE;
    device.queue.writeBuffer(probeMatrix, 0,
      probed(Number(x), Number(y), width, height));
    const encoder = device.createCommandEncoder();
    const pass = encoder.beginRenderPass({
      colorAttachments: [{ view: texel.createView(), clearValue: [0, 0, 0, 0],
        loadOp: "clear", storeOp: "store" }],
      depthStencilAttachment: { view: texelDepth.createView(),
        depthClearValue: 1, depthLoadOp: "clear", depthStoreOp: "discard" },
    });
    pass.setPipeline(prober);
    pass.setBindGroup(0, probeBind);
    pass.setVertexBuffer(1, instances);
    for (const [handle, first, n] of last.runs) {
      const { vertices, indices, count } = meshes.get(handle);
      pass.setVertexBuffer(0, vertices);
      pass.setIndexBuffer(indices, "uint16");
      pass.drawIndexed(count, n, 0, 0, first);
    }
    pass.end();
    const read = device.createBuffer({ size: 16, usage: MAP_READ | COPY_DST });
    encoder.copyTextureToBuffer({ texture: texel }, { buffer: read }, [1, 1]);
    device.queue.submit([encoder.finish()]);
    try {
      await read.mapAsync(GPUMapMode.READ);
      const [id, face, depth, hit] = new Uint32Array(read.getMappedRange());
      return hit ? [id, face, depth] : NONE;
    } finally {
      read.destroy();
    }
  };

  return { mesh, unmesh, highlight, ground, draw, pick, occlude, resize, release };
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
      ground: live((plane) => renderer.ground(plane)),
      pick: live((x, y) => renderer.pick(x, y)),
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
