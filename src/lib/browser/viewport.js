// viewport.js - the browser viewer as a component. mount runs a Habu Wasm
// module's turns (turn.js) on the main thread, holds its meshes on the GPU
// and draws their instances with WebGPU on the caller's canvas, and sends the
// canvas's pointer, wheel, key and size to the module, with a resize turn at
// mount and at each change of the drawing buffer's size or of devicePixelRatio.
// The caller owns the page: nothing here looks up the DOM, and every other
// record reaches the caller through a callback.
//
//   mount(canvas, { Module, module, fetch, rpc, onTree, onText, onSvg, onInput })
//     -> Promise<viewport>
//   Module                 turn.js's Module class, which the caller imports
//   module                 the module's URL, fetched with fetch
//   fetch(url, init)       window.fetch's contract; the module, and every
//                          fetch, post and request record, go through it
//   rpc(method, paramsJSON) an rpc record's method and params JSON text ->
//                          Promise of { text, binary }; a rejection is a
//                          transport failure
//   onTree(rows, selected) a tree record's rows and selected key (turn.js)
//   onText(text, error)    a text record, error false; a failure after mount
//                          resolved, error true (it is also logged)
//   onSvg(bytes)           an svg record's copied bytes, shown in place of the
//                          canvas, whose visibility it hides; null when a draw
//                          removes the shown image
//   onInput(enabled)       true as the module starts, then each input record
//
//   viewport.post(event)   { kind: "typed", text }, { kind: "control", key,
//                          opened } or { kind: "key", key }: the field's
//                          submitted text, a tree row's key, selected or, when
//                          opened is true, opened (turn.js's control event),
//                          or a page control's KeyboardEvent.key, a key turn
//                          with no modifiers; the promise settles
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
// and back: with no ground they are the view's own, so the studio turns with
// the camera; with a ground they are the ground record's (turn.js), up its
// normal and right and back turning about it with the camera, a turntable.
// V points from the fragment toward view's eye, a point when its w is 1 and
// a direction when 0. The normal is turned toward V: meshes are drawn
// without culling and may be open. hit, after vs, marks the nearest fragment
// in pick's one texel, with 1 in its fourth channel to tell a hit from the
// clear.
// The key casts the instances' soft shadow on the ground: caster draws every
// row, cast-only (flags bit 1) or not, into the map, SHADOW_SIDE square, by
// light.clip, whose x and y run along the key softbox's tangents and cover
// M, the ground record's, and whose depth runs away from the key over the
// casters' nearest to farthest key z; stamp, its fragment, sets in marks,
// for every surface, the height slices it covers in its texel; halve ORs
// each level of marks' 2x2 texels into the next, up to one texel; seen finds,
// at half the canvas's size, the share V of the softbox the ground sees past
// the casters, as the share of its rays through the slices that clear them
// (clearing, traced), into shade, and those rays' bits into clears; and
// ground draws the catcher, the quad light.corners on the plane, whose
// fragment, shadow, reads V there. Slice k
// of 64 holds the heights T 2^((k - 64) / 10) to T 2^((k - 63) / 10), T the
// casters' greatest height, and slice 0 every height below. vs, a pick's
// probe and so the depth occlude's view test reads take only the rows whose
// bit 1 is clear. The fit is the ground record's, never the rows', so a row
// outside the view still shades the ground in view, and the map's texel is
// the same whichever rows a module sends. The ground is not in the map: the
// plane's depth at light x, y is light.ground.x x + light.ground.y y +
// light.ground.z, light.reach.xy is the softbox's half extents in light x
// and y per unit of depth and light.reach.z T in units of depth;
// light.reach.w is 0. A caster below the plane neither shades it nor hides
// one above it from the key. The catcher reads white times
// (env(up, 1) + key V) / (env(up, 1) + key), key the key's irradiance on the
// ground, in linear light and not tone-mapped, so an unshadowed ground reads
// white as the clear does.
// occlude's key test draws each box's 64 footprints, one per ray, the ground
// whose ray can cross it, in shade's pixels, where crossed passes when the ray
// is one of the pixel's, its bit in clears is set and it crosses the box.
// box draws one occlude box per instance, under the camera's matrix, as the
// 12 triangles of its faces:
// vertex k takes corner corners[k], whose bit j picks the highest coordinate j
// in place of the lowest. A box whose corners' clip z lie on both sides of 0,
// the near plane, draws the full-viewport quad at depth 0 from vertices 0 to
// 5 in place of its faces, which that plane can cut away while the box's
// inside is in view.
//
// KEY is the key light's direction and KEY_BOX its softbox's half extents,
// per unit along KEY, in studio coordinates. SHADOW_SIDE is the shadow map's
// side, a power of two: a fixed side keeps the shadow's error, a texel over
// its distance, and its cost the same on every canvas.
const KEY = [-0.4, 0.6, 0.7];
const KEY_BOX = [0.45, 0.27];
const SHADOW_SIDE = 1024;
const ALL_CLEAR = 0xFFFF; // clears' bits for a pixel's 16 rays all clear
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
// marks, tree and grid are one buffer as caster, halve and seen use it.
@group(0) @binding(4) var<storage, read_write> marks: array<atomic<u32>>;
@group(0) @binding(5) var<storage, read_write> tree: array<u32>;
@group(0) @binding(6) var<uniform> halved: u32; // the level halve writes
@group(0) @binding(7) var<storage, read> grid: array<vec2<u32>>;
@group(0) @binding(8) var shade: texture_2d<f32>;
@group(0) @binding(9) var filtered: sampler;
@group(0) @binding(10) var clears: texture_2d<u32>;
// The caster pass's mesh: its vertices, three u32 each as the vertex buffer
// holds them, and its triangles' u16 indices, two to a u32.
@group(1) @binding(0) var<storage, read> verts: array<u32>;
@group(1) @binding(1) var<storage, read> tris: array<u32>;

const PI = 3.14159265358979;
const SIDE = ${SHADOW_SIDE}u;
const TOP = ${Math.log2(SHADOW_SIDE)}u; // marks' level of one texel
const NUDGE = 0.002; // how far ahead of t traced looks a cell up, in texels

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

// The height above the plane along the key over T, and the triangle's
// corners in map texel x, y and height share.
struct Cast {
  @builtin(position) @invariant at: vec4<f32>,
  @location(0) @interpolate(linear) height: f32,
  @location(1) @interpolate(flat) a: vec3<f32>,
  @location(2) @interpolate(flat) b: vec3<f32>,
  @location(3) @interpolate(flat) c: vec3<f32>,
}

// An instance's row as caster reads it.
struct Row {
  @location(1) c0: vec3<f32>,
  @location(2) c1: vec3<f32>,
  @location(3) c2: vec3<f32>,
  @location(4) c3: vec3<f32>,
  @location(9) origin: vec3<u32>,
}

// Vertex i of the mesh by light.clip, as placed takes it to world.
fn vertexAt(row: Row, i: u32) -> vec4<f32> {
  let w = 3u * i;
  let grid = vec3<u32>(verts[w] & 0xFFFFu, verts[w] >> 16u, verts[w + 1u] & 0xFFFFu);
  let g = vec3<f32>(grid + row.origin);
  return light.clip * vec4<f32>(row.c0 * g.x + row.c1 * g.y + row.c2 * g.z + row.c3, 1.0);
}

fn onMap(q: vec4<f32>) -> vec3<f32> {
  return vec3<f32>(mapped(q.xy) * f32(SIDE), (plane(q.xy) - q.z) / light.reach.z);
}

// Vertex v of a non-indexed draw of the mesh's triangles: corner v % 3 of
// triangle v / 3.
@vertex fn caster(@builtin(vertex_index) v: u32, row: Row) -> Cast {
  let t = v - v % 3u;
  var q: array<vec4<f32>, 3>;
  for (var k = 0u; k < 3u; k++) {
    let i = t + k;
    q[k] = vertexAt(row, (tris[i / 2u] >> (16u * (i & 1u))) & 0xFFFFu);
  }
  let p = q[v % 3u];
  return Cast(p, (plane(p.xy) - p.z) / light.reach.z, onMap(q[0]), onMap(q[1]),
    onMap(q[2]));
}

// Level l of marks, after every level below it: its first texel, then its
// side.
fn level(l: u32) -> vec2<u32> {
  let side = SIDE >> l;
  return vec2<u32>((4u * SIDE * SIDE - 4u * side * side) / 3u, side);
}

// The slice of height share h, 10 an octave: penumbra width grows with
// height; at 8 an octave V is up to 0.089 off traced rays, at 10 0.076.
fn slice(h: f32) -> u32 {
  return u32(clamp(floor(64.0 + 10.0 * log2(max(h, 1e-30))), 0.0, 63.0));
}

// The bits of slices a to b in a texel's two u32s of marks, slice k bit k % 32
// of word k / 32; none when b < a.
fn span(a: u32, b: u32) -> vec2<u32> {
  let base = vec2<u32>(0u, 32u);
  let s = max(vec2<u32>(a), base);
  let e = min(vec2<u32>(b), base + 31u);
  let m = (vec2<u32>(0xFFFFFFFFu) >> (31u - (max(e, s) - s))) << (s - base);
  return select(vec2<u32>(0u), m, s <= e);
}

fn cross2(a: vec2<f32>, b: vec2<f32>) -> f32 {
  return a.x * b.y - a.y * b.x;
}

// The least and most height share triangle a, b, c (map texel x, y and
// share) reaches in the texel from lo to lo + 1, whose centre, at share h,
// it covers. A linear function's extremes over the triangle cut to the texel
// lie at the cut's corners: the triangle's corners in the texel, the points
// where its edges cross the texel's sides and the texel's corners in it, each
// a mix of a, b and c, so the result stays within their shares however thin
// the triangle.
fn reached(a: vec3<f32>, b: vec3<f32>, c: vec3<f32>, lo: vec2<f32>, h: f32) -> vec2<f32> {
  var r = vec2<f32>(h);
  let hi = lo + 1.0;
  var v = array<vec3<f32>, 3>(a, b, c);
  for (var i = 0u; i < 3u; i++) {
    let p = v[i];
    let q = v[(i + 1u) % 3u];
    if (all(p.xy >= lo) && all(p.xy <= hi)) { r = vec2<f32>(min(r.x, p.z), max(r.y, p.z)); }
    for (var k = 0u; k < 4u; k++) {
      let j = k & 1u;
      let side = select(lo[j], hi[j], k >= 2u);
      let e = q[j] - p[j];
      if (e != 0.0) {
        let u = (side - p[j]) / e;
        let m = mix(p, q, clamp(u, 0.0, 1.0));
        let o = 1u - j;
        if (u >= 0.0 && u <= 1.0 && m[o] >= lo[o] && m[o] <= hi[o]) {
          r = vec2<f32>(min(r.x, m.z), max(r.y, m.z));
        }
      }
    }
  }
  let area = cross2(b.xy - a.xy, c.xy - a.xy);
  if (area != 0.0) {
    for (var k = 0u; k < 4u; k++) {
      let x = lo + vec2<f32>(f32(k & 1u), f32(k >> 1u));
      let wa = cross2(b.xy - x, c.xy - x) / area;
      let wb = cross2(c.xy - x, a.xy - x) / area;
      let wc = 1.0 - wa - wb;
      if (wa >= 0.0 && wb >= 0.0 && wc >= 0.0) {
        let s = clamp(wa * a.z + wb * b.z + wc * c.z, min(min(a.z, b.z), c.z),
          max(max(a.z, b.z), c.z));
        r = vec2<f32>(min(r.x, s), max(r.y, s));
      }
    }
  }
  return r;
}

// A fragment marks the slices of the heights its triangle reaches in its
// texel, so a steep face leaves no gap between texel centres, and a mark
// lies within a texel and a slice of a point of the triangle.
@fragment fn stamp(f: Cast) {
  if (f.height > 0.0) {
    let p = vec2<u32>(f.at.xy);
    let band = reached(f.a, f.b, f.c, floor(f.at.xy), f.height);
    let m = span(slice(band.x), slice(band.y));
    let i = 2u * (p.y * SIDE + p.x);
    for (var w = 0u; w < 2u; w++) {
      if (m[w] != 0u) { atomicOr(&marks[i + w], m[w]); }
    }
  }
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

// Level halved's texel: the OR of its 2x2 texels a level below.
@compute @workgroup_size(8, 8) fn halve(@builtin(global_invocation_id) g: vec3<u32>) {
  let dst = level(halved);
  let src = level(halved - 1u);
  if (g.x >= dst.y || g.y >= dst.y) { return; }
  for (var w = 0u; w < 2u; w++) {
    var m = 0u;
    for (var k = 0u; k < 4u; k++) {
      let c = 2u * g.xy + vec2<u32>(k & 1u, k >> 1u);
      m |= tree[2u * (src.x + c.y * src.y + c.x) + w];
    }
    tree[2u * (dst.x + g.y * dst.y + g.x) + w] = m;
  }
}

// 1 when the ray from map texel coordinate at, going dir texels as its height
// share t goes 0 to 1, crosses a marked slice, else 0. The ray is a machine
// over its level and t, starting at, and never rising past, the level whose
// cell is a quarter to a half of its run. Clear: t is 1 or past the map's edge.
// Advance: the cell's slices t to its exit are empty, so t moves to the exit,
// a level up only when the ray leaves the parent cell too. Descend: the cell
// is marked above level 0. Blocked: it is marked at level 0. t is f32, so a
// ray that fails to advance would hang the frame: it is blocked after 400
// steps, where the scenes' rays end within 48.
fn traced(at: vec2<f32>, dir: vec2<f32>) -> f32 {
  let safe = select(dir, vec2<f32>(1e-20), abs(dir) < vec2<f32>(1e-20));
  let inv = 1.0 / safe;
  let a = (vec2<f32>(0.0) - at) * inv;
  let b = (vec2<f32>(f32(SIDE)) - at) * inv;
  var t = max(max(min(a.x, b.x), min(a.y, b.y)), 0.0);
  let end = min(min(max(a.x, b.x), max(a.y, b.y)), 1.0);
  let ahead = step(vec2<f32>(0.0), safe);
  // A cell is looked up a little ahead of t, so a ray at its exit finds the
  // next cell.
  let nudge = (2.0 * ahead - 1.0) * NUDGE;
  let cap = u32(clamp(ceil(log2(max(max(abs(dir.x), abs(dir.y)), 1.0))) - 2.0, 0.0,
    f32(TOP)));
  var lvl = cap;
  var lo = slice(t);
  for (var n = 0u; n < 400u; n++) {
    if (t >= end) { return 0.0; }
    let size = f32(1u << lvl);
    let l = level(lvl);
    let cell = clamp(floor((at + safe * t + nudge) / size), vec2<f32>(0.0),
      vec2<f32>(f32(l.y) - 1.0));
    let far = ((cell + ahead) * size - at) * inv;
    let leave = min(max(min(far.x, far.y), t + 1e-6), end);
    let hi = slice(leave);
    let g = grid[l.x + u32(cell.y) * l.y + u32(cell.x)];
    if (all((g & span(lo, hi)) == vec2<u32>(0u))) {
      t = leave;
      lo = hi;
      // It leaves the parent cell when it leaves an odd cell forward or an
      // even one back across the axis it exits by.
      let across = far.x < far.y;
      let odd = (u32(select(cell.y, cell.x, across)) & 1u) == 1u;
      if (odd == (select(ahead.y, ahead.x, across) > 0.5)) { lvl = min(lvl + 1u, cap); }
    } else if (lvl == 0u) {
      return 1.0;
    } else {
      lvl--;
    }
  }
  return 1.0;
}

// Ray i of 64 from a ground point, in map texels per unit of height share: it
// leaves at c, -1 to 1 across the softbox, column i and row 19 i mod 64, a
// lattice taking each row and column once, and rises 1 + slope.c times as
// fast as the key's.
fn ray(i: u32) -> vec2<f32> {
  let reach = 0.5 * f32(SIDE) * light.reach.z * light.reach.xy;
  let slope = light.ground.xy * light.reach.xy;
  let c = (2.0 * vec2<f32>(vec2<u32>(i, (19u * i) % 64u)) + 1.0) / 64.0 - 1.0;
  return reach * c * vec2<f32>(1.0, -1.0) / (1.0 + dot(slope, c));
}

// The turn of shade's pixel px: a 2x2 block of pixels takes its four turns,
// a pixel's 16 rays i = 4 m + turn, so shadow's four taps give each all 64.
fn turn(px: vec2<f32>) -> u32 {
  return (u32(px.x) & 1u) + 2u * (u32(px.y) & 1u);
}

// Bit m set when ray 4 m + turn(px) from the ground point q, light x and y,
// traces clear, for shade's pixel px.
fn clearing(q: vec2<f32>, px: vec2<f32>) -> u32 {
  let at = mapped(q) * f32(SIDE);
  var bits = 0u;
  for (var m = 0u; m < 16u; m++) {
    if (traced(at, ray(4u * m + turn(px))) == 0.0) { bits |= 1u << m; }
  }
  return bits;
}

struct Seen {
  @location(0) v: vec4<f32>,
  @location(1) bits: vec4<u32>,
}

@fragment fn seen(f: Floor) -> Seen {
  let bits = clearing(f.seen.xy, floor(f.at.xy));
  return Seen(vec4<f32>(f32(countOneBits(bits)) / 16.0, 0.0, 0.0, 1.0),
    vec4<u32>(bits, 0u, 0u, 0u));
}

@fragment fn shadow(f: Floor) -> @location(0) vec4<f32> {
  let at = f.at.xy / (2.0 * vec2<f32>(textureDimensions(shade)));
  // Four taps half a texel apart weigh a 2x2 block's turns of V alike.
  let h = 0.5 / vec2<f32>(textureDimensions(shade));
  let v = 0.25 * (textureSampleLevel(shade, filtered, at + h, 0.0).x
    + textureSampleLevel(shade, filtered, at - h, 0.0).x
    + textureSampleLevel(shade, filtered, at + vec2<f32>(h.x, -h.y), 0.0).x
    + textureSampleLevel(shade, filtered, at - vec2<f32>(h.x, -h.y), 0.0).x);
  let ambient = env(vec3<f32>(0.0, 1.0, 0.0), 1.0).x;
  let key = 3.0 * keyward().y / PI;
  return vec4<f32>(encoded(vec3<f32>((ambient + key * v) / (ambient + key))),
    1.0);
}

// The lowest height share of slice k.
fn bottom(k: u32) -> f32 {
  return select(exp2((f32(k) - 64.0) / 10.0), 0.0, k == 0u);
}

// The height share of light-space point q.
fn share(q: vec4<f32>) -> f32 {
  return (plane(q.xy) - q.z) / light.reach.z;
}

// Ray r's footprint of a box, the ground whose ray r can cross it, in shade's
// pixels: the ground's world point; the box grown by a map texel and traced's
// nudge in light x and y and a slice in height share, as the world's axes take
// them, so every ray a mark of a triangle in the box blocks crosses it: the
// mark lies in the ray's texel and slice; the world steps of a map texel in
// light x and in y at a fixed height share, and of a unit of height share at
// a fixed light x, y; and r.
struct Reach {
  @builtin(position) at: vec4<f32>,
  @location(0) world: vec3<f32>,
  @location(1) @interpolate(flat) lo: vec3<f32>,
  @location(2) @interpolate(flat) hi: vec3<f32>,
  @location(3) @interpolate(flat) wx: vec3<f32>,
  @location(4) @interpolate(flat) wy: vec3<f32>,
  @location(5) @interpolate(flat) wh: vec3<f32>,
  @location(6) @interpolate(flat) r: u32,
}

// Vertex v % 6 of ray v / 6's footprint: the light-space rectangle of the
// grown box's corners in map texels, widened the other way by how far the ray
// goes between its lowest and highest height shares, each 0 or more,
// cut to the catcher's, and laid on the catcher's plane: light x, y is affine
// along it, so the catcher's corners place it.
@vertex fn footprint(@builtin(vertex_index) v: u32, @location(0) lo: vec3<f32>,
    @location(1) hi: vec3<f32>) -> Reach {
  let k = v % 6u;
  let r = v / 6u;
  let m = mat3x3<f32>(light.clip[0].xyz, light.clip[1].xyz, light.clip[2].xyz);
  let inv = transpose(mat3x3<f32>(cross(m[1], m[2]), cross(m[2], m[0]),
    cross(m[0], m[1]))) * (1.0 / dot(m[0], cross(m[1], m[2])));
  let texel = 2.0 / f32(SIDE);
  let wx = inv * vec3<f32>(texel, 0.0, light.ground.x * texel);
  let wy = inv * vec3<f32>(0.0, -texel, -light.ground.y * texel);
  let wh = inv * vec3<f32>(0.0, 0.0, -light.reach.z);
  var top = -3e38;
  for (var c = 0u; c < 8u; c++) {
    let high = vec3<bool>((c & 1u) != 0u, (c & 2u) != 0u, (c & 4u) != 0u);
    top = max(top, share(light.clip * vec4<f32>(select(lo, hi, high), 1.0)));
  }
  let s = slice(top);
  let grow = (1.0 + NUDGE) * (abs(wx) + abs(wy)) +
    max(bottom(s + 1u) - bottom(s), bottom(1u)) * abs(wh);
  let a = lo - grow;
  let b = hi + grow;
  var r0 = vec2<f32>(3e38);
  var r1 = vec2<f32>(-3e38);
  top = -3e38;
  var low = 3e38;
  for (var c = 0u; c < 8u; c++) {
    let high = vec3<bool>((c & 1u) != 0u, (c & 2u) != 0u, (c & 4u) != 0u);
    let q = light.clip * vec4<f32>(select(a, b, high), 1.0);
    r0 = min(r0, mapped(q.xy) * f32(SIDE));
    r1 = max(r1, mapped(q.xy) * f32(SIDE));
    top = max(top, share(q));
    low = min(low, share(q));
  }
  let most = max(ray(r) * max(top, 0.0), ray(r) * max(low, 0.0));
  let least = min(ray(r) * max(top, 0.0), ray(r) * max(low, 0.0));
  let e0 = mapped((light.clip * light.corners[0]).xy) * f32(SIDE);
  let e2 = mapped((light.clip * light.corners[2]).xy) * f32(SIDE);
  let f0 = max(r0 - most, min(e0, e2));
  let f1 = max(min(r1 - least, max(e0, e2)), f0);
  var order = array<vec2<f32>, 6>(vec2<f32>(0.0, 0.0), vec2<f32>(1.0, 0.0),
    vec2<f32>(1.0, 1.0), vec2<f32>(0.0, 0.0), vec2<f32>(1.0, 1.0), vec2<f32>(0.0, 1.0));
  let u = (mix(f0, f1, order[k]) - e0) / (e2 - e0);
  let p = light.corners[0] + u.x * (light.corners[1] - light.corners[0]) +
    u.y * (light.corners[3] - light.corners[0]);
  let c = clip * p;
  return Reach(vec4<f32>(c.xy, 0.5 * c.w, c.w), p.xyz, a, b, wx, wy, wh, r);
}

// Whether the ray from world point p, going d a unit of height share, crosses
// the box lo to hi at a height share of 0 or more, past T too, where traced
// finds it clear: an occlude box can reach above the casters' box.
fn crosses(p: vec3<f32>, d: vec3<f32>, lo: vec3<f32>, hi: vec3<f32>) -> bool {
  let safe = select(d, vec3<f32>(1e-30), abs(d) < vec3<f32>(1e-30));
  let a = (lo - p) / safe;
  let b = (hi - p) / safe;
  let t0 = max(max(max(min(a.x, b.x), min(a.y, b.y)), min(a.z, b.z)), 0.0);
  let t1 = min(min(max(a.x, b.x), max(a.y, b.y)), max(a.z, b.z));
  return t0 <= t1;
}

@fragment fn crossed(f: Reach) {
  if ((f.r & 3u) != turn(f.at.xy)) { discard; }
  let bits = textureLoad(clears, vec2<i32>(f.at.xy), 0).x;
  let d = ray(f.r);
  if (((bits >> (f.r / 4u)) & 1u) == 1u &&
      crosses(f.world, d.x * f.wx + d.y * f.wy + f.wh, f.lo, f.hi)) { return; }
  discard;
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
const CAST_ONLY = 2; // a row's flags bit 1: it casts and is not drawn
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
// numbers in binary64, or null when m is singular or the view comes out not finite. eye is
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
  const view = [...eye, ...right, 0, ...up, 0, ...back, 0];
  return view.every(Number.isFinite) ? view : null;
}

const cross3 = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
  a[0] * b[1] - a[1] * b[0]];
const scaled = (a, k) => a.map((v) => v * k);
const plus = (...vs) => [0, 1, 2].map((k) => vs.reduce((s, v) => s + v[k], 0));

// The light event's numbers (turn.js): its tangents are softbox's for
// keyward(), so the shadow's softbox is the one env shades with.
export const LIGHT = (() => {
  const c = unit(KEY);
  const across = unit(cross3([0, 1, 0], c));
  return Float32Array.of(...across, ...cross3(c, across), ...c, ...KEY_BOX,
    SHADOW_SIDE);
})();

// The key light's world axes for the studio's view: x and y the key
// softbox's tangents, z toward the key, LIGHT's laid on the studio's axes.
function keyAxes(axes) {
  const [right, up, back] = [4, 8, 12].map((k) => axes.slice(k, k + 3));
  const world = (k) => plus(scaled(right, LIGHT[k]), scaled(up, LIGHT[k + 1]),
    scaled(back, LIGHT[k + 2]));
  return { x: world(0), y: world(3), z: world(6) };
}

// The Light uniform, 40 f32s, for a ground record of 21 (turn.js) and the
// key's axes. The map's x and y cover M, so its texel is M's larger side over
// SHADOW_SIDE; its depth runs over the casters' key z, from the nearest, 0,
// to the farthest, 1, 1% to spare at each end, so clipping keeps a caster at
// either end. reach.z is T in units of depth. The catcher is C laid on the
// plane: past it the catcher would draw only the clear's white. An axis in
// which M has no extent takes a spare of a millionth of its largest extent or
// the casters' depth, so the fit keeps the scene's own scale whatever its
// units.
function lit(fit, a) {
  const n = Array.from(fit.subarray(0, 3)), d = fit[3];
  const [lo, hi] = [[fit[10], fit[12]], [fit[11], fit[13]]];
  const [x0, x1, y0, y1] = fit.subarray(14, 18);
  const [tall, near, far] = fit.subarray(18, 21);
  const nx = dot3(n, a.x), ny = dot3(n, a.y), nz = dot3(n, a.z);
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
  const corner = (x, y) => [...plus(scaled(a.x, x), scaled(a.y, y),
    scaled(a.z, -(d + x * nx + y * ny) / nz)), 1];
  const out = new Float32Array(40);
  for (let c = 0; c < 4; c++) for (let r = 0; r < 4; r++) out[4 * c + r] = rows[r][c];
  out.set([...corner(x0, y0), ...corner(x1, y0), ...corner(x1, y1),
    ...corner(x0, y1)], 16);
  out.set([ex * nx / (nz * span), ey * ny / (nz * span),
    (top + (d + cx * nx + cy * ny) / nz) / span, 0], 32);
  out.set([span * LIGHT[9] / ex, span * LIGHT[10] / ey, D / span, 0], 36);
  return out;
}

// WebGPU on the canvas: setup's renderer, whose release destroys the device
// and everything allocated from it. A canvas holding another context is
// rejected before a device exists; a later setup failure releases the device.
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

// The rows regrouped, the drawn rows (flags bit 1 clear) then the cast-only
// rows, each by handle, each handle in the order it first appears, each row
// followed by origin(handle)'s three u32s in PLACED bytes; and the [handle,
// first, count] runs over the drawn rows and over the cast-only rows.
function grouped(rows, count, origin) {
  const v = new DataView(rows.buffer, rows.byteOffset, rows.byteLength);
  const groups = [new Map(), new Map()];
  for (let i = 0; i < count; i++) {
    const handle = v.getUint32(i * ROW + 48, true);
    const group = groups[v.getUint32(i * ROW + 56, true) & CAST_ONLY ? 1 : 0];
    if (!group.has(handle)) group.set(handle, []);
    group.get(handle).push(i);
  }
  const ordered = new Uint8Array(count * PLACED);
  const out = new DataView(ordered.buffer);
  let at = 0;
  const [runs, casts] = groups.map((group) => {
    const list = [];
    for (const [handle, rowsOf] of group) {
      list.push([handle, at, rowsOf.length]);
      const o = origin(handle);
      for (const i of rowsOf) {
        ordered.set(rows.subarray(i * ROW, (i + 1) * ROW), PLACED * at);
        o.forEach((v, k) => out.setUint32(PLACED * at + ROW + 4 * k, v, true));
        at++;
      }
    }
    return list;
  });
  return [ordered, runs, casts];
}

// The renderer holds each mesh by handle as a vertex buffer of its 12-byte
// vertices and an index buffer of its u16 triangles, and its origin; draw
// writes the rows, each with its mesh's origin, to an instance buffer and
// issues one indexed, instanced draw per handle, and
// highlight writes the words to a storage buffer. Each buffer is padded to
// the 4 bytes WebGPU's sizes and writes need, and the instance and highlight
// buffers grow when a write needs more. draw samples 4 times per pixel: into
// a 4x colour texture resolved into the canvas's, then discarded, against a
// 4x depth texture, both of the drawing buffer's size, which draw makes
// again when that size has changed. The depth texture keeps the last draw's
// depth, the matrix buffer its matrix and the view buffer the last view
// viewer gave, the identity matrix's before any, with the right, up and back
// of the ground held then. While a ground is held, marks exist with area, the
// caster pass's attachment SHADOW_SIDE square, and with shade and clears,
// half the drawing buffer's size rounded up, which a draw that shades makes
// again when that size has changed. Each draw that shades writes light and
// shadeMatrix, so these stay the last such draw's until the next, and occlude
// after a resize answers the last draw. clears is made, and cleared outside
// the catcher, with every ray clear (ALL_CLEAR), as V is 1 there and marks are
// made empty. A draw while a ground of 21 is held first draws every row into
// marks along the key, ORs the marks into their levels, draws V into shade
// and its clear rays into clears, then draws the catcher first in its main
// pass, which draws only the rows that are not cast-only; so does a pick's
// probe. With no ground or no shadow a cast-only row draws nothing.
// occlude draws its boxes against the depth, and their footprints into shade's
// pixels against clears when the last draw drew a shadow and marks are held,
// in passes of at most QUERIES occlusion queries, one per box, and reads
// the resolved sample counts back through buffers of its own, released once
// read, so a box passes a test when any sample does. pick,
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
  const boxVertex = {
    module,
    entryPoint: "box",
    buffers: [{ arrayStride: 24, stepMode: "instance", attributes: [
      { shaderLocation: 0, offset: 0, format: "float32x3" },
      { shaderLocation: 1, offset: 12, format: "float32x3" }] }],
  };
  const occluder = device.createRenderPipeline({
    layout: "auto",
    vertex: boxVertex,
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth24plus", depthWriteEnabled: false, depthCompare: "less-equal" },
    multisample: { count: 4 },
  });
  // The key test and the caster pass write no pixel: a render pass needs an
  // attachment, which sets its size.
  const keyOccluder = device.createRenderPipeline({
    layout: "auto",
    vertex: { ...boxVertex, entryPoint: "footprint" },
    fragment: { module, entryPoint: "crossed", targets: [{ format: "r8unorm", writeMask: 0 }] },
    primitive: { topology: "triangle-list", cullMode: "none" },
  });
  const caster = device.createRenderPipeline({
    layout: "auto",
    vertex: { module, entryPoint: "caster", buffers: [layouts[1]] },
    fragment: { module, entryPoint: "stamp", targets: [{ format: "r8unorm", writeMask: 0 }] },
    primitive: { topology: "triangle-list", cullMode: "none" },
  });
  const halver = device.createComputePipeline({ layout: "auto",
    compute: { module, entryPoint: "halve" } });
  const shader = device.createRenderPipeline({
    layout: "auto",
    vertex: { module, entryPoint: "ground" },
    fragment: { module, entryPoint: "seen", targets: [{ format: "r8unorm" }, { format: "r32uint" }] },
    primitive: { topology: "triangle-list", cullMode: "none" },
  });
  const catcher = device.createRenderPipeline({
    layout: "auto",
    vertex: { module, entryPoint: "ground" },
    fragment: { module, entryPoint: "shadow", targets: [{ format }] },
    primitive: { topology: "triangle-list", cullMode: "none" },
    depthStencil: { format: "depth24plus", depthWriteEnabled: false, depthCompare: "always" },
    multisample: { count: 4 },
  });
  const filtered = device.createSampler({ magFilter: "linear", minFilter: "linear" });
  const { UNIFORM, STORAGE, VERTEX, INDEX, COPY_DST, COPY_SRC, QUERY_RESOLVE, MAP_READ } =
    GPUBufferUsage;
  const matrix = device.createBuffer({ size: 64, usage: UNIFORM | COPY_DST });
  // The last view viewer gave, kept for a draw whose matrix is singular; each
  // write of it or studio's view to the view buffer takes it to f32.
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
  const shadeMatrix = device.createBuffer({ size: 64, usage: UNIFORM | COPY_DST });
  let fit = null; // the ground record held, 10 or 21 f32, or null
  const bound = (user, entries) => device.createBindGroup({
    layout: user.getBindGroupLayout(0),
    entries: entries.map(([binding, resource]) => ({ binding, resource })),
  });
  // halve's uniform for each level it writes, 1 to the top.
  const halvings = Array.from({ length: Math.log2(SHADOW_SIDE) }, (_, k) => {
    const b = device.createBuffer({ size: 4, usage: UNIFORM | COPY_DST });
    device.queue.writeBuffer(b, 0, Uint32Array.of(k + 1));
    return b;
  });
  // The shadow's targets and the bind groups that name them, while a ground is
  // held.
  let shadows = null;
  const shaded = () => {
    const size = [Math.ceil(canvas.width / 2), Math.ceil(canvas.height / 2)];
    const usage = GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING;
    shadows.shade = device.createTexture({ format: "r8unorm", size, usage });
    shadows.clears = device.createTexture({ format: "r32uint", size, usage });
    const encoder = device.createCommandEncoder();
    encoder.beginRenderPass({ colorAttachments: [{ view: shadows.clears.createView(),
      clearValue: [ALL_CLEAR, 0, 0, 0], loadOp: "clear", storeOp: "store" }] }).end();
    device.queue.submit([encoder.finish()]);
    shadows.catcher = bound(catcher, [[0, { buffer: matrix }], [3, { buffer: light }],
      [8, shadows.shade.createView()], [9, filtered]]);
    shadows.key = bound(keyOccluder, [[0, { buffer: shadeMatrix }], [3, { buffer: light }],
      [10, shadows.clears.createView()]]);
  };
  const shadowMap = () => {
    const area = device.createTexture({ size: [SHADOW_SIDE, SHADOW_SIDE],
      format: "r8unorm", usage: GPUTextureUsage.RENDER_ATTACHMENT });
    // Two u32s for each texel of every level, SHADOW_SIDE² + … + 1 of them.
    const marks = device.createBuffer({ size: 8 * (4 * SHADOW_SIDE ** 2 - 1) / 3,
      usage: STORAGE | COPY_DST });
    shadows = { area, marks,
      caster: bound(caster, [[3, { buffer: light }], [4, { buffer: marks }]]),
      halves: halvings.map((b) => bound(halver, [[5, { buffer: marks }], [6, { buffer: b }]])),
      seen: bound(shader, [[0, { buffer: shadeMatrix }], [3, { buffer: light }],
        [7, { buffer: marks }]]) };
    shaded();
  };
  const unshadow = () => {
    for (const name of ["area", "marks", "shade", "clears"]) shadows[name].destroy();
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
  const depthTexture = () => device.createTexture({
    size: [canvas.width, canvas.height], format: "depth24plus", sampleCount: 4,
    usage: GPUTextureUsage.RENDER_ATTACHMENT,
  });
  const colorTexture = () => device.createTexture({
    size: [canvas.width, canvas.height], format, sampleCount: 4,
    usage: GPUTextureUsage.RENDER_ATTACHMENT,
  });
  let depth = depthTexture();
  let color = colorTexture();
  // A held handle -> { vertices, indices, count, origin }.
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
    const held = filled(vertices, VERTEX | STORAGE);
    let indices;
    try {
      indices = filled(triangles, INDEX | STORAGE);
    } catch (e) {
      held.destroy();
      throw e;
    }
    const cast = device.createBindGroup({ layout: caster.getBindGroupLayout(1), entries: [
      { binding: 0, resource: { buffer: held } }, { binding: 1, resource: { buffer: indices } }] });
    meshes.set(handle, { vertices: held, indices, count: 3 * triangleCount,
      origin: origin.slice(), cast });
  };
  const unmesh = (handle) => {
    const { vertices, indices } = meshes.get(handle);
    vertices.destroy();
    indices.destroy();
    meshes.delete(handle);
  };

  const ground = (held) => {
    fit = held && held.slice();
    if (fit && !shadows) shadowMap();
    if (!fit && shadows) unshadow();
  };

  // The last draw's runs of drawn rows and its matrix, copied, as a pick
  // reads them, and whether it drew a shadow; null before the first.
  let last = null;

  // rows, count instance rows of held meshes (turn.js); 0 rows clears.
  const draw = (rows, count, clip) => {
    const [ordered, runs, casts] = grouped(rows, count,
      (h) => meshes.get(h).origin);

    last = { runs, clip: new Float32Array(clip.slice().buffer) };
    if (count) {
      instances = grown(instances, ordered.byteLength, VERTEX | COPY_DST);
      device.queue.writeBuffer(instances, 0, ordered);
    }
    device.queue.writeBuffer(matrix, 0, clip);
    seen = viewer(last.clip) ?? seen;
    const axes = fit ? [...seen.slice(0, 4), ...fit.subarray(4, 7), 0,
      ...fit.subarray(0, 3), 0, ...fit.subarray(7, 10), 0] : seen;
    device.queue.writeBuffer(view, 0, Float32Array.from(axes));
    const cast = fit !== null && fit.length === 21;
    last.cast = cast;
    const encoder = device.createCommandEncoder();
    if (cast) {
      const [w, h] = [Math.ceil(canvas.width / 2), Math.ceil(canvas.height / 2)];
      if (shadows.shade.width !== w || shadows.shade.height !== h) {
        shadows.shade.destroy();
        shadows.clears.destroy();
        shaded();
      }
      const { area, marks, shade, clears } = shadows;
      device.queue.writeBuffer(light, 0, lit(fit, keyAxes(axes)));
      // shade's texel is two pixels square, so it overruns an odd canvas.
      device.queue.writeBuffer(shadeMatrix, 0,
        windowed(0, 0, 2 * shade.width, 2 * shade.height));
      // halve writes every texel above level 0.
      encoder.clearBuffer(marks, 0, 8 * SHADOW_SIDE ** 2);
      const casting = encoder.beginRenderPass({ colorAttachments: [{
        view: area.createView(), loadOp: "clear", storeOp: "discard" }] });
      casting.setPipeline(caster);
      casting.setBindGroup(0, shadows.caster);
      if (count) casting.setVertexBuffer(0, instances);
      for (const [handle, first, n] of [...runs, ...casts]) {
        const { cast, count: indexCount } = meshes.get(handle);
        casting.setBindGroup(1, cast);
        casting.draw(indexCount, n, 0, first);
      }
      casting.end();
      const halving = encoder.beginComputePass();
      const run = (bound, side) => {
        halving.setBindGroup(0, bound);
        halving.dispatchWorkgroups(Math.ceil(side / 8), Math.ceil(side / 8));
      };
      halving.setPipeline(halver);
      shadows.halves.forEach((bound, k) => run(bound, SHADOW_SIDE >> (k + 1)));
      halving.end();
      const shading = encoder.beginRenderPass({ colorAttachments: [{
        view: shade.createView(), clearValue: [1, 1, 1, 1], loadOp: "clear", storeOp: "store" }, {
        view: clears.createView(), clearValue: [ALL_CLEAR, 0, 0, 0], loadOp: "clear",
        storeOp: "store" }] });
      shading.setPipeline(shader);
      shading.setBindGroup(0, shadows.seen);
      shading.draw(6);
      shading.end();
    }
    if (depth.width !== canvas.width || depth.height !== canvas.height) {
      depth.destroy();
      color.destroy();
      depth = depthTexture();
      color = colorTexture();
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

  // boxes, six f32 each, lowest corner then highest -> a byte per box: 1 when
  // any of its samples passed the last draw's depth, plus 2 when a ray of V
  // crosses it and no marked slice: a box without 2 can go and leave V as is.
  const occlude = async (boxes) => {
    const count = boxes.length / 6;
    if (count === 0) return new Uint8Array(0);
    const held = device.createBuffer({ size: boxes.byteLength, usage: VERTEX, mappedAtCreation: true });
    new Float32Array(held.getMappedRange()).set(boxes);
    held.unmap();
    // A test's bit is its place here: its pipeline, bind group, vertex count
    // and attachments.
    const tests = [[occluder, boxBind, 36, () => ({ colorAttachments: [],
      depthStencilAttachment: { view: depth.createView(), depthReadOnly: true } })]];
    if (last.cast && shadows) tests.push([keyOccluder, shadows.key, 6 * 64, () => ({
      colorAttachments: [{ view: shadows.shade.createView(), loadOp: "load", storeOp: "store" }] })]);
    // A test's counts, the u64s rounded up to the 256 bytes a resolve aligns to.
    const span = 256 * Math.ceil(count / 32);
    const size = tests.length * span;
    const resolved = device.createBuffer({ size, usage: QUERY_RESOLVE | COPY_SRC });
    const read = device.createBuffer({ size, usage: MAP_READ | COPY_DST });
    const sets = [];
    try {
      const encoder = device.createCommandEncoder();
      for (let first = 0; first < count; first += QUERIES) {
        const n = Math.min(QUERIES, count - first);
        tests.forEach(([tester, bound, vertices, attachments], t) => {
          const set = device.createQuerySet({ type: "occlusion", count: n });
          sets.push(set);
          const pass = encoder.beginRenderPass({ ...attachments(), occlusionQuerySet: set });
          pass.setPipeline(tester);
          pass.setBindGroup(0, bound);
          pass.setVertexBuffer(0, held);
          for (let i = 0; i < n; i++) {
            pass.beginOcclusionQuery(i);
            pass.draw(vertices, 1, 0, first + i);
            pass.endOcclusionQuery();
          }
          pass.end();
          // first is a multiple of QUERIES, so the offset is 256-aligned.
          encoder.resolveQuerySet(set, 0, n, resolved, t * span + 8 * first);
        });
      }
      encoder.copyBufferToBuffer(resolved, 0, read, 0, size);
      device.queue.submit([encoder.finish()]);
      await read.mapAsync(GPUMapMode.READ);
      const passed = new BigUint64Array(read.getMappedRange());
      return Uint8Array.from({ length: count }, (_, i) => tests.reduce((bits, _, t) =>
        bits | (passed[t * span / 8 + i] > 0n ? 1 << t : 0), 0));
    } finally {
      for (const resource of [held, resolved, read, ...sets]) resource.destroy();
    }
  };

  // The last draw's matrix composed with the map of the w by h pixels of the
  // canvas at px, py, which may overrun it, onto a whole target.
  const windowed = (px, py, w, h) => {
    const out = new Float32Array(16);
    const [W, H] = [canvas.width, canvas.height];
    for (let k = 0; k < 16; k += 4) {
      const [x, y, z, d] = last.clip.subarray(k, k + 4);
      out.set([(W * x + (W - 2 * px - w) * d) / w,
        (H * y + (h - H + 2 * py) * d) / h, z, d], k);
    }
    return out;
  };

  // [instance, face, depthBits] at pixel x, y (BigInts) of the last draw.
  const pick = async (x, y) => {
    const [width, height] = [canvas.width, canvas.height];
    if (!last?.runs.length || x < 0n || y < 0n || x >= BigInt(width) ||
        y >= BigInt(height))
      return NONE;
    device.queue.writeBuffer(probeMatrix, 0, windowed(Number(x), Number(y), 1, 1));
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

  return { mesh, unmesh, highlight, ground, draw, pick, occlude, release };
}

function modifiers(e) {
  return (e.shiftKey ? 1 : 0) | (e.ctrlKey ? 2 : 0) |
    (e.altKey ? 4 : 0) | (e.metaKey ? 8 : 0);
}

// devicePixelRatio in thousandths, at least 1: a resize turn's a.
function ratio() {
  return Math.max(1, Math.round(devicePixelRatio * 1000));
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
  // By index, so a hole reads as undefined and is rejected.
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
      ground: live((held) => renderer.ground(held)),
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
      ratio,
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
  // The start and light turns report their first failure once: a module
  // that ends at hello rejects both.
  send(() => Promise.all([module.start(), module.light(LIGHT)]));
  // The a the module was last told; 0 before the mount's first resize, so that
  // one always sends.
  let told = 0;
  const resize = () => {
    const [width, height] = drawingSize(canvas);
    const sized = canvas.width === width && canvas.height === height;
    if (sized && ratio() === told) return;
    if (!sized) {
      canvas.width = width;
      canvas.height = height;
    }
    told = ratio();
    send(() => module.resize());
  };
  observer = new ResizeObserver(resize);
  observer.observe(canvas);
  window.addEventListener("resize", resize, { signal });
  // A move to another screen changes devicePixelRatio and no CSS size, so
  // neither listener above fires; the query matches only the current ratio, so
  // it is re-armed at each change.
  const watch = () => matchMedia(`(resolution: ${devicePixelRatio}dppx)`)
    .addEventListener("change", () => { watch(); resize(); }, { once: true, signal });
  watch();
  resize();

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
        case "key": return send(() => module.key(event.key, 0));
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
