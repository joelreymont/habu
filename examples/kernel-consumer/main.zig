// main.zig - launch a habu-exported kernel against its manifest ABI.
//
// Descriptive example (Zig 0.16) of the consumer side of the
// habu-kernel-manifest v1 contract (docs/ptx-sketch.md "Kernel ABI
// contract"). Every driver-API argument comes from a manifest field:
//
//   manifest field        -> driver-API use
//   ---------------------    ----------------------------------------------
//   ptx_sha256            -> integrity check of the embedded .ptx BEFORE load
//   manifest_content_hash -> integrity check of the manifest itself
//   (ptx bytes)           -> cuModuleLoadData(&module, ptx)
//   name                  -> cuModuleGetFunction(&fn, module, name)
//   target, ptx_version   -> preflight: device arch / driver PTX ISA support
//   param_slots (order!)  -> void* kernelParams[]: one pointer per slot, in
//                            array order; .u64 slots point at a CUdeviceptr,
//                            .u32/.f32 slots at a 4-byte scalar
//   param_bytes           -> only for the legacy cuParamSetSize path; unused
//                            with kernelParams
//   grid_derivation       -> gridDim.x: "ceil-n-<B>" => ceil(N/B) over the
//                            runtime extent N; "extent-<t>" => the runtime
//                            value of extent t (one block per row)
//   block                 -> blockDim {x,y,z}
//   params[].lowering     -> which runtime values feed which slots: span =
//                            base devptr + dedup'd len, matrix = base + cols
//                            (rows launch-derived, stride dense-derived),
//                            uniform = scalar
//
// The kernel here is habu's checked SAXPY: y = a*x + y over n f32 elements.

const std = @import("std");

const ptx = @embedFile("SAXPY.ptx");
const manifest_json = @embedFile("SAXPY.manifest.json");

// ---- habu-kernel-manifest v1 (typed view of the launch-relevant fields;
// `params` stays a dynamic value: its per-kind `lowering` objects are
// heterogeneous and this launcher only needs the flat slots) ----
const Slot = struct {
    param: []const u8,
    offset: u32,
    size: u32,
    ptx_type: []const u8,
    role: []const u8,
};

const Manifest = struct {
    schema: []const u8,
    version: u32,
    name: []const u8,
    target: []const u8,
    ptx_version: []const u8,
    address_size: u32,
    block: struct { x: u32, y: u32, z: u32 },
    grid_derivation: []const u8,
    param_bytes: u32,
    params: std.json.Value,
    param_slots: []Slot,
    ptx_sha256: []const u8,
    manifest_content_hash: []const u8,
};

// ---- CUDA Driver API (libcuda; only what this launch needs) ----
const CUresult = c_int;
const CUdevice = c_int;
const CUdeviceptr = u64;
const CUcontext = ?*anyopaque;
const CUmodule = ?*anyopaque;
const CUfunction = ?*anyopaque;

extern "c" fn cuInit(flags: c_uint) CUresult;
extern "c" fn cuDeviceGet(dev: *CUdevice, ordinal: c_int) CUresult;
extern "c" fn cuDevicePrimaryCtxRetain(ctx: *CUcontext, dev: CUdevice) CUresult;
extern "c" fn cuDevicePrimaryCtxRelease(dev: CUdevice) CUresult;
extern "c" fn cuCtxSetCurrent(ctx: CUcontext) CUresult;
extern "c" fn cuModuleLoadData(module: *CUmodule, image: *const anyopaque) CUresult;
extern "c" fn cuModuleUnload(module: CUmodule) CUresult;
extern "c" fn cuModuleGetFunction(f: *CUfunction, module: CUmodule, name: [*:0]const u8) CUresult;
extern "c" fn cuMemAlloc_v2(dptr: *CUdeviceptr, bytes: usize) CUresult;
extern "c" fn cuMemFree_v2(dptr: CUdeviceptr) CUresult;
extern "c" fn cuMemcpyHtoD_v2(dst: CUdeviceptr, src: *const anyopaque, bytes: usize) CUresult;
extern "c" fn cuMemcpyDtoH_v2(dst: *anyopaque, src: CUdeviceptr, bytes: usize) CUresult;
extern "c" fn cuCtxSynchronize() CUresult;
extern "c" fn cuLaunchKernel(
    f: CUfunction,
    grid_x: c_uint,
    grid_y: c_uint,
    grid_z: c_uint,
    block_x: c_uint,
    block_y: c_uint,
    block_z: c_uint,
    shared_bytes: c_uint,
    stream: ?*anyopaque,
    kernel_params: ?[*]?*anyopaque,
    extra: ?[*]?*anyopaque,
) CUresult;

fn check(rc: CUresult) !void {
    if (rc != 0) return error.CudaDriverFailure;
}

// ptx_sha256: SHA-256 hex of the .ptx artifact bytes - verify BEFORE load.
fn verifyPtxHash(expected_hex: []const u8) !void {
    var digest: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(ptx, &digest, .{});
    var hex_buf: [64]u8 = undefined;
    const hex = std.fmt.bufPrint(&hex_buf, "{x}", .{&digest}) catch unreachable;
    if (!std.mem.eql(u8, hex, expected_hex)) return error.PtxHashMismatch;
}

// manifest_content_hash: SHA-256 hex of the manifest bytes STRICTLY BEFORE
// the ,"manifest_content_hash" suffix (contract: verifiable by slicing).
fn verifyManifestHash(expected_hex: []const u8) !void {
    const key = ",\"manifest_content_hash\"";
    const cut = std.mem.lastIndexOf(u8, manifest_json, key) orelse return error.ManifestShape;
    var digest: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(manifest_json[0..cut], &digest, .{});
    var hex_buf: [64]u8 = undefined;
    const hex = std.fmt.bufPrint(&hex_buf, "{x}", .{&digest}) catch unreachable;
    if (!std.mem.eql(u8, hex, expected_hex)) return error.ManifestHashMismatch;
}

// grid_derivation -> gridDim.x. "ceil-n-<B>": grid-strided over runtime
// extent n. "extent-<t>": one block per row, gridDim.x == runtime extent.
fn gridDimX(derivation: []const u8, extent: u32) !u32 {
    if (std.mem.startsWith(u8, derivation, "ceil-n-")) {
        const b = try std.fmt.parseInt(u32, derivation["ceil-n-".len..], 10);
        return (extent + b - 1) / b;
    }
    if (std.mem.startsWith(u8, derivation, "extent-")) return extent;
    if (std.mem.eql(u8, derivation, "once")) return 1;
    return error.UnknownGridDerivation;
}

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    const io = init.io;

    var out_buf: [4096]u8 = undefined;
    var fw = std.Io.File.stdout().writerStreaming(io, &out_buf);
    const out = &fw.interface;

    // -- parse + verify the manifest before touching the driver --
    const parsed = try std.json.parseFromSlice(Manifest, gpa, manifest_json, .{});
    defer parsed.deinit();
    const m = parsed.value;

    if (!std.mem.eql(u8, m.schema, "habu-kernel-manifest")) return error.UnknownSchema;
    if (m.version != 1) return error.UnsupportedManifestVersion;
    try verifyPtxHash(m.ptx_sha256);
    try verifyManifestHash(m.manifest_content_hash);

    // target/ptx_version preflight would compare against the device's
    // compute capability and the driver's supported PTX ISA here.
    try out.print("kernel {s} for {s} (PTX ISA {s})\n", .{ m.name, m.target, m.ptx_version });

    // -- host data: y = a*x + y, n f32 elements --
    const n: u32 = 1024;
    const a: f32 = 3.0;
    const x_host = try gpa.alloc(f32, n);
    defer gpa.free(x_host);
    const y_host = try gpa.alloc(f32, n);
    defer gpa.free(y_host);
    for (x_host, 0..) |*v, i| v.* = @floatFromInt(i);
    @memset(y_host, 1.0);

    // -- driver setup: primary context (docs/ptx-sketch.md decision 7) --
    try check(cuInit(0));
    var dev: CUdevice = undefined;
    try check(cuDeviceGet(&dev, 0));
    var ctx: CUcontext = undefined;
    try check(cuDevicePrimaryCtxRetain(&ctx, dev));
    defer _ = cuDevicePrimaryCtxRelease(dev);
    try check(cuCtxSetCurrent(ctx));

    // (ptx bytes) -> cuModuleLoadData; name -> cuModuleGetFunction. The
    // embedded @embedFile slice is NUL-terminated ([:0]), as the driver
    // expects for a text image.
    var module: CUmodule = undefined;
    try check(cuModuleLoadData(&module, ptx.ptr));
    defer _ = cuModuleUnload(module);
    const name_z = try gpa.dupeZ(u8, m.name);
    defer gpa.free(name_z);
    var function: CUfunction = undefined;
    try check(cuModuleGetFunction(&function, module, name_z));

    // -- device buffers for the span bases --
    const bytes: usize = @as(usize, n) * @sizeOf(f32);
    var d_x: CUdeviceptr = undefined;
    var d_y: CUdeviceptr = undefined;
    try check(cuMemAlloc_v2(&d_x, bytes));
    defer _ = cuMemFree_v2(d_x);
    try check(cuMemAlloc_v2(&d_y, bytes));
    defer _ = cuMemFree_v2(d_y);
    try check(cuMemcpyHtoD_v2(d_x, x_host.ptr, bytes));
    try check(cuMemcpyHtoD_v2(d_y, y_host.ptr, bytes));

    // param_slots -> kernelParams: ONE pointer per slot, IN ARRAY ORDER.
    // SAXPY v1 slots: p_x (.u64 span base), p_y (.u64 span base), p_a (.f32
    // uniform scalar), p_n (.u32 dedup'd extent - both spans share it, the
    // checker proved their extents equal). Validate the order instead of
    // assuming it, then point each slot at the matching host value.
    const expected_slots = [_][]const u8{ "p_x", "p_y", "p_a", "p_n" };
    if (m.param_slots.len != expected_slots.len) return error.UnexpectedSlotCount;
    for (m.param_slots, expected_slots) |slot, want|
        if (!std.mem.eql(u8, slot.param, want)) return error.UnexpectedSlotOrder;

    var a_val: f32 = a;
    var n_val: u32 = n;
    var kernel_params = [_]?*anyopaque{ @ptrCast(&d_x), @ptrCast(&d_y), @ptrCast(&a_val), @ptrCast(&n_val) };

    // grid_derivation + runtime extent -> gridDim.x; block -> blockDim.
    const grid_x = try gridDimX(m.grid_derivation, n);
    try check(cuLaunchKernel(
        function,
        grid_x,
        1,
        1,
        m.block.x,
        m.block.y,
        m.block.z,
        0, // no dynamic shared memory in this kernel
        null, // default stream
        &kernel_params,
        null,
    ));
    try check(cuCtxSynchronize());

    // -- read back + spot-check y = a*x + y --
    try check(cuMemcpyDtoH_v2(y_host.ptr, d_y, bytes));
    const i = n - 1;
    const want = a * @as(f32, @floatFromInt(i)) + 1.0;
    if (y_host[i] != want) return error.WrongKernelResult;
    try out.print("SAXPY ok: y[{d}] = {d}\n", .{ i, y_host[i] });
    try out.flush();
}
