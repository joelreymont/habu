const std = @import("std");

const habu = @import("habu");
const runtime = habu.runtime;
const interp = habu.interp;
const compiler_mod = habu.compiler;
const compile_chunk = habu.testutil;

const Heap = runtime.Heap;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;
const Chunk = runtime.Chunk;
const Value = runtime.Value;

pub fn opsPerSec(ops: u64, ns: u64) f64 {
    if (ns == 0) return 0;
    const s = @as(f64, @floatFromInt(ns)) / 1e9;
    return @as(f64, @floatFromInt(ops)) / s;
}

pub fn compileChunk(
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: *Vm,
    comp: *Compiler,
    chunk_pool: *std.ArrayList(Value),
    source: []const u8,
) !*Chunk {
    return try compile_chunk.compileChunk(allocator, heap, vm, comp, chunk_pool, source);
}
