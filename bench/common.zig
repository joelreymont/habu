const std = @import("std");

const habu = @import("habu");
const runtime = habu.runtime;
const interp = habu.interp;
const compiler_mod = habu.compiler;
const bytecode = habu.bytecode;

const Heap = runtime.Heap;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;
const Env = compiler_mod.Env;
const IrBuilder = compiler_mod.IrBuilder;
const Parser = habu.reader.Parser;
const Op = bytecode.Op;
const Emitter = bytecode.Emitter;

pub fn opsPerSec(ops: u64, ns: u64) f64 {
    if (ns == 0) return 0;
    const s = @as(f64, @floatFromInt(ns)) / 1e9;
    return @as(f64, @floatFromInt(ops)) / s;
}

fn patchChunkIndices(chunk: *runtime.objects.Chunk, base: u16) void {
    const code = chunk.getCode();
    var i: usize = 0;
    while (i + 1 < code.len) {
        const low: u16 = code[i];
        const high: u16 = code[i + 1];
        const opcode = low | (high << 8);
        const op: Op = @enumFromInt(opcode);
        const size = op.operandSize();

        if (op == .make_closure) {
            const rel_idx = std.mem.readInt(u16, code[i + 2 ..][0..2], .little);
            const abs_idx = rel_idx + base;
            std.mem.writeInt(u16, code[i + 2 ..][0..2], abs_idx, .little);
        }

        i += 2 + size;
    }
}

pub fn compileChunk(
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: *Vm,
    comp: *Compiler,
    chunk_pool: *std.ArrayList(*runtime.objects.Chunk),
    source: []const u8,
) !*runtime.objects.Chunk {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var parser = try Parser.init(a, heap, source, &vm.builtins);
    const expr = try parser.parse();

    const saved_builder = comp.builder;
    const saved_allocator = comp.allocator;
    comp.builder = IrBuilder.init(a);
    comp.allocator = a;
    defer {
        comp.builder = saved_builder;
        comp.allocator = saved_allocator;
    }

    var env = Env.init(a, null);
    defer env.deinit();

    const ir_node = if (comp.compile(expr, &env)) |node| node else |err| {
        return err;
    };

    var emitter = Emitter.initWithHeap(allocator, heap);
    defer emitter.deinit();
    try emitter.emit(ir_node);
    const chunk = try emitter.finalize();
    const child_chunks = try emitter.getChildChunks();
    defer allocator.free(child_chunks);

    const chunk_base: u16 = @intCast(chunk_pool.items.len);
    for (child_chunks) |c| {
        patchChunkIndices(c.toPtr(runtime.objects.Chunk), chunk_base);
    }

    try chunk_pool.ensureUnusedCapacity(allocator, child_chunks.len);
    for (child_chunks) |c| {
        chunk_pool.appendAssumeCapacity(c.toPtr(runtime.objects.Chunk));
    }

    const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
    patchChunkIndices(chunk_ptr, chunk_base);
    vm.setChunkPool(chunk_pool.items);
    return chunk_ptr;
}

