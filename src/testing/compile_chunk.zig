const std = @import("std");

const runtime = @import("../runtime/runtime.zig");
const reader = @import("../reader/reader.zig");
const compiler_mod = @import("../compiler/compiler.zig");
const bytecode = @import("../bytecode/bytecode.zig");
const interp = @import("../interp/interp.zig");
const specialize = @import("../compiler/passes/p07c_specialize.zig");
const jit_backend = @import("../jit/backend_api.zig");
const Ir = compiler_mod.ir.Ir;

const Heap = runtime.Heap;
const Chunk = runtime.Chunk;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;
const Env = compiler_mod.Env;
const IrBuilder = compiler_mod.IrBuilder;
const Parser = reader.Parser;
const Op = bytecode.Op;
const Emitter = bytecode.Emitter;

fn patchChunkIndices(chunk: *Chunk, base: u16) void {
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
    chunk_pool: *std.ArrayList(runtime.Value),
    source: []const u8,
) !*Chunk {
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
    const specialized = try specialize.specialize(a, ir_node);

    var emitter = Emitter.initWithHeap(allocator, heap);
    emitter.speed = comp.optimize_current.speed;
    emitter.safety = comp.optimize_current.safety;
    defer emitter.deinit();
    try emitter.emit(specialized);
    const chunk = try emitter.finalize();
    const child_chunks = try emitter.getChildChunks();
    defer allocator.free(child_chunks);

    const chunk_base: u16 = @intCast(chunk_pool.items.len);
    for (child_chunks) |c| {
        patchChunkIndices(c.toPtr(Chunk), chunk_base);
    }

    try chunk_pool.ensureUnusedCapacity(allocator, child_chunks.len);
    for (child_chunks) |c| {
        chunk_pool.appendAssumeCapacity(c);
    }

    const chunk_ptr = chunk.toPtr(Chunk);
    patchChunkIndices(chunk_ptr, chunk_base);
    vm.setChunkPool(chunk_pool.items);

    // Try hoist SSA JIT compilation for eligible lambdas
    tryHoistCompile(allocator, specialized, child_chunks, chunk_base, vm);

    return chunk_ptr;
}

fn tryHoistCompile(
    allocator: std.mem.Allocator,
    ir_node: *const Ir,
    child_chunks: []const runtime.value.Value,
    chunk_base: u16,
    vm: *Vm,
) void {
    const Candidate = struct {
        name: []const u8,
        lambda_ir: *const Ir,
    };

    const extractCandidate = struct {
        fn run(node: *const Ir) ?Candidate {
            return switch (node.*) {
                .define => |d| switch (d.value.*) {
                    .lambda => .{ .name = d.name, .lambda_ir = d.value },
                    else => null,
                },
                .set_symbol_function => |op| blk: {
                    if (op.left.* != .lit) break :blk null;
                    if (!op.left.lit.isSymbol()) break :blk null;
                    if (op.right.* != .lambda) break :blk null;
                    break :blk .{
                        .name = op.left.lit.toPtr(runtime.Symbol).getName(),
                        .lambda_ir = op.right,
                    };
                },
                .progn => |exprs| blk: {
                    for (exprs) |expr| {
                        if (run(expr)) |candidate| break :blk candidate;
                    }
                    break :blk null;
                },
                else => null,
            };
        }
    }.run;

    _ = chunk_base;
    const trace = std.posix.getenv("HABU_TRACE_JIT") != null;
    const candidate = extractCandidate(ir_node) orelse {
        if (trace) std.debug.print("JIT bench: no JIT candidate in top-level IR ({s})\n", .{@tagName(ir_node.*)});
        return;
    };

    const name = candidate.name;
    const lambda_ir = candidate.lambda_ir;
    const lambda = lambda_ir.lambda;

    if (trace) {
        std.debug.print(
            "JIT bench: consider '{s}' speed={d} safety={d} caps={d} opt={d} key={d} rest={} chunks={d}\n",
            .{
                name,
                lambda.speed,
                lambda.safety,
                lambda.captures.len,
                lambda.optional_params.len,
                lambda.key_params.len,
                lambda.rest_param != null,
                child_chunks.len,
            },
        );
    }

    if (lambda.speed < 3 or lambda.safety > 0) {
        if (trace) std.debug.print("JIT bench: skip '{s}' optimize gates\n", .{name});
        return;
    }
    if (lambda.captures.len > 0) {
        if (trace) std.debug.print("JIT bench: skip '{s}' captures\n", .{name});
        return;
    }
    if (lambda.optional_params.len > 0) {
        if (trace) std.debug.print("JIT bench: skip '{s}' optional params\n", .{name});
        return;
    }
    if (lambda.key_params.len > 0) {
        if (trace) std.debug.print("JIT bench: skip '{s}' key params\n", .{name});
        return;
    }
    if (lambda.rest_param != null) {
        if (trace) std.debug.print("JIT bench: skip '{s}' rest param\n", .{name});
        return;
    }
    if (child_chunks.len == 0) {
        if (trace) std.debug.print("JIT bench: skip '{s}' no child chunk\n", .{name});
        return;
    }

    const chunk_val = child_chunks[0];
    const chunk_ptr = chunk_val.toPtr(Chunk);

    var compiled = jit_backend.compileIr(allocator, lambda_ir, name) catch |err| {
        if (trace) {
            if (err == error.UnsupportedIrNode) {
                const bad = jit_backend.IrTranslator.firstUnsupportedTag(lambda.body) orelse std.meta.activeTag(lambda.body.*);
                std.debug.print("JIT bench: compile fail '{s}' {s} body={s} unsupported={s}\n", .{
                    name,
                    @errorName(err),
                    @tagName(lambda.body.*),
                    @tagName(bad),
                });
            } else {
                std.debug.print("JIT bench: compile fail '{s}' {s}\n", .{ name, @errorName(err) });
            }
        }
        return;
    };
    const persistent = allocator.create(jit_backend.CompiledFn) catch {
        compiled.deinit();
        if (trace) std.debug.print("JIT bench: persistent alloc fail '{s}'\n", .{name});
        return;
    };
    persistent.* = compiled;
    vm.registerJitFn(chunk_ptr, persistent) catch {
        persistent.deinit();
        allocator.destroy(persistent);
        if (trace) std.debug.print("JIT bench: register fail '{s}'\n", .{name});
        return;
    };
    if (trace) std.debug.print("JIT bench: registered '{s}' map={d}\n", .{ name, vm.jit_fns.count() });
}
