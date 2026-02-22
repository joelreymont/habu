const std = @import("std");

const runtime = @import("../runtime/runtime.zig");
const reader = @import("../reader/reader.zig");
const compiler_mod = @import("../compiler/compiler.zig");
const bytecode = @import("../bytecode/bytecode.zig");
const interp = @import("../interp/interp.zig");
const specialize = @import("../compiler/passes/p07c_specialize.zig");
const jit_backend = @import("../jit/backend_api.zig");
const jit_candidates = @import("../jit/candidates.zig");
const Ir = compiler_mod.ir.Ir;

const Heap = runtime.Heap;
const Chunk = runtime.Chunk;
const Symbol = runtime.Symbol;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;
const Env = compiler_mod.Env;
const IrBuilder = compiler_mod.IrBuilder;
const Parser = reader.Parser;
const Op = bytecode.Op;
const Emitter = bytecode.Emitter;

fn patchChunkIndices(chunk: *Chunk, base: u16) !void {
    const code = chunk.getCode();
    var i: usize = 0;
    while (i < code.len) {
        const insn = try bytecode.opcodes.decodeInstruction(code, i);
        if (insn.op == .make_closure) {
            const rel_idx = std.mem.readInt(u16, code[insn.operand_off..][0..2], .little);
            const abs_idx = try std.math.add(u16, rel_idx, base);
            std.mem.writeInt(u16, code[insn.operand_off..][0..2], abs_idx, .little);
        }
        i = insn.next_off;
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
    emitter.setRetainedValueLookup(Compiler.retainedValueLookup, comp);
    defer emitter.deinit();
    try emitter.emit(specialized);
    const chunk = try emitter.finalize();
    const child_chunks = try emitter.getChildChunks();
    defer allocator.free(child_chunks);

    const chunk_base: u16 = @intCast(chunk_pool.items.len);
    for (child_chunks) |c| {
        try patchChunkIndices(c.toPtr(Chunk), chunk_base);
    }

    try chunk_pool.ensureUnusedCapacity(allocator, child_chunks.len);
    for (child_chunks) |c| {
        chunk_pool.appendAssumeCapacity(c);
    }

    const chunk_ptr = chunk.toPtr(Chunk);
    try patchChunkIndices(chunk_ptr, chunk_base);
    vm.setChunkPoolOwned(chunk_pool);

    // Try hoist SSA JIT compilation for eligible lambdas
    try tryHoistCompile(allocator, specialized, child_chunks, chunk_base, vm);

    return chunk_ptr;
}

fn tryHoistCompile(
    allocator: std.mem.Allocator,
    ir_node: *const Ir,
    child_chunks: []const runtime.value.Value,
    chunk_base: u16,
    vm: *Vm,
) !void {
    const trace = std.posix.getenv("HABU_TRACE_JIT") != null;
    var candidates = std.ArrayList(jit_candidates.LambdaCandidate){};
    defer {
        jit_candidates.freeLambdaCandidates(allocator, candidates.items);
        candidates.deinit(allocator);
    }
    try jit_candidates.collectLambdaCandidates(allocator, ir_node, &candidates);
    if (candidates.items.len == 0) {
        if (trace) std.debug.print("JIT bench: no JIT candidates in top-level IR ({s})\n", .{@tagName(ir_node.*)});
        return;
    }

    const used_chunks = try allocator.alloc(bool, child_chunks.len);
    defer allocator.free(used_chunks);
    @memset(used_chunks, false);
    const live_chunks = try allocator.alloc(runtime.Value, child_chunks.len);
    defer allocator.free(live_chunks);
    const chunk_base_usize: usize = chunk_base;

    for (candidates.items) |candidate| {
        for (child_chunks, 0..) |child_chunk, idx| {
            const pool_idx = chunk_base_usize + idx;
            const pooled = if (pool_idx < vm.chunk_pool.len) vm.chunk_pool[pool_idx] else child_chunk;
            live_chunks[idx] = vm.resolveForwardedValue(pooled);
        }
        const live_name_sym = vm.resolveForwardedValue(candidate.name_sym);
        const compile_name = if (live_name_sym.isSymbol())
            live_name_sym.toPtr(Symbol).getName()
        else
            candidate.name;

        vm.jit_adm.cand += 1;
        if (jit_candidates.ineligibleReason(candidate.lambda_ir)) |reason| {
            switch (reason) {
                .not_lambda => vm.jit_adm.fail_other += 1,
                .speed => vm.jit_adm.sk_speed += 1,
                .safety => vm.jit_adm.sk_safety += 1,
                .assert_fixnum_body => vm.jit_adm.sk_assert += 1,
                .captures => vm.jit_adm.sk_caps += 1,
                .optional_params => vm.jit_adm.sk_opt += 1,
                .key_params => vm.jit_adm.sk_key += 1,
                .rest_param => vm.jit_adm.sk_rest += 1,
            }
            if (trace and candidate.lambda_ir.* == .lambda) {
                const lambda = candidate.lambda_ir.lambda;
                std.debug.print(
                    "JIT bench: skip '{s}' reason={s} speed={d} safety={d} caps={d} opt={d} key={d} rest={}\n",
                    .{
                        compile_name,
                        jit_candidates.reasonLabel(reason),
                        lambda.speed,
                        lambda.safety,
                        lambda.captures.len,
                        lambda.optional_params.len,
                        lambda.key_params.len,
                        lambda.rest_param != null,
                    },
                );
            }
            continue;
        }

        const chunk_ptr = jit_candidates.findMatchingChunk(&candidate, live_name_sym, live_chunks, used_chunks) orelse {
            vm.jit_adm.sk_chunk += 1;
            if (trace) std.debug.print("JIT bench: no chunk for '{s}' local={s}\n", .{ compile_name, candidate.local_name });
            continue;
        };
        vm.jit_adm.elig += 1;

        const lambda = candidate.lambda_ir.lambda;
        if (trace) {
            std.debug.print(
                "JIT bench: consider '{s}' speed={d} safety={d} caps={d} opt={d} key={d} rest={} chunks={d} chunk=0x{x}\n",
                .{
                    compile_name,
                    lambda.speed,
                    lambda.safety,
                    lambda.captures.len,
                    lambda.optional_params.len,
                    lambda.key_params.len,
                    lambda.rest_param != null,
                    child_chunks.len,
                    @intFromPtr(chunk_ptr),
                },
            );
        }

        const compiled = jit_backend.compileIr(allocator, candidate.lambda_ir, compile_name) catch |err| {
            if (err == error.UnsupportedIrNode) {
                vm.jit_adm.fail_unsupported += 1;
            } else {
                vm.jit_adm.fail_other += 1;
            }
            if (trace) {
                if (err == error.UnsupportedIrNode) {
                    const bad = jit_backend.IrTranslator.firstUnsupportedTag(lambda.body) orelse std.meta.activeTag(lambda.body.*);
                    std.debug.print("JIT bench: compile fail '{s}' {s} body={s} unsupported={s}\n", .{
                        compile_name,
                        @errorName(err),
                        @tagName(lambda.body.*),
                        @tagName(bad),
                    });
                } else {
                    std.debug.print("JIT bench: compile fail '{s}' {s}\n", .{ compile_name, @errorName(err) });
                }
            }
            continue;
        };
        const persistent = allocator.create(jit_backend.CompiledFn) catch {
            vm.jit_adm.fail_other += 1;
            return error.OutOfMemory;
        };
        persistent.* = compiled;
        vm.registerJitFn(chunk_ptr, persistent) catch {
            persistent.deinit();
            allocator.destroy(persistent);
            vm.jit_adm.fail_other += 1;
            return error.OutOfMemory;
        };
        vm.jit_adm.comp += 1;
        if (trace) std.debug.print("JIT bench: registered '{s}' map={d}\n", .{ compile_name, vm.jit_fns.count() });
    }
}
