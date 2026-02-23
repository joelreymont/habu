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

fn shouldRootJitLiteral(v: runtime.Value) bool {
    return v.isPointer() and !v.isMagicSymbol();
}

fn internGlobalRefSymbol(heap: *Heap, qname: []const u8) !?runtime.Value {
    if (std.mem.indexOf(u8, qname, "::")) |sep| {
        return try internPackageSymbol(heap, qname[0..sep], qname[sep + 2 ..]);
    }
    if (std.mem.indexOfScalar(u8, qname, ':')) |sep| {
        return try internPackageSymbol(heap, qname[0..sep], qname[sep + 1 ..]);
    }
    return try heap.intern(qname);
}

fn internPackageSymbol(heap: *Heap, pkg_name: []const u8, sym_name: []const u8) !?runtime.Value {
    if (sym_name.len == 0) return null;
    if (try heap.internInPackage(pkg_name, sym_name)) |sym| return sym;

    if (pkg_name.len <= 128 and sym_name.len <= 256) {
        var pkg_buf: [128]u8 = undefined;
        var sym_buf: [256]u8 = undefined;
        for (pkg_name, 0..) |ch, i| pkg_buf[i] = std.ascii.toUpper(ch);
        for (sym_name, 0..) |ch, i| sym_buf[i] = std.ascii.toUpper(ch);
        return try heap.internInPackage(pkg_buf[0..pkg_name.len], sym_buf[0..sym_name.len]);
    }
    return null;
}

fn collectJitLiteralRoots(
    vm: *Vm,
    heap: *Heap,
    ir_node: *const Ir,
    roots: *jit_backend.LiteralRoots,
) !void {
    switch (ir_node.*) {
        .lit => |v| {
            if (shouldRootJitLiteral(v)) {
                const key = @intFromPtr(ir_node);
                if (!roots.contains(key)) {
                    const slot = try vm.registerJitLiteral(v);
                    try roots.put(key, slot);
                }
            }
        },
        .global_ref => |gr| {
            const key = @intFromPtr(ir_node);
            if (!roots.contains(key)) {
                const sym = (try internGlobalRefSymbol(heap, gr.name)) orelse return error.UnsupportedIrNode;
                const slot = try vm.registerJitLiteral(sym);
                try roots.put(key, slot);
            }
        },
        .lambda => |lam| {
            if (lam.captures.len != 0) return error.UnsupportedIrNode;
            if (lam.optional_params.len != 0) return error.UnsupportedIrNode;
            if (lam.key_params.len != 0) return error.UnsupportedIrNode;
            if (lam.rest_param != null) return error.UnsupportedIrNode;
            try collectJitLiteralRoots(vm, heap, lam.body, roots);
        },
        .fixnum_add,
        .fixnum_sub,
        .add,
        .sub,
        .fixnum_le,
        .fixnum_lt,
        .fixnum_gt,
        .fixnum_ge,
        .fixnum_eq,
        .le,
        .lt,
        .gt,
        .ge,
        .num_eq,
        .fixnum_mul,
        .mul,
        .eq,
        .cons,
        .logand,
        .mod,
        .rem,
        .append,
        .assoc,
        .make_string,
        .position,
        .position_eq,
        .position_equal,
        => |op| {
            try collectJitLiteralRoots(vm, heap, op.left, roots);
            try collectJitLiteralRoots(vm, heap, op.right, roots);
        },
        .@"if" => |f| {
            try collectJitLiteralRoots(vm, heap, f.cond, roots);
            try collectJitLiteralRoots(vm, heap, f.then_branch, roots);
            try collectJitLiteralRoots(vm, heap, f.else_branch, roots);
        },
        .block => |b| try collectJitLiteralRoots(vm, heap, b.body, roots),
        .progn => |exprs| {
            for (exprs) |expr| {
                try collectJitLiteralRoots(vm, heap, expr, roots);
            }
        },
        .let => |l| {
            for (l.bindings) |binding| {
                try collectJitLiteralRoots(vm, heap, binding.value, roots);
            }
            try collectJitLiteralRoots(vm, heap, l.body, roots);
        },
        .set => |s| try collectJitLiteralRoots(vm, heap, s.value, roots),
        .loop => |l| {
            try collectJitLiteralRoots(vm, heap, l.cond, roots);
            try collectJitLiteralRoots(vm, heap, l.body, roots);
        },
        .progv => |p| {
            try collectJitLiteralRoots(vm, heap, p.symbols, roots);
            try collectJitLiteralRoots(vm, heap, p.values, roots);
            try collectJitLiteralRoots(vm, heap, p.body, roots);
        },
        .assert_fixnum => |op| try collectJitLiteralRoots(vm, heap, op.operand, roots),
        .call => |c| {
            try collectJitLiteralRoots(vm, heap, c.func, roots);
            for (c.args) |arg| {
                try collectJitLiteralRoots(vm, heap, arg, roots);
            }
        },
        .tailcall => |tc| {
            try collectJitLiteralRoots(vm, heap, tc.func, roots);
            for (tc.args) |arg| {
                try collectJitLiteralRoots(vm, heap, arg, roots);
            }
        },
        .nilp,
        .not,
        .consp,
        .abs,
        .zerop,
        .oddp,
        .evenp,
        .car,
        .cdr,
        .unsafe_car,
        .unsafe_cdr,
        .length,
        .sqrt,
        .round,
        .intern,
        .vec_len,
        .str_len,
        => |op| try collectJitLiteralRoots(vm, heap, op.operand, roots),
        .hash_count => |h| try collectJitLiteralRoots(vm, heap, h.operand, roots),
        .hash_capacity => |h| try collectJitLiteralRoots(vm, heap, h.operand, roots),
        .hash_clear => |h| try collectJitLiteralRoots(vm, heap, h.operand, roots),
        .hash_test => |h| try collectJitLiteralRoots(vm, heap, h.operand, roots),
        .hash_keys => |h| try collectJitLiteralRoots(vm, heap, h.operand, roots),
        .hash_alist => |h| try collectJitLiteralRoots(vm, heap, h.operand, roots),
        .vec_new => |v| {
            try collectJitLiteralRoots(vm, heap, v.size, roots);
            if (v.init) |init_val| try collectJitLiteralRoots(vm, heap, init_val, roots);
        },
        .vec_ref, .str_ref, .str_concat => |op| {
            try collectJitLiteralRoots(vm, heap, op.left, roots);
            try collectJitLiteralRoots(vm, heap, op.right, roots);
        },
        .vec_set => |v| {
            try collectJitLiteralRoots(vm, heap, v.vec, roots);
            try collectJitLiteralRoots(vm, heap, v.index, roots);
            try collectJitLiteralRoots(vm, heap, v.value, roots);
        },
        .hash_get => |h| {
            try collectJitLiteralRoots(vm, heap, h.table, roots);
            try collectJitLiteralRoots(vm, heap, h.key, roots);
            if (h.default) |d| try collectJitLiteralRoots(vm, heap, d, roots);
        },
        .hash_set => |h| {
            try collectJitLiteralRoots(vm, heap, h.table, roots);
            try collectJitLiteralRoots(vm, heap, h.key, roots);
            try collectJitLiteralRoots(vm, heap, h.value, roots);
        },
        .hash_rem => |h| {
            try collectJitLiteralRoots(vm, heap, h.table, roots);
            try collectJitLiteralRoots(vm, heap, h.key, roots);
        },
        .format => |f| {
            try collectJitLiteralRoots(vm, heap, f.dest, roots);
            try collectJitLiteralRoots(vm, heap, f.control, roots);
            for (f.args) |arg| {
                try collectJitLiteralRoots(vm, heap, arg, roots);
            }
        },
        .str_set => |s| {
            try collectJitLiteralRoots(vm, heap, s.str, roots);
            try collectJitLiteralRoots(vm, heap, s.index, roots);
            try collectJitLiteralRoots(vm, heap, s.value, roots);
        },
        .substring => |s| {
            try collectJitLiteralRoots(vm, heap, s.str, roots);
            try collectJitLiteralRoots(vm, heap, s.start, roots);
            try collectJitLiteralRoots(vm, heap, s.end, roots);
        },
        .arr_new => |a| {
            for (a.dimensions) |dim| {
                try collectJitLiteralRoots(vm, heap, dim, roots);
            }
            if (a.init) |init_ir| {
                try collectJitLiteralRoots(vm, heap, init_ir, roots);
            }
        },
        .arr_new_dyn => |a| {
            try collectJitLiteralRoots(vm, heap, a.dimensions, roots);
            if (a.init) |init_ir| {
                try collectJitLiteralRoots(vm, heap, init_ir, roots);
            }
        },
        .arr_ref => |a| {
            try collectJitLiteralRoots(vm, heap, a.array, roots);
            for (a.subscripts) |sub| {
                try collectJitLiteralRoots(vm, heap, sub, roots);
            }
        },
        .arr_set => |a| {
            try collectJitLiteralRoots(vm, heap, a.array, roots);
            for (a.subscripts) |sub| {
                try collectJitLiteralRoots(vm, heap, sub, roots);
            }
            try collectJitLiteralRoots(vm, heap, a.value, roots);
        },
        else => {},
    }
}

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
    try tryHoistCompile(allocator, heap, specialized, child_chunks, chunk_base, vm);

    return chunk_ptr;
}

fn tryHoistCompile(
    allocator: std.mem.Allocator,
    heap: *Heap,
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

        var literal_roots = jit_backend.LiteralRoots.init(allocator);
        defer literal_roots.deinit();
        if (candidate.lambda_ir.* == .lambda) {
            collectJitLiteralRoots(vm, heap, candidate.lambda_ir.lambda.body, &literal_roots) catch |err| {
                vm.jit_adm.fail_other += 1;
                if (trace) std.debug.print("JIT bench: literal roots fail '{s}' {s}\n", .{ compile_name, @errorName(err) });
                continue;
            };
        }
        const literal_roots_ptr: ?*const jit_backend.LiteralRoots = if (literal_roots.count() > 0)
            &literal_roots
        else
            null;

        const compiled = jit_backend.compileIrWithKnownFnsAndLiteralRoots(
            allocator,
            candidate.lambda_ir,
            compile_name,
            null,
            literal_roots_ptr,
        ) catch |err| {
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
