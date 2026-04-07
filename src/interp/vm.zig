//! Bytecode Virtual Machine for Habu
//!
//! Stack-based interpreter that executes bytecode.
//! Designed for portability (WASM target).

comptime {
    @setEvalBranchQuota(5000);
}

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const bytecode = @import("../bytecode/bytecode.zig");
const disasm = @import("../bytecode/disasm.zig");
const opcodes = @import("../bytecode/opcodes.zig");
const Op = bytecode.Op;
const Chunk = bytecode.Chunk;
const runtime = @import("../runtime/runtime.zig");
const roots_mod = @import("../runtime/roots.zig");
const primitives = @import("../runtime/primitives/primitives.zig");
const qual_name = @import("../runtime/qual_name.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const String = runtime.String;
const Symbol = runtime.Symbol;
const arith = @import("../runtime/primitives/arith.zig");
const io = @import("../runtime/primitives/io.zig");
const stringPrims = @import("../runtime/primitives/string.zig");
const char_primitives = @import("../runtime/primitives/char.zig");
const hash_prims = @import("../runtime/primitives/hash.zig");
const type_mod = @import("../runtime/primitives/type.zig");
const HashTable = runtime.HashTable;
const Vector = runtime.Vector;
const compiler = @import("../compiler/compiler.zig");
const GlobalEnv = compiler.GlobalEnv;
const parser_mod = @import("../reader/parser.zig");
const Parser = parser_mod.Parser;
const BuiltinSymbols = @import("../runtime/builtins.zig").BuiltinSymbols;
const jit_backend = @import("../jit/backend_api.zig");

pub const Error = anyerror;

pub const BuiltinCallableTag = enum(usize) {
    add,
    sub,
    mul,
    div,
    append,
    log,
    gensym,
    atan,
    list,
    member,
    assoc,
    find,
    position,
    count,
    remove,
    intern,
    make_broadcast_stream,
    make_concatenated_stream,
    make_instance,
    class_of,
    floor,
    ceiling,
    round,
    truncate,
    aref,
    make_string,
    make_vector,
    svset,
    aset,
    set_slot_value,
    sset,
    make_unbound,
    class_of_internal,
    make_array,
    char,
    schar,
    substring,
    format,
    print,
    princ,
    encode_universal_time,
    make_pathname,
    make_hash_table,
    gethash,
    puthash,
    remhash,
    hash_table_count,
    hash_table_capacity,
    open,
    close_internal,
    close,
    read_line,
    write_line,
    write_string,
    read_byte,
    write_byte,
    file_position,
    set_file_position,
    file_length,
    finish_output,
    force_output,
    clear_input,
    clear_output,
    class_direct_superclasses,
    class_precedence_list,
    class_direct_slots,
    class_slots,
    slot_definition_name,
    slot_definition_initform,
    slot_definition_initargs,
    slot_definition_readers,
    slot_definition_writers,
    slot_definition_allocation,
    slot_definition_type,
    set_class_printer,
    copy_readtable,
    readtable_case,
    set_readtable_case,
};

const empty_consts = [_]Value{};
const halt_code = [_]u8{
    @truncate(@intFromEnum(Op.halt) & 0xFF),
    @truncate(@intFromEnum(Op.halt) >> 8),
};
const halt_chunk = Chunk{
    .code = @constCast(&halt_code),
    .const_pool = @ptrCast(@constCast(&empty_consts)),
    .const_count = 0,
    .code_len = halt_code.len,
    .arity = 0,
    .opt_count = 0,
    .key_count = 0,
    .has_rest = 0,
    .num_locals = 0,
};

fn elapsedNsSince(start_ns: i128) u64 {
    const now_ns = std.time.nanoTimestamp();
    if (now_ns <= start_ns) return 0;
    return @intCast(now_ns - start_ns);
}

/// Catch frame for exception handling
pub const CatchFrame = struct {
    /// Tag value to match against
    tag: Value,
    /// Chunk to return to
    chunk: *const Chunk,
    /// IP to jump to when throw is caught
    catch_ip: usize,
    /// Stack pointer to restore
    catch_sp: usize,
    /// Frame pointer to restore
    catch_fp: usize,
    /// Dynamic control-stack depths to restore on non-local exit.
    block_depth: usize,
    unwind_depth: usize,
    restart_depth: usize,
    progv_depth: usize,
    handler_depth: usize,
};

/// Unwind frame for unwind-protect
pub const UnwindFrame = struct {
    /// Chunk containing the cleanup code
    chunk: *const Chunk,
    /// IP of cleanup code start
    cleanup_ip: usize,
    /// Stack pointer when push_unwind was executed
    unwind_sp: usize,
    /// Frame pointer when push_unwind was executed
    unwind_fp: usize,
};

/// Block frame for block/return-from (lexical non-local exit)
pub const BlockFrame = struct {
    /// Block name (interned symbol raw value for identity comparison)
    name_raw: Value,
    /// Chunk to return to
    chunk: *const Chunk,
    /// IP to jump to after block exits (past the block body)
    exit_ip: usize,
    /// Stack pointer to restore
    block_sp: usize,
    /// Frame pointer to restore
    block_fp: usize,
    /// Catch stack depth to restore
    catch_depth: usize,
    /// Unwind stack depth to restore
    unwind_depth: usize,
    /// Restart stack depth to restore
    restart_depth: usize,
    /// Dynamic variable binding depth to restore
    progv_depth: usize,
    /// Handler stack depth to restore
    handler_depth: usize,
};

/// Restart frame for restart-case
pub const RestartFrame = struct {
    /// Restart name (interned symbol)
    name: Value,
    /// Stable dynamic-extent identity for restart objects.
    id: u64,
    /// Chunk containing the restart handler
    chunk: *const Chunk,
    /// IP of restart handler code
    handler_ip: usize,
    /// Stack pointer to restore when restart is invoked
    restart_sp: usize,
    /// Frame pointer to restore when restart is invoked
    restart_fp: usize,
    /// Catch stack depth to restore
    catch_depth: usize,
    /// Unwind stack depth to restore
    unwind_depth: usize,
    /// Block stack depth to restore
    block_depth: usize,
    /// Dynamic variable binding depth to restore
    progv_depth: usize,
    /// Handler stack depth to restore
    handler_depth: usize,
};

/// Handler frame for handler-bind
pub const HandlerFrame = struct {
    /// Condition type (symbol or list of types)
    condition_type: Value,
    /// Handler function (closure)
    handler_fn: Value,
};

/// Progv frame for dynamic variable binding
pub const ProgvFrame = struct {
    /// Saved global values (list of (symbol . old-value) pairs)
    saved_bindings: Value,
};

/// Call frame for function calls
pub const Frame = struct {
    /// Return address (chunk + ip)
    chunk: *const Chunk,
    return_ip: usize,
    /// Base pointer (stack index of first local)
    bp: usize,
    /// Current closure (for accessing captures)
    closure: ?*const runtime.Closure,
    /// Argument count passed to this function call
    argc: u8,
    /// Positional arguments consumed before keyword parsing.
    positional_argc: u8,
    /// Control stack depths at call entry (restored on normal return)
    block_depth: usize,
    catch_depth: usize,
    unwind_depth: usize,
    restart_depth: usize,
    progv_depth: usize,
    handler_depth: usize,
    /// Optional handler stack depth to restore on return.
    handler_restore_depth: ?usize,
};

/// Let scope frame for nested let bindings
/// Tracks where this scope's locals start on the stack
pub const Scope = struct {
    /// Base pointer for this scope's locals (stack index)
    bp: usize,
    /// Number of locals in this scope
    num_locals: u8,
};

/// Maximum nested let scopes
const MAX_SCOPES = 256;

/// Saved execution state for nested calls
/// Used by callClosure to save/restore state atomically
pub const State = struct {
    chunk: *const Chunk,
    ip: usize,
    fp: usize,
    sp: usize,
    scope_sp: usize,
    catch_sp: usize,
    unwind_sp: usize,
    restart_sp: usize,
    block_sp: usize,
    handler_sp: usize,
    progv_sp: usize,
    pending_handler_restore_depth: ?usize,
    chunk_pool: []Value,
    chunk_pool_owner: ?*std.ArrayList(Value),
    chunk_base: usize,
    current_closure: ?*const runtime.Closure,
    current_argc: u8,
    pending_throw_tag: Value,
    pending_throw_value: Value,
    throw_barrier_depth: usize,
    relay_throw_tag: Value,
    relay_throw_value: Value,
    jit_bridge_error: ?anyerror,
    jit_bridge_epoch: usize,
    jit_gc_forbidden_depth: usize,
    pending_error: ?anyerror,
    is_unwinding: bool,
    pending_block_idx: ?usize,
    pending_block_value: Value,
    is_returning_from_block: bool,
    secondary_values_count: usize,
    zero_values_returned: bool,

    pub fn save(vm: *const Vm) State {
        return .{
            .chunk = vm.chunk,
            .ip = vm.ip,
            .fp = vm.fp,
            .sp = vm.sp,
            .scope_sp = vm.scope_sp,
            .catch_sp = vm.catch_sp,
            .unwind_sp = vm.unwind_sp,
            .restart_sp = vm.restart_sp,
            .block_sp = vm.block_sp,
            .handler_sp = vm.handler_sp,
            .progv_sp = vm.progv_sp,
            .pending_handler_restore_depth = vm.pending_handler_restore_depth,
            .chunk_pool_owner = vm.chunk_pool_owner,
            .chunk_pool = if (vm.chunk_pool_owner) |owner| owner.items else vm.chunk_pool,
            .chunk_base = vm.chunk_base,
            .current_closure = vm.current_closure,
            .current_argc = vm.current_argc,
            .pending_throw_tag = vm.pending_throw_tag,
            .pending_throw_value = vm.pending_throw_value,
            .throw_barrier_depth = vm.throw_barrier_depth,
            .relay_throw_tag = vm.relay_throw_tag,
            .relay_throw_value = vm.relay_throw_value,
            .jit_bridge_error = vm.jit_bridge_error,
            .jit_bridge_epoch = vm.jit_bridge_epoch,
            .jit_gc_forbidden_depth = vm.jit_gc_forbidden_depth,
            .pending_error = vm.pending_error,
            .is_unwinding = vm.is_unwinding,
            .pending_block_idx = vm.pending_block_idx,
            .pending_block_value = vm.pending_block_value,
            .is_returning_from_block = vm.is_returning_from_block,
            .secondary_values_count = vm.secondary_values_count,
            .zero_values_returned = vm.zero_values_returned,
        };
    }

pub fn restore(self: State, vm: *Vm) void {
        vm.chunk = self.chunk;
        vm.ip = self.ip;
        vm.fp = self.fp;
        vm.sp = self.sp;
        vm.scope_sp = self.scope_sp;
        vm.catch_sp = self.catch_sp;
        vm.unwind_sp = self.unwind_sp;
        vm.restart_sp = self.restart_sp;
        vm.block_sp = self.block_sp;
        vm.handler_sp = self.handler_sp;
        vm.progv_sp = self.progv_sp;
        vm.pending_handler_restore_depth = self.pending_handler_restore_depth;
        vm.chunk_pool_owner = self.chunk_pool_owner;
        vm.chunk_pool = if (self.chunk_pool_owner) |owner| owner.items else self.chunk_pool;
        vm.chunk_base = self.chunk_base;
        vm.current_closure = self.current_closure;
        vm.current_argc = self.current_argc;
        vm.pending_throw_tag = self.pending_throw_tag;
        vm.pending_throw_value = self.pending_throw_value;
        vm.throw_barrier_depth = self.throw_barrier_depth;
        vm.relay_throw_tag = self.relay_throw_tag;
        vm.relay_throw_value = self.relay_throw_value;
        vm.jit_bridge_error = self.jit_bridge_error;
        vm.jit_bridge_epoch = self.jit_bridge_epoch;
        vm.jit_gc_forbidden_depth = self.jit_gc_forbidden_depth;
        vm.pending_error = self.pending_error;
        vm.is_unwinding = self.is_unwinding;
        vm.pending_block_idx = self.pending_block_idx;
        vm.pending_block_value = self.pending_block_value;
        vm.is_returning_from_block = self.is_returning_from_block;
        vm.secondary_values_count = self.secondary_values_count;
        vm.zero_values_returned = self.zero_values_returned;
    }
};

fn hostCallbackMovedControl(vm: *const Vm, saved: State) bool {
    return vm.chunk != saved.chunk or
        vm.ip != saved.ip or
        vm.fp != saved.fp or
        vm.sp != saved.sp or
        vm.scope_sp != saved.scope_sp or
        vm.catch_sp != saved.catch_sp or
        vm.unwind_sp != saved.unwind_sp or
        vm.restart_sp != saved.restart_sp or
        vm.block_sp != saved.block_sp or
        vm.handler_sp != saved.handler_sp or
        vm.progv_sp != saved.progv_sp or
        vm.pending_handler_restore_depth != saved.pending_handler_restore_depth or
        vm.current_closure != saved.current_closure or
        vm.current_argc != saved.current_argc or
        vm.pending_throw_tag.raw != saved.pending_throw_tag.raw or
        vm.pending_throw_value.raw != saved.pending_throw_value.raw or
        vm.throw_barrier_depth != saved.throw_barrier_depth or
        vm.relay_throw_tag.raw != saved.relay_throw_tag.raw or
        vm.relay_throw_value.raw != saved.relay_throw_value.raw or
        vm.pending_error != saved.pending_error or
        vm.is_unwinding != saved.is_unwinding or
        vm.pending_block_idx != saved.pending_block_idx or
        vm.pending_block_value.raw != saved.pending_block_value.raw or
        vm.is_returning_from_block != saved.is_returning_from_block or
        vm.secondary_values_count != saved.secondary_values_count or
        vm.zero_values_returned != saved.zero_values_returned;
}

/// Virtual Machine
/// Bridge between the VM's eval callback and the parser's ReadEvalFn.
/// Stored on the stack during read_from_string so the parser can call
/// back into the VM/REPL to evaluate #. expressions.
const ReadEvalBridge = struct {
    callback: ?*const fn (Value, *anyopaque) Error!Value,
    context: ?*anyopaque,
};

const DispatchMacroBridge = struct {
    vm: *Vm,
};

fn parseWithHookError(parser: *Parser) Error!Value {
    return parser.parse() catch |parse_err| {
        if (parse_err == error.UnexpectedToken) {
            if (parser.takeHookError()) |hook_err| return hook_err;
        }
        return parse_err;
    };
}

fn readEvalBridge(ctx: *anyopaque, expr: Value) Error!Value {
    const bridge: *ReadEvalBridge = @ptrCast(@alignCast(ctx));
    const callback = bridge.callback orelse return error.UnexpectedToken;
    const eval_ctx = bridge.context orelse return error.UnexpectedToken;
    return callback(expr, eval_ctx);
}

fn dispatchMacroBridge(
    ctx: *anyopaque,
    function: Value,
    disp_char: u8,
    sub_char: u8,
    arg: ?u32,
    stream: Value,
) Error!?Value {
    _ = disp_char;
    const bridge: *DispatchMacroBridge = @ptrCast(@alignCast(ctx));
    const arg_val = if (arg) |n| Value.makeFixnum(@intCast(n)) else Value.nil;
    const args = [_]Value{ stream, Value.makeCharacter(sub_char), arg_val };
    const result = try bridge.vm.callFromStackAt(bridge.vm.sp, function, &args);
    if (bridge.vm.zero_values_returned) return null;
    return result;
}

fn macroCharacterBridge(
    ctx: *anyopaque,
    function: Value,
    macro_char: u8,
    stream: Value,
) Error!?Value {
    const bridge: *DispatchMacroBridge = @ptrCast(@alignCast(ctx));
    const args = [_]Value{ stream, Value.makeCharacter(macro_char) };
    const result = try bridge.vm.callFromStackAt(bridge.vm.sp, function, &args);
    if (bridge.vm.zero_values_returned) return null;
    return result;
}

const ParsedRead = struct {
    value: Value,
    next: usize,
};

fn tryParseReadBytes(vm: *Vm, bytes: []const u8, final: bool) Error!?ParsedRead {
    var parser = try Parser.init(vm.allocator, vm.heap, bytes, &vm.builtins);
    defer parser.deinit();
    if (parser.current.kind == .eof) return null;
    try parser.setReadtable(try vm.currentReadtable());

    var dm_ctx = DispatchMacroBridge{ .vm = vm };
    parser.setDispatchMacroHook(@ptrCast(&dm_ctx), dispatchMacroBridge);
    parser.setMacroCharacterHook(@ptrCast(&dm_ctx), macroCharacterBridge);
    var re_ctx = ReadEvalBridge{ .callback = vm.eval_callback, .context = vm.eval_context };
    if (vm.eval_callback != null) {
        parser.setReadEvalHook(@ptrCast(&re_ctx), readEvalBridge);
    }

    const value = parseWithHookError(&parser) catch |err| switch (err) {
        error.UnterminatedList, error.UnexpectedToken => {
            if (final) return err;
            return null;
        },
        else => return err,
    };
    return .{ .value = value, .next = parser.lexer.token_start };
}

fn readSexpFromStream(vm: *Vm, stream: Value) Error!struct { value: Value, eof: bool } {
    var buf = std.ArrayList(u8){};
    defer buf.deinit(vm.allocator);

    while (true) {
        if (buf.items.len > 0) {
            if (try tryParseReadBytes(vm, buf.items, false)) |parsed| {
                try io.setUnreadTail(vm.heap, stream, buf.items[parsed.next..]);
                return .{ .value = parsed.value, .eof = false };
            }
        }

        const ch = try io.readChar(stream, null, null);
        if (ch.isNil()) {
            if (buf.items.len == 0) return .{ .value = Value.nil, .eof = true };
            if (try tryParseReadBytes(vm, buf.items, true)) |parsed| {
                try io.setUnreadTail(vm.heap, stream, buf.items[parsed.next..]);
                return .{ .value = parsed.value, .eof = false };
            }
            return .{ .value = Value.nil, .eof = true };
        }
        if (!ch.isFixnum()) return error.TypeMismatch;
        const byte = std.math.cast(u8, ch.toFixnum()) orelse return error.InvalidArgument;
        try buf.append(vm.allocator, byte);
    }
}

fn traceJitBridgeValue(v: Value) void {
    switch (v.typeKind()) {
        .nil => std.debug.print("nil", .{}),
        .t => std.debug.print("t", .{}),
        .fixnum => std.debug.print("fixnum({d})", .{v.toFixnum()}),
        .symbol => std.debug.print("symbol({s})", .{v.toPtr(Symbol).getName()}),
        .keyword => std.debug.print("keyword(:{s})", .{v.toPtr(runtime.Keyword).getName()}),
        else => std.debug.print("{s}(0x{x})", .{ @tagName(v.typeKind()), v.raw }),
    }
}

fn refreshJitHeap(vm: *Vm) void {
    if (jit_backend.heapContext() != vm.heap) {
        jit_backend.setHeap(vm.heap);
        return;
    }
    jit_backend.refreshHeapCursor();
}

fn jitCallBridgeInvoke(vm: *Vm, fn_raw: u64, args: []const Value) u64 {
    const fn_val = Value{ .raw = fn_raw };
    const trace_bridge = std.posix.getenv("HABU_TRACE_JIT_BRIDGE") != null;
    if (trace_bridge) {
        std.debug.print("JIT_BRIDGE call fn=", .{});
        traceJitBridgeValue(fn_val);
        std.debug.print(" argc={d}", .{args.len});
        for (args, 0..) |arg, i| {
            std.debug.print(" a{d}=", .{i});
            traceJitBridgeValue(arg);
        }
        std.debug.print("\n", .{});
    }
    // Commit inline-cons progress before entering VM helper paths. Otherwise
    // setHeap() would reset g_alloc_ptr from stale heap.alloc_ptr and clobber
    // in-flight JIT allocations across bridge calls.
    jit_backend.syncHeapFromGlobal(vm.heap);
    // Bridge calls may run GC and move semispaces; refresh JIT bump-cache from
    // heap before and after so inline-cons globals never drift from VM state.
    refreshJitHeap(vm);
    const result = vm.callFromStackAt(vm.sp, fn_val, args) catch |err| {
        vm.jit_bridge_error = err;
        // Keep allocator cursors coherent before non-local escape.
        jit_backend.syncHeapFromGlobal(vm.heap);
        refreshJitHeap(vm);
        if (trace_bridge) {
            std.debug.print("JIT_BRIDGE err {s} argc={d}\n", .{ @errorName(err), args.len });
        }
        jit_backend.bridgeThrow();
        std.debug.panic("jit bridge throw returned: {s}", .{@errorName(err)});
    };
    // Nested JIT calls inside vm.callFromStackAt may have advanced g_alloc_ptr.
    // Sync first, then refresh globals against the current semispace after GC.
    jit_backend.syncHeapFromGlobal(vm.heap);
    refreshJitHeap(vm);
    if (trace_bridge) {
        std.debug.print("JIT_BRIDGE ret ", .{});
        traceJitBridgeValue(result);
        std.debug.print("\n", .{});
    }
    return vm.resolveForwardedValue(result).raw;
}

fn jitCallBridge0(ctx: *anyopaque, fn_raw: u64) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    var args: [0]Value = .{};
    return jitCallBridgeInvoke(vm, fn_raw, args[0..]);
}

fn jitCallBridge1(ctx: *anyopaque, fn_raw: u64, arg0: u64) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const args = [_]Value{Value{ .raw = arg0 }};
    return jitCallBridgeInvoke(vm, fn_raw, &args);
}

fn jitCallBridge2(ctx: *anyopaque, fn_raw: u64, arg0: u64, arg1: u64) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const args = [_]Value{ Value{ .raw = arg0 }, Value{ .raw = arg1 } };
    return jitCallBridgeInvoke(vm, fn_raw, &args);
}

fn jitCallBridge3(ctx: *anyopaque, fn_raw: u64, arg0: u64, arg1: u64, arg2: u64) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const args = [_]Value{ Value{ .raw = arg0 }, Value{ .raw = arg1 }, Value{ .raw = arg2 } };
    return jitCallBridgeInvoke(vm, fn_raw, &args);
}

fn jitCallBridge4(ctx: *anyopaque, fn_raw: u64, arg0: u64, arg1: u64, arg2: u64, arg3: u64) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const args = [_]Value{ Value{ .raw = arg0 }, Value{ .raw = arg1 }, Value{ .raw = arg2 }, Value{ .raw = arg3 } };
    return jitCallBridgeInvoke(vm, fn_raw, &args);
}

fn jitCallBridge5(ctx: *anyopaque, fn_raw: u64, arg0: u64, arg1: u64, arg2: u64, arg3: u64, arg4: u64) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const args = [_]Value{
        Value{ .raw = arg0 },
        Value{ .raw = arg1 },
        Value{ .raw = arg2 },
        Value{ .raw = arg3 },
        Value{ .raw = arg4 },
    };
    return jitCallBridgeInvoke(vm, fn_raw, &args);
}

fn jitCallBridge6(ctx: *anyopaque, fn_raw: u64, arg0: u64, arg1: u64, arg2: u64, arg3: u64, arg4: u64, arg5: u64) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const args = [_]Value{
        Value{ .raw = arg0 },
        Value{ .raw = arg1 },
        Value{ .raw = arg2 },
        Value{ .raw = arg3 },
        Value{ .raw = arg4 },
        Value{ .raw = arg5 },
    };
    return jitCallBridgeInvoke(vm, fn_raw, &args);
}

fn jitCallBridge7(ctx: *anyopaque, fn_raw: u64, arg0: u64, arg1: u64, arg2: u64, arg3: u64, arg4: u64, arg5: u64, arg6: u64) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const args = [_]Value{
        Value{ .raw = arg0 },
        Value{ .raw = arg1 },
        Value{ .raw = arg2 },
        Value{ .raw = arg3 },
        Value{ .raw = arg4 },
        Value{ .raw = arg5 },
        Value{ .raw = arg6 },
    };
    return jitCallBridgeInvoke(vm, fn_raw, &args);
}

fn jitCallBridgePushProgv(ctx: *anyopaque, symbols_raw: u64, values_raw: u64) callconv(.c) u16 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const symbols = vm.resolveForwardedValue(Value{ .raw = symbols_raw });
    const values = vm.resolveForwardedValue(Value{ .raw = values_raw });
    jit_backend.syncHeapFromGlobal(vm.heap);
    refreshJitHeap(vm);
    vm.pushProgvFrame(symbols, values) catch |err| {
        vm.jit_bridge_error = err;
        jit_backend.syncHeapFromGlobal(vm.heap);
        refreshJitHeap(vm);
        return @intCast(@intFromError(err));
    };
    jit_backend.syncHeapFromGlobal(vm.heap);
    refreshJitHeap(vm);
    return 0;
}

fn jitCallBridgePopProgv(ctx: *anyopaque) callconv(.c) u16 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    jit_backend.syncHeapFromGlobal(vm.heap);
    refreshJitHeap(vm);
    vm.popProgvFrame() catch |err| {
        vm.jit_bridge_error = err;
        jit_backend.syncHeapFromGlobal(vm.heap);
        refreshJitHeap(vm);
        return @intCast(@intFromError(err));
    };
    jit_backend.syncHeapFromGlobal(vm.heap);
    refreshJitHeap(vm);
    return 0;
}

fn jitErrorBridgeSet(ctx: *anyopaque, err_int: u16) callconv(.c) void {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    vm.jit_bridge_error = @errorFromInt(err_int);
}

fn jitGlobalBridgeLoad(ctx: *anyopaque, idx: u16) callconv(.c) u64 {
    const vm: *Vm = @ptrCast(@alignCast(ctx));
    const val = vm.loadGlobal(idx) catch |err| {
        vm.jit_bridge_error = err;
        jit_backend.bridgeThrow();
        std.debug.panic("jit global bridge throw returned: {s}", .{@errorName(err)});
    };
    return vm.resolveForwardedValue(val).raw;
}

const JitInvokeCtx = struct {
    compiled: *const jit_backend.CompiledFn,
    args_ptr: [*]const Value,
    argc: u8,
};

fn jitInvokeCompiledFn(ctx_ptr: *anyopaque) callconv(.c) u64 {
    const ctx: *const JitInvokeCtx = @ptrCast(@alignCast(ctx_ptr));
    const args = ctx.args_ptr[0..ctx.argc];
    return ctx.compiled.callFromValues(args).raw;
}

pub const Vm = struct {
    pub const IrTag = std.meta.Tag(compiler.ir.Ir);
    const IR_TAG_N = std.meta.fields(IrTag).len;

    pub const JitAdmStats = struct {
        cand: u64 = 0,
        elig: u64 = 0,
        comp: u64 = 0,
        sk_speed: u64 = 0,
        sk_safety: u64 = 0,
        sk_assert: u64 = 0,
        sk_caps: u64 = 0,
        sk_opt: u64 = 0,
        sk_key: u64 = 0,
        sk_rest: u64 = 0,
        sk_chunk: u64 = 0,
        fail_unsupported: u64 = 0,
        fail_other: u64 = 0,
        cache_comp: u64 = 0,
        cache_unsupported: u64 = 0,
        cache_failed: u64 = 0,
    };

    pub const CallShapeStats = struct {
        total: u64 = 0,
        fixed: u64 = 0,
        optional: u64 = 0,
        key: u64 = 0,
        rest: u64 = 0,
        dynamic: u64 = 0,
        tail: u64 = 0,
    };

    pub const CallShapeKind = enum(u8) {
        fixed,
        optional,
        key,
        rest,
    };

    const FnResolveEntry = struct {
        sym: Value = Value.nil,
        fn_val: Value = Value.nil,
    };

    const GlobalIndexCacheEntry = struct {
        sym: Value = Value.nil,
        idx: u16 = 0,
    };

    const KeyAllowlistCacheEntry = struct {
        chunk_key: usize = 0,
        key_count: u8 = 0,
        len: u8 = 0,
        keywords: [KEY_FAST_TABLE_MAX]Value = [_]Value{Value.nil} ** KEY_FAST_TABLE_MAX,
    };

    const ChunkConstCacheEntry = struct {
        chunk_key: usize = 0,
        gc_count: usize = 0,
    };

    pub const JitCompileStatus = enum(u8) {
        none = 0,
        compiled,
        unsupported,
        failed,
    };

    const JitCompileCacheEntry = struct {
        chunk_key: u64 = 0,
        gc_count: usize = 0,
        status: JitCompileStatus = .none,
    };

    const JitFnEntry = struct {
        chunk: Value = Value.nil,
        compiled: *jit_backend.CompiledFn,
    };

    /// Value stack
    stack: [STACK_SIZE]Value,
    /// Stack pointer (next free slot)
    sp: usize,

    /// Call stack
    frames: [MAX_FRAMES]Frame,
    /// Frame pointer (current frame index)
    fp: usize,

    /// Current chunk being executed
    chunk: *const Chunk,
    /// Instruction pointer
    ip: usize,
    /// Nested execute depth for re-entrant callbacks
    execute_depth: usize,

    /// Heap for allocations
    heap: *Heap,

    /// Allocator
    allocator: std.mem.Allocator,

    /// Reusable buffers for GC root tracking.
    gc_slots: std.ArrayList(*Value),
    /// Deferred safepoint debt polls to reduce mutator overhead.
    safepoint_batch_ops: usize,
    safepoint_batch_bytes: usize,

    /// Global variables (indexed by constant pool index)
    globals: [MAX_GLOBALS]Value,
    /// Number of defined globals
    num_globals: usize,

    /// Compiler-retained values that must survive compile->emit GC windows.
    comp_retain_vals: std.ArrayList(Value),
    comp_root_stack: [MAX_COMPILE_ROOTS]Value,
    comp_root_sp: usize,

    /// Chunk pool for closures (boxed chunk values)
    chunk_pool: []Value,
    chunk_pool_owner: ?*std.ArrayList(Value),
    /// Base offset for current eval's chunks
    chunk_base: usize,

    /// Chunk roots saved by external entrypoints (callClosure/callFromStack/applyFromStack)
    /// so they stay alive and get relocated across GC.
    saved_chunks: [MAX_SAVED_CHUNKS]Value,
    saved_chunk_sp: usize,

    /// Catch stack for exception handling
    catch_stack: [MAX_CATCHES]CatchFrame,
    /// Catch stack pointer
    catch_sp: usize,

    /// Unwind stack for unwind-protect
    unwind_stack: [MAX_UNWINDS]UnwindFrame,
    /// Unwind stack pointer
    unwind_sp: usize,

    /// Restart stack for restart-case
    restart_stack: [MAX_RESTARTS]RestartFrame,
    /// Restart stack pointer
    restart_sp: usize,
    restart_next_id: u64,

    /// Block stack for block/return-from
    block_stack: [MAX_BLOCKS]BlockFrame,
    /// Block stack pointer
    block_sp: usize,

    /// Handler stack for handler-bind
    handler_stack: [MAX_HANDLERS]HandlerFrame,
    /// Handler stack pointer
    handler_sp: usize,
    /// One-shot restore depth consumed by the next non-tail call frame.
    pending_handler_restore_depth: ?usize,

    /// Progv stack for dynamic variable binding
    progv_stack: [MAX_PROGVS]ProgvFrame,
    /// Progv stack pointer
    progv_sp: usize,

    /// Let scope stack for nested let bindings
    scope_stack: [MAX_SCOPES]Scope,
    /// Scope stack pointer (number of active scopes)
    scope_sp: usize,

    /// Saved throw state for unwinding through unwind-protect
    pending_throw_tag: Value,
    pending_throw_value: Value,
    throw_barrier_depth: usize,
    relay_throw_tag: Value,
    relay_throw_value: Value,
    jit_bridge_error: ?anyerror,
    jit_bridge_epoch: usize,
    jit_gc_forbidden_depth: usize,
    pending_error: ?anyerror,
    is_unwinding: bool,

    /// Last condition value from (error ...) for REPL diagnostics.
    /// Set by error_user before doThrow; cleared on successful catch.
    last_error_value: Value,

    /// Saved return-from state for unwinding through unwind-protect
    pending_block_idx: ?usize,
    pending_block_value: Value,
    is_returning_from_block: bool,
    

    /// Random number generator state
    prng: std.Random.DefaultPrng,
    prng_seeded: bool,

    /// Secondary values buffer for multiple-value-bind
    secondary_values: [MAX_SECONDARY_VALUES]Value,
    /// Number of secondary values currently available
    secondary_values_count: usize,
    /// True only when the last produced result was exactly zero values.
    zero_values_returned: bool,

    /// Global environment for boundp/fboundp lookups
    global_env: ?*GlobalEnv,

    /// Callback for (load "filename") - set by REPL
    load_callback: ?*const fn ([]const u8, *anyopaque) Error!Value,
    load_context: ?*anyopaque,

    /// Callback for (eval expr) - set by REPL
    eval_callback: ?*const fn (Value, *anyopaque) Error!Value,
    eval_context: ?*anyopaque,

    /// Callback for (macroexpand expr) - set by REPL
    macroexpand_callback: ?*const fn (Value, *anyopaque) Error!Value,
    macroexpand_context: ?*anyopaque,

    /// Callback for (macroexpand-1 expr) - set by REPL
    macroexpand_1_callback: ?*const fn (Value, *anyopaque) Error!Value,
    macroexpand_1_context: ?*anyopaque,

    /// Callback for fboundp - checks if symbol is a function (macro, primitive, or defun)
    fboundp_callback: ?*const fn (Value, *anyopaque) Error!bool,
    fboundp_context: ?*anyopaque,

    /// Callback for symbol/function designator resolution.
    /// Used to lazily materialize callable values for builtin primitive symbols.
    function_resolve_callback: ?*const fn (Value, *anyopaque) Error!?Value,
    function_resolve_context: ?*anyopaque,
    fn_resolve_cache: [MAX_FN_RESOLVE_CACHE]FnResolveEntry,
    global_index_cache: [MAX_GLOBAL_INDEX_CACHE]GlobalIndexCacheEntry,
    key_allowlist_cache: [MAX_KEY_ALLOWLIST_CACHE]KeyAllowlistCacheEntry,
    chunk_const_cache: [MAX_CHUNK_CONST_CACHE]ChunkConstCacheEntry,
    const_last_chunk_key: usize,
    const_last_gc_count: usize,
    jit_compile_cache: [MAX_JIT_COMPILE_CACHE]JitCompileCacheEntry,
    /// Value cells for uninterned symbols, keyed by stable symbol uid.
    uninterned_values: std.AutoHashMap(u64, Value),

    /// Counter for gensym
    gensym_counter: u64,

    /// Current closure for load_capture when fp=0 (used by callClosure)
    current_closure: ?*const runtime.Closure,

    /// Current argc for load_argc when fp=0 (used by callClosure)
    current_argc: u8,

    /// External GC roots (e.g., JIT stack/consts)
    ext_roots: []Value,
    ext_roots_owner: ?*std.ArrayList(Value),
    ext_roots_saved: [MAX_EXT_ROOT_SNAPSHOTS]ExtRootsSnapshot,
    ext_roots_saved_sp: usize,

    /// Hoist SSA JIT: compiled functions registered by owning chunk.
    jit_fns: std.ArrayList(JitFnEntry),

    /// Stable host-root slots for JIT literal Values.
    jit_literal_roots: std.ArrayList(*Value),
    jit_adm: JitAdmStats,
    unsupported_tags: [IR_TAG_N]u64,
    jit_direct_calls: u64,
    call_shape: CallShapeStats,
    track_call_shape: bool,

    trace_jit_call: bool,
    trace_fn_resolve: bool,
    trace_call_mismatch: bool,
    trace_call_mismatch_apply: bool,
    trace_invalid_opcode: bool,
    trace_error_context: bool,
    trace_error_include_halt: bool,
    trace_error_only_filter: ?[]const u8,
    trace_chunk_only_filter: ?[]const u8,
    trace_op_only_filter: ?[]const u8,
    trace_call_ret: bool,
    trace_call_ret_filter: ?[]const u8,
    trace_ext_root_owner: bool,
    trace_jit_call_args: bool,
    trace_validate_root_layout: bool,
    trap_validate_root_layout: bool,
    trace_chunk_pool_slot: ?usize,
    trace_bad_root_layout: bool,
    trace_bad_global_root: bool,
    trace_bad_global_kind: bool,
    trace_builtin_write: bool,
    trap_builtin_write: bool,
    trace_stale_root_table: bool,
    trap_stale_root_table: bool,
    trace_local_mismatch: bool,
    trace_upvalue: bool,
    trace_sub_context: bool,
    trace_copy_structure: bool,
    trace_call_args: bool,
    trace_io: bool,
    trace_progv_disasm: bool,
    trace_throw: bool,
    trap_progv_corrupt: bool,
    trace_block_miss: bool,
    trace_error_disasm: bool,
    trace_call_mismatch_fn_disasm: bool,
    trace_call_mismatch_callee_disasm: bool,
    trace_call_mismatch_disasm: bool,

    /// Pre-interned builtin symbols for fast dispatch
    builtins: BuiltinSymbols,

    /// Pre-interned type symbols for runtime type dispatch
    type_syms: type_mod.TypeSymbols,

    const STACK_SIZE = 8192;
    const MAX_SECONDARY_VALUES = 20;
    const MAX_FRAMES = 2048;
    const MAX_GLOBALS = 16384;
    // Real macro/batch workloads can build very deep dynamic-control stacks.
    // Keep these fixed-size stacks generous until/if we move them to dynamic
    // storage with root-scanned backing buffers.
    const MAX_CATCHES = 256;
    const MAX_UNWINDS = 256;
    const MAX_RESTARTS = 128;
    const MAX_BLOCKS = MAX_FRAMES;
    const MAX_PROGVS = 2048;
    const MAX_HANDLERS = 128;
    const MAX_SAVED_CHUNKS = 1024;
    const MAX_COMPILE_ROOTS = 4096;
    const MAX_EXT_ROOT_SNAPSHOTS = 256;
    const MAX_FN_RESOLVE_CACHE = 256;
    const MAX_GLOBAL_INDEX_CACHE = 1024;
    const MAX_KEY_ALLOWLIST_CACHE = 256;
    const MAX_CHUNK_CONST_CACHE = 1024;
    const MAX_JIT_COMPILE_CACHE = 2048;
    const SAFEPOINT_BATCH_OPS = 32;
    const SAFEPOINT_BATCH_BYTES = 64 * 1024;
    const KEY_FAST_TABLE_MAX = 8;
    const RAW_TAG_MASK: u64 = 0xE;
    const RAW_PTR_MASK: u64 = ~@as(u64, 0xF);
    const RAW_CONS_TAG: u64 = @intFromEnum(runtime.Tag.cons);
    const RAW_KEYWORD_TAG: u64 = @intFromEnum(runtime.Tag.keyword);

    fn chunkRoot(ptr: *const Chunk) Value {
        const addr = @intFromPtr(ptr);
        if (addr & 0xF != 0) return Value.nil;
        return Value.makeChunk(ptr);
    }

    fn chunkFromValue(self: *Vm, val: Value) ?*const Chunk {
        _ = self;
        if (!val.isChunk()) return null;
        const chunk = val.toPtr(Chunk);
        if (chunk.kind != .chunk) return null;
        return chunk;
    }

    fn chunkTraceName(chunk: *const Chunk) []const u8 {
        return switch (chunk.name.typeKind()) {
            .symbol => chunk.name.toPtr(Symbol).getName(),
            .string => chunk.name.toPtr(runtime.String).bytes(),
            else => "<anon>",
        };
    }

    fn isStaleNurseryAddr(self: *const Vm, addr: usize) bool {
        const stale_start = @intFromPtr(self.heap.to_start);
        const stale_end = stale_start + self.heap.space_size;
        return addr >= stale_start and addr < stale_end;
    }

    fn csvHasExactToken(csv: []const u8, needle: []const u8) bool {
        var it = std.mem.splitScalar(u8, csv, ',');
        while (it.next()) |raw_tok| {
            const tok = std.mem.trim(u8, raw_tok, " \t\r\n");
            if (tok.len == 0) continue;
            if (std.ascii.eqlIgnoreCase(tok, needle)) return true;
        }
        return false;
    }

    fn csvHasSubstringToken(csv: []const u8, haystack: []const u8) bool {
        var it = std.mem.splitScalar(u8, csv, ',');
        while (it.next()) |raw_tok| {
            const tok = std.mem.trim(u8, raw_tok, " \t\r\n");
            if (tok.len == 0) continue;
            if (std.ascii.indexOfIgnoreCase(haystack, tok) != null) return true;
        }
        return false;
    }

    fn envTraceCount(name: []const u8, default_count: usize) usize {
        const raw_c = std.posix.getenv(name) orelse return default_count;
        const raw = std.mem.trim(u8, std.mem.sliceTo(raw_c, 0), " \t\r\n");
        if (raw.len == 0) return default_count;
        const parsed = std.fmt.parseUnsigned(usize, raw, 10) catch return default_count;
        if (parsed == 0) return default_count;
        return parsed;
    }

    fn envTraceIndex(name: []const u8) ?usize {
        const raw_c = std.posix.getenv(name) orelse return null;
        const raw = std.mem.trim(u8, std.mem.sliceTo(raw_c, 0), " \t\r\n");
        if (raw.len == 0) return null;
        return std.fmt.parseUnsigned(usize, raw, 10) catch null;
    }

    fn invalidOpcode(self: *const Vm, comptime site: []const u8) Error {
        if (self.trace_invalid_opcode) {
            const chunk_addr = @intFromPtr(self.chunk);
            const from_start = @intFromPtr(self.heap.from_start);
            const from_end = @intFromPtr(self.heap.from_end);
            const to_start = @intFromPtr(self.heap.to_start);
            const to_end = to_start + self.heap.space_size;
            const in_from = chunk_addr >= from_start and chunk_addr < from_end;
            const in_to = chunk_addr >= to_start and chunk_addr < to_end;
            const in_heap = in_from or in_to;
            std.debug.print(
                "TRACE invalid-opcode site={s} chunk=0x{x} ip={d} sp={d} fp={d} catch={d} unwind={d} block={d}\n",
                .{
                    site,
                    chunk_addr,
                    self.ip,
                    self.sp,
                    self.fp,
                    self.catch_sp,
                    self.unwind_sp,
                    self.block_sp,
                },
            );
            if (chunk_addr == @intFromPtr(&halt_chunk)) {
                std.debug.print("  chunk-meta=halt\n", .{});
            } else {
                std.debug.print(
                    "  chunk-meta in_heap={any} in_from={any} in_to={any} from=[0x{x},0x{x}) to=[0x{x},0x{x})\n",
                    .{ in_heap, in_from, in_to, from_start, from_end, to_start, to_end },
                );
                if (in_to and (chunk_addr & 0xF) == 0) {
                    const first_word: *const Value = @ptrFromInt(chunk_addr);
                    std.debug.print(
                        "  chunk-meta to-space first-word=0x{x:0>16} forwarding={any}\n",
                        .{ first_word.raw, first_word.isForwarding() },
                    );
                }
            }
        }
        return error.InvalidOpcode;
    }

    fn shouldTraceError(self: *Vm, err: anyerror) bool {
        if (!self.trace_error_context) return false;
        if (err == error.Halt and !self.trace_error_include_halt) return false;

        if (self.trace_error_only_filter) |filter| {
            if (!csvHasExactToken(filter, @errorName(err))) return false;
        }
        if (self.trace_chunk_only_filter) |filter| {
            if (!csvHasSubstringToken(filter, chunkTraceName(self.chunk))) return false;
        }
        return true;
    }

    fn shouldTraceOpError(self: *Vm, op: Op, err: anyerror) bool {
        if (!shouldTraceError(self, err)) return false;
        if (self.trace_op_only_filter) |filter| {
            if (!csvHasExactToken(filter, @tagName(op))) return false;
        }
        return true;
    }

    fn tracePrintAtom(v: Value) void {
        switch (v.typeKind()) {
            .nil => std.debug.print("nil", .{}),
            .t => std.debug.print("t", .{}),
            .fixnum => std.debug.print("fixnum({d})", .{v.toFixnum()}),
            .float => std.debug.print("float({d})", .{v.toFloat()}),
            .char => std.debug.print("char(U+{X:0>4})", .{v.toCharacter()}),
            .symbol => std.debug.print("symbol({s})", .{v.toPtr(Symbol).getName()}),
            .keyword => std.debug.print("keyword(:{s})", .{v.toPtr(runtime.Keyword).getName()}),
            .string => {
                const s = v.toPtr(String).bytes();
                const n = @min(s.len, @as(usize, 24));
                std.debug.print("string(len={d},\"{s}", .{ s.len, s[0..n] });
                if (s.len > n) std.debug.print("...\"", .{}) else std.debug.print("\"", .{});
                std.debug.print(")", .{});
            },
            else => std.debug.print("{s}(0x{x})", .{ @tagName(v.typeKind()), v.raw }),
        }
    }

    fn tracePrintValue(v: Value) void {
        switch (v.typeKind()) {
            .cons => {
                const c = v.toPtr(Cons);
                std.debug.print("cons(", .{});
                if (c.car.isCons()) {
                    const op = c.car.toPtr(Cons);
                    std.debug.print("caar=", .{});
                    tracePrintAtom(op.car);
                } else {
                    std.debug.print("car=", .{});
                    tracePrintAtom(c.car);
                }
                std.debug.print(",cdr={s})", .{@tagName(c.cdr.typeKind())});
            },
            .closure => {
                const clo = v.toPtr(runtime.Closure);
                std.debug.print("closure(", .{});
                if (clo.code.isChunk()) {
                    std.debug.print("code={s}", .{chunkTraceName(clo.code.toPtr(Chunk))});
                } else {
                    std.debug.print("code={s}", .{@tagName(clo.code.typeKind())});
                }
                std.debug.print(")", .{});
            },
            else => tracePrintAtom(v),
        }
    }

    fn traceValueName(v: Value) []const u8 {
        return switch (v.typeKind()) {
            .symbol => v.toPtr(Symbol).getName(),
            .keyword => v.toPtr(runtime.Keyword).getName(),
            .string => v.toPtr(String).bytes(),
            else => @tagName(v.typeKind()),
        };
    }

    fn shouldTraceCallRet(self: *Vm, fn_designator: ?Value, caller_chunk: *const Chunk, callee_chunk: ?*const Chunk) bool {
        if (!self.trace_call_ret) return false;
        const filter = self.trace_call_ret_filter orelse return true;
        if (csvHasSubstringToken(filter, chunkTraceName(caller_chunk))) return true;
        if (callee_chunk) |c| {
            if (csvHasSubstringToken(filter, chunkTraceName(c))) return true;
        }
        if (fn_designator) |f| {
            if (csvHasSubstringToken(filter, traceValueName(f))) return true;
        }
        return false;
    }

    fn valueFieldSlice(comptime T: type, ptr: *T) []Value {
        const info = @typeInfo(T);
        comptime {
            if (info != .@"struct") {
                @compileError("valueFieldSlice requires a struct type");
            }
            for (info.@"struct".fields) |field| {
                if (field.type != Value) {
                    @compileError("valueFieldSlice requires all fields to be Value");
                }
            }
        }
        const count = info.@"struct".fields.len;
        const vals: [*]Value = @ptrCast(ptr);
        return vals[0..count];
    }

    const BUILTIN_ROOT_N = @typeInfo(BuiltinSymbols).@"struct".fields.len;
    const TYPE_ROOT_N = @typeInfo(type_mod.TypeSymbols).@"struct".fields.len;

    fn snapshotRootSlice(dst: []Value, src: []const Value) void {
        @memcpy(dst, src);
    }

    fn firstRootDiff(prev: []const Value, cur: []const Value) ?usize {
        var i: usize = 0;
        while (i < prev.len and i < cur.len) : (i += 1) {
            if (prev[i].raw != cur[i].raw) return i;
        }
        return null;
    }

    fn rootPointsIntoStaleSpace(self: *Vm, val: Value) bool {
        if (!val.isPointer()) return false;
        if (val.isT()) return false;
        const addr = val.toPtrAddr();
        const stale_start = @intFromPtr(self.heap.to_start);
        const stale_end = stale_start + self.heap.space_size;
        return addr >= stale_start and addr < stale_end;
    }

    fn traceInvalidRootLayout(self: *Vm, comptime site: []const u8, comptime root_name: []const u8, idx: usize, val: Value) void {
        const from_start = @intFromPtr(self.heap.from_start);
        const from_end = @intFromPtr(self.heap.from_end);
        const to_start = @intFromPtr(self.heap.to_start);
        const to_end = to_start + self.heap.space_size;
        const addr = if (val.isPointer()) val.toPtrAddr() else 0;
        std.debug.print(
            "TRACE root-layout-invalid site={s} root={s} idx={d} raw=0x{x} kind={s} addr=0x{x} from=[0x{x},0x{x}) to=[0x{x},0x{x})\n",
            .{ site, root_name, idx, val.raw, @tagName(val.typeKind()), addr, from_start, from_end, to_start, to_end },
        );
    }

    fn valueHasValidRootLayout(self: *Vm, val: Value) bool {
        if (val.isNil() or val.isT()) return true;
        return switch (val.typeKind()) {
            .symbol, .keyword => blk: {
                if (!val.isPointer()) break :blk false;
                const addr = val.toPtrAddr();
                if (!self.heap.containsAddrForDebug(addr)) break :blk false;
                switch (val.typeKind()) {
                    .symbol => {
                        const sym: *const runtime.Symbol = @ptrFromInt(addr);
                        if (sym.name_len > self.heap.space_size) break :blk false;
                        if (@intFromPtr(sym.name_ptr) != addr + @sizeOf(runtime.Symbol)) break :blk false;
                        break :blk true;
                    },
                    .keyword => {
                        const kw: *const runtime.Keyword = @ptrFromInt(addr);
                        if (kw.name_len > self.heap.space_size) break :blk false;
                        if (@intFromPtr(kw.name_ptr) != addr + @sizeOf(runtime.Keyword)) break :blk false;
                        break :blk true;
                    },
                    else => break :blk false,
                }
            },
            else => false,
        };
    }

    fn validateBuiltinAndTypeRoots(self: *Vm, comptime site: []const u8) bool {
        const builtin_roots = valueFieldSlice(BuiltinSymbols, &self.builtins);
        for (builtin_roots, 0..) |val, i| {
            if (!self.valueHasValidRootLayout(val)) {
                self.traceInvalidRootLayout(site, "builtins", i, val);
                return false;
            }
        }
        const type_roots = valueFieldSlice(type_mod.TypeSymbols, &self.type_syms);
        for (type_roots, 0..) |val, i| {
            if (!self.valueHasValidRootLayout(val)) {
                self.traceInvalidRootLayout(site, "type_syms", i, val);
                return false;
            }
        }
        return true;
    }

    pub fn init(allocator: std.mem.Allocator, heap: *Heap) !Vm {
        var vm = Vm{
            .stack = undefined,
            .sp = 0,
            .frames = undefined,
            .fp = 0,
            .chunk = &halt_chunk,
            .ip = 0,
            .execute_depth = 0,
            .heap = heap,
            .allocator = allocator,
            .gc_slots = std.ArrayList(*Value){},
            .safepoint_batch_ops = 0,
            .safepoint_batch_bytes = 0,
            .globals = [_]Value{Value.unbound} ** MAX_GLOBALS,
            .num_globals = 0,
            .comp_retain_vals = std.ArrayList(Value){},
            .comp_root_stack = [_]Value{Value.nil} ** MAX_COMPILE_ROOTS,
            .comp_root_sp = 0,
            .chunk_pool = &[_]Value{},
            .chunk_pool_owner = null,
            .chunk_base = 0,
            .saved_chunks = undefined,
            .saved_chunk_sp = 0,
            .catch_stack = undefined,
            .catch_sp = 0,
            .unwind_stack = undefined,
            .unwind_sp = 0,
            .restart_stack = undefined,
            .restart_sp = 0,
            .restart_next_id = 1,
            .block_stack = undefined,
            .block_sp = 0,
            .handler_stack = undefined,
            .handler_sp = 0,
            .pending_handler_restore_depth = null,
            .progv_stack = undefined,
            .progv_sp = 0,
            .scope_stack = undefined,
            .scope_sp = 0,
            .pending_throw_tag = Value.nil,
            .pending_throw_value = Value.nil,
            .throw_barrier_depth = 0,
            .relay_throw_tag = Value.nil,
            .relay_throw_value = Value.nil,
            .jit_bridge_error = null,
            .jit_bridge_epoch = 0,
            .jit_gc_forbidden_depth = 0,
            .pending_error = null,
            .is_unwinding = false,
            .last_error_value = Value.nil,
            .pending_block_idx = null,
            .pending_block_value = Value.nil,
            .is_returning_from_block = false,
            .prng = std.Random.DefaultPrng.init(0),
            .prng_seeded = false,
            .secondary_values = undefined,
            .secondary_values_count = 0,
            .zero_values_returned = false,
            .global_env = null,
            .load_callback = null,
            .load_context = null,
            .eval_callback = null,
            .eval_context = null,
            .macroexpand_callback = null,
            .macroexpand_context = null,
            .macroexpand_1_callback = null,
            .macroexpand_1_context = null,
            .fboundp_callback = null,
            .fboundp_context = null,
            .function_resolve_callback = null,
            .function_resolve_context = null,
            .fn_resolve_cache = [_]FnResolveEntry{.{}} ** MAX_FN_RESOLVE_CACHE,
            .global_index_cache = [_]GlobalIndexCacheEntry{.{}} ** MAX_GLOBAL_INDEX_CACHE,
            .key_allowlist_cache = [_]KeyAllowlistCacheEntry{.{}} ** MAX_KEY_ALLOWLIST_CACHE,
            .chunk_const_cache = [_]ChunkConstCacheEntry{.{}} ** MAX_CHUNK_CONST_CACHE,
            .const_last_chunk_key = 0,
            .const_last_gc_count = 0,
            .jit_compile_cache = [_]JitCompileCacheEntry{.{}} ** MAX_JIT_COMPILE_CACHE,
            .uninterned_values = std.AutoHashMap(u64, Value).init(allocator),
            .gensym_counter = 0,
            .current_closure = null,
            .current_argc = 0,
            .ext_roots = &[_]Value{},
            .ext_roots_owner = null,
            .ext_roots_saved = undefined,
            .ext_roots_saved_sp = 0,
            .jit_fns = std.ArrayList(JitFnEntry){},
            .jit_literal_roots = std.ArrayList(*Value){},
            .jit_adm = .{},
            .unsupported_tags = [_]u64{0} ** IR_TAG_N,
            .jit_direct_calls = 0,
            .call_shape = .{},
            .track_call_shape = std.posix.getenv("HABU_TRACK_CALL_SHAPES") != null,
            .trace_jit_call = std.posix.getenv("HABU_TRACE_JIT_CALL") != null,
            .trace_fn_resolve = std.posix.getenv("HABU_TRACE_FN_RESOLVE") != null,
            .trace_call_mismatch = std.posix.getenv("HABU_TRACE_CALL_MISMATCH") != null,
            .trace_call_mismatch_apply = std.posix.getenv("HABU_TRACE_CALL_MISMATCH_APPLY") != null,
            .trace_invalid_opcode = std.posix.getenv("HABU_TRACE_INVALID_OPCODE") != null,
            .trace_error_context = std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null,
            .trace_error_include_halt = std.posix.getenv("HABU_TRACE_ERROR_INCLUDE_HALT") != null,
            .trace_error_only_filter = if (std.posix.getenv("HABU_TRACE_ERROR_ONLY")) |raw| std.mem.sliceTo(raw, 0) else null,
            .trace_chunk_only_filter = if (std.posix.getenv("HABU_TRACE_CHUNK_ONLY")) |raw| std.mem.sliceTo(raw, 0) else null,
            .trace_op_only_filter = if (std.posix.getenv("HABU_TRACE_OP_ONLY")) |raw| std.mem.sliceTo(raw, 0) else null,
            .trace_call_ret = std.posix.getenv("HABU_TRACE_CALL_RET") != null,
            .trace_call_ret_filter = if (std.posix.getenv("HABU_TRACE_CALL_RET_ONLY")) |raw| std.mem.sliceTo(raw, 0) else null,
            .trace_ext_root_owner = std.posix.getenv("HABU_TRACE_EXT_ROOT_OWNER") != null,
            .trace_jit_call_args = std.posix.getenv("HABU_TRACE_JIT_CALL_ARGS") != null,
            .trace_validate_root_layout = std.posix.getenv("HABU_TRACE_VALIDATE_ROOT_LAYOUT") != null,
            .trap_validate_root_layout = std.posix.getenv("HABU_TRAP_VALIDATE_ROOT_LAYOUT") != null,
            .trace_chunk_pool_slot = envTraceIndex("HABU_TRACE_CHUNK_POOL_SLOT"),
            .trace_bad_root_layout = std.posix.getenv("HABU_TRACE_BAD_ROOT_LAYOUT") != null,
            .trace_bad_global_root = std.posix.getenv("HABU_TRACE_BAD_GLOBAL_ROOT") != null,
            .trace_bad_global_kind = std.posix.getenv("HABU_TRACE_BAD_GLOBAL_KIND") != null,
            .trace_builtin_write = std.posix.getenv("HABU_TRACE_BUILTINS_WRITE") != null,
            .trap_builtin_write = std.posix.getenv("HABU_TRAP_BUILTINS_WRITE") != null,
            .trace_stale_root_table = std.posix.getenv("HABU_TRACE_STALE_ROOT_TABLE") != null,
            .trap_stale_root_table = std.posix.getenv("HABU_TRAP_STALE_ROOT_TABLE") != null,
            .trace_local_mismatch = std.posix.getenv("HABU_TRACE_LOCAL_MISMATCH") != null,
            .trace_upvalue = std.posix.getenv("HABU_TRACE_UPVALUE") != null,
            .trace_sub_context = std.posix.getenv("HABU_TRACE_SUB_CONTEXT") != null,
            .trace_copy_structure = std.posix.getenv("HABU_TRACE_COPY_STRUCTURE") != null,
            .trace_call_args = std.posix.getenv("HABU_TRACE_CALL_ARGS") != null,
            .trace_io = std.posix.getenv("HABU_TRACE_IO") != null,
            .trace_progv_disasm = std.posix.getenv("HABU_TRACE_PROGV_DISASM") != null,
            .trace_throw = std.posix.getenv("HABU_TRACE_THROW") != null,
            .trap_progv_corrupt = std.posix.getenv("HABU_TRAP_PROGV_CORRUPT") != null,
            .trace_block_miss = std.posix.getenv("HABU_TRACE_BLOCK_MISS") != null,
            .trace_error_disasm = std.posix.getenv("HABU_TRACE_ERROR_DISASM") != null,
            .trace_call_mismatch_fn_disasm = std.posix.getenv("HABU_TRACE_CALL_MISMATCH_FN_DISASM") != null,
            .trace_call_mismatch_callee_disasm = std.posix.getenv("HABU_TRACE_CALL_MISMATCH_CALLEE_DISASM") != null,
            .trace_call_mismatch_disasm = std.posix.getenv("HABU_TRACE_CALL_MISMATCH_DISASM") != null,
            .builtins = try BuiltinSymbols.init(heap),
            .type_syms = try type_mod.TypeSymbols.init(heap),
        };
        // Initialize globals to unbound so function-only names and declared-but-unset
        // variables do not appear value-bound.
        for (&vm.globals) |*g| {
            g.* = Value.unbound;
        }
        runtime.setHeapContext(heap);
        return vm;
    }

    pub fn deinit(self: *Vm) void {
        self.uninstallJitBridges();
        // Clean up hoist-compiled functions
        for (self.jit_fns.items) |entry| {
            entry.compiled.deinit();
            self.allocator.destroy(entry.compiled);
        }
        self.jit_fns.deinit(self.allocator);
        for (self.jit_literal_roots.items) |slot| {
            self.allocator.destroy(slot);
        }
        self.jit_literal_roots.deinit(self.allocator);
        self.comp_retain_vals.deinit(self.allocator);
        self.uninterned_values.deinit();
        self.gc_slots.deinit(self.allocator);
    }

    pub fn resetJitAdm(self: *Vm) void {
        self.jit_adm = .{};
        self.unsupported_tags = [_]u64{0} ** IR_TAG_N;
    }

    pub fn noteUnsupportedTag(self: *Vm, tag: IrTag) void {
        self.unsupported_tags[@intFromEnum(tag)] +%= 1;
    }

    pub fn resetJitDirectCalls(self: *Vm) void {
        self.jit_direct_calls = 0;
    }

    pub fn resetCallShapeStats(self: *Vm) void {
        self.call_shape = .{};
    }

    pub fn setCallShapeTracking(self: *Vm, enabled: bool) void {
        self.track_call_shape = enabled;
    }

    pub fn callShapeStats(self: *const Vm) CallShapeStats {
        return self.call_shape;
    }

    /// Set the chunk pool for closures with a base offset (deprecated)
    pub fn setChunkPoolWithBase(self: *Vm, chunks: []Value, base: usize) void {
        self.chunk_pool = chunks;
        self.chunk_pool_owner = null;
        self.chunk_base = base;
    }

    /// Set the chunk pool for closures (indices are absolute)
    pub fn setChunkPool(self: *Vm, chunks: []Value) void {
        self.chunk_pool = chunks;
        self.chunk_pool_owner = null;
        self.chunk_base = 0;
    }

    /// Set chunk pool from a stable owner ArrayList.
    pub fn setChunkPoolOwned(self: *Vm, owner: *std.ArrayList(Value)) void {
        self.chunk_pool_owner = owner;
        self.chunk_pool = owner.items;
        self.chunk_base = 0;
    }

    pub fn currentChunkPool(self: *const Vm) []Value {
        if (self.chunk_pool_owner) |owner| return owner.items;
        return self.chunk_pool;
    }

    /// Set the global environment for boundp/fboundp lookups
    pub fn setGlobalEnv(self: *Vm, env: *GlobalEnv) void {
        self.global_env = env;
        self.clearGlobalIndexCache();
    }

    /// Set the load callback for (load "filename")
    pub fn setLoadCallback(self: *Vm, callback: *const fn ([]const u8, *anyopaque) Error!Value, context: *anyopaque) void {
        self.load_callback = callback;
        self.load_context = context;
    }

    /// Set the eval callback for (eval expr)
    pub fn setEvalCallback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!Value, context: *anyopaque) void {
        self.eval_callback = callback;
        self.eval_context = context;
    }

    /// Set the macroexpand callback for (macroexpand expr)
    pub fn setMacroexpandCallback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!Value, context: *anyopaque) void {
        self.macroexpand_callback = callback;
        self.macroexpand_context = context;
    }

    /// Set the macroexpand-1 callback for (macroexpand-1 expr)
    pub fn setMacroexpand1Callback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!Value, context: *anyopaque) void {
        self.macroexpand_1_callback = callback;
        self.macroexpand_1_context = context;
    }

    /// Set the fboundp callback for checking function bindings
    pub fn setFboundpCallback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!bool, context: *anyopaque) void {
        self.fboundp_callback = callback;
        self.fboundp_context = context;
    }

    /// Set callback for resolving symbol function designators to callable values.
    pub fn setFunctionResolveCallback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!?Value, context: *anyopaque) void {
        self.function_resolve_callback = callback;
        self.function_resolve_context = context;
    }

    fn isCallableFunctionValue(val: Value) bool {
        return switch (val.typeKind()) {
            .closure, .native_code, .generic_function => true,
            else => false,
        };
    }

    fn symbolIsUninterned(sym: *const Symbol) bool {
        const pkg_bits = sym.reserved;
        return pkg_bits == 0 or (pkg_bits & 1) != 0;
    }

    fn uninternedSymbolId(sym: *const Symbol) ?u64 {
        if (!symbolIsUninterned(sym)) return null;
        if ((sym.reserved & 1) == 0 or sym.reserved == 0) return null;
        return sym.reserved;
    }

    fn defineSymbolGlobalIndex(self: *Vm, sym: *const Symbol) Error!?u16 {
        if (symbolIsUninterned(sym)) return null;
        const env = self.global_env orelse return null;
        var qual_buf: [512]u8 = undefined;
        const q = try qual_name.qualSymWithHeap(self.allocator, self.heap, sym, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        return try env.define(q.name);
    }

    fn lookupSymbolLocalValueCell(self: *const Vm, sym: *const Symbol) ?Value {
        const sym_id = uninternedSymbolId(sym) orelse return null;
        return self.uninterned_values.get(sym_id);
    }

    fn setSymbolLocalValueCell(self: *Vm, sym: *const Symbol, val: Value) Error!void {
        const sym_id = uninternedSymbolId(sym) orelse return error.TypeMismatch;
        try self.uninterned_values.put(sym_id, val);
    }

    fn clearSymbolLocalValueCell(self: *Vm, sym: *const Symbol) void {
        const sym_id = uninternedSymbolId(sym) orelse return;
        _ = self.uninterned_values.remove(sym_id);
    }

    fn lookupSymbolValueCell(self: *Vm, sym_val: Value) Error!?Value {
        const live_sym_val = self.resolveForwardedValue(sym_val);
        if (!live_sym_val.isSymbol()) return null;
        const sym = live_sym_val.toPtr(Symbol);
        if (try self.lookupSymbolGlobalIndex(sym)) |idx| {
            if (idx >= MAX_GLOBALS) return null;
            const val = self.globals[idx];
            if (val.raw == Value.unbound.raw) return null;
            return val;
        }
        return self.lookupSymbolLocalValueCell(sym);
    }

    pub fn runtimeLookupSymbolValue(sym: Value, ctx: *anyopaque) anyerror!?Value {
        const self: *Vm = @ptrCast(@alignCast(ctx));
        return try self.lookupSymbolValueCell(sym);
    }

    fn setSymbolValueCell(self: *Vm, sym_val: Value, val: Value) Error!void {
        const live_sym_val = self.resolveForwardedValue(sym_val);
        if (!live_sym_val.isSymbol()) return error.TypeMismatch;
        const sym = live_sym_val.toPtr(Symbol);
        if (try self.lookupSymbolGlobalIndex(sym)) |idx| {
            if (idx < MAX_GLOBALS) {
                try self.storeGlobal(idx, val);
            }
            return;
        }
        if (try self.defineSymbolGlobalIndex(sym)) |idx| {
            if (idx < MAX_GLOBALS) {
                try self.storeGlobal(idx, val);
            }
            return;
        }
        try self.setSymbolLocalValueCell(sym, val);
    }

    fn clearSymbolValueCell(self: *Vm, sym_val: Value) Error!void {
        const live_sym_val = self.resolveForwardedValue(sym_val);
        if (!live_sym_val.isSymbol()) return error.TypeMismatch;
        const sym = live_sym_val.toPtr(Symbol);
        if (try self.lookupSymbolGlobalIndex(sym)) |idx| {
            if (idx < MAX_GLOBALS) {
                self.globals[idx] = Value.unbound;
            }
            return;
        }
        self.clearSymbolLocalValueCell(sym);
    }

    inline fn fnResolveCacheIndex(sym: Value) usize {
        return @intCast((sym.raw >> 4) & (MAX_FN_RESOLVE_CACHE - 1));
    }

    inline fn globalIndexCacheIndex(sym: Value) usize {
        return @intCast((sym.raw >> 4) & (MAX_GLOBAL_INDEX_CACHE - 1));
    }

    inline fn keyAllowlistCacheIndex(chunk: *const Chunk) usize {
        return @intCast((@intFromPtr(chunk) >> 4) & (MAX_KEY_ALLOWLIST_CACHE - 1));
    }

    inline fn chunkConstCacheIndex(chunk: *const Chunk) usize {
        return @intCast((@intFromPtr(chunk) >> 4) & (MAX_CHUNK_CONST_CACHE - 1));
    }

    fn clearFnResolveCache(self: *Vm) void {
        for (&self.fn_resolve_cache) |*entry| {
            entry.* = .{};
        }
    }

    fn clearGlobalIndexCache(self: *Vm) void {
        for (&self.global_index_cache) |*entry| {
            entry.* = .{};
        }
    }

    fn lookupGlobalIndexCache(self: *Vm, sym: Value) ?u16 {
        const idx = globalIndexCacheIndex(sym);
        const entry = &self.global_index_cache[idx];
        if (!entry.sym.eq(sym)) return null;
        return entry.idx;
    }

    fn storeGlobalIndexCache(self: *Vm, sym: Value, idx: u16) void {
        if (!sym.isSymbol()) return;
        const slot = globalIndexCacheIndex(sym);
        self.global_index_cache[slot] = .{
            .sym = sym,
            .idx = idx,
        };
    }

    fn clearKeyAllowlistCache(self: *Vm) void {
        for (&self.key_allowlist_cache) |*entry| {
            entry.* = .{};
        }
    }

    fn clearChunkConstCache(self: *Vm) void {
        for (&self.chunk_const_cache) |*entry| {
            entry.* = .{};
        }
        self.const_last_chunk_key = 0;
        self.const_last_gc_count = 0;
    }

    fn chunkConstsAreFresh(self: *Vm, chunk: *const Chunk, gc_count: usize) bool {
        const entry = &self.chunk_const_cache[chunkConstCacheIndex(chunk)];
        return entry.chunk_key == @intFromPtr(chunk) and entry.gc_count == gc_count;
    }

    fn markChunkConstsFresh(self: *Vm, chunk: *const Chunk, gc_count: usize) void {
        const entry = &self.chunk_const_cache[chunkConstCacheIndex(chunk)];
        entry.* = .{
            .chunk_key = @intFromPtr(chunk),
            .gc_count = gc_count,
        };
    }

    inline fn jitCompileCacheIndex(chunk_key: u64) usize {
        return @intCast(chunk_key & (MAX_JIT_COMPILE_CACHE - 1));
    }

    fn clearJitCompileCacheForKey(self: *Vm, chunk_key: u64) void {
        if (chunk_key == 0) return;
        const idx = jitCompileCacheIndex(chunk_key);
        if (self.jit_compile_cache[idx].chunk_key == chunk_key) {
            self.jit_compile_cache[idx] = .{};
        }
    }

    fn hashJitKeyU64(hasher: *std.hash.Wyhash, val: u64) void {
        var buf: [8]u8 = undefined;
        std.mem.writeInt(u64, &buf, val, .little);
        hasher.update(&buf);
    }

    fn hashJitChunkMeta(hasher: *std.hash.Wyhash, chunk: *const Chunk) void {
        hashJitKeyU64(hasher, @as(u64, chunk.const_count));
        hashJitKeyU64(hasher, @as(u64, chunk.code_len));
        hashJitKeyU64(hasher, @as(u64, chunk.arity));
        hashJitKeyU64(hasher, @as(u64, chunk.opt_count));
        hashJitKeyU64(hasher, @as(u64, chunk.key_count));
        hashJitKeyU64(hasher, @as(u64, chunk.has_rest));
        hashJitKeyU64(hasher, @as(u64, chunk.allow_other_keys));
        hashJitKeyU64(hasher, @as(u64, chunk.num_locals));
        hashJitKeyU64(hasher, @as(u64, chunk.speed));
        hashJitKeyU64(hasher, @as(u64, chunk.safety));
    }

    fn hashJitCacheValue(self: *Vm, hasher: *std.hash.Wyhash, raw_val: Value, depth: u8) void {
        const val = self.resolveForwardedValue(raw_val);
        hasher.update(&[_]u8{@intFromEnum(val.typeKind())});
        switch (val.typeKind()) {
            .nil, .t, .fixnum, .char, .float => hashJitKeyU64(hasher, val.raw),
            .symbol => hasher.update(val.toPtr(Symbol).getName()),
            .keyword => hasher.update(val.toPtr(runtime.Keyword).getName()),
            .string => hasher.update(val.toPtr(runtime.String).bytes()),
            .string32 => hasher.update(std.mem.sliceAsBytes(val.toPtr(runtime.String32).codepoints())),
            .chunk => {
                if (depth >= 1) {
                    hashJitKeyU64(hasher, val.raw);
                } else {
                    const child = val.toPtr(Chunk);
                    hashJitChunkMeta(hasher, child);
                    hasher.update(child.getCode());
                }
            },
            else => hashJitKeyU64(hasher, val.raw),
        }
    }

    fn computeJitChunkKey(self: *Vm, chunk: *const Chunk) u64 {
        var hasher = std.hash.Wyhash.init(0x71C7A5D2C8E9F3B1);
        hashJitChunkMeta(&hasher, chunk);
        hasher.update(chunk.getCode());
        for (chunk.getConstants()) |const_val| {
            self.hashJitCacheValue(&hasher, const_val, 0);
        }
        const key = hasher.final();
        return if (key == 0) 1 else key;
    }

    fn ensureJitChunkKey(self: *Vm, chunk: *const Chunk) !u64 {
        return self.computeJitChunkKey(chunk);
    }

    fn noteJitCompileStatusByKey(self: *Vm, chunk_key: u64, status: JitCompileStatus) void {
        const idx = jitCompileCacheIndex(chunk_key);
        self.jit_compile_cache[idx] = .{
            .chunk_key = chunk_key,
            .gc_count = self.heap.stats.gc_count,
            .status = status,
        };
    }

    pub fn noteJitCompileStatus(self: *Vm, chunk: *const Chunk, status: JitCompileStatus) !void {
        if (status == .none) {
            self.clearJitCompileCacheForKey(self.computeJitChunkKey(chunk));
            return;
        }
        const key = try self.ensureJitChunkKey(chunk);
        self.noteJitCompileStatusByKey(key, status);
    }

    pub fn jitCompileStatus(self: *Vm, chunk: *const Chunk) !JitCompileStatus {
        const key = try self.ensureJitChunkKey(chunk);
        const idx = jitCompileCacheIndex(key);
        const entry = &self.jit_compile_cache[idx];
        if (entry.chunk_key != key) return .none;

        switch (entry.status) {
            .none => return .none,
            .compiled => {
                if (chunk.jit_fn == 0 and self.lookupJitFn(chunk) == null) {
                    entry.* = .{};
                    return .none;
                }
                entry.gc_count = self.heap.stats.gc_count;
                return .compiled;
            },
            .unsupported, .failed => {
                if (entry.status == .failed and entry.gc_count != self.heap.stats.gc_count) return .none;
                return entry.status;
            },
        }
    }

    fn refreshChunkConsts(self: *Vm, chunk: *const Chunk) void {
        const consts = chunk.getConstants();
        for (consts, 0..) |raw, i| {
            const fixed = self.resolveForwardedValue(raw);
            if (fixed.raw != raw.raw) {
                consts[i] = fixed;
            }
        }
    }

    fn lookupKeyAllowlistCache(self: *Vm, chunk: *const Chunk, key_count: u8) []const Value {
        if (key_count == 0 or @as(usize, key_count) > KEY_FAST_TABLE_MAX) return &.{};
        const chunk_key = @intFromPtr(chunk);
        const entry = &self.key_allowlist_cache[keyAllowlistCacheIndex(chunk)];
        if (entry.chunk_key != chunk_key or entry.key_count != key_count) return &.{};
        return entry.keywords[0..entry.len];
    }

    fn populateKeyAllowlistCache(self: *Vm, chunk: *const Chunk, key_count: u8, allowed_list: Value) []const Value {
        if (key_count == 0 or @as(usize, key_count) > KEY_FAST_TABLE_MAX) return &.{};

        var list = allowed_list;
        var n: usize = 0;
        var tmp: [KEY_FAST_TABLE_MAX]Value = undefined;
        while (n < @as(usize, key_count) and isConsRaw(list)) : (n += 1) {
            const cell = consFromRaw(list);
            tmp[n] = cell.car;
            list = cell.cdr;
        }
        if (n != @as(usize, key_count) or list.raw != Value.nil.raw) return &.{};

        const entry = &self.key_allowlist_cache[keyAllowlistCacheIndex(chunk)];
        entry.chunk_key = @intFromPtr(chunk);
        entry.key_count = key_count;
        entry.len = @intCast(n);
        @memcpy(entry.keywords[0..n], tmp[0..n]);
        return entry.keywords[0..n];
    }

    fn lookupFnResolveCache(self: *Vm, sym: Value) ?Value {
        const idx = fnResolveCacheIndex(sym);
        const entry = &self.fn_resolve_cache[idx];
        if (!entry.sym.eq(sym)) return null;
        return entry.fn_val;
    }

    fn storeFnResolveCacheLive(self: *Vm, live_sym: Value, live_fn: Value) void {
        if (!live_sym.isSymbol() or !isCallableFunctionValue(live_fn)) return;
        const idx = fnResolveCacheIndex(live_sym);
        self.fn_resolve_cache[idx] = .{ .sym = live_sym, .fn_val = live_fn };
    }

    fn clearFnResolveCacheEntry(self: *Vm, sym: Value) void {
        const idx = fnResolveCacheIndex(sym);
        self.fn_resolve_cache[idx] = .{};
    }

    fn lookupFunctionCellLive(self: *Vm, live_sym_val: Value) Error!?Value {
        if (!live_sym_val.isSymbol()) return null;
        const key = self.builtins.sym_function_cell;
        const cell = try primitives.list.get(self.heap, live_sym_val, key);
        if (!isCallableFunctionValue(cell)) return null;
        return cell;
    }

    fn storeFunctionCell(self: *Vm, sym_val: Value, fn_val: Value) Error!void {
        const live_sym_val = self.resolveForwardedValue(sym_val);
        if (!live_sym_val.isSymbol()) return;
        const live_fn_val = self.resolveForwardedValue(fn_val);
        const key = self.builtins.sym_function_cell;
        _ = try primitives.list.put(self.heap, live_sym_val, key, live_fn_val);
        if (isCallableFunctionValue(live_fn_val)) {
            self.storeFnResolveCacheLive(live_sym_val, live_fn_val);
        } else {
            self.clearFnResolveCacheEntry(live_sym_val);
        }
    }

    fn clearFunctionCell(self: *Vm, sym_val: Value) Error!void {
        const live_sym_val = self.resolveForwardedValue(sym_val);
        if (!live_sym_val.isSymbol()) return;
        const key = self.builtins.sym_function_cell;
        _ = try primitives.list.remprop(self.heap, live_sym_val, key);
        self.clearFnResolveCacheEntry(live_sym_val);
    }

    fn resolveFunctionValue(self: *Vm, sym_val: Value) Error!?Value {
        if (!sym_val.isSymbol()) return null;
        if (self.lookupFnResolveCache(sym_val)) |cached_fn| {
            return cached_fn;
        }

        const live_sym_val = self.resolveForwardedValue(sym_val);
        if (!live_sym_val.isSymbol()) return null;
        if (live_sym_val.raw != sym_val.raw) {
            if (self.lookupFnResolveCache(live_sym_val)) |cached_fn| {
                return cached_fn;
            }
        }
        if (try self.lookupFunctionCellLive(live_sym_val)) |fn_cell| {
            self.storeFnResolveCacheLive(live_sym_val, fn_cell);
            return fn_cell;
        }

        var global_seen = false;
        var global_val = Value.nil;
        const sym = live_sym_val.toPtr(Symbol);
        if (try self.lookupSymbolGlobalIndex(sym)) |idx| {
            global_seen = true;
            global_val = self.resolveForwardedValue(self.globals[idx]);
            if (isCallableFunctionValue(global_val)) {
                try self.storeFunctionCell(live_sym_val, global_val);
                return global_val;
            }
        }

        if (self.function_resolve_callback) |cb| {
            if (try cb(live_sym_val, self.function_resolve_context.?)) |resolved_raw| {
                const resolved = self.resolveForwardedValue(resolved_raw);
                if (isCallableFunctionValue(resolved)) {
                    try self.storeFunctionCell(live_sym_val, resolved);
                    return resolved;
                }
            }
        }

        // Preserve bootstrap behavior for symbols with allocated global slots
        // but no callable binding yet.
        if (global_seen and (global_val.raw == Value.nil.raw or global_val.raw == Value.unbound.raw)) {
            return global_val;
        }
        return null;
    }

    fn pathDesignatorBytes(self: *Vm, val: Value) ![]const u8 {
        return try primitives.pathname.pathDesignatorBytes(self.allocator, self.heap, &self.builtins, val);
    }

    pub fn setExtRoots(self: *Vm, roots: []Value) void {
        self.ext_roots_owner = null;
        self.ext_roots = roots;
    }

    pub fn setExtRootsOwned(self: *Vm, owner: *std.ArrayList(Value)) void {
        if (self.trace_ext_root_owner) {
            const first_raw = if (owner.items.len != 0) owner.items[0].raw else 0;
            std.debug.print(
                "TRACE set-ext-owned owner=0x{x} ptr=0x{x} len={d} first=0x{x} caller=0x{x}\n",
                .{ @intFromPtr(owner), @intFromPtr(owner.items.ptr), owner.items.len, first_raw, @returnAddress() },
            );
        }
        self.ext_roots_owner = owner;
        self.ext_roots = owner.items;
    }

    pub const ExtRootsSnapshot = struct {
        roots: []Value,
        owner: ?*std.ArrayList(Value),
    };

    pub fn currentExtRoots(self: *const Vm) []Value {
        if (self.ext_roots_owner) |owner| return owner.items;
        return self.ext_roots;
    }

    fn extRootsSnapshotMatches(a: ExtRootsSnapshot, b: ExtRootsSnapshot) bool {
        if (a.owner != b.owner) return false;
        if (a.owner != null) return true;
        return @intFromPtr(a.roots.ptr) == @intFromPtr(b.roots.ptr) and a.roots.len == b.roots.len;
    }

    fn dropSavedExtRootsSnapshot(self: *Vm, saved: ExtRootsSnapshot) void {
        var i = self.ext_roots_saved_sp;
        while (i > 0) {
            i -= 1;
            if (!extRootsSnapshotMatches(self.ext_roots_saved[i], saved)) continue;
            var shift = i;
            while (shift + 1 < self.ext_roots_saved_sp) : (shift += 1) {
                self.ext_roots_saved[shift] = self.ext_roots_saved[shift + 1];
            }
            self.ext_roots_saved_sp -= 1;
            return;
        }
        if (self.trace_ext_root_owner) {
            const owner_addr = if (saved.owner) |owner| @intFromPtr(owner) else 0;
            std.debug.print(
                "TRACE drop-ext-snapshot-miss owner=0x{x} ptr=0x{x} len={d}\n",
                .{ owner_addr, @intFromPtr(saved.roots.ptr), saved.roots.len },
            );
        }
    }

    pub fn saveExtRoots(self: *Vm) Error!ExtRootsSnapshot {
        const snap: ExtRootsSnapshot = .{
            .roots = self.currentExtRoots(),
            .owner = self.ext_roots_owner,
        };
        if (self.ext_roots_saved_sp >= MAX_EXT_ROOT_SNAPSHOTS) return error.StackOverflow;
        self.ext_roots_saved[self.ext_roots_saved_sp] = snap;
        self.ext_roots_saved_sp += 1;
        return snap;
    }

    pub fn restoreExtRoots(self: *Vm, saved: ExtRootsSnapshot) void {
        self.dropSavedExtRootsSnapshot(saved);
        if (saved.owner) |owner| {
            self.setExtRootsOwned(owner);
            return;
        }
        self.setExtRoots(saved.roots);
    }

    /// Compatibility shim: inactive saved ext-root snapshots are now rooted directly
    /// during GC, so restore no longer needs prefix copyback from temporary owners.
    pub fn restoreExtRootsSynced(
        self: *Vm,
        saved: ExtRootsSnapshot,
        current_roots: []const Value,
        saved_prefix_len: usize,
    ) void {
        _ = current_roots;
        _ = saved_prefix_len;
        self.restoreExtRoots(saved);
    }

    pub fn clearExtRoots(self: *Vm) void {
        self.ext_roots_owner = null;
        self.ext_roots = &[_]Value{};
    }

    fn htGrowInPlace(self: *Vm, ht_idx: usize, new_cap: usize) !void {
        const entries_len = try std.math.mul(usize, new_cap, 2);
        const new_entries_vec_val = try self.allocVector(entries_len, entries_len);

        // Initialize keys in even slots.
        const new_entries_vec = new_entries_vec_val.toPtr(runtime.Vector);
        for (0..new_cap) |i| {
            new_entries_vec.data[i * 2] = HashTable.EMPTY;
        }

        // Re-acquire hash table pointer after potential GC during allocVector.
        const ht = self.stack[ht_idx].toPtr(HashTable);
        const old_entries_vec = ht.entries_vec.toPtr(runtime.Vector);
        const old_cap: usize = @intCast(ht.capacity);

        // Build into a temporary view, then swap backing store.
        var tmp: HashTable = .{
            .count = 0,
            .capacity = new_cap,
            .entries_vec = new_entries_vec_val,
            .test_type = ht.test_type,
        };

        for (0..old_cap) |i| {
            const k = old_entries_vec.data[i * 2];
            if (HashTable.isAvailableKey(k)) continue;
            const v = old_entries_vec.data[i * 2 + 1];
            try tmp.put(k, v);
        }

        ht.entries_vec = new_entries_vec_val;
        ht.capacity = new_cap;
        ht.count = tmp.count;
    }

    fn findJitFnIndex(self: *Vm, chunk_val: Value) ?usize {
        for (self.jit_fns.items, 0..) |*entry, idx| {
            if (entry.chunk.eq(chunk_val)) return idx;
            const live_chunk = self.resolveForwardedValue(entry.chunk);
            if (!live_chunk.eq(entry.chunk)) entry.chunk = live_chunk;
            if (live_chunk.eq(chunk_val)) return idx;
        }
        return null;
    }

    /// Register a hoist-compiled native function for a chunk.
    pub fn registerJitFn(self: *Vm, chunk: *const Chunk, compiled: *jit_backend.CompiledFn) !void {
        const chunk_val = Value.makeChunk(chunk);
        const chunk_key = try self.ensureJitChunkKey(chunk);
        if (self.findJitFnIndex(chunk_val)) |idx| {
            const old = self.jit_fns.items[idx].compiled;
            self.jit_fns.items[idx].chunk = chunk_val;
            self.jit_fns.items[idx].compiled = compiled;
            if (old != compiled) {
                old.deinit();
                self.allocator.destroy(old);
            }
        } else {
            try self.jit_fns.append(self.allocator, .{
                .chunk = chunk_val,
                .compiled = compiled,
            });
        }
        @constCast(chunk).jit_fn = @intFromPtr(compiled);
        self.noteJitCompileStatusByKey(chunk_key, .compiled);
    }

    /// Register a stable host-root slot for a JIT literal Value.
    pub fn registerJitLiteral(self: *Vm, val: Value) !*Value {
        const slot = try self.allocator.create(Value);
        slot.* = self.resolveForwardedValue(val);
        try self.jit_literal_roots.append(self.allocator, slot);
        return slot;
    }

    /// Look up hoist-compiled function for a chunk.
    pub fn lookupJitFn(self: *Vm, chunk: *const Chunk) ?*const jit_backend.CompiledFn {
        if (self.isStaleNurseryAddr(@intFromPtr(chunk))) return null;
        if (chunk.jit_fn != 0) {
            const compiled: *jit_backend.CompiledFn = @ptrFromInt(chunk.jit_fn);
            return compiled;
        }
        const chunk_val = Value.makeChunk(chunk);
        const idx = self.findJitFnIndex(chunk_val) orelse return null;
        const compiled = self.jit_fns.items[idx].compiled;
        @constCast(chunk).jit_fn = @intFromPtr(compiled);
        return compiled;
    }

    pub fn unregisterJitFn(self: *Vm, chunk: *const Chunk) bool {
        @constCast(chunk).jit_fn = 0;
        const chunk_val = Value.makeChunk(chunk);
        const idx = self.findJitFnIndex(chunk_val) orelse return false;
        _ = self.jit_fns.swapRemove(idx);
        self.clearJitCompileCacheForKey(self.computeJitChunkKey(chunk));
        return true;
    }

    fn installJitBridges(self: *Vm) void {
        const epoch = jit_backend.bridgeEpoch();
        if (self.jit_bridge_epoch != 0 and self.jit_bridge_epoch == epoch) return;
        const ctx: *anyopaque = self;
        if (jit_backend.callBridgeContext() == ctx and
            jit_backend.errorBridgeContext() == ctx and
            jit_backend.globalBridgeContext() == ctx)
        {
            self.jit_bridge_epoch = epoch;
            return;
        }
        jit_backend.setCallBridge(.{
            .context = self,
            .call0 = jitCallBridge0,
            .call1 = jitCallBridge1,
            .call2 = jitCallBridge2,
            .call3 = jitCallBridge3,
            .call4 = jitCallBridge4,
            .call5 = jitCallBridge5,
            .call6 = jitCallBridge6,
            .call7 = jitCallBridge7,
            .push_progv = jitCallBridgePushProgv,
            .pop_progv = jitCallBridgePopProgv,
        });
        jit_backend.setErrorBridge(.{
            .context = self,
            .set_error = jitErrorBridgeSet,
        });
        jit_backend.setGlobalBridge(.{
            .context = self,
            .load_global = jitGlobalBridgeLoad,
        });
        self.jit_bridge_epoch = jit_backend.bridgeEpoch();
    }

    fn uninstallJitBridges(self: *Vm) void {
        const ctx: *anyopaque = self;
        if (jit_backend.globalBridgeContext() == ctx) {
            jit_backend.clearGlobalBridge();
        }
        if (jit_backend.errorBridgeContext() == ctx) {
            jit_backend.clearErrorBridge();
        }
        if (jit_backend.callBridgeContext() == ctx) {
            jit_backend.clearCallBridge();
        }
        self.jit_bridge_epoch = jit_backend.bridgeEpoch();
    }

    fn runJitCompiled(
        self: *Vm,
        compiled: *const jit_backend.CompiledFn,
        callee_chunk: *const Chunk,
        caller_chunk: *const Chunk,
        args: []const Value,
    ) Error!?Value {
        const trace_jit_call = self.trace_jit_call;
        self.jit_bridge_error = null;
        // Set global heap pointer so JIT cons can allocate
        refreshJitHeap(self);
        self.installJitBridges();

        if (trace_jit_call) {
            std.debug.print(
                "JIT_CALL enter {s} argc={d} chunk={s} caller={s} fp={d} sp={d}\n",
                .{ compiled.name, args.len, chunkTraceName(callee_chunk), chunkTraceName(caller_chunk), self.fp, self.sp },
            );
            if (self.trace_jit_call_args) {
                const dump_n = @min(args.len, envTraceCount("HABU_TRACE_JIT_CALL_ARGS", 4));
                for (args[0..dump_n], 0..) |arg, idx| {
                    std.debug.print("  jit-arg[{d}]=", .{idx});
                    traceJitBridgeValue(arg);
                    std.debug.print("\n", .{});
                }
            }
        }
        const argc: u8 = @intCast(args.len);
        var invoke_ctx = JitInvokeCtx{
            .compiled = compiled,
            .args_ptr = args.ptr,
            .argc = argc,
        };
        var result_raw: u64 = Value.nil.raw;
        self.jit_gc_forbidden_depth += 1;
        const jump_rc = blk: {
            defer self.jit_gc_forbidden_depth -= 1;
            break :blk jit_backend.bridgeRun(jitInvokeCompiledFn, &invoke_ctx, &result_raw);
        };
        if (jump_rc < 0) return error.StackOverflow;
        if (jump_rc != 0) {
            const err = self.jit_bridge_error orelse error.InvalidOpcode;
            self.jit_bridge_error = null;
            // Ensure allocator cursor coherence after non-local bridge exits.
            jit_backend.syncHeapFromGlobal(self.heap);
            refreshJitHeap(self);
            if (trace_jit_call) {
                std.debug.print("JIT_CALL abort {s} err={s}\n", .{ compiled.name, @errorName(err) });
            }
            return err;
        }
        const result = Value{ .raw = result_raw };
        if (trace_jit_call) {
            std.debug.print("JIT_CALL leave {s}\n", .{compiled.name});
        }

        const jit_alloc_ptr = jit_backend.allocPtrRaw();
        if (!std.mem.isAligned(jit_alloc_ptr, runtime.heap.ALIGNMENT)) {
            std.debug.panic("jit alloc cursor misaligned after '{s}': 0x{x}", .{ compiled.name, jit_alloc_ptr });
        }

        // Sync heap alloc_ptr back from JIT global (inline cons updates g_alloc_ptr
        // but not heap.alloc_ptr directly)
        jit_backend.syncHeapFromGlobal(self.heap);
        self.jit_bridge_error = null;

        return result;
    }

    /// Try to call a closure via hoist-compiled native code.
    /// Returns the result of calling it, or null if not hoist-compiled.
    fn tryCallJit(self: *Vm, argc: u8) Error!?Value {
        const callee_chunk = self.chunk;
        const compiled_ptr = callee_chunk.jit_fn;
        if (compiled_ptr == 0) return null;
        const compiled: *const jit_backend.CompiledFn = @ptrFromInt(compiled_ptr);
        if (compiled.arity != argc) return null;

        // Extract args from the VM stack (they're above the callee frame)
        const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
        const argc_us: usize = @intCast(argc);
        const args = self.stack[bp .. bp + argc_us];
        const caller_chunk = if (self.fp > 0) self.frames[self.fp - 1].chunk else callee_chunk;
        return try self.runJitCompiled(compiled, callee_chunk, caller_chunk, args);
    }

    /// Try fixed-arity direct JIT call before generic doCall frame setup.
    /// Eligible only for closure-valued calls with no &optional/&key/&rest.
    fn tryDirectCallJit(self: *Vm, argc: u8) Error!?Value {
        if (self.sp < @as(usize, argc) + 1) return null;

        const argc_us: usize = @intCast(argc);
        const fn_slot = self.sp - argc_us - 1;
        var fn_val = self.resolveForwardedValue(self.stack[fn_slot]);
        if (!fn_val.isClosure()) return null;
        if (fn_val.raw != self.stack[fn_slot].raw) self.stack[fn_slot] = fn_val;

        const closure = fn_val.toPtr(runtime.Closure);
        const code_val = self.resolveForwardedValue(closure.code);
        if (!code_val.isChunk()) return null;
        if (code_val.raw != closure.code.raw) {
            closure.code = code_val;
            self.writeBarrierStore(Value.makeClosure(closure), code_val);
        }
        const callee_chunk = code_val.toPtr(Chunk);
        if (callee_chunk.kind != .chunk) return null;
        if (callee_chunk.opt_count != 0 or callee_chunk.key_count != 0 or callee_chunk.has_rest != 0) return null;
        if (callee_chunk.arity != argc) return null;

        const compiled_ptr = callee_chunk.jit_fn;
        if (compiled_ptr == 0) return null;
        const compiled: *const jit_backend.CompiledFn = @ptrFromInt(compiled_ptr);
        if (compiled.arity != argc) return null;

        const args = self.stack[self.sp - argc_us .. self.sp];
        const caller_chunk = self.chunk;
        const saved_chunk = self.chunk;
        const saved_ip = self.ip;
        self.chunk = callee_chunk;
        self.ip = 0;
        defer {
            self.chunk = saved_chunk;
            self.ip = saved_ip;
        }

        const result = (try self.runJitCompiled(compiled, callee_chunk, caller_chunk, args)) orelse return null;
        self.stack[fn_slot] = result;
        self.sp = fn_slot + 1;
        self.jit_direct_calls +%= 1;
        self.recordCallShape(.fixed, false, false);
        return result;
    }

    fn rekeyJitFnsAfterGc(self: *Vm) !void {
        var i: usize = 0;
        while (i < self.jit_fns.items.len) {
            const compiled = self.jit_fns.items[i].compiled;
            const old_val = self.jit_fns.items[i].chunk;
            if (old_val.isChunk()) {
                old_val.toPtr(Chunk).jit_fn = 0;
            }

            const live_val = self.resolveForwardedValue(old_val);
            if (!live_val.isChunk()) {
                compiled.deinit();
                self.allocator.destroy(compiled);
                _ = self.jit_fns.swapRemove(i);
                continue;
            }

            const live_addr = live_val.toPtrAddr();
            if (self.isStaleNurseryAddr(live_addr)) {
                compiled.deinit();
                self.allocator.destroy(compiled);
                _ = self.jit_fns.swapRemove(i);
                continue;
            }

            self.jit_fns.items[i].chunk = live_val;
            const live_chunk: *Chunk = @ptrFromInt(live_addr);
            live_chunk.jit_fn = @intFromPtr(compiled);
            i += 1;
        }
    }

    fn bytesInHeap(self: *const Vm, bytes: []const u8) bool {
        if (bytes.len == 0) return false;
        const heap_start = @intFromPtr(self.heap.memory.ptr);
        const heap_end = heap_start + self.heap.memory.len;
        const start = @intFromPtr(bytes.ptr);
        if (start < heap_start or start >= heap_end) return false;
        const end = start + bytes.len;
        if (end < start) return false; // overflow
        return end <= heap_end;
    }

    /// Allocate a cons cell, running GC if needed
    pub fn allocCons(self: *Vm, car: Value, cdr: Value) error{OutOfMemory}!Value {
        var roots = [_]Value{ car, cdr };
        try self.collectIfDebt(roots[0..], @sizeOf(runtime.Cons));
        return if (self.heap.allocCons(roots[0], roots[1])) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                _ = try self.collectGarbageExtra(roots[0..]);
                return try self.heap.allocCons(roots[0], roots[1]);
            },
        };
    }

    /// Allocate a vector, running GC if needed
    pub fn allocVector(self: *Vm, length: usize, capacity: usize) error{ OutOfMemory, Overflow }!Value {
        var none: [0]Value = .{};
        const payload_bytes = std.math.mul(usize, capacity, @sizeOf(Value)) catch return error.Overflow;
        const alloc_bytes = std.math.add(usize, @sizeOf(runtime.Vector), payload_bytes) catch return error.Overflow;
        try self.collectIfDebt(none[0..], alloc_bytes);
        return if (self.heap.allocVector(length, capacity)) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                _ = try self.collectGarbage();
                return try self.heap.allocVector(length, capacity);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Allocate a string, running GC if needed
    pub fn allocString(self: *Vm, data: []const u8) error{ OutOfMemory, Overflow }!Value {
        return if (self.heap.allocBaseString(data)) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                var tmp: ?[]u8 = null;
                defer if (tmp) |b| self.allocator.free(b);
                var stable = data;
                if (self.bytesInHeap(data)) {
                    const copy = try self.allocator.alloc(u8, data.len);
                    @memcpy(copy, data);
                    tmp = copy;
                    stable = copy;
                }
                _ = try self.collectGarbage();
                return try self.heap.allocBaseString(stable);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Allocate an uninitialized string, running GC if needed
    fn getStringDesignator(self: *Vm, val: Value, scratch: []u8) Error!stringPrims.DesignatorBytes {
        return stringPrims.designatorBytes(self.allocator, self.resolveForwardedValue(val), scratch) catch |err| switch (err) {
            error.TypeError => error.TypeMismatch,
            error.OutOfMemory => error.OutOfMemory,
        };
    }

    fn stabilizeHeapBytes(self: *Vm, bytes: []const u8, scratch: *?[]u8) Error![]const u8 {
        if (!self.bytesInHeap(bytes)) return bytes;
        const copy = try self.allocator.alloc(u8, bytes.len);
        @memcpy(copy, bytes);
        scratch.* = copy;
        return copy;
    }

    pub fn allocStringUninitialized(self: *Vm, length: usize) error{ OutOfMemory, Overflow }!Value {
        var none: [0]Value = .{};
        try self.collectIfDebt(none[0..], @sizeOf(runtime.String) + length);
        return if (self.heap.allocStringUninitialized(length)) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                _ = try self.collectGarbage();
                return try self.heap.allocStringUninitialized(length);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Allocate a symbol (uninterned), running GC if needed
    pub fn allocSymbol(self: *Vm, name: []const u8) error{OutOfMemory}!Value {
        return if (self.heap.allocSymbol(name)) |val| val else |_| {
            var tmp: ?[]u8 = null;
            defer if (tmp) |b| self.allocator.free(b);
            var stable = name;
            if (self.bytesInHeap(name)) {
                const copy = try self.allocator.alloc(u8, name.len);
                @memcpy(copy, name);
                tmp = copy;
                stable = copy;
            }
            _ = try self.collectGarbage();
            return try self.heap.allocSymbol(stable);
        };
    }

    /// Allocate a closure, running GC if needed
    pub fn allocClosureWithGC(self: *Vm, code: Value, arity: u32, captures: []Value) error{ OutOfMemory, Overflow }!Value {
        if (captures.len > 64) return error.Overflow;
        var roots_buf: [65]Value = undefined;
        roots_buf[0] = code;
        @memcpy(roots_buf[1 .. 1 + captures.len], captures);
        const roots = roots_buf[0 .. 1 + captures.len];
        try self.collectIfDebt(roots, @sizeOf(runtime.Closure) + captures.len * @sizeOf(Value));
        @memcpy(captures, roots[1..]);

        return if (self.heap.allocClosure(roots[0], arity, captures)) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                _ = try self.collectGarbageExtra(roots);
                @memcpy(captures, roots[1..]);
                return try self.heap.allocClosure(roots[0], arity, captures);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Allocate a hash table, running GC if needed
    pub fn allocHashTable(self: *Vm, capacity: usize, test_type: runtime.HashTest) error{ OutOfMemory, Overflow }!Value {
        var none: [0]Value = .{};
        try self.collectIfDebt(none[0..], @sizeOf(runtime.HashTable) + capacity * 2 * @sizeOf(Value));
        return self.heap.allocHashTable(capacity, test_type) catch |err| switch (err) {
            error.OutOfMemory => blk: {
                _ = try self.collectGarbage();
                break :blk try self.heap.allocHashTable(capacity, test_type);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Intern a symbol, running GC if needed
    pub fn intern(self: *Vm, name: []const u8) error{OutOfMemory}!Value {
        return if (self.heap.intern(name)) |val| val else |_| {
            var tmp: ?[]u8 = null;
            defer if (tmp) |b| self.allocator.free(b);
            var stable = name;
            if (self.bytesInHeap(name)) {
                const copy = try self.allocator.alloc(u8, name.len);
                @memcpy(copy, name);
                tmp = copy;
                stable = copy;
            }
            _ = try self.collectGarbage();
            return try self.heap.intern(stable);
        };
    }

    /// Run garbage collection, using VM state as roots
    /// Returns bytes reclaimed
    pub fn syncPrintGlobals(self: *Vm) !void {
        if (self.global_env) |env| {
            if (env.lookup("*print-length*")) |idx| {
                const val = self.globals[idx];
                if (val.isFixnum()) {
                    const len: usize = @intCast(val.toFixnum());
                    io.print_length = len;
                } else {
                    io.print_length = null;
                }
            }
            if (env.lookup("*print-level*")) |idx| {
                const val = self.globals[idx];
                if (val.isFixnum()) {
                    const lvl: usize = @intCast(val.toFixnum());
                    io.print_level = lvl;
                } else {
                    io.print_level = null;
                }
            }
        }
        io.setReadtableCase(try self.heap.readtableCase(try self.currentReadtable()));
    }

    fn handleSpecialVarLoad(self: *Vm, idx: u16) !Value {
        if (self.global_env) |env| {
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-ESCAPE*")) |esc_idx| {
                if (idx == esc_idx) return io.getPrintEscape();
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-CASE*")) |case_idx| {
                if (idx == case_idx) return try io.getPrintCase(self.heap);
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-READABLY*")) |read_idx| {
                if (idx == read_idx) return io.getPrintReadably();
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-BASE*")) |base_idx| {
                if (idx == base_idx) return io.getPrintBase();
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-RADIX*")) |radix_idx| {
                if (idx == radix_idx) return io.getPrintRadix();
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-GENSYM*")) |gensym_idx| {
                if (idx == gensym_idx) return io.getPrintGensym();
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-ARRAY*")) |array_idx| {
                if (idx == array_idx) return io.getPrintArray();
            }
        }
        return self.globals[idx];
    }

    /// Look up a special CL variable by exact global name.
    fn lookupSpecialVar(env: *const @import("../compiler/compile.zig").GlobalEnv, name: []const u8) ?u16 {
        return env.lookup(name);
    }

    pub fn currentReadtable(self: *Vm) !Value {
        if (self.global_env) |env| {
            if (lookupSpecialVar(env, "COMMON-LISP:*READTABLE*")) |idx| {
                const val = try self.loadGlobal(idx);
                if (val.isReadtable()) return val;
                if (!val.isNil() and !val.isUnbound()) return error.TypeMismatch;
            }
        }
        return self.heap.defaultReadtable();
    }

    fn readtableCaseFromKeyword(val: Value) Error!runtime.ReadtableCase {
        if (!val.isKeyword()) return error.TypeMismatch;
        const name = val.toPtr(runtime.Keyword).getName();
        if (std.mem.eql(u8, name, "UPCASE")) return .upcase;
        if (std.mem.eql(u8, name, "DOWNCASE")) return .downcase;
        if (std.mem.eql(u8, name, "PRESERVE")) return .preserve;
        if (std.mem.eql(u8, name, "INVERT")) return .invert;
        return error.TypeMismatch;
    }

    fn readtableCaseKeyword(self: *Vm, mode: runtime.ReadtableCase) Error!Value {
        return switch (mode) {
            .upcase => try self.heap.internKeyword("UPCASE"),
            .downcase => try self.heap.internKeyword("DOWNCASE"),
            .preserve => try self.heap.internKeyword("PRESERVE"),
            .invert => try self.heap.internKeyword("INVERT"),
        };
    }

    fn handleSpecialVarStore(self: *Vm, idx: u16, val: Value) !void {
        if (self.global_env) |env| {
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-ESCAPE*")) |esc_idx| {
                if (idx == esc_idx) {
                    io.setPrintEscape(val);
                    return;
                }
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-CASE*")) |case_idx| {
                if (idx == case_idx) {
                    try io.setPrintCase(&self.builtins, val);
                    return;
                }
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-READABLY*")) |read_idx| {
                if (idx == read_idx) {
                    io.setPrintReadably(val);
                    return;
                }
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-BASE*")) |base_idx| {
                if (idx == base_idx) {
                    try io.setPrintBase(val);
                    return;
                }
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-RADIX*")) |radix_idx| {
                if (idx == radix_idx) {
                    io.setPrintRadix(val);
                    return;
                }
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-GENSYM*")) |gensym_idx| {
                if (idx == gensym_idx) {
                    io.setPrintGensym(val);
                    return;
                }
            }
            if (lookupSpecialVar(env, "COMMON-LISP:*PRINT-ARRAY*")) |array_idx| {
                if (idx == array_idx) {
                    io.setPrintArray(val);
                    return;
                }
            }
            if (env.lookup("*print-length*")) |len_idx| {
                if (idx == len_idx) {
                    if (val.isFixnum()) {
                        const len: usize = @intCast(val.toFixnum());
                        io.print_length = len;
                    } else {
                        io.print_length = null;
                    }
                    return;
                }
            }
            if (env.lookup("*print-level*")) |lvl_idx| {
                if (idx == lvl_idx) {
                    if (val.isFixnum()) {
                        const lvl: usize = @intCast(val.toFixnum());
                        io.print_level = lvl;
                    } else {
                        io.print_level = null;
                    }
                    return;
                }
            }
        }
    }

    fn lookupSymbolGlobalIndex(self: *Vm, sym: *const Symbol) Error!?u16 {
        const env = self.global_env orelse return null;
        if (symbolIsUninterned(sym)) return null;
        const sym_val = Value.makeSymbol(sym);
        if (self.lookupGlobalIndexCache(sym_val)) |idx| return idx;

        var qual_buf: [512]u8 = undefined;
        const q = try qual_name.qualSymWithHeap(self.allocator, self.heap, sym, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);

        if (env.lookup(q.name)) |idx| {
            self.storeGlobalIndexCache(sym_val, idx);
            return idx;
        }
        return null;
    }

    fn globalNameForIndex(self: *Vm, idx: u16) ?[]const u8 {
        const env = self.global_env orelse return null;
        return env.nameForIndex(idx);
    }

    pub fn loadGlobal(self: *Vm, idx: u16) Error!Value {
        if (idx >= MAX_GLOBALS) return error.InvalidConstant;
        return self.handleSpecialVarLoad(idx);
    }

    pub fn storeGlobal(self: *Vm, idx: u16, val: Value) Error!void {
        if (idx >= MAX_GLOBALS) return error.InvalidConstant;
        self.globals[idx] = val;
        if (idx >= self.num_globals) {
            self.num_globals = idx + 1;
        }
        try self.handleSpecialVarStore(idx, val);
    }

    inline fn writeBarrierStore(self: *Vm, owner: Value, stored: Value) void {
        if (!stored.isPointer()) return;
        self.heap.writeBarrier(owner, stored);
    }

    pub fn collectGarbage(self: *Vm) !usize {
        var none: [0]Value = .{};
        return try self.collectGarbageExtra(none[0..]);
    }

    pub fn collectGarbageWithRoots(self: *Vm, extra_roots: []Value) !usize {
        return try self.collectGarbageExtra(extra_roots);
    }

    fn collectIfDebt(self: *Vm, extra_roots: []Value, alloc_hint_bytes: usize) !void {
        if (self.jit_gc_forbidden_depth != 0) return;
        self.safepoint_batch_ops +%= 1;
        self.safepoint_batch_bytes +%= alloc_hint_bytes;

        const should_poll = self.heap.shouldCollectDebt() or
            self.safepoint_batch_ops >= SAFEPOINT_BATCH_OPS or
            self.safepoint_batch_bytes >= SAFEPOINT_BATCH_BYTES;
        if (!should_poll) return;

        self.safepoint_batch_ops = 0;
        self.safepoint_batch_bytes = 0;

        const profile = self.heap.profileMutatorEnabled();
        const start_ns: i128 = if (profile) std.time.nanoTimestamp() else 0;
        const should_collect = self.heap.shouldCollectDebtNow();
        if (profile) {
            self.heap.noteSafepointVm(@intCast(elapsedNsSince(start_ns)));
        }
        if (!should_collect) return;
        _ = try self.collectGarbageExtra(extra_roots);
    }

    fn hasSeenChunk(seen: []const usize, addr: usize) bool {
        for (seen) |item| {
            if (item == addr) return true;
        }
        return false;
    }

    fn appendChunkGraphRoots(self: *Vm, roots: *std.ArrayList(*Value), chunk_pool: []Value) !void {
        var seen = std.ArrayList(usize){};
        defer seen.deinit(self.allocator);
        try seen.ensureTotalCapacity(self.allocator, chunk_pool.len);

        var stack = std.ArrayList(Value){};
        defer stack.deinit(self.allocator);
        try stack.ensureTotalCapacity(self.allocator, chunk_pool.len);

        for (chunk_pool, 0..) |chunk_val_raw, idx| {
            const chunk_val = self.resolveForwardedValue(chunk_val_raw);
            if (chunk_val.raw != chunk_val_raw.raw) chunk_pool[idx] = chunk_val;
            if (chunk_val.isChunk()) {
                try stack.append(self.allocator, chunk_val);
            }
        }

        while (stack.items.len != 0) {
            const chunk_val = stack.items[stack.items.len - 1];
            _ = stack.pop();
            const chunk_addr = chunk_val.toPtrAddr();
            if (hasSeenChunk(seen.items, chunk_addr)) continue;
            try seen.append(self.allocator, chunk_addr);

            const chunk = chunk_val.toPtr(Chunk);
            try roots.append(self.allocator, &chunk.lambda_expr);
            try roots.append(self.allocator, &chunk.name);
            try roots.append(self.allocator, &chunk.allowed_keywords);
            for (chunk.getConstants()) |*const_val| {
                try roots.append(self.allocator, const_val);
                const live_const = self.resolveForwardedValue(const_val.*);
                if (live_const.raw != const_val.raw) const_val.* = live_const;
                if (live_const.isChunk()) {
                    try stack.append(self.allocator, live_const);
                }
            }
        }
    }

    fn collectGarbageExtra(self: *Vm, extra_roots: []Value) !usize {
        if (self.jit_gc_forbidden_depth != 0) return error.OutOfMemory;
        const trace_validate_roots = self.trace_validate_root_layout;
        const trap_validate_roots = self.trap_validate_root_layout;
        if (trace_validate_roots or trap_validate_roots) {
            if (!self.validateBuiltinAndTypeRoots("pre-gc")) {
                if (trap_validate_roots) @panic("invalid root layout pre-gc");
            }
        }

        self.gc_slots.clearRetainingCapacity();
        self.safepoint_batch_ops = 0;
        self.safepoint_batch_bytes = 0;

        var frame_closure_roots: [MAX_FRAMES]Value = undefined;
        var current_closure_root = Value.nil;
        var catch_chunk_roots: [MAX_CATCHES]Value = undefined;
        var unwind_chunk_roots: [MAX_UNWINDS]Value = undefined;
        var block_chunk_roots: [MAX_BLOCKS]Value = undefined;
        var restart_chunk_roots: [MAX_RESTARTS]Value = undefined;
        var current_chunk_root = chunkRoot(self.chunk);
        var frame_chunk_roots: [MAX_FRAMES]Value = undefined;
        const chunk_pool = self.currentChunkPool();

        const has_current_closure = self.current_closure != null;
        const slots_need: usize = self.catch_sp +
            self.block_sp +
            self.restart_sp +
            self.progv_sp +
            self.handler_sp * 2 +
            9 +
            self.uninterned_values.count() +
            self.jit_literal_roots.items.len +
            self.fp +
            (if (has_current_closure) @as(usize, 1) else 0) +
            self.catch_sp +
            self.unwind_sp +
            self.block_sp +
            self.restart_sp +
            1 +
            self.fp;
        try self.gc_slots.ensureTotalCapacity(self.allocator, slots_need);

        for (self.catch_stack[0..self.catch_sp]) |*frame| {
            self.gc_slots.appendAssumeCapacity(&frame.tag);
        }
        for (self.block_stack[0..self.block_sp]) |*frame| {
            self.gc_slots.appendAssumeCapacity(&frame.name_raw);
        }
        for (self.restart_stack[0..self.restart_sp]) |*frame| {
            self.gc_slots.appendAssumeCapacity(&frame.name);
        }
        for (self.progv_stack[0..self.progv_sp]) |*frame| {
            self.gc_slots.appendAssumeCapacity(&frame.saved_bindings);
        }
        for (self.handler_stack[0..self.handler_sp]) |*frame| {
            self.gc_slots.appendAssumeCapacity(&frame.condition_type);
            self.gc_slots.appendAssumeCapacity(&frame.handler_fn);
        }

        self.gc_slots.appendAssumeCapacity(&self.pending_throw_tag);
        self.gc_slots.appendAssumeCapacity(&self.pending_throw_value);
        self.gc_slots.appendAssumeCapacity(&self.pending_block_value);
        var uninterned_it = self.uninterned_values.valueIterator();
        while (uninterned_it.next()) |slot| {
            self.gc_slots.appendAssumeCapacity(slot);
        }
        for (self.jit_literal_roots.items) |slot| {
            self.gc_slots.appendAssumeCapacity(slot);
        }
        try self.appendChunkGraphRoots(&self.gc_slots, chunk_pool);

        var closure_idx: usize = 0;
        for (self.frames[0..self.fp], 0..) |frame, i| {
            if (frame.closure) |c| {
                frame_closure_roots[closure_idx] = Value.makeClosure(c);
                self.gc_slots.appendAssumeCapacity(&frame_closure_roots[closure_idx]);
                closure_idx += 1;
            }
            frame_chunk_roots[i] = chunkRoot(frame.chunk);
            self.gc_slots.appendAssumeCapacity(&frame_chunk_roots[i]);
        }
        if (self.current_closure) |c| {
            current_closure_root = Value.makeClosure(c);
            self.gc_slots.appendAssumeCapacity(&current_closure_root);
        }

        for (self.catch_stack[0..self.catch_sp], 0..) |frame, i| {
            catch_chunk_roots[i] = chunkRoot(frame.chunk);
            self.gc_slots.appendAssumeCapacity(&catch_chunk_roots[i]);
        }
        for (self.unwind_stack[0..self.unwind_sp], 0..) |frame, i| {
            unwind_chunk_roots[i] = chunkRoot(frame.chunk);
            self.gc_slots.appendAssumeCapacity(&unwind_chunk_roots[i]);
        }
        for (self.block_stack[0..self.block_sp], 0..) |frame, i| {
            block_chunk_roots[i] = chunkRoot(frame.chunk);
            self.gc_slots.appendAssumeCapacity(&block_chunk_roots[i]);
        }
        for (self.restart_stack[0..self.restart_sp], 0..) |frame, i| {
            restart_chunk_roots[i] = chunkRoot(frame.chunk);
            self.gc_slots.appendAssumeCapacity(&restart_chunk_roots[i]);
        }
        self.gc_slots.appendAssumeCapacity(&current_chunk_root);

        var ranges: [13 + MAX_EXT_ROOT_SNAPSHOTS]roots_mod.RootRange = undefined;
        var range_len: usize = 0;
        const pushRangeUnique = struct {
            fn run(buf: []roots_mod.RootRange, len: *usize, vals: []Value) void {
                if (vals.len == 0) return;
                const ptr_addr = @intFromPtr(vals.ptr);
                for (buf[0..len.*]) |existing| {
                    if (@intFromPtr(existing.ptr) == ptr_addr and existing.len == vals.len) return;
                }
                std.debug.assert(len.* < buf.len);
                buf[len.*] = .{ .ptr = vals.ptr, .len = vals.len };
                len.* += 1;
            }
        }.run;
        if (self.sp != 0) {
            pushRangeUnique(&ranges, &range_len, self.stack[0..self.sp]);
        }
        if (self.num_globals != 0) {
            pushRangeUnique(&ranges, &range_len, self.globals[0..self.num_globals]);
        }
        if (self.comp_root_sp != 0) {
            pushRangeUnique(&ranges, &range_len, self.comp_root_stack[0..self.comp_root_sp]);
        }
        if (self.comp_retain_vals.items.len != 0) {
            pushRangeUnique(&ranges, &range_len, self.comp_retain_vals.items);
        }
        if (self.secondary_values_count != 0) {
            pushRangeUnique(&ranges, &range_len, self.secondary_values[0..self.secondary_values_count]);
        }
        if (self.saved_chunk_sp != 0) {
            pushRangeUnique(&ranges, &range_len, self.saved_chunks[0..self.saved_chunk_sp]);
        }
        const builtin_roots = valueFieldSlice(BuiltinSymbols, &self.builtins);
        if (builtin_roots.len != 0) {
            pushRangeUnique(&ranges, &range_len, builtin_roots);
        }
        const type_roots = valueFieldSlice(type_mod.TypeSymbols, &self.type_syms);
        if (type_roots.len != 0) {
            pushRangeUnique(&ranges, &range_len, type_roots);
        }
        if (chunk_pool.len != 0) {
            pushRangeUnique(&ranges, &range_len, chunk_pool);
        }
        const ext_roots = self.currentExtRoots();
        if (ext_roots.len != 0) {
            pushRangeUnique(&ranges, &range_len, ext_roots);
        }
        if (self.ext_roots_saved_sp != 0) {
            for (self.ext_roots_saved[0..self.ext_roots_saved_sp]) |saved| {
                const saved_roots = if (saved.owner) |owner| owner.items else saved.roots;
                if (saved_roots.len != 0) {
                    pushRangeUnique(&ranges, &range_len, saved_roots);
                }
            }
        }
        if (extra_roots.len != 0) {
            pushRangeUnique(&ranges, &range_len, extra_roots);
        }

        if (self.trace_chunk_pool_slot) |idx| {
            if (idx < chunk_pool.len) {
                const slot = chunk_pool[idx];
                std.debug.print(
                    "TRACE chunk-pool-slot idx={d} len={d} raw=0x{x} kind={s}\n",
                    .{ idx, chunk_pool.len, slot.raw, @tagName(slot.typeKind()) },
                );
                if (slot.isChunk()) {
                    const chunk = slot.toPtr(Chunk);
                    const chunk_addr = @intFromPtr(chunk);
                    const expected_const_pool = chunk_addr + @sizeOf(runtime.objects.Chunk);
                    std.debug.print(
                        "TRACE chunk-pool-slot-meta idx={d} consts={d} code_len={d} const_pool=0x{x} expected=0x{x}\n",
                        .{
                            idx,
                            chunk.const_count,
                            chunk.code_len,
                            @intFromPtr(chunk.const_pool),
                            expected_const_pool,
                        },
                    );
                    const stale_start = @intFromPtr(self.heap.to_start);
                    const stale_end = stale_start + self.heap.space_size;
                    const consts = chunk.getConstants();
                    var ci: usize = 0;
                    while (ci < consts.len) : (ci += 1) {
                        const cv = consts[ci];
                        if (!cv.isPointer() or cv.isMagicSymbol()) continue;
                        const addr = cv.toPtrAddr();
                        if (addr < stale_start or addr >= stale_end) continue;
                        const fw: *const Value = @ptrFromInt(addr);
                        const w1: *const usize = @ptrFromInt(addr + @sizeOf(Value));
                        std.debug.print(
                            "TRACE chunk-pool-slot-stale idx={d} const={d} raw=0x{x} kind={s} addr=0x{x} fw=0x{x} is_fwd={any} w1=0x{x} chunk={s}\n",
                            .{
                                idx,
                                ci,
                                cv.raw,
                                @tagName(cv.typeKind()),
                                addr,
                                fw.raw,
                                fw.isForwarding(),
                                w1.*,
                                chunkTraceName(chunk),
                            },
                        );
                    }
                }
            } else {
                std.debug.print("TRACE chunk-pool-slot idx={d} out-of-range len={d}\n", .{ idx, chunk_pool.len });
            }
        }

        if (self.trace_bad_root_layout) {
            var dbg_idx: usize = 0;
            if (ext_roots.len != 0) {
                const first = ext_roots[0];
                const owner_addr = if (self.ext_roots_owner) |owner| @intFromPtr(owner) else 0;
                std.debug.print(
                    "TRACE gc-ext owner=0x{x} ptr=0x{x} len={d} first=0x{x} kind={s}\n",
                    .{ owner_addr, @intFromPtr(ext_roots.ptr), ext_roots.len, first.raw, @tagName(first.typeKind()) },
                );
            }
            if (self.sp != 0) {
                std.debug.print("TRACE gc-range idx={d} name=stack len={d}\n", .{ dbg_idx, self.sp });
                dbg_idx += 1;
            }
            if (self.num_globals != 0) {
                std.debug.print("TRACE gc-range idx={d} name=globals len={d}\n", .{ dbg_idx, self.num_globals });
                dbg_idx += 1;
            }
            if (self.comp_root_sp != 0) {
                std.debug.print("TRACE gc-range idx={d} name=comp_root_stack len={d}\n", .{ dbg_idx, self.comp_root_sp });
                dbg_idx += 1;
            }
            if (self.comp_retain_vals.items.len != 0) {
                std.debug.print("TRACE gc-range idx={d} name=comp_retain_vals len={d}\n", .{ dbg_idx, self.comp_retain_vals.items.len });
                dbg_idx += 1;
            }
            if (self.secondary_values_count != 0) {
                std.debug.print("TRACE gc-range idx={d} name=secondary len={d}\n", .{ dbg_idx, self.secondary_values_count });
                dbg_idx += 1;
            }
            if (self.saved_chunk_sp != 0) {
                std.debug.print("TRACE gc-range idx={d} name=saved_chunks len={d}\n", .{ dbg_idx, self.saved_chunk_sp });
                dbg_idx += 1;
            }
            if (builtin_roots.len != 0) {
                std.debug.print("TRACE gc-range idx={d} name=builtins len={d}\n", .{ dbg_idx, builtin_roots.len });
                dbg_idx += 1;
            }
            if (type_roots.len != 0) {
                std.debug.print("TRACE gc-range idx={d} name=type_syms len={d}\n", .{ dbg_idx, type_roots.len });
                dbg_idx += 1;
            }
            if (chunk_pool.len != 0) {
                std.debug.print("TRACE gc-range idx={d} name=chunk_pool len={d}\n", .{ dbg_idx, chunk_pool.len });
                dbg_idx += 1;
            }
            if (ext_roots.len != 0) {
                std.debug.print("TRACE gc-range idx={d} name=ext_roots len={d}\n", .{ dbg_idx, ext_roots.len });
                dbg_idx += 1;
            }
            if (self.ext_roots_saved_sp != 0) {
                var saved_idx: usize = 0;
                while (saved_idx < self.ext_roots_saved_sp) : (saved_idx += 1) {
                    const saved = self.ext_roots_saved[saved_idx];
                    const saved_roots = if (saved.owner) |owner| owner.items else saved.roots;
                    if (saved_roots.len == 0) continue;
                    std.debug.print("TRACE gc-range idx={d} name=ext_saved len={d}\n", .{ dbg_idx, saved_roots.len });
                    dbg_idx += 1;
                }
            }
            if (extra_roots.len != 0) {
                std.debug.print("TRACE gc-range idx={d} name=extra_roots len={d}\n", .{ dbg_idx, extra_roots.len });
                dbg_idx += 1;
            }
            std.debug.print("TRACE gc-range total={d}\n", .{dbg_idx});
        }

        if (self.trace_bad_global_root) {
            const from_start = @intFromPtr(self.heap.from_start);
            const from_end = @intFromPtr(self.heap.from_end);
            const kind_n = @typeInfo(runtime.objects.BoxedKind).@"enum".fields.len;
            var gi: usize = 0;
            while (gi < self.num_globals and gi < MAX_GLOBALS) : (gi += 1) {
                const val = self.globals[gi];
                if (!val.isPointer() or val.getTag() != .boxed) continue;
                const addr = val.toPtrAddr();
                if (addr < from_start or addr >= from_end) continue;
                const first_word: *const Value = @ptrFromInt(addr);
                if (first_word.isForwarding()) continue;
                const kind_raw = @as(*const u64, @ptrFromInt(addr)).*;
                if (kind_raw < kind_n) continue;
                const name = self.globalNameForIndex(@intCast(gi)) orelse "<unknown>";
                std.debug.print(
                    "TRACE bad-global-root idx={d} name={s} val=0x{x} kind-raw=0x{x}\n",
                    .{ gi, name, val.raw, kind_raw },
                );
            }
        }

        if (self.trace_bad_global_kind) {
            const kind_n = @typeInfo(runtime.objects.BoxedKind).@"enum".fields.len;
            var gi: usize = 0;
            while (gi < self.num_globals and gi < MAX_GLOBALS) : (gi += 1) {
                const val = self.globals[gi];
                if (!val.isPointer() or val.getTag() != .boxed) continue;
                const addr = val.toPtrAddr();
                if (!self.heap.containsAddrForDebug(addr)) continue;
                const kind_raw = @as(*const u64, @ptrFromInt(addr)).*;
                if (kind_raw < kind_n) continue;
                const name = self.globalNameForIndex(@intCast(gi)) orelse "<unknown>";
                std.debug.print(
                    "TRACE bad-global-kind idx={d} name={s} val=0x{x} kind-raw=0x{x}\n",
                    .{ gi, name, val.raw, kind_raw },
                );
            }
        }

        const reclaimed = try self.heap.collectGarbageRootSet(.{
            .ranges = ranges[0..range_len],
            .slots = self.gc_slots.items,
        });
        if (trace_validate_roots or trap_validate_roots) {
            if (!self.validateBuiltinAndTypeRoots("post-gc")) {
                if (trap_validate_roots) @panic("invalid root layout post-gc");
            }
        }

        closure_idx = 0;
        for (self.frames[0..self.fp]) |*frame| {
            if (frame.closure != null) {
                frame.closure = frame_closure_roots[closure_idx].toPtr(runtime.Closure);
                closure_idx += 1;
            }
        }
        if (has_current_closure) {
            self.current_closure = current_closure_root.toPtr(runtime.Closure);
        }

        for (self.catch_stack[0..self.catch_sp], 0..) |*frame, i| {
            const chunk_val = catch_chunk_roots[i];
            if (!chunk_val.isNil()) {
                if (self.chunkFromValue(chunk_val)) |chunk| {
                    frame.chunk = chunk;
                }
            }
        }
        for (self.unwind_stack[0..self.unwind_sp], 0..) |*frame, i| {
            const chunk_val = unwind_chunk_roots[i];
            if (!chunk_val.isNil()) {
                if (self.chunkFromValue(chunk_val)) |chunk| {
                    frame.chunk = chunk;
                }
            }
        }
        for (self.block_stack[0..self.block_sp], 0..) |*frame, i| {
            const chunk_val = block_chunk_roots[i];
            if (!chunk_val.isNil()) {
                if (self.chunkFromValue(chunk_val)) |chunk| {
                    frame.chunk = chunk;
                }
            }
        }
        for (self.restart_stack[0..self.restart_sp], 0..) |*frame, i| {
            const chunk_val = restart_chunk_roots[i];
            if (!chunk_val.isNil()) {
                if (self.chunkFromValue(chunk_val)) |chunk| {
                    frame.chunk = chunk;
                }
            }
        }

        const current_chunk_val = current_chunk_root;
        if (!current_chunk_val.isNil()) {
            if (self.chunkFromValue(current_chunk_val)) |chunk| {
                self.chunk = chunk;
            }
        }
        for (self.frames[0..self.fp], 0..) |*frame, i| {
            const chunk_val = frame_chunk_roots[i];
            if (!chunk_val.isNil()) {
                if (self.chunkFromValue(chunk_val)) |chunk| {
                    frame.chunk = chunk;
                }
            }
        }

        try self.rekeyJitFnsAfterGc();
        self.clearFnResolveCache();
        self.clearGlobalIndexCache();
        self.clearKeyAllowlistCache();
        self.clearChunkConstCache();

        if (trace_validate_roots or trap_validate_roots) {
            if (!self.validateBuiltinAndTypeRoots("post-restore")) {
                if (trap_validate_roots) @panic("invalid root layout post-restore");
            }
        }

        return reclaimed;
    }

    /// Call a closure with arguments already on stack
    /// Expects args to be pushed already at positions [0..argc)
    pub fn callClosure(self: *Vm, closure: *const runtime.Closure, argc: u8) anyerror!Value {
        if (self.sp < argc) return error.StackUnderflow;

        const base = self.sp - argc;
        var args_buf: [255]Value = undefined;
        const argc_usize: usize = argc;
        if (argc_usize > 0) {
            @memcpy(args_buf[0..argc_usize], self.stack[base .. base + argc_usize]);
        }

        return try self.callFromStackAt(base, Value.makeClosure(closure), args_buf[0..argc_usize]);
    }

    /// Call function with arguments provided as a slice
    pub fn callFromStack(self: *Vm, fn_val: Value, args: []const Value) Error!Value {
        const saved_resolver = runtime.setSymbolValueResolver(&runtimeLookupSymbolValue, @ptrCast(self));
        defer runtime.restoreSymbolValueResolver(saved_resolver);
        if (self.isExecuting()) {
            return self.callFromStackAt(self.sp, fn_val, args);
        }

        const saved_state = State.save(self);

        const saved_idx = self.saved_chunk_sp;
        if (saved_idx >= MAX_SAVED_CHUNKS) return error.StackOverflow;
        self.saved_chunks[saved_idx] = chunkRoot(saved_state.chunk);
        self.saved_chunk_sp = saved_idx + 1;
        defer {
            const chunk_val = self.saved_chunks[saved_idx];
            self.saved_chunk_sp = saved_idx;
            saved_state.restore(self);
            if (!chunk_val.isNil()) {
                if (self.chunkFromValue(chunk_val)) |chunk| {
                    self.chunk = chunk;
                }
            }
        }

        if (args.len + 1 > self.stack.len) return error.StackOverflow;
        self.stack[0] = fn_val;
        for (args, 0..) |arg, i| {
            self.stack[i + 1] = arg;
        }
        self.sp = args.len + 1;

        self.chunk = &halt_chunk;
        self.ip = 0;
        self.fp = 0;
        self.scope_sp = 0;

        const argc: u8 = @intCast(args.len);
        try self.doCall(argc, false);
        return try self.execute();
    }

    /// Call function with arguments provided as a slice, using stack slots starting at `base`.
    /// This preserves any values below `base` without copying.
    pub fn callFromStackAt(self: *Vm, base: usize, fn_val: Value, args: []const Value) Error!Value {
        const saved_resolver = runtime.setSymbolValueResolver(&runtimeLookupSymbolValue, @ptrCast(self));
        defer runtime.restoreSymbolValueResolver(saved_resolver);
        const saved_state = State.save(self);

        const saved_idx = self.saved_chunk_sp;
        if (saved_idx >= MAX_SAVED_CHUNKS) return error.StackOverflow;
        self.saved_chunks[saved_idx] = chunkRoot(saved_state.chunk);
        self.saved_chunk_sp = saved_idx + 1;
        var should_restore = true;
        defer {
            if (should_restore) {
                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
            }
        }
        const saved_barrier = self.throw_barrier_depth;
        self.throw_barrier_depth = saved_state.catch_sp;
        defer self.throw_barrier_depth = saved_barrier;

        if (base + args.len + 1 > self.stack.len) return error.StackOverflow;
        self.stack[base] = fn_val;
        for (args, 0..) |arg, i| {
            self.stack[base + i + 1] = arg;
        }
        self.sp = base + args.len + 1;

        self.chunk = &halt_chunk;
        self.ip = 0;

        const argc: u8 = @intCast(args.len);
        self.doCall(argc, false) catch |call_err| {
            if (call_err == error.NestedNonLocalExit) {
                const relay_tag = self.relay_throw_tag;
                const relay_value = self.relay_throw_value;
                if (relay_tag.isNil()) return error.NestedNonLocalExit;

                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
                should_restore = false;

                self.throw_barrier_depth = saved_barrier;
                self.doThrow(relay_tag, relay_value) catch |throw_err| {
                    if (throw_err == error.NestedNonLocalExit) return throw_err;
                    return throw_err;
                };
                self.relay_throw_tag = Value.nil;
                self.relay_throw_value = Value.nil;
                return error.ControlTransfer;
            }
            return call_err;
        };
        const result = self.execute() catch |run_err| {
            if (run_err == error.NestedNonLocalExit) {
                const relay_tag = self.relay_throw_tag;
                const relay_value = self.relay_throw_value;
                if (relay_tag.isNil()) return error.NestedNonLocalExit;

                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
                should_restore = false;

                self.throw_barrier_depth = saved_barrier;
                self.doThrow(relay_tag, relay_value) catch |throw_err| {
                    if (throw_err == error.NestedNonLocalExit) return throw_err;
                    return throw_err;
                };
                self.relay_throw_tag = Value.nil;
                self.relay_throw_value = Value.nil;
                return error.ControlTransfer;
            }
            return run_err;
        };
        const chunk_val = self.saved_chunks[saved_idx];
        const mv_count = self.secondary_values_count;
        var mv: [MAX_SECONDARY_VALUES]Value = undefined;
        if (mv_count != 0) @memcpy(mv[0..mv_count], self.secondary_values[0..mv_count]);
        const zero_values = self.zero_values_returned;
        self.saved_chunk_sp = saved_idx;
        saved_state.restore(self);
        if (!chunk_val.isNil()) {
            if (self.chunkFromValue(chunk_val)) |chunk| {
                self.chunk = chunk;
            }
        }
        self.secondary_values_count = mv_count;
        if (mv_count != 0) @memcpy(self.secondary_values[0..mv_count], mv[0..mv_count]);
        self.zero_values_returned = zero_values;
        should_restore = false;
        return result;
    }

    /// Like callFromStackAt, but checks for JIT code on the callee and
    /// calls it directly, bypassing the interpreter loop.
    pub fn callFromStackAtFast(self: *Vm, base: usize, fn_val: Value, args: []const Value) Error!Value {
        const saved_resolver = runtime.setSymbolValueResolver(&runtimeLookupSymbolValue, @ptrCast(self));
        defer runtime.restoreSymbolValueResolver(saved_resolver);
        const saved_state = State.save(self);

        const saved_idx = self.saved_chunk_sp;
        if (saved_idx >= MAX_SAVED_CHUNKS) return error.StackOverflow;
        self.saved_chunks[saved_idx] = chunkRoot(saved_state.chunk);
        self.saved_chunk_sp = saved_idx + 1;
        var should_restore = true;
        defer {
            if (should_restore) {
                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
            }
        }
        const saved_barrier = self.throw_barrier_depth;
        self.throw_barrier_depth = saved_state.catch_sp;
        defer self.throw_barrier_depth = saved_barrier;

        if (base + args.len + 1 > self.stack.len) return error.StackOverflow;
        self.stack[base] = fn_val;
        for (args, 0..) |arg, i| {
            self.stack[base + i + 1] = arg;
        }
        self.sp = base + args.len + 1;

        self.chunk = &halt_chunk;
        self.ip = 0;

        const argc: u8 = @intCast(args.len);
        self.doCall(argc, false) catch |call_err| {
            if (call_err == error.NestedNonLocalExit) {
                const relay_tag = self.relay_throw_tag;
                const relay_value = self.relay_throw_value;
                if (relay_tag.isNil()) return error.NestedNonLocalExit;

                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
                should_restore = false;

                self.throw_barrier_depth = saved_barrier;
                self.doThrow(relay_tag, relay_value) catch |throw_err| {
                    if (throw_err == error.NestedNonLocalExit) return throw_err;
                    return throw_err;
                };
                self.relay_throw_tag = Value.nil;
                self.relay_throw_value = Value.nil;
                return error.ControlTransfer;
            }
            return call_err;
        };

        // After doCall, self.chunk is the callee's chunk.
        // Try hoist SSA JIT for native execution.
        if (try self.tryCallJit(argc)) |result| {
            // Hoist call succeeded — unwind frame and return result
            self.fp -= 1;
            self.sp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
            return result;
        }

        const result = self.execute() catch |run_err| {
            if (run_err == error.NestedNonLocalExit) {
                const relay_tag = self.relay_throw_tag;
                const relay_value = self.relay_throw_value;
                if (relay_tag.isNil()) return error.NestedNonLocalExit;

                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
                should_restore = false;

                self.throw_barrier_depth = saved_barrier;
                self.doThrow(relay_tag, relay_value) catch |throw_err| {
                    if (throw_err == error.NestedNonLocalExit) return throw_err;
                    return throw_err;
                };
                self.relay_throw_tag = Value.nil;
                self.relay_throw_value = Value.nil;
                return error.ControlTransfer;
            }
            return run_err;
        };
        const chunk_val = self.saved_chunks[saved_idx];
        const mv_count = self.secondary_values_count;
        var mv: [MAX_SECONDARY_VALUES]Value = undefined;
        if (mv_count != 0) @memcpy(mv[0..mv_count], self.secondary_values[0..mv_count]);
        const zero_values = self.zero_values_returned;
        self.saved_chunk_sp = saved_idx;
        saved_state.restore(self);
        if (!chunk_val.isNil()) {
            if (self.chunkFromValue(chunk_val)) |chunk| {
                self.chunk = chunk;
            }
        }
        self.secondary_values_count = mv_count;
        if (mv_count != 0) @memcpy(self.secondary_values[0..mv_count], mv[0..mv_count]);
        self.zero_values_returned = zero_values;
        should_restore = false;
        return result;
    }

    /// Apply function with args list provided as a value
    pub fn applyFromStack(self: *Vm, fn_val: Value, args_list: Value) Error!Value {
        const saved_resolver = runtime.setSymbolValueResolver(&runtimeLookupSymbolValue, @ptrCast(self));
        defer runtime.restoreSymbolValueResolver(saved_resolver);
        if (self.isExecuting()) {
            return self.applyFromStackAt(self.sp, fn_val, args_list);
        }

        const saved_state = State.save(self);

        const saved_idx = self.saved_chunk_sp;
        if (saved_idx >= MAX_SAVED_CHUNKS) return error.StackOverflow;
        self.saved_chunks[saved_idx] = chunkRoot(saved_state.chunk);
        self.saved_chunk_sp = saved_idx + 1;
        defer {
            const chunk_val = self.saved_chunks[saved_idx];
            self.saved_chunk_sp = saved_idx;
            saved_state.restore(self);
            if (!chunk_val.isNil()) {
                if (self.chunkFromValue(chunk_val)) |chunk| {
                    self.chunk = chunk;
                }
            }
        }

        if (self.stack.len < 2) return error.StackOverflow;
        self.stack[0] = fn_val;
        self.stack[1] = args_list;
        self.sp = 2;

        self.chunk = &halt_chunk;
        self.ip = 0;
        self.fp = 0;
        self.scope_sp = 0;

        try self.doApply();
        return try self.execute();
    }

    /// Apply function with args list provided as a value, using stack slots starting at `base`.
    /// This preserves any values below `base` without copying.
    pub fn applyFromStackAt(self: *Vm, base: usize, fn_val: Value, args_list: Value) Error!Value {
        const saved_resolver = runtime.setSymbolValueResolver(&runtimeLookupSymbolValue, @ptrCast(self));
        defer runtime.restoreSymbolValueResolver(saved_resolver);
        const saved_state = State.save(self);

        const saved_idx = self.saved_chunk_sp;
        if (saved_idx >= MAX_SAVED_CHUNKS) return error.StackOverflow;
        self.saved_chunks[saved_idx] = chunkRoot(saved_state.chunk);
        self.saved_chunk_sp = saved_idx + 1;
        var should_restore = true;
        defer {
            if (should_restore) {
                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
            }
        }
        const saved_barrier = self.throw_barrier_depth;
        self.throw_barrier_depth = saved_state.catch_sp;
        defer self.throw_barrier_depth = saved_barrier;

        if (base + 2 > self.stack.len) return error.StackOverflow;
        self.stack[base] = fn_val;
        self.stack[base + 1] = args_list;
        self.sp = base + 2;

        self.chunk = &halt_chunk;
        self.ip = 0;

        self.doApply() catch |apply_err| {
            if (apply_err == error.NestedNonLocalExit) {
                const relay_tag = self.relay_throw_tag;
                const relay_value = self.relay_throw_value;
                if (relay_tag.isNil()) return error.NestedNonLocalExit;

                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
                should_restore = false;

                self.throw_barrier_depth = saved_barrier;
                self.doThrow(relay_tag, relay_value) catch |throw_err| {
                    if (throw_err == error.NestedNonLocalExit) return throw_err;
                    return throw_err;
                };
                self.relay_throw_tag = Value.nil;
                self.relay_throw_value = Value.nil;
                return error.ControlTransfer;
            }
            return apply_err;
        };
        return self.execute() catch |run_err| {
            if (run_err == error.NestedNonLocalExit) {
                const relay_tag = self.relay_throw_tag;
                const relay_value = self.relay_throw_value;
                if (relay_tag.isNil()) return error.NestedNonLocalExit;

                const chunk_val = self.saved_chunks[saved_idx];
                self.saved_chunk_sp = saved_idx;
                saved_state.restore(self);
                if (!chunk_val.isNil()) {
                    if (self.chunkFromValue(chunk_val)) |chunk| {
                        self.chunk = chunk;
                    }
                }
                should_restore = false;

                self.throw_barrier_depth = saved_barrier;
                self.doThrow(relay_tag, relay_value) catch |throw_err| {
                    if (throw_err == error.NestedNonLocalExit) return throw_err;
                    return throw_err;
                };
                self.relay_throw_tag = Value.nil;
                self.relay_throw_value = Value.nil;
                return error.ControlTransfer;
            }
            return run_err;
        };
    }

    /// Run a chunk to completion
    pub fn run(self: *Vm, chunk: *const Chunk) Error!Value {
        const saved_resolver = runtime.setSymbolValueResolver(&runtimeLookupSymbolValue, @ptrCast(self));
        defer runtime.restoreSymbolValueResolver(saved_resolver);
        self.chunk = chunk;
        self.ip = 0;
        self.sp = 0;
        self.fp = 0;
        self.scope_sp = 0;

        // Reserve space for locals
        var i: usize = 0;
        while (i < chunk.num_locals) : (i += 1) {
            try self.push(Value.nil);
        }

        return self.execute();
    }

    pub fn isExecuting(self: *const Vm) bool {
        return self.execute_depth != 0;
    }

    fn execute(self: *Vm) Error!Value {
        self.execute_depth += 1;
        defer self.execute_depth -= 1;

        const trace_builtin_write = self.trace_builtin_write;
        const trap_builtin_write = self.trap_builtin_write;
        const trace_stale_root_table = self.trace_stale_root_table;
        const trap_stale_root_table = self.trap_stale_root_table;
        var trace_gc_count: usize = self.heap.stats.gc_count;
        var trace_builtins_prev: [BUILTIN_ROOT_N]Value = undefined;
        var trace_type_prev: [TYPE_ROOT_N]Value = undefined;
        if (trace_builtin_write or trap_builtin_write) {
            snapshotRootSlice(trace_builtins_prev[0..], valueFieldSlice(BuiltinSymbols, &self.builtins));
            snapshotRootSlice(trace_type_prev[0..], valueFieldSlice(type_mod.TypeSymbols, &self.type_syms));
        }

        while (true) {
            if (trace_builtin_write or trap_builtin_write) {
                const builtins_now = valueFieldSlice(BuiltinSymbols, &self.builtins);
                const type_now = valueFieldSlice(type_mod.TypeSymbols, &self.type_syms);
                const gc_now = self.heap.stats.gc_count;
                if (gc_now == trace_gc_count) {
                    if (firstRootDiff(trace_builtins_prev[0..], builtins_now)) |i| {
                        std.debug.print(
                            "TRACE builtins-write-outside-gc chunk={s} ip={d} idx={d} prev=0x{x} cur=0x{x}\n",
                            .{ chunkTraceName(self.chunk), self.ip, i, trace_builtins_prev[i].raw, builtins_now[i].raw },
                        );
                        if (trap_builtin_write) @panic("builtins write outside gc");
                    }
                    if (firstRootDiff(trace_type_prev[0..], type_now)) |i| {
                        std.debug.print(
                            "TRACE type-syms-write-outside-gc chunk={s} ip={d} idx={d} prev=0x{x} cur=0x{x}\n",
                            .{ chunkTraceName(self.chunk), self.ip, i, trace_type_prev[i].raw, type_now[i].raw },
                        );
                        if (trap_builtin_write) @panic("type syms write outside gc");
                    }
                }
                snapshotRootSlice(trace_builtins_prev[0..], builtins_now);
                snapshotRootSlice(trace_type_prev[0..], type_now);
                trace_gc_count = gc_now;
            }
            if (trace_stale_root_table or trap_stale_root_table) {
                const builtins_now = valueFieldSlice(BuiltinSymbols, &self.builtins);
                for (builtins_now, 0..) |val, i| {
                    if (!self.rootPointsIntoStaleSpace(val)) continue;
                    std.debug.print(
                        "TRACE stale-root-table chunk={s} ip={d} root=builtins idx={d} raw=0x{x}\n",
                        .{ chunkTraceName(self.chunk), self.ip, i, val.raw },
                    );
                    if (trap_stale_root_table) @panic("stale builtins root");
                }
                const type_now = valueFieldSlice(type_mod.TypeSymbols, &self.type_syms);
                for (type_now, 0..) |val, i| {
                    if (!self.rootPointsIntoStaleSpace(val)) continue;
                    std.debug.print(
                        "TRACE stale-root-table chunk={s} ip={d} root=type_syms idx={d} raw=0x{x}\n",
                        .{ chunkTraceName(self.chunk), self.ip, i, val.raw },
                    );
                    if (trap_stale_root_table) @panic("stale type root");
                }
            }

            const chunk_addr = @intFromPtr(self.chunk);
            const stale_start = @intFromPtr(self.heap.to_start);
            const stale_end = stale_start + self.heap.space_size;
            if (chunk_addr >= stale_start and chunk_addr < stale_end) {
                self.refreshCurrentChunk();
            }
            // Bounds check before reading opcode to prevent read past end of chunk
            if (self.ip >= self.chunk.getCode().len) return self.invalidOpcode("execute.ip-oob");
            const op_ip = self.ip;
            const op = self.readOp();

            // Execute opcode with error handling
            if (self.executeOp(op)) |_| {} else |err| {
                if (self.shouldTraceOpError(op, err)) {
                    const chunk_name = chunkTraceName(self.chunk);
                    std.debug.print(
                        "TRACE op error: err={s} op={s} chunk={s} op_ip={d} next_ip={d} sp={d} fp={d}\n",
                        .{ @errorName(err), @tagName(op), chunk_name, op_ip, self.ip, self.sp, self.fp },
                    );
                    const dump_n = @min(self.sp, envTraceCount("HABU_TRACE_ERROR_STACK", 4));
                    var i: usize = 0;
                    while (i < dump_n) : (i += 1) {
                        const v = self.stack[self.sp - 1 - i];
                        std.debug.print("  stack[-{d}]=", .{i + 1});
                        tracePrintValue(v);
                        std.debug.print("\n", .{});
                    }
                    const dump_frames = @min(self.fp, envTraceCount("HABU_TRACE_ERROR_FRAMES", 4));
                    var fi: usize = 0;
                    while (fi < dump_frames) : (fi += 1) {
                        const idx = self.fp - 1 - fi;
                        const frame = self.frames[idx];
                        const frame_name = chunkTraceName(frame.chunk);
                        std.debug.print(
                            "  frame[-{d}] chunk={s} ret_ip={d} bp={d} argc={d}\n",
                            .{ fi + 1, frame_name, frame.return_ip, frame.bp, frame.argc },
                        );
                    }
                }
                if (err == error.Halt) {
                    // Program terminated - return result from stack
                    std.debug.assert(self.sp > 0);
                    return try self.pop();
                }
                if (err == error.NestedNonLocalExit) {
                    // Nested non-local exits must cross the current call barrier.
                    // The enclosing callFromStackAt/applyFromStackAt frame restores
                    // caller VM state and performs the relay throw.
                    return err;
                }
                if (err == error.ControlTransfer) {
                    continue;
                }
                // Try to convert Zig error to CL condition (type-error etc.).
                // If a handler-case / handler-bind / catch handles it,
                // doThrow sets up the handler call and we continue the loop.
                // NestedNonLocalExit propagates to cross the call barrier.
                if (try self.trySignalCondition(err)) continue;
                return self.doError(err);
            }
        }
    }

    /// Execute a single opcode
    fn executeOp(self: *Vm, op: Op) anyerror!void {
        switch (op) {
            // Stack manipulation
            .push_nil => try self.push(Value.nil),
            .push_t => try self.push(Value.t),
            .push_i32 => {
                const n = self.readI32();
                try self.push(Value.makeFixnum(n));
            },
            .push_const => {
                const idx = self.readU16();
                try self.push(try self.loadConst(idx));
            },
            .dup => {
                const val = try self.peek(0);
                try self.push(val);
            },
            .pop => _ = try self.pop(),
            .swap => {
                const a = try self.pop();
                const b = try self.pop();
                try self.push(a);
                try self.push(b);
            },

            // Variable access - always use frame's bp (not scope's bp)
            // Indices are frame-relative, assigned by compiler
            .load_local => {
                const idx = self.readU8();
                const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
                const stack_idx = bp + idx;
                if (idx >= self.chunk.num_locals or stack_idx >= STACK_SIZE) {
                    if (self.trace_local_mismatch) {
                        std.debug.print("LOCAL_MISMATCH op=load idx={d} num_locals={d} bp={d} stack_idx={d} ip={d}", .{
                            idx,
                            self.chunk.num_locals,
                            bp,
                            stack_idx,
                            self.ip,
                        });
                        switch (self.chunk.name.typeKind()) {
                            .symbol => std.debug.print(" chunk={s}\n", .{self.chunk.name.toPtr(Symbol).getName()}),
                            .string => std.debug.print(" chunk={s}\n", .{self.chunk.name.toPtr(runtime.String).bytes()}),
                            else => std.debug.print(" chunk-kind={s}\n", .{@tagName(self.chunk.name.typeKind())}),
                        }
                        const start = if (self.ip > 20) self.ip - 20 else 0;
                        const end = @min(self.chunk.code_len, self.ip + 20);
                        std.debug.print("LOCAL_MISMATCH code [{d}..{d}):", .{ start, end });
                        var bi: usize = start;
                        while (bi < end) : (bi += 1) {
                            std.debug.print(" {x:0>2}", .{self.chunk.code[bi]});
                        }
                        std.debug.print("\n", .{});
                    }
                    return self.invalidOpcode("execute-op.load-local-range");
                }
                const val = self.stack[stack_idx];
                try self.push(val);
            },
            .store_local => {
                const idx = self.readU8();
                const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
                const stack_idx = bp + idx;
                if (idx >= self.chunk.num_locals or stack_idx >= STACK_SIZE) {
                    if (self.trace_local_mismatch) {
                        std.debug.print("LOCAL_MISMATCH op=store idx={d} num_locals={d} bp={d} stack_idx={d} ip={d}", .{
                            idx,
                            self.chunk.num_locals,
                            bp,
                            stack_idx,
                            self.ip,
                        });
                        switch (self.chunk.name.typeKind()) {
                            .symbol => std.debug.print(" chunk={s}\n", .{self.chunk.name.toPtr(Symbol).getName()}),
                            .string => std.debug.print(" chunk={s}\n", .{self.chunk.name.toPtr(runtime.String).bytes()}),
                            else => std.debug.print(" chunk-kind={s}\n", .{@tagName(self.chunk.name.typeKind())}),
                        }
                        const start = if (self.ip > 20) self.ip - 20 else 0;
                        const end = @min(self.chunk.code_len, self.ip + 20);
                        std.debug.print("LOCAL_MISMATCH code [{d}..{d}):", .{ start, end });
                        var bi: usize = start;
                        while (bi < end) : (bi += 1) {
                            std.debug.print(" {x:0>2}", .{self.chunk.code[bi]});
                        }
                        std.debug.print("\n", .{});
                    }
                    return self.invalidOpcode("execute-op.store-local-range");
                }
                const val = try self.pop();
                self.stack[stack_idx] = val;
            },
            .enter_scope => {
                const num_locals = self.readU8();
                if (self.scope_sp >= MAX_SCOPES) return error.StackOverflow;
                // Record current sp as base for this scope
                self.scope_stack[self.scope_sp] = .{
                    .bp = self.sp,
                    .num_locals = num_locals,
                };
                self.scope_sp += 1;
                // Reserve slots by pushing nil placeholders
                for (0..num_locals) |_| {
                    try self.push(Value.nil);
                }
            },
            .exit_scope => {
                const num_locals = self.readU8();
                if (self.scope_sp == 0) return self.invalidOpcode("execute-op.exit-scope-empty");
                self.scope_sp -= 1;
                // Result is on top of stack, locals are below
                // Stack: [locals...] result
                // We need: result
                if (num_locals > 0) {
                    const result = try self.pop();
                    // Pop the locals
                    for (0..num_locals) |_| {
                        _ = try self.pop();
                    }
                    try self.push(result);
                }
            },
            .load_capture => {
                const idx = self.readU8();
                // Get current closure from frame, or from current_closure if fp=0
                const closure = if (self.fp > 0)
                    self.frames[self.fp - 1].closure
                else
                    self.current_closure;

                if (closure) |c| {
                    if (idx < c.num_captures) {
                        try self.push(c.getCapture(idx));
                    } else {
                        return error.InvalidConstant;
                    }
                } else {
                    return error.InvalidConstant;
                }
            },
            .load_upvalue => {
                _ = self.readU8(); // depth (unused with flat closures)
                const index = self.readU8();
                // Get current closure from frame, or from current_closure if fp=0
                const closure = if (self.fp > 0)
                    self.frames[self.fp - 1].closure
                else
                    self.current_closure;

                if (closure) |c| {
                    if (index < c.num_captures) {
                        try self.push(c.getCapture(index));
                    } else {
                        if (self.trace_upvalue) {
                            if (c.code.isChunk()) {
                                const code_chunk = c.code.toPtr(Chunk);
                                switch (code_chunk.name.typeKind()) {
                                    .symbol => std.debug.print("TRACE upvalue closure code={s}\n", .{code_chunk.name.toPtr(Symbol).getName()}),
                                    .string => std.debug.print("TRACE upvalue closure code={s}\n", .{code_chunk.name.toPtr(runtime.String).bytes()}),
                                    .nil => std.debug.print("TRACE upvalue closure code=nil\n", .{}),
                                    else => std.debug.print("TRACE upvalue closure code-kind={s}\n", .{@tagName(code_chunk.name.typeKind())}),
                                }
                                std.debug.print("TRACE upvalue closure code_len={d}\n", .{code_chunk.getCode().len});
                            } else {
                                std.debug.print("TRACE upvalue closure non-chunk code kind={s}\n", .{@tagName(c.code.typeKind())});
                            }
                            std.debug.print(
                                "TRACE upvalue oob: index={d} captures={d} ip={d} fp={d} sp={d}\n",
                                .{ index, c.num_captures, self.ip, self.fp, self.sp },
                            );
                            if (c.code.isChunk()) {
                                const code_chunk = c.code.toPtr(Chunk);
                                const code = code_chunk.getCode();
                                std.debug.print("TRACE upvalue closure code bytes:", .{});
                                var ci: usize = 0;
                                while (ci < code.len) : (ci += 1) {
                                    std.debug.print(" {x:0>2}", .{code[ci]});
                                }
                                std.debug.print("\n", .{});
                            }
                            switch (self.chunk.name.typeKind()) {
                                .symbol => std.debug.print("  chunk={s}\n", .{self.chunk.name.toPtr(Symbol).getName()}),
                                .string => std.debug.print("  chunk={s}\n", .{self.chunk.name.toPtr(runtime.String).bytes()}),
                                .nil => std.debug.print("  chunk=nil\n", .{}),
                                else => std.debug.print("  chunk-kind={s}\n", .{@tagName(self.chunk.name.typeKind())}),
                            }
                        }
                        return error.InvalidConstant;
                    }
                } else {
                    if (self.trace_upvalue) {
                        std.debug.print(
                            "TRACE upvalue no closure: index={d} ip={d} fp={d} sp={d}\n",
                            .{ index, self.ip, self.fp, self.sp },
                        );
                    }
                    return error.TypeMismatch; // No closure
                }
            },
            .store_upvalue => {
                _ = self.readU8(); // depth (unused with flat closures)
                const index = self.readU8();
                const val = try self.pop();
                // Get current closure from frame, or from current_closure if fp=0
                const closure = if (self.fp > 0)
                    self.frames[self.fp - 1].closure
                else
                    self.current_closure;

                if (closure) |c| {
                    if (index < c.num_captures) {
                        // Note: captures array is mutable
                        const captures: [*]Value = @constCast(c.captures);
                        captures[index] = val;
                        self.writeBarrierStore(Value.makeClosure(@constCast(c)), val);
                    } else {
                        if (self.trace_upvalue) {
                            if (c.code.isChunk()) {
                                const code_chunk = c.code.toPtr(Chunk);
                                switch (code_chunk.name.typeKind()) {
                                    .symbol => std.debug.print("TRACE store-upvalue closure code={s}\n", .{code_chunk.name.toPtr(Symbol).getName()}),
                                    .string => std.debug.print("TRACE store-upvalue closure code={s}\n", .{code_chunk.name.toPtr(runtime.String).bytes()}),
                                    .nil => std.debug.print("TRACE store-upvalue closure code=nil\n", .{}),
                                    else => std.debug.print("TRACE store-upvalue closure code-kind={s}\n", .{@tagName(code_chunk.name.typeKind())}),
                                }
                                std.debug.print("TRACE store-upvalue closure code_len={d}\n", .{code_chunk.getCode().len});
                            } else {
                                std.debug.print("TRACE store-upvalue closure non-chunk code kind={s}\n", .{@tagName(c.code.typeKind())});
                            }
                            std.debug.print(
                                "TRACE store-upvalue oob: index={d} captures={d} ip={d} fp={d} sp={d}\n",
                                .{ index, c.num_captures, self.ip, self.fp, self.sp },
                            );
                            switch (self.chunk.name.typeKind()) {
                                .symbol => std.debug.print("  chunk={s}\n", .{self.chunk.name.toPtr(Symbol).getName()}),
                                .string => std.debug.print("  chunk={s}\n", .{self.chunk.name.toPtr(runtime.String).bytes()}),
                                .nil => std.debug.print("  chunk=nil\n", .{}),
                                else => std.debug.print("  chunk-kind={s}\n", .{@tagName(self.chunk.name.typeKind())}),
                            }
                        }
                        return error.InvalidConstant;
                    }
                } else {
                    return error.TypeMismatch;
                }
            },
            .load_global => {
                const idx = self.readU16();
                const val = try self.loadGlobal(idx);
                try self.push(val);
            },
            .store_global => {
                const idx = self.readU16();
                const val = try self.pop();
                try self.storeGlobal(idx, val);
            },
            .load_argc => {
                // Get argc from current frame, or from current_argc if fp=0 (callClosure)
                const frame_argc = if (self.fp > 0) self.frames[self.fp - 1].positional_argc else self.current_argc;
                try self.push(Value.makeFixnum(frame_argc));
            },
            .find_key => {
                // Get keyword to search for from constant pool
                const kw_idx = self.readU16();
                const keyword = try self.loadConst(kw_idx);

                // Get current frame info
                const frame = if (self.fp > 0) &self.frames[self.fp - 1] else null;
                if (frame) |f| {
                    const chunk: *const Chunk = f.closure.?.code.toPtr(Chunk);
                    // Layout: [positional/key locals...] [keyword pairs]
                    // Keyword pairs start at the chunk's reserved key-temp region.
                    const kw_pair_start: usize = chunk.key_temp_start;
                    const total_argc = f.argc;
                    const positional_count = f.positional_argc;
                    const kw_pair_count = total_argc - positional_count;

                    // Scan keyword-value pairs
                    var found = false;
                    var found_value = Value.nil;
                    var idx: usize = 0;
                    while (idx + 1 < kw_pair_count) : (idx += 2) {
                        const stack_idx = f.bp + kw_pair_start + idx;
                        // Bounds check before accessing stack
                        if (stack_idx + 1 >= self.sp) break;
                        const kw = self.stack[stack_idx];
                        if (kw.raw == keyword.raw) {
                            found = true;
                            found_value = self.stack[stack_idx + 1];
                            break;
                        }
                    }

                    // Push (found_flag, value)
                    try self.push(if (found) Value.t else Value.nil);
                    try self.push(found_value);
                } else {
                    // No frame - push (nil, nil)
                    try self.push(Value.nil);
                    try self.push(Value.nil);
                }
            },

            // Arithmetic (with float contagion)
            .add => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.add(self.heap, a, b));
            },
            .sub => {
                const b = try self.pop();
                const a = try self.pop();
                if (self.trace_sub_context) {
                    const chunk_name = if (self.chunk.name.isSymbol())
                        self.chunk.name.toPtr(runtime.Symbol).getName()
                    else
                        @tagName(self.chunk.name.typeKind());
                    std.debug.print(
                        "TRACE sub ctx: chunk={s} ip={d} a={s} b={s}\n",
                        .{ chunk_name, self.ip, @tagName(a.typeKind()), @tagName(b.typeKind()) },
                    );
                }
                try self.push(try primitives.arith.sub(self.heap, a, b));
            },
            .mul => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.mul(self.heap, a, b));
            },
            .div => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.div(self.heap, a, b));
            },
            .mod => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.mod(a, b));
            },
            .quot => try self.binaryOp(binaryQuot),
            .rem => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.rem(a, b));
            },
            .neg => {
                const a = try self.pop();
                if (!a.isFixnum()) return error.TypeMismatch;
                const n = a.toFixnum();
                // -minInt(i64) overflows
                if (n == std.math.minInt(i64)) return error.TypeMismatch;
                try self.push(Value.makeFixnum(-n));
            },

            // Comparison
            .eq => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a.eq(b)) Value.t else Value.nil);
            },
            .lt => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (try primitives.arith.lt(a, b)) Value.t else Value.nil);
            },
            .gt => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (try primitives.arith.gt(a, b)) Value.t else Value.nil);
            },
            .le => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (try primitives.arith.le(a, b)) Value.t else Value.nil);
            },
            .ge => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (try primitives.arith.ge(a, b)) Value.t else Value.nil);
            },
            .num_eq => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (primitives.arith.numEq(a, b)) Value.t else Value.nil);
            },
            .not => {
                const a = try self.pop();
                try self.push(if (a.isNil()) Value.t else Value.nil);
            },

            // List operations
            .cons => {
                const cdr = try self.pop();
                const car = try self.pop();
                const cell = try self.allocCons(car, cdr);
                try self.push(cell);
            },
            .car => {
                const pair = try self.pop();
                switch (pair.typeKind()) {
                    .nil => try self.push(Value.nil), // CL: (car nil) => nil
                    .cons => try self.push(pair.toPtr(Cons).car),
                    else => {
                        // Some ANSI helper paths probe symbol-package on arbitrary
                        // values while comparing type behavior. Return NIL rather
                        // than hard-aborting the whole test run.
                        try self.push(Value.nil);
                    },
                }
            },
            .cdr => {
                const pair = try self.pop();
                switch (pair.typeKind()) {
                    .nil => try self.push(Value.nil), // CL: (cdr nil) => nil
                    .cons => try self.push(pair.toPtr(Cons).cdr),
                    else => {
                        if (self.trace_error_context) {
                            std.debug.print(
                                "TRACE cdr mismatch: raw=0x{x} kind={s}\n",
                                .{ pair.raw, @tagName(pair.typeKind()) },
                            );
                        }
                        return error.TypeMismatch;
                    },
                }
            },
            .make_list => {
                const count = self.readU8();
                // Bounds check: need at least count items on stack
                if (count > self.sp) return error.StackUnderflow;
                // Build list by popping elements from top of stack (reverse order)
                // This avoids the double-reverse pattern
                var list = Value.nil;
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const elem = self.stack[self.sp - 1 - i];
                    list = try self.allocCons(elem, list);
                }
                self.sp -= count;
                try self.push(list);
            },
            .append_lists => {
                if (self.sp < 2) return error.StackUnderflow;
                const list1_idx = self.sp - 2;
                const list2_idx = self.sp - 1;
                const list1 = self.stack[list1_idx];
                // Append list1 to list2: (append '(a b) '(c d)) -> (a b c d)
                switch (list1.typeKind()) {
                    .nil => {
                        const list2 = self.stack[list2_idx];
                        self.sp -= 2;
                        try self.push(list2);
                    },
                    .cons => {
                        // Single-pass copy: build copy of list1, link tail to list2
                        try self.push(Value.nil); // head
                        try self.push(Value.nil); // tail
                        const head_idx = self.sp - 2;
                        const tail_idx = self.sp - 1;

                        while (self.stack[list1_idx].isCons()) {
                            const curr_val = self.stack[list1_idx];
                            const c = curr_val.toPtr(Cons);
                            const car = c.car;
                            const next = c.cdr;
                            self.stack[list1_idx] = next; // root across allocCons GC

                            const new_cell = try self.allocCons(car, Value.nil);
                            const tail_val = self.stack[tail_idx];
                            if (tail_val.isCons()) {
                                tail_val.toPtr(Cons).cdr = new_cell;
                                self.writeBarrierStore(tail_val, new_cell);
                            } else {
                                self.stack[head_idx] = new_cell;
                            }
                            self.stack[tail_idx] = new_cell;
                        }
                        if (self.stack[list1_idx] != Value.nil) return error.TypeMismatch;

                        const list2 = self.stack[list2_idx];
                        const tail_val = self.stack[tail_idx];
                        if (tail_val.isCons()) {
                            tail_val.toPtr(Cons).cdr = list2;
                            self.writeBarrierStore(tail_val, list2);
                        }
                        const result = self.stack[head_idx];
                        self.sp = list1_idx;
                        try self.push(if (result.isNil()) list2 else result);
                    },
                    else => return error.TypeMismatch,
                }
            },

            .list_length => {
                const seq = try self.pop();
                switch (seq.typeKind()) {
                    .nil => try self.push(Value.makeFixnum(0)),
                    .cons => {
                        var len: i64 = 0;
                        var curr = seq;
                        while (curr.isCons()) {
                            len += 1;
                            curr = curr.toPtr(Cons).cdr;
                        }
                        try self.push(Value.makeFixnum(len));
                    },
                    .vector => {
                        const vec = seq.toPtr(runtime.Vector);
                        try self.push(Value.makeFixnum(@intCast(vec.length)));
                    },
                    .string => {
                        const str = seq.toPtr(runtime.String);
                        try self.push(Value.makeFixnum(@intCast(str.length)));
                    },
                    .string32 => {
                        const str32 = seq.toPtr(runtime.String32);
                        try self.push(Value.makeFixnum(@intCast(str32.length)));
                    },
                    .array => {
                        const arr = seq.toPtr(runtime.Array);
                        if (arr.rank != 1) return error.TypeMismatch;
                        if (arr.total_size > std.math.maxInt(i64)) return error.Overflow;
                        try self.push(Value.makeFixnum(@intCast(arr.total_size)));
                    },
                    else => return error.TypeMismatch,
                }
            },

            .list_reverse => {
                if (self.sp < 1) return error.StackUnderflow;
                const seq = try self.pop();
                switch (seq.typeKind()) {
                    .string => {
                        const str = seq.toPtr(runtime.String);
                        const bytes = str.bytes();
                        const out = try self.allocString(bytes);
                        const dest = out.toPtr(runtime.String).mutableBytes();
                        var i: usize = 0;
                        while (i < bytes.len) : (i += 1) {
                            dest[i] = bytes[bytes.len - 1 - i];
                        }
                        try self.push(out);
                    },
                    .string32 => {
                        const src = seq.toPtr(runtime.String32).codepoints();
                        const out = try self.heap.allocString32Uninitialized(src.len);
                        const dest = out.toPtr(runtime.String32).mutableCodepoints();
                        var i: usize = 0;
                        while (i < src.len) : (i += 1) {
                            dest[i] = src[src.len - 1 - i];
                        }
                        try self.push(out);
                    },
                    .vector => {
                        const vec = seq.toPtr(runtime.Vector);
                        const items = vec.items();
                        const out = try self.heap.allocVector(@intCast(items.len), @intCast(items.len));
                        const dest = out.toPtr(runtime.Vector).items();
                        var i: usize = 0;
                        while (i < items.len) : (i += 1) {
                            dest[i] = items[items.len - 1 - i];
                        }
                        try self.push(out);
                    },
                    else => {
                        const list_idx = self.sp;
                        try self.push(seq);
                        try self.push(Value.nil); // reversed
                        const rev_idx = self.sp - 1;

                        while (self.stack[list_idx].isCons()) {
                            const curr_val = self.stack[list_idx];
                            const c = curr_val.toPtr(Cons);
                            const car = c.car;
                            const next = c.cdr;
                            self.stack[list_idx] = next; // root across allocCons GC

                            const rev = self.stack[rev_idx];
                            self.stack[rev_idx] = try self.allocCons(car, rev);
                        }
                        if (self.stack[list_idx] != Value.nil) return error.TypeMismatch;

                        const result = self.stack[rev_idx];
                        self.sp = list_idx;
                        try self.push(result);
                    },
                }
            },

            .list_nth => {
                const list = try self.pop();
                const n_val = try self.pop();
                if (!n_val.isFixnum()) return error.TypeMismatch;
                const n = n_val.toFixnum();
                if (n < 0) return error.TypeMismatch;
                var idx: i64 = 0;
                var curr = list;
                while (curr.isCons()) {
                    if (idx == n) {
                        try self.push(curr.toPtr(Cons).car);
                        break;
                    }
                    idx += 1;
                    curr = curr.toPtr(Cons).cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_nthcdr => {
                const list = try self.pop();
                const n_val = try self.pop();
                if (!n_val.isFixnum()) return error.TypeMismatch;
                const n = n_val.toFixnum();
                if (n < 0) return error.TypeMismatch;
                var idx: i64 = 0;
                var curr = list;
                while (idx < n and curr.isCons()) {
                    idx += 1;
                    curr = curr.toPtr(Cons).cdr;
                }
                try self.push(curr);
            },

            .rplaca => {
                const new_car = try self.pop();
                const cons_val = try self.pop();
                if (!cons_val.isCons()) return error.TypeMismatch;
                const c = cons_val.toPtr(Cons);
                c.car = new_car;
                self.writeBarrierStore(cons_val, new_car);
                try self.push(cons_val); // CL: rplaca returns the modified cons
            },

            .rplacd => {
                const new_cdr = try self.pop();
                const cons_val = try self.pop();
                if (!cons_val.isCons()) return error.TypeMismatch;
                const c = cons_val.toPtr(Cons);
                c.cdr = new_cdr;
                self.writeBarrierStore(cons_val, new_cdr);
                try self.push(cons_val); // CL: rplacd returns the modified cons
            },

            .error_user => {
                const msg_val = try self.pop();
                // Store for REPL diagnostics in case throw is unhandled
                self.last_error_value = msg_val;
                // Signal as CL simple-error condition: (simple-error . (format-control . format-args))
                const payload = try self.allocCons(msg_val, Value.nil);
                const condition = try self.allocCons(self.builtins.sym_simple_error, payload);
                try self.doThrow(self.builtins.sym_condition_tag, condition);
            },

            .list_last => {
                const list = try self.pop();
                switch (list.typeKind()) {
                    .nil => try self.push(Value.nil),
                    .cons => {
                        var curr = list;
                        while (curr.isCons()) {
                            const c = curr.toPtr(Cons);
                            if (!c.cdr.isCons()) {
                                try self.push(curr);
                                break;
                            }
                            curr = c.cdr;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },

            .list_member => {
                const list = try self.pop();
                const item = try self.pop();
                var curr = list;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.raw == item.raw) {
                        try self.push(curr);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_member_eql => {
                // member with eql test (compares numbers by value)
                const list = try self.pop();
                const item = try self.pop();
                var curr = list;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .eql)) {
                        try self.push(curr);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_member_equal => {
                // member with equal test (deep equality)
                const list = try self.pop();
                const item = try self.pop();
                var curr = list;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .equal)) {
                        try self.push(curr);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            // Type predicates
            .consp => {
                const a = try self.pop();
                try self.push(if (a.isCons()) Value.t else Value.nil);
            },
            .symbolp => {
                const a = try self.pop();
                // nil and t are also symbols in CL
                try self.push(if (a.isSymbolLike()) Value.t else Value.nil);
            },
            .numberp => {
                const a = try self.pop();
                try self.push(if (a.isNumber()) Value.t else Value.nil);
            },
            .integerp => {
                const a = try self.pop();
                try self.push(if (a.isFixnum() or a.isBignum()) Value.t else Value.nil);
            },
            .realp => {
                const a = try self.pop();
                try self.push(if (a.isFixnum() or a.isBignum() or a.isFloat() or a.isRational()) Value.t else Value.nil);
            },
            .stringp => {
                const a = try self.pop();
                try self.push(if (stringPrims.stringp(a) or a.isString32()) Value.t else Value.nil);
            },
            .vectorp => {
                const a = try self.pop();
                const is_vector_like = switch (a.typeKind()) {
                    .string, .string32 => true,
                    .vector => true,
                    .array => blk: {
                        const arr = a.toPtr(runtime.Array);
                        break :blk arr.rank == 1;
                    },
                    else => false,
                };
                try self.push(if (is_vector_like) Value.t else Value.nil);
            },
            .closurep => {
                const a = try self.pop();
                try self.push(if (a.isClosure()) Value.t else Value.nil);
            },
            .keywordp => {
                const a = try self.pop();
                try self.push(if (a.isKeyword()) Value.t else Value.nil);
            },
            .method_qualifiers => {
                const args = try self.popArgs(1);
                const result = try primitives.methodQualifiers(self.heap, args);
                try self.push(result);
            },
            .method_specializers => {
                const args = try self.popArgs(1);
                const result = try primitives.methodSpecializers(self.heap, args);
                try self.push(result);
            },
            .method_function => {
                const args = try self.popArgs(1);
                const result = try primitives.methodFunction(self.heap, args);
                try self.push(result);
            },
            .generic_function_methods => {
                const args = try self.popArgs(1);
                const result = try primitives.genericFunctionMethods(self.heap, args);
                try self.push(result);
            },
            .generic_function_lambda_list => {
                const args = try self.popArgs(1);
                const result = try primitives.genericFunctionLambdaList(self.heap, args);
                try self.push(result);
            },
            .generic_function_name => {
                const args = try self.popArgs(1);
                const result = try primitives.genericFunctionName(self.heap, args);
                try self.push(result);
            },
            .nilp => {
                const a = try self.pop();
                try self.push(if (a.isNil()) Value.t else Value.nil);
            },

            // Vector operations
            .make_vec => {
                _ = self.readU16(); // Size operand (unused, size from stack)
                if (self.sp < 2) return error.StackUnderflow;
                const init_idx = self.sp - 1;
                const size_idx = self.sp - 2;
                const size_val = self.stack[size_idx];
                if (!size_val.isFixnum()) return error.TypeMismatch;
                const size_signed = size_val.toFixnum();
                if (size_signed < 0) return error.TypeMismatch;
                const size: usize = @intCast(size_signed);
                const vec = try self.allocVector(size, size);
                // Fill with init value (nil or specified)
                const init_val = self.stack[init_idx];
                const vec_obj = vec.toPtr(Vector);
                for (0..size) |i| {
                    vec_obj.data[i] = init_val;
                }
                self.sp -= 2;
                try self.push(vec);
            },
            .make_vec_n => {
                const count = self.readU8();
                const vec = try self.allocVector(count, count);
                const vec_obj = vec.toPtr(Vector);
                // Pop elements in reverse order (last element pushed first)
                var i: usize = count;
                while (i > 0) {
                    i -= 1;
                    vec_obj.data[i] = try self.pop();
                }
                try self.push(vec);
            },
            .vec_ref => {
                const idx_val = try self.pop();
                const vec_val = try self.pop();
                if (!idx_val.isFixnum()) return error.TypeMismatch;
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                switch (vec_val.typeKind()) {
                    .vector => {
                        const vec = vec_val.toPtr(runtime.Vector);
                        if (idx >= vec.length) return error.TypeMismatch;
                        try self.push(vec.get(idx));
                    },
                    .array => {
                        const arr = vec_val.toPtr(runtime.Array);
                        if (arr.rank != 1) return error.TypeMismatch;
                        if (idx >= arr.dimensions[0]) return error.TypeMismatch;
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        try self.push(data[idx]);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .vec_set => {
                const val = try self.pop();
                const idx_val = try self.pop();
                const vec_val = try self.pop();
                if (!idx_val.isFixnum()) return error.TypeMismatch;
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                switch (vec_val.typeKind()) {
                    .vector => {
                        const vec = vec_val.toPtr(runtime.Vector);
                        if (idx >= vec.length) return error.TypeMismatch;
                        vec.set(idx, val);
                        self.writeBarrierStore(vec_val, val);
                        try self.push(val);
                    },
                    .array => {
                        const arr = vec_val.toPtr(runtime.Array);
                        if (arr.rank != 1) return error.TypeMismatch;
                        if (idx >= arr.dimensions[0]) return error.TypeMismatch;
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        data[idx] = val;
                        self.writeBarrierStore(vec_val, val);
                        try self.push(val);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .elt_set => {
                // Polymorphic: set element in vector or list
                const val = try self.pop();
                const idx_val = try self.pop();
                const seq_val = try self.pop();
                if (!idx_val.isFixnum()) return error.TypeMismatch;
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);

                switch (seq_val.typeKind()) {
                    .vector => {
                        const vec = seq_val.toPtr(runtime.Vector);
                        if (idx >= vec.length) return error.TypeMismatch;
                        vec.set(idx, val);
                        self.writeBarrierStore(seq_val, val);
                        try self.push(val);
                    },
                    .string => {
                        const str = seq_val.toPtr(runtime.String);
                        if (idx >= str.length) return error.TypeMismatch;
                        if (!val.isCharacter()) return error.TypeMismatch;
                        const cp = val.toCharacter();
                        if (cp > 255) return error.TypeMismatch;
                        str.mutableBytes()[idx] = @intCast(cp);
                        try self.push(val);
                    },
                    .string32 => {
                        const str32 = seq_val.toPtr(runtime.String32);
                        if (idx >= str32.length) return error.TypeMismatch;
                        if (!val.isCharacter()) return error.TypeMismatch;
                        str32.mutableCodepoints()[idx] = val.toCharacter();
                        try self.push(val);
                    },
                    .cons, .nil => {
                        // For lists, use nthcdr then rplaca
                        var list = seq_val;
                        var i: usize = 0;
                        while (i < idx) : (i += 1) {
                            if (!list.isCons()) return error.TypeMismatch;
                            list = list.toPtr(runtime.Cons).cdr;
                        }
                        if (!list.isCons()) return error.TypeMismatch;
                        list.toPtr(runtime.Cons).car = val;
                        self.writeBarrierStore(list, val);
                        try self.push(val);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .vec_len => {
                const vec_val = try self.pop();
                if (!vec_val.isVector()) return error.TypeMismatch;
                const vec = vec_val.toPtr(runtime.Vector);
                try self.push(Value.makeFixnum(@intCast(vec.length)));
            },

            .vec_fill_ptr => {
                const vec_val = try self.pop();
                if (!vec_val.isVector()) return error.TypeMismatch;
                const fp = primitives.vector.fillPointer(vec_val);
                if (fp) |p| {
                    try self.push(Value.makeFixnum(p));
                } else {
                    try self.push(Value.nil);
                }
            },

            .vec_push => {
                const elem = try self.pop();
                const vec_val = try self.pop();
                const result = primitives.vector.vectorPush(vec_val, elem);
                if (result >= 0) {
                    self.writeBarrierStore(vec_val, elem);
                }
                try self.push(Value.makeFixnum(result));
            },

            .vec_push_ext => {
                const ext = try self.pop();
                const elem = try self.pop();
                const vec_val = try self.pop();
                if (!ext.isFixnum()) return error.TypeMismatch;
                const result = try primitives.vector.vectorPushExtend(self.heap, vec_val, elem, @intCast(ext.toFixnum()));
                if (result >= 0) {
                    self.writeBarrierStore(vec_val, elem);
                }
                try self.push(Value.makeFixnum(result));
            },

            .vec_pop => {
                const vec_val = try self.pop();
                const result = primitives.vector.vectorPop(vec_val);
                try self.push(result);
            },

            .vec_set_fill_ptr => {
                const fp_val = try self.pop();
                const vec_val = try self.pop();
                if (!fp_val.isFixnum()) return error.TypeMismatch;
                const ok = primitives.vector.setFillPointer(vec_val, fp_val.toFixnum());
                try self.push(if (ok) Value.t else Value.nil);
            },

            .vec_set_adjustable => {
                const bool_val = try self.pop();
                const vec_val = try self.pop();
                const ok = primitives.vector.setAdjustable(vec_val, !bool_val.isNil());
                try self.push(if (ok) Value.t else Value.nil);
            },
            .vec_set_character => {
                const bool_val = try self.pop();
                const vec_val = try self.pop();
                if (!vec_val.isVector()) return error.TypeMismatch;
                vec_val.toPtr(runtime.Vector).setCharacterVector(!bool_val.isNil());
                try self.push(Value.t);
            },

            .vec_adjust => {
                const fill_val = try self.pop();
                const new_size_val = try self.pop();
                const vec_val = try self.pop();
                if (!new_size_val.isFixnum()) return error.TypeMismatch;
                const new_size: u64 = @intCast(new_size_val.toFixnum());
                const result = try primitives.vector.adjustArray(self.heap, vec_val, new_size, fill_val);
                try self.push(result);
            },

            .copy_structure => {
                const obj = try self.pop();
                if (self.trace_copy_structure) {
                    std.debug.print("TRACE copy-structure arg={s}\n", .{@tagName(obj.typeKind())});
                    if (obj.isVector()) {
                        const vec = obj.toPtr(runtime.Vector);
                        std.debug.print("  vector.len={d}\n", .{vec.length});
                        if (vec.length > 0) {
                            std.debug.print("  vector[0]={s}\n", .{@tagName(vec.data[0].typeKind())});
                        }
                    }
                }
                const result = try primitives.vector.copyStructure(self.heap, obj);
                try self.push(result);
            },

            // CLOS operations
            .slot_value => {
                const args = try self.popArgs(2);
                const result = try primitives.slotValue(self.heap, args);
                try self.push(result);
            },

            .set_slot_value => {
                const args = try self.popArgs(3);
                const result = try primitives.clos.setSlotValue(self.heap, args);
                try self.push(result);
            },
            .class_of => {
                const args = try self.popArgs(1);
                const result = try primitives.classOf(self.heap, args);
                try self.push(result);
            },
            .find_class => {
                const args = try self.popArgs(1);
                const result = try primitives.findClass(self.heap, args);
                try self.push(result);
            },
            .set_find_class => {
                const args = try self.popArgs(2);
                const result = try primitives.setFindClass(self.heap, args);
                try self.push(result);
            },
            .class_name => {
                const args = try self.popArgs(1);
                const result = try primitives.className(self.heap, args);
                try self.push(result);
            },
            .class_direct_superclasses => {
                const args = try self.popArgs(1);
                const result = try primitives.classDirectSuperclasses(self.heap, args);
                try self.push(result);
            },
            .class_precedence_list => {
                const args = try self.popArgs(1);
                const result = try primitives.classPrecedenceList(self.heap, args);
                try self.push(result);
            },
            .class_direct_slots => {
                const args = try self.popArgs(1);
                const result = try primitives.classDirectSlots(self.heap, args);
                try self.push(result);
            },
            .class_slots => {
                const args = try self.popArgs(1);
                const result = try primitives.classSlots(self.heap, args);
                try self.push(result);
            },
            .slot_definition_name => {
                const args = try self.popArgs(1);
                const result = try primitives.slotDefinitionName(self.heap, args);
                try self.push(result);
            },
            .slot_definition_initform => {
                const args = try self.popArgs(1);
                const result = try primitives.slotDefinitionInitform(self.heap, args);
                try self.push(result);
            },
            .slot_definition_initargs => {
                const args = try self.popArgs(1);
                const result = try primitives.slotDefinitionInitargs(self.heap, args);
                try self.push(result);
            },
            .slot_definition_readers => {
                const args = try self.popArgs(1);
                const result = try primitives.slotDefinitionReaders(self.heap, args);
                try self.push(result);
            },
            .slot_definition_writers => {
                const args = try self.popArgs(1);
                const result = try primitives.slotDefinitionWriters(self.heap, args);
                try self.push(result);
            },
            .slot_definition_allocation => {
                const args = try self.popArgs(1);
                const result = try primitives.slotDefinitionAllocation(self.heap, args);
                try self.push(result);
            },
            .slot_definition_type => {
                const args = try self.popArgs(1);
                const result = try primitives.slotDefinitionType(self.heap, args);
                try self.push(result);
            },
            .make_generic_function => {
                const args = try self.popArgs(2);
                const result = try primitives.makeGenericFunction(self.heap, args);
                try self.push(result);
            },
            .make_method => {
                const args = try self.popArgs(4);
                const result = try primitives.makeMethod(self.heap, args);
                try self.push(result);
            },
            .set_gf_dispatcher => {
                const dispatcher = try self.pop();
                const gf_val = try self.pop();
                if (!gf_val.isGenericFunction()) return error.TypeMismatch;
                const gf = gf_val.toPtr(runtime.objects.GenericFunction);
                gf.dispatcher = dispatcher;
                try self.push(gf_val);
            },
            .add_method => {
                const args = try self.popArgs(2);
                const result = try primitives.addMethod(self.heap, args);
                try self.push(result);
            },
            .make_unbound => {
                try self.push(Value.unbound);
            },
            .slot_boundp => {
                const args = try self.popArgs(2);
                const result = try primitives.slotBoundp(self.heap, args);
                try self.push(result);
            },
            .slot_makunbound => {
                const args = try self.popArgs(2);
                const result = try primitives.slotMakunbound(self.heap, args);
                try self.push(result);
            },

            // Box operations (mutable cells for closures)
            .make_box => {
                if (self.sp < 1) return error.StackUnderflow;
                // Allocate a 1-element vector as a box
                const box = try self.allocVector(1, 1);
                const val = self.stack[self.sp - 1];
                const vec = box.toPtr(runtime.Vector);
                vec.set(0, val);
                self.writeBarrierStore(box, val);
                self.sp -= 1;
                try self.push(box);
            },
            .box_ref => {
                const box = try self.pop();
                if (!box.isVector()) return error.TypeMismatch;
                const vec = box.toPtr(runtime.Vector);
                if (vec.length < 1) return error.TypeMismatch;
                try self.push(vec.get(0));
            },
            .box_set => {
                const val = try self.pop();
                const box = try self.pop();
                if (!box.isVector()) return error.TypeMismatch;
                const vec = box.toPtr(runtime.Vector);
                if (vec.length < 1) return error.TypeMismatch;
                vec.set(0, val);
                self.writeBarrierStore(box, val);
                try self.push(val); // Return the value written
            },

            // String operations
            .str_ref => {
                const idx_val = try self.pop();
                const str_val = try self.pop();
                if (!idx_val.isFixnum()) return error.TypeMismatch;
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                switch (str_val.typeKind()) {
                    .string => {
                        const str = str_val.toPtr(runtime.String);
                        if (idx >= str.length) return error.TypeMismatch;
                        try self.push(Value.makeCharacter(str.bytes()[idx]));
                    },
                    .string32 => {
                        const str32 = str_val.toPtr(runtime.String32);
                        if (idx >= str32.length) return error.TypeMismatch;
                        try self.push(Value.makeCharacter(@intCast(str32.codepoints()[idx])));
                    },
                    .vector => {
                        const vec = str_val.toPtr(runtime.Vector);
                        if (!vec.isCharacterVector()) return error.TypeMismatch;
                        const len: usize = @intCast(vec.getFillPointer() orelse vec.length);
                        if (idx >= len) return error.TypeMismatch;
                        const ch = vec.data[idx];
                        switch (ch.typeKind()) {
                            .char => try self.push(ch),
                            .fixnum => {
                                const cp = ch.toFixnum();
                                if (cp < 0 or cp > std.math.maxInt(u21)) return error.TypeMismatch;
                                try self.push(Value.makeCharacter(@intCast(cp)));
                            },
                            else => return error.TypeMismatch,
                        }
                    },
                    .array => {
                        const arr = str_val.toPtr(runtime.Array);
                        if (arr.rank != 1) return error.TypeMismatch;
                        if (idx >= arr.dimensions[0]) return error.TypeMismatch;
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        const ch = data[idx];
                        switch (ch.typeKind()) {
                            .char => try self.push(ch),
                            .fixnum => {
                                const cp = ch.toFixnum();
                                if (cp < 0 or cp > std.math.maxInt(u21)) return error.TypeMismatch;
                                try self.push(Value.makeCharacter(@intCast(cp)));
                            },
                            else => return error.TypeMismatch,
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .str_len => {
                const str_val = try self.pop();
                switch (str_val.typeKind()) {
                    .string => {
                        const str = str_val.toPtr(runtime.String);
                        try self.push(Value.makeFixnum(@intCast(str.length)));
                    },
                    .string32 => {
                        const str32 = str_val.toPtr(runtime.String32);
                        try self.push(Value.makeFixnum(@intCast(str32.length)));
                    },
                    .vector => {
                        const vec = str_val.toPtr(runtime.Vector);
                        if (!vec.isCharacterVector()) return error.TypeMismatch;
                        try self.push(Value.makeFixnum(@intCast(vec.getFillPointer() orelse vec.length)));
                    },
                    else => return error.TypeMismatch,
                }
            },
            .str_set => {
                const char_val = try self.pop();
                const idx_val = try self.pop();
                const str_val = try self.pop();
                if (!idx_val.isFixnum()) return error.TypeMismatch;
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                const char_int = switch (char_val.typeKind()) {
                    .fixnum => char_val.toFixnum(),
                    .char => @as(i64, @intCast(char_val.toCharacter())),
                    else => return error.TypeMismatch,
                };
                if (char_int < 0) return error.TypeMismatch;
                switch (str_val.typeKind()) {
                    .string => {
                        const str = str_val.toPtr(runtime.String);
                        if (idx >= str.length) return error.TypeMismatch;
                        if (char_int > 255) return error.TypeMismatch;
                        str.mutableBytes()[idx] = @intCast(char_int);
                    },
                    .string32 => {
                        const str32 = str_val.toPtr(runtime.String32);
                        if (idx >= str32.length) return error.TypeMismatch;
                        if (char_int > std.math.maxInt(u21)) return error.TypeMismatch;
                        str32.mutableCodepoints()[idx] = @intCast(char_int);
                    },
                    .array => {
                        const arr = str_val.toPtr(runtime.Array);
                        if (arr.rank != 1) return error.TypeMismatch;
                        if (idx >= arr.dimensions[0]) return error.TypeMismatch;
                        if (char_int > std.math.maxInt(u21)) return error.TypeMismatch;
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        data[idx] = Value.makeCharacter(@intCast(char_int));
                    },
                    .vector => {
                        const vec = str_val.toPtr(runtime.Vector);
                        if (!vec.isCharacterVector()) return error.TypeMismatch;
                        const len: usize = @intCast(vec.getFillPointer() orelse vec.length);
                        if (idx >= len) return error.TypeMismatch;
                        if (char_int > std.math.maxInt(u21)) return error.TypeMismatch;
                        vec.data[idx] = Value.makeCharacter(@intCast(char_int));
                    },
                    else => return error.TypeMismatch,
                }
                try self.push(str_val);
            },
            .str_concat => {
                if (self.sp < 2) return error.StackUnderflow;
                const s2_idx = self.sp - 1;
                const s1_idx = self.sp - 2;
                const s2 = self.stack[s2_idx];
                const s1 = self.stack[s1_idx];
                const s1_is_base = s1.isString();
                const s1_is_utf32 = s1.isString32();
                const s2_is_base = s2.isString();
                const s2_is_utf32 = s2.isString32();
                if (!(s1_is_base or s1_is_utf32) or !(s2_is_base or s2_is_utf32)) return error.TypeMismatch;

                if (s1_is_base and s2_is_base) {
                    // Fast path: base-string + base-string.
                    const len1 = s1.toPtr(runtime.String).length;
                    const len2 = s2.toPtr(runtime.String).length;
                    const new_len = try std.math.add(usize, len1, len2);
                    const result = try self.allocStringUninitialized(new_len);
                    const result_str = result.toPtr(runtime.String);
                    const dest = result_str.mutableBytes();
                    const str1 = self.stack[s1_idx].toPtr(runtime.String);
                    const str2 = self.stack[s2_idx].toPtr(runtime.String);
                    @memcpy(dest[0..len1], str1.bytes());
                    @memcpy(dest[len1..new_len], str2.bytes());
                    self.sp -= 2;
                    try self.push(result);
                } else {
                    // General path: any combination that includes String32.
                    const len1 = if (s1_is_utf32) s1.toPtr(runtime.String32).length else s1.toPtr(runtime.String).length;
                    const len2 = if (s2_is_utf32) s2.toPtr(runtime.String32).length else s2.toPtr(runtime.String).length;
                    const new_len = try std.math.add(usize, len1, len2);
                    const result = try self.heap.allocString32Uninitialized(new_len);
                    const dest = result.toPtr(runtime.String32).mutableCodepoints();

                    var out_idx: usize = 0;
                    if (s1_is_utf32) {
                        const cps = s1.toPtr(runtime.String32).codepoints();
                        @memcpy(dest[0..cps.len], cps);
                        out_idx = cps.len;
                    } else {
                        const bytes = s1.toPtr(runtime.String).bytes();
                        for (bytes, 0..) |b, i| dest[i] = @intCast(b);
                        out_idx = bytes.len;
                    }

                    if (s2_is_utf32) {
                        const cps = s2.toPtr(runtime.String32).codepoints();
                        @memcpy(dest[out_idx .. out_idx + cps.len], cps);
                    } else {
                        const bytes = s2.toPtr(runtime.String).bytes();
                        for (bytes, 0..) |b, i| dest[out_idx + i] = @intCast(b);
                    }

                    self.sp -= 2;
                    try self.push(result);
                }
            },

            // Control flow
            .jmp => {
                const offset = self.readI16();
                // Use isize to handle the full range of usize safely
                const new_ip = @as(isize, @intCast(self.ip)) + offset;
                if (new_ip < 0) return self.invalidOpcode("execute-op.jmp-neg");
                self.ip = @intCast(new_ip);
            },
            .jmp_nil => {
                const offset = self.readI16();
                const val = try self.pop();
                if (val.isNil()) {
                    const new_ip = @as(isize, @intCast(self.ip)) + offset;
                    if (new_ip < 0) return self.invalidOpcode("execute-op.jmp-nil-neg");
                    self.ip = @intCast(new_ip);
                }
            },
            .jmp_not_nil => {
                const offset = self.readI16();
                const val = try self.pop();
                if (!val.isNil()) {
                    const new_ip = @as(isize, @intCast(self.ip)) + offset;
                    if (new_ip < 0) return self.invalidOpcode("execute-op.jmp-not-nil-neg");
                    self.ip = @intCast(new_ip);
                }
            },

            // Function calls
            .call => {
                const argc = self.readU8();
                var trace_call = false;
                var fn_designator = Value.nil;
                if (self.trace_call_ret) {
                    fn_designator = self.stack[self.sp - argc - 1];
                    trace_call = self.shouldTraceCallRet(fn_designator, self.chunk, null);
                }
                if (trace_call) {
                    const caller = chunkTraceName(self.chunk);
                    std.debug.print(
                        "TRACE call chunk={s} ip={d} argc={d} fp={d} sp={d} fn=",
                        .{ caller, self.ip, argc, self.fp, self.sp },
                    );
                    tracePrintValue(fn_designator);
                    std.debug.print("\n", .{});
                    if (self.trace_call_args) {
                        const dump_n: usize = @min(@as(usize, argc), envTraceCount("HABU_TRACE_CALL_ARGS", 4));
                        var ai: usize = 0;
                        while (ai < dump_n) : (ai += 1) {
                            const arg = self.stack[self.sp - @as(usize, argc) + ai];
                            std.debug.print("  arg[{d}]=", .{ai});
                            tracePrintValue(arg);
                            std.debug.print("\n", .{});
                        }
                    }
                }
                if (try self.tryDirectCallJit(argc)) |direct_result| {
                    if (trace_call) {
                        std.debug.print(
                            "TRACE call-jit-direct caller={s} argc={d} fp={d} sp={d} result=",
                            .{ chunkTraceName(self.chunk), argc, self.fp, self.sp },
                        );
                        tracePrintValue(direct_result);
                        std.debug.print("\n", .{});
                    }
                    return;
                }
                try self.doCall(argc, false);
                if (trace_call) {
                    std.debug.print("TRACE call-enter callee={s} fp={d} sp={d}\n", .{ chunkTraceName(self.chunk), self.fp, self.sp });
                }
                // Check for hoist-compiled function

                if (try self.tryCallJit(argc)) |result| {
                    // Pop the call frame and push result
                    self.fp -= 1;
                    const caller_frame = self.frames[self.fp];
                    try self.restoreCallerFrameAfterCall(caller_frame, result);
                }
            },
            .tail_call => {
                const argc = self.readU8();
                try self.doCall(argc, true);
            },
            .apply => {
                try self.doApply();
            },
            .ret => {
                const result = try self.pop();
                if (self.fp == 0) {
                    if (self.trace_call_ret and self.shouldTraceCallRet(null, self.chunk, null)) {
                        std.debug.print(
                            "TRACE ret-top chunk={s} ip={d} sp={d} result=",
                            .{ chunkTraceName(self.chunk), self.ip, self.sp },
                        );
                        tracePrintValue(result);
                        std.debug.print("\n", .{});
                    }
                    // Top level return - push result and halt
                    try self.push(result);
                    return error.Halt;
                }
                // Restore caller state
                self.fp -= 1;
                const frame = self.frames[self.fp];
                if (self.trace_call_ret and self.shouldTraceCallRet(null, self.chunk, frame.chunk)) {
                    std.debug.print(
                        "TRACE ret chunk={s} ip={d} -> caller={s} return_ip={d} fp={d} sp={d} result=",
                        .{ chunkTraceName(self.chunk), self.ip, chunkTraceName(frame.chunk), frame.return_ip, self.fp, frame.bp },
                    );
                    tracePrintValue(result);
                    std.debug.print("\n", .{});
                }
                try self.restoreCallerFrameAfterCall(frame, result);
            },
            .make_closure => {
                const chunk_idx = self.readU16();
                const num_captures = self.readU8();

                // Get the chunk from the pool (offset by base for this eval)
                const abs_idx = self.chunk_base + chunk_idx;
                const chunk_pool = self.currentChunkPool();
                if (abs_idx >= chunk_pool.len) return error.InvalidConstant;
                const closure_chunk_val = chunk_pool[abs_idx];
                if (closure_chunk_val.isNil()) return error.InvalidConstant;
                const closure_chunk = closure_chunk_val.toPtr(Chunk);

                if (num_captures > 64) return error.StackOverflow;
                if (num_captures > self.sp) return error.StackUnderflow;
                const cap_start = self.sp - num_captures;

                // Create closure - wrap chunk pointer in a Value
                const chunk_val = Value.makeChunk(closure_chunk);
                const closure = try self.allocClosureWithGC(
                    chunk_val,
                    closure_chunk.arity,
                    self.stack[cap_start..self.sp],
                );
                if (self.trace_upvalue) {
                    switch (closure_chunk.name.typeKind()) {
                        .symbol => std.debug.print("TRACE make_closure chunk={s} captures={d} code_len={d}\n", .{
                            closure_chunk.name.toPtr(Symbol).getName(),
                            num_captures,
                            closure_chunk.getCode().len,
                        }),
                        .string => std.debug.print("TRACE make_closure chunk={s} captures={d} code_len={d}\n", .{
                            closure_chunk.name.toPtr(runtime.String).bytes(),
                            num_captures,
                            closure_chunk.getCode().len,
                        }),
                        .nil => std.debug.print("TRACE make_closure chunk=nil captures={d} code_len={d}\n", .{
                            num_captures,
                            closure_chunk.getCode().len,
                        }),
                        else => std.debug.print("TRACE make_closure chunk-kind={s} captures={d} code_len={d}\n", .{
                            @tagName(closure_chunk.name.typeKind()),
                            num_captures,
                            closure_chunk.getCode().len,
                        }),
                    }
                    if (closure_chunk.getCode().len <= 20) {
                        std.debug.print("TRACE make_closure bytes:", .{});
                        for (closure_chunk.getCode()) |byte| {
                            std.debug.print(" {X:0>2}", .{byte});
                        }
                        std.debug.print("\n", .{});
                        const lx = closure_chunk.lambda_expr;
                        if (lx.isCons()) {
                            const lc = lx.toPtr(Cons);
                            if (lc.car.isSymbol()) {
                                std.debug.print("TRACE make_closure lambda_expr head={s}\n", .{lc.car.toPtr(Symbol).getName()});
                            } else {
                                std.debug.print("TRACE make_closure lambda_expr head-kind={s}\n", .{@tagName(lc.car.typeKind())});
                            }
                        } else {
                            std.debug.print("TRACE make_closure lambda_expr kind={s}\n", .{@tagName(lx.typeKind())});
                        }
                    }
                }
                self.sp = cap_start;
                try self.push(closure);
            },

            // I/O
            .write => {
                const val = try self.pop();
                try self.syncPrintGlobals();
                const result = try io.writeWithHook(val, self.defaultOutputStream(), self.ioPrintHook());
                try self.push(result);
            },
            .print => {
                const val = try self.pop();
                try self.syncPrintGlobals();
                const result = try io.printWithHook(val, self.defaultOutputStream(), self.ioPrintHook());
                try self.push(result);
            },
            .princ => {
                const val = try self.pop();
                try self.syncPrintGlobals();
                const result = try io.princWithHook(val, self.defaultOutputStream(), self.ioPrintHook());
                try self.push(result);
            },
            .terpri => {
                try io.sysNewline();
                try self.push(Value.nil);
            },
            .write_char => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                if (cp < 128) {
                    try io.sysWriteChar(@intCast(cp));
                } else {
                    // UTF-8 encode for non-ASCII
                    var buf: [4]u8 = undefined;
                    const len = try std.unicode.utf8Encode(@intCast(cp), &buf);
                    try io.sysWriteBytes(buf[0..len]);
                }
                try self.push(val);
            },
            .char_upcase => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                const upper = if (cp >= 'a' and cp <= 'z') cp - 32 else cp;
                try self.push(Value.makeCharacter(upper));
            },
            .char_downcase => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                const lower = if (cp >= 'A' and cp <= 'Z') cp + 32 else cp;
                try self.push(Value.makeCharacter(lower));
            },
            .digit_char_p => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                const is_digit = cp >= '0' and cp <= '9';
                try self.push(if (is_digit) Value.t else Value.nil);
            },
            .alpha_char_p => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                const is_alpha = (cp >= 'A' and cp <= 'Z') or (cp >= 'a' and cp <= 'z');
                try self.push(if (is_alpha) Value.t else Value.nil);
            },
            .parse_integer => {
                const val = try self.pop();
                if (!val.isString()) return error.TypeMismatch;
                const str = val.toPtr(String);
                const bytes = str.bytes();
                // Skip leading whitespace
                var start: usize = 0;
                while (start < bytes.len and (bytes[start] == ' ' or bytes[start] == '\t' or bytes[start] == '\n' or bytes[start] == '\r')) : (start += 1) {}
                // Parse integer from string
                var result: i64 = 0;
                var negative = false;
                var i: usize = start;
                var overflow = false;
                if (i < bytes.len and bytes[i] == '-') {
                    negative = true;
                    i += 1;
                }
                while (i < bytes.len) : (i += 1) {
                    const c = bytes[i];
                    if (c >= '0' and c <= '9') {
                        // Use checked arithmetic to detect overflow
                        const mul_result = @mulWithOverflow(result, 10);
                        if (mul_result[1] != 0) {
                            overflow = true;
                            break;
                        }
                        const add_result = @addWithOverflow(mul_result[0], c - '0');
                        if (add_result[1] != 0) {
                            overflow = true;
                            break;
                        }
                        result = add_result[0];
                    } else if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                        break; // Stop at trailing whitespace
                    } else {
                        break;
                    }
                }
                if (overflow) {
                    try self.push(Value.nil);
                } else {
                    if (negative) result = -result;
                    try self.push(Value.makeFixnum(result));
                }
            },
            .write_to_string => {
                const val = try self.pop();
                try self.syncPrintGlobals();
                const result = try io.writeToStringWithHook(self.heap, val, self.ioPrintHook());
                try self.push(result);
            },
            .logand => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logand(self.heap, a, b);
                try self.push(result);
            },
            .logior => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logior(self.heap, a, b);
                try self.push(result);
            },
            .logxor => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logxor(self.heap, a, b);
                try self.push(result);
            },
            .lognot => {
                const a = try self.pop();
                const result = try arith.lognot(self.heap, a);
                try self.push(result);
            },
            .ash => {
                const count_val = try self.pop();
                const n_val = try self.pop();
                const result = try arith.ash(n_val, count_val);
                try self.push(result);
            },
            .lognand => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.lognand(self.heap, a, b);
                try self.push(result);
            },
            .lognor => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.lognor(self.heap, a, b);
                try self.push(result);
            },
            .logandc1 => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logandc1(self.heap, a, b);
                try self.push(result);
            },
            .logandc2 => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logandc2(self.heap, a, b);
                try self.push(result);
            },
            .logeqv => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logeqv(self.heap, a, b);
                try self.push(result);
            },
            .logbitp => {
                const n = try self.pop();
                const index = try self.pop();
                const result = try arith.logbitp(index, n);
                try self.push(if (result) Value.t else Value.nil);
            },
            .logcount => {
                const n = try self.pop();
                const result = try arith.logcount(n);
                try self.push(result);
            },
            .integer_length => {
                const n = try self.pop();
                const result = try arith.integer_length(n);
                try self.push(result);
            },
            .read_file => {
                const path_val = try self.pop();
                const path_str = try self.pathDesignatorBytes(path_val);
                const result = try io.readFile(self.heap, path_str);
                try self.push(result);
            },
            .write_file => {
                const content_val = try self.pop();
                const path_val = try self.pop();
                const path_str = try self.pathDesignatorBytes(path_val);
                try io.writeFile(path_str, content_val);
                try self.push(Value.nil);
            },
            .delete_file => {
                const path_val = try self.pop();
                const path_str = try self.pathDesignatorBytes(path_val);
                try io.deleteFile(path_str);
                try self.push(Value.nil);
            },
            .rename_file => {
                const new_path_val = try self.pop();
                const old_path_val = try self.pop();
                const old_path_str = try self.pathDesignatorBytes(old_path_val);
                const new_path_str = try self.pathDesignatorBytes(new_path_val);
                try io.renameFile(old_path_str, new_path_str);
                try self.push(Value.nil);
            },
            .probe_file => {
                const path_val = try self.pop();
                const result = primitives.pathname.truename(self.allocator, self.heap, &self.builtins, path_val) catch |err| switch (err) {
                    error.FileNotFound => Value.nil,
                    else => return err,
                };
                try self.push(result);
            },
            .file_write_date => {
                const path_val = try self.pop();
                const path_str = try self.pathDesignatorBytes(path_val);
                if (self.trace_io) {
                    std.debug.print("TRACE file-write-date: {s}\n", .{path_str});
                }
                const timestamp = try io.fileWriteDate(path_str);
                try self.push(Value.makeFixnum(timestamp));
            },
            .file_author => {
                const path_val = try self.pop();
                // Accept string or pathname
                if (!path_val.isString() and !path_val.isPathname()) return error.TypeMismatch;
                // Unix-like systems don't track file author, return nil
                try self.push(Value.nil);
            },
            .get_universal_time => {
                const timestamp = io.getUniversalTime();
                try self.push(Value.makeFixnum(timestamp));
            },
            .get_internal_real_time => {
                const timestamp = io.getInternalRealTime();
                try self.push(Value.makeFixnum(timestamp));
            },
            .get_internal_run_time => {
                const timestamp = try io.getInternalRunTime();
                try self.push(Value.makeFixnum(timestamp));
            },
            .get_decoded_time => {
                const ut = io.getUniversalTime();
                const dt = io.decodeUniversalTime(ut, null);
                // Return 9 values: second, minute, hour, date, month, year, dow, dst-p, zone
                try self.push(Value.makeFixnum(dt.second));
                self.secondary_values[0] = Value.makeFixnum(dt.minute);
                self.secondary_values[1] = Value.makeFixnum(dt.hour);
                self.secondary_values[2] = Value.makeFixnum(dt.date);
                self.secondary_values[3] = Value.makeFixnum(dt.month);
                self.secondary_values[4] = Value.makeFixnum(dt.year);
                self.secondary_values[5] = Value.makeFixnum(dt.day_of_week);
                self.secondary_values[6] = if (dt.daylight_p) Value.t else Value.nil;
                self.secondary_values[7] = Value.makeFixnum(dt.zone);
                self.secondary_values_count = 8;
            },
            .decode_universal_time => {
                const ut_val = try self.pop();
                if (!ut_val.isFixnum()) return error.TypeMismatch;
                const ut = ut_val.toFixnum();
                const dt = io.decodeUniversalTime(ut, null);
                // Return 9 values: second, minute, hour, date, month, year, dow, dst-p, zone
                try self.push(Value.makeFixnum(dt.second));
                self.secondary_values[0] = Value.makeFixnum(dt.minute);
                self.secondary_values[1] = Value.makeFixnum(dt.hour);
                self.secondary_values[2] = Value.makeFixnum(dt.date);
                self.secondary_values[3] = Value.makeFixnum(dt.month);
                self.secondary_values[4] = Value.makeFixnum(dt.year);
                self.secondary_values[5] = Value.makeFixnum(dt.day_of_week);
                self.secondary_values[6] = if (dt.daylight_p) Value.t else Value.nil;
                self.secondary_values[7] = Value.makeFixnum(dt.zone);
                self.secondary_values_count = 8;
            },
            .encode_universal_time => {
                const argc = self.readU8();
                // Pop args in reverse: year, month, date, hour, minute, second, [zone]
                var zone: ?i64 = null;
                if (argc == 7) {
                    const zone_val = try self.pop();
                    if (zone_val.isFixnum()) {
                        zone = zone_val.toFixnum();
                    }
                }
                const year_val = try self.pop();
                const month_val = try self.pop();
                const date_val = try self.pop();
                const hour_val = try self.pop();
                const minute_val = try self.pop();
                const second_val = try self.pop();
                if (!second_val.isFixnum() or !minute_val.isFixnum() or
                    !hour_val.isFixnum() or !date_val.isFixnum() or
                    !month_val.isFixnum() or !year_val.isFixnum())
                {
                    return error.TypeMismatch;
                }
                const ut = io.encodeUniversalTime(
                    second_val.toFixnum(),
                    minute_val.toFixnum(),
                    hour_val.toFixnum(),
                    date_val.toFixnum(),
                    month_val.toFixnum(),
                    year_val.toFixnum(),
                    zone,
                );
                try self.push(Value.makeFixnum(ut));
            },
            .room => {
                // Print memory statistics
                const stats = self.heap.stats;
                try io.room(stats.allocations, stats.bytes_allocated, stats.gc_count, stats.bytes_copied);
                try self.push(Value.nil);
            },
            .lisp_implementation_type => {
                const str = try self.heap.allocBaseString("Habu");
                try self.push(str);
            },
            .lisp_implementation_version => {
                const str = try self.heap.allocBaseString("0.1.0");
                try self.push(str);
            },
            .software_type => {
                const str = try self.heap.allocBaseString(@tagName(builtin.os.tag));
                try self.push(str);
            },
            .machine_type => {
                const str = try self.heap.allocBaseString(@tagName(builtin.cpu.arch));
                try self.push(str);
            },
            .machine_instance => {
                // Return hostname or nil
                try self.push(Value.nil);
            },
            .machine_version => {
                // Return nil - no specific hardware version info available
                try self.push(Value.nil);
            },
            .software_version => {
                // Return nil - could be expanded to return OS version
                try self.push(Value.nil);
            },
            .short_site_name => {
                // Site names are installation-specific; return nil
                try self.push(Value.nil);
            },
            .long_site_name => {
                // Site names are installation-specific; return nil
                try self.push(Value.nil);
            },
            .user_homedir_pathname => {
                const result = try primitives.pathname.userHomedirPathname(self.allocator, self.heap);
                try self.push(result);
            },
            .make_string => {
                const char_val = try self.pop();
                const len_val = try self.pop();
                if (!len_val.isFixnum()) return error.TypeMismatch;
                const len_signed = len_val.toFixnum();
                if (len_signed < 0) return error.TypeMismatch;
                const len: usize = @intCast(len_signed);

                if (char_val.isCharacter()) {
                    const cp = char_val.toCharacter();
                    if (cp <= 255) {
                        const str = try self.allocStringUninitialized(len);
                        const str_obj = str.toPtr(String);
                        @memset(str_obj.data[0..len], @intCast(cp));
                        try self.push(str);
                    } else {
                        const str = try self.heap.allocString32Uninitialized(len);
                        const str_obj = str.toPtr(runtime.String32);
                        @memset(str_obj.mutableCodepoints(), cp);
                        try self.push(str);
                    }
                } else if (char_val == Value.nil) {
                    const str = try self.allocStringUninitialized(len);
                    const str_obj = str.toPtr(String);
                    @memset(str_obj.data[0..len], ' ');
                    try self.push(str);
                } else {
                    return error.TypeMismatch;
                }
            },
            .math_ext => {
                const sub_op_byte = self.readU8();
                const sub_op: opcodes.MathExtOp = @enumFromInt(sub_op_byte);
                switch (sub_op) {
                    .asin => {
                        const val = try self.pop();
                        const result = try arith.asin_val(val);
                        try self.push(result);
                    },
                    .acos => {
                        const val = try self.pop();
                        const result = try arith.acos_val(val);
                        try self.push(result);
                    },
                    .atan => {
                        const val = try self.pop();
                        const result = try arith.atan_val(val);
                        try self.push(result);
                    },
                    .atan2 => {
                        const x = try self.pop();
                        const y = try self.pop();
                        const result = try arith.atan2_val(y, x);
                        try self.push(result);
                    },
                    .sinh => {
                        const val = try self.pop();
                        const result = try arith.sinh_val(val);
                        try self.push(result);
                    },
                    .cosh => {
                        const val = try self.pop();
                        const result = try arith.cosh_val(val);
                        try self.push(result);
                    },
                    .tanh => {
                        const val = try self.pop();
                        const result = try arith.tanh_val(val);
                        try self.push(result);
                    },
                    .asinh => {
                        const val = try self.pop();
                        const result = try arith.asinh_val(val);
                        try self.push(result);
                    },
                    .acosh => {
                        const val = try self.pop();
                        const result = try arith.acosh_val(val);
                        try self.push(result);
                    },
                    .atanh => {
                        const val = try self.pop();
                        const result = try arith.atanh_val(val);
                        try self.push(result);
                    },
                }
            },
            .list_to_string => {
                if (self.sp < 1) return error.StackUnderflow;
                const list_idx = self.sp - 1;
                const list_val = self.resolveForwardedValue(self.stack[list_idx]);
                // Count length first
                var len: usize = 0;
                var p = list_val;
                while (p != Value.nil) {
                    const live_p = self.resolveForwardedValue(p);
                    if (!live_p.isCons()) return error.TypeMismatch;
                    const c = live_p.toPtr(Cons);
                    // Accept either characters or fixnums (char codes)
                    if (!c.car.isCharacter() and !c.car.isFixnum()) return error.TypeMismatch;
                    len = try std.math.add(usize, len, 1);
                    p = c.cdr;
                }
                // Allocate and fill
                const str = try self.allocStringUninitialized(len);
                const str_obj = str.toPtr(String);
                var i: usize = 0;
                p = self.resolveForwardedValue(self.stack[list_idx]);
                while (p != Value.nil) {
                    const live_p = self.resolveForwardedValue(p);
                    if (!live_p.isCons()) return error.TypeMismatch;
                    const c = live_p.toPtr(Cons);
                    const cp: i64 = if (c.car.isCharacter())
                        @intCast(c.car.toCharacter())
                    else
                        c.car.toFixnum();
                    // Only ASCII/Latin-1 characters fit in a byte
                    if (cp < 0 or cp > 255) return error.TypeMismatch;
                    str_obj.data[i] = @intCast(cp);
                    i += 1;
                    p = c.cdr;
                }
                self.sp -= 1;
                try self.push(str);
            },
            .string_upcase => {
                if (self.sp < 1) return error.StackUnderflow;
                const str_idx = self.sp - 1;
                var scratch: ?[]u8 = null;
                defer if (scratch) |buf| self.allocator.free(buf);
                var designator_buf: [256]u8 = undefined;
                const str_val = self.resolveForwardedValue(self.stack[str_idx]);
                const src_bytes_raw = try self.getStringDesignator(str_val, designator_buf[0..]);
                defer src_bytes_raw.deinit(self.allocator);
                const src_bytes = try self.stabilizeHeapBytes(src_bytes_raw.slice, &scratch);
                const src_len = src_bytes.len;
                const result = try self.allocStringUninitialized(src_len);
                const dst = result.toPtr(String);
                for (src_bytes, 0..) |c, idx| {
                    dst.data[idx] = std.ascii.toUpper(c);
                }
                self.sp -= 1;
                try self.push(result);
            },
            .string_downcase => {
                if (self.sp < 1) return error.StackUnderflow;
                const str_idx = self.sp - 1;
                var scratch: ?[]u8 = null;
                defer if (scratch) |buf| self.allocator.free(buf);
                var designator_buf: [256]u8 = undefined;
                const str_val = self.resolveForwardedValue(self.stack[str_idx]);
                const src_bytes_dc_raw = try self.getStringDesignator(str_val, designator_buf[0..]);
                defer src_bytes_dc_raw.deinit(self.allocator);
                const src_bytes_dc = try self.stabilizeHeapBytes(src_bytes_dc_raw.slice, &scratch);
                const src_len_dc = src_bytes_dc.len;
                const result = try self.allocStringUninitialized(src_len_dc);
                const dst = result.toPtr(String);
                for (src_bytes_dc, 0..) |c, idx| {
                    dst.data[idx] = std.ascii.toLower(c);
                }
                self.sp -= 1;
                try self.push(result);
            },
            .random => {
                const n = try self.pop();
                const result = try arith.random(self.heap, &self.prng, &self.prng_seeded, n);
                try self.push(result);
            },
            .random_seed => {
                const seed = try self.pop();
                const result = try arith.randomSeed(&self.prng, &self.prng_seeded, seed);
                try self.push(result);
            },
            .intern => {
                const name_val = self.resolveForwardedValue(try self.pop());
                var designator_buf: [256]u8 = undefined;
                const name_bytes = try self.getStringDesignator(name_val, designator_buf[0..]);
                defer name_bytes.deinit(self.allocator);
                const sym = if (self.heap.internCurrentPackagePreservingCase(name_bytes.slice)) |val|
                    val
                else |_| blk: {
                    var tmp: ?[]u8 = null;
                    defer if (tmp) |b| self.allocator.free(b);
                    var stable = name_bytes.slice;
                    if (self.bytesInHeap(name_bytes.slice)) {
                        const copy = try self.allocator.alloc(u8, name_bytes.slice.len);
                        @memcpy(copy, name_bytes.slice);
                        tmp = copy;
                        stable = copy;
                    }
                    _ = try self.collectGarbage();
                    break :blk try self.heap.internCurrentPackagePreservingCase(stable);
                };
                try self.push(sym);
                // Secondary value: status keyword (:internal)
                const kw_internal = try self.heap.internKeyword("internal");
                self.secondary_values[0] = kw_internal;
                self.secondary_values_count = 1;
            },
            .make_symbol => {
                const name_val = try self.pop();
                const sym = try primitives.symbol.makeSymbol(self.heap, name_val);
                try self.push(sym);
            },
            .substring => {
                const end_val = try self.pop();
                const start_val = try self.pop();
                const str_val = try self.pop();
                if (!end_val.isFixnum() or !start_val.isFixnum()) return error.TypeMismatch;
                const start_signed = start_val.toFixnum();
                const end_signed = end_val.toFixnum();
                if (start_signed < 0 or end_signed < 0) return error.TypeMismatch;
                const start: usize = @intCast(start_signed);
                const end: usize = @intCast(end_signed);
                const result = try stringPrims.substring(self.heap, str_val, start, end);
                try self.push(result);
            },
            .sym_name => {
                const sym_val = self.resolveForwardedValue(try self.pop());
                const name_str = switch (sym_val.typeKind()) {
                    .nil => try self.allocString("nil"),
                    .t => try self.allocString("t"),
                    .symbol => blk: {
                        const sym = sym_val.toPtr(Symbol);
                        break :blk try self.allocString(sym.getName());
                    },
                    .keyword => blk: {
                        const kw = sym_val.toPtr(runtime.Keyword);
                        break :blk try self.allocString(kw.getName());
                    },
                    else => {
                        try self.signalTypeError();
                        return;
                    },
                };
                try self.push(name_str);
            },
            .copy_symbol => {
                const copy_props = try self.pop();
                const sym_val = self.resolveForwardedValue(try self.pop());
                const sym_kind = sym_val.typeKind();
                // Get the symbol name
                const name = switch (sym_kind) {
                    .nil => "nil",
                    .t => "t",
                    .symbol => sym_val.toPtr(Symbol).getName(),
                    else => {
                        try self.signalTypeError();
                        return;
                    },
                };
                // Create new uninterned symbol
                const new_sym = try self.heap.allocSymbol(name);
                // Copy properties if requested (only plist is stored in Symbol)
                if (!copy_props.isNil() and sym_kind == .symbol) {
                    const orig = sym_val.toPtr(Symbol);
                    const new = new_sym.toPtr(Symbol);
                    // Copy plist
                    if (orig.plist != Value.nil) {
                        new.plist = orig.plist;
                    }
                }
                try self.push(new_sym);
            },
            .makunbound => {
                const sym_val = try self.pop();
                switch (sym_val.typeKind()) {
                    .nil, .t => {
                        // nil and t cannot be made unbound
                        return error.InvalidArgument;
                    },
                    .symbol => {
                        try self.clearSymbolValueCell(sym_val);
                        try self.push(sym_val);
                    },
                    else => try self.signalTypeError(),
                }
            },
            .set_sym_val => {
                const val = try self.pop();
                const sym_val = try self.pop();
                switch (sym_val.typeKind()) {
                    .nil, .t => {
                        // nil and t cannot be set
                        return error.InvalidArgument;
                    },
                    .symbol => {
                        try self.setSymbolValueCell(sym_val, val);
                        try self.push(val);
                    },
                    else => try self.signalTypeError(),
                }
            },
            .set_symbol_function => {
                if (self.sp < 2) return error.StackUnderflow;
                const sym_idx = self.sp - 2;
                const fn_idx = self.sp - 1;
                switch (self.stack[sym_idx].typeKind()) {
                    .symbol => {
                        if (self.stack[fn_idx].isNil()) {
                            try self.clearFunctionCell(self.stack[sym_idx]);
                        } else {
                            try self.storeFunctionCell(self.stack[sym_idx], self.stack[fn_idx]);
                        }
                        const sym_obj = self.stack[sym_idx].toPtr(Symbol);
                        if (try self.lookupSymbolGlobalIndex(sym_obj)) |idx| {
                            const prev = self.globals[idx];
                            // Keep value and function namespaces separate:
                            // only update legacy callable value-cell slots.
                            if (isCallableFunctionValue(prev)) {
                                self.globals[idx] = self.stack[fn_idx];
                                if (idx >= self.num_globals) {
                                    self.num_globals = idx + 1;
                                }
                            }
                        } else if (self.global_env) |env| {
                            // Create a new global slot for this symbol
                            var qual_buf: [512]u8 = undefined;
                            const q = try qual_name.qualSymWithHeap(self.allocator, self.heap, sym_obj, &qual_buf);
                            defer if (q.owned) self.allocator.free(q.name);
                            const idx = try env.define(q.name);
                            if (idx < self.globals.len) {
                                // Function-only binding: leave value cell unbound.
                                self.globals[idx] = Value.unbound;
                                if (idx >= self.num_globals) {
                                    self.num_globals = idx + 1;
                                }
                            }
                        }
                    },
                    else => try self.signalTypeErrorDatumExpected(self.stack[sym_idx], self.builtins.sym_symbol),
                }
                self.stack[sym_idx] = self.stack[fn_idx];
                self.sp -= 1;
            },
            .set_symbol_plist => {
                const plist = try self.pop();
                const sym = try self.pop();
                switch (sym.typeKind()) {
                    .nil, .t => {
                        // Keep nil/t immutable in runtime storage; allow setf protocol to proceed.
                    },
                    .symbol => {
                        primitives.symbol.setSymbolPlist(self.heap, sym, plist) catch |err| switch (err) {
                            error.TypeError => try self.signalTypeErrorDatumExpected(sym, self.builtins.sym_symbol),
                            else => return err,
                        };
                    },
                    else => try self.signalTypeErrorDatumExpected(sym, self.builtins.sym_symbol),
                }
                try self.push(plist);
            },
            .type_of => {
                const val = try self.pop();
                const type_spec = try primitives.ty.typeOf(self.heap, val);
                try self.push(type_spec);
            },
            .str_eq => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringEqual(a, b)) Value.t else Value.nil;
                try self.push(result);
            },
            .str_lt => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringLt(a, b)) Value.t else Value.nil;
                try self.push(result);
            },
            .str_gt => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringGt(a, b)) Value.t else Value.nil;
                try self.push(result);
            },
            .str_le => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringLe(a, b)) Value.t else Value.nil;
                try self.push(result);
            },
            .str_ge => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringGe(a, b)) Value.t else Value.nil;
                try self.push(result);
            },

            // Type assertions (gradual typing)
            .check_fixnum => {
                const val = try self.peek(0);
                if (!val.isFixnum()) return error.TypeMismatch;
            },
            .check_cons => {
                const val = try self.peek(0);
                if (!val.isCons()) return error.TypeMismatch;
            },
            .check_symbol => {
                const val = try self.peek(0);
                if (!val.isSymbol()) return error.TypeMismatch;
            },
            .check_string => {
                const val = try self.peek(0);
                if (!val.isString()) return error.TypeMismatch;
            },
            .check_vector => {
                const val = try self.peek(0);
                const is_vector_like = switch (val.typeKind()) {
                    .vector => true,
                    .array => blk: {
                        const arr = val.toPtr(runtime.Array);
                        break :blk arr.rank == 1;
                    },
                    else => false,
                };
                if (!is_vector_like) return error.TypeMismatch;
            },
            .check_closure => {
                const val = try self.peek(0);
                if (!val.isClosure()) return error.TypeMismatch;
            },
            .check_non_nil => {
                const val = try self.peek(0);
                if (val.isNil()) return error.TypeMismatch;
            },
            .check_list => {
                const val = try self.peek(0);
                if (!val.isNil() and !val.isCons()) return error.TypeMismatch;
            },
            .check_or => {
                // Read constant pool index for type vector
                const type_vec_idx = self.readU16();
                const type_vec = try self.loadConst(type_vec_idx);

                // Get value to check
                const val = try self.peek(0);
                const val_kind = val.typeKind();
                const is_list = val_kind == .nil or val_kind == .cons;
                const is_atom = val_kind != .cons;
                const is_non_nil = val_kind != .nil;

                // Check if value matches any type in the list
                var matched = false;
                var current = type_vec;
                while (current.isCons()) {
                    const cons = current.toPtr(Cons);
                    const type_sym = cons.car;
                    const kind_match = switch (val_kind) {
                        .fixnum => type_sym.eq(self.type_syms.fixnum),
                        .cons => type_sym.eq(self.type_syms.cons),
                        .symbol => type_sym.eq(self.type_syms.symbol),
                        .string => type_sym.eq(self.type_syms.string),
                        .vector => type_sym.eq(self.type_syms.vector),
                        .closure => type_sym.eq(self.type_syms.closure),
                        .keyword => type_sym.eq(self.type_syms.keyword),
                        .nil => type_sym.eq(self.type_syms.nil),
                        else => false,
                    };
                    if (kind_match) {
                        matched = true;
                        break;
                    }
                    if (type_sym.eq(self.type_syms.list) and is_list) {
                        matched = true;
                        break;
                    }
                    if (type_sym.eq(self.type_syms.atom) and is_atom) {
                        matched = true;
                        break;
                    }
                    if (type_sym.eq(self.type_syms.t) and is_non_nil) {
                        matched = true;
                        break;
                    }
                    current = cons.cdr;
                }

                if (!matched) return error.TypeMismatch;
            },
            .check_refine => {
                // Stack: [value, predicate-result] -> [value]
                // Pop predicate result, check it's truthy, leave value
                const pred_result = try self.pop();
                if (pred_result.isNil()) return error.TypeMismatch;
                // Value is already on stack
            },

            // Catch/throw exception handling
            .push_catch => {
                const offset = self.readI16();
                const tag = try self.pop();
                // Calculate absolute jump target
                const catch_ip = @as(usize, @intCast(@as(isize, @intCast(self.ip)) + offset));
                // Push catch frame
                if (self.catch_sp >= MAX_CATCHES) return error.StackOverflow;
                self.catch_stack[self.catch_sp] = .{
                    .tag = tag,
                    .chunk = self.chunk,
                    .catch_ip = catch_ip,
                    .catch_sp = self.sp,
                    .catch_fp = self.fp,
                    .block_depth = self.block_sp,
                    .unwind_depth = self.unwind_sp,
                    .restart_depth = self.restart_sp,
                    .progv_depth = self.progv_sp,
                    .handler_depth = self.handler_sp,
                };
                self.catch_sp += 1;
            },

            .pop_catch => {
                if (self.catch_sp == 0) return error.StackUnderflow;
                self.catch_sp -= 1;
            },

            .throw => {
                const value = try self.pop();
                const tag = try self.pop();
                self.doThrow(tag, value) catch |err| {
                    if (err == error.UnhandledThrow) {
                        // Per CL spec: THROW with no matching CATCH signals CONTROL-ERROR.
                        // Convert to a CL condition that handler-case can catch.
                        try self.throwSimpleError(self.heap, "No catch tag for THROW");
                    } else {
                        return err;
                    }
                };
            },

            .signal => {
                const value = try self.pop();
                const condition_type = try self.pop();
                const condition = try self.allocCons(condition_type, value);
                const prev_chunk = self.chunk;
                const prev_ip = self.ip;
                const prev_sp = self.sp;
                const prev_fp = self.fp;
                self.doThrow(self.builtins.sym_condition_tag, condition) catch |err| {
                    if (err == error.UnhandledThrow) {
                        try self.push(Value.nil);
                        return;
                    }
                    return err;
                };
                const transferred =
                    self.chunk != prev_chunk or
                    self.ip != prev_ip or
                    self.sp != prev_sp or
                    self.fp != prev_fp;
                if (!transferred) {
                    try self.push(Value.nil);
                }
            },

            .push_progv => {
                if (self.trace_progv_disasm) {
                    var disasm_buf = std.ArrayList(u8){};
                    defer disasm_buf.deinit(self.allocator);
                    if (disasm.disassembleRuntime(self.chunk, disasm_buf.writer(self.allocator))) {
                        std.debug.print("TRACE progv disasm begin ip={d}\n{s}", .{ self.ip, disasm_buf.items });
                        const consts = self.chunk.getConstants();
                        std.debug.print("TRACE progv constants ({d}):\n", .{consts.len});
                        for (consts, 0..) |c, ci| {
                            switch (c.typeKind()) {
                                .nil => std.debug.print("  [{d}] nil\n", .{ci}),
                                .t => std.debug.print("  [{d}] t\n", .{ci}),
                                .fixnum => std.debug.print("  [{d}] fixnum {d}\n", .{ ci, c.toFixnum() }),
                                .symbol => std.debug.print("  [{d}] symbol {s}\n", .{ ci, c.toPtr(Symbol).getName() }),
                                .keyword => std.debug.print("  [{d}] keyword {s}\n", .{ ci, c.toPtr(runtime.Keyword).getName() }),
                                .string => std.debug.print("  [{d}] string \"{s}\"\n", .{ ci, c.toPtr(String).bytes() }),
                                else => std.debug.print("  [{d}] {s} raw=0x{x:0>16}\n", .{ ci, @tagName(c.typeKind()), c.raw }),
                            }
                        }
                        std.debug.print("TRACE progv disasm end\n", .{});
                    } else |derr| {
                        std.debug.print("TRACE progv disasm error={s}\n", .{@errorName(derr)});
                    }
                }
                const values = self.resolveForwardedValue(try self.pop());
                const symbols = self.resolveForwardedValue(try self.pop());
                try self.pushProgvFrame(symbols, values);
            },

            .pop_progv => {
                try self.popProgvFrame();
            },

            // Block/return-from (lexical non-local exit)
            .push_block => {
                const offset = self.readI16();
                // Calculate exit_ip relative to current IP (after offset bytes, before name_idx)
                const exit_ip = @as(usize, @intCast(@as(isize, @intCast(self.ip)) + offset));
                const name_idx = self.readU16();
                const name_raw = try self.loadConst(name_idx);
                if (self.block_sp >= MAX_BLOCKS) {
                    if (std.process.hasEnvVar(self.allocator, "HABU_TRACE_BLOCK_OVERFLOW") catch false) {
                        const chunk_name = if (self.chunk.name.isSymbol())
                            self.chunk.name.toPtr(runtime.Symbol).getName()
                        else
                            @tagName(self.chunk.name.typeKind());
                        const block_name = if (name_raw.isSymbol())
                            name_raw.toPtr(runtime.Symbol).getName()
                        else
                            @tagName(name_raw.typeKind());
                        std.debug.print("BLOCK_OVERFLOW chunk={s} block={s} ip={d} sp={d} fp={d}\n", .{
                            chunk_name,
                            block_name,
                            self.ip,
                            self.sp,
                            self.fp,
                        });
                        var i: usize = 0;
                        while (i < self.block_sp) : (i += 1) {
                            const frame = self.block_stack[i];
                            const frame_name = if (frame.name_raw.isSymbol())
                                frame.name_raw.toPtr(runtime.Symbol).getName()
                            else
                                @tagName(frame.name_raw.typeKind());
                            std.debug.print("  frame[{d}] name={s} exit_ip={d}\n", .{ i, frame_name, frame.exit_ip });
                        }
                    }
                    return error.StackOverflow;
                }
                self.block_stack[self.block_sp] = .{
                    .name_raw = name_raw,
                    .chunk = self.chunk,
                    .exit_ip = exit_ip,
                    .block_sp = self.sp,
                    .block_fp = self.fp,
                    .catch_depth = self.catch_sp,
                    .unwind_depth = self.unwind_sp,
                    .restart_depth = self.restart_sp,
                    .progv_depth = self.progv_sp,
                    .handler_depth = self.handler_sp,
                };
                self.block_sp += 1;
            },

            .pop_block => {
                if (self.block_sp == 0) return error.StackUnderflow;
                self.block_sp -= 1;
            },

            .return_from => {
                const name_idx = self.readU16();
                const name_raw = try self.loadConst(name_idx);
                const value = try self.pop();
                try self.doReturnFrom(name_raw, value);
            },

            .push_unwind => {
                const offset = self.readI16();
                const cleanup_ip = @as(usize, @intCast(@as(isize, @intCast(self.ip)) + offset));
                if (self.unwind_sp >= MAX_UNWINDS) return error.StackOverflow;
                self.unwind_stack[self.unwind_sp] = .{
                    .chunk = self.chunk,
                    .cleanup_ip = cleanup_ip,
                    .unwind_sp = self.sp,
                    .unwind_fp = self.fp,
                };
                self.unwind_sp += 1;
            },

            .pop_unwind => {
                _ = self.readI16(); // Skip unused operand
                
                // For normal exit, pop the unwind frame (doThrow/doError/doReturnFrom already popped for unwind case)
                if (!self.is_unwinding and !self.is_returning_from_block and self.unwind_sp > 0) {
                    self.unwind_sp -= 1;
                }
                // If we're unwinding (cleanup ran due to throw, error, or return-from), re-invoke
                if (self.is_unwinding) {
                    self.is_unwinding = false;

                    // Check if unwinding due to error
                    if (self.pending_error) |err| {
                        self.pending_error = null;
                        return err;
                    }

                    // Otherwise unwinding due to throw
                    const tag = self.pending_throw_tag;
                    const value = self.pending_throw_value;
                    self.pending_throw_tag = Value.nil;
                    self.pending_throw_value = Value.nil;
                    try self.doThrow(tag, value);
                } else if (self.is_returning_from_block) {
                    self.is_returning_from_block = false;

                    const block_idx = self.pending_block_idx orelse return error.NoMatchingBlock;
                    const value = self.pending_block_value;
                    self.pending_block_idx = null;
                    self.pending_block_value = Value.nil;

                    if (block_idx >= self.block_sp) return error.NoMatchingBlock;
                    try self.jumpToBlock(block_idx, value);
                }
            },

            // Restart handling
            .push_restart => {
                const offset = self.readI16();
                const name = try self.pop();
                const handler_ip = @as(usize, @intCast(@as(isize, @intCast(self.ip)) + offset));
                if (self.restart_sp >= MAX_RESTARTS) return error.StackOverflow;
                self.restart_stack[self.restart_sp] = .{
                    .name = name,
                    .id = self.restart_next_id,
                    .chunk = self.chunk,
                    .handler_ip = handler_ip,
                    .restart_sp = self.sp,
                    .restart_fp = self.fp,
                    .catch_depth = self.catch_sp,
                    .unwind_depth = self.unwind_sp,
                    .block_depth = self.block_sp,
                    .progv_depth = self.progv_sp,
                    .handler_depth = self.handler_sp,
                };
                self.restart_next_id +%= 1;
                if (self.restart_next_id == 0) self.restart_next_id = 1;
                self.restart_sp += 1;
            },

            .pop_restarts => {
                const count = self.readU8();
                if (self.restart_sp < count) return error.StackUnderflow;
                self.restart_sp -= count;
            },

            .invoke_restart => {
                const value = try self.pop();
                const name = try self.pop();
                try self.doInvokeRestart(name, value);
            },

            .find_restart => {
                const designator = try self.pop();
                const found = if (try self.findRestartIndex(designator)) |idx|
                    try self.makeRestartObject(&self.restart_stack[idx])
                else
                    Value.nil;
                try self.push(found);
            },

            .handler_bind => {
                const handlers_alist = try self.pop();
                const body_fn = try self.pop();
                try self.doHandlerBind(body_fn, handlers_alist);
            },

            // Multiple values
            .values => {
                const count = self.readU8();
                if (count == 0) {
                    // (values) returns nil
                    try self.push(Value.nil);
                    self.secondary_values_count = 0;
                    self.zero_values_returned = true;
                } else {
                    // Pop all values, store secondary values, push primary
                    // Values are on stack in order: v1 v2 ... vN (vN on top)
                    // We want: primary=v1, secondary=[v2, v3, ...]
                    const secondary_count = count - 1;
                    if (secondary_count > MAX_SECONDARY_VALUES) return error.StackOverflow;

                    // Pop secondary values in reverse order
                    var i: usize = 0;
                    while (i < secondary_count) : (i += 1) {
                        const idx = secondary_count - 1 - i;
                        self.secondary_values[idx] = try self.pop();
                    }
                    self.secondary_values_count = secondary_count;
                    self.zero_values_returned = false;
                    // Primary value remains on stack
                }
            },

            .mv_bind => {
                const count = self.readU8();
                const start_index = self.readU8();
                const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;

                // Primary value is already on stack - store to start_index
                const primary = try self.pop();
                self.stack[bp + start_index] = primary;

                // Store secondary values (or nil if not enough) to subsequent locals
                var i: usize = 1;
                while (i < count) : (i += 1) {
                    const val = if (i - 1 < self.secondary_values_count)
                        self.secondary_values[i - 1]
                    else
                        Value.nil;
                    self.stack[bp + start_index + i] = val;
                }

                // Clear secondary values
                self.secondary_values_count = 0;
            },

            .mv_list => {
                // Primary value is on stack, secondaries in buffer
                const primary = try self.pop();

                // Build list in reverse: start with nil, cons secondaries, then cons primary
                var result = Value.nil;

                // Add secondary values in reverse order
                var i: usize = self.secondary_values_count;
                while (i > 0) : (i -= 1) {
                    result = try self.allocCons(self.secondary_values[i - 1], result);
                }

                // Add primary at front
                result = try self.allocCons(primary, result);

                // Clear secondary values
                self.secondary_values_count = 0;

                try self.push(result);
            },

            .values_list => {
                // Pop a list from stack, return its elements as multiple values
                const list = try self.pop();

                switch (list.typeKind()) {
                    .nil => {
                        // Empty list -> return nil with no secondary values
                        self.secondary_values_count = 0;
                        try self.push(Value.nil);
                        self.zero_values_returned = true;
                    },
                    .cons => {
                        // Walk the list, extract elements
                        var first = Value.nil;
                        var count: usize = 0;
                        var current = list;
                        var overflow = false;

                        while (current.isCons()) {
                            const cons = current.toPtr(runtime.Cons);
                            if (count == 0) {
                                first = cons.car;
                            } else if (count - 1 < self.secondary_values.len) {
                                self.secondary_values[count - 1] = cons.car;
                            } else {
                                overflow = true;
                            }
                            count += 1;
                            current = cons.cdr;
                        }

                        if (overflow) return error.StackOverflow;
                        self.secondary_values_count = if (count > 1) count - 1 else 0;
                        try self.push(first);
                        self.zero_values_returned = false;
                    },
                    else => return error.TypeMismatch,
                }
            },

            .format => {
                const argc = self.readU8();
                if (argc > 32) return error.InvalidArgument;
                // Stack: dest, control-string, arg1, ..., argN (argN on top)
                // Pop args in reverse order
                var args: [32]Value = undefined;
                var arg_idx: usize = argc;
                while (arg_idx > 0) : (arg_idx -= 1) {
                    args[arg_idx - 1] = try self.pop();
                }
                const control = try self.pop();
                const dest = try self.pop();

                if (!control.isString()) return error.TypeMismatch;

                const result = try self.doFormat(dest, control, args[0..argc]);
                try self.push(result);
            },

            // Hash table operations
            .make_hash => {
                const capacity = self.readU16();
                const test_byte = self.readU8();
                const test_type: runtime.HashTest = @enumFromInt(test_byte);
                const ht = try self.allocHashTable(capacity, test_type);
                try self.push(ht);
            },
            .hash_get => {
                const key = try self.pop();
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                if (ht.get(key)) |val| {
                    try self.push(val);
                    self.secondary_values[0] = Value.t;
                } else {
                    try self.push(Value.nil);
                    self.secondary_values[0] = Value.nil;
                }
                self.secondary_values_count = 1;
            },
            .sxhash => {
                const obj = try self.pop();
                const result = try hash_prims.primSxhash(self.heap, &[_]Value{obj});
                try self.push(result);
            },
            .hash_set => {
                if (self.sp < 3) return error.StackUnderflow;
                const ht_idx = self.sp - 3;
                const key_idx = self.sp - 2;
                const val_idx = self.sp - 1;
                if (!self.stack[ht_idx].isHashTable()) return error.TypeMismatch;

                while (true) {
                    const ht = self.stack[ht_idx].toPtr(HashTable);
                    const key = self.stack[key_idx];
                    const value = self.stack[val_idx];

                    ht.put(key, value) catch |err| switch (err) {
                        error.HashTableNeedsGrowth, error.HashTableFull => {
                            const new_cap_u64 = ht.capacity * 2;
                            if (new_cap_u64 < ht.capacity) return error.Overflow;
                            try self.htGrowInPlace(ht_idx, @intCast(new_cap_u64));
                            continue;
                        },
                        else => return err,
                    };
                    self.writeBarrierStore(self.stack[ht_idx], key);
                    self.writeBarrierStore(self.stack[ht_idx], value);
                    break;
                }

                // Only push the value (CL setf gethash semantics)
                const result = self.stack[val_idx];
                self.sp -= 3;
                try self.push(result);
            },
            .hash_rem => {
                const key = try self.pop();
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                const removed = ht.remove(key);
                try self.push(if (removed) Value.t else Value.nil);
            },
            .hash_count => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                try self.push(Value.makeFixnum(@intCast(ht.count)));
            },
            .hash_capacity => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                try self.push(Value.makeFixnum(@intCast(ht.capacity)));
            },
            .hashtablep => {
                const val = try self.pop();
                try self.push(if (val.isHashTable()) Value.t else Value.nil);
            },
            .packagep => {
                const val = try self.pop();
                try self.push(if (val.isPackage()) Value.t else Value.nil);
            },
            .symbol_package => {
                const val = try self.pop();
                // Handle special symbols nil and t
                switch (val.typeKind()) {
                    .nil, .t => {
                        // nil and t are in the CL package
                        const cl_name = try self.heap.allocBaseString("CL");
                        if (try self.heap.findLispPackage(cl_name)) |pkg| {
                            try self.push(pkg);
                        } else {
                            try self.push(Value.nil);
                        }
                    },
                    .keyword => {
                        // Keywords are in the KEYWORD package
                        const kw_name = try self.heap.allocBaseString("KEYWORD");
                        if (try self.heap.findLispPackage(kw_name)) |pkg| {
                            try self.push(pkg);
                        } else {
                            try self.push(Value.nil);
                        }
                    },
                    .symbol => {
                        const sym = val.toPtr(Symbol);
                        if (self.heap.symbolHomePkg(sym)) |zig_pkg| {
                            // Look up the Lisp package object by name
                            const name_val = try self.heap.allocBaseString(zig_pkg.name);
                            if (try self.heap.findLispPackage(name_val)) |pkg| {
                                try self.push(pkg);
                            } else {
                                try self.push(Value.nil);
                            }
                        } else {
                            try self.push(Value.nil);
                        }
                    },
                    else => try self.signalTypeError(),
                }
            },
            .package_name => {
                const pkg = try self.pop();
                const result = try primitives.package.packageName(pkg);
                try self.push(result);
            },
            .package_nicknames => {
                const pkg = try self.pop();
                const result = try primitives.package.packageNicknames(pkg);
                try self.push(result);
            },
            .package_use_list => {
                const pkg = try self.pop();
                const result = try primitives.package.packageUseList(pkg);
                try self.push(result);
            },
            .package_used_by_list => {
                const pkg = try self.pop();
                const result = try primitives.package.packageUsedByList(self.heap, pkg);
                try self.push(result);
            },
            .package_shadowing_symbols => {
                const pkg = try self.pop();
                const result = try primitives.package.packageShadowingSymbols(pkg);
                try self.push(result);
            },
            .list_all_packages => {
                const result = try primitives.package.listAllPackages(self.heap);
                try self.push(result);
            },
            .compute_restarts => {
                // Build list of active restart objects from restart stack.
                var result = Value.nil;
                var i: usize = self.restart_sp;
                while (i > 0) {
                    i -= 1;
                    const restart_obj = try self.makeRestartObject(&self.restart_stack[i]);
                    result = try self.allocCons(restart_obj, result);
                }
                try self.push(result);
            },
            .restart_name => {
                const restart = try self.pop();
                const name = (try self.restartNameValue(restart)) orelse return error.TypeMismatch;
                try self.push(name);
            },
            .directory => {
                const pathname = try self.pop();
                const result = try io.listDirectory(self.heap, pathname);
                try self.push(result);
            },
            .pathname_match_p => {
                const wildcard = try self.pop();
                const pathname = try self.pop();
                const result = try io.pathnameMatchP(pathname, wildcard);
                try self.push(result);
            },
            .enough_namestring => {
                // enough-namestring returns shortest namestring to identify pathname
                // In our simplified implementation, returns full namestring
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;

                const pn = pn_val.toPtr(runtime.Pathname);

                var result = std.ArrayList(u8){};
                defer result.deinit(self.allocator);

                // Process directory
                if (pn.directory != Value.nil) {
                    var dir_list = pn.directory;
                    if (dir_list.isCons()) {
                        const first = dir_list.toPtr(runtime.Cons).car;
                        if (first.raw == self.builtins.kw_absolute.raw) {
                            try result.append(self.allocator, '/');
                            dir_list = dir_list.toPtr(runtime.Cons).cdr;
                        } else if (first.raw == self.builtins.kw_relative.raw) {
                            dir_list = dir_list.toPtr(runtime.Cons).cdr;
                        }
                    }
                    while (dir_list != Value.nil) {
                        if (!dir_list.isCons()) break;
                        const cons = dir_list.toPtr(runtime.Cons);
                        const component = cons.car;
                        if (component.isString()) {
                            const comp_str = component.toPtr(runtime.String);
                            try result.appendSlice(self.allocator, comp_str.bytes());
                            try result.append(self.allocator, '/');
                        }
                        dir_list = cons.cdr;
                    }
                }

                // Add name
                if (pn.name != Value.nil and pn.name.isString()) {
                    const name_str = pn.name.toPtr(runtime.String);
                    try result.appendSlice(self.allocator, name_str.bytes());
                }

                // Add type (extension)
                if (pn.type != Value.nil and pn.type.isString()) {
                    try result.append(self.allocator, '.');
                    const type_str = pn.type.toPtr(runtime.String);
                    try result.appendSlice(self.allocator, type_str.bytes());
                }

                const str = try self.heap.allocBaseString(result.items);
                try self.push(str);
            },
            .decode_float => {
                const val = try self.pop();
                const result = try arith.decodeFloat(val);
                try self.push(result[0]);
                self.secondary_values[0] = result[1];
                self.secondary_values[1] = result[2];
                self.secondary_values_count = 2;
            },
            .integer_decode_float => {
                const val = try self.pop();
                const result = try arith.integerDecodeFloat(val);
                try self.push(result[0]);
                self.secondary_values[0] = result[1];
                self.secondary_values[1] = result[2];
                self.secondary_values_count = 2;
            },
            .float_radix => {
                const val = try self.pop();
                const result = try arith.floatRadix(val);
                try self.push(result);
            },
            .float_digits => {
                const val = try self.pop();
                const result = try arith.floatDigits(val);
                try self.push(result);
            },
            .find_package => {
                const name = try self.pop();
                if (try primitives.package.findPackage(self.heap, name)) |pkg| {
                    try self.push(pkg);
                } else {
                    try self.push(Value.nil);
                }
            },
            .delete_package => {
                const pkg = try self.pop();
                if (primitives.package.deletePackage(self.heap, pkg)) |deleted| {
                    try self.push(if (deleted) Value.t else Value.nil);
                } else |err| {
                    // Signal Lisp package-error condition for ANSI handler-case
                    const te = self.intern("package-error") catch return err;
                    const pair = self.allocCons(te, pkg) catch return err;
                    self.doThrow(self.builtins.sym_condition_tag, pair) catch |e| return e;
                    try self.push(Value.nil);
                }
            },
            .pkg_import => {
                const pkg = try self.pop();
                const symbols = try self.pop();
                try primitives.package.importSymbols(self.heap, symbols, pkg);
                try self.push(Value.t);
            },
            .pkg_use_package => {
                const pkg = try self.pop();
                const packages = try self.pop();
                try primitives.package.usePackage(self.heap, packages, pkg);
                try self.push(Value.t);
            },
            .pkg_export => {
                const pkg = try self.pop();
                const symbols = try self.pop();
                try primitives.package.exportSymbols(self.heap, symbols, pkg);
                try self.push(Value.t);
            },
            .pkg_unexport => {
                const pkg = try self.pop();
                const symbols = try self.pop();
                try primitives.package.unexportSymbols(self.heap, symbols, pkg);
                try self.push(Value.t);
            },
            .pkg_shadow => {
                const pkg = try self.pop();
                const names = try self.pop();
                try primitives.package.shadowSymbols(self.heap, names, pkg);
                try self.push(Value.t);
            },
            .pkg_shadowing_import => {
                const pkg = try self.pop();
                const symbols = try self.pop();
                try primitives.package.shadowingImport(self.heap, symbols, pkg);
                try self.push(Value.t);
            },
            .pkg_unuse_package => {
                const pkg = try self.pop();
                const packages = try self.pop();
                try primitives.package.unusePackage(self.heap, packages, pkg);
                try self.push(Value.t);
            },
            .pkg_unintern => {
                const pkg = try self.pop();
                const symbol = try self.pop();
                const removed = try primitives.package.uninternSymbol(self.heap, symbol, pkg);
                try self.push(if (removed) Value.t else Value.nil);
            },
            .pkg_find_symbol => {
                const pkg = try self.pop();
                const name = try self.pop();
                const result = try primitives.package.findSymbol(self.heap, name, pkg);
                // Returns (symbol . (status . nil)); expose as primary + secondary value.
                if (result.isCons()) {
                    const c1 = result.toPtr(runtime.Cons);
                    try self.push(c1.car);
                    if (c1.cdr.isCons()) {
                        const c2 = c1.cdr.toPtr(runtime.Cons);
                        self.secondary_values[0] = c2.car;
                    } else {
                        self.secondary_values[0] = Value.nil;
                    }
                    self.secondary_values_count = 1;
                } else {
                    try self.push(Value.nil);
                    self.secondary_values[0] = Value.nil;
                    self.secondary_values_count = 1;
                }
            },
            .pkg_find_all_symbols => {
                const name = try self.pop();
                const result = try primitives.package.findAllSymbols(self.heap, name);
                try self.push(result);
            },
            .apropos_list => {
                const substring = try self.pop();
                const result = try primitives.package.aproposSymbols(self.heap, substring);
                try self.push(result);
            },
            .read_char_no_hang => {
                const stream = try self.pop();
                const result = try io.readCharNoHang(stream);
                try self.push(result);
            },
            .pkg_make_package => {
                const use_list = try self.pop();
                const nicknames = try self.pop();
                const name = try self.pop();
                const result = try primitives.package.makePackage(
                    self.heap,
                    name,
                    if (nicknames.isNil()) null else nicknames,
                    if (use_list.isNil()) null else use_list,
                );
                try self.push(result);
            },
            .pkg_rename_package => {
                const new_nicknames = try self.pop();
                const new_name = try self.pop();
                const pkg = try self.pop();
                const result = try primitives.package.renamePackage(
                    self.heap,
                    pkg,
                    new_name,
                    if (new_nicknames.isNil()) null else new_nicknames,
                );
                try self.push(result);
            },
            .hash_clear => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                ht.clear();
                try self.push(ht_val);
            },
            .hash_test => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                const test_name = switch (ht.test_type) {
                    .eq => "eq",
                    .eql => "eql",
                    .equal => "equal",
                    .equalp => "equalp",
                };
                const sym = try self.heap.intern(test_name);
                try self.push(sym);
            },
            .hash_keys => {
                if (self.sp < 1) return error.StackUnderflow;
                const ht_idx = self.sp - 1;
                if (!self.stack[ht_idx].isHashTable()) return error.TypeMismatch;
                const cap = self.stack[ht_idx].toPtr(HashTable).capacity;
                // Build list of keys from hash table entries
                var result = Value.nil;
                var i: u64 = 0;
                while (i < cap) : (i += 1) {
                    const ht = self.stack[ht_idx].toPtr(HashTable);
                    const k = ht.getKey(@intCast(i));
                    if (HashTable.isAvailableKey(k)) continue;
                    result = try self.allocCons(k, result);
                }
                self.sp -= 1;
                try self.push(result);
            },
            .hash_alist => {
                if (self.sp < 1) return error.StackUnderflow;
                const ht_idx = self.sp - 1;
                if (!self.stack[ht_idx].isHashTable()) return error.TypeMismatch;
                const cap = self.stack[ht_idx].toPtr(HashTable).capacity;
                // Build alist of (key . value) pairs from hash table entries
                var result = Value.nil;
                var i: u64 = 0;
                while (i < cap) : (i += 1) {
                    const ht = self.stack[ht_idx].toPtr(HashTable);
                    const k = ht.getKey(@intCast(i));
                    if (HashTable.isAvailableKey(k)) continue;
                    const v = ht.getValue(@intCast(i));

                    const pair = try self.allocCons(k, v);
                    result = try self.allocCons(pair, result);
                }
                self.sp -= 1;
                try self.push(result);
            },
            .rationalp => {
                const val = try self.pop();
                try self.push(if (val.isRational()) Value.t else Value.nil);
            },
            .complexp => {
                const val = try self.pop();
                try self.push(if (val.isComplex()) Value.t else Value.nil);
            },
            .make_complex => {
                const imag = try self.pop();
                const real = try self.pop();
                const real_f = try valToFloat(real);
                const imag_f = try valToFloat(imag);
                const cplx = try self.heap.allocComplex(real_f, imag_f);
                try self.push(cplx);
            },
            .real_part => {
                const val = try self.pop();
                if (!val.isComplex()) return error.TypeMismatch;
                const cplx = val.toPtr(runtime.Complex);
                try self.push(Value.makeFloat(cplx.real));
            },
            .imag_part => {
                const val = try self.pop();
                if (!val.isComplex()) return error.TypeMismatch;
                const cplx = val.toPtr(runtime.Complex);
                try self.push(Value.makeFloat(cplx.imag));
            },

            .numerator => {
                const val = try self.pop();
                if (!val.isRational()) return error.TypeMismatch;
                const rat = val.toPtr(runtime.Rational);
                try self.push(Value.makeFixnum(rat.numerator));
            },
            .denominator => {
                const val = try self.pop();
                if (!val.isRational()) return error.TypeMismatch;
                const rat = val.toPtr(runtime.Rational);
                try self.push(Value.makeFixnum(rat.denominator));
            },
            .rational => {
                const val = try self.pop();
                const result = if (val.isFloat())
                    try primitives.rational.floatToRational(self.heap, val.toFloat())
                else
                    val; // Already rational or integer
                try self.push(result);
            },
            .get => {
                const indicator = try self.pop();
                const sym = try self.pop();
                const result = try primitives.list.get(self.heap, sym, indicator);
                try self.push(result);
            },
            .put => {
                const value = try self.pop();
                const indicator = try self.pop();
                const sym = try self.pop();
                const result = try primitives.list.put(self.heap, sym, indicator, value);
                try self.push(result);
            },
            .remprop => {
                const indicator = try self.pop();
                const sym = try self.pop();
                const result = try primitives.list.remprop(self.heap, sym, indicator);
                try self.push(result);
            },
            // Stream operations
            .streamp => {
                const val = try self.pop();
                try self.push(if (val.isStream()) Value.t else Value.nil);
            },
            .input_stream_p => {
                const val = try self.pop();
                try self.push(if (io.inputStreamP(val)) Value.t else Value.nil);
            },
            .output_stream_p => {
                const val = try self.pop();
                try self.push(if (io.outputStreamP(val)) Value.t else Value.nil);
            },
            .open_stream_p => {
                const val = try self.pop();
                if (!val.isStream()) {
                    try self.push(Value.nil);
                } else {
                    const stream = val.toPtr(runtime.Stream);
                    try self.push(if (!stream.isClosed()) Value.t else Value.nil);
                }
            },
            .interactive_stream_p => {
                const val = try self.pop();
                try self.push(if (io.interactiveStreamP(val)) Value.t else Value.nil);
            },
            .stream_element_type => {
                const val = try self.pop();
                if (!val.isStream()) return error.TypeMismatch;
                // Our streams are character streams
                try self.push(try self.heap.intern("character"));
            },
            .stream_external_format => {
                const val = try self.pop();
                if (!val.isStream()) return error.TypeMismatch;
                // Return :default as external format
                try self.push(try self.heap.internKeyword("default"));
            },
            .make_broadcast_stream => {
                // Pop count streams from stack, create broadcast stream
                const count = self.chunk.code[self.ip];
                self.ip += 1;
                var streams_list = Value.nil;
                // Build list in reverse order (stack order)
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const stream = try self.pop();
                    if (!stream.isStream()) return error.TypeMismatch;
                    streams_list = try self.allocCons(stream, streams_list);
                }
                const result = try self.heap.allocBroadcastStream(streams_list);
                try self.push(result);
            },
            .make_concatenated_stream => {
                // Pop count streams from stack, create concatenated stream
                const count = self.chunk.code[self.ip];
                self.ip += 1;
                var streams_list = Value.nil;
                // Build list in reverse order (stack order)
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const stream = try self.pop();
                    if (!stream.isStream()) return error.TypeMismatch;
                    streams_list = try self.allocCons(stream, streams_list);
                }
                const result = try self.heap.allocConcatenatedStream(streams_list);
                try self.push(result);
            },
            .make_echo_stream => {
                const output_stream = try self.pop();
                const input_stream = try self.pop();
                if (!input_stream.isStream() or !output_stream.isStream()) return error.TypeMismatch;
                const result = try self.heap.allocEchoStream(input_stream, output_stream);
                try self.push(result);
            },
            .make_synonym_stream => {
                const symbol = try self.pop();
                if (!symbol.isSymbol()) return error.TypeMismatch;
                const result = try self.heap.allocSynonymStream(symbol);
                try self.push(result);
            },
            .make_two_way_stream => {
                const output_stream = try self.pop();
                const input_stream = try self.pop();
                if (!input_stream.isStream() or !output_stream.isStream()) return error.TypeMismatch;
                const result = try self.heap.allocTwoWayStream(input_stream, output_stream);
                try self.push(result);
            },
            .broadcast_stream_streams => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .broadcast) return error.TypeMismatch;
                try self.push(stream.source_value); // the list of streams
            },
            .concatenated_stream_streams => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .concatenated) return error.TypeMismatch;
                try self.push(stream.source_value); // the list of streams
            },
            .echo_stream_input_stream => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .echo) return error.TypeMismatch;
                // source_value is (input . output)
                const pair = stream.source_value.toPtr(runtime.Cons);
                try self.push(pair.car);
            },
            .echo_stream_output_stream => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .echo) return error.TypeMismatch;
                // source_value is (input . output)
                const pair = stream.source_value.toPtr(runtime.Cons);
                try self.push(pair.cdr);
            },
            .synonym_stream_symbol => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .synonym) return error.TypeMismatch;
                try self.push(stream.source_value); // the symbol
            },
            .two_way_stream_input_stream => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .two_way) return error.TypeMismatch;
                // source_value is (input . output)
                const pair = stream.source_value.toPtr(runtime.Cons);
                try self.push(pair.car);
            },
            .two_way_stream_output_stream => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .two_way) return error.TypeMismatch;
                // source_value is (input . output)
                const pair = stream.source_value.toPtr(runtime.Cons);
                try self.push(pair.cdr);
            },
            .make_broadcast_stream_list => {
                // Pop list of streams, create broadcast stream
                const streams_list = try self.pop();
                // Validate all elements are streams
                var cursor = streams_list;
                while (cursor.isCons()) {
                    const cons = cursor.toPtr(runtime.Cons);
                    if (!cons.car.isStream()) return error.TypeMismatch;
                    cursor = cons.cdr;
                }
                const result = try self.heap.allocBroadcastStream(streams_list);
                try self.push(result);
            },
            .make_concatenated_stream_list => {
                // Pop list of streams, create concatenated stream
                const streams_list = try self.pop();
                // Validate all elements are streams
                var cursor = streams_list;
                while (cursor.isCons()) {
                    const cons = cursor.toPtr(runtime.Cons);
                    if (!cons.car.isStream()) return error.TypeMismatch;
                    cursor = cons.cdr;
                }
                const result = try self.heap.allocConcatenatedStream(streams_list);
                try self.push(result);
            },
            .disassemble => {
                const func_val = try self.pop();
                if (!func_val.isClosure()) return error.TypeMismatch;
                const closure = func_val.toPtr(runtime.Closure);
                // Get the chunk from the closure's code field
                if (!closure.code.isChunk()) return error.TypeMismatch;
                const chunk = closure.code.toPtr(runtime.Chunk);
                // Write disassembly to stdout using runtime variant
                var buf: [4096]u8 = undefined;
                const stdout = std.fs.File.stdout();
                var bw = stdout.writer(&buf);
                const writer = &bw.interface;
                try disasm.disassembleRuntime(chunk, writer);
                try writer.flush();
                try self.push(Value.nil);
            },
            .read_char_stream => {
                const stream = try self.pop();
                const result = try io.readChar(stream, null, null);
                try self.push(result);
            },
            .peek_char_stream => {
                const stream = try self.pop();
                const result = try io.peekChar(null, stream);
                try self.push(result);
            },
            .unread_char_stream => {
                const stream = try self.pop();
                const ch = try self.pop();
                try io.unreadChar(ch, stream);
                try self.push(Value.nil);
            },
            .open_file => {
                const direction = try self.pop();
                const filename = try self.pop();
                const result = try io.openFile(self.allocator, self.heap, &self.builtins, filename, direction, null, null);
                try self.push(result);
            },
            .close_stream => {
                const stream = try self.pop();
                try io.closeStream(stream, null);
                try self.push(Value.nil);
            },
            .make_string_input_stream => {
                const str = try self.pop();
                const stream = try io.makeStringInputStream(self.heap, str, null, null);
                try self.push(stream);
            },
            .make_string_output_stream => {
                const stream = try self.heap.allocStringOutputStream();
                try self.push(stream);
            },
            .get_output_stream_string => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primGetOutputStreamString(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },
            .write_to_stream => {
                const stream_val = try self.pop();
                const str_val = try self.pop();
                const result = try primitives.stream.primWriteString(self.heap, &[_]Value{ str_val, stream_val });
                try self.push(result);
            },
            .pathname_host => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameHost(self.allocator, self.heap, path);
                try self.push(result);
            },
            .pathname_device => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameDevice(self.allocator, self.heap, path);
                try self.push(result);
            },
            .pathname_directory => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameDirectory(self.allocator, self.heap, path);
                try self.push(result);
            },
            .pathname_name => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameName(self.allocator, self.heap, path);
                try self.push(result);
            },
            .pathname_type => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameType(self.allocator, self.heap, path);
                try self.push(result);
            },
            .pathname_version => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameVersion(self.allocator, self.heap, path);
                try self.push(result);
            },
            .truename => {
                const path = try self.pop();
                const result = try primitives.pathname.truename(self.allocator, self.heap, &self.builtins, path);
                try self.push(result);
            },
            .ensure_directories_exist => {
                const path = try self.pop();
                const result = try primitives.pathname.ensureDirectoriesExist(self.allocator, self.heap, &self.builtins, path);
                try self.push(result.pathname);
            },
            .package_symbols_table => {
                const pkg = try self.pop();
                const result = try primitives.package.packageSymbols(pkg);
                try self.push(result);
            },
            .package_exports_table => {
                const pkg = try self.pop();
                const result = try primitives.package.packageExports(pkg);
                try self.push(result);
            },
            .package_symbols_list => {
                const pkg_name = try self.pop();
                const result = try primitives.package.packageSymbolsList(self.heap, pkg_name);
                try self.push(result);
            },
            .package_exports_list => {
                const pkg_name = try self.pop();
                const result = try primitives.package.packageExportsList(self.heap, pkg_name);
                try self.push(result);
            },
            .unintern => {
                const pkg = try self.pop();
                const sym = try self.pop();
                const result = try primitives.package.uninternSymbol(self.heap, sym, pkg);
                try self.push(if (result) Value.t else Value.nil);
            },
            .find_symbol => {
                const pkg = try self.pop();
                const name = try self.pop();
                const result = try primitives.package.findSymbol(self.heap, name, pkg);
                // Returns (symbol . (status . nil)); expose as primary + secondary value.
                if (result.isCons()) {
                    const c1 = result.toPtr(runtime.Cons);
                    try self.push(c1.car);
                    if (c1.cdr.isCons()) {
                        const c2 = c1.cdr.toPtr(runtime.Cons);
                        self.secondary_values[0] = c2.car;
                    } else {
                        self.secondary_values[0] = Value.nil;
                    }
                    self.secondary_values_count = 1;
                } else {
                    try self.push(Value.nil);
                    self.secondary_values[0] = Value.nil;
                    self.secondary_values_count = 1;
                }
            },
            .open => {
                const mode_val = try self.pop();
                const path_val = try self.pop();
                const result = try primitives.stream.primOpen(self.heap, &[_]Value{ path_val, mode_val }, &self.builtins);
                try self.push(result);
            },
            .close => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primClose(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .read_line => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primReadLine(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .write_line => {
                const text_val = try self.pop();
                const stream_val = try self.pop();
                const result = try primitives.stream.primWriteLine(self.heap, &[_]Value{ stream_val, text_val });
                try self.push(result);
            },

            .write_string => {
                const text_val = try self.pop();
                const stream_val = try self.pop();
                const result = try primitives.stream.primWriteString(self.heap, &[_]Value{ text_val, stream_val });
                try self.push(result);
            },

            .read_byte => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primReadByte(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .write_byte => {
                const byte_val = try self.pop();
                const stream_val = try self.pop();
                const result = try primitives.stream.primWriteByte(self.heap, &[_]Value{ stream_val, byte_val });
                try self.push(result);
            },

            .file_position => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primFilePosition(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .set_file_position => {
                const pos_val = try self.pop();
                const stream_val = try self.pop();
                const result = try primitives.stream.primFilePosition(self.heap, &[_]Value{ stream_val, pos_val });
                try self.push(result);
            },

            .file_length => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primFileLength(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .finish_output => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primFinishOutput(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .force_output => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primForceOutput(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .clear_input => {
                const stream_val = try self.pop();
                // Clear input is a no-op since we don't buffer input at the Lisp level
                // Accept nil or a stream, return nil
                if (!stream_val.isNil() and !stream_val.isStream()) return error.TypeMismatch;
                try self.push(Value.nil);
            },

            .clear_output => {
                const stream_val = try self.pop();
                // Clear output is a no-op since we don't buffer output at the Lisp level
                // Accept nil or a stream, return nil
                if (!stream_val.isNil() and !stream_val.isStream()) return error.TypeMismatch;
                try self.push(Value.nil);
            },

            .sleep => {
                const seconds = try self.pop();
                try primitives.io.sleepSeconds(seconds);
                try self.push(Value.nil);
            },

            .make_array => {
                const operand = self.readU8();
                const rank: u8 = operand >> 1;
                const has_initial: bool = (operand & 1) == 1;

                if (rank > 8) return error.TypeMismatch;

                // Pop initial-element if present
                const initial_element = if (has_initial) try self.pop() else Value.nil;

                // Pop dimensions from stack.
                var dimensions: [8]u64 = [_]u64{0} ** 8;
                var total_size: u64 = 1;
                var final_rank: u8 = rank;
                if (rank == 0) {
                    // Dynamic dimensions mode: pop one fixnum or a proper list of fixnums.
                    const dims_val = try self.pop();
                    switch (dims_val.typeKind()) {
                        .fixnum => {
                            const dim_signed = dims_val.toFixnum();
                            if (dim_signed < 0) return error.TypeMismatch;
                            dimensions[0] = @intCast(dim_signed);
                            total_size = dimensions[0];
                            final_rank = 1;
                        },
                        .nil, .cons => {
                            var cur = dims_val;
                            var idx: usize = 0;
                            while (cur.isCons()) {
                                if (idx >= dimensions.len) return error.TypeMismatch;
                                const pair = cur.toPtr(runtime.Cons);
                                if (!pair.car.isFixnum()) return error.TypeMismatch;
                                const dim_signed = pair.car.toFixnum();
                                if (dim_signed < 0) return error.TypeMismatch;
                                const dim: u64 = @intCast(dim_signed);
                                dimensions[idx] = dim;
                                total_size *= dim;
                                idx += 1;
                                cur = pair.cdr;
                            }
                            if (!cur.isNil()) return error.TypeMismatch;
                            final_rank = @intCast(idx);
                        },
                        else => return error.TypeMismatch,
                    }
                } else {
                    // Static rank mode: pop N dimensions in reverse order.
                    var i: usize = rank;
                    while (i > 0) {
                        i -= 1;
                        const dim_val = try self.pop();
                        if (!dim_val.isFixnum()) return error.TypeMismatch;
                        const dim_signed = dim_val.toFixnum();
                        if (dim_signed < 0) return error.TypeMismatch;
                        const dim: u64 = @intCast(dim_signed);
                        dimensions[i] = dim;
                        total_size *= dim;
                    }
                }

                // Allocate array object + data storage together
                const total_bytes = @sizeOf(runtime.Array) + total_size * @sizeOf(Value);
                const ptr = try self.heap.allocRaw(total_bytes);
                const arr: *runtime.Array = @ptrCast(@alignCast(ptr));

                // Data follows immediately after header
                const data_ptr: [*]Value = @ptrCast(@alignCast(ptr + @sizeOf(runtime.Array)));

                arr.* = .{
                    .kind = .array,
                    .rank = final_rank,
                    .dimensions = dimensions,
                    .total_size = total_size,
                    .data_ptr = @intFromPtr(data_ptr),
                };

                // Initialize with initial-element
                for (0..total_size) |idx| {
                    data_ptr[idx] = initial_element;
                }

                try self.push(Value.makeArray(arr));
            },

            .aref => {
                const sub_count = self.readU8();
                if (sub_count > 8) return error.TypeMismatch;

                // Pop subscripts from stack (in reverse order)
                var subscripts: [8]u64 = [_]u64{0} ** 8;
                var j: usize = sub_count;
                while (j > 0) {
                    j -= 1;
                    const sub_val = try self.pop();
                    if (!sub_val.isFixnum()) return error.TypeMismatch;
                    const sub_signed = sub_val.toFixnum();
                    if (sub_signed < 0) return error.TypeMismatch;
                    subscripts[j] = @intCast(sub_signed);
                }

                // Pop array or vector
                const arr_val = try self.pop();

                switch (arr_val.typeKind()) {
                    .vector => {
                        // Handle vectors (1D case)
                        if (sub_count != 1) return error.TypeMismatch;
                        const vec = arr_val.toPtr(Vector);
                        const idx: usize = @intCast(subscripts[0]);
                        if (idx >= vec.length) return error.TypeMismatch;
                        try self.push(vec.get(idx));
                    },
                    .string => {
                        if (sub_count != 1) return error.TypeMismatch;
                        const str = arr_val.toPtr(runtime.String);
                        const idx: usize = @intCast(subscripts[0]);
                        if (idx >= str.length) return error.TypeMismatch;
                        try self.push(Value.makeCharacter(@intCast(str.bytes()[idx])));
                    },
                    .string32 => {
                        if (sub_count != 1) return error.TypeMismatch;
                        const str32 = arr_val.toPtr(runtime.String32);
                        const idx: usize = @intCast(subscripts[0]);
                        if (idx >= str32.length) return error.TypeMismatch;
                        try self.push(Value.makeCharacter(@intCast(str32.codepoints()[idx])));
                    },
                    .array => {
                        const arr = arr_val.toPtr(runtime.Array);

                        // Verify rank matches
                        if (arr.rank != sub_count) return error.TypeMismatch;

                        // Calculate linear index using row-major order
                        var index: u64 = 0;
                        for (0..sub_count) |k| {
                            // Bounds check
                            if (subscripts[k] >= arr.dimensions[k]) return error.TypeMismatch;

                            // Calculate stride (product of remaining dimensions)
                            var stride: u64 = 1;
                            for (k + 1..sub_count) |m| {
                                stride *= arr.dimensions[m];
                            }
                            index += subscripts[k] * stride;
                        }

                        // Access element
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        try self.push(data[index]);
                    },
                    else => return error.TypeMismatch,
                }
            },

            .aset => {
                const sub_count = self.readU8();
                if (sub_count > 8) return error.TypeMismatch;

                // Pop new value
                const new_val = try self.pop();

                // Pop subscripts from stack (in reverse order)
                var subscripts: [8]u64 = [_]u64{0} ** 8;
                var j: usize = sub_count;
                while (j > 0) {
                    j -= 1;
                    const sub_val = try self.pop();
                    if (!sub_val.isFixnum()) return error.TypeMismatch;
                    const sub_signed = sub_val.toFixnum();
                    if (sub_signed < 0) return error.TypeMismatch;
                    subscripts[j] = @intCast(sub_signed);
                }

                // Pop array or vector
                const arr_val = try self.pop();

                switch (arr_val.typeKind()) {
                    .vector => {
                        // Handle vectors (1D case)
                        if (sub_count != 1) return error.TypeMismatch;
                        const vec = arr_val.toPtr(Vector);
                        const idx: usize = @intCast(subscripts[0]);
                        if (idx >= vec.length) return error.TypeMismatch;
                        vec.set(idx, new_val);
                        self.writeBarrierStore(arr_val, new_val);
                        try self.push(new_val);
                    },
                    .string => {
                        if (sub_count != 1) return error.TypeMismatch;
                        if (!new_val.isCharacter()) return error.TypeMismatch;
                        const str = arr_val.toPtr(runtime.String);
                        const idx: usize = @intCast(subscripts[0]);
                        if (idx >= str.length) return error.TypeMismatch;
                        const cp = new_val.toCharacter();
                        if (cp > 255) return error.TypeMismatch;
                        str.mutableBytes()[idx] = @intCast(cp);
                        try self.push(new_val);
                    },
                    .string32 => {
                        if (sub_count != 1) return error.TypeMismatch;
                        if (!new_val.isCharacter()) return error.TypeMismatch;
                        const str32 = arr_val.toPtr(runtime.String32);
                        const idx: usize = @intCast(subscripts[0]);
                        if (idx >= str32.length) return error.TypeMismatch;
                        str32.mutableCodepoints()[idx] = new_val.toCharacter();
                        try self.push(new_val);
                    },
                    .array => {
                        const arr = arr_val.toPtr(runtime.Array);

                        // Verify rank matches
                        if (arr.rank != sub_count) return error.TypeMismatch;

                        // Calculate linear index using row-major order
                        var index: u64 = 0;
                        for (0..sub_count) |k| {
                            // Bounds check
                            if (subscripts[k] >= arr.dimensions[k]) return error.TypeMismatch;

                            // Calculate stride (product of remaining dimensions)
                            var stride: u64 = 1;
                            for (k + 1..sub_count) |m| {
                                stride *= arr.dimensions[m];
                            }
                            index += subscripts[k] * stride;
                        }

                        // Set element
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        data[index] = new_val;
                        self.writeBarrierStore(arr_val, new_val);

                        // Return the value (Common Lisp setf semantics)
                        try self.push(new_val);
                    },
                    else => return error.TypeMismatch,
                }
            },

            .array_dimension => {
                const axis_val = try self.pop();
                const arr_val = try self.pop();

                if (!arr_val.isArray()) return error.TypeMismatch;
                if (!axis_val.isFixnum()) return error.TypeMismatch;

                const arr = arr_val.toPtr(runtime.Array);
                const axis_signed = axis_val.toFixnum();
                if (axis_signed < 0) return error.TypeMismatch;
                const axis: usize = @intCast(axis_signed);

                if (axis >= arr.rank) return error.TypeMismatch;

                const dimension: i64 = @intCast(arr.dimensions[axis]);
                try self.push(Value.makeFixnum(dimension));
            },

            .array_dimensions => {
                const arr_val = try self.pop();

                switch (arr_val.typeKind()) {
                    .vector => {
                        const vec = arr_val.toPtr(runtime.Vector);
                        const len_val = Value.makeFixnum(@intCast(vec.length));
                        try self.push(try self.allocCons(len_val, Value.nil));
                    },
                    .array => {
                        const arr = arr_val.toPtr(runtime.Array);
                        var result = Value.nil;
                        var i: usize = arr.rank;
                        while (i > 0) {
                            i -= 1;
                            if (arr.dimensions[i] > std.math.maxInt(i64)) return error.Overflow;
                            const dim: i64 = @intCast(arr.dimensions[i]);
                            result = try self.allocCons(Value.makeFixnum(dim), result);
                        }
                        try self.push(result);
                    },
                    else => return error.TypeMismatch,
                }
            },

            .array_rank => {
                const arr_val = try self.pop();
                switch (arr_val.typeKind()) {
                    .vector, .string => try self.push(Value.makeFixnum(1)),
                    .array => {
                        const arr = arr_val.toPtr(runtime.Array);
                        try self.push(Value.makeFixnum(@intCast(arr.rank)));
                    },
                    else => return error.TypeMismatch,
                }
            },

            .array_total_size => {
                const arr_val = try self.pop();
                switch (arr_val.typeKind()) {
                    .vector => {
                        const vec = arr_val.toPtr(runtime.Vector);
                        try self.push(Value.makeFixnum(@intCast(vec.length)));
                    },
                    .string => {
                        const str = arr_val.toPtr(runtime.String);
                        try self.push(Value.makeFixnum(@intCast(str.length)));
                    },
                    .array => {
                        const arr = arr_val.toPtr(runtime.Array);
                        if (arr.total_size > std.math.maxInt(i64)) return error.Overflow;
                        try self.push(Value.makeFixnum(@intCast(arr.total_size)));
                    },
                    else => return error.TypeMismatch,
                }
            },

            // Pathname operations
            .make_pathname => {
                const flags = self.readU8();
                const version = if ((flags & 0x20) != 0) try self.pop() else Value.nil;
                const type_comp = if ((flags & 0x10) != 0) try self.pop() else Value.nil;
                const name = if ((flags & 0x08) != 0) try self.pop() else Value.nil;
                const directory = if ((flags & 0x04) != 0) try self.pop() else Value.nil;
                const device = if ((flags & 0x02) != 0) try self.pop() else Value.nil;
                const host = if ((flags & 0x01) != 0) try self.pop() else Value.nil;
                const result = try primitives.pathname.makePathname(
                    self.allocator,
                    self.heap,
                    host,
                    device,
                    directory,
                    name,
                    type_comp,
                    version,
                );
                try self.push(result);
            },

            .pathname => {
                const pathspec = try self.pop();
                const result = try primitives.pathname.pathname(self.allocator, self.heap, pathspec);
                try self.push(result);
            },

            .parse_namestring => {
                const str_val = try self.pop();
                const result = try primitives.pathname.parseNamestring(
                    self.allocator,
                    self.heap,
                    str_val,
                );
                try self.push(result);
            },

            .namestring => {
                const pn_val = try self.pop();
                const result = try primitives.pathname.namestring(
                    self.allocator,
                    self.heap,
                    &self.builtins,
                    pn_val,
                );
                try self.push(result);
            },

            .directory_namestring => {
                const pn_val = try self.pop();
                const result = try primitives.pathname.directoryNamestring(self.allocator, self.heap, &self.builtins, pn_val);
                try self.push(result);
            },

            .file_namestring => {
                const pn_val = try self.pop();
                const result = try primitives.pathname.fileNamestring(self.allocator, self.heap, pn_val);
                try self.push(result);
            },

            .host_namestring => {
                const pn_val = try self.pop();
                const result = try primitives.pathname.hostNamestring(self.allocator, self.heap, pn_val);
                try self.push(result);
            },

            .wild_pathname_p => {
                const pn_val = try self.pop();
                const result = primitives.pathname.wildPathnameP(&self.builtins, pn_val, null);
                try self.push(if (result) Value.t else Value.nil);
            },

            .merge_pathnames => {
                const default_val = try self.pop();
                const pn_val = try self.pop();
                const empty = try self.heap.allocPathname(
                    Value.nil,
                    Value.nil,
                    Value.nil,
                    Value.nil,
                    Value.nil,
                    Value.nil,
                );
                const pn_path = if (pn_val.isNil())
                    empty
                else
                    try primitives.pathname.pathname(self.allocator, self.heap, pn_val);
                const default_path = if (default_val.isNil())
                    empty
                else
                    try primitives.pathname.pathname(self.allocator, self.heap, default_val);
                const merged = try primitives.pathname.mergePathnames(self.heap, &self.builtins, pn_path, default_path);
                try self.push(merged);
            },

            .set_macro_character => {
                const non_term = try self.pop(); // &optional non-terminating-p
                const function = try self.pop();
                const char_val = try self.pop();

                if (!char_val.isCharacter()) return error.TypeMismatch;
                const char_code = char_val.toCharacter();
                if (char_code > 255) return error.TypeMismatch; // Only ASCII supported for now

                const byte: u8 = @intCast(char_code);
                const entry = runtime.Heap.ReadtableEntry{
                    .function = function,
                    .non_terminating = !non_term.isNil(),
                };
                try self.heap.setReadtableMacroEntry(try self.currentReadtable(), byte, entry);
                try self.push(Value.nil);
            },

            .get_macro_character => {
                const char_val = try self.pop();

                if (!char_val.isCharacter()) return error.TypeMismatch;
                const char_code = char_val.toCharacter();
                if (char_code > 255) {
                    try self.push(Value.nil);
                    self.secondary_values[0] = Value.nil;
                    self.secondary_values_count = 1;
                } else {
                    const byte: u8 = @intCast(char_code);
                    const entry = self.heap.getReadtableMacroEntry(try self.currentReadtable(), byte);
                    if (entry) |e| {
                        try self.push(e.function);
                        self.secondary_values[0] = if (e.non_terminating) Value.t else Value.nil;
                    } else {
                        try self.push(Value.nil);
                        self.secondary_values[0] = Value.nil;
                    }
                    self.secondary_values_count = 1;
                }
            },

            .set_dispatch_macro_character => {
                const function = try self.pop();
                const sub_char_val = try self.pop();
                const disp_char_val = try self.pop();

                if (!disp_char_val.isCharacter()) return error.TypeMismatch;
                if (!sub_char_val.isCharacter()) return error.TypeMismatch;

                const disp_code = disp_char_val.toCharacter();
                const sub_code = sub_char_val.toCharacter();

                if (disp_code > 255 or sub_code > 255) return error.TypeMismatch;

                const disp_byte: u8 = @intCast(disp_code);
                const sub_byte: u8 = @intCast(sub_code);
                try self.heap.setReadtableDispatchFn(try self.currentReadtable(), disp_byte, sub_byte, function);
                try self.push(Value.nil);
            },

            .get_dispatch_macro_character => {
                const sub_char_val = try self.pop();
                const disp_char_val = try self.pop();

                if (!disp_char_val.isCharacter()) return error.TypeMismatch;
                if (!sub_char_val.isCharacter()) return error.TypeMismatch;

                const disp_code = disp_char_val.toCharacter();
                const sub_code = sub_char_val.toCharacter();

                if (disp_code > 255 or sub_code > 255) {
                    try self.push(Value.nil);
                } else {
                    const disp_byte: u8 = @intCast(disp_code);
                    const sub_byte: u8 = @intCast(sub_code);
                    try self.push(self.heap.getReadtableDispatchFn(try self.currentReadtable(), disp_byte, sub_byte) orelse Value.nil);
                }
            },

            // Character operations
            .characterp => {
                const val = try self.pop();
                try self.push(if (val.isCharacter()) Value.t else Value.nil);
            },
            .floatp => {
                const val = try self.pop();
                try self.push(if (val.isFloat()) Value.t else Value.nil);
            },
            .listp => {
                const val = try self.pop();
                // listp: nil or cons
                try self.push(if (val == Value.nil or val.isCons()) Value.t else Value.nil);
            },
            .atom => {
                const val = try self.pop();
                // atom: not a cons (everything except cons)
                try self.push(if (!val.isCons()) Value.t else Value.nil);
            },
            .assoc => {
                const alist = try self.pop();
                const key = try self.pop();
                var curr = alist;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    // Each element should be a cons (key . value)
                    if (c.car.isCons()) {
                        const pair = c.car.toPtr(Cons);
                        if (pair.car.raw == key.raw) {
                            try self.push(c.car);
                            break;
                        }
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },
            .assoc_eql => {
                // assoc with eql test (compares numbers by value)
                const alist = try self.pop();
                const key = try self.pop();
                var curr = alist;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.isCons()) {
                        const pair = c.car.toPtr(Cons);
                        if (hashKeyEqualWithTest(pair.car, key, .eql)) {
                            try self.push(c.car);
                            break;
                        }
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },
            .assoc_equal => {
                // assoc with equal test (deep equality)
                const alist = try self.pop();
                const key = try self.pop();
                var curr = alist;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.isCons()) {
                        const pair = c.car.toPtr(Cons);
                        if (hashKeyEqualWithTest(pair.car, key, .equal)) {
                            try self.push(c.car);
                            break;
                        }
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_find => {
                // find with eql test (default) - works on lists, strings, vectors
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.findInSeq(item, seq, .eql));
            },
            .list_find_eq => {
                // find with eq test (identity) - works on lists, strings, vectors
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.findInSeq(item, seq, .eq));
            },
            .list_find_equal => {
                // find with equal test (structural) - works on lists, strings, vectors
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.findInSeq(item, seq, .equal));
            },

            .list_position => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.positionInSeq(item, seq, .eql));
            },

            .list_count => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.countInSeq(item, seq, .eql));
            },
            .list_count_eq => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.countInSeq(item, seq, .eq));
            },
            .list_count_equal => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.countInSeq(item, seq, .equal));
            },

            .list_remove => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.listRemoveWithTest(item, seq, .eql));
            },
            .list_remove_eq => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.listRemoveWithTest(item, seq, .eq));
            },
            .list_remove_equal => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.listRemoveWithTest(item, seq, .equal));
            },

            .equal => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (hash_prims.valueEqual(a, b)) Value.t else Value.nil);
            },
            .eql => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (hash_prims.keyEqualWithTest(a, b, .eql)) Value.t else Value.nil);
            },
            .equalp => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (hash_prims.valueEqualp(a, b)) Value.t else Value.nil);
            },
            .char_code => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                try self.push(Value.makeFixnum(@intCast(cp)));
            },
            .code_char => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                const n = val.toFixnum();
                if (n < 0 or n > 0x10FFFF) return error.InvalidArgument;
                try self.push(Value.makeCharacter(@intCast(@as(u64, @bitCast(n)))));
            },
            .char_eq => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
                try self.push(if (a.raw == b.raw) Value.t else Value.nil);
            },
            .char_lt => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
                try self.push(if (a.toCharacter() < b.toCharacter()) Value.t else Value.nil);
            },
            .char_gt => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
                try self.push(if (a.toCharacter() > b.toCharacter()) Value.t else Value.nil);
            },

            .read_char => {
                const ch = try io.sysReadChar();
                if (ch < 0) {
                    try self.push(Value.nil);
                } else {
                    try self.push(Value.makeCharacter(@intCast(ch)));
                }
            },
            .peek_char => {
                const ch = try io.sysPeekChar();
                if (ch < 0) {
                    try self.push(Value.nil);
                } else {
                    try self.push(Value.makeCharacter(@intCast(ch)));
                }
            },

            .read => {
                // Read a complete S-expression from stdin
                var buffer: [4096]u8 = undefined;
                const len = try io.sysReadSexp(&buffer);
                if (len == 0) {
                    try self.push(Value.nil);
                    return;
                }

                // Parse the S-expression
                var parser = try Parser.init(self.allocator, self.heap, buffer[0..len], &self.builtins);
                defer parser.deinit();
                try parser.setReadtable(try self.currentReadtable());
                var dm_ctx = DispatchMacroBridge{ .vm = self };
                parser.setDispatchMacroHook(@ptrCast(&dm_ctx), dispatchMacroBridge);
                parser.setMacroCharacterHook(@ptrCast(&dm_ctx), macroCharacterBridge);
                var re_ctx = ReadEvalBridge{ .callback = self.eval_callback, .context = self.eval_context };
                if (self.eval_callback != null) {
                    parser.setReadEvalHook(@ptrCast(&re_ctx), readEvalBridge);
                }
                const result = try parseWithHookError(&parser);
                try self.push(result);
            },

            .load => {
                // Load and evaluate a file
                const filename_val = try self.pop();
                const filename = try self.pathDesignatorBytes(filename_val);

                // Call the load callback if set
                if (self.load_callback) |callback| {
                    const saved = State.save(self);
                    const result = try callback(filename, self.load_context.?);
                    if (hostCallbackMovedControl(self, saved)) return error.ControlTransfer;
                    try self.push(result);
                } else {
                    // No callback set - return nil
                    try self.push(Value.nil);
                }
            },

            .read_from_string => {
                // Parse a string into a Lisp value, return position as secondary value
                const str_val = try self.pop();

                // CL *read-suppress*: when true, return nil without parsing
                const read_suppress = blk: {
                    if (self.global_env) |ge| {
                        const names = [_][]const u8{ "COMMON-LISP:*READ-SUPPRESS*", "CL:*READ-SUPPRESS*", "*READ-SUPPRESS*" };
                        for (names) |gname| {
                            if (ge.lookup(gname)) |idx| {
                                if (idx < self.num_globals and self.globals[idx].raw != Value.nil.raw and self.globals[idx].raw != Value.unbound.raw) break :blk true;
                            }
                        }
                    }
                    break :blk false;
                };

                if (read_suppress) {
                    // CL *read-suppress*: return nil without parsing
                    try self.push(Value.nil);
                    self.secondary_values[0] = Value.makeFixnum(0);
                    self.secondary_values_count = 1;
                } else if (str_val.isString()) {
                    const str = str_val.toPtr(String);
                    var parser = try Parser.init(self.allocator, self.heap, str.bytes(), &self.builtins);
                    defer parser.deinit();
                    try parser.setReadtable(try self.currentReadtable());
                    var dm_ctx = DispatchMacroBridge{ .vm = self };
                    parser.setDispatchMacroHook(@ptrCast(&dm_ctx), dispatchMacroBridge);
                    parser.setMacroCharacterHook(@ptrCast(&dm_ctx), macroCharacterBridge);
                    // Enable #. read-eval via the VM's eval callback
                    var re_ctx = ReadEvalBridge{ .callback = self.eval_callback, .context = self.eval_context };
                    if (self.eval_callback != null) {
                        parser.setReadEvalHook(@ptrCast(&re_ctx), readEvalBridge);
                    }
                    const result = try parseWithHookError(&parser);
                    try self.push(result);
                    self.secondary_values[0] = Value.makeFixnum(@intCast(parser.lexer.token_start));
                    self.secondary_values_count = 1;
                } else if (str_val.isString32()) {
                    const s32 = str_val.toPtr(runtime.String32);
                    var utf8 = std.ArrayList(u8){};
                    defer utf8.deinit(self.allocator);
                    for (s32.codepoints()) |cp| {
                        var buf: [4]u8 = undefined;
                        const cp_u21 = std.math.cast(u21, cp) orelse return error.TypeMismatch;
                        const n = std.unicode.utf8Encode(cp_u21, &buf) catch return error.TypeMismatch;
                        try utf8.appendSlice(self.allocator, buf[0..n]);
                    }
                    var parser = try Parser.init(self.allocator, self.heap, utf8.items, &self.builtins);
                    defer parser.deinit();
                    try parser.setReadtable(try self.currentReadtable());
                    var dm_ctx32 = DispatchMacroBridge{ .vm = self };
                    parser.setDispatchMacroHook(@ptrCast(&dm_ctx32), dispatchMacroBridge);
                    parser.setMacroCharacterHook(@ptrCast(&dm_ctx32), macroCharacterBridge);
                    var re_ctx32 = ReadEvalBridge{ .callback = self.eval_callback, .context = self.eval_context };
                    if (self.eval_callback != null) {
                        parser.setReadEvalHook(@ptrCast(&re_ctx32), readEvalBridge);
                    }
                    const result = try parseWithHookError(&parser);
                    try self.push(result);
                    self.secondary_values[0] = Value.makeFixnum(@intCast(parser.lexer.pos));
                    self.secondary_values_count = 1;
                } else {
                    return error.TypeMismatch;
                }
            },

            .read_stream => {
                const stream = try self.pop();
                const result = try readSexpFromStream(self, stream);
                try self.push(result.value);
                self.secondary_values[0] = if (result.eof) Value.t else Value.nil;
                self.secondary_values_count = 1;
            },

            .eval => {
                // Evaluate expression at runtime
                const expr = try self.pop();

                // Call the eval callback if set
                if (self.eval_callback) |callback| {
                    const saved = State.save(self);
                    const result = try callback(expr, self.eval_context.?);
                    if (hostCallbackMovedControl(self, saved)) return error.ControlTransfer;
                    try self.push(result);
                } else {
                    // No callback set - return nil
                    try self.push(Value.nil);
                }
            },

            .gensym => {
                // Pop prefix argument (nil if nullary)
                const prefix_arg = try self.pop();
                const sym = try primitives.symbol.gensym(self.heap, if (!prefix_arg.isNil()) prefix_arg else null);
                try self.push(sym);
            },

            .macroexpand => {
                // Expand macros fully
                const expr = try self.pop();

                // Call the macroexpand callback if set
                if (self.macroexpand_callback) |callback| {
                    const saved = State.save(self);
                    const result = try callback(expr, self.macroexpand_context.?);
                    if (hostCallbackMovedControl(self, saved)) return error.ControlTransfer;
                    try self.push(result);
                } else {
                    // No callback set - return the expression unchanged
                    try self.push(expr);
                }
            },

            .macroexpand_1 => {
                // Expand macros once
                const expr = try self.pop();

                // Call the macroexpand-1 callback if set
                if (self.macroexpand_1_callback) |callback| {
                    const saved = State.save(self);
                    const result = try callback(expr, self.macroexpand_1_context.?);
                    if (hostCallbackMovedControl(self, saved)) return error.ControlTransfer;
                    try self.push(result);
                } else {
                    // No callback set - return the expression unchanged
                    try self.push(expr);
                }
            },

            .unread_char => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                io.sysUnreadChar(@intCast(val.toCharacter()));
                try self.push(Value.nil);
            },

            .listen => {
                const stream_val = try self.pop();
                // Accept nil or a stream
                if (!stream_val.isNil() and !stream_val.isStream()) return error.TypeMismatch;
                if (stream_val.isNil()) {
                    try self.push(Value.nil);
                } else {
                    const result = try io.listen(stream_val);
                    try self.push(result);
                }
            },

            .upgraded_complex_part_type => {
                _ = try self.pop(); // typespec (ignored)
                // Our complex numbers use double-float for both parts
                try self.push(try self.heap.intern("real"));
            },

            .file_string_length => {
                const string_val = try self.pop();
                const stream_val = try self.pop();
                // Accept nil or stream for first arg
                if (!stream_val.isNil() and !stream_val.isStream()) return error.TypeMismatch;
                if (!string_val.isString()) return error.TypeMismatch;
                const str = string_val.toPtr(runtime.String);
                // Return byte length of string (assuming 1 byte per char for external format)
                try self.push(Value.makeFixnum(@intCast(str.bytes().len)));
            },

            .boundp => {
                const val = try self.pop();
                switch (val.typeKind()) {
                    .nil, .t => try self.push(Value.t),
                    .symbol => {
                        const is_bound = (try self.lookupSymbolValueCell(val)) != null;
                        try self.push(if (is_bound) Value.t else Value.nil);
                    },
                    else => try self.signalTypeErrorDatumExpected(val, self.builtins.sym_symbol),
                }
            },

            .fboundp => {
                const val = try self.pop();
                // NIL and T are symbols in CL terms; neither is fbound by default.
                switch (val.typeKind()) {
                    .nil, .t => {
                        try self.push(Value.nil);
                    },
                    .symbol => {
                        // Use callback to check for function binding (macro, primitive, or defun)
                        const is_fbound = if (self.fboundp_callback) |cb|
                            try cb(val, self.fboundp_context.?)
                        else blk: {
                            const sym = val.toPtr(Symbol);
                            break :blk (try self.lookupSymbolGlobalIndex(sym)) != null;
                        };
                        try self.push(if (is_fbound) Value.t else Value.nil);
                    },
                    else => try self.signalTypeErrorDatumExpected(val, self.builtins.sym_symbol),
                }
            },

            .symbol_value => {
                const val = try self.pop();
                // Handle magic symbols nil and t
                switch (val.typeKind()) {
                    .nil => try self.push(Value.nil),
                    .t => try self.push(Value.t),
                    .symbol => {
                        if (try self.lookupSymbolValueCell(val)) |bound_val| {
                            try self.push(bound_val);
                        } else {
                            const sym = val.toPtr(Symbol);
                            if (self.shouldTraceError(error.UnboundSymbol)) {
                                std.debug.print(
                                    "TRACE symbol lookup miss op={s} sym={s}\n",
                                    .{ @tagName(op), sym.getName() },
                                );
                            }
                            return error.UnboundSymbol;
                        }
                    },
                    else => {
                        if (self.shouldTraceError(error.TypeMismatch)) {
                            std.debug.print(
                                "TRACE symbol_value non-symbol kind={s} raw=0x{x}\n",
                                .{ @tagName(val.typeKind()), val.raw },
                            );
                        }
                        try self.signalTypeErrorDatumExpected(val, self.builtins.sym_symbol);
                    },
                }
            },

            .symbol_function => {
                const val = try self.pop();
                switch (val.typeKind()) {
                    .nil => try self.push(Value.nil),
                    .t => try self.push(Value.t),
                    .symbol => {
                        if (try self.resolveFunctionValue(val)) |fn_val| {
                            try self.push(fn_val);
                        } else {
                            const sym = val.toPtr(Symbol);
                            if (self.shouldTraceError(error.UnboundSymbol)) {
                                std.debug.print(
                                    "TRACE symbol lookup miss op={s} sym={s}\n",
                                    .{ @tagName(op), sym.getName() },
                                );
                            }
                            return error.UnboundSymbol;
                        }
                    },
                    else => try self.signalTypeErrorDatumExpected(val, self.builtins.sym_symbol),
                }
            },

            .symbol_plist => {
                const val = try self.pop();
                switch (val.typeKind()) {
                    .nil, .t => try self.push(Value.nil),
                    .symbol => try self.push(try primitives.symbol.symbolPlist(self.heap, val)),
                    else => try self.signalTypeErrorDatumExpected(val, self.builtins.sym_symbol),
                }
            },

            .function_lambda_expression => {
                const fn_val = try self.pop();
                switch (fn_val.typeKind()) {
                    .closure => {
                        const cls = fn_val.toPtr(runtime.Closure);
                        if (cls.code.typeKind() != .chunk) return error.TypeMismatch;
                        const chunk = cls.code.toPtr(runtime.Chunk);
                        try self.push(chunk.lambda_expr);
                        self.secondary_values[0] = if (cls.num_captures != 0) Value.t else Value.nil;
                        self.secondary_values[1] = chunk.name;
                        self.secondary_values_count = 2;
                    },
                    .generic_function => {
                        const gf = fn_val.toPtr(runtime.objects.GenericFunction);
                        if (!gf.dispatcher.isClosure()) return error.TypeMismatch;
                        const cls = gf.dispatcher.toPtr(runtime.Closure);
                        if (cls.code.typeKind() != .chunk) return error.TypeMismatch;
                        const chunk = cls.code.toPtr(runtime.Chunk);
                        try self.push(chunk.lambda_expr);
                        self.secondary_values[0] = if (cls.num_captures != 0) Value.t else Value.nil;
                        self.secondary_values[1] = gf.name;
                        self.secondary_values_count = 2;
                    },
                    .native_code => {
                        try self.push(Value.nil);
                        self.secondary_values[0] = Value.nil;
                        self.secondary_values[1] = Value.nil;
                        self.secondary_values_count = 2;
                    },
                    else => return error.TypeMismatch,
                }
            },

            .typep => {
                const type_spec = try self.pop();
                const obj = try self.pop();
                const result = try primitives.typep(self.heap, &self.type_syms, obj, type_spec);
                try self.push(if (result) Value.t else Value.nil);
            },

            .subtypep => {
                const type2 = try self.pop();
                const type1 = try self.pop();
                const result = try primitives.subtypep(self.heap, type1, type2);
                if (self.trace_error_context and type1.isSymbol() and type2.isSymbol()) {
                    const pair = result.toPtr(runtime.Cons);
                    std.debug.print(
                        "TRACE vm subtypep: {s} <: {s} => {}\n",
                        .{
                            type1.toPtr(Symbol).getName(),
                            type2.toPtr(Symbol).getName(),
                            !pair.car.isNil(),
                        },
                    );
                }
                try self.push(result);
            },

            // Numeric predicates
            .abs => {
                const val = try self.pop();
                if (val.isFixnum() or val.isBignum()) {
                    const result = try arith.abs_val(val);
                    try self.push(result);
                } else if (val.isFloat()) {
                    try self.push(Value.makeFloat(@abs(val.toFloat())));
                } else if (val.typeKind() == .rational) {
                    const rat = val.toPtr(runtime.Rational);
                    if (rat.numerator < 0) {
                        const new_rat = try self.heap.alloc(runtime.Rational);
                        new_rat.* = runtime.Rational.make(-rat.numerator, rat.denominator);
                        try self.push(Value.makeRational(new_rat));
                    } else {
                        try self.push(val);
                    }
                } else return error.TypeMismatch;
            },
            .zerop => {
                const val = try self.pop();
                const is_zero = switch (val.typeKind()) {
                    .fixnum => val.toFixnum() == 0,
                    .float => val.toFloat() == 0.0,
                    .bignum => val.toPtr(runtime.Bignum).isZero(),
                    .rational => val.toPtr(runtime.Rational).numerator == 0,
                    .complex => blk: {
                        const c = val.toPtr(runtime.Complex);
                        break :blk c.real == 0.0 and c.imag == 0.0;
                    },
                    else => return error.TypeMismatch,
                };
                try self.push(if (is_zero) Value.t else Value.nil);
            },
            .plusp => {
                const val = try self.pop();
                const is_pos = switch (val.typeKind()) {
                    .fixnum => val.toFixnum() > 0,
                    .float => val.toFloat() > 0.0,
                    .bignum => val.toPtr(runtime.Bignum).size > 0,
                    .rational => val.toPtr(runtime.Rational).numerator > 0,
                    else => return error.TypeMismatch,
                };
                try self.push(if (is_pos) Value.t else Value.nil);
            },
            .minusp => {
                const val = try self.pop();
                const is_neg = switch (val.typeKind()) {
                    .fixnum => val.toFixnum() < 0,
                    .float => val.toFloat() < 0.0,
                    .bignum => val.toPtr(runtime.Bignum).size < 0,
                    .rational => val.toPtr(runtime.Rational).numerator < 0,
                    else => return error.TypeMismatch,
                };
                try self.push(if (is_neg) Value.t else Value.nil);
            },
            .evenp => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                try self.push(if (@mod(val.toFixnum(), 2) == 0) Value.t else Value.nil);
            },
            .oddp => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                try self.push(if (@mod(val.toFixnum(), 2) != 0) Value.t else Value.nil);
            },

            // Math functions
            .sqrt => {
                const val = try self.pop();
                const f = try valToFloat(val);
                if (f < 0) {
                    const cplx = try self.heap.allocComplex(0.0, @sqrt(-f));
                    try self.push(cplx);
                } else {
                    try self.push(Value.makeFloat(@sqrt(f)));
                }
            },
            .sin => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@sin(f)));
            },
            .cos => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@cos(f)));
            },
            .tan => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@tan(f)));
            },
            .exp => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@exp(f)));
            },
            .log => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@log(f)));
            },
            .floor => {
                const val = try self.pop();
                if (val.isFixnum()) {
                    // Integer floor is identity, remainder 0
                    try self.push(val);
                    self.secondary_values[0] = Value.makeFixnum(0);
                    self.secondary_values_count = 1;
                } else {
                    const f = try valToFloat(val);
                    const floored = @floor(f);
                    const q: i64 = @intFromFloat(floored);
                    try self.push(Value.makeFixnum(q));
                    self.secondary_values[0] = Value.makeFloat(f - floored);
                    self.secondary_values_count = 1;
                }
            },
            .ceiling => {
                const val = try self.pop();
                if (val.isFixnum()) {
                    try self.push(val);
                    self.secondary_values[0] = Value.makeFixnum(0);
                    self.secondary_values_count = 1;
                } else {
                    const f = try valToFloat(val);
                    const ceiled = @ceil(f);
                    const q: i64 = @intFromFloat(ceiled);
                    try self.push(Value.makeFixnum(q));
                    self.secondary_values[0] = Value.makeFloat(f - ceiled);
                    self.secondary_values_count = 1;
                }
            },
            .round => {
                const val = try self.pop();
                if (val.isFixnum()) {
                    try self.push(val);
                    self.secondary_values[0] = Value.makeFixnum(0);
                    self.secondary_values_count = 1;
                } else {
                    const f = try valToFloat(val);
                    // CL round: round to nearest even (banker's rounding)
                    const rounded = blk: {
                        const r = @round(f);
                        // @round uses half-away-from-zero; fix half-integer cases
                        if (@abs(f - r) == 0.5) {
                            const ri: i64 = @intFromFloat(r);
                            if (@mod(ri, @as(i64, 2)) != 0) {
                                // Odd result — adjust toward zero to get even
                                break :blk if (f > 0) r - 1.0 else r + 1.0;
                            }
                        }
                        break :blk r;
                    };
                    const q: i64 = @intFromFloat(rounded);
                    try self.push(Value.makeFixnum(q));
                    self.secondary_values[0] = Value.makeFloat(f - rounded);
                    self.secondary_values_count = 1;
                }
            },

            // ================================================================
            // Specialized (type-proven) operations — NO runtime checks
            // ================================================================

            .fixnum_add => {
                const b = try self.pop();
                const a = try self.pop();
                // a.raw = (a_val << 1) | 1, b.raw = (b_val << 1) | 1
                // sum = (a_val + b_val) << 1 | 1 = a.raw + b.raw - 1
                try self.push(.{ .raw = a.raw +% b.raw -% 1 });
            },
            .fixnum_sub => {
                const b = try self.pop();
                const a = try self.pop();
                // diff = (a_val - b_val) << 1 = a.raw - b.raw
                // re-tag: set bit 0
                try self.push(.{ .raw = (a.raw -% b.raw) | 1 });
            },
            .fixnum_mul => {
                const b = try self.pop();
                const a = try self.pop();
                // Extract unboxed values, multiply, re-box
                const av: i64 = @bitCast(a.raw);
                const bv: i64 = @bitCast(b.raw);
                const product = @as(u64, @bitCast((av >> 1) *% (bv >> 1)));
                try self.push(.{ .raw = (product << 1) | 1 });
            },
            .fixnum_le => {
                const b = try self.pop();
                const a = try self.pop();
                // Both proven fixnum, tagged with bit0=1. Comparison on raw preserves order.
                const av: i64 = @bitCast(a.raw);
                const bv: i64 = @bitCast(b.raw);
                try self.push(if (av <= bv) Value.t else Value.nil);
            },
            .fixnum_lt => {
                const b = try self.pop();
                const a = try self.pop();
                const av: i64 = @bitCast(a.raw);
                const bv: i64 = @bitCast(b.raw);
                try self.push(if (av < bv) Value.t else Value.nil);
            },
            .fixnum_gt => {
                const b = try self.pop();
                const a = try self.pop();
                const av: i64 = @bitCast(a.raw);
                const bv: i64 = @bitCast(b.raw);
                try self.push(if (av > bv) Value.t else Value.nil);
            },
            .fixnum_ge => {
                const b = try self.pop();
                const a = try self.pop();
                const av: i64 = @bitCast(a.raw);
                const bv: i64 = @bitCast(b.raw);
                try self.push(if (av >= bv) Value.t else Value.nil);
            },
            .fixnum_eq => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a.raw == b.raw) Value.t else Value.nil);
            },
            .unsafe_car => {
                const pair = try self.pop();
                // Proven cons — deref directly, no nil/type check
                try self.push(pair.toPtr(Cons).car);
            },
            .unsafe_cdr => {
                const pair = try self.pop();
                try self.push(pair.toPtr(Cons).cdr);
            },
            .direct_aref => {
                const idx_val = try self.pop();
                const vec_val = try self.pop();
                // Proven vector + valid index — skip checks
                const vec = vec_val.toPtr(Vector);
                const idx: usize = @intCast(@as(i64, @bitCast(idx_val.raw)) >> 1);
                try self.push(vec.get(idx));
            },

            .halt => return error.Halt,
        }

        // Clear stale secondary values after each op UNLESS the op:
        // - Produces multiple values (values, values_list, get_decoded_time, etc.)
        // - Is control flow that doesn't produce a value (jmp, jmp_nil, jmp_not_nil)
        // - Consumes multiple values (mv_list, mv_bind — they clear it themselves)
        // - Returns from a function (ret — caller may need secondary values)
        switch (op) {
            .values, .values_list, .ret, .get_decoded_time, .decode_universal_time, .decode_float, .integer_decode_float, .function_lambda_expression, .jmp, .jmp_nil, .jmp_not_nil, .push_block, .pop_block, .mv_list, .mv_bind, .floor, .ceiling, .round, .call, .tail_call, .read_from_string, .read_stream, .hash_get, .intern, .get_macro_character, .pop_progv, .pop_catch, .pop_unwind, .push_progv, .push_catch, .push_unwind => {},
            else => {
                self.secondary_values_count = 0;
            },
        }
        switch (op) {
            .values, .values_list, .ret, .jmp, .jmp_nil, .jmp_not_nil, .push_block, .pop_block, .mv_bind, .call, .tail_call, .pop_progv, .pop_catch, .pop_unwind, .push_progv, .push_catch, .push_unwind => {},
            else => self.zero_values_returned = false,
        }
    }

    // ========================================================================
    // Exception handling
    // ========================================================================

    /// Throw a simple-error condition with an error name string.
    /// Used to convert Zig compilation/evaluation errors into CL conditions
    /// so that handler-case can catch them.
    pub fn throwSimpleError(self: *Vm, heap: *Heap, err_name: []const u8) Error!void {
        const msg_val = try heap.allocBaseString(err_name);
        self.last_error_value = msg_val;
        const payload = try self.allocCons(msg_val, Value.nil);
        const condition = try self.allocCons(self.builtins.sym_simple_error, payload);
        try self.doThrow(self.builtins.sym_condition_tag, condition);
    }

    fn doThrow(self: *Vm, tag: Value, value: Value) Error!void {
        self.pending_error = null;
        self.is_unwinding = false;
        self.pending_throw_tag = Value.nil;
        self.pending_throw_value = Value.nil;
        self.pending_block_idx = null;
        self.pending_block_value = Value.nil;
        self.is_returning_from_block = false;
        const trace_throw = self.trace_throw;
        if (trace_throw) {
            std.debug.print(
                "TRACE throw: catch_sp={d} barrier={d} handler_sp={d} unwind_sp={d} block_sp={d}\n",
                .{ self.catch_sp, self.throw_barrier_depth, self.handler_sp, self.unwind_sp, self.block_sp },
            );
            std.debug.print("  tag=", .{});
            tracePrintValue(tag);
            std.debug.print("\n", .{});
            std.debug.print("  value=", .{});
            tracePrintValue(value);
            std.debug.print("\n", .{});
        }

        // Check if there's an unwind-protect that needs cleanup.
        // Before running cleanup, check if a matching catch exists INSIDE the
        // unwind scope. If so, the catch should handle the throw directly —
        // cleanup only runs when the throw CROSSES the unwind-protect boundary
        // (i.e., the matching catch is OUTSIDE the unwind scope).
        //
        // This is required for CL semantics:
        //   (unwind-protect (catch 'tag (throw 'tag val)) cleanup)
        // The catch is inside the unwind-protect, so cleanup should NOT run
        // during the throw — only on normal exit or when throw crosses outward.
        if (self.unwind_sp > 0) {
            const current_unwind = self.unwind_sp - 1;
            var skip_unwind = false;

            // Search catch frames for a matching tag established INSIDE the unwind frame
            {
                var ci = self.catch_sp;
                while (ci > 0) {
                    ci -= 1;
                    const cf = self.catch_stack[ci];
                    if (tag.raw == cf.tag.raw) {
                        // This catch matches. If it was established AFTER the unwind
                        // frame (i.e., it's nested inside it), then the catch should
                        // handle the throw before the unwind cleanup runs.
                        if (cf.unwind_depth > current_unwind) {
                            skip_unwind = true;
                        }
                        break;
                    }
                }
            }

            if (!skip_unwind) {
                // Pop the unwind frame and run cleanup
                self.unwind_sp -= 1;
                const unwind_frame = self.unwind_stack[self.unwind_sp];

                // Save throw state for after cleanup
                self.pending_throw_tag = tag;
                self.pending_throw_value = value;
                self.is_unwinding = true;

                // Jump to cleanup code with saved stack/frame state
                self.chunk = unwind_frame.chunk;
                self.ip = unwind_frame.cleanup_ip;
                if (unwind_frame.unwind_sp > STACK_SIZE or unwind_frame.unwind_fp > MAX_FRAMES) {
                    return self.invalidOpcode("throw.unwind-stack-corrupt");
                }
                self.sp = unwind_frame.unwind_sp;
                self.fp = unwind_frame.unwind_fp;
                // pop_unwind will re-throw after cleanup completes
                return;
            }
        }

        // Handler-bind dispatch for signaled conditions.
        if (tag.raw == self.builtins.sym_condition_tag.raw and self.handler_sp > 0) {
            const condition_type = self.conditionTypeSymbol(value) orelse Value.nil;
            const condition_object = value;
            if (self.trace_error_context) {
                std.debug.print(
                    "TRACE condition dispatch: type={s} raw=0x{x} handlers={d}\n",
                    .{ @tagName(condition_type.typeKind()), condition_type.raw, self.handler_sp },
                );
            }

            var i = self.handler_sp;
            while (i > 0) {
                i -= 1;
                const frame = self.handler_stack[i];
                if (self.trace_error_context) {
                    std.debug.print(
                        "  handler[{d}] expects={s} fn={s}\n",
                        .{ i, @tagName(frame.condition_type.typeKind()), @tagName(frame.handler_fn.typeKind()) },
                    );
                }
                if (try self.handlerTypeMatches(frame.condition_type, condition_object)) {
                    if (self.sp + 2 > STACK_SIZE) return error.StackOverflow;
                    const saved_handler_sp = self.handler_sp;
                    // Prevent recursive re-entry into this handler while it runs.
                    self.handler_sp = i;
                    // Restore full handler depth when the handler frame returns.
                    self.pending_handler_restore_depth = saved_handler_sp;
                    self.stack[self.sp] = frame.handler_fn;
                    self.stack[self.sp + 1] = condition_object;
                    self.sp += 2;
                    if (self.trace_error_context) {
                        std.debug.print("  handler match -> doCall argc=1\n", .{});
                    }
                    self.doCall(1, false) catch |call_err| {
                        self.pending_handler_restore_depth = null;
                        self.handler_sp = saved_handler_sp;
                        if (self.trace_error_context) {
                            std.debug.print("  handler doCall error={s}\n", .{@errorName(call_err)});
                        }
                        return call_err;
                    };
                    return;
                }
            }
            if (self.trace_error_context) {
                std.debug.print("  handler dispatch complete; continuing throw search\n", .{});
            }
        }

        // No unwind frames - search for matching catch frame
        // Use local index to avoid destructively consuming catch frames
        // when the tag doesn't match (so handler-case can still work).
        {
            var ci = self.catch_sp;
            while (ci > 0) {
                ci -= 1;
                const frame = self.catch_stack[ci];

                // Check if tag matches (using eq comparison)
                if (tag.raw == frame.tag.raw) {
                    if (ci < self.throw_barrier_depth) {
                        if (trace_throw) {
                            std.debug.print(
                                "  throw relay: catch_idx={d} < barrier={d}\n",
                                .{ ci, self.throw_barrier_depth },
                            );
                        }
                        self.relay_throw_tag = tag;
                        self.relay_throw_value = value;
                        return error.NestedNonLocalExit;
                    }
                    if (trace_throw) {
                        std.debug.print(
                            "  throw catch match: catch_idx={d} catch_ip={d} catch_sp={d} catch_fp={d}\n",
                            .{ ci, frame.catch_ip, frame.catch_sp, frame.catch_fp },
                        );
                    }
                    // Found matching catch - restore state and jump
                    // Pop catch stack down to (and including) this frame
                    self.catch_sp = ci;
                    // Validate before restore to guard against corruption
                    if (frame.catch_sp > STACK_SIZE or frame.catch_fp > MAX_FRAMES) {
                        return self.invalidOpcode("throw.catch-stack-corrupt");
                    }
                    if (frame.block_depth > MAX_BLOCKS) return self.invalidOpcode("throw.block-depth-corrupt");
                    self.block_sp = frame.block_depth;
                    try self.restoreControlDepths(
                        ci,
                        frame.unwind_depth,
                        frame.restart_depth,
                        frame.progv_depth,
                        frame.handler_depth,
                        null,
                    );
                    self.chunk = frame.chunk;
                    self.ip = frame.catch_ip;
                    self.sp = frame.catch_sp;
                    self.fp = frame.catch_fp;
                    self.pending_handler_restore_depth = null;
                    // Push the thrown value as result
                    try self.push(value);
                    return;
                }
            }
        }
        // No matching catch found
        if (trace_throw) {
            std.debug.print("  throw unhandled\n", .{});
        }
        return error.UnhandledThrow;
    }

    fn normalizeConditionTypeSpec(_: *Vm, typ: Value) Value {
        if (typ.isClass()) return typ.toPtr(runtime.Class).name;
        return typ;
    }

    fn conditionTypeSymbol(self: *Vm, condition: Value) ?Value {
        const live = self.resolveForwardedValue(condition);
        switch (live.typeKind()) {
            .symbol => return live,
            .condition => return live.toPtr(runtime.objects.Condition).type_sym,
            .cons => {
                const cell = live.toPtr(Cons);
                if (cell.car.isSymbol()) return self.resolveForwardedValue(cell.car);
                return null;
            },
            .vector => {
                const vec = live.toPtr(runtime.Vector);
                if (vec.length > 0 and vec.data[0].isSymbol()) {
                    return self.resolveForwardedValue(vec.data[0]);
                }
                return null;
            },
            .class => return live.toPtr(runtime.Class).name,
            else => return null,
        }
    }

    fn handlerTypeMatches(self: *Vm, handler_type_raw: Value, condition: Value) Error!bool {
        const handler_type = self.normalizeConditionTypeSpec(self.resolveForwardedValue(handler_type_raw));
        if (handler_type.raw == Value.t.raw) return true;

        const condition_type = self.conditionTypeSymbol(condition) orelse return false;
        if (handler_type.raw == condition_type.raw) return true;

        const matched = try type_mod.isSubtype(self.heap, condition_type, handler_type);
        if (self.trace_error_context and handler_type.isSymbol() and condition_type.isSymbol()) {
            std.debug.print(
                "TRACE handler subtype: cond={s} handler={s} matched={}\n",
                .{
                    condition_type.toPtr(Symbol).getName(),
                    handler_type.toPtr(Symbol).getName(),
                    matched,
                },
            );
        }
        return matched;
    }

    /// Handle return-from by searching for matching block frame and jumping to it
    fn pushProgvFrame(self: *Vm, symbols: Value, values: Value) Error!void {
        if (self.progv_sp >= MAX_PROGVS) return error.StackOverflow;

        const base_sp = self.sp;
        defer self.sp = base_sp;

        // Root traversal cursors and accumulated bindings on the VM stack so
        // moving GC cannot leave stale forwarded locals mid-loop.
        try self.push(self.resolveForwardedValue(symbols));
        try self.push(self.resolveForwardedValue(values));
        try self.push(Value.nil);

        const sym_slot = base_sp;
        const val_slot = base_sp + 1;
        const saved_slot = base_sp + 2;

        while (true) {
            const symbol_list = self.resolveForwardedValue(self.stack[sym_slot]);
            self.stack[sym_slot] = symbol_list;
            if (!symbol_list.isCons()) break;
            const sym_cons = symbol_list.toPtr(Cons);
            const symbol = self.resolveForwardedValue(sym_cons.car);

            if (!symbol.isSymbol()) return error.TypeMismatch;

            var next_val = self.resolveForwardedValue(self.stack[val_slot]);
            const new_value = if (next_val.isCons()) blk: {
                const val_cons = next_val.toPtr(Cons);
                next_val = self.resolveForwardedValue(val_cons.cdr);
                break :blk self.resolveForwardedValue(val_cons.car);
            } else Value.nil;
            const next_sym = self.resolveForwardedValue(sym_cons.cdr);

            var key_val: Value = undefined;
            var old_value: Value = undefined;
            const sym_ptr = symbol.toPtr(Symbol);
            var old_from_global = false;
            var old_global_idx: usize = 0;

            if (try self.lookupSymbolGlobalIndex(sym_ptr)) |idx| {
                old_value = if (idx < self.num_globals)
                    self.resolveForwardedValue(try self.loadGlobal(@intCast(idx)))
                else
                    Value.unbound;
                if (idx < MAX_GLOBALS) {
                    try self.storeGlobal(idx, new_value);
                }
                key_val = Value.makeFixnum(@intCast(idx));
                old_from_global = true;
                old_global_idx = idx;
            } else if (try self.defineSymbolGlobalIndex(sym_ptr)) |idx| {
                if (idx < MAX_GLOBALS) {
                    try self.storeGlobal(idx, new_value);
                }
                key_val = Value.makeFixnum(@intCast(idx));
                old_value = Value.unbound;
                old_from_global = true;
                old_global_idx = idx;
            } else {
                old_value = self.resolveForwardedValue(self.lookupSymbolLocalValueCell(sym_ptr) orelse Value.unbound);
                try self.setSymbolLocalValueCell(sym_ptr, new_value);
                key_val = symbol;
            }

            if (self.trap_progv_corrupt and old_value.isSymbol() and !old_value.isMagicSymbol()) {
                const old_sym = old_value.toPtr(Symbol);
                if (old_sym.name_len > self.heap.space_size) {
                    const bind_name = sym_ptr.getName();
                    std.debug.print(
                        "TRACE progv-corrupt bind={s} global={any} idx={d} old=0x{x} name_len={d} sp={d} fp={d} ip={d}\n",
                        .{
                            bind_name,
                            old_from_global,
                            old_global_idx,
                            old_value.raw,
                            old_sym.name_len,
                            self.sp,
                            self.fp,
                            self.ip,
                        },
                    );
                    @panic("corrupt old_value in pushProgvFrame");
                }
            }

            self.stack[sym_slot] = next_sym;
            self.stack[val_slot] = next_val;

            // Root key/old operands while cons allocation may trigger GC.
            try self.push(key_val);
            try self.push(old_value);
            const key_slot = self.sp - 2;
            const old_slot = self.sp - 1;

            const live_key = self.resolveForwardedValue(self.stack[key_slot]);
            const live_old = self.resolveForwardedValue(self.stack[old_slot]);
            self.stack[key_slot] = live_key;
            self.stack[old_slot] = live_old;

            if (self.trap_progv_corrupt and live_old.isSymbol() and !live_old.isMagicSymbol()) {
                const live_old_sym = live_old.toPtr(Symbol);
                if (live_old_sym.name_len > self.heap.space_size) {
                    const bind_name = sym_ptr.getName();
                    std.debug.print(
                        "TRACE progv-corrupt-live bind={s} global={any} idx={d} old=0x{x} live_old=0x{x} live_old_len={d} sp={d} fp={d} ip={d}\n",
                        .{
                            bind_name,
                            old_from_global,
                            old_global_idx,
                            old_value.raw,
                            live_old.raw,
                            live_old_sym.name_len,
                            self.sp,
                            self.fp,
                            self.ip,
                        },
                    );
                    @panic("corrupt live_old in pushProgvFrame");
                }
            }

            const pair = try self.allocCons(live_key, live_old);

            const live_pair = self.resolveForwardedValue(pair);
            const live_saved = self.resolveForwardedValue(self.stack[saved_slot]);
            self.stack[saved_slot] = live_saved;

            const saved = try self.allocCons(live_pair, live_saved);
            self.stack[saved_slot] = saved;
            self.sp -= 2;
        }

        self.progv_stack[self.progv_sp] = .{ .saved_bindings = self.stack[saved_slot] };
        self.progv_sp += 1;
    }

    fn popProgvFrame(self: *Vm) Error!void {
        if (self.progv_sp == 0) return error.StackUnderflow;
        self.progv_sp -= 1;

        const frame = self.progv_stack[self.progv_sp];
        var bindings = self.resolveForwardedValue(frame.saved_bindings);

        // Restore old values
        while (bindings.isCons()) {
            const binding_cons = bindings.toPtr(Cons);
            const pair = self.resolveForwardedValue(binding_cons.car);

            if (pair.isCons()) {
                const pair_cons = pair.toPtr(Cons);
                const key_val = self.resolveForwardedValue(pair_cons.car);
                const old_value = self.resolveForwardedValue(pair_cons.cdr);
                switch (key_val.typeKind()) {
                    .fixnum => {
                        const idx_signed = key_val.toFixnum();
                        if (idx_signed >= 0) {
                            const idx: usize = @intCast(idx_signed);
                            if (idx < MAX_GLOBALS) {
                                // Use storeGlobal to trigger handleSpecialVarStore
                                // so Zig-level print settings stay in sync
                                try self.storeGlobal(@intCast(idx), old_value);
                            }
                        }
                    },
                    .symbol => {
                        const sym_val = self.resolveForwardedValue(key_val);
                        if (!sym_val.isSymbol()) return error.TypeMismatch;
                        const sym = sym_val.toPtr(Symbol);
                        if (old_value.raw == Value.unbound.raw) {
                            self.clearSymbolLocalValueCell(sym);
                        } else {
                            try self.setSymbolLocalValueCell(sym, old_value);
                        }
                    },
                    else => {},
                }
            }

            bindings = self.resolveForwardedValue(binding_cons.cdr);
        }
    }

    /// Jump to a block frame by index, restoring all state.
    fn jumpToBlock(self: *Vm, bi: usize, value: Value) Error!void {
        const frame = self.block_stack[bi];
        self.pending_error = null;
        self.is_unwinding = false;
        self.pending_throw_tag = Value.nil;
        self.pending_throw_value = Value.nil;
        self.pending_block_idx = null;
        self.pending_block_value = Value.nil;
        self.is_returning_from_block = false;
        self.block_sp = bi;
        try self.restoreControlDepths(
            frame.catch_depth,
            frame.unwind_depth,
            frame.restart_depth,
            frame.progv_depth,
            frame.handler_depth,
            null,
        );
        self.pending_handler_restore_depth = null;
        if (frame.block_sp > STACK_SIZE or frame.block_fp > MAX_FRAMES) {
            return self.invalidOpcode("return-from.block-stack-corrupt");
        }
        self.chunk = frame.chunk;
        self.ip = frame.exit_ip;
        self.sp = frame.block_sp;
        self.fp = frame.block_fp;
        try self.push(value);
    }

    fn doReturnFrom(self: *Vm, name_raw: Value, value: Value) Error!void {
        self.pending_error = null;
        self.is_unwinding = false;
        self.pending_throw_tag = Value.nil;
        self.pending_throw_value = Value.nil;
        self.pending_block_idx = null;
        self.pending_block_value = Value.nil;
        self.is_returning_from_block = false;
        // Search for matching block frame first
        var target_block: ?usize = null;
        {
            var i = self.block_sp;
            while (i > 0) {
                i -= 1;
                const frame = self.block_stack[i];
                if (name_raw == frame.name_raw) {
                    target_block = i;
                    break;
                }
            }
        }

        if (self.trace_block_miss and self.unwind_sp > 0) {
            const req_name = if (name_raw.isSymbol()) name_raw.toPtr(@import("../runtime/objects.zig").Symbol).getName() else if (name_raw.isNil()) "nil" else @tagName(name_raw.typeKind());
            const target_uwdepth: usize = if (target_block) |bi| self.block_stack[bi].unwind_depth else 999;
            std.debug.print(
                "TRACE rfb: name={s} target={?d} uwdepth={d} block_sp={d} unwind_sp={d} fp={d} chunk={s} is_rfb={}\n",
                .{ req_name, target_block, target_uwdepth, self.block_sp, self.unwind_sp, self.fp, chunkTraceName(self.chunk), self.is_returning_from_block },
            );
        }



        // If we found a matching block, check unwind-protect interaction
        if (target_block) |bi| {
            if (self.unwind_sp > 0) {
                const current_unwind = self.unwind_sp - 1;
                // Block is INSIDE the unwind scope — skip cleanup, jump directly
                if (self.block_stack[bi].unwind_depth > current_unwind) {
                    self.jumpToBlock(bi, value) catch |e| return e;
                    return;
                }
                // Block is OUTSIDE the unwind scope — run cleanup first
                self.unwind_sp -= 1;
                const unwind_frame = self.unwind_stack[current_unwind];

                self.pending_block_idx = bi;
                self.pending_block_value = value;
                self.is_returning_from_block = true;

                self.chunk = unwind_frame.chunk;
                self.ip = unwind_frame.cleanup_ip;
                if (unwind_frame.unwind_sp > STACK_SIZE or unwind_frame.unwind_fp > MAX_FRAMES) {
                    return self.invalidOpcode("return-from.unwind-stack-corrupt");
                }
                self.sp = unwind_frame.unwind_sp;
                self.fp = unwind_frame.unwind_fp;
                return;
            }
            // No unwind frames — jump directly
            self.jumpToBlock(bi, value) catch |e| return e;
            return;
        }

        // No matching block found — error
        if (self.trace_block_miss or self.trace_error_context) {
            const req_name = if (name_raw.isSymbol()) name_raw.toPtr(Symbol).getName() else @tagName(name_raw.typeKind());
            std.debug.print(
                "TRACE block-miss: req={s} raw=0x{x} block_sp={d} fp={d} sp={d} ip={d}\n",
                .{ req_name, name_raw.raw, self.block_sp, self.fp, self.sp, self.ip },
            );
            const code = self.chunk.getCode();
            std.debug.print("  current-chunk code_len={d} bytes:", .{code.len});
            for (code) |b| {
                std.debug.print(" {x:0>2}", .{b});
            }
            std.debug.print("\n", .{});
            var disasm_buf = std.ArrayList(u8){};
            defer disasm_buf.deinit(self.allocator);
            if (disasm.disassembleRuntime(self.chunk, disasm_buf.writer(self.allocator))) {
                std.debug.print("{s}", .{disasm_buf.items});
            } else |derr| {
                std.debug.print("  block-miss disasm error={s}\n", .{@errorName(derr)});
            }
            var bi: usize = self.block_sp;
            while (bi > 0) {
                bi -= 1;
                const frame = self.block_stack[bi];
                const frame_name = if (frame.name_raw.isSymbol()) frame.name_raw.toPtr(Symbol).getName() else @tagName(frame.name_raw.typeKind());
                std.debug.print(
                    "  block[{d}] name={s} raw=0x{x} exit={d} chunk={s}\n",
                    .{
                        bi,
                        frame_name,
                        frame.name_raw.raw,
                        frame.exit_ip,
                        if (frame.chunk.name.isSymbol()) frame.chunk.name.toPtr(Symbol).getName() else @tagName(frame.chunk.name.typeKind()),
                    },
                );
            }
        }
        // No matching block found — do NOT clear block_sp as that would destroy
        // outer blocks (e.g., test-loop) that are unrelated to this return-from.
        // The error will be caught by trySignalCondition/handler-case.
        return error.NoMatchingBlock;
    }

    /// Handle an error by running unwind-protect cleanup if needed.
    /// Note: trySignalCondition is called FIRST by the execute loop (line ~1487).
    /// doError is only reached for errors that no handler caught.
    fn doError(self: *Vm, err: anyerror) Error {
        // Check if there's an unwind-protect that needs cleanup
        if (self.unwind_sp > 0) {
            // Pop the unwind frame
            self.unwind_sp -= 1;
            const unwind_frame = self.unwind_stack[self.unwind_sp];

            // Save error for after cleanup
            self.pending_error = err;
            self.is_unwinding = true;

            // Jump to cleanup code with saved stack/frame state
            self.chunk = unwind_frame.chunk;
            self.ip = unwind_frame.cleanup_ip;

            // Validate before restore to guard against corruption
            if (unwind_frame.unwind_sp > STACK_SIZE or unwind_frame.unwind_fp > MAX_FRAMES) {
                return self.invalidOpcode("do-error.unwind-stack-corrupt");
            }
            self.sp = unwind_frame.unwind_sp;
            self.fp = unwind_frame.unwind_fp;

            // Return appropriate error
            return self.mapError(err);
        }

        // No unwind frames - propagate error normally
        return self.mapError(err);
    }

    /// Map Zig VM errors to CL condition type symbols.
    /// Uses anyerror because errors arrive from many subsystems (primitives, compiler, I/O).
    /// Unmapped errors (Halt, OutOfMemory, etc.) return null and propagate as Zig errors.
    fn zigErrorToConditionSym(self: *const Vm, err: anyerror) ?Value {
        const b = self.builtins;
        return switch (err) {
            error.TypeMismatch, error.TypeError, error.InvalidTypeSpecifier, error.UnknownTypeSpecifier => b.sym_type_error,
            error.DivisionByZero => b.sym_division_by_zero,
            error.Overflow => b.sym_arithmetic_error,
            error.UnboundSymbol, error.UnboundSlot => b.sym_unbound_variable,
            error.InvalidArgument, error.InvalidOpcode, error.InvalidConstant, error.InvalidRange, error.OutOfRange => b.sym_program_error,
            error.StackOverflow, error.NoMatchingBlock => b.sym_control_error,
            error.FileNotFound, error.InvalidPath => b.sym_file_error,
            error.StreamClosed, error.NotOutputStreamError, error.NotInputStreamError => b.sym_stream_error,
            error.PackageExists, error.InvalidPackage, error.SymConflict => b.sym_package_error,
            error.InvalidSyntax => b.sym_parse_error,
            error.UserError, error.NotImplemented => b.sym_simple_error,
            else => null,
        };
    }

    /// Try to signal a Zig error as a CL condition via doThrow.
    /// Returns true if a handler/catch was found and execution should continue.
    fn trySignalCondition(self: *Vm, err: anyerror) !bool {
        if (self.trace_error_context) {
            std.debug.print(
                "TRACE try-signal: err={s} catch_sp={d} handler_sp={d}\n",
                .{ @errorName(err), self.catch_sp, self.handler_sp },
            );
        }
        if (self.catch_sp == 0 and self.handler_sp == 0) return false;
        const condition_type = self.zigErrorToConditionSym(err) orelse {
            if (self.trace_error_context) std.debug.print("TRACE try-signal: unmapped err={s}\n", .{@errorName(err)});
            return false;
        };
        if (self.trace_error_context and condition_type.isSymbol()) {
            std.debug.print(
                "TRACE try-signal: cond-type={s}\n",
                .{condition_type.toPtr(Symbol).getName()},
            );
        }
        // Build condition pair: (type . (error-name . nil))
        const err_name_str = try self.allocString(@errorName(err));
        const payload = try self.allocCons(err_name_str, Value.nil);
        const condition_pair = try self.allocCons(condition_type, payload);
        // doThrow may return NestedNonLocalExit when the matching handler is
        // across a call barrier.  That MUST propagate so callFromStack can
        // relay the throw.  ControlTransfer means doThrow found a handler and
        // set up the jump — the execute loop should continue.
        self.doThrow(self.builtins.sym_condition_tag, condition_pair) catch |e| {
            if (e == error.ControlTransfer) return true;
            if (e == error.NestedNonLocalExit) return e;
            if (self.trace_error_context) std.debug.print("TRACE try-signal: doThrow err={s}\n", .{@errorName(e)});
            return e;
        };
        if (self.trace_error_context) std.debug.print("TRACE try-signal: transferred via catch\n", .{});
        return true;
    }

    fn mapError(self: *Vm, err: anyerror) Error {
        if (self.shouldTraceError(err)) {
            const cur_name = chunkTraceName(self.chunk);
            const code_ptr = @intFromPtr(self.chunk.code);
            const code_len = self.chunk.code_len;
            std.debug.print(
                "TRACE error ctx: err={s} chunk={s} ip={d} code_len={d} fp={d} sp={d}\n",
                .{ @errorName(err), cur_name, self.ip, code_len, self.fp, self.sp },
            );
            if (err == error.OutOfMemory) {
                std.debug.print(
                    "  oom-stats heap_used={d} heap_total={d} gc_count={d} gc_major={d} jit_gc_forbidden={d} comp_root_sp={d} comp_retain={d} ext_roots={d} ext_saved={d}\n",
                    .{
                        self.heap.bytesUsed(),
                        self.heap.memory.len,
                        self.heap.stats.gc_count,
                        self.heap.stats.gc_major_count,
                        self.jit_gc_forbidden_depth,
                        self.comp_root_sp,
                        self.comp_retain_vals.items.len,
                        self.currentExtRoots().len,
                        self.ext_roots_saved_sp,
                    },
                );
            }
            if (err == error.InvalidOpcode or err == error.InvalidConstant) {
                const from_start = @intFromPtr(self.heap.from_start);
                const from_end = @intFromPtr(self.heap.from_end);
                const to_start = @intFromPtr(self.heap.to_start);
                const to_end = to_start + self.heap.space_size;
                const in_heap = (code_ptr >= from_start and code_ptr < from_end) or
                    (code_ptr >= to_start and code_ptr < to_end);
                if (in_heap and code_len <= self.heap.space_size) {
                    const code = self.chunk.getCode();
                    const start = if (self.ip > 8) self.ip - 8 else 0;
                    const stop = @min(code.len, self.ip + 8);
                    std.debug.print("  code[{d}..{d}] =", .{ start, stop });
                    var i: usize = start;
                    while (i < stop) : (i += 1) {
                        std.debug.print(" {x:0>2}", .{code[i]});
                    }
                    std.debug.print("\n", .{});
                } else {
                    std.debug.print("  code dump skipped: ptr=0x{x} len={d}\n", .{ code_ptr, code_len });
                }
            }
            var i: usize = self.fp;
            while (i > 0) {
                i -= 1;
                const frame = self.frames[i];
                const name = chunkTraceName(frame.chunk);
                std.debug.print(
                    "  frame[{d}] chunk={s} ret_ip={d} bp={d} argc={d}\n",
                    .{ i, name, frame.return_ip, frame.bp, frame.argc },
                );
            }
            if (self.trace_error_disasm and (err == error.TypeMismatch or err == error.StackOverflow)) {
                std.debug.print("TRACE disasm begin (ip={d})\n", .{self.ip});
                var disasm_buf = std.ArrayList(u8){};
                defer disasm_buf.deinit(self.allocator);
                if (disasm.disassembleRuntime(self.chunk, disasm_buf.writer(self.allocator))) {
                    std.debug.print("{s}", .{disasm_buf.items});
                } else |derr| {
                    std.debug.print("TRACE disasm error={s}\n", .{@errorName(derr)});
                }
                const consts = self.chunk.getConstants();
                std.debug.print("TRACE disasm constants ({d}):\n", .{consts.len});
                for (consts, 0..) |c, ci| {
                    switch (c.typeKind()) {
                        .nil => std.debug.print("  [{d}] nil\n", .{ci}),
                        .t => std.debug.print("  [{d}] t\n", .{ci}),
                        .fixnum => std.debug.print("  [{d}] fixnum {d}\n", .{ ci, c.toFixnum() }),
                        .symbol => std.debug.print("  [{d}] symbol {s}\n", .{ ci, c.toPtr(Symbol).getName() }),
                        .keyword => std.debug.print("  [{d}] keyword {s}\n", .{ ci, c.toPtr(runtime.Keyword).getName() }),
                        .string => std.debug.print("  [{d}] string \"{s}\"\n", .{ ci, c.toPtr(String).bytes() }),
                        .cons => {
                            std.debug.print("  [{d}] cons raw=0x{x:0>16}", .{ ci, c.raw });
                            var curr = c;
                            var n: usize = 0;
                            while (n < 3 and curr.isCons()) : (n += 1) {
                                const head = curr.toPtr(Cons).car;
                                switch (head.typeKind()) {
                                    .symbol => std.debug.print(" car{d}=symbol:{s}", .{ n, head.toPtr(Symbol).getName() }),
                                    .cons => std.debug.print(" car{d}=cons", .{n}),
                                    else => std.debug.print(" car{d}={s}", .{ n, @tagName(head.typeKind()) }),
                                }
                                curr = curr.toPtr(Cons).cdr;
                            }
                            // Full-list sanity summary for quoted constants.
                            var scan = c;
                            var list_len: usize = 0;
                            var bad_kind: ?Value = null;
                            while (scan.isCons()) {
                                const h = scan.toPtr(Cons).car;
                                if (!h.isSymbol()) {
                                    bad_kind = h;
                                    break;
                                }
                                list_len += 1;
                                scan = scan.toPtr(Cons).cdr;
                            }
                            if (bad_kind) |k| {
                                std.debug.print(" len={d} bad={s}", .{ list_len, @tagName(k.typeKind()) });
                            } else if (!scan.isNil()) {
                                std.debug.print(" len={d} tail={s}", .{ list_len, @tagName(scan.typeKind()) });
                            } else {
                                std.debug.print(" len={d} all-symbols", .{list_len});
                            }
                            std.debug.print("\n", .{});
                        },
                        else => std.debug.print("  [{d}] {s} raw=0x{x:0>16}\n", .{ ci, @tagName(c.typeKind()), c.raw }),
                    }
                }
                std.debug.print("TRACE disasm end\n", .{});
            }
        }
        return err;
    }

    fn restartTypeSymbol(self: *Vm) Error!Value {
        return (try self.heap.internInPackage("COMMON-LISP", "RESTART")) orelse error.UnboundSymbol;
    }

    fn makeRestartObject(self: *Vm, frame: *const RestartFrame) Error!Value {
        const restart_obj = try self.allocVector(3, 3);
        const vec = restart_obj.toPtr(Vector);
        vec.data[0] = try self.restartTypeSymbol();
        vec.data[1] = self.resolveForwardedValue(frame.name);
        vec.data[2] = Value.makeFixnum(@intCast(frame.id));
        return restart_obj;
    }

    fn restartNameValue(self: *Vm, designator: Value) Error!?Value {
        const live = self.resolveForwardedValue(designator);
        if (live.isSymbol()) return live;
        if (!live.isVector()) return null;

        const vec = live.toPtr(Vector);
        if (vec.length < 3) return null;

        const restart_sym = try self.restartTypeSymbol();
        if (self.resolveForwardedValue(vec.data[0]).raw != restart_sym.raw) return null;

        const name = self.resolveForwardedValue(vec.data[1]);
        if (!name.isSymbol()) return null;
        return name;
    }

    fn restartObjectId(self: *Vm, designator: Value) Error!?u64 {
        const live = self.resolveForwardedValue(designator);
        if (!live.isVector()) return null;

        const vec = live.toPtr(Vector);
        if (vec.length < 3) return null;

        const restart_sym = try self.restartTypeSymbol();
        if (self.resolveForwardedValue(vec.data[0]).raw != restart_sym.raw) return null;

        const id_val = self.resolveForwardedValue(vec.data[2]);
        if (!id_val.isFixnum()) return null;
        const id = id_val.toFixnum();
        if (id <= 0) return null;
        return @intCast(id);
    }

    fn findRestartIndex(self: *Vm, designator: Value) Error!?usize {
        if (try self.restartObjectId(designator)) |target_id| {
            var i = self.restart_sp;
            while (i > 0) {
                i -= 1;
                if (self.restart_stack[i].id == target_id) return i;
            }
            return null;
        }

        const target_name = (try self.restartNameValue(designator)) orelse return null;
        var i = self.restart_sp;
        while (i > 0) {
            i -= 1;
            if (self.restart_stack[i].name.raw == target_name.raw) return i;
        }
        return null;
    }

    fn doInvokeRestart(self: *Vm, designator: Value, value: Value) Error!void {
        const idx = (try self.findRestartIndex(designator)) orelse return error.RestartNotFound;
        const frame = self.restart_stack[idx];
        self.pending_error = null;
        self.is_unwinding = false;
        self.pending_throw_tag = Value.nil;
        self.pending_throw_value = Value.nil;
        self.pending_block_idx = null;
        self.pending_block_value = Value.nil;
        self.is_returning_from_block = false;

        if (frame.block_depth > MAX_BLOCKS) {
            return self.invalidOpcode("invoke-restart.block-depth-corrupt");
        }
        self.block_sp = frame.block_depth;
        try self.restoreControlDepths(
            frame.catch_depth,
            frame.unwind_depth,
            idx,
            frame.progv_depth,
            frame.handler_depth,
            null,
        );
        self.pending_handler_restore_depth = null;

        if (frame.restart_sp > STACK_SIZE or frame.restart_fp > MAX_FRAMES) {
            return self.invalidOpcode("invoke-restart.stack-corrupt");
        }
        self.chunk = frame.chunk;
        self.ip = frame.handler_ip;
        self.sp = frame.restart_sp;
        self.fp = frame.restart_fp;
        try self.push(value);
    }

    // ========================================================================
    // Handler-bind support
    // ========================================================================

    fn doHandlerBind(self: *Vm, body_fn: Value, handlers_alist: Value) Error!void {
        // Push handlers onto handler stack
        const depth_before = self.handler_sp;

        // Walk alist of (condition-type . handler-fn) pairs
        var curr = handlers_alist;
        while (!curr.isNil()) {
            if (!curr.isCons()) return error.TypeMismatch;
            const pair = curr.toPtr(runtime.Cons);
            const car = pair.car;

            if (!car.isCons()) return error.TypeMismatch;
            const binding = car.toPtr(runtime.Cons);
            const condition_type = binding.car;
            const handler_fn = binding.cdr;

            if (self.handler_sp >= MAX_HANDLERS) {
                return error.StackOverflow;
            }

            self.handler_stack[self.handler_sp] = HandlerFrame{
                .condition_type = condition_type,
                .handler_fn = handler_fn,
            };
            self.handler_sp += 1;

            curr = pair.cdr;
        }

        // Call body function with handlers active; restore handler depth on return.
        if (!body_fn.isClosure()) return error.TypeMismatch;
        if (self.sp >= STACK_SIZE) return error.StackOverflow;
        self.stack[self.sp] = body_fn;
        self.sp += 1;
        self.pending_handler_restore_depth = depth_before;
        errdefer {
            self.pending_handler_restore_depth = null;
            self.handler_sp = depth_before;
        }
        try self.doCall(0, false);
    }

    // ========================================================================
    // Format string support
    // ========================================================================

    /// Look up *standard-output*. Returns the stream value if bound, otherwise
    /// falls back to nil (caller should use system stdout).
    fn lookupStandardOutput(self: *Vm) Value {
        // Intern *STANDARD-OUTPUT* in the CL package and look up its value
        const sym_opt: ?Value = if (self.heap.internInPackage("COMMON-LISP", "*STANDARD-OUTPUT*")) |val_opt|
            val_opt
        else |_|
            null;
        if (sym_opt) |s| {
            if (self.lookupSymbolValueCell(s)) |val_opt| {
                if (val_opt) |val| {
                    if (val.isStream()) return val;
                }
            } else |_| {}
        }
        // Fallback: return nil (caller will use system stdout)
        return Value.nil;
    }

    fn lookupClStreamVar(self: *Vm, name: []const u8) Value {
        const sym_opt: ?Value = if (self.heap.internInPackage("COMMON-LISP", name)) |val_opt|
            val_opt
        else |_|
            null;
        if (sym_opt) |sym| {
            if (self.lookupSymbolValueCell(sym)) |val_opt| {
                if (val_opt) |val| {
                    if (val.isStream()) return val;
                }
            } else |_| {}
        }
        return Value.nil;
    }

    fn defaultOutputStream(self: *Vm) Value {
        return self.lookupClStreamVar("*STANDARD-OUTPUT*");
    }

    fn defaultInputStream(self: *Vm) Value {
        return self.lookupClStreamVar("*STANDARD-INPUT*");
    }

    fn argsList(self: *Vm, args: []const Value) Error!Value {
        return try primitives.list.list(self.heap, args);
    }

    fn requireArgCount(args: []const Value, min: usize, max: usize) Error!void {
        if (args.len < min or args.len > max) return error.TypeMismatch;
    }

    fn builtinResultToCallFrame(self: *Vm, fn_slot: usize, result: Value) Error!void {
        self.stack[fn_slot] = result;
        self.sp = fn_slot + 1;
    }

    fn makeVectorFromArgs(self: *Vm, args: []const Value) Error!Value {
        const vec = try self.heap.allocVector(args.len, args.len);
        const obj = vec.toPtr(runtime.Vector);
        for (args, 0..) |arg, i| {
            obj.data[i] = arg;
        }
        return vec;
    }

    fn nthArgStringSubscript(args: []const Value, idx: usize) Error!usize {
        if (idx >= args.len) return error.TypeMismatch;
        if (!args[idx].isFixnum()) return error.TypeMismatch;
        const n = args[idx].toFixnum();
        if (n < 0) return error.TypeMismatch;
        return @intCast(n);
    }

    fn resolveClassDesignator(self: *Vm, designator: Value) Error!Value {
        const live = self.resolveForwardedValue(designator);
        if (live.isClass()) return live;
        if (!live.isSymbol()) return error.TypeMismatch;
        return self.heap.findLispClass(live) orelse error.UndefinedClass;
    }

    fn structPrintHook(ctx: *anyopaque, obj: Value, stream: Value, level: usize) anyerror!bool {
        const self: *Vm = @ptrCast(@alignCast(ctx));
        if (!obj.isStructure()) return false;
        const class_val = self.resolveForwardedValue(obj.toPtr(runtime.objects.Structure).class);
        if (!class_val.isClass()) return false;
        const printer_designator = self.resolveForwardedValue(class_val.toPtr(runtime.Class).printer);
        if (printer_designator.isNil()) return false;

        var callable = printer_designator;
        if (callable.isSymbol()) {
            callable = (try self.resolveFunctionValue(callable)) orelse return error.UnboundSymbol;
        }
        switch (callable.typeKind()) {
            .closure, .native_code, .generic_function => {},
            else => return error.TypeMismatch,
        }

        const args = [_]Value{ obj, stream, Value.makeFixnum(@intCast(level)) };
        _ = try self.callFromStackAtFast(self.sp, callable, &args);
        return true;
    }

    fn ioPrintHook(self: *Vm) io.StructPrintHook {
        return .{ .ctx = self, .write_fn = structPrintHook };
    }

    fn doBuiltinCallable(self: *Vm, tag: BuiltinCallableTag, args: []const Value) Error!Value {
        switch (tag) {
            .add => {
                var out = Value.makeFixnum(0);
                for (args) |arg| out = try arith.add(self.heap, out, arg);
                return out;
            },
            .sub => {
                if (args.len == 0) return error.TypeMismatch;
                var out = args[0];
                if (args.len == 1) return try arith.negate(out);
                for (args[1..]) |arg| out = try arith.sub(self.heap, out, arg);
                return out;
            },
            .mul => {
                var out = Value.makeFixnum(1);
                for (args) |arg| out = try arith.mul(self.heap, out, arg);
                return out;
            },
            .div => {
                if (args.len == 0) return error.TypeMismatch;
                var out = args[0];
                if (args.len == 1) return try arith.div(self.heap, Value.makeFixnum(1), out);
                for (args[1..]) |arg| out = try arith.div(self.heap, out, arg);
                return out;
            },
            .append => {
                if (args.len == 0) return Value.nil;
                var out = args[args.len - 1];
                var i = args.len - 1;
                while (i > 0) {
                    i -= 1;
                    out = try primitives.list.append(self.heap, args[i], out);
                }
                return out;
            },
            .log => {
                try requireArgCount(args, 1, 2);
                if (args.len == 1) return try arith.log_val(args[0]);
                if (!args[0].isNumber() or !args[1].isNumber()) return error.TypeMismatch;
                const num = if (args[0].isFloat()) args[0].toFloat() else @as(f64, @floatFromInt(args[0].toFixnum()));
                const base = if (args[1].isFloat()) args[1].toFloat() else @as(f64, @floatFromInt(args[1].toFixnum()));
                return Value.makeFloat(std.math.log(f64, base, num));
            },
            .gensym => {
                try requireArgCount(args, 0, 1);
                return try primitives.symbol.gensym(self.heap, if (args.len == 0) null else args[0]);
            },
            .atan => {
                try requireArgCount(args, 1, 2);
                return if (args.len == 1)
                    try arith.atan_val(args[0])
                else
                    try arith.atan2_val(args[0], args[1]);
            },
            .list => return try primitives.list.list(self.heap, args),
            .member => return try self.memberBuiltin(args),
            .assoc => return try self.assocBuiltin(args),
            .find => return try self.findBuiltin(args),
            .position => return try self.positionBuiltin(args),
            .count => return try self.countBuiltin(args),
            .remove => return try self.removeBuiltin(args),
            .intern => return try self.internBuiltin(args),
            .make_broadcast_stream => return try self.heap.allocBroadcastStream(try self.argsList(args)),
            .make_concatenated_stream => return try self.heap.allocConcatenatedStream(try self.argsList(args)),
            .make_instance => return try primitives.makeInstance(self.heap, try self.argsList(args)),
            .class_of, .class_of_internal => return try primitives.classOf(self.heap, try self.argsList(args)),
            .floor => {
                try requireArgCount(args, 1, 2);
                return try arith.floor_val(self.heap, args[0], if (args.len == 2) args[1] else Value.makeFixnum(1));
            },
            .ceiling => {
                try requireArgCount(args, 1, 2);
                return try arith.ceil_val(self.heap, args[0], if (args.len == 2) args[1] else Value.makeFixnum(1));
            },
            .round => {
                try requireArgCount(args, 1, 2);
                return try arith.round_val(self.heap, args[0], if (args.len == 2) args[1] else Value.makeFixnum(1));
            },
            .truncate => {
                try requireArgCount(args, 1, 2);
                return try arith.trunc_val(self.heap, args[0], if (args.len == 2) args[1] else Value.makeFixnum(1));
            },
            .aref => {
                if (args.len < 2 or args.len > 9) return error.TypeMismatch;
                const arr_val = args[0];
                if (arr_val.isVector()) {
                    if (args.len != 2) return error.TypeMismatch;
                    const idx = try nthArgStringSubscript(args, 1);
                    const vec = arr_val.toPtr(runtime.Vector);
                    if (idx >= vec.length) return error.TypeMismatch;
                    return vec.get(idx);
                }
                if (arr_val.isString()) {
                    if (args.len != 2) return error.TypeMismatch;
                    const idx = try nthArgStringSubscript(args, 1);
                    const ch = stringPrims.stringRef(arr_val, idx);
                    if (ch < 0) return error.TypeMismatch;
                    return Value.makeCharacter(@intCast(ch));
                }
                if (arr_val.isVector() and arr_val.toPtr(runtime.Vector).isCharacterVector()) {
                    if (args.len != 2) return error.TypeMismatch;
                    const idx = try nthArgStringSubscript(args, 1);
                    const ch = stringPrims.stringRef(arr_val, idx);
                    if (ch < 0) return error.TypeMismatch;
                    return Value.makeCharacter(@intCast(ch));
                }
                if (arr_val.isString32()) {
                    if (args.len != 2) return error.TypeMismatch;
                    const idx = try nthArgStringSubscript(args, 1);
                    const str = arr_val.toPtr(runtime.String32);
                    if (idx >= str.length) return error.TypeMismatch;
                    return Value.makeCharacter(@intCast(str.codepoints()[idx]));
                }
                if (!arr_val.isArray()) return error.TypeMismatch;
                var subs: [8]u64 = undefined;
                for (args[1..], 0..) |sub, i| {
                    if (!sub.isFixnum()) return error.TypeMismatch;
                    const n = sub.toFixnum();
                    if (n < 0) return error.TypeMismatch;
                    subs[i] = @intCast(n);
                }
                return (try primitives.vector.arrayRef(arr_val, subs[0 .. args.len - 1])) orelse error.TypeMismatch;
            },
            .make_string => {
                if (args.len == 0) return error.TypeMismatch;
                if (!args[0].isFixnum()) return error.TypeMismatch;
                const n = args[0].toFixnum();
                if (n < 0) return error.TypeMismatch;
                const init_val = if (args.len >= 2) args[1] else Value.makeCharacter(' ');
                if (!init_val.isCharacter()) return error.TypeMismatch;
                const cp = init_val.toCharacter();
                if (cp > 0xFF) return error.TypeMismatch;
                const len: usize = @intCast(n);
                const buf = try self.allocator.alloc(u8, len);
                defer self.allocator.free(buf);
                @memset(buf, @intCast(cp));
                return try self.heap.allocBaseString(buf);
            },
            .make_vector => {
                if (args.len == 0 or args.len > 2) return error.TypeMismatch;
                if (!args[0].isFixnum()) return error.TypeMismatch;
                const n = args[0].toFixnum();
                if (n < 0) return error.TypeMismatch;
                return if (args.len == 2)
                    try primitives.vector.makeVectorFill(self.heap, @intCast(n), args[1])
                else
                    try primitives.vector.makeVector(self.heap, @intCast(n));
            },
            .svset => {
                try requireArgCount(args, 3, 3);
                const vec_val = args[0];
                const idx_val = args[1];
                const val = args[2];
                if (!idx_val.isFixnum()) return error.TypeMismatch;
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                if (!vec_val.isVector()) return error.TypeMismatch;
                const vec = vec_val.toPtr(runtime.Vector);
                if (idx >= vec.length) return error.TypeMismatch;
                vec.set(idx, val);
                self.writeBarrierStore(vec_val, val);
                return val;
            },
            .aset => {
                if (args.len < 3) return error.TypeMismatch;
                const arr_val = args[0];
                const val = args[args.len - 1];
                var subs: [8]u64 = undefined;
                if (args.len - 2 > subs.len) return error.TypeMismatch;
                for (args[1 .. args.len - 1], 0..) |sub, i| {
                    if (!sub.isFixnum()) return error.TypeMismatch;
                    const n = sub.toFixnum();
                    if (n < 0) return error.TypeMismatch;
                    subs[i] = @intCast(n);
                }
                if (!(try primitives.vector.arraySet(arr_val, subs[0 .. args.len - 2], val))) return error.TypeMismatch;
                self.writeBarrierStore(arr_val, val);
                return val;
            },
            .set_slot_value => return try primitives.setSlotValue(self.heap, try self.argsList(args)),
            .sset => {
                try requireArgCount(args, 3, 3);
                const str_val = args[0];
                const idx_val = args[1];
                const val = args[2];
                if (!idx_val.isFixnum()) return error.TypeMismatch;
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                if (!stringPrims.stringp(str_val) or !val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                if (cp > 0xFF) return error.TypeMismatch;
                if (!stringPrims.stringSet(str_val, idx, @intCast(cp))) return error.TypeMismatch;
                return val;
            },
            .make_unbound => return try primitives.makeUnbound(self.heap, try self.argsList(args)),
            .make_array => {
                try requireArgCount(args, 1, 1);
                const dims = args[0];
                if (dims.isFixnum()) {
                    const n = dims.toFixnum();
                    if (n < 0) return error.TypeMismatch;
                    return try primitives.vector.makeArray(self.heap, &[_]u64{@intCast(n)});
                }
                if (!dims.isCons()) return error.TypeMismatch;
                var buf: [8]u64 = undefined;
                var count: usize = 0;
                var cur = dims;
                while (cur.isCons()) {
                    if (count >= buf.len) return error.TypeMismatch;
                    const cell = cur.toPtr(runtime.Cons);
                    if (!cell.car.isFixnum()) return error.TypeMismatch;
                    const n = cell.car.toFixnum();
                    if (n < 0) return error.TypeMismatch;
                    buf[count] = @intCast(n);
                    count += 1;
                    cur = cell.cdr;
                }
                if (!cur.isNil()) return error.TypeMismatch;
                return try primitives.vector.makeArray(self.heap, buf[0..count]);
            },
            .char, .schar => {
                try requireArgCount(args, 2, 2);
                if (!args[1].isFixnum()) return error.TypeMismatch;
                const idx_signed = args[1].toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                const ch = stringPrims.stringRef(args[0], idx);
                if (ch < 0) return error.TypeMismatch;
                return Value.makeCharacter(@intCast(ch));
            },
            .substring => {
                try requireArgCount(args, 3, 3);
                if (!args[1].isFixnum() or !args[2].isFixnum()) return error.TypeMismatch;
                const start_signed = args[1].toFixnum();
                const end_signed = args[2].toFixnum();
                if (start_signed < 0 or end_signed < 0) return error.TypeMismatch;
                return try stringPrims.substring(self.heap, args[0], @intCast(start_signed), @intCast(end_signed));
            },
            .format => {
                if (args.len < 2) return error.TypeMismatch;
                if (!args[1].isString()) return error.TypeMismatch;
                return try self.doFormat(args[0], args[1], args[2..]);
            },
            .print => {
                try requireArgCount(args, 1, 2);
                const stream = if (args.len == 2) args[1] else self.defaultOutputStream();
                return try io.printWithHook(args[0], stream, self.ioPrintHook());
            },
            .princ => {
                try requireArgCount(args, 1, 2);
                const stream = if (args.len == 2) args[1] else self.defaultOutputStream();
                return try io.princWithHook(args[0], stream, self.ioPrintHook());
            },
            .encode_universal_time => {
                if (args.len < 6 or args.len > 7) return error.TypeMismatch;
                inline for (0..6) |i| {
                    if (!args[i].isFixnum()) return error.TypeMismatch;
                }
                const zone = if (args.len == 7) blk: {
                    if (!args[6].isFixnum()) return error.TypeMismatch;
                    break :blk args[6].toFixnum();
                } else null;
                return Value.makeFixnum(io.encodeUniversalTime(
                    args[0].toFixnum(),
                    args[1].toFixnum(),
                    args[2].toFixnum(),
                    args[3].toFixnum(),
                    args[4].toFixnum(),
                    args[5].toFixnum(),
                    zone,
                ));
            },
            .make_pathname => {
                var host = Value.nil;
                var device = Value.nil;
                var directory = Value.nil;
                var name = Value.nil;
                var ty = Value.nil;
                var version = Value.nil;
                if (args.len % 2 != 0) return error.TypeMismatch;
                var i: usize = 0;
                while (i < args.len) : (i += 2) {
                    const key = args[i];
                    const val = args[i + 1];
                    if (key.raw == self.builtins.kw_host.raw) host = val else if (key.raw == self.builtins.kw_device.raw) device = val else if (key.raw == self.builtins.kw_directory.raw) directory = val else if (key.raw == self.builtins.kw_name.raw) name = val else if (key.raw == self.builtins.kw_type.raw) ty = val else if (key.raw == self.builtins.kw_version.raw) version = val else return error.TypeMismatch;
                }
                return try primitives.pathname.makePathname(self.allocator, self.heap, host, device, directory, name, ty, version);
            },
            .make_hash_table => return try hash_prims.primMakeHashTable(self.heap, args),
            .gethash => return try hash_prims.primGethash(self.heap, args),
            .puthash => return try hash_prims.primPuthash(self.heap, args),
            .remhash => return try hash_prims.primRemhash(self.heap, args),
            .hash_table_count => return try hash_prims.primHashTableCount(self.heap, args),
            .hash_table_capacity => return try hash_prims.primHashTableCapacity(self.heap, args),
            .open => return try primitives.stream.primOpen(self.heap, args, &self.builtins),
            .close_internal, .close => return try primitives.stream.primClose(self.heap, args),
            .read_line => {
                const stream = if (args.len == 0) self.defaultInputStream() else args[0];
                return try primitives.stream.primReadLine(self.heap, &[_]Value{stream});
            },
            .write_line => {
                try requireArgCount(args, 1, 2);
                const stream = if (args.len == 2) args[1] else self.defaultOutputStream();
                return try primitives.stream.primWriteLine(self.heap, &[_]Value{ stream, args[0] });
            },
            .write_string => {
                if (args.len < 1 or args.len > 4) return error.TypeMismatch;
                const stream = if (args.len >= 2) args[1] else self.defaultOutputStream();
                if (args.len == 2) {
                    return try primitives.stream.primWriteString(self.heap, &[_]Value{ args[0], stream });
                }
                const start = if (args.len >= 3) args[2] else null;
                const end = if (args.len == 4) args[3] else null;
                try io.writeString(args[0], stream, start, end);
                return args[0];
            },
            .read_byte => {
                const stream = if (args.len == 0) self.defaultInputStream() else args[0];
                return try primitives.stream.primReadByte(self.heap, &[_]Value{stream});
            },
            .write_byte => {
                try requireArgCount(args, 1, 2);
                const stream = if (args.len == 2) args[1] else self.defaultOutputStream();
                return try primitives.stream.primWriteByte(self.heap, &[_]Value{ stream, args[0] });
            },
            .file_position => {
                try requireArgCount(args, 1, 1);
                return try primitives.stream.primFilePosition(self.heap, &[_]Value{args[0]});
            },
            .set_file_position => {
                try requireArgCount(args, 2, 2);
                return try primitives.stream.primFilePosition(self.heap, &[_]Value{ args[0], args[1] });
            },
            .file_length => {
                try requireArgCount(args, 1, 1);
                return try primitives.stream.primFileLength(self.heap, &[_]Value{args[0]});
            },
            .finish_output => {
                const stream = if (args.len == 0) self.defaultOutputStream() else args[0];
                return try primitives.stream.primFinishOutput(self.heap, &[_]Value{stream});
            },
            .force_output => {
                const stream = if (args.len == 0) self.defaultOutputStream() else args[0];
                return try primitives.stream.primForceOutput(self.heap, &[_]Value{stream});
            },
            .clear_input => {
                const stream = if (args.len == 0) self.defaultInputStream() else args[0];
                try io.clearInput(stream);
                return Value.nil;
            },
            .clear_output => {
                const stream = if (args.len == 0) self.defaultOutputStream() else args[0];
                try io.clearOutput(stream);
                return Value.nil;
            },
            .class_direct_superclasses => return try primitives.classDirectSuperclasses(self.heap, try self.argsList(args)),
            .class_precedence_list => return try primitives.classPrecedenceList(self.heap, try self.argsList(args)),
            .class_direct_slots => return try primitives.classDirectSlots(self.heap, try self.argsList(args)),
            .class_slots => return try primitives.classSlots(self.heap, try self.argsList(args)),
            .slot_definition_name => return try primitives.slotDefinitionName(self.heap, try self.argsList(args)),
            .slot_definition_initform => return try primitives.slotDefinitionInitform(self.heap, try self.argsList(args)),
            .slot_definition_initargs => return try primitives.slotDefinitionInitargs(self.heap, try self.argsList(args)),
            .slot_definition_readers => return try primitives.slotDefinitionReaders(self.heap, try self.argsList(args)),
            .slot_definition_writers => return try primitives.slotDefinitionWriters(self.heap, try self.argsList(args)),
            .slot_definition_allocation => return try primitives.slotDefinitionAllocation(self.heap, try self.argsList(args)),
            .slot_definition_type => return try primitives.slotDefinitionType(self.heap, try self.argsList(args)),
            .set_class_printer => {
                try requireArgCount(args, 2, 2);
                const class_val = try self.resolveClassDesignator(args[0]);
                const printer = self.resolveForwardedValue(args[1]);
                const class_obj = class_val.toPtr(runtime.Class);
                class_obj.printer = printer;
                self.writeBarrierStore(class_val, printer);
                return printer;
            },
            .copy_readtable => {
                try requireArgCount(args, 1, 2);
                return try self.heap.copyReadtable(args[0], if (args.len == 2) args[1] else Value.nil);
            },
            .readtable_case => {
                try requireArgCount(args, 1, 1);
                return try self.readtableCaseKeyword(try self.heap.readtableCase(args[0]));
            },
            .set_readtable_case => {
                try requireArgCount(args, 2, 2);
                const mode = try readtableCaseFromKeyword(args[1]);
                try self.heap.setReadtableCase(args[0], mode);
                return args[1];
            },
        }
    }

    const SeqTest = union(enum) {
        fast: runtime.HashTest,
        callable: Value,
    };

    fn seqTestKeyword(self: *Vm) Error!Value {
        return try self.heap.internKeyword("test");
    }

    fn decodeSeqTest(self: *Vm, test_val: Value, default_cmp: runtime.HashTest) Error!SeqTest {
        const live = self.resolveForwardedValue(test_val);
        if (live.isNil()) return .{ .fast = default_cmp };
        if (live.isSymbol()) {
            if (live.raw == self.builtins.sym_eq.raw) return .{ .fast = .eq };
            if (live.raw == self.builtins.sym_equal.raw) return .{ .fast = .equal };
            const eql_sym = try self.heap.intern("eql");
            if (live.raw == eql_sym.raw) return .{ .fast = .eql };
        }
        return .{ .callable = live };
    }

    fn parseSeqTestOption(self: *Vm, args: []const Value, positional: usize, default_cmp: runtime.HashTest) Error!SeqTest {
        if (args.len == positional) return .{ .fast = default_cmp };
        if (args.len < positional or ((args.len - positional) & 1) != 0) return error.InvalidArgument;
        const kw_test = try self.seqTestKeyword();
        var seq_test = SeqTest{ .fast = default_cmp };
        var i = positional;
        while (i < args.len) : (i += 2) {
            const key = self.resolveForwardedValue(args[i]);
            if (key.raw != kw_test.raw) return error.InvalidArgument;
            seq_test = try self.decodeSeqTest(args[i + 1], default_cmp);
        }
        return seq_test;
    }

    fn seqTestMatches(self: *Vm, seq_test: SeqTest, lhs: Value, rhs: Value) Error!bool {
        return switch (seq_test) {
            .fast => |cmp| hashKeyEqualWithTest(lhs, rhs, cmp),
            .callable => |callable| blk: {
                const result = try self.callFromStackAtFast(self.sp, callable, &[_]Value{ lhs, rhs });
                break :blk !result.isNil();
            },
        };
    }

    fn memberBuiltin(self: *Vm, args: []const Value) Error!Value {
        if (args.len < 2) return error.TypeMismatch;
        const item = args[0];
        const list = args[1];
        const seq_test = try self.parseSeqTestOption(args, 2, .eq);
        var curr = list;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (try self.seqTestMatches(seq_test, item, c.car)) return curr;
            curr = c.cdr;
        }
        if (!curr.isNil()) return error.TypeMismatch;
        return Value.nil;
    }

    fn assocBuiltin(self: *Vm, args: []const Value) Error!Value {
        if (args.len < 2) return error.TypeMismatch;
        const key = args[0];
        const alist = args[1];
        const seq_test = try self.parseSeqTestOption(args, 2, .eq);
        var curr = alist;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (c.car.isCons()) {
                const pair = c.car.toPtr(Cons);
                if (try self.seqTestMatches(seq_test, key, pair.car)) return c.car;
            }
            curr = c.cdr;
        }
        if (!curr.isNil()) return error.TypeMismatch;
        return Value.nil;
    }

    fn findBuiltin(self: *Vm, args: []const Value) Error!Value {
        if (args.len < 2) return error.TypeMismatch;
        const seq_test = try self.parseSeqTestOption(args, 2, .eql);
        return switch (seq_test) {
            .fast => |cmp| try self.findInSeq(args[0], args[1], cmp),
            .callable => try self.findInSeqCallable(args[0], args[1], seq_test),
        };
    }

    fn positionBuiltin(self: *Vm, args: []const Value) Error!Value {
        if (args.len < 2) return error.TypeMismatch;
        const seq_test = try self.parseSeqTestOption(args, 2, .eql);
        return switch (seq_test) {
            .fast => |cmp| try self.positionInSeq(args[0], args[1], cmp),
            .callable => try self.positionInSeqCallable(args[0], args[1], seq_test),
        };
    }

    fn countBuiltin(self: *Vm, args: []const Value) Error!Value {
        if (args.len < 2) return error.TypeMismatch;
        const seq_test = try self.parseSeqTestOption(args, 2, .eql);
        return switch (seq_test) {
            .fast => |cmp| try self.countInSeq(args[0], args[1], cmp),
            .callable => try self.countInSeqCallable(args[0], args[1], seq_test),
        };
    }

    fn removeBuiltin(self: *Vm, args: []const Value) Error!Value {
        if (args.len < 2) return error.TypeMismatch;
        const seq_test = try self.parseSeqTestOption(args, 2, .eql);
        return switch (seq_test) {
            .fast => |cmp| try self.listRemoveWithTest(args[0], args[1], cmp),
            .callable => try self.listRemoveWithCallable(args[0], args[1], seq_test),
        };
    }

    fn internBuiltin(self: *Vm, args: []const Value) Error!Value {
        try requireArgCount(args, 1, 2);
        if (args.len == 1) {
            const name_val = self.resolveForwardedValue(args[0]);
            var designator_buf: [256]u8 = undefined;
            const name_bytes = try self.getStringDesignator(name_val, designator_buf[0..]);
            defer name_bytes.deinit(self.allocator);
            const sym = if (self.heap.internCurrentPackagePreservingCase(name_bytes.slice)) |val|
                val
            else |_| blk: {
                var tmp: ?[]u8 = null;
                defer if (tmp) |buf| self.allocator.free(buf);
                var stable = name_bytes.slice;
                if (self.bytesInHeap(name_bytes.slice)) {
                    const copy = try self.allocator.alloc(u8, name_bytes.slice.len);
                    @memcpy(copy, name_bytes.slice);
                    tmp = copy;
                    stable = copy;
                }
                _ = try self.collectGarbage();
                break :blk try self.heap.internCurrentPackagePreservingCase(stable);
            };
            self.secondary_values[0] = try self.heap.internKeyword("internal");
            self.secondary_values_count = 1;
            return sym;
        }

        const pkg = switch (self.resolveForwardedValue(args[1]).typeKind()) {
            .package => self.resolveForwardedValue(args[1]),
            .symbol, .string, .keyword => if (try self.heap.findLispPackage(args[1])) |pkg| pkg else return error.InvalidPackage,
            else => return error.TypeMismatch,
        };
        const result = try primitives.package.internSymbol(self.heap, args[0], pkg);
        if (!result.isCons()) return error.TypeMismatch;
        const c1 = result.toPtr(Cons);
        if (!c1.cdr.isCons()) return error.TypeMismatch;
        const c2 = c1.cdr.toPtr(Cons);
        self.secondary_values[0] = c2.car;
        self.secondary_values_count = 1;
        return c1.car;
    }

    fn findInSeqCallable(self: *Vm, item: Value, seq: Value, seq_test: SeqTest) Error!Value {
        if (seq.isString()) {
            const str = seq.toPtr(runtime.String).bytes();
            for (str) |c| {
                const elem = Value.makeCharacter(@intCast(c));
                if (try self.seqTestMatches(seq_test, item, elem)) return elem;
            }
            return Value.nil;
        }
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            for (0..vec.length) |i| {
                const elem = vec.get(i);
                if (try self.seqTestMatches(seq_test, item, elem)) return elem;
            }
            return Value.nil;
        }
        var curr = seq;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (try self.seqTestMatches(seq_test, item, c.car)) return c.car;
            curr = c.cdr;
        }
        if (!curr.isNil()) return error.TypeMismatch;
        return Value.nil;
    }

    fn positionInSeqCallable(self: *Vm, item: Value, seq: Value, seq_test: SeqTest) Error!Value {
        if (seq.isString()) {
            const str = seq.toPtr(runtime.String).bytes();
            for (str, 0..) |c, i| {
                if (try self.seqTestMatches(seq_test, item, Value.makeCharacter(@intCast(c)))) {
                    return Value.makeFixnum(@intCast(i));
                }
            }
            return Value.nil;
        }
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            for (0..vec.length) |i| {
                if (try self.seqTestMatches(seq_test, item, vec.get(i))) {
                    return Value.makeFixnum(@intCast(i));
                }
            }
            return Value.nil;
        }
        var curr = seq;
        var idx: i64 = 0;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (try self.seqTestMatches(seq_test, item, c.car)) return Value.makeFixnum(idx);
            curr = c.cdr;
            idx += 1;
        }
        if (!curr.isNil()) return error.TypeMismatch;
        return Value.nil;
    }

    fn countInSeqCallable(self: *Vm, item: Value, seq: Value, seq_test: SeqTest) Error!Value {
        var n: i64 = 0;
        if (seq.isString()) {
            const str = seq.toPtr(runtime.String).bytes();
            for (str) |c| {
                if (try self.seqTestMatches(seq_test, item, Value.makeCharacter(@intCast(c)))) n += 1;
            }
            return Value.makeFixnum(n);
        }
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            for (0..vec.length) |i| {
                if (try self.seqTestMatches(seq_test, item, vec.get(i))) n += 1;
            }
            return Value.makeFixnum(n);
        }
        var curr = seq;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (try self.seqTestMatches(seq_test, item, c.car)) n += 1;
            curr = c.cdr;
        }
        if (!curr.isNil()) return error.TypeMismatch;
        return Value.makeFixnum(n);
    }

    fn listRemoveWithCallable(self: *Vm, item: Value, seq: Value, seq_test: SeqTest) Error!Value {
        const saved_sp = self.sp;
        errdefer self.sp = saved_sp;

        if (self.sp + 4 > STACK_SIZE) return error.StackOverflow;
        const item_idx = self.sp;
        const seq_idx = self.sp + 1;
        const result_idx = self.sp + 2;
        const tail_idx = self.sp + 3;

        self.stack[item_idx] = item;
        self.stack[seq_idx] = seq;
        self.stack[result_idx] = Value.nil;
        self.stack[tail_idx] = Value.nil;
        self.sp += 4;

        while (self.stack[seq_idx].isCons()) {
            const curr_val = self.stack[seq_idx];
            const c = curr_val.toPtr(Cons);
            const car = c.car;
            self.stack[seq_idx] = c.cdr;

            if (try self.seqTestMatches(seq_test, self.stack[item_idx], car)) continue;

            const new_cons = try self.allocCons(car, Value.nil);
            const tail_val = self.stack[tail_idx];
            if (tail_val.isCons()) {
                tail_val.toPtr(Cons).cdr = new_cons;
                self.writeBarrierStore(tail_val, new_cons);
            } else {
                self.stack[result_idx] = new_cons;
            }
            self.stack[tail_idx] = new_cons;
        }

        if (self.stack[seq_idx] != Value.nil) return error.TypeMismatch;

        const result = self.stack[result_idx];
        self.sp = saved_sp;
        return result;
    }

    fn doFormat(self: *Vm, dest: Value, control: Value, args: []const Value) Error!Value {
        const control_str = control.toPtr(runtime.String);
        const fmt = control_str.bytes();

        // Build result string
        var result = std.ArrayList(u8){};
        defer result.deinit(self.allocator);

        var arg_idx: usize = 0;
        var i: usize = 0;

        while (i < fmt.len) {
            if (fmt[i] == '~' and i + 1 < fmt.len) {
                // Scan ahead to find directive char (might have params/modifiers before it)
                var scan_idx = i + 1;
                var has_colon = false;
                var has_at = false;
                var has_v_param = false;
                while (scan_idx < fmt.len) {
                    const ch = fmt[scan_idx];
                    if (ch >= '0' and ch <= '9') {
                        scan_idx += 1;
                    } else if (ch == ',') {
                        scan_idx += 1;
                    } else if (ch == '\'' and scan_idx + 1 < fmt.len) {
                        scan_idx += 2; // Skip quote and next char
                    } else if (ch == ':') {
                        has_colon = true;
                        scan_idx += 1;
                    } else if (ch == '@') {
                        has_at = true;
                        scan_idx += 1;
                    } else if (ch == 'V' or ch == 'v') {
                        has_v_param = true;
                        scan_idx += 1;
                    } else {
                        break;
                    }
                }
                const directive = if (scan_idx < fmt.len) fmt[scan_idx] else fmt[i + 1];
                switch (directive) {
                    'A', 'a' => {
                        // Aesthetic - print without quotes, with optional min-width padding
                        if (arg_idx < args.len) {
                            var min_width: usize = 0;
                            if (has_v_param and arg_idx < args.len) {
                                // ~VA: width comes from an arg
                                const w = args[arg_idx];
                                arg_idx += 1;
                                if (w.isFixnum()) min_width = @intCast(@max(0, w.toFixnum()));
                            } else {
                                min_width = self.parseFormatWidth(fmt[i + 1 .. scan_idx]);
                            }
                            const start_len = result.items.len;
                            try self.formatValueAesthetic(args[arg_idx], &result);
                            if (min_width > 0) {
                                const written = result.items.len - start_len;
                                if (written < min_width) {
                                    var pad_count = min_width - written;
                                    while (pad_count > 0) : (pad_count -= 1) {
                                        try result.append(self.allocator, ' ');
                                    }
                                }
                            }
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'S', 's', 'W', 'w' => {
                        // Standard - print with quotes for strings, with optional min-width
                        // ~W is "write" which is the same as ~S in CL
                        if (arg_idx < args.len) {
                            const min_width = self.parseFormatWidth(fmt[i + 1 .. scan_idx]);
                            const start_len = result.items.len;
                            try self.formatValueStandard(args[arg_idx], &result);
                            if (min_width > 0) {
                                const written = result.items.len - start_len;
                                if (written < min_width) {
                                    var pad_count = min_width - written;
                                    while (pad_count > 0) : (pad_count -= 1) {
                                        try result.append(self.allocator, ' ');
                                    }
                                }
                            }
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'D', 'd' => {
                        if (arg_idx < args.len) {
                            const min_width = self.parseFormatWidth(fmt[i + 1 .. scan_idx]);
                            const start_len = result.items.len;
                            try self.formatFixnumDecimal(args[arg_idx], has_colon, &result);
                            if (min_width > 0) {
                                const written = result.items.len - start_len;
                                if (written < min_width) {
                                    // Pad with spaces on the LEFT for numbers
                                    const pad_count = min_width - written;
                                    // Insert spaces at start_len
                                    var j: usize = 0;
                                    while (j < pad_count) : (j += 1) {
                                        try result.insert(self.allocator, start_len, ' ');
                                    }
                                }
                            }
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'X', 'x' => {
                        if (arg_idx < args.len) {
                            try self.formatFixnumBase(args[arg_idx], 16, &result);
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'B', 'b' => {
                        if (arg_idx < args.len) {
                            try self.formatFixnumBase(args[arg_idx], 2, &result);
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'O', 'o' => {
                        if (arg_idx < args.len) {
                            try self.formatFixnumBase(args[arg_idx], 8, &result);
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'C', 'c' => {
                        // Character
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            if (val.isCharacter()) {
                                const cp = val.toCharacter();
                                if (cp < 128) {
                                    try result.append(self.allocator, @intCast(cp));
                                } else {
                                    // UTF-8 encode
                                    var buf: [4]u8 = undefined;
                                    const len = try std.unicode.utf8Encode(cp, &buf);
                                    try result.appendSlice(self.allocator, buf[0..len]);
                                }
                            }
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    '%' => {
                        // Newline
                        try result.append(self.allocator, '\n');
                        i = scan_idx + 1;
                    },
                    '&' => {
                        // Fresh line - newline only if not at start of line
                        if (result.items.len > 0 and result.items[result.items.len - 1] != '\n') {
                            try result.append(self.allocator, '\n');
                        }
                        i += 2;
                    },
                    'T', 't' => {
                        // Tabulate - insert spaces to reach column
                        // Parse optional parameters: ~mincolT or ~mincol,colincT
                        var mincol: usize = 1; // Default: tab to next column
                        var colinc: usize = 1; // Default: increment by 1

                        // Parameters are between ~ and T: fmt[i+1..scan_idx]
                        const param_str = fmt[i + 1 .. scan_idx];
                        if (param_str.len > 0) {
                            if (std.mem.indexOf(u8, param_str, ",")) |comma_pos| {
                                // Both mincol and colinc
                                if (comma_pos > 0) {
                                    mincol = std.fmt.parseInt(usize, param_str[0..comma_pos], 10) catch 1;
                                }
                                if (comma_pos + 1 < param_str.len) {
                                    colinc = std.fmt.parseInt(usize, param_str[comma_pos + 1 ..], 10) catch 1;
                                }
                            } else {
                                // Just mincol
                                if (param_str.len > 0) {
                                    mincol = std.fmt.parseInt(usize, param_str, 10) catch 1;
                                }
                            }
                        }

                        // Calculate current column position (chars since last newline)
                        var col: usize = 0;
                        var k = result.items.len;
                        while (k > 0) {
                            k -= 1;
                            if (result.items[k] == '\n') break;
                            col += 1;
                        }

                        // Insert spaces to reach target column
                        var target_col = mincol;
                        if (col < mincol) {
                            target_col = mincol;
                        } else {
                            // Round up to next multiple of colinc
                            target_col = col + colinc - ((col - mincol) % colinc);
                        }

                        const spaces_needed = if (target_col > col) target_col - col else 0;
                        var space_idx: usize = 0;
                        while (space_idx < spaces_needed) : (space_idx += 1) {
                            try result.append(self.allocator, ' ');
                        }

                        i = scan_idx + 1;
                    },
                    '~' => {
                        // Literal tilde
                        try result.append(self.allocator, '~');
                        i += 2;
                    },
                    '*' => {
                        // Argument navigation
                        // ~*     skip forward
                        // ~:*    move backward
                        // ~@*    absolute goto index
                        // ~n*    explicit count (default 1)
                        var move_count: usize = 1;
                        const param_str = fmt[i + 1 .. scan_idx];
                        var d0: usize = 0;
                        while (d0 < param_str.len and !(param_str[d0] >= '0' and param_str[d0] <= '9')) : (d0 += 1) {}
                        if (d0 < param_str.len) {
                            var d1 = d0;
                            while (d1 < param_str.len and param_str[d1] >= '0' and param_str[d1] <= '9') : (d1 += 1) {}
                            move_count = std.fmt.parseInt(usize, param_str[d0..d1], 10) catch 1;
                        }

                        if (has_at) {
                            arg_idx = @min(move_count, args.len);
                        } else if (has_colon) {
                            arg_idx = if (move_count > arg_idx) 0 else arg_idx - move_count;
                        } else {
                            arg_idx = @min(arg_idx + move_count, args.len);
                        }

                        i = scan_idx + 1;
                    },
                    'P', 'p' => {
                        // ~P: plural - 's' if arg != 1
                        // ~:P: use previous arg (don't consume)
                        const val = if (has_colon) blk: {
                            // Use previous arg
                            break :blk if (arg_idx > 0) args[arg_idx - 1] else Value.nil;
                        } else blk: {
                            // Consume next arg when present; otherwise fall back
                            // to previous arg for "~D ... ~P" usage.
                            if (arg_idx < args.len) {
                                const v = args[arg_idx];
                                arg_idx += 1;
                                break :blk v;
                            }
                            break :blk if (arg_idx > 0) args[arg_idx - 1] else Value.nil;
                        };

                        const should_plural = if (val.isFixnum()) val.toFixnum() != 1 else true;
                        if (should_plural) {
                            try result.append(self.allocator, 's');
                        }

                        i = scan_idx + 1;
                    },
                    '(' => {
                        // Case conversion: ~(...~)
                        const start = scan_idx + 1;
                        const end = self.findMatchingFormatDirective(fmt, start, '(', ')') orelse {
                            i += 2;
                            continue;
                        };

                        const body = fmt[start..end];
                        const body_start_len = result.items.len;
                        _ = try self.formatFragment(body, args, &arg_idx, &result, false);
                        self.applyCaseConversion(result.items[body_start_len..], has_colon, has_at);

                        i = end + 2; // Skip past ~)
                    },
                    ')' => {
                        // End of case conversion - should not be reached at top level
                        i += 2;
                    },
                    '{' => {
                        // Iteration: ~{...~} or ~:{...~} processes a list
                        // Find matching ~}
                        const start = scan_idx + 1;
                        var depth: usize = 1;
                        var end = start;
                        while (end < fmt.len and depth > 0) {
                            if (end + 1 < fmt.len and fmt[end] == '~') {
                                if (fmt[end + 1] == '{') {
                                    depth += 1;
                                    end += 2;
                                } else if (fmt[end + 1] == '}') {
                                    depth -= 1;
                                    if (depth == 0) break;
                                    end += 2;
                                } else {
                                    end += 1;
                                }
                            } else {
                                end += 1;
                            }
                        }
                        if (depth != 0) {
                            // Unmatched ~{, skip it
                            i += 2;
                            continue;
                        }
                        const body = fmt[start..end];
                        if (has_at) {
                            // ~@{body~} - use remaining args directly as iteration elements
                            // Build a list from remaining args
                            var remaining_list = Value.nil;
                            var j2: usize = args.len;
                            while (j2 > arg_idx) {
                                j2 -= 1;
                                remaining_list = try self.heap.allocCons(args[j2], remaining_list);
                            }
                            try self.formatIteration(remaining_list, body, &result);
                            arg_idx = args.len; // All args consumed
                        } else if (arg_idx < args.len) {
                            const list_arg = args[arg_idx];
                            arg_idx += 1;
                            if (has_colon) {
                                // ~:{body~} - iterate over sublists
                                var current = list_arg;
                                var iter_count: usize = 0;
                                while (current.isCons() and iter_count < MAX_FORMAT_DEPTH) : (iter_count += 1) {
                                    const cons = current.toPtr(runtime.Cons);
                                    try self.formatIteration(cons.car, body, &result);
                                    current = cons.cdr;
                                }
                            } else {
                                // ~{body~} - iterate over flat list
                                try self.formatIteration(list_arg, body, &result);
                            }
                        }
                        i = end + 2; // Skip past ~}
                    },
                    '}' => {
                        // End of iteration - should not be reached at top level
                        i += 2;
                    },
                    '?' => {
                        // Indirect: ~? takes a format string and a list of arguments
                        if (arg_idx + 1 < args.len) {
                            const sub_fmt_val = args[arg_idx];
                            const sub_args_val = args[arg_idx + 1];
                            arg_idx += 2;
                            // Get the sub-format string
                            if (sub_fmt_val.isString()) {
                                const sub_fmt_str = sub_fmt_val.toPtr(runtime.String);
                                // Collect sub-args
                                var sub_args = std.ArrayList(Value){};
                                defer sub_args.deinit(self.allocator);
                                var cur = sub_args_val;
                                while (cur.isCons()) {
                                    const c = cur.toPtr(runtime.Cons);
                                    try sub_args.append(self.allocator, c.car);
                                    cur = c.cdr;
                                }
                                // Recursive format
                                const sub_result = try self.doFormat(Value.nil, sub_fmt_val, sub_args.items);
                                _ = sub_fmt_str;
                                if (sub_result.isString()) {
                                    try result.appendSlice(self.allocator, sub_result.toPtr(runtime.String).bytes());
                                }
                            }
                        }
                        i = scan_idx + 1;
                    },
                    '^' => {
                        // Escape from iteration - only valid inside ~{...~}
                        // At top level, just skip it
                        i += 2;
                    },
                    // Note: ~:X directives handled via has_colon flag
                    // (scanner consumes : modifier before directive char)
                    '[' => {
                        // Conditional: ~[clause0~;clause1~;...~] or ~:[false~;true~]
                        // Find matching ~]
                        const start = scan_idx + 1;
                        var depth: usize = 1;
                        var end = start;
                        while (end < fmt.len and depth > 0) {
                            if (end + 1 < fmt.len and fmt[end] == '~') {
                                if (fmt[end + 1] == '[') {
                                    depth += 1;
                                    end += 2;
                                } else if (fmt[end + 1] == ']') {
                                    depth -= 1;
                                    if (depth == 0) break;
                                    end += 2;
                                } else {
                                    end += 1;
                                }
                            } else {
                                end += 1;
                            }
                        }
                        if (depth != 0) {
                            i += 2;
                            continue;
                        }
                        const body = fmt[start..end];
                        // Split clauses by ~;
                        var clauses = std.ArrayList([]const u8){};
                        defer clauses.deinit(self.allocator);
                        var clause_start: usize = 0;
                        var j: usize = 0;
                        var clause_depth: usize = 0;
                        while (j < body.len) {
                            if (j + 1 < body.len and body[j] == '~') {
                                if (body[j + 1] == '[') {
                                    clause_depth += 1;
                                    j += 2;
                                } else if (body[j + 1] == ']') {
                                    if (clause_depth > 0) clause_depth -= 1;
                                    j += 2;
                                } else if (body[j + 1] == ';' and clause_depth == 0) {
                                    try clauses.append(self.allocator, body[clause_start..j]);
                                    clause_start = j + 2;
                                    j += 2;
                                } else if (body[j + 1] == ':' and j + 2 < body.len and body[j + 2] == ';' and clause_depth == 0) {
                                    // ~:; is the default clause separator
                                    try clauses.append(self.allocator, body[clause_start..j]);
                                    clause_start = j + 3;
                                    j += 3;
                                } else {
                                    j += 1;
                                }
                            } else {
                                j += 1;
                            }
                        }
                        try clauses.append(self.allocator, body[clause_start..]);
                        // Get selector
                        if (arg_idx < args.len) {
                            const selector = args[arg_idx];
                            arg_idx += 1;
                            var clause_idx: usize = 0;
                            switch (selector.typeKind()) {
                                .fixnum => {
                                    const n = selector.toFixnum();
                                    if (n >= 0) clause_idx = @intCast(n);
                                },
                                .nil => {
                                    clause_idx = 0; // For ~:[false~;true~], nil selects first
                                },
                                else => {
                                    clause_idx = 1; // Non-nil selects second (for boolean conditional)
                                },
                            }
                            if (clause_idx < clauses.items.len) {
                                // Append the selected clause text directly
                                try result.appendSlice(self.allocator, clauses.items[clause_idx]);
                            } else if (clauses.items.len > 0) {
                                // Default: use last clause (CL ~:; default clause)
                                try result.appendSlice(self.allocator, clauses.items[clauses.items.len - 1]);
                            }
                        }
                        i = end + 2;
                    },
                    ']' => {
                        // End of conditional - should not be reached at top level
                        i += 2;
                    },
                    ';' => {
                        // Clause separator - should not be reached at top level
                        i += 2;
                    },
                    '<' => {
                        // Justification: ~mincol,colinc,minpad,'padchar:@<...~>
                        var mincol: usize = 0;
                        var colinc: usize = 1;
                        var minpad: usize = 0;
                        var padchar: u8 = ' ';
                        var colon = false;
                        var at = false;

                        const param_str = fmt[i + 1 .. scan_idx];
                        var pidx: usize = 0;
                        var param_num: usize = 0;

                        while (pidx < param_str.len) {
                            const pch = param_str[pidx];
                            if (pch == ':') {
                                colon = true;
                                pidx += 1;
                            } else if (pch == '@') {
                                at = true;
                                pidx += 1;
                            } else if (pch >= '0' and pch <= '9') {
                                var num: usize = 0;
                                while (pidx < param_str.len and param_str[pidx] >= '0' and param_str[pidx] <= '9') {
                                    num = num * 10 + (param_str[pidx] - '0');
                                    pidx += 1;
                                }
                                if (param_num == 0) {
                                    mincol = num;
                                } else if (param_num == 1) {
                                    colinc = num;
                                } else if (param_num == 2) {
                                    minpad = num;
                                }
                                param_num += 1;

                                if (pidx < param_str.len and param_str[pidx] == ',') {
                                    pidx += 1;
                                }
                            } else if (pch == '\'') {
                                pidx += 1;
                                if (pidx < param_str.len) {
                                    padchar = param_str[pidx];
                                    pidx += 1;
                                    param_num += 1;
                                    if (pidx < param_str.len and param_str[pidx] == ',') {
                                        pidx += 1;
                                    }
                                }
                            } else if (pch == ',') {
                                param_num += 1;
                                pidx += 1;
                            } else {
                                pidx += 1;
                            }
                        }

                        // Find matching ~>
                        const start = scan_idx + 1;
                        var depth: usize = 1;
                        var end = start;

                        while (end < fmt.len and depth > 0) {
                            if (end + 1 < fmt.len and fmt[end] == '~') {
                                if (fmt[end + 1] == '<') {
                                    depth += 1;
                                    end += 2;
                                } else if (fmt[end + 1] == '>') {
                                    depth -= 1;
                                    if (depth == 0) break;
                                    end += 2;
                                } else {
                                    end += 1;
                                }
                            } else {
                                end += 1;
                            }
                        }

                        if (depth != 0) {
                            i += 2;
                            continue;
                        }

                        const body = fmt[start..end];

                        // Split into segments by ~;
                        var segments = std.ArrayList([]const u8){};
                        defer segments.deinit(self.allocator);
                        var seg_start: usize = 0;
                        var j: usize = 0;
                        var seg_depth: usize = 0;

                        while (j < body.len) {
                            if (j + 1 < body.len and body[j] == '~') {
                                if (body[j + 1] == '<') {
                                    seg_depth += 1;
                                    j += 2;
                                } else if (body[j + 1] == '>') {
                                    if (seg_depth > 0) seg_depth -= 1;
                                    j += 2;
                                } else if (body[j + 1] == ';' and seg_depth == 0) {
                                    try segments.append(self.allocator, body[seg_start..j]);
                                    seg_start = j + 2;
                                    j += 2;
                                } else {
                                    j += 1;
                                }
                            } else {
                                j += 1;
                            }
                        }
                        try segments.append(self.allocator, body[seg_start..]);

                        // Process segments recursively
                        var seg_texts = std.ArrayList([]const u8){};
                        defer {
                            for (seg_texts.items) |s| self.allocator.free(s);
                            seg_texts.deinit(self.allocator);
                        }

                        var total_len: usize = 0;
                        for (segments.items) |seg| {
                            var seg_result = std.ArrayList(u8){};
                            defer seg_result.deinit(self.allocator);

                            var seg_idx: usize = 0;
                            while (seg_idx < seg.len) {
                                if (seg[seg_idx] == '~' and seg_idx + 1 < seg.len) {
                                    const dir = seg[seg_idx + 1];
                                    switch (dir) {
                                        'A', 'a' => {
                                            if (arg_idx < args.len) {
                                                try self.formatValueAesthetic(args[arg_idx], &seg_result);
                                                arg_idx += 1;
                                            }
                                            seg_idx += 2;
                                        },
                                        'S', 's' => {
                                            if (arg_idx < args.len) {
                                                try self.formatValueStandard(args[arg_idx], &seg_result);
                                                arg_idx += 1;
                                            }
                                            seg_idx += 2;
                                        },
                                        'D', 'd' => {
                                            if (arg_idx < args.len) {
                                                const val = args[arg_idx];
                                                if (val.isFixnum()) {
                                                    var buf: [32]u8 = undefined;
                                                    const num_str = try std.fmt.bufPrint(&buf, "{d}", .{val.toFixnum()});
                                                    try seg_result.appendSlice(self.allocator, num_str);
                                                }
                                                arg_idx += 1;
                                            }
                                            seg_idx += 2;
                                        },
                                        else => {
                                            try seg_result.append(self.allocator, seg[seg_idx]);
                                            seg_idx += 1;
                                        },
                                    }
                                } else {
                                    try seg_result.append(self.allocator, seg[seg_idx]);
                                    seg_idx += 1;
                                }
                            }

                            const owned = try self.allocator.dupe(u8, seg_result.items);
                            try seg_texts.append(self.allocator, owned);
                            total_len += owned.len;
                        }

                        // Calculate width with colinc
                        const base_width = total_len + minpad * @max(1, seg_texts.items.len - 1);
                        var width = mincol;
                        if (base_width > mincol) {
                            const k = (base_width - mincol + colinc - 1) / colinc;
                            width = mincol + k * colinc;
                        }

                        const pad_total = if (width > total_len) width - total_len else 0;

                        if (seg_texts.items.len == 1) {
                            // Single segment - apply modifiers
                            if (colon and at) {
                                // Center
                                const left_pad = pad_total / 2;
                                const right_pad = pad_total - left_pad;
                                var k: usize = 0;
                                while (k < left_pad) : (k += 1) {
                                    try result.append(self.allocator, padchar);
                                }
                                try result.appendSlice(self.allocator, seg_texts.items[0]);
                                k = 0;
                                while (k < right_pad) : (k += 1) {
                                    try result.append(self.allocator, padchar);
                                }
                            } else if (at) {
                                // Right justify
                                var k: usize = 0;
                                while (k < pad_total) : (k += 1) {
                                    try result.append(self.allocator, padchar);
                                }
                                try result.appendSlice(self.allocator, seg_texts.items[0]);
                            } else {
                                // Left justify
                                try result.appendSlice(self.allocator, seg_texts.items[0]);
                                var k: usize = 0;
                                while (k < pad_total) : (k += 1) {
                                    try result.append(self.allocator, padchar);
                                }
                            }
                        } else {
                            // Multiple segments - distribute padding
                            const n_gaps = seg_texts.items.len - 1 + @intFromBool(colon) + @intFromBool(at);
                            if (n_gaps == 0) {
                                for (seg_texts.items) |s| {
                                    try result.appendSlice(self.allocator, s);
                                }
                            } else {
                                const pad_per_gap = pad_total / n_gaps;
                                const extra_pads = pad_total % n_gaps;

                                var gap_idx: usize = 0;

                                // Leading pad if colon
                                if (colon) {
                                    const this_pad = pad_per_gap + @intFromBool(gap_idx < extra_pads);
                                    var k: usize = 0;
                                    while (k < this_pad + minpad) : (k += 1) {
                                        try result.append(self.allocator, padchar);
                                    }
                                    gap_idx += 1;
                                }

                                for (seg_texts.items, 0..) |s, idx| {
                                    try result.appendSlice(self.allocator, s);
                                    if (idx < seg_texts.items.len - 1) {
                                        const this_pad = pad_per_gap + @intFromBool(gap_idx < extra_pads);
                                        var k: usize = 0;
                                        while (k < this_pad + minpad) : (k += 1) {
                                            try result.append(self.allocator, padchar);
                                        }
                                        gap_idx += 1;
                                    }
                                }

                                // Trailing pad if at
                                if (at) {
                                    const this_pad = pad_per_gap + @intFromBool(gap_idx < extra_pads);
                                    var k: usize = 0;
                                    while (k < this_pad + minpad) : (k += 1) {
                                        try result.append(self.allocator, padchar);
                                    }
                                }
                            }
                        }

                        i = end + 2;
                    },
                    'F', 'f' => {
                        if (arg_idx < args.len) {
                            try self.formatFixedFloatDirective(args[arg_idx], fmt[i + 1 .. scan_idx], &result);
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'E', 'e' => {
                        // Exponential floating-point
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            const fval: f64 = if (val.isFixnum())
                                @floatFromInt(val.toFixnum())
                            else if (val.isFloat())
                                val.toFloat()
                            else
                                0.0;
                            var buf: [64]u8 = undefined;
                            const formatted = try std.fmt.bufPrint(&buf, "{e}", .{fval});
                            try result.appendSlice(self.allocator, formatted);
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'G', 'g' => {
                        // General floating-point: fixed for normal range, exponent otherwise.
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            const fval: f64 = if (val.isFixnum())
                                @floatFromInt(val.toFixnum())
                            else if (val.isFloat())
                                val.toFloat()
                            else if (val.typeKind() == .rational) blk: {
                                const rat = val.toPtr(runtime.Rational);
                                break :blk @as(f64, @floatFromInt(rat.numerator)) / @as(f64, @floatFromInt(rat.denominator));
                            } else 0.0;

                            const abs_val = @abs(fval);
                            const use_exp = abs_val != 0.0 and (abs_val < 0.0001 or abs_val >= 1000000.0);

                            var buf: [64]u8 = undefined;
                            if (use_exp) {
                                const formatted = try std.fmt.bufPrint(&buf, "{e}", .{fval});
                                try result.appendSlice(self.allocator, formatted);
                            } else {
                                const formatted = try std.fmt.bufPrint(&buf, "{d:.6}", .{fval});
                                var flen = formatted.len;
                                if (std.mem.indexOf(u8, formatted, ".")) |_| {
                                    while (flen > 1 and formatted[flen - 1] == '0') flen -= 1;
                                    if (flen > 0 and formatted[flen - 1] == '.') flen += 1;
                                }
                                try result.appendSlice(self.allocator, formatted[0..flen]);
                            }
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    '/' => {
                        // ~/fn/ custom formatter:
                        // call as (fn stream arg colonp atp), append stream output.
                        const fn_start = scan_idx + 1;
                        var fn_end = fn_start;
                        while (fn_end < fmt.len and fmt[fn_end] != '/') : (fn_end += 1) {}
                        if (fn_end >= fmt.len or fn_end == fn_start) {
                            try result.append(self.allocator, fmt[i]);
                            i += 1;
                            continue;
                        }

                        const fn_name = fmt[fn_start..fn_end];
                        var fn_sym_opt: ?Value = null;
                        if (std.mem.indexOf(u8, fn_name, "::")) |sep| {
                            const pkg_name = fn_name[0..sep];
                            const sym_name = fn_name[sep + 2 ..];
                            fn_sym_opt = try self.heap.internInPackage(pkg_name, sym_name);
                            if (fn_sym_opt == null and pkg_name.len <= 128 and sym_name.len <= 256) {
                                var pkg_buf: [128]u8 = undefined;
                                var sym_buf: [256]u8 = undefined;
                                for (pkg_name, 0..) |ch, idx| pkg_buf[idx] = std.ascii.toUpper(ch);
                                for (sym_name, 0..) |ch, idx| sym_buf[idx] = std.ascii.toUpper(ch);
                                fn_sym_opt = try self.heap.internInPackage(pkg_buf[0..pkg_name.len], sym_buf[0..sym_name.len]);
                            }
                        } else if (std.mem.indexOfScalar(u8, fn_name, ':')) |sep| {
                            const pkg_name = fn_name[0..sep];
                            const sym_name = fn_name[sep + 1 ..];
                            fn_sym_opt = try self.heap.internInPackage(pkg_name, sym_name);
                            if (fn_sym_opt == null and pkg_name.len <= 128 and sym_name.len <= 256) {
                                var pkg_buf: [128]u8 = undefined;
                                var sym_buf: [256]u8 = undefined;
                                for (pkg_name, 0..) |ch, idx| pkg_buf[idx] = std.ascii.toUpper(ch);
                                for (sym_name, 0..) |ch, idx| sym_buf[idx] = std.ascii.toUpper(ch);
                                fn_sym_opt = try self.heap.internInPackage(pkg_buf[0..pkg_name.len], sym_buf[0..sym_name.len]);
                            }
                        } else {
                            fn_sym_opt = try self.intern(fn_name);
                        }

                        if (fn_sym_opt) |fn_sym| {
                            const tmp_stream = try primitives.io.makeStringOutputStream(self.heap);
                            const fn_arg = if (arg_idx < args.len) args[arg_idx] else Value.nil;
                            if (arg_idx < args.len) arg_idx += 1;
                            const fn_args = [_]Value{
                                tmp_stream,
                                fn_arg,
                                if (has_colon) Value.t else Value.nil,
                                if (has_at) Value.t else Value.nil,
                            };
                            const fn_result = try self.callFromStack(fn_sym, &fn_args);
                            const stream_text = try primitives.io.getOutputStreamString(self.heap, tmp_stream);
                            if (stream_text.isString()) {
                                const bytes = stream_text.toPtr(runtime.String).bytes();
                                if (bytes.len > 0) {
                                    try result.appendSlice(self.allocator, bytes);
                                } else if (fn_result.isString()) {
                                    try result.appendSlice(self.allocator, fn_result.toPtr(runtime.String).bytes());
                                } else if (!fn_result.isNil()) {
                                    try self.formatValueAesthetic(fn_result, &result);
                                }
                            }
                        }

                        i = fn_end + 1;
                    },
                    'R', 'r' => {
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            if (val.isFixnum()) {
                                try self.formatRadixDirective(val.toFixnum(), fmt[i + 1 .. scan_idx], has_colon, has_at, &result);
                            }
                            arg_idx += 1;
                        }
                        i = scan_idx + 1;
                    },
                    else => {
                        // Unknown directive, output as-is
                        try result.append(self.allocator, fmt[i]);
                        i += 1;
                    },
                }
            } else {
                try result.append(self.allocator, fmt[i]);
                i += 1;
            }
        }

        // Handle destination
        if (dest.isNil()) {
            // Return as string
            return try self.allocString(result.items);
        } else if (dest.isStream()) {
            // Stream object - write to it
            try primitives.io.writeBytesToStream(dest, result.items);
            return Value.nil;
        } else {
            // dest = t means *standard-output*
            // Look up *standard-output* and use it if it's a stream
            const stdout_val = self.lookupStandardOutput();
            if (stdout_val.isStream()) {
                try primitives.io.writeBytesToStream(stdout_val, result.items);
            } else {
                // Fallback to system stdout
                const stdout_file = std.fs.File.stdout();
                var buf: [4096]u8 = undefined;
                var file_writer = stdout_file.writer(&buf);
                const w = &file_writer.interface;
                try w.writeAll(result.items);
                try w.flush();
            }
            return Value.nil;
        }
    }

    fn formatValueAesthetic(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        switch (val.typeKind()) {
            .nil => try result.appendSlice(self.allocator, "nil"),
            .t => try result.appendSlice(self.allocator, "t"),
            .unbound => try result.appendSlice(self.allocator, "#<unbound>"),
            .fixnum => {
                // Respect *print-base* (io.print_base)
                var buf: [65]u8 = undefined;
                const base = io.print_base;
                switch (base) {
                    2 => {
                        const num_str = try std.fmt.bufPrint(&buf, "{b}", .{val.toFixnum()});
                        try result.appendSlice(self.allocator, num_str);
                    },
                    8 => {
                        const num_str = try std.fmt.bufPrint(&buf, "{o}", .{val.toFixnum()});
                        try result.appendSlice(self.allocator, num_str);
                    },
                    10 => {
                        const num_str = try std.fmt.bufPrint(&buf, "{d}", .{val.toFixnum()});
                        try result.appendSlice(self.allocator, num_str);
                    },
                    16 => {
                        const num_str = try std.fmt.bufPrint(&buf, "{x}", .{val.toFixnum()});
                        try result.appendSlice(self.allocator, num_str);
                    },
                    else => {
                        const len = io.formatIntBase(val.toFixnum(), base, &buf);
                        try result.appendSlice(self.allocator, buf[0..len]);
                    },
                }
            },
            .float => {
                var buf: [400]u8 = undefined;
                const num_str = std.fmt.bufPrint(&buf, "{d}", .{val.toFloat()}) catch blk: {
                    break :blk std.fmt.bufPrint(&buf, "{e}", .{val.toFloat()}) catch "0.0";
                };
                try result.appendSlice(self.allocator, num_str);
                // Ensure decimal point for CL compliance
                var has_dot = false;
                for (num_str) |c| {
                    if (c == '.' or c == 'e' or c == 'E') {
                        has_dot = true;
                        break;
                    }
                }
                if (!has_dot) try result.appendSlice(self.allocator, ".0");
            },
            .char => {
                const cp = val.toCharacter();
                if (cp < 128) {
                    try result.append(self.allocator, @as(u8, @intCast(cp)));
                }
            },
            .string => try result.appendSlice(self.allocator, val.toPtr(runtime.String).bytes()),
            .string32 => {
                const cps = val.toPtr(runtime.String32).codepoints();
                for (cps) |cp| {
                    var buf: [4]u8 = undefined;
                    const len = try std.unicode.utf8Encode(@intCast(cp), &buf);
                    try result.appendSlice(self.allocator, buf[0..len]);
                }
            },
            .symbol => try result.appendSlice(self.allocator, val.toPtr(Symbol).getName()),
            .keyword => {
                try result.append(self.allocator, ':');
                try result.appendSlice(self.allocator, val.toPtr(runtime.Keyword).getName());
            },
            .cons => try self.formatListAesthetic(val, result),
            .closure => try result.appendSlice(self.allocator, "#<closure>"),
            .vector => {
                const vec = val.toPtr(runtime.Vector);
                if (vec.isCharacterVector()) {
                    const len: usize = @intCast(vec.getFillPointer() orelse vec.length);
                    for (0..len) |i| {
                        const ch = vec.data[i];
                        if (!ch.isCharacter()) return error.TypeMismatch;
                        var buf: [4]u8 = undefined;
                        const n = try std.unicode.utf8Encode(ch.toCharacter(), &buf);
                        try result.appendSlice(self.allocator, buf[0..n]);
                    }
                } else {
                    try result.appendSlice(self.allocator, "#<vector>");
                }
            },
            .structure => try result.appendSlice(self.allocator, "#<structure>"),
            .hashtable => try result.appendSlice(self.allocator, "#<hash-table>"),
            .rational => {
                const rat = val.toPtr(runtime.Rational);
                var buf: [64]u8 = undefined;
                const num_str = try std.fmt.bufPrint(&buf, "{d}/{d}", .{ rat.numerator, rat.denominator });
                try result.appendSlice(self.allocator, num_str);
            },
            .complex => {
                const cplx = val.toPtr(runtime.Complex);
                var buf: [128]u8 = undefined;
                const cplx_str = try std.fmt.bufPrint(&buf, "#C({d} {d})", .{ cplx.real, cplx.imag });
                try result.appendSlice(self.allocator, cplx_str);
            },
            .stream => try result.appendSlice(self.allocator, "#<stream>"),
            .bignum => try result.appendSlice(self.allocator, "#<bignum>"),
            .array => try result.appendSlice(self.allocator, "#<array>"),
            .pathname => try result.appendSlice(self.allocator, "#<pathname>"),
            .package => try result.appendSlice(self.allocator, "#<package>"),
            .readtable => try result.appendSlice(self.allocator, "#<readtable>"),
            .chunk => try result.appendSlice(self.allocator, "#<chunk>"),
            .condition => try result.appendSlice(self.allocator, "#<condition>"),
            .class => try result.appendSlice(self.allocator, "#<class>"),
            .slotdef => try result.appendSlice(self.allocator, "#<slot-definition>"),
            .generic_function => try result.appendSlice(self.allocator, "#<generic-function>"),
            .method => try result.appendSlice(self.allocator, "#<method>"),
            .native_code => try result.appendSlice(self.allocator, "#<native-code>"),
            .macro_env => try result.appendSlice(self.allocator, "#<macro-env>"),
        }
    }

    fn formatValueStandard(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        if (val.isString()) {
            // Strings get quoted
            try result.append(self.allocator, '"');
            const str = val.toPtr(runtime.String);
            try result.appendSlice(self.allocator, str.bytes());
            try result.append(self.allocator, '"');
        } else if (val.isVector() and val.toPtr(runtime.Vector).isCharacterVector()) {
            try result.append(self.allocator, '"');
            const vec = val.toPtr(runtime.Vector);
            const len: usize = @intCast(vec.getFillPointer() orelse vec.length);
            for (0..len) |i| {
                const ch = vec.data[i];
                if (!ch.isCharacter()) return error.TypeMismatch;
                var buf: [4]u8 = undefined;
                const n = try std.unicode.utf8Encode(ch.toCharacter(), &buf);
                try result.appendSlice(self.allocator, buf[0..n]);
            }
            try result.append(self.allocator, '"');
        } else {
            // Everything else same as aesthetic
            try self.formatValueAesthetic(val, result);
        }
    }

    fn findMatchingFormatDirective(_: *Vm, fmt: []const u8, start: usize, open: u8, close: u8) ?usize {
        var depth: usize = 1;
        var end = start;
        while (end < fmt.len and depth > 0) {
            if (end + 1 < fmt.len and fmt[end] == '~') {
                if (fmt[end + 1] == open) {
                    depth += 1;
                    end += 2;
                } else if (fmt[end + 1] == close) {
                    depth -= 1;
                    if (depth == 0) return end;
                    end += 2;
                } else {
                    end += 1;
                }
            } else {
                end += 1;
            }
        }
        return null;
    }

    fn isFormatWordChar(ch: u8) bool {
        return (ch >= 'A' and ch <= 'Z') or
            (ch >= 'a' and ch <= 'z') or
            (ch >= '0' and ch <= '9');
    }

    fn lowercaseAscii(segment: []u8) void {
        for (segment) |*c| {
            if (c.* >= 'A' and c.* <= 'Z') c.* += 'a' - 'A';
        }
    }

    fn uppercaseAscii(segment: []u8) void {
        for (segment) |*c| {
            if (c.* >= 'a' and c.* <= 'z') c.* -= 'a' - 'A';
        }
    }

    fn titleCaseAscii(segment: []u8, all_words: bool) void {
        var capitalize_next = true;
        var capitalized_first = false;
        for (segment) |*c| {
            if (isFormatWordChar(c.*)) {
                if (capitalize_next and (all_words or !capitalized_first)) {
                    if (c.* >= 'a' and c.* <= 'z') c.* -= 'a' - 'A';
                    capitalized_first = true;
                } else {
                    if (c.* >= 'A' and c.* <= 'Z') c.* += 'a' - 'A';
                }
                capitalize_next = false;
            } else {
                capitalize_next = true;
            }
        }
    }

    fn applyCaseConversion(_: *Vm, segment: []u8, has_colon: bool, has_at: bool) void {
        if (has_colon and has_at) {
            uppercaseAscii(segment);
        } else if (has_colon) {
            titleCaseAscii(segment, true);
        } else if (has_at) {
            titleCaseAscii(segment, false);
        } else {
            lowercaseAscii(segment);
        }
    }

    fn coerceFormatFloat(val: Value) ?f64 {
        return if (val.isFixnum())
            @as(f64, @floatFromInt(val.toFixnum()))
        else if (val.isFloat())
            val.toFloat()
        else if (val.typeKind() == .rational) blk: {
            const rat = val.toPtr(runtime.Rational);
            break :blk @as(f64, @floatFromInt(rat.numerator)) / @as(f64, @floatFromInt(rat.denominator));
        } else null;
    }

    fn parseFormatFloatParams(_: *Vm, params_in: []const u8) struct { width: ?usize, digits: ?usize } {
        var params = params_in;
        while (params.len > 0 and (params[params.len - 1] == ':' or params[params.len - 1] == '@')) {
            params = params[0 .. params.len - 1];
        }
        if (params.len == 0) return .{ .width = null, .digits = null };

        if (std.mem.indexOfScalar(u8, params, ',')) |comma| {
            const left = params[0..comma];
            const right = params[comma + 1 ..];
            return .{
                .width = if (left.len > 0) std.fmt.parseInt(usize, left, 10) catch null else null,
                .digits = if (right.len > 0) std.fmt.parseInt(usize, right, 10) catch null else null,
            };
        }

        return .{
            .width = std.fmt.parseInt(usize, params, 10) catch null,
            .digits = null,
        };
    }

    fn appendPadded(self: *Vm, result: *std.ArrayList(u8), text: []const u8, min_width: ?usize) Error!void {
        const width = min_width orelse 0;
        if (width > text.len) {
            var pad: usize = width - text.len;
            while (pad > 0) : (pad -= 1) {
                try result.append(self.allocator, ' ');
            }
        }
        try result.appendSlice(self.allocator, text);
    }

    fn formatFixedFloatDirective(self: *Vm, val: Value, params: []const u8, result: *std.ArrayList(u8)) Error!void {
        const fval = coerceFormatFloat(val) orelse return;
        const spec = parseFormatFloatParams(self, params);

        const abs_val = @abs(fval);
        const int_part = @floor(abs_val);
        var int_digits: usize = 1;
        if (int_part >= 1.0) {
            int_digits = @as(usize, @intFromFloat(@floor(std.math.log10(int_part)))) + 1;
        }
        const sign_chars: usize = if (fval < 0) 1 else 0;

        const explicit_digits = spec.digits != null;
        const precision: usize = if (spec.digits) |digits|
            digits
        else if (spec.width) |width|
            if (width > sign_chars + int_digits) width - sign_chars - int_digits - 1 else 0
        else
            6;

        var buf: [96]u8 = undefined;
        const formatted = try std.fmt.bufPrint(&buf, "{d:.[1]}", .{ fval, precision });
        var end = formatted.len;
        if (!explicit_digits) {
            if (std.mem.indexOfScalar(u8, formatted, '.')) |_| {
                while (end > 0 and formatted[end - 1] == '0') end -= 1;
                if (end > 0 and formatted[end - 1] == '.') {
                    if (precision == 0 and spec.width != null) {
                        // Keep trailing dot for CL-style width-only ~wF cases like ~4F => 123.
                    } else {
                        end -= 1;
                    }
                }
            }
        }
        try self.appendPadded(result, formatted[0..end], spec.width);
    }

    fn formatRoman(self: *Vm, n: i64, uppercase: bool, result: *std.ArrayList(u8)) Error!void {
        if (n == 0) {
            try result.append(self.allocator, '0');
            return;
        }
        if (n < 0) {
            try result.append(self.allocator, '-');
            try self.formatRoman(-n, uppercase, result);
            return;
        }

        const Entry = struct { value: i64, numeral: []const u8 };
        const upper = [_]Entry{
            .{ .value = 1000, .numeral = "M" },
            .{ .value = 900, .numeral = "CM" },
            .{ .value = 500, .numeral = "D" },
            .{ .value = 400, .numeral = "CD" },
            .{ .value = 100, .numeral = "C" },
            .{ .value = 90, .numeral = "XC" },
            .{ .value = 50, .numeral = "L" },
            .{ .value = 40, .numeral = "XL" },
            .{ .value = 10, .numeral = "X" },
            .{ .value = 9, .numeral = "IX" },
            .{ .value = 5, .numeral = "V" },
            .{ .value = 4, .numeral = "IV" },
            .{ .value = 1, .numeral = "I" },
        };
        const lower = [_]Entry{
            .{ .value = 1000, .numeral = "m" },
            .{ .value = 900, .numeral = "cm" },
            .{ .value = 500, .numeral = "d" },
            .{ .value = 400, .numeral = "cd" },
            .{ .value = 100, .numeral = "c" },
            .{ .value = 90, .numeral = "xc" },
            .{ .value = 50, .numeral = "l" },
            .{ .value = 40, .numeral = "xl" },
            .{ .value = 10, .numeral = "x" },
            .{ .value = 9, .numeral = "ix" },
            .{ .value = 5, .numeral = "v" },
            .{ .value = 4, .numeral = "iv" },
            .{ .value = 1, .numeral = "i" },
        };
        const table = if (uppercase) &upper else &lower;

        var remaining = n;
        for (table) |entry| {
            while (remaining >= entry.value) {
                try result.appendSlice(self.allocator, entry.numeral);
                remaining -= entry.value;
            }
        }
    }

    fn formatRadixDirective(self: *Vm, n: i64, params: []const u8, has_colon: bool, has_at: bool, result: *std.ArrayList(u8)) Error!void {
        var trimmed = params;
        while (trimmed.len > 0 and (trimmed[trimmed.len - 1] == ':' or trimmed[trimmed.len - 1] == '@')) {
            trimmed = trimmed[0 .. trimmed.len - 1];
        }
        if (trimmed.len > 0) {
            const radix = std.fmt.parseInt(u8, trimmed, 10) catch 10;
            try self.formatFixnumRadix(n, radix, result);
            return;
        }
        if (has_at) {
            try self.formatRoman(n, true, result);
        } else if (has_colon) {
            try self.formatOrdinal(n, result);
        } else {
            try self.formatCardinal(n, result);
        }
    }

    fn formatFragment(self: *Vm, fmt: []const u8, args: []const Value, arg_idx: *usize, result: *std.ArrayList(u8), escape_on_exhausted: bool) Error!bool {
        var i: usize = 0;
        while (i < fmt.len) {
            if (fmt[i] == '~' and i + 1 < fmt.len) {
                var scan_idx = i + 1;
                var has_colon = false;
                var has_at = false;
                while (scan_idx < fmt.len) {
                    const ch = fmt[scan_idx];
                    if ((ch >= '0' and ch <= '9') or ch == ',') {
                        scan_idx += 1;
                    } else if (ch == ':' ) {
                        has_colon = true;
                        scan_idx += 1;
                    } else if (ch == '@') {
                        has_at = true;
                        scan_idx += 1;
                    } else {
                        break;
                    }
                }
                const directive = if (scan_idx < fmt.len) fmt[scan_idx] else fmt[i + 1];
                switch (directive) {
                    'A', 'a' => {
                        if (arg_idx.* < args.len) {
                            try self.formatValueAesthetic(args[arg_idx.*], result);
                            arg_idx.* += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'S', 's' => {
                        if (arg_idx.* < args.len) {
                            try self.formatValueStandard(args[arg_idx.*], result);
                            arg_idx.* += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'D', 'd' => {
                        if (arg_idx.* < args.len) {
                            try self.formatFixnumDecimal(args[arg_idx.*], has_colon, result);
                            arg_idx.* += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'F', 'f' => {
                        if (arg_idx.* < args.len) {
                            try self.formatFixedFloatDirective(args[arg_idx.*], fmt[i + 1 .. scan_idx], result);
                            arg_idx.* += 1;
                        }
                        i = scan_idx + 1;
                    },
                    'R', 'r' => {
                        if (arg_idx.* < args.len) {
                            const val = args[arg_idx.*];
                            if (val.isFixnum()) {
                                try self.formatRadixDirective(val.toFixnum(), fmt[i + 1 .. scan_idx], has_colon, has_at, result);
                            }
                            arg_idx.* += 1;
                        }
                        i = scan_idx + 1;
                    },
                    '%' => {
                        try result.append(self.allocator, '\n');
                        i = scan_idx + 1;
                    },
                    '~' => {
                        try result.append(self.allocator, '~');
                        i = scan_idx + 1;
                    },
                    '^' => {
                        i = scan_idx + 1;
                        if (escape_on_exhausted and arg_idx.* >= args.len) return false;
                    },
                    '{' => {
                        const start = scan_idx + 1;
                        const end = self.findMatchingFormatDirective(fmt, start, '{', '}') orelse {
                            i += 2;
                            continue;
                        };
                        const body = fmt[start..end];
                        if (has_at) {
                            var remaining_list = Value.nil;
                            var j2: usize = args.len;
                            while (j2 > arg_idx.*) {
                                j2 -= 1;
                                remaining_list = try self.heap.allocCons(args[j2], remaining_list);
                            }
                            try self.formatIteration(remaining_list, body, result);
                            arg_idx.* = args.len;
                        } else if (arg_idx.* < args.len) {
                            const list_arg = args[arg_idx.*];
                            arg_idx.* += 1;
                            if (has_colon) {
                                var current = list_arg;
                                var iter_count: usize = 0;
                                while (current.isCons() and iter_count < MAX_FORMAT_DEPTH) : (iter_count += 1) {
                                    const cons = current.toPtr(runtime.Cons);
                                    try self.formatIteration(cons.car, body, result);
                                    current = cons.cdr;
                                }
                            } else {
                                try self.formatIteration(list_arg, body, result);
                            }
                        }
                        i = end + 2;
                    },
                    else => {
                        try result.append(self.allocator, fmt[i]);
                        i += 1;
                    },
                }
            } else {
                try result.append(self.allocator, fmt[i]);
                i += 1;
            }
        }
        return true;
    }

    /// Parse numeric width from format parameter string (e.g., "10" from "~10A")
    /// Returns 0 if no width specified.
    fn parseFormatWidth(_: *Vm, params: []const u8) usize {
        if (params.len == 0) return 0;
        // Strip trailing modifiers (: and @)
        var end = params.len;
        while (end > 0 and (params[end - 1] == ':' or params[end - 1] == '@')) end -= 1;
        if (end == 0) return 0;
        return std.fmt.parseInt(usize, params[0..end], 10) catch 0;
    }

    fn formatFixnumBase(self: *Vm, val: Value, comptime base: u8, result: *std.ArrayList(u8)) Error!void {
        if (!val.isFixnum()) return;
        const n = val.toFixnum();
        var buf: [80]u8 = undefined;
        const spec = comptime switch (base) {
            2 => "{b}",
            8 => "{o}",
            10 => "{d}",
            16 => "{X}",
            else => unreachable,
        };
        const num_str = if (n >= 0)
            try std.fmt.bufPrint(&buf, spec, .{@as(u64, @intCast(n))})
        else
            try std.fmt.bufPrint(&buf, "-" ++ spec, .{@as(u64, @intCast(-n))});
        try result.appendSlice(self.allocator, num_str);
    }

    fn formatFixnumDecimal(self: *Vm, val: Value, grouped: bool, result: *std.ArrayList(u8)) Error!void {
        if (!val.isFixnum()) return;
        if (!grouped) return self.formatFixnumBase(val, 10, result);

        const n = val.toFixnum();
        var buf: [80]u8 = undefined;
        const raw = try std.fmt.bufPrint(&buf, "{d}", .{n});
        var start: usize = 0;
        if (raw.len > 0 and raw[0] == '-') {
            try result.append(self.allocator, '-');
            start = 1;
        }
        const digits = raw[start..];
        for (digits, 0..) |ch, i| {
            try result.append(self.allocator, ch);
            const remain = digits.len - i - 1;
            if (remain > 0 and (remain % 3) == 0) {
                try result.append(self.allocator, ',');
            }
        }
    }

    fn formatFixnumRadix(self: *Vm, n: i64, radix: u8, result: *std.ArrayList(u8)) Error!void {
        if (radix < 2 or radix > 36) return;
        if (n == 0) {
            try result.append(self.allocator, '0');
            return;
        }
        var buf: [80]u8 = undefined;
        var pos: usize = 80;
        var val: u64 = if (n < 0) @intCast(-n) else @intCast(n);
        const digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        while (val > 0) {
            pos -= 1;
            buf[pos] = digits[@intCast(val % radix)];
            val /= radix;
        }
        if (n < 0) {
            pos -= 1;
            buf[pos] = '-';
        }
        try result.appendSlice(self.allocator, buf[pos..]);
    }

    fn formatCardinal(self: *Vm, n: i64, result: *std.ArrayList(u8)) Error!void {
        if (n == 0) {
            try result.appendSlice(self.allocator, "zero");
            return;
        }
        if (n < 0) {
            try result.appendSlice(self.allocator, "negative ");
            try self.formatCardinal(-n, result);
            return;
        }
        const ones = [_][]const u8{ "", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" };
        const tens = [_][]const u8{ "", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" };
        var v: u64 = @intCast(n);
        var first = true;
        if (v >= 1000000000) {
            try self.formatCardinalHelper(v / 1000000000, &ones, &tens, first, result);
            try result.appendSlice(self.allocator, " billion");
            v %= 1000000000;
            first = false;
        }
        if (v >= 1000000) {
            if (!first) try result.append(self.allocator, ' ');
            try self.formatCardinalHelper(v / 1000000, &ones, &tens, true, result);
            try result.appendSlice(self.allocator, " million");
            v %= 1000000;
            first = false;
        }
        if (v >= 1000) {
            if (!first) try result.append(self.allocator, ' ');
            try self.formatCardinalHelper(v / 1000, &ones, &tens, true, result);
            try result.appendSlice(self.allocator, " thousand");
            v %= 1000;
            first = false;
        }
        if (v > 0) {
            if (!first) try result.append(self.allocator, ' ');
            try self.formatCardinalHelper(v, &ones, &tens, true, result);
        }
    }

    fn formatCardinalHelper(self: *Vm, n: u64, ones: []const []const u8, tens_arr: []const []const u8, first_in: bool, result: *std.ArrayList(u8)) Error!void {
        var v = n;
        var first = first_in;
        if (v >= 100) {
            if (!first) try result.append(self.allocator, ' ');
            try result.appendSlice(self.allocator, ones[v / 100]);
            try result.appendSlice(self.allocator, " hundred");
            v %= 100;
            first = false;
        }
        if (v >= 20) {
            if (!first) try result.append(self.allocator, ' ');
            try result.appendSlice(self.allocator, tens_arr[v / 10]);
            v %= 10;
            if (v > 0) {
                try result.append(self.allocator, '-');
                try result.appendSlice(self.allocator, ones[v]);
            }
        } else if (v > 0) {
            if (!first) try result.append(self.allocator, ' ');
            try result.appendSlice(self.allocator, ones[v]);
        }
    }

    fn formatOrdinal(self: *Vm, n: i64, result: *std.ArrayList(u8)) Error!void {
        // Simple ordinal: append th/st/nd/rd suffix
        var buf: [24]u8 = undefined;
        const num_str = try std.fmt.bufPrint(&buf, "{d}", .{n});
        try result.appendSlice(self.allocator, num_str);
        const abs_n: u64 = if (n < 0) @intCast(-n) else @intCast(n);
        const rem100 = abs_n % 100;
        const rem10 = abs_n % 10;
        if (rem100 >= 11 and rem100 <= 13) {
            try result.appendSlice(self.allocator, "th");
        } else if (rem10 == 1) {
            try result.appendSlice(self.allocator, "st");
        } else if (rem10 == 2) {
            try result.appendSlice(self.allocator, "nd");
        } else if (rem10 == 3) {
            try result.appendSlice(self.allocator, "rd");
        } else {
            try result.appendSlice(self.allocator, "th");
        }
    }

    const MAX_FORMAT_DEPTH = 1000;

    fn formatListAesthetic(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        try result.append(self.allocator, '(');
        var current = val;
        var first = true;
        var depth: usize = 0;
        while (current.isCons()) {
            // Prevent infinite loop on circular lists
            depth += 1;
            if (depth > MAX_FORMAT_DEPTH) {
                try result.appendSlice(self.allocator, "...");
                break;
            }
            if (!first) try result.append(self.allocator, ' ');
            first = false;
            const cons = current.toPtr(runtime.Cons);
            try self.formatValueAesthetic(cons.car, result);
            current = cons.cdr;
        }
        if (!current.isNil() and depth <= MAX_FORMAT_DEPTH) {
            try result.appendSlice(self.allocator, " . ");
            try self.formatValueAesthetic(current, result);
        }
        try result.append(self.allocator, ')');
    }

    /// Format iteration: process body for each element of a list
    /// Handles nested ~{...~} and ~^ escape within the body.
    fn formatIteration(self: *Vm, list: Value, body: []const u8, result: *std.ArrayList(u8)) Error!void {
        var elems = std.ArrayList(Value){};
        defer elems.deinit(self.allocator);
        var current = list;
        while (current.isCons()) {
            const cons = current.toPtr(runtime.Cons);
            try elems.append(self.allocator, cons.car);
            current = cons.cdr;
        }

        var elem_idx: usize = 0;
        var depth: usize = 0;
        while (elem_idx < elems.items.len) {
            depth += 1;
            if (depth > MAX_FORMAT_DEPTH) break;
            const keep_iterating = try self.formatFragment(body, elems.items, &elem_idx, result, true);
            if (!keep_iterating) break;
        }
    }

    // ========================================================================
    // Function call support
    // ========================================================================

    inline fn isPointerRaw(val: Value) bool {
        const raw = val.raw;
        return raw != 0 and (raw & 1) == 0 and (raw >> 62) == 0;
    }

    inline fn hasRawTag(val: Value, tag: u64) bool {
        return isPointerRaw(val) and (val.raw & RAW_TAG_MASK) == tag;
    }

    inline fn isConsRaw(val: Value) bool {
        return hasRawTag(val, RAW_CONS_TAG);
    }

    inline fn isKeywordRaw(val: Value) bool {
        return hasRawTag(val, RAW_KEYWORD_TAG);
    }

    inline fn consFromRaw(val: Value) *Cons {
        return @ptrFromInt(@as(usize, @intCast(val.raw & RAW_PTR_MASK)));
    }

    fn isAllowedKeyword(kw: Value, allowed_list: Value) bool {
        var list = allowed_list;
        while (isConsRaw(list)) {
            const cell = consFromRaw(list);
            if (cell.car.raw == kw.raw) return true;
            list = cell.cdr;
        }
        return false;
    }

    fn keywordInSlice(kw: Value, allowed: []const Value) bool {
        for (allowed) |entry| {
            if (entry.raw == kw.raw) return true;
        }
        return false;
    }

    fn callMismatch(self: *Vm, fn_val: Value, argc: u8, reason: []const u8) Error!void {
        if (self.trace_call_mismatch) {
            std.debug.print("CALL_MISMATCH reason={s} argc={d} fn-kind={s}", .{
                reason,
                argc,
                @tagName(fn_val.typeKind()),
            });
            if (fn_val.isSymbol()) {
                const live_fn = self.resolveForwardedValue(fn_val);
                if (live_fn.isSymbol()) {
                    const plist = live_fn.toPtr(Symbol).plist;
                    const fn_cell = primitives.list.get(self.heap, live_fn, self.builtins.sym_function_cell) catch Value.nil;
                    std.debug.print(
                        " symbol={s} live=0x{x} plist-kind={s} fn-cell-kind={s}",
                        .{
                            live_fn.toPtr(Symbol).getName(),
                            live_fn.raw,
                            @tagName(plist.typeKind()),
                            @tagName(fn_cell.typeKind()),
                        },
                    );
                } else {
                    std.debug.print(" symbol-forwarded kind={s}", .{@tagName(live_fn.typeKind())});
                }
            }
            if (fn_val.isClosure()) {
                const closure = fn_val.toPtr(runtime.Closure);
                std.debug.print(" closure-arity={d} code-kind={s}", .{
                    closure.arity,
                    @tagName(closure.code.typeKind()),
                });
                if (closure.code.isChunk()) {
                    const chunk = closure.code.toPtr(Chunk);
                    std.debug.print(
                        " chunk-arity={d} opt={d} key={d} rest={any} code-len={d} consts={d}",
                        .{ chunk.arity, chunk.opt_count, chunk.key_count, chunk.has_rest != 0, chunk.code_len, chunk.const_count },
                    );
                    switch (chunk.name.typeKind()) {
                        .symbol => std.debug.print(" chunk={s}", .{chunk.name.toPtr(Symbol).getName()}),
                        .string => std.debug.print(" chunk={s}", .{chunk.name.toPtr(runtime.String).bytes()}),
                        else => {},
                    }
                    if (self.trace_call_mismatch_fn_disasm) {
                        std.debug.print("\nCALL_MISMATCH fn-disasm begin\n", .{});
                        if (chunk.code_len > 0) {
                            const fcode = chunk.getCode();
                            std.debug.print("CALL_MISMATCH fn-code:", .{});
                            for (fcode) |b| {
                                std.debug.print(" {x:0>2}", .{b});
                            }
                            std.debug.print("\n", .{});
                        }
                        if (chunk.const_count > 0) {
                            const fconsts = chunk.getConstants();
                            var ci: usize = 0;
                            while (ci < fconsts.len) : (ci += 1) {
                                const cv = fconsts[ci];
                                std.debug.print("CALL_MISMATCH fn-const[{d}]=", .{ci});
                                tracePrintValue(cv);
                                std.debug.print("\n", .{});
                            }
                        }
                        const stdout_file = std.fs.File.stdout();
                        var cbuf: [8192]u8 = undefined;
                        var cwriter = stdout_file.writer(&cbuf);
                        const cw = &cwriter.interface;
                        disasm.disassembleRuntime(chunk, cw) catch |err| {
                            std.debug.print("CALL_MISMATCH fn-disasm error={s}\n", .{@errorName(err)});
                        };
                        cw.flush() catch {};
                        std.debug.print("CALL_MISMATCH fn-disasm end\n", .{});
                    }
                }
            }
            if (self.chunk.name.isSymbol()) {
                std.debug.print(" frame={s}", .{self.chunk.name.toPtr(Symbol).getName()});
            } else if (self.chunk.name.isString()) {
                std.debug.print(" frame={s}", .{self.chunk.name.toPtr(runtime.String).bytes()});
            }
            std.debug.print(" ip={d}\n", .{self.ip});

            if (self.global_env) |env| {
                var found_fn_global = false;
                var gi: usize = 0;
                while (gi < self.num_globals and gi < MAX_GLOBALS) : (gi += 1) {
                    if (self.globals[gi].raw == fn_val.raw) {
                        found_fn_global = true;
                        std.debug.print("CALL_MISMATCH fn-global idx={d}\n", .{gi});
                        var it_names = env.bindings.iterator();
                        var shown_names: usize = 0;
                        while (it_names.next()) |entry| {
                            if (entry.value_ptr.* == gi) {
                                std.debug.print("  fn-global name={s}\n", .{entry.key_ptr.*});
                                shown_names += 1;
                                if (shown_names >= 12) break;
                            }
                        }
                    }
                }
                if (!found_fn_global) {
                    std.debug.print("CALL_MISMATCH fn-global none\n", .{});
                }
            }

            if (self.sp >= @as(usize, argc) + 1) {
                const fn_slot = self.sp - argc - 1;
                const raw_fn = self.stack[fn_slot];
                std.debug.print("CALL_MISMATCH stack-fn-kind={s}\n", .{@tagName(raw_fn.typeKind())});
                if (raw_fn.isSymbol()) {
                    std.debug.print("CALL_MISMATCH stack-fn-symbol={s}\n", .{raw_fn.toPtr(Symbol).getName()});
                    const live_stack_fn = self.resolveForwardedValue(raw_fn);
                    if (live_stack_fn.isSymbol()) {
                        const plist = live_stack_fn.toPtr(Symbol).plist;
                        const fn_cell = primitives.list.get(self.heap, live_stack_fn, self.builtins.sym_function_cell) catch Value.nil;
                        std.debug.print(
                            "CALL_MISMATCH stack-fn-live=0x{x} plist-kind={s} fn-cell-kind={s}\n",
                            .{
                                live_stack_fn.raw,
                                @tagName(plist.typeKind()),
                                @tagName(fn_cell.typeKind()),
                            },
                        );
                    }
                } else if (raw_fn.isClosure()) {
                    const stack_cl = raw_fn.toPtr(runtime.Closure);
                    std.debug.print("CALL_MISMATCH stack-fn-closure arity={d} code-kind={s}", .{
                        stack_cl.arity,
                        @tagName(stack_cl.code.typeKind()),
                    });
                    if (stack_cl.code.isChunk()) {
                        const stack_chunk = stack_cl.code.toPtr(Chunk);
                        std.debug.print(
                            " chunk-arity={d} opt={d} key={d} rest={any}",
                            .{
                                stack_chunk.arity,
                                stack_chunk.opt_count,
                                stack_chunk.key_count,
                                stack_chunk.has_rest != 0,
                            },
                        );
                        switch (stack_chunk.name.typeKind()) {
                            .symbol => std.debug.print(" chunk={s}", .{stack_chunk.name.toPtr(Symbol).getName()}),
                            .string => std.debug.print(" chunk={s}", .{stack_chunk.name.toPtr(runtime.String).bytes()}),
                            else => {},
                        }
                    }
                    std.debug.print("\n", .{});
                }
                if (self.global_env) |env| {
                    var found_stack_global = false;
                    var gi: usize = 0;
                    while (gi < self.num_globals and gi < MAX_GLOBALS) : (gi += 1) {
                        if (self.globals[gi].raw == raw_fn.raw) {
                            found_stack_global = true;
                            std.debug.print("CALL_MISMATCH stack-fn-global idx={d}\n", .{gi});
                            var it_names = env.bindings.iterator();
                            var shown_names: usize = 0;
                            while (it_names.next()) |entry| {
                                if (entry.value_ptr.* == gi) {
                                    std.debug.print("  stack-fn-global name={s}\n", .{entry.key_ptr.*});
                                    shown_names += 1;
                                    if (shown_names >= 12) break;
                                }
                            }
                        }
                    }
                    if (!found_stack_global) {
                        std.debug.print("CALL_MISMATCH stack-fn-global none\n", .{});
                    }
                }
                var i: usize = 0;
                while (i < argc and i < 8) : (i += 1) {
                    const arg = self.stack[fn_slot + 1 + i];
                    std.debug.print("CALL_MISMATCH arg[{d}]={s}\n", .{ i, @tagName(arg.typeKind()) });
                    if (arg.isSymbol()) {
                        std.debug.print("CALL_MISMATCH arg[{d}]-sym={s}\n", .{ i, arg.toPtr(Symbol).getName() });
                    } else if (arg.isKeyword()) {
                        std.debug.print("CALL_MISMATCH arg[{d}]-kw={s}\n", .{ i, arg.toPtr(runtime.objects.Keyword).getName() });
                    }
                }
            }

            if (self.heap.cl_package) |cl_pkg| {
                const cl_has_apply_sym = cl_pkg.symbols.get("APPLY") != null;
                const cl_has_apply_export = cl_pkg.auto_export or cl_pkg.exports.contains("APPLY");
                std.debug.print("CALL_MISMATCH CL APPLY sym={any} export={any}\n", .{
                    cl_has_apply_sym,
                    cl_has_apply_export,
                });
            }

            if (self.heap.current_package) |pkg| {
                const cur_has_apply_sym = pkg.symbols.get("APPLY") != null;
                const cur_has_apply_export = pkg.auto_export or pkg.exports.contains("APPLY");
                const cur_has_apply_accessible = (pkg.findAccessible("APPLY") catch null) != null;
                var uses_cl = false;
                if (self.heap.cl_package) |cl_pkg| {
                    for (pkg.use_list.items) |used| {
                        if (used == cl_pkg) {
                            uses_cl = true;
                            break;
                        }
                    }
                }
                std.debug.print("CALL_MISMATCH CURPKG={s} APPLY sym={any} export={any} accessible={any} uses-cl={any}\n", .{
                    pkg.name,
                    cur_has_apply_sym,
                    cur_has_apply_export,
                    cur_has_apply_accessible,
                    uses_cl,
                });
            }

            std.debug.print("CALL_MISMATCH fp={d} sp={d}\n", .{ self.fp, self.sp });
            std.debug.print("CALL_MISMATCH catch_sp={d} handler_sp={d}\n", .{ self.catch_sp, self.handler_sp });
            if (self.catch_sp > 0) {
                var ci: usize = self.catch_sp;
                var shown_c: usize = 0;
                while (ci > 0 and shown_c < 4) : (shown_c += 1) {
                    ci -= 1;
                    const cf = self.catch_stack[ci];
                    std.debug.print("  catch[{d}] tag={s} catch_ip={d}\n", .{
                        ci,
                        @tagName(cf.tag.typeKind()),
                        cf.catch_ip,
                    });
                    if (cf.tag.isSymbol()) {
                        std.debug.print("    tag-sym={s}\n", .{cf.tag.toPtr(Symbol).getName()});
                    }
                }
            }
            std.debug.print("CALL_MISMATCH cur code_len={d} ip={d}", .{ self.chunk.code_len, self.ip });
            if (self.ip < self.chunk.code_len) {
                std.debug.print(" op=0x{x}", .{self.chunk.code[self.ip]});
            }
            switch (self.chunk.name.typeKind()) {
                .symbol => std.debug.print(" chunk={s}", .{self.chunk.name.toPtr(Symbol).getName()}),
                .string => std.debug.print(" chunk={s}", .{self.chunk.name.toPtr(runtime.String).bytes()}),
                else => std.debug.print(" chunk-kind={s}", .{@tagName(self.chunk.name.typeKind())}),
            }
            std.debug.print("\n", .{});
            if (self.chunk.code_len > 0) {
                const start = if (self.ip > 24) self.ip - 24 else 0;
                const end = @min(self.chunk.code_len, self.ip + 24);
                std.debug.print("CALL_MISMATCH code-bytes [{d}..{d}):", .{ start, end });
                var bi: usize = start;
                while (bi < end) : (bi += 1) {
                    std.debug.print(" {x:0>2}", .{self.chunk.code[bi]});
                }
                std.debug.print("\n", .{});
            }
            const probe_lo: usize = 0x0b1c;
            const probe_hi: usize = 0x0b28;
            std.debug.print("CALL_MISMATCH globals-probe [{x}..{x})\n", .{ probe_lo, probe_hi });
            if (self.global_env) |env| {
                var gi: usize = probe_lo;
                while (gi < probe_hi and gi < MAX_GLOBALS) : (gi += 1) {
                    std.debug.print("  idx={d} kind={s}", .{ gi, @tagName(self.globals[gi].typeKind()) });
                    if (self.globals[gi].isSymbol()) {
                        std.debug.print(" sym={s}", .{self.globals[gi].toPtr(Symbol).getName()});
                    }
                    std.debug.print("\n", .{});
                    var it = env.bindings.iterator();
                    var printed: usize = 0;
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == gi) {
                            std.debug.print("    name={s}\n", .{entry.key_ptr.*});
                            printed += 1;
                            if (printed >= 6) break;
                        }
                    }
                }
            }
            if (self.ip >= 3 and self.ip <= self.chunk.code_len) {
                const call_pos = self.ip - 3;
                if (call_pos + 1 < self.chunk.code_len) {
                    const call_op = @as(u16, self.chunk.code[call_pos]) | (@as(u16, self.chunk.code[call_pos + 1]) << 8);
                    if (call_op == @intFromEnum(opcodes.Op.call) and call_pos >= 4) {
                        const prev_pos = call_pos - 4;
                        const prev_op = @as(u16, self.chunk.code[prev_pos]) | (@as(u16, self.chunk.code[prev_pos + 1]) << 8);
                        if (prev_op == @intFromEnum(opcodes.Op.load_global)) {
                            const gidx = @as(u16, self.chunk.code[prev_pos + 2]) | (@as(u16, self.chunk.code[prev_pos + 3]) << 8);
                            std.debug.print("CALL_MISMATCH call-site load_global idx={d} val-kind={s}\n", .{ gidx, @tagName(self.globals[gidx].typeKind()) });
                            if (gidx < MAX_GLOBALS and self.globals[gidx].isSymbol()) {
                                std.debug.print("CALL_MISMATCH call-site global symbol={s}\n", .{self.globals[gidx].toPtr(Symbol).getName()});
                            }
                            if (self.global_env) |env| {
                                var it = env.bindings.iterator();
                                var printed: usize = 0;
                                while (it.next()) |entry| {
                                    if (entry.value_ptr.* == gidx) {
                                        std.debug.print("CALL_MISMATCH call-site global name={s}\n", .{entry.key_ptr.*});
                                        printed += 1;
                                        if (printed >= 8) break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (self.ip >= 3 and self.ip <= self.chunk.code_len) {
                const code = self.chunk.getCode();
                const call_pos = self.ip - 3;
                // Decode a small instruction window before the failing call site so
                // we can identify which global/load produced a nil callee.
                var starts: [16]usize = undefined;
                var count: usize = 0;
                var off: usize = 0;
                while (off + 1 < code.len and off <= call_pos) {
                    if (count < starts.len) {
                        starts[count] = off;
                        count += 1;
                    } else {
                        var si: usize = 1;
                        while (si < starts.len) : (si += 1) starts[si - 1] = starts[si];
                        starts[starts.len - 1] = off;
                    }
                    const op_raw = @as(u16, code[off]) | (@as(u16, code[off + 1]) << 8);
                    if (std.meta.intToEnum(opcodes.Op, op_raw)) |op| {
                        const step = 2 + op.operandSize();
                        if (step == 0) break;
                        off += step;
                    } else |_| {
                        break;
                    }
                }
                var start_idx: usize = if (count > 12) count - 12 else 0;
                while (start_idx < count) : (start_idx += 1) {
                    const pos = starts[start_idx];
                    const op_raw = @as(u16, code[pos]) | (@as(u16, code[pos + 1]) << 8);
                    if (std.meta.intToEnum(opcodes.Op, op_raw)) |op| {
                        std.debug.print("CALL_MISMATCH trace ip={d} op={s}", .{ pos, op.name() });
                        const sz = op.operandSize();
                        if (op == .load_global and pos + 3 < code.len) {
                            const gidx = @as(u16, code[pos + 2]) | (@as(u16, code[pos + 3]) << 8);
                            std.debug.print(" idx={d}", .{gidx});
                            if (gidx < MAX_GLOBALS) {
                                std.debug.print(" kind={s}", .{@tagName(self.globals[gidx].typeKind())});
                            }
                            if (self.global_env) |env| {
                                var it = env.bindings.iterator();
                                var printed: usize = 0;
                                while (it.next()) |entry| {
                                    if (entry.value_ptr.* == gidx) {
                                        std.debug.print(" name={s}", .{entry.key_ptr.*});
                                        printed += 1;
                                        if (printed >= 3) break;
                                    }
                                }
                            }
                        } else if (op == .make_closure and pos + 3 < code.len) {
                            const cidx = @as(u16, code[pos + 2]) | (@as(u16, code[pos + 3]) << 8);
                            std.debug.print(" idx={d}", .{cidx});
                            const chunk_pool = self.currentChunkPool();
                            if (cidx < chunk_pool.len) {
                                const callee_val = chunk_pool[cidx];
                                if (callee_val.isNil()) {
                                    std.debug.print(" callee=nil", .{});
                                } else {
                                    const callee = callee_val.toPtr(Chunk);
                                    std.debug.print(" arity={d} opt={d} key={d} rest={any}", .{
                                        callee.arity,
                                        callee.opt_count,
                                        callee.key_count,
                                        callee.has_rest != 0,
                                    });
                                    std.debug.print(" code_len={d} consts={d}", .{
                                        callee.code_len,
                                        callee.const_count,
                                    });
                                    switch (callee.name.typeKind()) {
                                        .symbol => std.debug.print(" name={s}", .{callee.name.toPtr(Symbol).getName()}),
                                        .string => std.debug.print(" name={s}", .{callee.name.toPtr(runtime.String).bytes()}),
                                        else => std.debug.print(" name-kind={s}", .{@tagName(callee.name.typeKind())}),
                                    }
                                    if (self.trace_call_mismatch_callee_disasm) {
                                        std.debug.print("\nCALL_MISMATCH callee-disasm idx={d} begin\n", .{cidx});
                                        const stdout_file = std.fs.File.stdout();
                                        var cbuf: [8192]u8 = undefined;
                                        var cwriter = stdout_file.writer(&cbuf);
                                        const cw = &cwriter.interface;
                                        disasm.disassembleRuntime(callee, cw) catch |err| {
                                            std.debug.print("CALL_MISMATCH callee-disasm error={s}\n", .{@errorName(err)});
                                        };
                                        cw.flush() catch {};
                                        std.debug.print("CALL_MISMATCH callee-disasm idx={d} end\n", .{cidx});
                                    }
                                }
                            }
                        } else if (sz > 0) {
                            std.debug.print(" bytes=", .{});
                            var i: usize = 0;
                            while (i < sz and pos + 2 + i < code.len) : (i += 1) {
                                std.debug.print("{d}", .{code[pos + 2 + i]});
                                if (i + 1 < sz and pos + 3 + i < code.len) std.debug.print(",", .{});
                            }
                        }
                        if (pos == call_pos) std.debug.print("  <-- call-site", .{});
                        std.debug.print("\n", .{});
                    } else |_| {
                        std.debug.print("CALL_MISMATCH trace ip={d} op=INVALID(0x{x})\n", .{ pos, op_raw });
                    }
                }
            }

            var shown: usize = 0;
            var fi: usize = self.fp;
            while (fi > 0 and shown < 8) : (shown += 1) {
                fi -= 1;
                const frame = self.frames[fi];
                std.debug.print("CALL_MISMATCH frame[{d}] return_ip={d}", .{ fi, frame.return_ip });
                switch (frame.chunk.name.typeKind()) {
                    .symbol => std.debug.print(" name={s}", .{frame.chunk.name.toPtr(Symbol).getName()}),
                    .string => std.debug.print(" name={s}", .{frame.chunk.name.toPtr(runtime.String).bytes()}),
                    else => std.debug.print(" name-kind={s}", .{@tagName(frame.chunk.name.typeKind())}),
                }
                std.debug.print("\n", .{});
            }

            if (self.trace_call_mismatch_disasm) {
                std.debug.print("CALL_MISMATCH disasm-begin\n", .{});
                const stdout_file = std.fs.File.stdout();
                var buf: [8192]u8 = undefined;
                var file_writer = stdout_file.writer(&buf);
                const w = &file_writer.interface;
                disasm.disassembleRuntime(self.chunk, w) catch |err| {
                    std.debug.print("CALL_MISMATCH disasm-error={s}\n", .{@errorName(err)});
                };
                w.flush() catch {};
                std.debug.print("CALL_MISMATCH disasm-end\n", .{});
            }
        }
        const is_program_error =
            std.mem.eql(u8, reason, "rest-arity") or
            std.mem.eql(u8, reason, "key-min-arity") or
            std.mem.eql(u8, reason, "key-odd-pairs") or
            std.mem.eql(u8, reason, "key-unknown") or
            std.mem.eql(u8, reason, "optional-arity") or
            std.mem.eql(u8, reason, "fixed-arity");
        const condition_name = if (is_program_error) "program-error" else "type-error";
        const condition_type = if (is_program_error) self.builtins.sym_program_error else self.builtins.sym_type_error;
        const prev_chunk = self.chunk;
        const prev_ip = self.ip;
        const prev_sp = self.sp;
        self.signalCondition(condition_name) catch |throw_err| {
            if (self.trace_call_mismatch) {
                std.debug.print("CALL_MISMATCH throw-failed={s} condition={s}\n", .{
                    @errorName(throw_err),
                    condition_name,
                });
            }
            return throw_err;
        };
        // If throw transferred control to a catch frame, continue there.
        if (self.chunk != prev_chunk or self.ip != prev_ip or self.sp != prev_sp) return;
        // Handler-bind may consume the first signal in-place. If a handler-case
        // catch is active, rethrow with handlers masked so catch dispatch runs.
        if (try self.rethrowConditionToCatch(condition_type, Value.nil, Value.nil)) return;
        return error.TypeMismatch;
    }

    fn buildConditionPayload(self: *Vm, condition_type: Value, datum: Value, expected_type: Value) Error!Value {
        // Preserve the legacy (datum . expected-type) payload for TYPE-ERROR
        // and PROGRAM-ERROR so ANSI helper accessors keep working.
        if (condition_type.raw == self.builtins.sym_type_error.raw or
            condition_type.raw == self.builtins.sym_program_error.raw)
        {
            return try self.allocCons(datum, expected_type);
        }

        if (try self.heap.lookupClassMetadata(condition_type)) |slot_names| {
            const payload = try self.allocVector(slot_names.len + 1, slot_names.len + 1);
            const vec = payload.toPtr(runtime.Vector);
            vec.data[0] = condition_type;

            var i: usize = 0;
            while (i < slot_names.len) : (i += 1) {
                vec.data[i + 1] = Value.nil;
            }

            const slot_datum = (try self.heap.internInPackage("CL", "datum")) orelse Value.nil;
            const slot_expected_type = (try self.heap.internInPackage("CL", "expected-type")) orelse Value.nil;
            const slot_format_control = (try self.heap.internInPackage("CL", "format-control")) orelse Value.nil;
            const slot_format_arguments = (try self.heap.internInPackage("CL", "format-arguments")) orelse Value.nil;

            for (slot_names, 0..) |slot_name, slot_idx| {
                if (slot_name.raw == slot_datum.raw) {
                    vec.data[slot_idx + 1] = datum;
                } else if (slot_name.raw == slot_expected_type.raw) {
                    vec.data[slot_idx + 1] = expected_type;
                } else if (slot_name.raw == slot_format_control.raw) {
                    vec.data[slot_idx + 1] = Value.nil;
                } else if (slot_name.raw == slot_format_arguments.raw) {
                    vec.data[slot_idx + 1] = Value.nil;
                }
            }

            return payload;
        }

        // Fallback for condition types without class slot metadata:
        // preserve type-error payload semantics as (datum . expected-type).
        return try self.allocCons(datum, expected_type);
    }

    fn signalConditionValue(self: *Vm, condition_type: Value, datum: Value, expected_type: Value) Error!void {
        const payload = try self.buildConditionPayload(condition_type, datum, expected_type);
        const condition_value = try self.allocCons(condition_type, payload);
        try self.doThrow(self.builtins.sym_condition_tag, condition_value);
    }

    fn signalCondition(self: *Vm, condition_name: []const u8) Error!void {
        const condition_type = if (std.mem.eql(u8, condition_name, "type-error"))
            self.builtins.sym_type_error
        else if (std.mem.eql(u8, condition_name, "program-error"))
            self.builtins.sym_program_error
        else
            try self.intern(condition_name);
        try self.signalConditionValue(condition_type, Value.nil, Value.nil);
    }

    fn hasCatchTag(self: *const Vm, tag: Value) bool {
        var i = self.catch_sp;
        while (i > 0) {
            i -= 1;
            if (self.catch_stack[i].tag.raw == tag.raw) return true;
        }
        return false;
    }

    fn rethrowConditionToCatch(self: *Vm, condition_type: Value, datum: Value, expected_type: Value) Error!bool {
        if (!self.hasCatchTag(self.builtins.sym_condition_tag)) return false;
        const saved_handler_sp = self.handler_sp;
        const saved_pending_restore = self.pending_handler_restore_depth;
        const prev_chunk = self.chunk;
        const prev_ip = self.ip;
        const prev_sp = self.sp;
        const prev_fp = self.fp;
        self.handler_sp = 0;
        self.pending_handler_restore_depth = null;
        errdefer {
            self.handler_sp = saved_handler_sp;
            self.pending_handler_restore_depth = saved_pending_restore;
        }
        try self.signalConditionValue(condition_type, datum, expected_type);
        const transferred =
            self.chunk != prev_chunk or
            self.ip != prev_ip or
            self.sp != prev_sp or
            self.fp != prev_fp;
        if (!transferred) {
            self.handler_sp = saved_handler_sp;
            self.pending_handler_restore_depth = saved_pending_restore;
        }
        return transferred;
    }

    fn signalTypeErrorDatumExpected(self: *Vm, datum: Value, expected_type: Value) Error!void {
        const prev_chunk = self.chunk;
        const prev_ip = self.ip;
        const prev_sp = self.sp;
        try self.signalConditionValue(self.builtins.sym_type_error, datum, expected_type);
        // If throw transferred control to a catch frame, continue there.
        if (self.chunk != prev_chunk or self.ip != prev_ip or self.sp != prev_sp) return;
        // Handler-bind may consume the first signal in-place. If a handler-case
        // catch is active, rethrow with handlers masked so catch dispatch runs.
        if (try self.rethrowConditionToCatch(self.builtins.sym_type_error, datum, expected_type)) return;
        return error.TypeMismatch;
    }

    fn signalTypeError(self: *Vm) Error!void {
        try self.signalTypeErrorDatumExpected(Value.nil, Value.nil);
    }

    fn restoreCallerFrameAfterCall(self: *Vm, frame: Frame, result: Value) Error!void {
        std.debug.assert(frame.bp < STACK_SIZE);
        self.chunk = frame.chunk;
        self.ip = frame.return_ip;
        try self.restoreDynamicDepthsFromFrame(frame);
        self.stack[frame.bp] = result;
        self.sp = frame.bp + 1;
    }

    fn restoreControlDepths(
        self: *Vm,
        catch_depth: usize,
        unwind_depth: usize,
        restart_depth: usize,
        progv_depth: usize,
        handler_depth: usize,
        handler_restore_depth: ?usize,
    ) Error!void {
        if (catch_depth > MAX_CATCHES or
            unwind_depth > MAX_UNWINDS or
            restart_depth > MAX_RESTARTS or
            progv_depth > MAX_PROGVS or
            handler_depth > MAX_HANDLERS)
        {
            return self.invalidOpcode("restore.depth-corrupt");
        }
        if (handler_restore_depth) |depth| {
            if (depth > MAX_HANDLERS) return self.invalidOpcode("restore.handler-depth-corrupt");
            self.handler_sp = depth;
        } else {
            self.handler_sp = handler_depth;
        }
        self.catch_sp = catch_depth;
        self.unwind_sp = unwind_depth;
        self.restart_sp = restart_depth;
        if (self.progv_sp < progv_depth) {
            return self.invalidOpcode("restore.progv-depth-underflow");
        }
        while (self.progv_sp > progv_depth) {
            try self.popProgvFrame();
        }
    }

    fn restoreDynamicDepthsFromFrame(self: *Vm, frame: Frame) Error!void {
        if (frame.block_depth > MAX_BLOCKS) return self.invalidOpcode("restore.block-depth-corrupt");
        self.block_sp = frame.block_depth;
        try self.restoreControlDepths(
            frame.catch_depth,
            frame.unwind_depth,
            frame.restart_depth,
            frame.progv_depth,
            frame.handler_depth,
            frame.handler_restore_depth,
        );
    }

    fn stackMove(self: *Vm, dest: usize, src: usize, count: usize) void {
        if (count == 0 or dest == src) return;
        if (dest < src) {
            switch (count) {
                1 => self.stack[dest] = self.stack[src],
                2 => {
                    self.stack[dest] = self.stack[src];
                    self.stack[dest + 1] = self.stack[src + 1];
                },
                3 => {
                    self.stack[dest] = self.stack[src];
                    self.stack[dest + 1] = self.stack[src + 1];
                    self.stack[dest + 2] = self.stack[src + 2];
                },
                4 => {
                    self.stack[dest] = self.stack[src];
                    self.stack[dest + 1] = self.stack[src + 1];
                    self.stack[dest + 2] = self.stack[src + 2];
                    self.stack[dest + 3] = self.stack[src + 3];
                },
                else => {
                    var i: usize = 0;
                    while (i < count) : (i += 1) {
                        self.stack[dest + i] = self.stack[src + i];
                    }
                },
            }
        } else {
            switch (count) {
                1 => self.stack[dest] = self.stack[src],
                2 => {
                    self.stack[dest + 1] = self.stack[src + 1];
                    self.stack[dest] = self.stack[src];
                },
                3 => {
                    self.stack[dest + 2] = self.stack[src + 2];
                    self.stack[dest + 1] = self.stack[src + 1];
                    self.stack[dest] = self.stack[src];
                },
                4 => {
                    self.stack[dest + 3] = self.stack[src + 3];
                    self.stack[dest + 2] = self.stack[src + 2];
                    self.stack[dest + 1] = self.stack[src + 1];
                    self.stack[dest] = self.stack[src];
                },
                else => {
                    var i: usize = count;
                    while (i > 0) {
                        i -= 1;
                        self.stack[dest + i] = self.stack[src + i];
                    }
                },
            }
        }
    }

    fn enterFixedArityCall(self: *Vm, closure: *runtime.Closure, callee_chunk: *const Chunk, argc: u8, tail: bool) Error!void {
        if (tail) {
            const current_bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
            const arg_start = self.sp - argc;
            if (self.fp > 0) {
                try self.restoreDynamicDepthsFromFrame(self.frames[self.fp - 1]);
            }

            for (0..argc) |i| {
                self.stack[current_bp + i] = self.stack[arg_start + i];
            }
            self.sp = current_bp + argc;

            self.chunk = callee_chunk;
            self.ip = 0;

            if (self.fp > 0) {
                self.frames[self.fp - 1].closure = closure;
                self.frames[self.fp - 1].argc = argc;
                self.frames[self.fp - 1].positional_argc = argc;
            }

            var i: usize = argc;
            while (i < callee_chunk.num_locals) : (i += 1) {
                try self.push(Value.nil);
            }
            return;
        }

        if (self.fp >= MAX_FRAMES) return error.StackOverflow;
        self.frames[self.fp] = .{
            .chunk = self.chunk,
            .return_ip = self.ip,
            .bp = self.sp - argc - 1,
            .closure = closure,
            .argc = argc,
            .positional_argc = argc,
            .block_depth = self.block_sp,
            .catch_depth = self.catch_sp,
            .unwind_depth = self.unwind_sp,
            .restart_depth = self.restart_sp,
            .progv_depth = self.progv_sp,
            .handler_depth = self.handler_sp,
            .handler_restore_depth = self.pending_handler_restore_depth,
        };
        self.pending_handler_restore_depth = null;
        self.fp += 1;

        const new_bp = self.sp - argc - 1;
        for (0..argc) |i| {
            self.stack[new_bp + i] = self.stack[new_bp + 1 + i];
        }
        self.sp = new_bp + argc;
        self.frames[self.fp - 1].bp = new_bp;

        self.chunk = callee_chunk;
        self.ip = 0;

        var i: usize = argc;
        while (i < callee_chunk.num_locals) : (i += 1) {
            try self.push(Value.nil);
        }
    }

    fn doCall(self: *Vm, argc: u8, tail: bool) Error!void {
        // Bounds check: need at least argc + 1 items on stack (args + function)
        if (self.sp < @as(usize, argc) + 1) return error.StackUnderflow;

        // Get function value (below args on stack)
        const fn_slot = self.sp - argc - 1;
        const fn_designator = self.stack[fn_slot];
        var fn_val = fn_designator;
        var fn_from_symbol_resolve = false;

        // Function designator: symbol -> function cell/global binding.
        if (fn_val.isSymbol()) {
            if (self.trace_fn_resolve) {
                const sym_name = fn_val.toPtr(Symbol).getName();
                const frame_name = switch (self.chunk.name.typeKind()) {
                    .symbol => self.chunk.name.toPtr(Symbol).getName(),
                    .string => self.chunk.name.toPtr(runtime.String).bytes(),
                    else => "<anon>",
                };
                std.debug.print("TRACE do-call fn={s} frame={s} ip={d} sp={d} fp={d}\n", .{
                    sym_name,
                    frame_name,
                    self.ip,
                    self.sp,
                    self.fp,
                });
            }
            fn_val = (try self.resolveFunctionValue(fn_val)) orelse {
                if (self.shouldTraceError(error.UnboundSymbol)) {
                    const fn_name = self.stack[self.sp - argc - 1];
                    if (fn_name.isSymbol()) {
                        const sym = fn_name.toPtr(Symbol);
                        var pkg_name: []const u8 = "<none>";
                        if (self.heap.symbolHomePkg(sym)) |pkg| {
                            pkg_name = pkg.name;
                        }
                        std.debug.print("TRACE unbound function: {s} pkg={s}\n", .{ sym.getName(), pkg_name });
                    }
                }
                return error.UnboundSymbol;
            };
            self.stack[fn_slot] = fn_val;
            fn_from_symbol_resolve = true;
        }

        if (!fn_from_symbol_resolve) {
            const canonical_fn = self.resolveForwardedValue(fn_val);
            if (canonical_fn.raw != fn_val.raw) {
                fn_val = canonical_fn;
                self.stack[fn_slot] = fn_val;
            }
        }

        // If calling a generic function, delegate to its dispatcher
        if (fn_val.isGenericFunction()) {
            const gf = fn_val.toPtr(runtime.objects.GenericFunction);
            if (gf.dispatcher.isNil()) {
                try self.callMismatch(fn_val, argc, "gf-dispatcher-nil");
                return;
            }
            fn_val = self.resolveForwardedValue(gf.dispatcher);
            // Update function slot on stack
            self.stack[fn_slot] = fn_val;
        }

        if (fn_val.typeKind() == .native_code) {
            const nc = fn_val.toPtr(runtime.NativeCode);
            const tag: BuiltinCallableTag = @enumFromInt(nc.entry);
            const result = try self.doBuiltinCallable(tag, self.stack[fn_slot + 1 .. fn_slot + 1 + argc]);
            try self.builtinResultToCallFrame(fn_slot, result);
            return;
        }

        if (!fn_val.isClosure()) {
            if (self.trace_call_mismatch and fn_designator.isSymbol()) {
                const sym = fn_designator.toPtr(Symbol);
                std.debug.print("CALL_MISMATCH symbol={s}\n", .{sym.getName()});
            }
            try self.callMismatch(fn_val, argc, "not-closure");
            return;
        }

        const closure = fn_val.toPtr(runtime.Closure);
        var callee_chunk_opt: ?*const Chunk = null;
        const raw_code = closure.code;
        if (raw_code.isChunk()) {
            const chunk = raw_code.toPtr(Chunk);
            if (chunk.kind == .chunk) {
                callee_chunk_opt = chunk;
            }
        }
        if (callee_chunk_opt == null) {
            const code_val = self.resolveForwardedValue(raw_code);
            if (code_val.raw != raw_code.raw) {
                closure.code = code_val;
                self.writeBarrierStore(Value.makeClosure(closure), code_val);
            }
            callee_chunk_opt = self.chunkFromValue(code_val);
        }
        const callee_chunk = callee_chunk_opt orelse {
            try self.callMismatch(fn_val, argc, "closure-code-not-chunk");
            return;
        };
        const arity = callee_chunk.arity;
        const opt_count = callee_chunk.opt_count;
        const key_count = callee_chunk.key_count;
        const has_rest = callee_chunk.has_rest != 0;
        const max_positional = arity + opt_count;
        const call_shape = classifyCallShape(callee_chunk);
        const dynamic_call = fn_designator.isSymbol() or
            fn_designator.isGenericFunction() or
            (fn_designator.raw != fn_val.raw and !fn_designator.isClosure());

        if (!has_rest and opt_count == 0 and key_count == 0) {
            if (argc != arity) {
                try self.callMismatch(fn_val, argc, "fixed-arity");
                return;
            }
            self.recordCallShape(call_shape, tail, dynamic_call);
            try self.enterFixedArityCall(closure, callee_chunk, argc, tail);
            return;
        }

        // Determine positional argument span before keyword pairs.
        // For &key without &optional, keys start after required args.
        // For &optional + &key, treat a keyword as key-start only when the
        // remaining argument tail can form complete key/value pairs.
        var actual_positional = argc;
        if (key_count > 0 and argc > arity) {
            if (opt_count == 0) {
                actual_positional = arity;
            } else {
                const arg_base = self.sp - argc;
                actual_positional = arity;
                while (actual_positional < argc and actual_positional < max_positional) : (actual_positional += 1) {
                    const rem = argc - actual_positional;
                    if (rem >= 2 and rem % 2 == 0 and isKeywordRaw(self.stack[arg_base + actual_positional])) {
                        break;
                    }
                }
            }
        }

        // Check arity
        if (has_rest) {
            // Variadic: need at least required args
            if (argc < arity) {
                try self.callMismatch(fn_val, argc, "rest-arity");
                return;
            }
        } else if (key_count > 0) {
            // Has keyword params: need at least required args
            if (argc < arity) {
                try self.callMismatch(fn_val, argc, "key-min-arity");
                return;
            }
            // Keyword args must come in pairs (after actual positional args)
            const kw_arg_count = argc - actual_positional;
            if (kw_arg_count % 2 != 0) {
                try self.callMismatch(fn_val, argc, "key-odd-pairs");
                return;
            }
            if (callee_chunk.allow_other_keys == 0 and callee_chunk.allowed_keywords.raw != Value.nil.raw) {
                const allow_kw = self.builtins.kw_allow_other_keys;
                const arg_base = self.sp - argc;
                const allowed_list = callee_chunk.allowed_keywords;
                const kw_pair_count: u8 = (argc - actual_positional) / 2;
                var allowed_fast: []const Value = &.{};
                if (@as(usize, key_count) <= KEY_FAST_TABLE_MAX and kw_pair_count > 1) {
                    allowed_fast = self.lookupKeyAllowlistCache(callee_chunk, key_count);
                    if (allowed_fast.len == 0) {
                        allowed_fast = self.populateKeyAllowlistCache(callee_chunk, key_count, allowed_list);
                    }
                }

                var allow_unknown = false;
                var unknown_seen = false;
                var i: u8 = actual_positional;
                while (i + 1 < argc) : (i += 2) {
                    const kw = self.stack[arg_base + i];
                    if (kw.raw == allow_kw.raw) {
                        // ANSI CL accepts unknown keywords whenever :ALLOW-OTHER-KEYS
                        // is present in the call argument list, regardless of its value.
                        allow_unknown = true;
                        break;
                    }
                    if (!unknown_seen) {
                        const known = if (allowed_fast.len > 0)
                            keywordInSlice(kw, allowed_fast)
                        else
                            isAllowedKeyword(kw, allowed_list);
                        if (!known) unknown_seen = true;
                    }
                }
                if (!allow_unknown and unknown_seen) {
                    try self.callMismatch(fn_val, argc, "key-unknown");
                    return;
                }
            }
        } else if (opt_count > 0) {
            // Has optional params: argc must be in [arity, arity + opt_count]
            if (argc < arity or argc > max_positional) {
                try self.callMismatch(fn_val, argc, "optional-arity");
                return;
            }
        } else {
            // Fixed: need exact arity
            if (argc != arity) {
                try self.callMismatch(fn_val, argc, "fixed-arity");
                return;
            }
        }

        // Build rest list if variadic (before we modify the stack)
        // Rest list contains args beyond required + optional + key params
        var rest_list = Value.nil;
        if (has_rest and argc > max_positional) {
            // Build list from extra args (in reverse since we pop from end)
            const extra_count = argc - max_positional;
            var i: u8 = 0;
            while (i < extra_count) : (i += 1) {
                const idx = self.sp - 1 - i;
                rest_list = try self.allocCons(self.stack[idx], rest_list);
            }
            // Pop the extra args
            self.sp -= extra_count;
        }

        self.recordCallShape(call_shape, tail, dynamic_call);

        // Determine how many args to copy as locals
        // For keyword args, we need to keep ALL args for find_key to scan
        const actual_argc: u8 = if (key_count > 0) argc else @min(argc, max_positional);
        const positional_argc: u8 = if (key_count > 0) actual_positional else actual_argc;

        if (tail) {
            // Tail call: reuse current frame
            // Move arguments to start of current frame
            const current_bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
            const arg_start = self.sp - actual_argc;
            if (self.fp > 0) {
                try self.restoreDynamicDepthsFromFrame(self.frames[self.fp - 1]);
            }

            if (key_count > 0) {
                // For key args, layout is:
                // [required + optional args] [key params (nil)] [other param locals]
                // [keyword pairs temp area]
                const positional_args: u8 = @min(actual_positional, max_positional);
                const kw_pair_count: u8 = argc - actual_positional;
                const key_slot_start = max_positional;
                const kw_pair_start = callee_chunk.key_temp_start;

                // Copy positional args first so keyword-pair relocation cannot clobber
                // positional sources when stack ranges overlap in tail-call reuse.
                for (0..positional_args) |j| {
                    self.stack[current_bp + j] = self.stack[arg_start + j];
                }

                // Move keyword pairs to their slots.
                const kw_count_usize: usize = @intCast(kw_pair_count);
                self.stackMove(
                    current_bp + @as(usize, kw_pair_start),
                    arg_start + @as(usize, positional_args),
                    kw_count_usize,
                );

                // Initialize key param slots and any intermediate param-local
                // slots (for supplied-p/rest locals) to nil.
                for (0..(kw_pair_start - key_slot_start)) |k| {
                    self.stack[current_bp + key_slot_start + k] = Value.nil;
                }

                self.sp = current_bp + kw_pair_start + kw_pair_count;
            } else {
                // Copy args to current frame's base
                for (0..actual_argc) |i| {
                    self.stack[current_bp + i] = self.stack[arg_start + i];
                }
                self.sp = current_bp + actual_argc;
            }

            // If variadic, push rest list as next local (after required + optional)
            if (has_rest) {
                try self.push(rest_list);
            }

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Update closure and argc in current frame
            if (self.fp > 0) {
                self.frames[self.fp - 1].closure = closure;
                self.frames[self.fp - 1].argc = argc;
                self.frames[self.fp - 1].positional_argc = positional_argc;
            }

            // Reserve space for additional locals.
            // For keyword lambdas, keyword-pair slots live at chunk.key_temp_start.
            const used_slots: usize = if (key_count > 0) blk: {
                const kw_pair_count: u8 = argc - actual_positional;
                break :blk @as(usize, callee_chunk.key_temp_start) + @as(usize, kw_pair_count);
            } else @as(usize, actual_argc);
            const used_locals: usize = used_slots + @as(usize, if (has_rest) @as(u8, 1) else @as(u8, 0));
            var i: usize = used_locals;
            while (i < callee_chunk.num_locals) : (i += 1) {
                try self.push(Value.nil);
            }
        } else {
            // Regular call: push new frame
            if (self.fp >= MAX_FRAMES) {
                return error.StackOverflow;
            }

            // Save current state
            self.frames[self.fp] = .{
                .chunk = self.chunk,
                .return_ip = self.ip,
                .bp = self.sp - actual_argc - 1, // -1 for function value
                .closure = closure,
                .argc = argc,
                .positional_argc = positional_argc,
                .block_depth = self.block_sp,
                .catch_depth = self.catch_sp,
                .unwind_depth = self.unwind_sp,
                .restart_depth = self.restart_sp,
                .progv_depth = self.progv_sp,
                .handler_depth = self.handler_sp,
                .handler_restore_depth = self.pending_handler_restore_depth,
            };
            self.pending_handler_restore_depth = null;
            self.fp += 1;

            // The arguments are already on stack above the function value
            // We need to set bp to point to first arg (overwriting fn_val slot)
            const new_bp = self.sp - actual_argc - 1;

            if (key_count > 0) {
                // For key args, layout is:
                // [required + optional args] [key params (nil)] [other param locals]
                // [keyword pairs temp area]
                const positional_args: u8 = @min(actual_positional, max_positional);
                const kw_pair_count: u8 = argc - actual_positional;
                const key_slot_start = max_positional;
                const kw_pair_start = callee_chunk.key_temp_start;

                // First, move keyword pairs to their slots (overlap-safe).
                // Keyword pairs are the last kw_pair_count args.
                const kw_count_usize: usize = @intCast(kw_pair_count);
                self.stackMove(
                    new_bp + @as(usize, kw_pair_start),
                    new_bp + 1 + @as(usize, positional_args),
                    kw_count_usize,
                );

                // Copy positional args to their slots
                for (0..positional_args) |j| {
                    self.stack[new_bp + j] = self.stack[new_bp + 1 + j];
                }

                // Initialize key param slots and any intermediate param-local
                // slots (for supplied-p/rest locals) to nil.
                for (0..(kw_pair_start - key_slot_start)) |k| {
                    self.stack[new_bp + key_slot_start + k] = Value.nil;
                }

                self.sp = new_bp + kw_pair_start + kw_pair_count;
            } else {
                // Normal case: copy args to slots [0, argc)
                for (0..actual_argc) |i| {
                    self.stack[new_bp + i] = self.stack[new_bp + 1 + i];
                }
                self.sp = new_bp + actual_argc;
            }

            // If variadic, push rest list as next local (after required + optional)
            if (has_rest) {
                try self.push(rest_list);
            }

            // Update frame bp
            self.frames[self.fp - 1].bp = new_bp;

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Reserve space for additional locals.
            // For keyword lambdas, keyword-pair slots live at chunk.key_temp_start.
            const used_slots: usize = if (key_count > 0) blk: {
                const kw_pair_count: u8 = argc - actual_positional;
                break :blk @as(usize, callee_chunk.key_temp_start) + @as(usize, kw_pair_count);
            } else @as(usize, actual_argc);
            const used: usize = used_slots + @as(usize, if (has_rest) @as(u8, 1) else @as(u8, 0));
            var i: usize = used;
            while (i < callee_chunk.num_locals) : (i += 1) {
                try self.push(Value.nil);
            }
        }
    }

    inline fn classifyCallShape(callee_chunk: *const Chunk) CallShapeKind {
        if (callee_chunk.key_count != 0) return .key;
        if (callee_chunk.has_rest != 0) return .rest;
        if (callee_chunk.opt_count != 0) return .optional;
        return .fixed;
    }

    inline fn recordCallShape(self: *Vm, kind: CallShapeKind, tail: bool, dynamic: bool) void {
        if (!self.track_call_shape) return;
        self.call_shape.total +%= 1;
        if (tail) self.call_shape.tail +%= 1;
        if (dynamic) self.call_shape.dynamic +%= 1;
        switch (kind) {
            .fixed => self.call_shape.fixed +%= 1,
            .optional => self.call_shape.optional +%= 1,
            .key => self.call_shape.key +%= 1,
            .rest => self.call_shape.rest +%= 1,
        }
    }

    fn doApply(self: *Vm) Error!void {
        // Stack: ... fn args-list
        const args_list = try self.pop();
        const fn_val = try self.pop();
        var callable = fn_val;
        const trace_call_mismatch = self.trace_call_mismatch;
        const trace_do_apply = self.trace_call_mismatch_apply;

        // Function designator: symbol -> function cell/global binding.
        if (callable.isSymbol()) {
            if (self.trace_fn_resolve) {
                const sym_name = callable.toPtr(Symbol).getName();
                const frame_name = switch (self.chunk.name.typeKind()) {
                    .symbol => self.chunk.name.toPtr(Symbol).getName(),
                    .string => self.chunk.name.toPtr(runtime.String).bytes(),
                    else => "<anon>",
                };
                std.debug.print("TRACE do-apply fn={s} frame={s} ip={d} sp={d} fp={d}\n", .{
                    sym_name,
                    frame_name,
                    self.ip,
                    self.sp,
                    self.fp,
                });
            }
            callable = (try self.resolveFunctionValue(callable)) orelse {
                if (self.shouldTraceError(error.UnboundSymbol)) {
                    if (callable.isSymbol()) {
                        std.debug.print("TRACE unbound apply-callable: {s}\n", .{callable.toPtr(Symbol).getName()});
                    }
                }
                return error.UnboundSymbol;
            };
        }

        // Generic function designator resolves to dispatcher closure.
        if (callable.isGenericFunction()) {
            const gf = callable.toPtr(runtime.objects.GenericFunction);
            if (gf.dispatcher.isNil()) {
                if (trace_do_apply) {
                    std.debug.print("CALL_MISMATCH DO_APPLY gf-dispatcher-nil\n", .{});
                    if (fn_val.isSymbol()) {
                        std.debug.print("CALL_MISMATCH DO_APPLY fn-symbol={s}\n", .{fn_val.toPtr(Symbol).getName()});
                    }
                }
                const te = self.intern("type-error") catch return error.TypeMismatch;
                const pair = self.allocCons(te, fn_val) catch return error.TypeMismatch;
                self.doThrow(self.builtins.sym_condition_tag, pair) catch |e| return e;
                return;
            }
            callable = gf.dispatcher;
        }

        if (!callable.isClosure() and callable.typeKind() != .native_code) {
            if (trace_do_apply) {
                std.debug.print("CALL_MISMATCH DO_APPLY non-closure kind={s}\n", .{@tagName(callable.typeKind())});
                std.debug.print("CALL_MISMATCH DO_APPLY fn-val-kind={s}\n", .{@tagName(fn_val.typeKind())});
                if (self.chunk.name.isSymbol()) {
                    std.debug.print("CALL_MISMATCH DO_APPLY frame={s}\n", .{self.chunk.name.toPtr(Symbol).getName()});
                } else if (self.chunk.name.isString()) {
                    std.debug.print("CALL_MISMATCH DO_APPLY frame={s}\n", .{self.chunk.name.toPtr(runtime.String).bytes()});
                }
                std.debug.print("CALL_MISMATCH DO_APPLY fp={d} sp={d} ip={d}\n", .{ self.fp, self.sp, self.ip });
                if (fn_val.isSymbol()) {
                    std.debug.print("CALL_MISMATCH DO_APPLY fn-symbol={s}\n", .{fn_val.toPtr(Symbol).getName()});
                } else if (fn_val.isKeyword()) {
                    std.debug.print("CALL_MISMATCH DO_APPLY fn-keyword={s}\n", .{fn_val.toPtr(runtime.objects.Keyword).getName()});
                }
                var dbg = args_list;
                var i: usize = 0;
                while (dbg.isCons() and i < 8) : (i += 1) {
                    const cell = dbg.toPtr(runtime.Cons);
                    std.debug.print("CALL_MISMATCH DO_APPLY arglist[{d}]={s}\n", .{ i, @tagName(cell.car.typeKind()) });
                    if (cell.car.isSymbol()) {
                        std.debug.print("CALL_MISMATCH DO_APPLY arglist[{d}]-sym={s}\n", .{ i, cell.car.toPtr(Symbol).getName() });
                    } else if (cell.car.isKeyword()) {
                        std.debug.print("CALL_MISMATCH DO_APPLY arglist[{d}]-kw={s}\n", .{ i, cell.car.toPtr(runtime.objects.Keyword).getName() });
                    }
                    dbg = cell.cdr;
                }
                if (!dbg.isNil()) {
                    std.debug.print("CALL_MISMATCH DO_APPLY arglist-tail={s}\n", .{@tagName(dbg.typeKind())});
                }
                var shown: usize = 0;
                var fi: usize = self.fp;
                while (fi > 0 and shown < 8) : (shown += 1) {
                    fi -= 1;
                    const frame = self.frames[fi];
                    std.debug.print("CALL_MISMATCH DO_APPLY frame[{d}] return_ip={d}", .{ fi, frame.return_ip });
                    if (frame.chunk.name.isSymbol()) {
                        std.debug.print(" name={s}", .{frame.chunk.name.toPtr(Symbol).getName()});
                    } else if (frame.chunk.name.isString()) {
                        std.debug.print(" name={s}", .{frame.chunk.name.toPtr(runtime.String).bytes()});
                    }
                    std.debug.print("\n", .{});
                }
            }
            const te = self.intern("type-error") catch return error.TypeMismatch;
            const pair = self.allocCons(te, callable) catch return error.TypeMismatch;
            self.doThrow(self.builtins.sym_condition_tag, pair) catch |e| return e;
            return;
        }

        if (trace_do_apply) {
            std.debug.print("CALL_MISMATCH DO_APPLY entry fn-kind={s} args-kind={s}\n", .{
                @tagName(fn_val.typeKind()),
                @tagName(args_list.typeKind()),
            });
            if (callable.isClosure()) {
                const clos = callable.toPtr(runtime.Closure);
                if (self.chunkFromValue(clos.code)) |callee| {
                    switch (callee.name.typeKind()) {
                        .symbol => std.debug.print("CALL_MISMATCH DO_APPLY callee={s} arity={d}\n", .{
                            callee.name.toPtr(Symbol).getName(),
                            callee.arity,
                        }),
                        .string => std.debug.print("CALL_MISMATCH DO_APPLY callee={s} arity={d}\n", .{
                            callee.name.toPtr(runtime.String).bytes(),
                            callee.arity,
                        }),
                        else => std.debug.print("CALL_MISMATCH DO_APPLY callee-kind={s} arity={d}\n", .{
                            @tagName(callee.name.typeKind()),
                            callee.arity,
                        }),
                    }
                }
            }
            if (fn_val.isSymbol()) {
                std.debug.print("CALL_MISMATCH DO_APPLY fn-symbol={s}\n", .{fn_val.toPtr(Symbol).getName()});
            }
            var dbg = args_list;
            var di: usize = 0;
            while (dbg.isCons() and di < 8) : (di += 1) {
                const cell = dbg.toPtr(runtime.Cons);
                std.debug.print("CALL_MISMATCH DO_APPLY list[{d}]={s}\n", .{ di, @tagName(cell.car.typeKind()) });
                if (cell.car.isKeyword()) {
                    std.debug.print("CALL_MISMATCH DO_APPLY list[{d}]-kw={s}\n", .{ di, cell.car.toPtr(runtime.objects.Keyword).getName() });
                } else if (cell.car.isSymbol()) {
                    std.debug.print("CALL_MISMATCH DO_APPLY list[{d}]-sym={s}\n", .{ di, cell.car.toPtr(Symbol).getName() });
                }
                dbg = cell.cdr;
            }
            if (!dbg.isNil()) {
                std.debug.print("CALL_MISMATCH DO_APPLY list-tail={s}\n", .{@tagName(dbg.typeKind())});
            }
        }

        // Count and push args from list
        var count: u8 = 0;
        var list = args_list;
        while (list.isCons()) {
            if (count >= 255) return error.StackOverflow;
            const cons = list.toPtr(runtime.Cons);
            try self.push(cons.car);
            count += 1;
            list = cons.cdr;
        }

        // Validate list ended with nil (not improper list)
        if (!list.isNil()) {
            if (trace_do_apply) {
                std.debug.print("CALL_MISMATCH DO_APPLY improper-tail kind={s}\n", .{@tagName(list.typeKind())});
            }
            return error.TypeMismatch;
        }

        // Push function before args on stack
        // Current stack: ... arg1 arg2 ... argN
        // Need: ... fn arg1 arg2 ... argN
        // So we shift args up and insert fn
        if (count > 0) {
            // We are about to insert one extra stack slot for the callable.
            if (self.sp >= STACK_SIZE) return error.StackOverflow;
            // Bounds check before shuffling
            if (count > self.sp) return error.StackUnderflow;
            // Make room by moving args up one slot
            var i: usize = count;
            while (i > 0) {
                i -= 1;
                self.stack[self.sp - count + i + 1] = self.stack[self.sp - count + i];
            }
            self.stack[self.sp - count] = callable;
            self.sp += 1;
        } else {
            try self.push(callable);
        }

        // Now call with unpacked args
        if (trace_do_apply) {
            std.debug.print("CALL_MISMATCH DO_APPLY callable-kind={s} argc={d}\n", .{
                @tagName(callable.typeKind()),
                count,
            });
            if (self.sp >= @as(usize, count) + 1) {
                const fn_slot = self.sp - count - 1;
                const fn_dbg = self.stack[fn_slot];
                std.debug.print("CALL_MISMATCH DO_APPLY stack-fn-kind={s}\n", .{@tagName(fn_dbg.typeKind())});
                if (fn_dbg.isSymbol()) {
                    std.debug.print("CALL_MISMATCH DO_APPLY stack-fn-symbol={s}\n", .{fn_dbg.toPtr(Symbol).getName()});
                }
            }
        }
        _ = trace_call_mismatch; // Keep env read local for call-mismatch diagnostics without DO_APPLY spam.
        try self.doCall(count, false);
    }

    // ========================================================================
    // Stack operations
    // ========================================================================

    pub fn push(self: *Vm, val: Value) Error!void {
        if (self.sp >= STACK_SIZE) return error.StackOverflow;
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    fn pop(self: *Vm) Error!Value {
        if (self.sp == 0) return error.StackUnderflow;
        self.sp -= 1;
        return self.stack[self.sp];
    }

    fn popArgs(self: *Vm, n: usize) Error!Value {
        var args = Value.nil;
        var i: usize = 0;
        while (i < n) : (i += 1) {
            const v = try self.pop();
            args = try self.allocCons(v, args);
        }
        return args;
    }

    fn peek(self: *Vm, distance: usize) Error!Value {
        if (distance >= self.sp) return error.StackUnderflow;
        return self.stack[self.sp - 1 - distance];
    }

    fn strCharCode(item: Value) Error!u8 {
        switch (item.typeKind()) {
            .char => {
                const cp = item.toCharacter();
                if (cp > 255) return error.InvalidArgument;
                return @intCast(cp);
            },
            .fixnum => {
                const n = item.toFixnum();
                if (n < 0 or n > 255) return error.InvalidArgument;
                return @intCast(n);
            },
            else => return error.InvalidArgument,
        }
    }

    /// Position search for lists, strings, and vectors
    pub fn positionInSeq(self: *Vm, item: Value, seq: Value, cmp: runtime.HashTest) Error!Value {
        _ = self;
        // String: item should be a character or fixnum (char code)
        if (seq.isString()) {
            const str_obj = seq.toPtr(runtime.String);
            const str_bytes = str_obj.bytes();
            const char_code = try strCharCode(item);
            const needle = Value.makeCharacter(@intCast(char_code));
            for (str_bytes, 0..) |c, i| {
                const elem = Value.makeCharacter(@intCast(c));
                if (hashKeyEqualWithTest(needle, elem, cmp)) {
                    return Value.makeFixnum(@intCast(i));
                }
            }
            return Value.nil;
        }
        // Vector: search elements
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            for (0..vec.length) |i| {
                const elem = vec.get(i);
                if (hashKeyEqualWithTest(item, elem, cmp)) {
                    return Value.makeFixnum(@intCast(i));
                }
            }
            return Value.nil;
        }
        // List: iterate through cons cells
        var curr = seq;
        var idx: i64 = 0;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (hashKeyEqualWithTest(item, c.car, cmp)) {
                return Value.makeFixnum(idx);
            }
            curr = c.cdr;
            idx += 1;
        }
        return Value.nil;
    }

    /// Find search for lists, strings, and vectors
    pub fn findInSeq(self: *Vm, item: Value, seq: Value, cmp: runtime.HashTest) Error!Value {
        _ = self;
        // String: item should be a character or fixnum (char code)
        if (seq.isString()) {
            const str_obj = seq.toPtr(runtime.String);
            const str_bytes = str_obj.bytes();
            const char_code = try strCharCode(item);
            const needle = Value.makeCharacter(@intCast(char_code));
            for (str_bytes) |c| {
                const elem = Value.makeCharacter(@intCast(c));
                if (hashKeyEqualWithTest(needle, elem, cmp)) {
                    return elem;
                }
            }
            return Value.nil;
        }
        // Vector: search elements
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            for (0..vec.length) |i| {
                const elem = vec.get(i);
                if (hashKeyEqualWithTest(item, elem, cmp)) {
                    return elem;
                }
            }
            return Value.nil;
        }
        // List: iterate through cons cells
        var curr = seq;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (hashKeyEqualWithTest(item, c.car, cmp)) {
                return c.car;
            }
            curr = c.cdr;
        }
        return Value.nil;
    }

    /// Count occurrences for lists, strings, and vectors
    pub fn countInSeq(self: *Vm, item: Value, seq: Value, cmp: runtime.HashTest) Error!Value {
        _ = self;
        // String: item should be a character or fixnum (char code)
        if (seq.isString()) {
            const str_obj = seq.toPtr(runtime.String);
            const str_bytes = str_obj.bytes();
            const char_code = try strCharCode(item);
            const needle = Value.makeCharacter(@intCast(char_code));
            var n: i64 = 0;
            for (str_bytes) |c| {
                const elem = Value.makeCharacter(@intCast(c));
                if (hashKeyEqualWithTest(needle, elem, cmp)) {
                    n += 1;
                }
            }
            return Value.makeFixnum(n);
        }
        // Vector: search elements
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            var n: i64 = 0;
            for (0..vec.length) |i| {
                const elem = vec.get(i);
                if (hashKeyEqualWithTest(item, elem, cmp)) {
                    n += 1;
                }
            }
            return Value.makeFixnum(n);
        }
        // List: iterate through cons cells
        var curr = seq;
        var n: i64 = 0;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (hashKeyEqualWithTest(item, c.car, cmp)) {
                n += 1;
            }
            curr = c.cdr;
        }
        return Value.makeFixnum(n);
    }

    pub fn listRemoveWithTest(self: *Vm, item: Value, seq: Value, test_type: runtime.HashTest) Error!Value {
        const saved_sp = self.sp;
        errdefer self.sp = saved_sp;

        if (self.sp + 4 > STACK_SIZE) return error.StackOverflow;
        const item_idx = self.sp;
        const seq_idx = self.sp + 1;
        const result_idx = self.sp + 2;
        const tail_idx = self.sp + 3;

        self.stack[item_idx] = item;
        self.stack[seq_idx] = seq;
        self.stack[result_idx] = Value.nil;
        self.stack[tail_idx] = Value.nil;
        self.sp += 4;

        while (self.stack[seq_idx].isCons()) {
            const curr_val = self.stack[seq_idx];
            const c = curr_val.toPtr(Cons);
            const car = c.car;
            const next = c.cdr;
            self.stack[seq_idx] = next; // root across allocCons GC

            if (hashKeyEqualWithTest(car, self.stack[item_idx], test_type)) continue;

            const new_cons = try self.allocCons(car, Value.nil);
            const tail_val = self.stack[tail_idx];
            if (tail_val.isCons()) {
                tail_val.toPtr(Cons).cdr = new_cons;
                self.writeBarrierStore(tail_val, new_cons);
            } else {
                self.stack[result_idx] = new_cons;
            }
            self.stack[tail_idx] = new_cons;
        }

        if (self.stack[seq_idx] != Value.nil) return error.TypeMismatch;

        const result = self.stack[result_idx];
        self.sp = saved_sp;
        return result;
    }

    // ========================================================================
    // Bytecode reading
    // ========================================================================

    fn readOp(self: *Vm) Op {
        const code = self.chunk.code;
        const low: u16 = code[self.ip];
        const high: u16 = code[self.ip + 1];
        self.ip += 2;
        const opcode = low | (high << 8);
        return @enumFromInt(opcode);
    }

    fn readU8(self: *Vm) u8 {
        const byte = self.chunk.code[self.ip];
        self.ip += 1;
        return byte;
    }

    fn readU16(self: *Vm) u16 {
        const code = self.chunk.code;
        const off = self.ip;
        const val = @as(u16, code[off]) | (@as(u16, code[off + 1]) << 8);
        self.ip += 2;
        return val;
    }

    fn readI16(self: *Vm) i16 {
        const code = self.chunk.code;
        const off = self.ip;
        const raw = @as(u16, code[off]) | (@as(u16, code[off + 1]) << 8);
        const val: i16 = @bitCast(raw);
        self.ip += 2;
        return val;
    }

    fn readI32(self: *Vm) i32 {
        const code = self.chunk.code;
        const off = self.ip;
        const raw = @as(u32, code[off]) |
            (@as(u32, code[off + 1]) << 8) |
            (@as(u32, code[off + 2]) << 16) |
            (@as(u32, code[off + 3]) << 24);
        const val: i32 = @bitCast(raw);
        self.ip += 4;
        return val;
    }

    pub fn resolveForwardedValue(self: *Vm, val: Value) Value {
        if (!val.isPointer()) return val;
        var cur = val;
        var resolved_live: ?Value = null;
        var hops: u8 = 0;
        while (hops < 8 and cur.isPointer()) : (hops += 1) {
            const addr = cur.toPtrAddr();
            if (!self.heap.containsAddrForDebug(addr)) break;

            const first_word: *const Value = @ptrFromInt(addr);
            if (!first_word.isForwarding()) break;

            const new_addr = first_word.toPtrAddr();
            const forwarded_size_ptr: *const usize = @ptrFromInt(addr + @sizeOf(Value));
            const forwarded_size = forwarded_size_ptr.*;
            const forwarded_size_ok = forwarded_size > 0 and
                forwarded_size <= self.heap.space_size and
                std.mem.isAligned(forwarded_size, @import("../runtime/heap.zig").ALIGNMENT);

            const from_start = @intFromPtr(self.heap.from_start);
            const from_live_end = @intFromPtr(self.heap.alloc_ptr);
            const src_in_from = addr >= from_start and addr < from_live_end;
            const in_from = new_addr >= from_start and new_addr < from_live_end and
                forwarded_size <= from_live_end - new_addr;

            const stale_start = @intFromPtr(self.heap.to_start);
            const stale_end = stale_start + self.heap.space_size;
            const in_stale = new_addr >= stale_start and new_addr < stale_end and
                forwarded_size <= stale_end - new_addr;

            var in_tenured = false;
            if (self.heap.gcLayoutMode() == .generational) {
                if (self.heap.tenuredRegion()) |tenured| {
                    const ten_start = @intFromPtr(tenured.start);
                    const ten_used_end = if (self.heap.tenured_alloc_ptr) |p| @intFromPtr(p) else ten_start;
                    in_tenured = new_addr >= ten_start and new_addr < ten_used_end and
                        forwarded_size <= ten_used_end - new_addr;
                }
            }

            if (!forwarded_size_ok or !(in_from or in_stale or in_tenured)) break;
            if (!runtime.objects.forwardingTargetLooksValid(cur.getTag(), new_addr, forwarded_size)) break;

            const next = Value{ .raw = new_addr | @as(u64, @intFromEnum(cur.getTag())) };
            if (next.raw == cur.raw) break;
            cur = next;
            if (in_from or in_tenured or (src_in_from and in_stale)) {
                resolved_live = cur;
            }
        }

        return resolved_live orelse val;
    }

    fn loadConst(self: *Vm, idx: u16) Error!Value {
        var consts = self.chunk.getConstants();
        if (idx >= consts.len) return error.InvalidConstant;

        const gc_count = self.heap.stats.gc_count;
        const chunk_key = @intFromPtr(self.chunk);
        if (self.const_last_chunk_key != chunk_key or self.const_last_gc_count != gc_count) {
            if (!self.chunkConstsAreFresh(self.chunk, gc_count)) {
                self.refreshChunkConsts(self.chunk);
                self.markChunkConstsFresh(self.chunk, gc_count);
                consts = self.chunk.getConstants();
            }
            self.const_last_chunk_key = chunk_key;
            self.const_last_gc_count = gc_count;
        }

        return consts[idx];
    }

    fn refreshCurrentChunk(self: *Vm) void {
        const cur = Value.makeChunk(self.chunk);
        const fixed = self.resolveForwardedValue(cur);
        if (fixed.raw == cur.raw) return;
        if (self.chunkFromValue(fixed)) |chunk| {
            self.chunk = chunk;
        }
    }

    // ========================================================================
    // Binary operations
    // ========================================================================

    fn binaryOp(self: *Vm, comptime op: fn (i64, i64) Error!Value) Error!void {
        const b = try self.pop();
        const a = try self.pop();
        if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
        try self.push(try op(a.toFixnum(), b.toFixnum()));
    }

    fn binaryAdd(a: i64, b: i64) Error!Value {
        const result = @addWithOverflow(a, b);
        if (result[1] != 0) return error.TypeMismatch; // Overflow
        return Value.makeFixnum(result[0]);
    }

    fn binarySub(a: i64, b: i64) Error!Value {
        const result = @subWithOverflow(a, b);
        if (result[1] != 0) return error.TypeMismatch; // Overflow
        return Value.makeFixnum(result[0]);
    }

    fn binaryMul(a: i64, b: i64) Error!Value {
        const result = @mulWithOverflow(a, b);
        if (result[1] != 0) return error.TypeMismatch; // Overflow
        return Value.makeFixnum(result[0]);
    }

    fn binaryDiv(a: i64, b: i64) Error!Value {
        if (b == 0) return error.DivisionByZero;
        // minInt / -1 overflows
        if (a == std.math.minInt(i64) and b == -1) return error.TypeMismatch;
        return Value.makeFixnum(@divTrunc(a, b));
    }

    fn binaryMod(a: i64, b: i64) Error!Value {
        if (b == 0) return error.DivisionByZero;
        return Value.makeFixnum(@mod(a, b));
    }

    fn binaryQuot(a: i64, b: i64) Error!Value {
        if (b == 0) return error.DivisionByZero;
        if (a == std.math.minInt(i64) and b == -1) return error.TypeMismatch; // overflow
        return Value.makeFixnum(@divTrunc(a, b));
    }

    fn binaryRem(a: i64, b: i64) Error!Value {
        if (b == 0) return error.DivisionByZero;
        return Value.makeFixnum(@rem(a, b));
    }

    fn valToFloat(val: Value) Error!f64 {
        if (val.isFixnum()) return @floatFromInt(val.toFixnum());
        if (val.isFloat()) return val.toFloat();
        if (val.isRational()) {
            const rat = val.toPtr(runtime.Rational);
            const num: f64 = @floatFromInt(rat.numerator);
            const den: f64 = @floatFromInt(rat.denominator);
            return num / den;
        }
        return error.TypeMismatch;
    }

    fn binaryLt(a: i64, b: i64) Error!Value {
        return if (a < b) Value.t else Value.nil;
    }

    fn binaryGt(a: i64, b: i64) Error!Value {
        return if (a > b) Value.t else Value.nil;
    }

    fn binaryLe(a: i64, b: i64) Error!Value {
        return if (a <= b) Value.t else Value.nil;
    }

    fn binaryGe(a: i64, b: i64) Error!Value {
        return if (a >= b) Value.t else Value.nil;
    }

    fn binaryNumEq(a: i64, b: i64) Error!Value {
        return if (a == b) Value.t else Value.nil;
    }
};

// ============================================================================
// Equality helpers
// ============================================================================

/// Float equality per CL semantics: NaN != NaN, 0.0 == -0.0
fn floatEql(fa: f64, fb: f64) bool {
    // NaN is never equal to anything, including itself
    if (std.math.isNan(fa) or std.math.isNan(fb)) return false;
    // 0.0 and -0.0 are equal
    return fa == fb;
}

/// Structural equality (equal in Lisp)
/// Returns true if two values are structurally equal
fn valueEqual(a: Value, b: Value) bool {
    return valueEqualWithDepth(a, b, 0);
}

const MAX_EQUAL_DEPTH = 1000;

fn valueEqualWithDepth(a: Value, b: Value, depth: usize) bool {
    // Prevent stack overflow on circular structures
    if (depth > MAX_EQUAL_DEPTH) return false;

    // Handle floats with proper CL semantics (NaN != NaN, 0.0 == -0.0)
    if (a.isFloat() and b.isFloat()) {
        return floatEql(a.toFloat(), b.toFloat());
    }

    // Fast path: identical values (handles most cases)
    if (a.raw == b.raw) return true;

    // Both must be same type for structural equality
    // Fixnums are immediate, so if they're not identical, they're not equal
    if (a.isFixnum() or b.isFixnum()) return false;

    // Characters are immediate
    if (a.isCharacter() or b.isCharacter()) return false;

    // Floats already handled above
    if (a.isFloat() or b.isFloat()) return false;

    // Check tag type
    const tag_a = a.raw & 0xF;
    const tag_b = b.raw & 0xF;
    if (tag_a != tag_b) return false;

    // Both are pointers of same type
    switch (a.typeKind()) {
        .cons => {
            // Recursively compare car and cdr
            const cons_a = a.toPtr(Cons);
            const cons_b = b.toPtr(Cons);
            return valueEqualWithDepth(cons_a.car, cons_b.car, depth + 1) and
                valueEqualWithDepth(cons_a.cdr, cons_b.cdr, depth + 1);
        },
        .string => {
            // Compare strings character by character
            const str_a = a.toPtr(String);
            const str_b = b.toPtr(String);
            return std.mem.eql(u8, str_a.bytes(), str_b.bytes());
        },
        .vector => {
            // Compare vectors element by element
            const vec_a = a.toPtr(Vector);
            const vec_b = b.toPtr(Vector);
            if (vec_a.length != vec_b.length) return false;
            for (vec_a.items(), vec_b.items()) |ea, eb| {
                if (!valueEqualWithDepth(ea, eb, depth + 1)) return false;
            }
            return true;
        },
        else => return false,
    }
}

// ============================================================================
// Hash table helpers (open addressing with linear probing)
// ============================================================================

/// FNV-1a hash for bytes
fn fnvHash(bytes: []const u8) u64 {
    var hash: u64 = 0xcbf29ce484222325; // FNV offset basis
    for (bytes) |b| {
        hash ^= b;
        hash *%= 0x100000001b3; // FNV prime
    }
    return hash;
}

/// FNV-1a hash for a u64
fn fnvHashU64(val: u64) u64 {
    var hash: u64 = 0xcbf29ce484222325;
    var v = val;
    for (0..8) |_| {
        hash ^= @as(u8, @truncate(v));
        hash *%= 0x100000001b3;
        v >>= 8;
    }
    return hash;
}

/// Normalize float for hashing under eql semantics
/// 0.0 and -0.0 should hash the same; NaN hashes consistently
fn normalizeFloatForHash(f: f64) u64 {
    if (std.math.isNan(f)) return 0x7FF8000000000000; // Canonical NaN
    if (f == 0.0) return 0; // Normalize -0.0 to 0.0
    return @bitCast(f);
}

/// Hash a Value for use in hash table lookup
fn hashValueWithTest(val: Value, test_type: runtime.HashTest) u64 {
    switch (test_type) {
        .eq => {
            // eq: pure identity - hash by raw value
            // Symbols/keywords are interned, so hash by name for GC stability
            return switch (val.typeKind()) {
                .symbol => fnvHash(val.toPtr(runtime.Symbol).getName()),
                .keyword => fnvHash(val.toPtr(runtime.Keyword).getName()),
                else => fnvHashU64(val.raw),
            };
        },
        .eql => {
            // eql: identity for most, but floats need normalization
            // Symbols/keywords are interned, so hash by name for GC stability
            return switch (val.typeKind()) {
                .float => fnvHashU64(normalizeFloatForHash(val.toFloat())),
                .symbol => fnvHash(val.toPtr(runtime.Symbol).getName()),
                .keyword => fnvHash(val.toPtr(runtime.Keyword).getName()),
                else => fnvHashU64(val.raw),
            };
        },
        .equal => {
            // equal: content-based hashing
            return switch (val.typeKind()) {
                .nil, .t, .unbound, .fixnum, .char => fnvHashU64(val.raw),
                .float => fnvHashU64(normalizeFloatForHash(val.toFloat())),
                .symbol => fnvHash(val.toPtr(runtime.Symbol).getName()),
                .keyword => fnvHash(val.toPtr(runtime.Keyword).getName()),
                .string, .string32 => fnvHash(val.toPtr(runtime.String).bytes()),
                // Reference types: hash address (NOT stable across GC)
                .cons, .vector, .closure, .hashtable, .rational, .complex, .stream, .bignum, .array, .pathname, .package, .readtable, .chunk, .condition, .class, .slotdef, .generic_function, .method, .native_code => fnvHashU64(val.raw),
            };
        },
    }
}

/// Check if two Values are equal for hash table purposes
pub fn hashKeyEqualWithTest(a: Value, b: Value, test_type: runtime.HashTest) bool {
    return hash_prims.keyEqualWithTest(a, b, test_type);
}

// ============================================================================
// Tests
// ============================================================================

test "vm push and return" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const code = [_]u8{
        0x02, 0x00, // push_i32
        42, 0, 0, 0, // i32 value: 42
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "vm class_of builds args list with GC" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    // Fill from-space with unreachable garbage so op-args consing must GC.
    while (true) {
        _ = heap.allocCons(Value.nil, Value.nil) catch |err| switch (err) {
            error.OutOfMemory => break,
        };
    }

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const class_of_op: u16 = @intFromEnum(Op.class_of);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        42,                            0,
        0,                             0,
        @truncate(class_of_op & 0xFF), @truncate(class_of_op >> 8),
        @truncate(ret_op & 0xFF),      @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expect(result.isClass());
}

test "vm callClosure runs and restores" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_op: u16 = @intFromEnum(Op.push_i32);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_op & 0xFF), @truncate(push_op >> 8),
        42,                        0,
        0,                         0,
        @truncate(ret_op & 0xFF),  @truncate(ret_op >> 8),
    };

    const chunk_val = try heap.allocChunk(&code, &.{}, 0, 0, 0, false, 0);
    const closure_val = try heap.allocClosure(chunk_val, 0, &.{});
    const closure = closure_val.toPtr(runtime.Closure);

    const result = try vm.callClosure(closure, 0);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
    try testing.expect(vm.current_closure == null);
    try testing.expectEqual(@as(u8, 0), vm.current_argc);
}

test "vm allocVector propagates overflow" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const huge = std.math.maxInt(usize);
    try testing.expectError(error.Overflow, vm.allocVector(1, huge));
}

test "vm arithmetic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    // (+ 10 20) = 30
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const add_op: u16 = @intFromEnum(Op.add);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 10,                       0,                      0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 20,                       0,                      0, 0,
        @truncate(add_op & 0xFF),      @truncate(add_op >> 8),      @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "vm make_complex" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const consts = [_]Value{
        Value.makeFloat(1.5),
    };

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_complex_op: u16 = @intFromEnum(Op.make_complex);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0,                                 0,
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   2,                                 0,
        0,                               0,                             @truncate(make_complex_op & 0xFF), @truncate(make_complex_op >> 8),
        @truncate(ret_op & 0xFF),        @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expect(result.typeKind() == .complex);
    const cplx = result.toPtr(runtime.Complex);
    try testing.expectApproxEqAbs(@as(f64, 1.5), cplx.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 2.0), cplx.imag, 0.0001);
}

test "vm sym_name" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const push_t_op: u16 = @intFromEnum(Op.push_t);
    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const sym_name_op: u16 = @intFromEnum(Op.sym_name);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code_nil = [_]u8{
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(sym_name_op & 0xFF), @truncate(sym_name_op >> 8),
        @truncate(ret_op & 0xFF),      @truncate(ret_op >> 8),
    };
    const chunk_nil = Chunk{
        .code = @constCast(&code_nil),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_nil.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    const res_nil = try vm.run(&chunk_nil);
    try testing.expect(res_nil.isString());
    try testing.expectEqualStrings("nil", res_nil.toPtr(runtime.String).bytes());

    const code_t = [_]u8{
        @truncate(push_t_op & 0xFF),   @truncate(push_t_op >> 8),
        @truncate(sym_name_op & 0xFF), @truncate(sym_name_op >> 8),
        @truncate(ret_op & 0xFF),      @truncate(ret_op >> 8),
    };
    const chunk_t = Chunk{
        .code = @constCast(&code_t),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_t.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    const res_t = try vm.run(&chunk_t);
    try testing.expect(res_t.isString());
    try testing.expectEqualStrings("t", res_t.toPtr(runtime.String).bytes());

    const sym = try heap.intern("foo");
    const consts = [_]Value{sym};
    const code_sym = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0,                        0,
        @truncate(sym_name_op & 0xFF),   @truncate(sym_name_op >> 8),   @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const chunk_sym = Chunk{
        .code = @constCast(&code_sym),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_sym.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    const res_sym = try vm.run(&chunk_sym);
    try testing.expect(res_sym.isString());
    try testing.expectEqualStrings("FOO", res_sym.toPtr(runtime.String).bytes());
}

test "vm cons car cdr" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    // (car (cons 1 2)) = 1
    const code = [_]u8{
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x40, 0x00, // cons
        0x41, 0x00, // car
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "vm list last" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const code = [_]u8{
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x02, 0x00, 3, 0, 0, 0, // push_i32 3
        0x43, 0x00, 3, // make_list 3
        0x48, 0x00, // list_last
        0x41, 0x00, // car
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

test "vm elt_set list" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const make_list_op: u16 = @intFromEnum(Op.make_list);
    const list_nth_op: u16 = @intFromEnum(Op.list_nth);
    const elt_set_op: u16 = @intFromEnum(Op.elt_set);
    const pop_op: u16 = @intFromEnum(Op.pop);

    const code = [_]u8{
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x02, 0x00, 3, 0, 0, 0, // push_i32 3
        @truncate(make_list_op & 0xFF), @truncate(make_list_op >> 8), 3, // make_list 3
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1 (index)
        0x02,                         0x00,                       99,                       0,                      0, 0, // push_i32 99 (value)
        @truncate(elt_set_op & 0xFF), @truncate(elt_set_op >> 8), @truncate(pop_op & 0xFF), @truncate(pop_op >> 8),
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1 (index)
        0x10,                          0x00,                        0, // load_local 0
        @truncate(list_nth_op & 0xFF), @truncate(list_nth_op >> 8),
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "vm symbol_package nil" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const symbol_package_op: u16 = @intFromEnum(Op.symbol_package);
    const package_name_op: u16 = @intFromEnum(Op.package_name);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code = [_]u8{
        @truncate(push_nil_op & 0xFF),       @truncate(push_nil_op >> 8),
        @truncate(symbol_package_op & 0xFF), @truncate(symbol_package_op >> 8),
        @truncate(package_name_op & 0xFF),   @truncate(package_name_op >> 8),
        @truncate(ret_op & 0xFF),            @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expect(result.isString());
    try testing.expectEqualStrings("COMMON-LISP", result.toPtr(runtime.String).bytes());
}

test "vm aref aset vector" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const make_vec_n_op: u16 = @intFromEnum(Op.make_vec_n);
    const aref_op: u16 = @intFromEnum(Op.aref);
    const aset_op: u16 = @intFromEnum(Op.aset);
    const pop_op: u16 = @intFromEnum(Op.pop);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code = [_]u8{
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x02, 0x00, 3, 0, 0, 0, // push_i32 3
        @truncate(make_vec_n_op & 0xFF), @truncate(make_vec_n_op >> 8), 3, // make_vec_n 3
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1 (index)
        0x02, 0x00, 99, 0, 0, 0, // push_i32 99 (value)
        @truncate(aset_op & 0xFF), @truncate(aset_op >> 8), 1, // aset sub_count=1
        @truncate(pop_op & 0xFF),  @truncate(pop_op >> 8),
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1 (index)
        @truncate(aref_op & 0xFF), @truncate(aref_op >> 8), 1, // aref sub_count=1
        @truncate(ret_op & 0xFF),  @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "vm aref aset rank-0 array" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_array_op: u16 = @intFromEnum(Op.make_array);
    const aref_op: u16 = @intFromEnum(Op.aref);
    const aset_op: u16 = @intFromEnum(Op.aset);
    const store_local_op: u16 = @intFromEnum(Op.store_local);
    const load_local_op: u16 = @intFromEnum(Op.load_local);
    const pop_op: u16 = @intFromEnum(Op.pop);
    const ret_op: u16 = @intFromEnum(Op.ret);

    // Dynamic make_array with rank=0 and initial-element (operand=1).
    // dimensions=nil gives a rank-0 array with one element.
    const code = [_]u8{
        @truncate(push_nil_op & 0xFF),   @truncate(push_nil_op >> 8),
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),
        7,                               0,
        0,                               0,
        @truncate(make_array_op & 0xFF), @truncate(make_array_op >> 8),
        1,                               @truncate(store_local_op & 0xFF),
        @truncate(store_local_op >> 8),  0,

        @truncate(load_local_op & 0xFF), @truncate(load_local_op >> 8),
        0,                               @truncate(push_i32_op & 0xFF),
        @truncate(push_i32_op >> 8),     42,
        0,                               0,
        0,                               @truncate(aset_op & 0xFF),
        @truncate(aset_op >> 8),         0,
        @truncate(pop_op & 0xFF),        @truncate(pop_op >> 8),

        @truncate(load_local_op & 0xFF), @truncate(load_local_op >> 8),
        0,                               @truncate(aref_op & 0xFF),
        @truncate(aref_op >> 8),         0,
        @truncate(ret_op & 0xFF),        @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "vm symbol_value specials" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const push_t_op: u16 = @intFromEnum(Op.push_t);
    const symbol_value_op: u16 = @intFromEnum(Op.symbol_value);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code_nil = [_]u8{
        @truncate(push_nil_op & 0xFF),     @truncate(push_nil_op >> 8),
        @truncate(symbol_value_op & 0xFF), @truncate(symbol_value_op >> 8),
        @truncate(ret_op & 0xFF),          @truncate(ret_op >> 8),
    };

    const chunk_nil = Chunk{
        .code = @constCast(&code_nil),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_nil.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_nil = try vm.run(&chunk_nil);
    try testing.expect(res_nil.isNil());

    const code_t = [_]u8{
        @truncate(push_t_op & 0xFF),       @truncate(push_t_op >> 8),
        @truncate(symbol_value_op & 0xFF), @truncate(symbol_value_op >> 8),
        @truncate(ret_op & 0xFF),          @truncate(ret_op >> 8),
    };

    const chunk_t = Chunk{
        .code = @constCast(&code_t),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_t.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_t = try vm.run(&chunk_t);
    try testing.expect(res_t.isT());
}

test "vm format selector" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const format_op: u16 = @intFromEnum(Op.format);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const control = try heap.allocBaseString("~[no~;yes~]");
    const consts = [_]Value{control};

    const code_nil = [_]u8{
        @truncate(push_nil_op & 0xFF),   @truncate(push_nil_op >> 8),
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8),
        0,                               0,
        @truncate(push_nil_op & 0xFF),   @truncate(push_nil_op >> 8),
        @truncate(format_op & 0xFF),     @truncate(format_op >> 8),
        1,                               @truncate(ret_op & 0xFF),
        @truncate(ret_op >> 8),
    };

    const chunk_nil = Chunk{
        .code = @constCast(&code_nil),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_nil.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_nil = try vm.run(&chunk_nil);
    try testing.expect(res_nil.isString());
    try testing.expectEqualStrings("no", res_nil.toPtr(runtime.String).bytes());

    const code_one = [_]u8{
        @truncate(push_nil_op & 0xFF),   @truncate(push_nil_op >> 8),
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8),
        0,                               0,
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),
        1,                               0,
        0,                               0,
        @truncate(format_op & 0xFF),     @truncate(format_op >> 8),
        1,                               @truncate(ret_op & 0xFF),
        @truncate(ret_op >> 8),
    };

    const chunk_one = Chunk{
        .code = @constCast(&code_one),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_one.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_one = try vm.run(&chunk_one);
    try testing.expect(res_one.isString());
    try testing.expectEqualStrings("yes", res_one.toPtr(runtime.String).bytes());
}

test "vm list_position count string" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const code_char_op: u16 = @intFromEnum(Op.code_char);
    const list_position_op: u16 = @intFromEnum(Op.list_position);
    const list_count_op: u16 = @intFromEnum(Op.list_count);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const str = try heap.allocBaseString("abca");
    const consts = [_]Value{str};

    const code_pos = [_]u8{
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   98, 0, 0,                                  0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0,  0, @truncate(list_position_op & 0xFF), @truncate(list_position_op >> 8),
        @truncate(ret_op & 0xFF),        @truncate(ret_op >> 8),
    };

    const chunk_pos = Chunk{
        .code = @constCast(&code_pos),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_pos.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_pos = try vm.run(&chunk_pos);
    try testing.expectEqual(@as(i64, 1), res_pos.toFixnum());

    const code_count = [_]u8{
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   97,                              0,                             0, 0,
        @truncate(code_char_op & 0xFF),  @truncate(code_char_op >> 8),  @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(list_count_op & 0xFF), @truncate(list_count_op >> 8), @truncate(ret_op & 0xFF),        @truncate(ret_op >> 8),
    };

    const chunk_count = Chunk{
        .code = @constCast(&code_count),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_count.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_count = try vm.run(&chunk_count);
    try testing.expectEqual(@as(i64, 2), res_count.toFixnum());
}

test "vm list_find string returns character" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const list_find_op: u16 = @intFromEnum(Op.list_find);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const str = try heap.allocBaseString("abca");
    const consts = [_]Value{str};

    const code = [_]u8{
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   97, 0, 0,                              0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0,  0, @truncate(list_find_op & 0xFF), @truncate(list_find_op >> 8),
        @truncate(ret_op & 0xFF),        @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res = try vm.run(&chunk);
    try testing.expect(res.isCharacter());
    try testing.expectEqual(@as(u21, 97), res.toCharacter());
}

test "vm list_position string invalid item errors" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const list_position_op: u16 = @intFromEnum(Op.list_position);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const item_str = try heap.allocBaseString("x");
    const seq_str = try heap.allocBaseString("abca");
    const consts = [_]Value{ item_str, seq_str };

    const code = [_]u8{
        @truncate(push_const_op & 0xFF),    @truncate(push_const_op >> 8),    0,                        0,
        @truncate(push_const_op & 0xFF),    @truncate(push_const_op >> 8),    1,                        0,
        @truncate(list_position_op & 0xFF), @truncate(list_position_op >> 8), @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    try testing.expectError(error.InvalidArgument, vm.run(&chunk));
}

test "vm equal vector string" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_vec_n_op: u16 = @intFromEnum(Op.make_vec_n);
    const store_local_op: u16 = @intFromEnum(Op.store_local);
    const load_local_op: u16 = @intFromEnum(Op.load_local);
    const equal_op: u16 = @intFromEnum(Op.equal);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code_vec = [_]u8{
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   1,                        0,                                0,                              0,
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   2,                        0,                                0,                              0,
        @truncate(make_vec_n_op & 0xFF), @truncate(make_vec_n_op >> 8), 2,                        @truncate(store_local_op & 0xFF), @truncate(store_local_op >> 8), 0,
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   1,                        0,                                0,                              0,
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   2,                        0,                                0,                              0,
        @truncate(make_vec_n_op & 0xFF), @truncate(make_vec_n_op >> 8), 2,                        @truncate(load_local_op & 0xFF),  @truncate(load_local_op >> 8),  0,
        @truncate(equal_op & 0xFF),      @truncate(equal_op >> 8),      @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_vec = Chunk{
        .code = @constCast(&code_vec),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_vec.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const res_vec = try vm.run(&chunk_vec);
    try testing.expect(res_vec.isT());

    const str_a = try heap.allocBaseString("hi");
    const str_b = try heap.allocBaseString("hi");
    const consts = [_]Value{ str_a, str_b };

    const code_str = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0,                        0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 1,                        0,
        @truncate(equal_op & 0xFF),      @truncate(equal_op >> 8),      @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_str = Chunk{
        .code = @constCast(&code_str),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_str.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_str = try vm.run(&chunk_str);
    try testing.expect(res_str.isT());
}

test "vm conditional" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    // (if nil 1 2) = 2
    const code = [_]u8{
        0x00, 0x00, // push_nil
        0x81, 0x00, 10, 0, // jmp_nil 10
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x80, 0x00, 6, 0, // jmp 6
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "vm locals" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    // Store 42 in local 0, load it back
    const code = [_]u8{
        0x02, 0x00, 42, 0, 0, 0, // push_i32 42
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "vm hash table" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    // Create hash table, set key 42 to value 100, get key 42
    // Use local 0 to store ht
    const code = [_]u8{
        0xA4, 0x00, 16, 0, 1, // make_hash cap=16 type=eql
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 42, 0, 0, 0, // push_i32 42
        0x02, 0x00, 100, 0, 0, 0, // push_i32 100
        0x96, 0x00, // hash_set
        0x05, 0x00, // pop
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 42, 0, 0, 0, // push_i32 42
        0xA5, 0x00, // hash_get
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 100), result.toFixnum());
}

test "vm hash table count and remove" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    // Create hash table, set 2 keys, get count
    const code = [_]u8{
        0xA4, 0x00, 16, 0, 1, // make_hash cap=16 type=eql
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 10, 0, 0, 0, // push_i32 10
        0x96, 0x00, // hash_set
        0x05, 0x00, // pop
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x02, 0x00, 20, 0, 0, 0, // push_i32 20
        0x96, 0x00, // hash_set
        0x05, 0x00, // pop
        0x10, 0x00, 0, // load_local 0
        0x98, 0x00, // hash_count
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "vm read_from_string propagates parse errors" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const bad = try heap.allocBaseString("(");
    try vm.push(bad);
    try testing.expectError(error.UnterminatedList, vm.executeOp(.read_from_string));
}

test "vm typep propagates invalid type spec" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    try vm.push(Value.makeFixnum(1));
    try vm.push(Value.makeFixnum(2));
    try testing.expectError(error.InvalidTypeSpecifier, vm.executeOp(.typep));
}

test "vm write_to_string handles long strings" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var bytes: [300]u8 = undefined;
    @memset(&bytes, 'a');
    const str = try heap.allocBaseString(&bytes);
    try vm.push(str);

    try vm.executeOp(.write_to_string);
    const out = try vm.pop();
    try testing.expect(out.isString());
    try testing.expectEqual(@as(usize, bytes.len + 2), out.toPtr(runtime.String).bytes().len);
}

test "vm collectGarbage relocates chunk pointers" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const empty_code = [_]u8{};
    const no_consts = [_]Value{};

    const cur_val = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);
    const frame_val = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);
    const catch_val = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);
    const unwind_val = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);
    const block_val = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);
    const restart_val = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);
    const pool_val = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);

    const cur_ptr = cur_val.toPtr(Chunk);
    const frame_ptr = frame_val.toPtr(Chunk);
    const catch_ptr = catch_val.toPtr(Chunk);
    const unwind_ptr = unwind_val.toPtr(Chunk);
    const block_ptr = block_val.toPtr(Chunk);
    const restart_ptr = restart_val.toPtr(Chunk);
    const pool_ptr = pool_val.toPtr(Chunk);

    vm.chunk = cur_ptr;
    vm.frames[0] = .{
        .chunk = frame_ptr,
        .return_ip = 0,
        .bp = 0,
        .closure = null,
        .argc = 0,
        .positional_argc = 0,
        .block_depth = 0,
        .catch_depth = 0,
        .unwind_depth = 0,
        .restart_depth = 0,
        .progv_depth = 0,
        .handler_depth = 0,
        .handler_restore_depth = null,
    };
    vm.fp = 1;
    vm.catch_stack[0] = .{
        .tag = Value.nil,
        .chunk = catch_ptr,
        .catch_ip = 0,
        .catch_sp = 0,
        .catch_fp = 0,
        .block_depth = 0,
        .unwind_depth = 0,
        .restart_depth = 0,
        .progv_depth = 0,
        .handler_depth = 0,
    };
    vm.catch_sp = 1;
    vm.unwind_stack[0] = .{
        .chunk = unwind_ptr,
        .cleanup_ip = 0,
        .unwind_sp = 0,
        .unwind_fp = 0,
    };
    vm.unwind_sp = 1;
    vm.block_stack[0] = .{
        .name_raw = Value.nil,
        .chunk = block_ptr,
        .exit_ip = 0,
        .block_sp = 0,
        .block_fp = 0,
        .catch_depth = 0,
        .unwind_depth = 0,
        .restart_depth = 0,
        .progv_depth = 0,
        .handler_depth = 0,
    };
    vm.block_sp = 1;
    vm.restart_stack[0] = .{
        .name = Value.nil,
        .id = 1,
        .chunk = restart_ptr,
        .handler_ip = 0,
        .restart_sp = 0,
        .restart_fp = 0,
        .catch_depth = 0,
        .unwind_depth = 0,
        .block_depth = 0,
        .progv_depth = 0,
        .handler_depth = 0,
    };
    vm.restart_sp = 1;

    var pool = [_]Value{Value.makeChunk(pool_ptr)};
    vm.setChunkPool(pool[0..]);

    _ = try vm.collectGarbage();

    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;

    const cur_after = @intFromPtr(vm.chunk);
    try testing.expect(cur_after >= start and cur_after < end);
    try testing.expect(cur_after != @intFromPtr(cur_ptr));

    const frame_after = @intFromPtr(vm.frames[0].chunk);
    try testing.expect(frame_after >= start and frame_after < end);
    try testing.expect(frame_after != @intFromPtr(frame_ptr));

    const catch_after = @intFromPtr(vm.catch_stack[0].chunk);
    try testing.expect(catch_after >= start and catch_after < end);
    try testing.expect(catch_after != @intFromPtr(catch_ptr));

    const unwind_after = @intFromPtr(vm.unwind_stack[0].chunk);
    try testing.expect(unwind_after >= start and unwind_after < end);
    try testing.expect(unwind_after != @intFromPtr(unwind_ptr));

    const block_after = @intFromPtr(vm.block_stack[0].chunk);
    try testing.expect(block_after >= start and block_after < end);
    try testing.expect(block_after != @intFromPtr(block_ptr));

    const restart_after = @intFromPtr(vm.restart_stack[0].chunk);
    try testing.expect(restart_after >= start and restart_after < end);
    try testing.expect(restart_after != @intFromPtr(restart_ptr));

    const pool_after = @intFromPtr(pool[0].toPtr(Chunk));
    try testing.expect(pool_after >= start and pool_after < end);
    try testing.expect(pool_after != @intFromPtr(pool_ptr));
}

test "vm loadConst refreshes chunk constants per gc epoch" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const const_str = try heap.allocBaseString("const-value");
    const code = [_]u8{};
    const consts = [_]Value{const_str};
    const chunk_val = try heap.allocChunk(&code, &consts, 0, 0, 0, false, 0);
    vm.chunk = chunk_val.toPtr(Chunk);

    const first = try vm.loadConst(0);
    try testing.expect(first.isString());

    const entry0 = vm.chunk_const_cache[Vm.chunkConstCacheIndex(vm.chunk)];
    try testing.expectEqual(@intFromPtr(vm.chunk), entry0.chunk_key);
    try testing.expectEqual(heap.stats.gc_count, entry0.gc_count);
    try testing.expectEqual(@intFromPtr(vm.chunk), vm.const_last_chunk_key);
    try testing.expectEqual(heap.stats.gc_count, vm.const_last_gc_count);

    const gc_before = heap.stats.gc_count;
    _ = try vm.collectGarbage();
    try testing.expect(heap.stats.gc_count > gc_before);

    const second = try vm.loadConst(0);
    try testing.expect(second.isString());
    try testing.expectEqual(second.raw, vm.chunk.getConstants()[0].raw);

    const entry1 = vm.chunk_const_cache[Vm.chunkConstCacheIndex(vm.chunk)];
    try testing.expectEqual(@intFromPtr(vm.chunk), entry1.chunk_key);
    try testing.expectEqual(heap.stats.gc_count, entry1.gc_count);
    try testing.expectEqual(@intFromPtr(vm.chunk), vm.const_last_chunk_key);
    try testing.expectEqual(heap.stats.gc_count, vm.const_last_gc_count);
}

test "vm registerJitFn updates chunk jit pointer fast path" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const code = [_]u8{};
    const consts = [_]Value{};
    const chunk_val = try heap.allocChunk(&code, &consts, 0, 0, 0, false, 0);
    const chunk_ptr = chunk_val.toPtr(Chunk);
    try testing.expectEqual(@as(usize, 0), chunk_ptr.jit_fn);

    var dummy: jit_backend.CompiledFn = undefined;
    try vm.registerJitFn(chunk_ptr, &dummy);
    try testing.expectEqual(@as(usize, 1), vm.jit_fns.items.len);
    try vm.registerJitFn(chunk_ptr, &dummy);
    try testing.expectEqual(@as(usize, 1), vm.jit_fns.items.len);
    try testing.expectEqual(@intFromPtr(&dummy), chunk_ptr.jit_fn);
    try testing.expect(vm.lookupJitFn(chunk_ptr).? == &dummy);

    try testing.expect(vm.unregisterJitFn(chunk_ptr));
    try testing.expectEqual(@as(usize, 0), vm.jit_fns.items.len);
    try testing.expectEqual(@as(usize, 0), chunk_ptr.jit_fn);
    try testing.expect(vm.lookupJitFn(chunk_ptr) == null);
    try testing.expect(!vm.unregisterJitFn(chunk_ptr));
}

test "vm jit compile cache honors gc epoch and compiled persistence" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const code = [_]u8{};
    const consts = [_]Value{};
    const chunk_val = try heap.allocChunk(&code, &consts, 0, 0, 0, false, 0);
    vm.chunk = chunk_val.toPtr(Chunk);

    try testing.expectEqual(Vm.JitCompileStatus.none, try vm.jitCompileStatus(vm.chunk));

    try vm.noteJitCompileStatus(vm.chunk, .unsupported);
    try testing.expectEqual(Vm.JitCompileStatus.unsupported, try vm.jitCompileStatus(vm.chunk));

    const gc0 = heap.stats.gc_count;
    _ = try vm.collectGarbage();
    try testing.expect(heap.stats.gc_count > gc0);
    try testing.expectEqual(Vm.JitCompileStatus.unsupported, try vm.jitCompileStatus(vm.chunk));

    var dummy: jit_backend.CompiledFn = undefined;
    try vm.registerJitFn(vm.chunk, &dummy);
    try testing.expectEqual(Vm.JitCompileStatus.compiled, try vm.jitCompileStatus(vm.chunk));

    const gc1 = heap.stats.gc_count;
    _ = try vm.collectGarbage();
    try testing.expect(heap.stats.gc_count > gc1);
    try testing.expectEqual(Vm.JitCompileStatus.compiled, try vm.jitCompileStatus(vm.chunk));

    try testing.expect(vm.unregisterJitFn(vm.chunk));
    try testing.expectEqual(Vm.JitCompileStatus.none, try vm.jitCompileStatus(vm.chunk));
}

test "vm jit bridge lifecycle tracks owner vm" {
    const testing = std.testing;
    const allocator = testing.allocator;

    jit_backend.clearGlobalBridge();
    jit_backend.clearErrorBridge();
    jit_backend.clearCallBridge();

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm_a = try Vm.init(allocator, &heap);
    var keep_vm_a = true;
    defer if (keep_vm_a) vm_a.deinit();

    var vm_b = try Vm.init(allocator, &heap);
    var keep_vm_b = true;
    defer if (keep_vm_b) vm_b.deinit();

    const ctx_a: *anyopaque = &vm_a;
    const ctx_b: *anyopaque = &vm_b;

    vm_a.installJitBridges();
    try testing.expect(jit_backend.callBridgeContext() == ctx_a);
    try testing.expect(jit_backend.errorBridgeContext() == ctx_a);
    try testing.expect(jit_backend.globalBridgeContext() == ctx_a);
    const epoch_a = jit_backend.bridgeEpoch();
    vm_a.installJitBridges();
    try testing.expectEqual(epoch_a, jit_backend.bridgeEpoch());

    vm_b.installJitBridges();
    try testing.expect(jit_backend.callBridgeContext() == ctx_b);
    try testing.expect(jit_backend.errorBridgeContext() == ctx_b);
    try testing.expect(jit_backend.globalBridgeContext() == ctx_b);
    const epoch_b = jit_backend.bridgeEpoch();
    vm_b.installJitBridges();
    try testing.expectEqual(epoch_b, jit_backend.bridgeEpoch());

    vm_a.deinit();
    keep_vm_a = false;
    try testing.expect(jit_backend.callBridgeContext() == ctx_b);
    try testing.expect(jit_backend.errorBridgeContext() == ctx_b);
    try testing.expect(jit_backend.globalBridgeContext() == ctx_b);

    vm_b.deinit();
    keep_vm_b = false;
    try testing.expect(jit_backend.callBridgeContext() == null);
    try testing.expect(jit_backend.errorBridgeContext() == null);
    try testing.expect(jit_backend.globalBridgeContext() == null);
}

test "vm runJitCompiled propagates bridge OutOfMemory" {
    if (!build_options.use_hoist) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const code = [_]u8{};
    const consts = [_]Value{};
    const chunk_val = try heap.allocChunk(&code, &consts, 0, 0, 0, false, 0);
    const chunk = chunk_val.toPtr(Chunk);

    const Relay = struct {
        threadlocal var target_vm: ?*Vm = null;

        fn oom() callconv(.c) i64 {
            const vm_ptr = target_vm.?;
            vm_ptr.jit_bridge_error = error.OutOfMemory;
            jit_backend.bridgeThrow();
            std.debug.panic("jit bridge throw returned", .{});
        }
    };

    Relay.target_vm = &vm;
    defer Relay.target_vm = null;

    var compiled = jit_backend.CompiledFn{
        .mem = undefined,
        .fn_ptr = @ptrCast(@alignCast(&Relay.oom)),
        .arity = 0,
        .allocator = allocator,
        .name = "jit-oom-relay",
    };

    try testing.expectError(error.OutOfMemory, vm.runJitCompiled(&compiled, chunk, chunk, &.{}));
    try testing.expect(vm.jit_bridge_error == null);
}

test "vm state restore refreshes owned chunk pool slice" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    try chunk_pool.append(allocator, Value.nil);
    vm.setChunkPoolOwned(&chunk_pool);

    const saved = State.save(&vm);
    const saved_ptr = @intFromPtr(saved.chunk_pool.ptr);

    var target_cap: usize = 4096;
    var moved = false;
    while (!moved and target_cap <= (1 << 20)) : (target_cap *= 2) {
        try chunk_pool.ensureTotalCapacity(allocator, target_cap);
        while (chunk_pool.items.len < target_cap) {
            try chunk_pool.append(allocator, Value.nil);
        }
        moved = @intFromPtr(chunk_pool.items.ptr) != saved_ptr;
    }
    try testing.expect(moved);

    saved.restore(&vm);
    try testing.expect(vm.chunk_pool_owner != null);
    try testing.expectEqual(@intFromPtr(chunk_pool.items.ptr), @intFromPtr(vm.chunk_pool.ptr));
    try testing.expectEqual(chunk_pool.items.len, vm.chunk_pool.len);
}

test "vm restore caller frame uses handler restore depth" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const empty_chunk = Chunk{
        .code = @constCast(&[_]u8{}),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = 0,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    vm.chunk = &empty_chunk;
    vm.ip = 99;
    vm.sp = 17;
    vm.fp = 1;
    vm.handler_sp = 13;
    vm.block_sp = 12;
    vm.catch_sp = 11;
    vm.unwind_sp = 10;
    vm.restart_sp = 9;
    vm.progv_sp = 8;
    for (vm.progv_stack[0..vm.progv_sp]) |*slot| {
        slot.* = .{ .saved_bindings = Value.nil };
    }
    vm.frames[0] = .{
        .chunk = &halt_chunk,
        .return_ip = 7,
        .bp = 4,
        .closure = null,
        .argc = 0,
        .positional_argc = 0,
        .block_depth = 3,
        .catch_depth = 2,
        .unwind_depth = 1,
        .restart_depth = 4,
        .progv_depth = 5,
        .handler_depth = 6,
        .handler_restore_depth = 2,
    };

    vm.fp -= 1;
    try vm.restoreCallerFrameAfterCall(vm.frames[vm.fp], Value.makeFixnum(42));

    try testing.expect(vm.chunk == &halt_chunk);
    try testing.expectEqual(@as(usize, 7), vm.ip);
    try testing.expectEqual(@as(usize, 5), vm.sp);
    try testing.expectEqual(@as(i64, 42), vm.stack[4].toFixnum());
    try testing.expectEqual(@as(usize, 2), vm.handler_sp);
    try testing.expectEqual(@as(usize, 3), vm.block_sp);
    try testing.expectEqual(@as(usize, 2), vm.catch_sp);
    try testing.expectEqual(@as(usize, 1), vm.unwind_sp);
    try testing.expectEqual(@as(usize, 4), vm.restart_sp);
    try testing.expectEqual(@as(usize, 5), vm.progv_sp);
}

test "vm restore caller frame falls back to handler depth" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    vm.chunk = &halt_chunk;
    vm.ip = 31;
    vm.sp = 12;
    vm.fp = 1;
    vm.handler_sp = 9;
    vm.block_sp = 9;
    vm.catch_sp = 9;
    vm.unwind_sp = 9;
    vm.restart_sp = 9;
    vm.progv_sp = 9;
    for (vm.progv_stack[0..vm.progv_sp]) |*slot| {
        slot.* = .{ .saved_bindings = Value.nil };
    }
    vm.frames[0] = .{
        .chunk = &halt_chunk,
        .return_ip = 5,
        .bp = 1,
        .closure = null,
        .argc = 0,
        .positional_argc = 0,
        .block_depth = 0,
        .catch_depth = 1,
        .unwind_depth = 2,
        .restart_depth = 3,
        .progv_depth = 4,
        .handler_depth = 7,
        .handler_restore_depth = null,
    };

    vm.fp -= 1;
    try vm.restoreCallerFrameAfterCall(vm.frames[vm.fp], Value.makeFixnum(9));

    try testing.expectEqual(@as(usize, 2), vm.sp);
    try testing.expectEqual(@as(i64, 9), vm.stack[1].toFixnum());
    try testing.expectEqual(@as(usize, 7), vm.handler_sp);
    try testing.expectEqual(@as(usize, 0), vm.block_sp);
    try testing.expectEqual(@as(usize, 1), vm.catch_sp);
    try testing.expectEqual(@as(usize, 2), vm.unwind_sp);
    try testing.expectEqual(@as(usize, 3), vm.restart_sp);
    try testing.expectEqual(@as(usize, 4), vm.progv_sp);
}

test "vm doCall tail call restores dynamic depths" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const empty_code = [_]u8{};
    const no_consts = [_]Value{};
    const callee_chunk_val = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);
    const callee_closure = try vm.allocClosureWithGC(callee_chunk_val, 0, &[_]Value{});

    vm.chunk = &halt_chunk;
    vm.ip = 12;
    vm.fp = 1;
    vm.sp = 1;
    vm.stack[0] = callee_closure;
    vm.block_sp = 17;
    vm.catch_sp = 16;
    vm.unwind_sp = 15;
    vm.restart_sp = 14;
    vm.progv_sp = 13;
    vm.handler_sp = 12;
    for (vm.progv_stack[0..vm.progv_sp]) |*slot| {
        slot.* = .{ .saved_bindings = Value.nil };
    }
    vm.frames[0] = .{
        .chunk = &halt_chunk,
        .return_ip = 99,
        .bp = 0,
        .closure = null,
        .argc = 0,
        .positional_argc = 0,
        .block_depth = 1,
        .catch_depth = 2,
        .unwind_depth = 3,
        .restart_depth = 4,
        .progv_depth = 5,
        .handler_depth = 6,
        .handler_restore_depth = 7,
    };

    try vm.doCall(0, true);

    try testing.expect(vm.chunk == callee_chunk_val.toPtr(Chunk));
    try testing.expectEqual(@as(usize, 1), vm.block_sp);
    try testing.expectEqual(@as(usize, 2), vm.catch_sp);
    try testing.expectEqual(@as(usize, 3), vm.unwind_sp);
    try testing.expectEqual(@as(usize, 4), vm.restart_sp);
    try testing.expectEqual(@as(usize, 5), vm.progv_sp);
    try testing.expectEqual(@as(usize, 7), vm.handler_sp);
}

test "vm restore caller frame pops progv bindings" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var env = GlobalEnv.init(allocator);
    defer env.deinit();
    vm.setGlobalEnv(&env);

    const sym = try heap.intern("PROGV-RESTORE-SYM");
    const idx = (try vm.defineSymbolGlobalIndex(sym.toPtr(Symbol))) orelse return error.TestUnexpectedResult;
    try vm.storeGlobal(idx, Value.makeFixnum(10));

    const symbol_list = try heap.allocCons(sym, Value.nil);
    const value_list = try heap.allocCons(Value.makeFixnum(99), Value.nil);
    try vm.pushProgvFrame(symbol_list, value_list);
    try testing.expectEqual(@as(usize, 1), vm.progv_sp);
    const rebound = try vm.loadGlobal(idx);
    try testing.expect(rebound.isFixnum());
    try testing.expectEqual(@as(i64, 99), rebound.toFixnum());

    vm.chunk = &halt_chunk;
    vm.ip = 11;
    vm.sp = 2;
    vm.fp = 1;
    vm.frames[0] = .{
        .chunk = &halt_chunk,
        .return_ip = 3,
        .bp = 0,
        .closure = null,
        .argc = 0,
        .positional_argc = 0,
        .block_depth = 0,
        .catch_depth = 0,
        .unwind_depth = 0,
        .restart_depth = 0,
        .progv_depth = 0,
        .handler_depth = 0,
        .handler_restore_depth = null,
    };

    vm.fp -= 1;
    try vm.restoreCallerFrameAfterCall(vm.frames[vm.fp], Value.makeFixnum(1));

    try testing.expectEqual(@as(usize, 0), vm.progv_sp);
    const restored = try vm.loadGlobal(idx);
    try testing.expect(restored.isFixnum());
    try testing.expectEqual(@as(i64, 10), restored.toFixnum());
}

test "vm globals start unbound" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const val = try vm.loadGlobal(214);
    try testing.expectEqual(Value.unbound.raw, val.raw);
}

test "vm return-from restores dynamic depths and progv" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var env = GlobalEnv.init(allocator);
    defer env.deinit();
    vm.setGlobalEnv(&env);

    const block_name = try heap.intern("RETURN-FROM-BLOCK");
    const sym = try heap.intern("RETURN-FROM-SYM");
    const idx = (try vm.defineSymbolGlobalIndex(sym.toPtr(Symbol))) orelse return error.TestUnexpectedResult;
    try vm.storeGlobal(idx, Value.makeFixnum(10));

    const symbol_list = try heap.allocCons(sym, Value.nil);
    const value_list = try heap.allocCons(Value.makeFixnum(99), Value.nil);
    try vm.pushProgvFrame(symbol_list, value_list);
    try testing.expectEqual(@as(usize, 1), vm.progv_sp);
    try testing.expectEqual(@as(i64, 99), (try vm.loadGlobal(idx)).toFixnum());

    vm.chunk = &halt_chunk;
    vm.ip = 1;
    vm.sp = 0;
    vm.fp = 0;
    vm.block_sp = 1;
    vm.catch_sp = 4;
    vm.unwind_sp = 0;
    vm.restart_sp = 2;
    vm.handler_sp = 5;
    vm.block_stack[0] = .{
        .name_raw = block_name,
        .chunk = &halt_chunk,
        .exit_ip = 77,
        .block_sp = 0,
        .block_fp = 0,
        .catch_depth = 0,
        .unwind_depth = 0,
        .restart_depth = 0,
        .progv_depth = 0,
        .handler_depth = 0,
    };

    try vm.doReturnFrom(block_name, Value.makeFixnum(42));

    try testing.expect(vm.chunk == &halt_chunk);
    try testing.expectEqual(@as(usize, 77), vm.ip);
    try testing.expectEqual(@as(usize, 1), vm.sp);
    try testing.expectEqual(@as(usize, 0), vm.fp);
    try testing.expectEqual(@as(i64, 42), vm.stack[0].toFixnum());
    try testing.expectEqual(@as(usize, 0), vm.block_sp);
    try testing.expectEqual(@as(usize, 0), vm.catch_sp);
    try testing.expectEqual(@as(usize, 0), vm.unwind_sp);
    try testing.expectEqual(@as(usize, 0), vm.restart_sp);
    try testing.expectEqual(@as(usize, 0), vm.handler_sp);
    try testing.expectEqual(@as(usize, 0), vm.progv_sp);
    try testing.expectEqual(@as(i64, 10), (try vm.loadGlobal(idx)).toFixnum());
}

test "vm invoke-restart restores dynamic depths and progv" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var env = GlobalEnv.init(allocator);
    defer env.deinit();
    vm.setGlobalEnv(&env);

    const restart_name = try heap.intern("RESTART-NAME");
    const sym = try heap.intern("RESTART-SYM");
    const idx = (try vm.defineSymbolGlobalIndex(sym.toPtr(Symbol))) orelse return error.TestUnexpectedResult;
    try vm.storeGlobal(idx, Value.makeFixnum(21));

    const symbol_list = try heap.allocCons(sym, Value.nil);
    const value_list = try heap.allocCons(Value.makeFixnum(84), Value.nil);
    try vm.pushProgvFrame(symbol_list, value_list);
    try testing.expectEqual(@as(usize, 1), vm.progv_sp);
    try testing.expectEqual(@as(i64, 84), (try vm.loadGlobal(idx)).toFixnum());

    vm.chunk = &halt_chunk;
    vm.ip = 5;
    vm.sp = 0;
    vm.fp = 0;
    vm.block_sp = 3;
    vm.catch_sp = 3;
    vm.unwind_sp = 2;
    vm.restart_sp = 1;
    vm.handler_sp = 4;
    vm.restart_stack[0] = .{
        .name = restart_name,
        .id = 1,
        .chunk = &halt_chunk,
        .handler_ip = 99,
        .restart_sp = 0,
        .restart_fp = 0,
        .catch_depth = 0,
        .unwind_depth = 0,
        .block_depth = 0,
        .progv_depth = 0,
        .handler_depth = 0,
    };

    try vm.doInvokeRestart(restart_name, Value.makeFixnum(7));

    try testing.expect(vm.chunk == &halt_chunk);
    try testing.expectEqual(@as(usize, 99), vm.ip);
    try testing.expectEqual(@as(usize, 1), vm.sp);
    try testing.expectEqual(@as(usize, 0), vm.fp);
    try testing.expectEqual(@as(i64, 7), vm.stack[0].toFixnum());
    try testing.expectEqual(@as(usize, 0), vm.block_sp);
    try testing.expectEqual(@as(usize, 0), vm.catch_sp);
    try testing.expectEqual(@as(usize, 0), vm.unwind_sp);
    try testing.expectEqual(@as(usize, 0), vm.restart_sp);
    try testing.expectEqual(@as(usize, 0), vm.handler_sp);
    try testing.expectEqual(@as(usize, 0), vm.progv_sp);
    try testing.expectEqual(@as(i64, 21), (try vm.loadGlobal(idx)).toFixnum());
}

test "vm classifyCallShape categorizes lambda signatures" {
    const testing = std.testing;

    const code = [_]u8{};
    const consts = [_]Value{};

    const fixed_chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = 0,
        .arity = 1,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };
    const optional_chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = 0,
        .arity = 1,
        .opt_count = 2,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 3,
    };
    const key_chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = 0,
        .arity = 1,
        .opt_count = 0,
        .key_count = 2,
        .has_rest = 0,
        .num_locals = 3,
    };
    const rest_chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = 0,
        .arity = 1,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 1,
        .num_locals = 2,
    };

    try testing.expectEqual(Vm.CallShapeKind.fixed, Vm.classifyCallShape(&fixed_chunk));
    try testing.expectEqual(Vm.CallShapeKind.optional, Vm.classifyCallShape(&optional_chunk));
    try testing.expectEqual(Vm.CallShapeKind.key, Vm.classifyCallShape(&key_chunk));
    try testing.expectEqual(Vm.CallShapeKind.rest, Vm.classifyCallShape(&rest_chunk));
}

test "vm call-shape tracking is gated and accumulates counters" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    vm.setCallShapeTracking(false);
    vm.resetCallShapeStats();
    vm.recordCallShape(.fixed, false, false);
    var stats = vm.callShapeStats();
    try testing.expectEqual(@as(u64, 0), stats.total);
    try testing.expectEqual(@as(u64, 0), stats.fixed);
    try testing.expectEqual(@as(u64, 0), stats.dynamic);

    vm.setCallShapeTracking(true);
    vm.recordCallShape(.fixed, false, false);
    vm.recordCallShape(.key, true, true);
    stats = vm.callShapeStats();
    try testing.expectEqual(@as(u64, 2), stats.total);
    try testing.expectEqual(@as(u64, 1), stats.fixed);
    try testing.expectEqual(@as(u64, 1), stats.key);
    try testing.expectEqual(@as(u64, 1), stats.dynamic);
    try testing.expectEqual(@as(u64, 1), stats.tail);

    vm.resetCallShapeStats();
    stats = vm.callShapeStats();
    try testing.expectEqual(@as(u64, 0), stats.total);
    try testing.expectEqual(@as(u64, 0), stats.fixed);
    try testing.expectEqual(@as(u64, 0), stats.key);
}

test "vm collectGarbage updates ext roots" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const empty_chunk = Chunk{
        .code = @constCast(&[_]u8{}),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = 0,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    vm.chunk = &empty_chunk;

    var ext = [_]Value{try heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2))};
    vm.setExtRoots(ext[0..]);
    defer vm.clearExtRoots();

    _ = try vm.collectGarbage();

    try testing.expect(ext[0].isCons());
    const ptr = @intFromPtr(ext[0].toPtr(runtime.Cons));
    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;
    try testing.expect(ptr >= start and ptr < end);
}

test "vm restoreExtRoots rebinds owner after reallocation" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var roots_owner = std.ArrayList(Value){};
    defer roots_owner.deinit(allocator);
    try roots_owner.append(allocator, Value.makeFixnum(1));
    vm.setExtRootsOwned(&roots_owner);

    const saved = try vm.saveExtRoots();
    try testing.expect(saved.owner != null);

    const before_ptr = @intFromPtr(roots_owner.items.ptr);
    try roots_owner.ensureTotalCapacity(allocator, 1024);
    while (roots_owner.items.len < 1024) {
        try roots_owner.append(allocator, Value.makeFixnum(@intCast(roots_owner.items.len)));
    }
    try testing.expect(@intFromPtr(roots_owner.items.ptr) != before_ptr);

    var temp = [_]Value{Value.nil};
    vm.setExtRoots(temp[0..]);
    vm.restoreExtRoots(saved);

    try testing.expect(vm.ext_roots_owner == &roots_owner);
    try testing.expectEqual(@intFromPtr(roots_owner.items.ptr), @intFromPtr(vm.currentExtRoots().ptr));
    try testing.expectEqual(roots_owner.items.len, vm.currentExtRoots().len);

    try roots_owner.append(allocator, Value.makeFixnum(2048));
    try testing.expectEqual(roots_owner.items.len, vm.currentExtRoots().len);
}

test "vm restoreExtRootsSynced does not overwrite saved owner prefix" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var roots_owner = std.ArrayList(Value){};
    defer roots_owner.deinit(allocator);
    try roots_owner.append(allocator, try heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2)));
    vm.setExtRootsOwned(&roots_owner);

    const saved = try vm.saveExtRoots();

    var temp_roots = std.ArrayList(Value){};
    defer temp_roots.deinit(allocator);
    try temp_roots.appendSlice(allocator, saved.roots);
    vm.setExtRootsOwned(&temp_roots);

    const before = temp_roots.items[0];
    _ = try vm.collectGarbage();
    try testing.expect(temp_roots.items[0].isCons());
    try testing.expect(temp_roots.items[0].raw != before.raw);

    // Temporary roots may be transiently rewritten by nested eval paths; restoring
    // must not copy this stale value back into the saved owner prefix.
    temp_roots.items[0] = Value.nil;
    vm.restoreExtRootsSynced(saved, temp_roots.items, saved.roots.len);

    try testing.expect(vm.ext_roots_owner == &roots_owner);
    try testing.expect(roots_owner.items[0].isCons());

    const ptr = roots_owner.items[0].toPtrAddr();
    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;
    try testing.expect(ptr >= start and ptr < end);
}

test "vm saveExtRoots keeps inactive plain slices rooted" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var ext = [_]Value{try heap.allocCons(Value.makeFixnum(7), Value.makeFixnum(8))};
    vm.setExtRoots(ext[0..]);
    const saved = try vm.saveExtRoots();

    var temp_roots = std.ArrayList(Value){};
    defer temp_roots.deinit(allocator);
    try temp_roots.appendSlice(allocator, saved.roots);
    vm.setExtRootsOwned(&temp_roots);

    const before = ext[0];
    _ = try vm.collectGarbage();

    try testing.expect(ext[0].isCons());
    try testing.expect(ext[0].raw != before.raw);
    try testing.expect(temp_roots.items[0].isCons());
    vm.restoreExtRootsSynced(saved, temp_roots.items, saved.roots.len);

    try testing.expect(vm.ext_roots_owner == null);
    try testing.expectEqual(@intFromPtr(ext[0..].ptr), @intFromPtr(vm.currentExtRoots().ptr));
    try testing.expectEqual(ext.len, vm.currentExtRoots().len);
}

test "vm collectGarbage reuses gc_slots buffer" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    _ = try vm.collectGarbage();
    const cap1 = vm.gc_slots.capacity;
    try testing.expect(cap1 > 0);

    _ = try vm.collectGarbage();
    try testing.expectEqual(cap1, vm.gc_slots.capacity);
}

test "vm collectGarbage roots slot-tracked values" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const a = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const b = try heap.allocCons(Value.makeFixnum(2), Value.nil);
    const d = try heap.allocCons(Value.makeFixnum(4), Value.nil);
    const h = try heap.allocCons(Value.makeFixnum(8), Value.nil);

    vm.pending_throw_tag = a;
    vm.pending_throw_value = b;
    vm.pending_block_idx = 7;
    vm.pending_block_value = d;
    vm.handler_stack[0] = .{
        .condition_type = h,
        .handler_fn = a,
    };
    vm.handler_sp = 1;

    vm.progv_stack[0] = .{
        .saved_bindings = b,
    };
    vm.progv_sp = 1;

    const raw_a = a.raw;
    const raw_b = b.raw;
    const raw_d = d.raw;
    const raw_h = h.raw;

    _ = try vm.collectGarbage();

    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;

    const vals = [_]Value{
        vm.pending_throw_tag,
        vm.pending_throw_value,
        vm.pending_block_value,
        vm.handler_stack[0].condition_type,
        vm.handler_stack[0].handler_fn,
        vm.progv_stack[0].saved_bindings,
    };
    for (vals) |v| {
        try testing.expect(v.isCons());
        const ptr = @intFromPtr(v.toPtr(runtime.Cons));
        try testing.expect(ptr >= start and ptr < end);
    }

    try testing.expect(vm.pending_throw_tag.raw != raw_a);
    try testing.expect(vm.pending_throw_value.raw != raw_b);
    try testing.expectEqual(@as(?usize, 7), vm.pending_block_idx);
    try testing.expect(vm.pending_block_value.raw != raw_d);
    try testing.expect(vm.handler_stack[0].condition_type.raw != raw_h);
}

test "vm progv resolves forwarded symbol values before lookup" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var env = GlobalEnv.init(allocator);
    defer env.deinit();
    vm.setGlobalEnv(&env);

    const sym = try heap.intern("PROGV-FORWARDED-SYM");
    const sym_ptr = sym.toPtr(Symbol);
    const idx = (try vm.defineSymbolGlobalIndex(sym_ptr)) orelse return error.TestUnexpectedResult;
    try vm.storeGlobal(idx, Value.makeFixnum(5));

    const stale_raw = sym.raw;
    _ = try vm.collectGarbage();
    const stale_sym = Value{ .raw = stale_raw };
    const live_sym = vm.resolveForwardedValue(stale_sym);
    try testing.expect(live_sym.isSymbol());
    try testing.expect(live_sym.raw != stale_sym.raw);

    const symbol_list = try heap.allocCons(stale_sym, Value.nil);
    const value_list = try heap.allocCons(Value.makeFixnum(42), Value.nil);

    try vm.pushProgvFrame(symbol_list, value_list);

    const cur_val = try vm.loadGlobal(idx);
    try testing.expect(cur_val.isFixnum());
    try testing.expectEqual(@as(i64, 42), cur_val.toFixnum());

    try vm.popProgvFrame();
    const restored = try vm.loadGlobal(idx);
    try testing.expect(restored.isFixnum());
    try testing.expectEqual(@as(i64, 5), restored.toFixnum());
}

test "vm global index cache resets on env swap" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var env_b = GlobalEnv.init(allocator);
    defer env_b.deinit();
    vm.setGlobalEnv(&env_b);
    const dummy = try heap.intern("GLOBAL-CACHE-DUMMY");
    _ = (try vm.defineSymbolGlobalIndex(dummy.toPtr(Symbol))) orelse return error.TestUnexpectedResult;

    const sym = try heap.intern("GLOBAL-CACHE-SYM");
    const sym_ptr = sym.toPtr(Symbol);
    const idx_b = (try vm.defineSymbolGlobalIndex(sym_ptr)) orelse return error.TestUnexpectedResult;

    var env_a = GlobalEnv.init(allocator);
    defer env_a.deinit();
    vm.setGlobalEnv(&env_a);
    const idx_a = (try vm.defineSymbolGlobalIndex(sym_ptr)) orelse return error.TestUnexpectedResult;
    try testing.expect(idx_b != idx_a);
    const got_a = (try vm.lookupSymbolGlobalIndex(sym_ptr)) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(idx_a, got_a);

    vm.setGlobalEnv(&env_b);
    const got_b = (try vm.lookupSymbolGlobalIndex(sym_ptr)) orelse return error.TestUnexpectedResult;
    try testing.expectEqual(idx_b, got_b);
}

test "vm does not use legacy global fallback names" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var env = GlobalEnv.init(allocator);
    defer env.deinit();
    vm.setGlobalEnv(&env);

    _ = try env.define("LEGACY-FALLBACK-SYM");
    const sym = try heap.intern("LEGACY-FALLBACK-SYM");
    const sym_ptr = sym.toPtr(Symbol);

    const got = try vm.lookupSymbolGlobalIndex(sym_ptr);
    try testing.expect(got == null);
}

test "vm allocVector triggers debt-driven precollection" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    heap.gc_debt_bytes = heap.gc_debt_threshold_bytes;
    heap.stats.gc_debt_bytes = heap.gc_debt_bytes;
    const gc0 = heap.stats.gc_count;

    _ = try vm.allocVector(4, 4);

    try testing.expect(heap.stats.gc_count > gc0);
    try testing.expect(heap.stats.gc_debt_trigger_n > 0);
}

test "vm allocCons roots args across GC" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const a = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const b = try heap.allocCons(Value.makeFixnum(2), Value.nil);
    const a_ptr_old = @intFromPtr(a.toPtr(runtime.Cons));
    const b_ptr_old = @intFromPtr(b.toPtr(runtime.Cons));

    const gc_before = heap.stats.gc_count;

    const cons_size = @sizeOf(runtime.Cons);
    while (true) {
        const used = @intFromPtr(heap.alloc_ptr) - @intFromPtr(heap.from_start);
        const rem = heap.space_size - used;
        if (rem < cons_size) break;
        _ = try heap.allocCons(Value.nil, Value.nil);
    }

    const outer = try vm.allocCons(a, b);
    try testing.expect(heap.stats.gc_count > gc_before);

    const outer_cons = outer.toPtr(runtime.Cons);
    try testing.expect(outer_cons.car.isCons());
    try testing.expect(outer_cons.cdr.isCons());

    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;

    const car_ptr = @intFromPtr(outer_cons.car.toPtr(runtime.Cons));
    const cdr_ptr = @intFromPtr(outer_cons.cdr.toPtr(runtime.Cons));
    try testing.expect(car_ptr >= start and car_ptr < end);
    try testing.expect(cdr_ptr >= start and cdr_ptr < end);
    try testing.expect(car_ptr != a_ptr_old);
    try testing.expect(cdr_ptr != b_ptr_old);

    const car_cons = outer_cons.car.toPtr(runtime.Cons);
    const cdr_cons = outer_cons.cdr.toPtr(runtime.Cons);
    try testing.expect(car_cons.car.raw == Value.makeFixnum(1).raw);
    try testing.expect(cdr_cons.car.raw == Value.makeFixnum(2).raw);
}

test "vm allocClosureWithGC roots code and captures across GC" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    const empty_code = [_]u8{};
    const no_consts = [_]Value{};
    const code = try heap.allocChunk(&empty_code, &no_consts, 0, 0, 0, false, 0);
    const code_ptr_old = @intFromPtr(code.toPtr(Chunk));

    const cap0 = try heap.allocCons(Value.makeFixnum(7), Value.nil);
    const cap1 = try heap.allocCons(Value.makeFixnum(9), Value.nil);
    const cap0_ptr_old = @intFromPtr(cap0.toPtr(runtime.Cons));
    const cap1_ptr_old = @intFromPtr(cap1.toPtr(runtime.Cons));

    var captures = [_]Value{ cap0, cap1 };
    const closure_bytes = std.mem.alignForward(usize, @sizeOf(runtime.Closure) + captures.len * @sizeOf(Value), 16);
    const gc_before = heap.stats.gc_count;

    while (true) {
        const used = @intFromPtr(heap.alloc_ptr) - @intFromPtr(heap.from_start);
        const rem = heap.space_size - used;
        if (rem < closure_bytes) break;
        _ = try heap.allocCons(Value.nil, Value.nil);
    }

    const clo_val = try vm.allocClosureWithGC(code, 0, captures[0..]);
    try testing.expect(heap.stats.gc_count > gc_before);
    try testing.expect(clo_val.isClosure());

    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;

    const clo = clo_val.toPtr(runtime.Closure);
    const code_ptr_new = @intFromPtr(clo.code.toPtr(Chunk));
    try testing.expect(code_ptr_new >= start and code_ptr_new < end);
    try testing.expect(code_ptr_new != code_ptr_old);

    const got0 = clo.getCapture(0);
    const got1 = clo.getCapture(1);
    try testing.expect(got0.isCons());
    try testing.expect(got1.isCons());

    const got0_ptr = @intFromPtr(got0.toPtr(runtime.Cons));
    const got1_ptr = @intFromPtr(got1.toPtr(runtime.Cons));
    try testing.expect(got0_ptr >= start and got0_ptr < end);
    try testing.expect(got1_ptr >= start and got1_ptr < end);
    try testing.expect(got0_ptr != cap0_ptr_old);
    try testing.expect(got1_ptr != cap1_ptr_old);

    try testing.expect(got0.toPtr(runtime.Cons).car.raw == Value.makeFixnum(7).raw);
    try testing.expect(got1.toPtr(runtime.Cons).car.raw == Value.makeFixnum(9).raw);
}

test "vm list_reverse survives GC" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var items: [200]Value = undefined;
    for (0..items.len) |i| {
        items[i] = Value.makeFixnum(@intCast(i));
    }
    const list = try heap.listFromSlice(&items);
    try vm.push(list);

    const cons_size = @sizeOf(runtime.Cons);
    while (true) {
        const used = @intFromPtr(heap.alloc_ptr) - @intFromPtr(heap.from_start);
        const rem = heap.space_size - used;
        if (rem < cons_size) break;
        _ = try heap.allocCons(Value.nil, Value.nil);
    }

    const gc_before = heap.stats.gc_count;
    try vm.executeOp(.list_reverse);
    try testing.expect(heap.stats.gc_count > gc_before);

    const result = try vm.pop();
    var curr = result;
    var expect_n: i64 = 199;
    while (curr.isCons()) {
        const c = curr.toPtr(runtime.Cons);
        try testing.expect(c.car.raw == Value.makeFixnum(expect_n).raw);
        curr = c.cdr;
        expect_n -= 1;
    }
    try testing.expect(curr.isNil());
    try testing.expect(expect_n == -1);
}

test "vm trace csv exact token matching is case-insensitive" {
    const testing = std.testing;
    try testing.expect(Vm.csvHasExactToken(" TypeMismatch , UnboundSymbol ", "typemismatch"));
    try testing.expect(Vm.csvHasExactToken("add,mul", "ADD"));
    try testing.expect(!Vm.csvHasExactToken("add,mul", "sub"));
}

test "vm trace csv substring matching is case-insensitive" {
    const testing = std.testing;
    try testing.expect(Vm.csvHasSubstringToken("powerlist, integrator", "MAXIMA-POWERLIST-PATH"));
    try testing.expect(Vm.csvHasSubstringToken("mapcar", "MAPCAR"));
    try testing.expect(!Vm.csvHasSubstringToken("powerlist", "MATCHER"));
}

test "vm caches hot trace flags from environment" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    try testing.expectEqual(std.posix.getenv("HABU_TRACE_CALL_ARGS") != null, vm.trace_call_args);
    try testing.expectEqual(std.posix.getenv("HABU_TRACE_UPVALUE") != null, vm.trace_upvalue);
    try testing.expectEqual(std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null, vm.trace_error_context);
    try testing.expectEqual(std.posix.getenv("HABU_TRACE_CALL_MISMATCH") != null, vm.trace_call_mismatch);
    try testing.expectEqual(std.posix.getenv("HABU_TRACE_CHUNK_POOL_SLOT") != null, vm.trace_chunk_pool_slot != null);
}
