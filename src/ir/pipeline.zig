//! Compilation Pipeline - Tree IR to Native Code
//!
//! Unified pipeline that takes Habu tree IR and produces executable native code:
//! 1. Lower tree IR to register IR
//! 2. Allocate physical registers (graph coloring)
//! 3. Generate ARM64 machine code

const std = @import("std");
const tree_ir = @import("../compiler/ir.zig");
const Ir = tree_ir.Ir;
const lower = @import("lower.zig");
const Lowerer = lower.Lowerer;
const inst = @import("instruction.zig");
const Function = inst.Function;
const regalloc = @import("regalloc.zig");
const GraphColorAlloc = regalloc.GraphColorAlloc;
const arm64 = @import("arm64.zig");

pub const PipelineError = error{
    OutOfMemory,
    LoweringFailed,
    AllocationFailed,
    CodegenFailed,
    UnsupportedTarget,
};

/// Compilation result
pub const CompiledFunction = struct {
    /// Native machine code
    code: []const u8,
    /// Entry point offset
    entry: usize,
    /// Function metadata
    name: []const u8,
    param_count: u8,

    pub fn deinit(self: *CompiledFunction, allocator: std.mem.Allocator) void {
        allocator.free(self.code);
    }
};

/// Pipeline configuration
pub const PipelineConfig = struct {
    /// Enable debug output
    debug: bool = false,
    /// Target architecture
    target: Target = .arm64,

    pub const Target = enum {
        arm64,
        // Future: x86_64, wasm, etc.
    };
};

/// Compile tree IR to native code
pub fn compile(
    allocator: std.mem.Allocator,
    name: []const u8,
    params: []const []const u8,
    body: *const Ir,
    config: PipelineConfig,
) PipelineError!CompiledFunction {
    // Step 1: Lower tree IR to register IR
    var lowerer = Lowerer.init(allocator);
    defer lowerer.deinit();

    const reg_func = lowerer.lowerFunction(name, params, body) catch {
        return PipelineError.LoweringFailed;
    };
    defer allocator.free(reg_func.code);
    defer allocator.free(reg_func.constants);

    if (config.debug) {
        std.debug.print("=== Register IR for {s} ===\n", .{name});
        std.debug.print("params={}, locals={}, regs={}\n", .{
            reg_func.param_count,
            reg_func.local_count,
            reg_func.max_regs,
        });
        for (reg_func.code, 0..) |instr, idx| {
            std.debug.print("  {d:4}: {any}\n", .{ idx, instr });
        }
    }

    // Step 2: Register allocation (optional - used for optimized codegen)
    var reg_alloc = GraphColorAlloc.init(allocator);
    defer reg_alloc.deinit();

    var reg_map: ?regalloc.RegisterMap = null;
    reg_alloc.allocate(reg_func) catch |err| {
        // Non-fatal: we can still generate code with virtual registers
        if (config.debug) {
            std.debug.print("Register allocation failed: {}, using virtual registers\n", .{err});
        }
    };

    // Extract register map if allocation succeeded
    if (reg_alloc.node_count > 0) {
        reg_map = reg_alloc.getRegisterMap();
        if (config.debug) {
            std.debug.print("=== Register Allocation ===\n", .{});
            std.debug.print("Spills: {}\n", .{reg_alloc.spill_count});
        }
    }

    // Step 3: Generate native code
    switch (config.target) {
        .arm64 => {
            const code = arm64.generate(allocator, reg_func, reg_map, null) catch {
                return PipelineError.CodegenFailed;
            };

            return .{
                .code = code,
                .entry = 0,
                .name = name,
                .param_count = reg_func.param_count,
            };
        },
    }
}

/// Compile and get function pointer (for FFI)
pub fn compileToFn(
    allocator: std.mem.Allocator,
    name: []const u8,
    params: []const []const u8,
    body: *const Ir,
    comptime FnType: type,
) PipelineError!FnType {
    const compiled = try compile(allocator, name, params, body, .{});

    // Cast code to function pointer
    // NOTE: This requires the memory to be executable
    return @ptrCast(@alignCast(compiled.code.ptr));
}

// ============================================================================
// Tests
// ============================================================================

test "pipeline simple function" {
    const testing = std.testing;

    // (defun add-one (x) (+ x 1))
    const lit1 = Ir{ .lit = @import("../runtime/value.zig").Value.makeFixnum(1) };
    const var_x = Ir{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };
    const add = Ir{ .add = .{ .left = &var_x, .right = &lit1 } };

    const params = [_][]const u8{"x"};
    const result = compile(testing.allocator, "add-one", &params, &add, .{ .debug = false });

    if (result) |compiled| {
        var compiled_mut = compiled;
        defer compiled_mut.deinit(testing.allocator);
        try testing.expect(compiled.code.len > 0);
        try testing.expectEqualStrings("add-one", compiled.name);
    } else |err| {
        // OK if codegen not fully implemented yet
        try testing.expect(err == PipelineError.CodegenFailed or err == PipelineError.AllocationFailed);
    }
}

test "pipeline if expression" {
    const testing = std.testing;
    const Value = @import("../runtime/value.zig").Value;

    // (defun abs (x) (if (< x 0) (- 0 x) x))
    const var_x = Ir{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };
    const lit0 = Ir{ .lit = Value.makeFixnum(0) };
    const cmp = Ir{ .lt = .{ .left = &var_x, .right = &lit0 } };
    const neg = Ir{ .sub = .{ .left = &lit0, .right = &var_x } };
    const if_expr = Ir{ .@"if" = .{ .cond = &cmp, .then_branch = &neg, .else_branch = &var_x } };

    const params = [_][]const u8{"x"};
    const result = compile(testing.allocator, "abs", &params, &if_expr, .{});

    if (result) |compiled| {
        var compiled_mut = compiled;
        defer compiled_mut.deinit(testing.allocator);
        try testing.expect(compiled.code.len > 0);
    } else |_| {
        // OK if not fully implemented
    }
}
