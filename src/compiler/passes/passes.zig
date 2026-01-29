//! Compiler Passes
//!
//! Nanopass transformations for the Habu compiler.
//! Each pass does ONE thing only.
//!
//! Full Pipeline:
//!   Source → parse → p01_expand → p02_desugar → p03_lift →
//!   p04_resolve → p05_capture → p06_annotate → p07_infer →
//!   p08_erase → p09_emit → Bytecode
//!
//! Stages:
//!   p01_expand:   Value → Value   (macro expansion)
//!   p02_desugar:  Value → Value   (let*, cond, and, or, defun → core forms)
//!   p03_lift:     Value → Ir      (S-expr to IR, unresolved vars)
//!   p04_resolve:  Ir → Ir         (name resolution, fill depth/index)
//!   p05_capture:  Ir → Ir         (closure analysis, identify captures)
//!   p06_annotate: Ir → TypedIr    (wrap with type/quantity slots)
//!   p07_infer:    TypedIr → TypedIr (run BiChecker, populate types)
//!   p08_erase:    TypedIr → Ir    (remove zero-quantity nodes)
//!   p09_emit:     Ir → Chunk      (bytecode generation)

pub const pass = @import("pass.zig");
pub const pipeline = @import("pipeline.zig");
pub const ir_types = @import("ir_types.zig");

// Nano passes - each does ONE thing
pub const expand = @import("p01_expand.zig");
pub const desugar = @import("p02_desugar.zig");
pub const lift = @import("p03_lift.zig");
pub const resolve = @import("p04_resolve.zig");
pub const capture = @import("p05_capture.zig");
pub const annotate = @import("p06_annotate.zig");
pub const infer = @import("p07_infer.zig");
pub const erase = @import("p08_erase.zig");
pub const emit = @import("p09_emit.zig");

// Re-export pass infrastructure
pub const Pass = pass.Pass;
pub const PassError = pass.PassError;
pub const PassResult = pass.PassResult;
pub const Diagnostic = pass.Diagnostic;
pub const makePass = pass.makePass;
pub const identityPass = pass.identityPass;

// Re-export standard pass types
pub const IrPass = pass.IrPass;
pub const AnnotatePass = pass.AnnotatePass;
pub const TypedPass = pass.TypedPass;
pub const ErasePass = pass.ErasePass;
pub const SExprPass = pass.SExprPass;
pub const LiftPass = pass.LiftPass;

// Re-export pipeline
pub const IrPipeline = pipeline.IrPipeline;
pub const PipelineResult = pipeline.PipelineResult;
pub const Config = pipeline.Config;
pub const Stats = pipeline.Stats;

// Re-export IR types
pub const TypedIr = ir_types.TypedIr;
pub const TypedIrBuilder = ir_types.TypedIrBuilder;
pub const Quantity = ir_types.Quantity;

/// Run the complete compilation pipeline: Value → Chunk
/// Full end-to-end compiler: p01→p02→p03→p04→p05→p06→p07→p08→p09
pub fn runFullPipeline(
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: ?*Vm,
    macros: *const expand.MacroTable,
    globals: *resolve.GlobalRegistry,
    expr: Value,
) FullPipelineError!Value {
    // p01: Expand macros
    var expander = expand.Expander.init(allocator, heap, vm, macros);
    const expanded = try expander.expand(expr);

    // p02: Desugar
    const vm_ptr = vm orelse return error.InvalidInput;
    var desugarer = desugar.Desugarer.init(allocator, heap, &vm_ptr.builtins);
    const desugared = try desugarer.desugar(expanded);

    // p03-p05: Frontend (lift→resolve→capture)
    const ir = try runFrontendPipeline(allocator, heap, globals, desugared);

    // p06-p08: Type pipeline (annotate→infer→erase)
    const typed_ir = try runNanoPipeline(allocator, &vm_ptr.builtins, ir);

    // p09: Emit bytecode
    const emit_result = emit.emit(allocator, heap, typed_ir) catch return error.EmitFailed;
    return emit_result.output;
}

pub const FullPipelineError = anyerror;

/// Run the frontend pipeline: Value → lift → resolve → capture → Ir
/// This converts S-expressions to fully-resolved IR with capture analysis.
///
/// Pipeline: p03_lift → p04_resolve → p05_capture
pub fn runFrontendPipeline(
    allocator: std.mem.Allocator,
    heap: *Heap,
    globals: *resolve.GlobalRegistry,
    expr: Value,
) FrontendError!*const Ir {
    // Pass 1: Lift (Value → Ir with unresolved vars)
    var lifter = try lift.Lifter.init(allocator, heap);
    const lifted_ir = lifter.lift(expr) catch |err| switch (err) {
        error.InvalidSyntax => return error.InvalidSyntax,
        error.InvalidLambda => return error.InvalidLambda,
        error.InvalidLet => return error.InvalidLet,
        error.InvalidIf => return error.InvalidIf,
        error.OutOfMemory => return error.OutOfMemory,
    };

    // Pass 2: Resolve (fill depth/index)
    const resolved_ir = resolve.resolve(allocator, globals, lifted_ir) catch |err| switch (err) {
        error.UnboundVariable => return error.UnboundVariable,
        error.OutOfMemory => return error.OutOfMemory,
    };

    // Pass 3: Capture (identify free variables)
    const captured_ir = capture.capture(allocator, resolved_ir) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
    };

    return captured_ir;
}

/// Errors from frontend pipeline
pub const FrontendError = error{
    InvalidSyntax,
    InvalidLambda,
    InvalidLet,
    InvalidIf,
    UnboundVariable,
    OutOfMemory,
};

/// Run the nano pass pipeline: Ir → annotate → infer → erase → Ir
/// This is the main entry point for type-aware compilation.
///
/// Uses page_allocator for intermediate TypedIr nodes since they're
/// temporary and don't need to be tracked for leak detection.
pub fn runNanoPipeline(allocator: std.mem.Allocator, builtins: *const @import("../../runtime/builtins.zig").BuiltinSymbols, ir: *const Ir) PassError!*const Ir {
    // Use page_allocator for intermediate TypedIr (not tracked for leaks)
    const typed_alloc = std.heap.page_allocator;

    // Pass 1: Annotate (Ir → TypedIr)
    const annotate_result = annotate.annotate(typed_alloc, ir) catch |err| {
        std.debug.print("Annotate pass failed: {}\n", .{err});
        return err;
    };
    const typed_ir = annotate_result.output;

    // Pass 2: Infer (TypedIr → TypedIr)
    const infer_result = infer.infer(typed_alloc, builtins, typed_ir) catch |err| {
        std.debug.print("Infer pass failed: {}\n", .{err});
        return err;
    };
    const inferred_ir = infer_result.output;

    // Pass 3: Erase (TypedIr → Ir)
    // Use original allocator for final Ir output
    const erase_result = erase.erase(allocator, inferred_ir) catch |err| {
        std.debug.print("Erase pass failed: {}\n", .{err});
        return err;
    };

    return erase_result.output;
}

/// Create an empty homogeneous Ir → Ir pipeline
/// Use runNanoPipeline() for the actual type-aware compilation
pub fn createPipeline(allocator: std.mem.Allocator) IrPipeline {
    return IrPipeline.init(allocator, .{});
}

const std = @import("std");
const Ir = @import("../ir.zig").Ir;
const Heap = @import("../../runtime/heap.zig").Heap;
const Value = @import("../../runtime/value.zig").Value;
const Vm = @import("../../interp/vm.zig").Vm;
const Chunk = @import("../../bytecode/bytecode.zig").Chunk;

// Re-export S-expression passes
pub const Desugarer = desugar.Desugarer;
pub const Expander = expand.Expander;
pub const MacroTable = expand.MacroTable;

// Re-export IR passes
pub const Lifter = lift.Lifter;
pub const LiftSymbols = lift.LiftSymbols;
pub const Resolver = resolve.Resolver;
pub const Scope = resolve.Scope;
pub const GlobalRegistry = resolve.GlobalRegistry;
pub const CaptureAnalyzer = capture.CaptureAnalyzer;

test "runFrontendPipeline - literal" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    var globals = resolve.GlobalRegistry.init(allocator);
    defer globals.deinit();

    const expr = Value.makeFixnum(42);
    const ir = try runFrontendPipeline(allocator, &heap, &globals, expr);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(ir.*));
}

test "runFullPipeline - simple literal" {
    const testing = std.testing;

    // Use arena to handle IR allocations that are hard to track
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    var macros = expand.MacroTable.init(allocator);
    defer macros.deinit();

    var globals = resolve.GlobalRegistry.init(allocator);
    defer globals.deinit();

    var vm = try Vm.init(allocator, &heap);

    // Test: simple literal
    const expr = Value.makeFixnum(42);

    const chunk = try runFullPipeline(allocator, &heap, &vm, &macros, &globals, expr);
    _ = chunk; // Arena handles cleanup

    // If we get here without error, the pipeline worked
}

test {
    _ = pass;
    _ = pipeline;
    _ = ir_types;
    _ = expand;
    _ = desugar;
    _ = lift;
    _ = resolve;
    _ = capture;
    _ = annotate;
    _ = infer;
    _ = erase;
    _ = emit;
}
