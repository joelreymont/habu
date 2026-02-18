const std = @import("std");
const hoist = @import("hoist");

const Function = hoist.function.Function;
const Signature = hoist.signature.Signature;
const AbiParam = hoist.signature.AbiParam;
const HoistType = hoist.types.Type;
const FunctionBuilder = hoist.builder.FunctionBuilder;
const ContextBuilder = hoist.context.ContextBuilder;

const I64 = HoistType.I64;

comptime {
    if (!@hasDecl(hoist, "function")) @compileError("hoist API contract: missing hoist.function");
    if (!@hasDecl(hoist, "signature")) @compileError("hoist API contract: missing hoist.signature");
    if (!@hasDecl(hoist, "types")) @compileError("hoist API contract: missing hoist.types");
    if (!@hasDecl(hoist, "builder")) @compileError("hoist API contract: missing hoist.builder");
    if (!@hasDecl(hoist, "context")) @compileError("hoist API contract: missing hoist.context");
}

pub fn run(allocator: std.mem.Allocator) !void {
    var sig = Signature.init(allocator, .system_v);
    try sig.params.append(allocator, AbiParam.new(I64));
    try sig.returns.append(allocator, AbiParam.new(I64));

    var func = try Function.init(allocator, "habu_hoist_contract", sig);
    defer func.deinit();

    var b = try FunctionBuilder.init(allocator, &func);
    defer b.deinit();

    const entry = try b.createBlock();
    try func.dfg.setBlockParams(entry, &.{I64});
    b.switchToBlock(entry);
    try b.sealBlock(entry);
    try b.retValues(&.{func.dfg.blockParams(entry)[0]});

    var ctx_builder = ContextBuilder.init(allocator);
    _ = try ctx_builder.targetNative();
    var ctx = ctx_builder.optLevel(.aggressive).callConv(.system_v).verification(true).build();
    defer ctx.deinit();

    var code = try ctx.compileFunction(&func);
    defer code.deinit();
    if (code.code.items.len == 0) return error.EmptyCode;
}
