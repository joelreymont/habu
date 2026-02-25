const std = @import("std");
const runtime = @import("../runtime/runtime.zig");
const ir = @import("../compiler/ir.zig");

const Ir = ir.Ir;
const Value = runtime.Value;
const Symbol = runtime.Symbol;
const Chunk = runtime.Chunk;

pub const LambdaCandidate = struct {
    name: []const u8,
    local_name: []const u8,
    name_sym: Value,
    lambda_ir: *const Ir,
};

pub const IneligibleReason = enum {
    not_lambda,
    speed,
    safety,
    assert_fixnum_body,
    captures,
    optional_params,
    key_params,
    rest_param,
};

fn stripQualifiedName(name: []const u8) []const u8 {
    if (std.mem.indexOf(u8, name, "::")) |sep| return name[sep + 2 ..];
    if (std.mem.indexOfScalar(u8, name, ':')) |sep| return name[sep + 1 ..];
    return name;
}

fn appendCandidate(
    allocator: std.mem.Allocator,
    out: *std.ArrayList(LambdaCandidate),
    name: []const u8,
    lambda_ir: *const Ir,
) !void {
    if (lambda_ir.* != .lambda) return;

    const local_name = stripQualifiedName(name);

    const owned_name = try allocator.dupe(u8, name);
    errdefer allocator.free(owned_name);
    const owned_local_name = try allocator.dupe(u8, local_name);
    errdefer allocator.free(owned_local_name);

    try out.append(allocator, .{
        .name = owned_name,
        .local_name = owned_local_name,
        .name_sym = lambda_ir.lambda.name,
        .lambda_ir = lambda_ir,
    });
}

fn collectNode(
    allocator: std.mem.Allocator,
    node: *const Ir,
    out: *std.ArrayList(LambdaCandidate),
) !void {
    switch (node.*) {
        .define => |d| {
            if (d.value.* == .lambda) {
                try appendCandidate(allocator, out, d.name, d.value);
            }
        },
        .set_symbol_function => |op| {
            if (op.right.* == .lambda) {
                const name = if (op.left.* == .quote_sym)
                    op.left.quote_sym
                else
                    "<set-symbol-function>";
                try appendCandidate(allocator, out, name, op.right);
            }
        },
        .progn => |exprs| {
            for (exprs) |expr| try collectNode(allocator, expr, out);
        },
        else => {},
    }
}

pub fn collectLambdaCandidates(
    allocator: std.mem.Allocator,
    node: *const Ir,
    out: *std.ArrayList(LambdaCandidate),
) !void {
    try collectNode(allocator, node, out);
}

pub fn freeLambdaCandidates(
    allocator: std.mem.Allocator,
    candidates: []const LambdaCandidate,
) void {
    for (candidates) |candidate| {
        allocator.free(candidate.name);
        allocator.free(candidate.local_name);
    }
}

pub fn isEligible(lambda_ir: *const Ir) bool {
    return ineligibleReason(lambda_ir) == null;
}

pub fn ineligibleReason(lambda_ir: *const Ir) ?IneligibleReason {
    if (lambda_ir.* != .lambda) return .not_lambda;
    const lambda = lambda_ir.lambda;

    // Keep current bridge constraints.
    if (lambda.captures.len > 0) return .captures;
    if (lambda.optional_params.len > 0) return .optional_params;
    if (lambda.key_params.len > 0) return .key_params;
    if (lambda.rest_param != null) return .rest_param;

    return null;
}

pub fn reasonLabel(reason: IneligibleReason) []const u8 {
    return switch (reason) {
        .not_lambda => "not_lambda",
        .speed => "speed",
        .safety => "safety",
        .assert_fixnum_body => "assert_fixnum_body",
        .captures => "captures",
        .optional_params => "optional_params",
        .key_params => "key_params",
        .rest_param => "rest_param",
    };
}

fn chunkSignatureMatches(lambda_ir: *const Ir, chunk: *const Chunk) bool {
    if (lambda_ir.* != .lambda) return false;
    const lambda = lambda_ir.lambda;
    if (chunk.arity != @as(u8, @intCast(lambda.params.len))) return false;
    if (chunk.opt_count != @as(u8, @intCast(lambda.optional_params.len))) return false;
    if (chunk.key_count != @as(u8, @intCast(lambda.key_params.len))) return false;
    if ((chunk.has_rest != 0) != (lambda.rest_param != null)) return false;
    return true;
}

fn chunkNameMatches(candidate: *const LambdaCandidate, live_name_sym: Value, chunk: *const Chunk) bool {
    if (live_name_sym.isSymbol() and chunk.name.raw == live_name_sym.raw) return true;

    if (chunk.name.isSymbol()) {
        const chunk_name = chunk.name.toPtr(Symbol).getName();
        if (std.ascii.eqlIgnoreCase(chunk_name, candidate.local_name)) return true;
        if (std.ascii.eqlIgnoreCase(chunk_name, candidate.name)) return true;
        return false;
    }
    if (chunk.name.isString()) {
        const chunk_name = chunk.name.toPtr(runtime.String).bytes();
        if (std.ascii.eqlIgnoreCase(chunk_name, candidate.local_name)) return true;
        if (std.ascii.eqlIgnoreCase(chunk_name, candidate.name)) return true;
        return false;
    }
    return false;
}

pub fn findMatchingChunk(
    candidate: *const LambdaCandidate,
    live_name_sym: Value,
    child_chunks: []const Value,
    used: []bool,
) ?*const Chunk {
    if (child_chunks.len == 0 or child_chunks.len != used.len) return null;

    for (child_chunks, 0..) |chunk_val, idx| {
        if (used[idx]) continue;
        const chunk = chunk_val.toPtr(Chunk);
        if (!chunkSignatureMatches(candidate.lambda_ir, chunk)) continue;
        if (!chunkNameMatches(candidate, live_name_sym, chunk)) continue;
        used[idx] = true;
        return chunk;
    }
    return null;
}

// ═══════════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════════

const testing = std.testing;

fn makeLambdaIr(
    captures: []const ir.Ir.Capture,
    optional_params: []const ir.Ir.OptionalParam,
    key_params: []const ir.Ir.KeyParam,
    rest_param: ?[]const u8,
) Ir {
    const body_storage = struct {
        var body: Ir = .{ .lit = Value.nil };
    };
    return .{ .lambda = .{
        .params = &.{},
        .optional_params = optional_params,
        .key_params = key_params,
        .rest_param = rest_param,
        .captures = captures,
        .body = &body_storage.body,
        .name = Value.nil,
    } };
}

test "ineligibleReason: plain lambda is eligible" {
    var lambda_ir = makeLambdaIr(&.{}, &.{}, &.{}, null);
    try testing.expectEqual(null, ineligibleReason(&lambda_ir));
    try testing.expect(isEligible(&lambda_ir));
}

test "ineligibleReason: lambda with captures" {
    const cap = [_]ir.Ir.Capture{.{ .name = "x", .depth = 1, .index = 0 }};
    var lambda_ir = makeLambdaIr(&cap, &.{}, &.{}, null);
    try testing.expectEqual(IneligibleReason.captures, ineligibleReason(&lambda_ir));
    try testing.expect(!isEligible(&lambda_ir));
}

test "ineligibleReason: lambda with optional params" {
    const body_storage = struct {
        var body: Ir = .{ .lit = Value.nil };
    };
    const opt = [_]ir.Ir.OptionalParam{.{
        .name = "y",
        .default = &body_storage.body,
    }};
    var lambda_ir = makeLambdaIr(&.{}, &opt, &.{}, null);
    try testing.expectEqual(IneligibleReason.optional_params, ineligibleReason(&lambda_ir));
}

test "ineligibleReason: lambda with rest param" {
    var lambda_ir = makeLambdaIr(&.{}, &.{}, &.{}, "rest");
    try testing.expectEqual(IneligibleReason.rest_param, ineligibleReason(&lambda_ir));
}

test "ineligibleReason: non-lambda is not eligible" {
    var lit_ir: Ir = .{ .lit = Value.nil };
    try testing.expectEqual(IneligibleReason.not_lambda, ineligibleReason(&lit_ir));
    try testing.expect(!isEligible(&lit_ir));
}

test "collectLambdaCandidates: collects from define" {
    var lambda_ir = makeLambdaIr(&.{}, &.{}, &.{}, null);
    var define_ir: Ir = .{ .define = .{
        .name = "test-fn",
        .index = 0,
        .value = &lambda_ir,
    } };

    var candidates = std.ArrayList(LambdaCandidate){};
    defer {
        freeLambdaCandidates(testing.allocator, candidates.items);
        candidates.deinit(testing.allocator);
    }

    try collectLambdaCandidates(testing.allocator, &define_ir, &candidates);
    try testing.expectEqual(@as(usize, 1), candidates.items.len);
    try testing.expectEqualStrings("test-fn", candidates.items[0].name);
    try testing.expectEqualStrings("test-fn", candidates.items[0].local_name);
}

test "collectLambdaCandidates: strips qualified name" {
    var lambda_ir = makeLambdaIr(&.{}, &.{}, &.{}, null);
    var define_ir: Ir = .{ .define = .{
        .name = "CL-USER::my-func",
        .index = 0,
        .value = &lambda_ir,
    } };

    var candidates = std.ArrayList(LambdaCandidate){};
    defer {
        freeLambdaCandidates(testing.allocator, candidates.items);
        candidates.deinit(testing.allocator);
    }

    try collectLambdaCandidates(testing.allocator, &define_ir, &candidates);
    try testing.expectEqual(@as(usize, 1), candidates.items.len);
    try testing.expectEqualStrings("CL-USER::my-func", candidates.items[0].name);
    try testing.expectEqualStrings("my-func", candidates.items[0].local_name);
}

test "collectLambdaCandidates: collects from progn" {
    var lambda1 = makeLambdaIr(&.{}, &.{}, &.{}, null);
    var lambda2 = makeLambdaIr(&.{}, &.{}, &.{}, null);
    var def1: Ir = .{ .define = .{ .name = "fn-a", .index = 0, .value = &lambda1 } };
    var def2: Ir = .{ .define = .{ .name = "fn-b", .index = 1, .value = &lambda2 } };
    const exprs = [_]*const Ir{ &def1, &def2 };
    var progn_ir: Ir = .{ .progn = &exprs };

    var candidates = std.ArrayList(LambdaCandidate){};
    defer {
        freeLambdaCandidates(testing.allocator, candidates.items);
        candidates.deinit(testing.allocator);
    }

    try collectLambdaCandidates(testing.allocator, &progn_ir, &candidates);
    try testing.expectEqual(@as(usize, 2), candidates.items.len);
    try testing.expectEqualStrings("fn-a", candidates.items[0].name);
    try testing.expectEqualStrings("fn-b", candidates.items[1].name);
}

test "collectLambdaCandidates: skips non-lambda define" {
    var lit_ir: Ir = .{ .lit = Value.nil };
    var define_ir: Ir = .{ .define = .{
        .name = "not-a-lambda",
        .index = 0,
        .value = &lit_ir,
    } };

    var candidates = std.ArrayList(LambdaCandidate){};
    defer candidates.deinit(testing.allocator);

    try collectLambdaCandidates(testing.allocator, &define_ir, &candidates);
    try testing.expectEqual(@as(usize, 0), candidates.items.len);
}
