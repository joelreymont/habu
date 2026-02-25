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
        if (!chunk_val.isChunk()) continue;
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

// ═══════════════════════════════════════════════════════════════════
// stripQualifiedName edge cases
// ═══════════════════════════════════════════════════════════════════

test "stripQualifiedName: empty string" {
    try testing.expectEqualStrings("", stripQualifiedName(""));
}

test "stripQualifiedName: no qualifier" {
    try testing.expectEqualStrings("FOO", stripQualifiedName("FOO"));
    try testing.expectEqualStrings("my-func", stripQualifiedName("my-func"));
}

test "stripQualifiedName: double colon qualifier" {
    try testing.expectEqualStrings("BAR", stripQualifiedName("FOO::BAR"));
    try testing.expectEqualStrings("my-func", stripQualifiedName("CL-USER::my-func"));
}

test "stripQualifiedName: single colon qualifier" {
    try testing.expectEqualStrings("BAR", stripQualifiedName("FOO:BAR"));
    try testing.expectEqualStrings("print", stripQualifiedName("CL:print"));
}

test "stripQualifiedName: trailing double colon" {
    try testing.expectEqualStrings("", stripQualifiedName("FOO::"));
    try testing.expectEqualStrings("", stripQualifiedName("PACKAGE::"));
}

test "stripQualifiedName: trailing single colon" {
    try testing.expectEqualStrings("", stripQualifiedName("FOO:"));
}

test "stripQualifiedName: just double colon" {
    try testing.expectEqualStrings("", stripQualifiedName("::"));
}

test "stripQualifiedName: just single colon" {
    try testing.expectEqualStrings("", stripQualifiedName(":"));
}

test "stripQualifiedName: leading double colon" {
    try testing.expectEqualStrings("FOO", stripQualifiedName("::FOO"));
}

test "stripQualifiedName: leading single colon" {
    try testing.expectEqualStrings("FOO", stripQualifiedName(":FOO"));
}

test "stripQualifiedName: multiple qualifiers prefers double colon" {
    // First :: at index 1, returns everything after first ::
    try testing.expectEqualStrings("B::C", stripQualifiedName("A::B::C"));
}

test "stripQualifiedName: mixed qualifiers prefers double colon" {
    // :: at index 1 comes before : at index 3
    try testing.expectEqualStrings("B:C", stripQualifiedName("A::B:C"));
}

test "stripQualifiedName: only single colons" {
    // No ::, uses first :
    try testing.expectEqualStrings("B:C", stripQualifiedName("A:B:C"));
}

// ═══════════════════════════════════════════════════════════════════
// findMatchingChunk edge cases
// ═══════════════════════════════════════════════════════════════════

test "findMatchingChunk: empty child_chunks" {
    var lambda_ir = makeLambdaIr(&.{}, &.{}, &.{}, null);
    const candidate = LambdaCandidate{
        .name = "test",
        .local_name = "test",
        .name_sym = Value.nil,
        .lambda_ir = &lambda_ir,
    };
    var used: [0]bool = .{};
    try testing.expectEqual(null, findMatchingChunk(&candidate, Value.nil, &.{}, &used));
}

test "findMatchingChunk: length mismatch between chunks and used" {
    var lambda_ir = makeLambdaIr(&.{}, &.{}, &.{}, null);
    const candidate = LambdaCandidate{
        .name = "test",
        .local_name = "test",
        .name_sym = Value.nil,
        .lambda_ir = &lambda_ir,
    };

    const chunks = [_]Value{Value.nil};
    var used: [0]bool = .{}; // wrong length

    try testing.expectEqual(null, findMatchingChunk(&candidate, Value.nil, &chunks, &used));
}

fn makeLambdaIrWithParams(
    params: []const []const u8,
    optional_params: []const ir.Ir.OptionalParam,
    key_params: []const ir.Ir.KeyParam,
    rest_param: ?[]const u8,
) Ir {
    const body_storage = struct {
        var body: Ir = .{ .lit = Value.nil };
    };
    return .{ .lambda = .{
        .params = params,
        .optional_params = optional_params,
        .key_params = key_params,
        .rest_param = rest_param,
        .captures = &.{},
        .body = &body_storage.body,
        .name = Value.nil,
    } };
}

fn allocChunkNamed(
    heap: *runtime.Heap,
    name: Value,
    arity: u8,
    opt_count: u8,
    key_count: u8,
    has_rest: bool,
) !Value {
    const code = [_]u8{};
    const constants = [_]Value{};
    const chunk_val = try heap.allocChunk(&code, &constants, arity, opt_count, key_count, has_rest, 0);
    chunk_val.toPtr(Chunk).name = name;
    return chunk_val;
}

test "collectLambdaCandidates: set_symbol_function with quote_sym" {
    var lambda_ir = makeLambdaIr(&.{}, &.{}, &.{}, null);
    var quote_sym_ir: Ir = .{ .quote_sym = "PKG::my-fn" };
    var set_ir: Ir = .{ .set_symbol_function = .{ .left = &quote_sym_ir, .right = &lambda_ir } };

    var candidates = std.ArrayList(LambdaCandidate){};
    defer {
        freeLambdaCandidates(testing.allocator, candidates.items);
        candidates.deinit(testing.allocator);
    }

    try collectLambdaCandidates(testing.allocator, &set_ir, &candidates);
    try testing.expectEqual(@as(usize, 1), candidates.items.len);
    try testing.expectEqualStrings("PKG::my-fn", candidates.items[0].name);
    try testing.expectEqualStrings("my-fn", candidates.items[0].local_name);
}

test "collectLambdaCandidates: set_symbol_function fallback name" {
    var lambda_ir = makeLambdaIr(&.{}, &.{}, &.{}, null);
    var left_ir: Ir = .{ .lit = Value.nil };
    var set_ir: Ir = .{ .set_symbol_function = .{ .left = &left_ir, .right = &lambda_ir } };

    var candidates = std.ArrayList(LambdaCandidate){};
    defer {
        freeLambdaCandidates(testing.allocator, candidates.items);
        candidates.deinit(testing.allocator);
    }

    try collectLambdaCandidates(testing.allocator, &set_ir, &candidates);
    try testing.expectEqual(@as(usize, 1), candidates.items.len);
    try testing.expectEqualStrings("<set-symbol-function>", candidates.items[0].name);
}

test "findMatchingChunk: matches signature and mutates used on success" {
    const allocator = testing.allocator;
    var heap = try runtime.Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const params = [_][]const u8{"x"};
    var lambda_ir = makeLambdaIrWithParams(&params, &.{}, &.{}, null);
    const name_sym = try heap.intern("MY-FN");
    const candidate = LambdaCandidate{
        .name = "MY-FN",
        .local_name = "MY-FN",
        .name_sym = name_sym,
        .lambda_ir = &lambda_ir,
    };

    const chunk_val = try allocChunkNamed(&heap, name_sym, 1, 0, 0, false);
    const chunks = [_]Value{chunk_val};
    var used = [_]bool{false};

    const matched = findMatchingChunk(&candidate, name_sym, &chunks, &used);
    try testing.expect(matched != null);
    try testing.expect(used[0]);
}

test "findMatchingChunk: signature mismatch keeps used unchanged" {
    const allocator = testing.allocator;
    var heap = try runtime.Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const params = [_][]const u8{"x"};
    var lambda_ir = makeLambdaIrWithParams(&params, &.{}, &.{}, null);
    const name_sym = try heap.intern("MY-FN");
    const candidate = LambdaCandidate{
        .name = "MY-FN",
        .local_name = "MY-FN",
        .name_sym = name_sym,
        .lambda_ir = &lambda_ir,
    };

    const chunk_val = try allocChunkNamed(&heap, name_sym, 0, 0, 0, false); // arity mismatch
    const chunks = [_]Value{chunk_val};
    var used = [_]bool{false};

    try testing.expectEqual(null, findMatchingChunk(&candidate, name_sym, &chunks, &used));
    try testing.expect(!used[0]);
}

test "findMatchingChunk: optional/key/rest signature dimensions must match" {
    const allocator = testing.allocator;
    var heap = try runtime.Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var body: Ir = .{ .lit = Value.nil };
    const opt_params = [_]ir.Ir.OptionalParam{.{ .name = "opt", .default = null }};
    const key_params = [_]ir.Ir.KeyParam{.{ .keyword = "k", .name = "k", .default = null }};
    var lambda_ir = makeLambdaIrWithParams(&.{}, &opt_params, &key_params, "rest");
    lambda_ir.lambda.body = &body;

    const name_sym = try heap.intern("SIG-FN");
    const candidate = LambdaCandidate{
        .name = "SIG-FN",
        .local_name = "SIG-FN",
        .name_sym = name_sym,
        .lambda_ir = &lambda_ir,
    };

    const chunk_opt_miss = try allocChunkNamed(&heap, name_sym, 0, 0, 1, true);
    const chunk_key_miss = try allocChunkNamed(&heap, name_sym, 0, 1, 0, true);
    const chunk_rest_miss = try allocChunkNamed(&heap, name_sym, 0, 1, 1, false);
    const chunk_match = try allocChunkNamed(&heap, name_sym, 0, 1, 1, true);
    const chunks = [_]Value{ chunk_opt_miss, chunk_key_miss, chunk_rest_miss, chunk_match };
    var used = [_]bool{ false, false, false, false };

    const matched = findMatchingChunk(&candidate, name_sym, &chunks, &used);
    try testing.expect(matched != null);
    try testing.expect(!used[0]);
    try testing.expect(!used[1]);
    try testing.expect(!used[2]);
    try testing.expect(used[3]);
}

test "findMatchingChunk: name mismatch keeps used unchanged" {
    const allocator = testing.allocator;
    var heap = try runtime.Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var lambda_ir = makeLambdaIrWithParams(&.{}, &.{}, &.{}, null);
    const name_sym = try heap.intern("EXPECTED");
    const other_sym = try heap.intern("OTHER");
    const candidate = LambdaCandidate{
        .name = "EXPECTED",
        .local_name = "EXPECTED",
        .name_sym = name_sym,
        .lambda_ir = &lambda_ir,
    };

    const chunk_val = try allocChunkNamed(&heap, other_sym, 0, 0, 0, false);
    const chunks = [_]Value{chunk_val};
    var used = [_]bool{false};

    try testing.expectEqual(null, findMatchingChunk(&candidate, name_sym, &chunks, &used));
    try testing.expect(!used[0]);
}

test "findMatchingChunk: case-insensitive local-name match with string chunk name" {
    const allocator = testing.allocator;
    var heap = try runtime.Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var lambda_ir = makeLambdaIrWithParams(&.{}, &.{}, &.{}, null);
    const string_name = try heap.allocBaseString("foo");
    const candidate = LambdaCandidate{
        .name = "PKG::Foo",
        .local_name = "Foo",
        .name_sym = Value.nil,
        .lambda_ir = &lambda_ir,
    };

    const chunk_val = try allocChunkNamed(&heap, string_name, 0, 0, 0, false);
    const chunks = [_]Value{chunk_val};
    var used = [_]bool{false};

    const matched = findMatchingChunk(&candidate, Value.nil, &chunks, &used);
    try testing.expect(matched != null);
    try testing.expect(used[0]);
}

test "findMatchingChunk: non-chunk child value is ignored safely" {
    var lambda_ir = makeLambdaIrWithParams(&.{}, &.{}, &.{}, null);
    const candidate = LambdaCandidate{
        .name = "test",
        .local_name = "test",
        .name_sym = Value.nil,
        .lambda_ir = &lambda_ir,
    };

    const chunks = [_]Value{Value.nil};
    var used = [_]bool{false};
    try testing.expectEqual(null, findMatchingChunk(&candidate, Value.nil, &chunks, &used));
    try testing.expect(!used[0]);
}

test "findMatchingChunk: pre-used entry is skipped" {
    const allocator = testing.allocator;
    var heap = try runtime.Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var lambda_ir = makeLambdaIrWithParams(&.{}, &.{}, &.{}, null);
    const name_sym = try heap.intern("FN");
    const candidate = LambdaCandidate{
        .name = "FN",
        .local_name = "FN",
        .name_sym = name_sym,
        .lambda_ir = &lambda_ir,
    };

    const chunk_val = try allocChunkNamed(&heap, name_sym, 0, 0, 0, false);
    const chunks = [_]Value{chunk_val};
    var used = [_]bool{true};

    try testing.expectEqual(null, findMatchingChunk(&candidate, name_sym, &chunks, &used));
    try testing.expect(used[0]);
}
