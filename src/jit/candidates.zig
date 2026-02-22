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
    name_raw: u64,
    lambda_ir: *const Ir,
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

    var local_name = stripQualifiedName(name);
    var name_raw: u64 = 0;
    if (lambda_ir.lambda.name.isSymbol()) {
        const sym = lambda_ir.lambda.name.toPtr(Symbol);
        local_name = sym.getName();
        name_raw = lambda_ir.lambda.name.raw;
    }

    try out.append(allocator, .{
        .name = name,
        .local_name = local_name,
        .name_raw = name_raw,
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
            if (op.left.* == .lit and op.left.lit.isSymbol() and op.right.* == .lambda) {
                const sym_name = op.left.lit.toPtr(Symbol).getName();
                try appendCandidate(allocator, out, sym_name, op.right);
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

pub fn isEligible(lambda_ir: *const Ir) bool {
    if (lambda_ir.* != .lambda) return false;
    const lambda = lambda_ir.lambda;

    // Require explicit (optimize (speed 3) (safety 0)) for JIT.
    if (lambda.speed < 3 or lambda.safety > 0) return false;
    // Skip functions whose body is just a type assertion.
    if (lambda.body.* == .assert_fixnum) return false;
    // Keep current bridge constraints.
    if (lambda.captures.len > 0) return false;
    if (lambda.optional_params.len > 0) return false;
    if (lambda.key_params.len > 0) return false;
    if (lambda.rest_param != null) return false;

    return true;
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

fn chunkNameMatches(candidate: *const LambdaCandidate, chunk: *const Chunk) bool {
    if (candidate.name_raw != 0 and chunk.name.raw == candidate.name_raw) return true;

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
    child_chunks: []const Value,
    used: []bool,
) ?*const Chunk {
    if (child_chunks.len == 0 or child_chunks.len != used.len) return null;

    for (child_chunks, 0..) |chunk_val, idx| {
        if (used[idx]) continue;
        const chunk = chunk_val.toPtr(Chunk);
        if (!chunkSignatureMatches(candidate.lambda_ir, chunk)) continue;
        if (!chunkNameMatches(candidate, chunk)) continue;
        used[idx] = true;
        return chunk;
    }

    for (child_chunks, 0..) |chunk_val, idx| {
        if (used[idx]) continue;
        const chunk = chunk_val.toPtr(Chunk);
        if (!chunkSignatureMatches(candidate.lambda_ir, chunk)) continue;
        used[idx] = true;
        return chunk;
    }
    return null;
}
