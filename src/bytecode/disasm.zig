//! Bytecode disassembler for debugging
//!
//! Pretty-prints bytecode in human-readable format.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const Op = opcodes.Op;
const Chunk = opcodes.Chunk;

/// Disassemble a chunk to a writer
pub fn disassemble(chunk: *const Chunk, writer: anytype) !void {
    try writer.print("; Function: {s}\n", .{chunk.name});
    try writer.print("; Arity: {d}, Locals: {d}\n", .{ chunk.arity, chunk.num_locals });
    try writer.writeAll("; Constants:\n");

    for (chunk.constants, 0..) |c, i| {
        try writer.print(";   [{d}] = 0x{x:0>16}\n", .{ i, c });
    }

    try writer.writeAll("; Code:\n");

    var offset: usize = 0;
    while (offset < chunk.code.len) {
        offset = try disassembleInstruction(chunk, offset, writer);
    }
}

/// Disassemble a single instruction, return next offset
pub fn disassembleInstruction(chunk: *const Chunk, offset: usize, writer: anytype) !usize {
    try writer.print("{d:0>4}  ", .{offset});

    const opcode_byte = chunk.code[offset];
    const op = std.meta.intToEnum(Op, opcode_byte) catch {
        try writer.print("??? (0x{x:0>2})\n", .{opcode_byte});
        return offset + 1;
    };

    return switch (op) {
        // No operand
        .push_nil,
        .push_t,
        .dup,
        .pop,
        .swap,
        .load_argc,
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .neg,
        .eq,
        .lt,
        .gt,
        .le,
        .ge,
        .num_eq,
        .not,
        .cons,
        .car,
        .cdr,
        .append_lists,
        .list_length,
        .list_reverse,
        .list_nth,
        .list_last,
        .list_member,
        .list_nthcdr,
        .consp,
        .symbolp,
        .numberp,
        .stringp,
        .vectorp,
        .closurep,
        .keywordp,
        .nilp,
        .vec_ref,
        .vec_set,
        .vec_len,
        .slot_value,
        .make_box,
        .box_ref,
        .box_set,
        .str_ref,
        .str_len,
        .str_concat,
        .ret,
        .print,
        .random,
        .random_seed,
        .type_of,
        .intern,
        .substring,
        .sym_name,
        .get,
        .put,
        .remprop,
        .str_eq,
        .halt,
        .check_fixnum,
        .check_cons,
        .check_symbol,
        .check_string,
        .check_vector,
        .check_closure,
        .check_non_nil,
        .check_list,
        .check_refine,
        .apply,
        .pop_catch,
        .throw,
        .hash_get,
        .hash_get_default,
        .hash_set,
        .hash_rem,
        .hash_count,
        .hash_clear,
        .hash_test,
        .hashtablep,
        .rationalp,
        .complexp,
        .make_complex,
        .real_part,
        .imag_part,
        .numerator,
        .denominator,
        .characterp,
        .floatp,
        .char_code,
        .code_char,
        .char_eq,
        .char_lt,
        .char_gt,
        .read_char,
        .peek_char,
        .unread_char,
        .boundp,
        .fboundp,
        .symbol_value,
        .symbol_function,
        .typep,
        .abs,
        .zerop,
        .plusp,
        .minusp,
        .evenp,
        .oddp,
        .mv_list,
        .read,
        .load,
        .read_from_string,
        .eval,
        .gensym,
        .macroexpand,
        .princ,
        .terpri,
        .write_char,
        .char_upcase,
        .char_downcase,
        .digit_char_p,
        .alpha_char_p,
        .parse_integer,
        .write_to_string,
        .logand,
        .logior,
        .logxor,
        .lognot,
        .ash,
        .read_file,
        .write_file,
        .make_string,
        .string_to_list,
        .list_to_string,
        .string_upcase,
        .string_downcase,
        .listp,
        .atom,
        .assoc,
        .equal,
        .eql,
        .rplaca,
        .rplacd,
        .error_user,
        .list_member_eql,
        .list_member_equal,
        .assoc_eql,
        .assoc_equal,
        .list_find,
        .list_find_eq,
        .list_find_equal,
        .list_position,
        .list_position_eq,
        .list_position_equal,
        .list_count,
        .list_count_eq,
        .list_count_equal,
        .list_remove,
        .list_remove_eq,
        .list_remove_equal,
        .invoke_restart,
        .find_restart,
        .streamp,
        .input_stream_p,
        .output_stream_p,
        .make_string_input_stream,
        .make_string_output_stream,
        .get_output_stream_string,
        .close,
        => {
            try writer.print("{s}\n", .{op.name()});
            return offset + 1;
        },

        // 1 byte operand
        .load_local, .store_local, .load_capture, .call, .tail_call, .make_list, .make_vec_n, .values, .mv_bind, .format, .enter_scope, .exit_scope, .pop_restarts, .open, .make_array => {
            const operand = chunk.readU8(offset + 1);
            try writer.print("{s} {d}\n", .{ op.name(), operand });
            return offset + 2;
        },

        // 2 byte operand (u16)
        .push_const, .load_global, .store_global, .make_vec, .make_hash, .find_key => {
            const operand = chunk.readU16(offset + 1);
            try writer.print("{s} {d}\n", .{ op.name(), operand });
            return offset + 3;
        },

        // 2 byte operand (i16 jump)
        .jmp, .jmp_nil, .jmp_not_nil, .push_catch, .push_unwind, .pop_unwind, .push_restart => {
            const displacement = chunk.readI16(offset + 1);
            const target = @as(i32, @intCast(offset)) + 3 + displacement;
            try writer.print("{s} {d} (-> {d})\n", .{ op.name(), displacement, target });
            return offset + 3;
        },

        // 2 byte operand (depth, index)
        .load_upvalue, .store_upvalue => {
            const depth = chunk.readU8(offset + 1);
            const index = chunk.readU8(offset + 2);
            try writer.print("{s} depth={d} index={d}\n", .{ op.name(), depth, index });
            return offset + 3;
        },

        // 3 byte operand (code index + captures)
        .make_closure => {
            const code_idx = chunk.readU16(offset + 1);
            const num_captures = chunk.readU8(offset + 3);
            try writer.print("{s} code={d} captures={d}\n", .{ op.name(), code_idx, num_captures });
            return offset + 4;
        },

        // 4 byte operand
        .push_i32 => {
            const val = chunk.readI32(offset + 1);
            try writer.print("{s} {d}\n", .{ op.name(), val });
            return offset + 5;
        },
    };
}

/// Disassemble to string (for testing)
pub fn disassembleToString(allocator: std.mem.Allocator, chunk: *const Chunk) ![]u8 {
    var list = std.ArrayList(u8){};
    defer list.deinit(allocator);

    try disassemble(chunk, list.writer(allocator));
    return list.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "disassemble simple" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const code = [_]u8{
        @intFromEnum(Op.push_nil),
        @intFromEnum(Op.push_t),
        @intFromEnum(Op.cons),
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 0,
        .name = "test",
    };

    const result = try disassembleToString(allocator, &chunk);
    defer allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "push_nil") != null);
    try testing.expect(std.mem.indexOf(u8, result, "push_t") != null);
    try testing.expect(std.mem.indexOf(u8, result, "cons") != null);
    try testing.expect(std.mem.indexOf(u8, result, "ret") != null);
}

test "disassemble with operands" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const code = [_]u8{
        @intFromEnum(Op.push_i32),
        42,
        0,
        0,
        0, // 42 as i32 LE
        @intFromEnum(Op.load_local),
        5, // local 5
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 2,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 6,
        .name = "with_args",
    };

    const result = try disassembleToString(allocator, &chunk);
    defer allocator.free(result);

    try testing.expect(std.mem.indexOf(u8, result, "push_i32 42") != null);
    try testing.expect(std.mem.indexOf(u8, result, "load_local 5") != null);
    try testing.expect(std.mem.indexOf(u8, result, "Arity: 2") != null);
}

test "disassemble jump" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // jmp_nil with displacement of 5
    const code = [_]u8{
        @intFromEnum(Op.jmp_nil),
        5,
        0, // displacement = 5
        @intFromEnum(Op.push_t),
        @intFromEnum(Op.jmp),
        3,
        0, // displacement = 3
        @intFromEnum(Op.push_nil),
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 0,
        .name = "jumpy",
    };

    const result = try disassembleToString(allocator, &chunk);
    defer allocator.free(result);

    // Should show target addresses
    try testing.expect(std.mem.indexOf(u8, result, "jmp_nil") != null);
    try testing.expect(std.mem.indexOf(u8, result, "->") != null);
}
