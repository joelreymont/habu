//! Bytecode disassembler for debugging
//!
//! Pretty-prints bytecode in human-readable format.

const std = @import("std");
const opcodes = @import("opcodes.zig");
const Op = opcodes.Op;

/// Disasm-only chunk shape for tests and standalone bytecode dumps.
/// Runtime uses `runtime.objects.Chunk` (see disassembleRuntime).
const Chunk = struct {
    code: []u8,
    constants: []u64,
    arity: u8,
    optional_count: u8,
    key_count: u8,
    has_rest: bool,
    num_locals: u8,
    name: []const u8,

    pub fn readU8(self: *const Chunk, offset: usize) u8 {
        return self.code[offset];
    }

    pub fn readU16(self: *const Chunk, offset: usize) u16 {
        return @as(u16, self.code[offset]) |
            (@as(u16, self.code[offset + 1]) << 8);
    }

    pub fn readI16(self: *const Chunk, offset: usize) i16 {
        return @bitCast(self.readU16(offset));
    }

    pub fn readI32(self: *const Chunk, offset: usize) i32 {
        return @bitCast(@as(u32, self.code[offset]) |
            (@as(u32, self.code[offset + 1]) << 8) |
            (@as(u32, self.code[offset + 2]) << 16) |
            (@as(u32, self.code[offset + 3]) << 24));
    }
};

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

    if (offset + 1 >= chunk.code.len) return error.UnexpectedEof;
    const opcode = chunk.readU16(offset);
    const op = try std.meta.intToEnum(Op, opcode);

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
        .quot,
        .rem,
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
        .integerp,
        .realp,
        .stringp,
        .vectorp,
        .closurep,
        .keywordp,
        .nilp,
        .vec_ref,
        .vec_set,
        .elt_set,
        .vec_len,
        .slot_value,
        .set_slot_value,
        .make_generic_function,
        .make_method,
        .set_gf_dispatcher,
        .add_method,
        .make_unbound,
        .slot_boundp,
        .slot_makunbound,
        .make_box,
        .box_ref,
        .box_set,
        .str_ref,
        .str_set,
        .str_len,
        .str_concat,
        .str_eq,
        .str_lt,
        .str_gt,
        .str_le,
        .str_ge,
        .ret,
        .print,
        .write,
        .random,
        .random_seed,
        .type_of,
        .class_of,
        .find_class,
        .class_name,
        .class_direct_superclasses,
        .class_precedence_list,
        .class_direct_slots,
        .class_slots,
        .slot_definition_name,
        .slot_definition_initform,
        .slot_definition_initargs,
        .slot_definition_readers,
        .slot_definition_writers,
        .slot_definition_allocation,
        .slot_definition_type,
        .intern,
        .make_symbol,
        .unintern,
        .substring,
        .sym_name,
        .copy_symbol,
        .makunbound,
        .set_sym_val,
        .get,
        .put,
        .remprop,
        .set_macro_character,
        .get_macro_character,
        .set_dispatch_macro_character,
        .get_dispatch_macro_character,
        .read_line,
        .write_line,
        .write_string,
        .read_byte,
        .write_byte,
        .file_position,
        .file_length,
        .finish_output,
        .force_output,
        .clear_input,
        .clear_output,
        .sleep,
        .delete_file,
        .rename_file,
        .probe_file,
        .file_write_date,
        .file_author,
        .file_string_length,
        .packagep,
        .symbol_package,
        .package_name,
        .package_nicknames,
        .package_use_list,
        .package_used_by_list,
        .package_shadowing_symbols,
        .list_all_packages,
        .find_package,
        .delete_package,
        .pkg_export,
        .pkg_import,
        .pkg_use_package,
        .pkg_unexport,
        .pkg_shadow,
        .pkg_shadowing_import,
        .pkg_unuse_package,
        .pkg_unintern,
        .pkg_find_symbol,
        .pkg_find_all_symbols,
        .pkg_make_package,
        .pkg_rename_package,
        .apropos_list,
        .read_char_no_hang,
        .compute_restarts,
        .restart_name,
        .directory,
        .pathname_match_p,
        .enough_namestring,
        .decode_float,
        .integer_decode_float,
        .float_radix,
        .float_digits,
        .get_universal_time,
        .get_internal_real_time,
        .get_internal_run_time,
        .get_decoded_time,
        .decode_universal_time,
        .room,
        .lisp_implementation_type,
        .lisp_implementation_version,
        .software_type,
        .machine_type,
        .machine_instance,
        .machine_version,
        .software_version,
        .short_site_name,
        .long_site_name,
        .user_homedir_pathname,
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
        .pop_block,
        .throw,
        .write_to_stream,
        .hash_get,
        .sxhash,
        .hash_set,
        .hash_rem,
        .hash_count,
        .hash_capacity,
        .hash_clear,
        .hash_test,
        .hash_keys,
        .hash_alist,
        .hashtablep,
        .method_qualifiers,
        .method_specializers,
        .method_function,
        .generic_function_methods,
        .generic_function_lambda_list,
        .generic_function_name,
        .rationalp,
        .complexp,
        .make_complex,
        .real_part,
        .imag_part,
        .numerator,
        .denominator,
        .rational,
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
        .listen,
        .upgraded_complex_part_type,
        .boundp,
        .fboundp,
        .symbol_value,
        .symbol_function,
        .typep,
        .subtypep,
        .abs,
        .zerop,
        .plusp,
        .minusp,
        .evenp,
        .oddp,
        .sqrt,
        .sin,
        .cos,
        .tan,
        .exp,
        .log,
        .floor,
        .ceiling,
        .round,
        .mv_list,
        .values_list,
        .read,
        .load,
        .read_from_string,
        .eval,
        .gensym,
        .macroexpand,
        .macroexpand_1,
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
        .lognand,
        .lognor,
        .logandc1,
        .logandc2,
        .logeqv,
        .logbitp,
        .logcount,
        .integer_length,
        .read_file,
        .write_file,
        .make_string,
        .math_ext,
        .list_to_string,
        .string_upcase,
        .string_downcase,
        .listp,
        .atom,
        .assoc,
        .equal,
        .eql,
        .equalp,
        .copy_structure,
        .function_lambda_expression,
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
        .list_count,
        .list_count_eq,
        .list_count_equal,
        .list_remove,
        .list_remove_eq,
        .list_remove_equal,
        .invoke_restart,
        .find_restart,
        .handler_bind,
        .streamp,
        .input_stream_p,
        .output_stream_p,
        .make_string_input_stream,
        .make_string_output_stream,
        .get_output_stream_string,
        .close,
        .array_dimension,
        .array_dimensions,
        .pathname,
        .parse_namestring,
        .namestring,
        .directory_namestring,
        .file_namestring,
        .host_namestring,
        .wild_pathname_p,
        .open_stream_p,
        .interactive_stream_p,
        .stream_element_type,
        .stream_external_format,
        .merge_pathnames,
        .pathname_host,
        .pathname_device,
        .pathname_directory,
        .pathname_name,
        .pathname_type,
        .pathname_version,
        .truename,
        .ensure_directories_exist,
        .package_symbols_table,
        .package_exports_table,
        .package_symbols_list,
        .package_exports_list,
        .find_symbol,
        .push_progv,
        .pop_progv,
        .vec_fill_ptr,
        .vec_push,
        .vec_push_ext,
        .vec_pop,
        .vec_set_fill_ptr,
        .vec_set_adjustable,
        .vec_adjust,
        // Compound stream opcodes (no operand)
        .make_echo_stream,
        .make_synonym_stream,
        .make_two_way_stream,
        .make_broadcast_stream_list,
        .make_concatenated_stream_list,
        .broadcast_stream_streams,
        .concatenated_stream_streams,
        .echo_stream_input_stream,
        .echo_stream_output_stream,
        .synonym_stream_symbol,
        .two_way_stream_input_stream,
        .two_way_stream_output_stream,
        .disassemble,
        .read_char_stream,
        .peek_char_stream,
        .open_file,
        .close_stream,
        => {
            try writer.print("{s}\n", .{op.name()});
            return offset + 2;
        },

        // 1 byte operand
        .load_local, .store_local, .load_capture, .call, .tail_call, .make_list, .make_vec_n, .values, .mv_bind, .format, .enter_scope, .exit_scope, .pop_restarts, .open, .make_array, .aref, .aset, .make_pathname, .encode_universal_time, .make_broadcast_stream, .make_concatenated_stream => {
            const operand = chunk.readU8(offset + 2);
            try writer.print("{s} {d}\n", .{ op.name(), operand });
            return offset + 3;
        },

        // 2 byte operand (u16)
        .push_const, .load_global, .store_global, .make_vec, .make_hash, .find_key, .check_or, .return_from => {
            const operand = chunk.readU16(offset + 2);
            try writer.print("{s} {d}\n", .{ op.name(), operand });
            return offset + 4;
        },

        // 2 byte operand (i16 jump)
        .jmp, .jmp_nil, .jmp_not_nil, .push_catch, .push_unwind, .pop_unwind, .push_restart => {
            const displacement = chunk.readI16(offset + 2);
            const target = @as(i32, @intCast(offset)) + 4 + displacement;
            try writer.print("{s} {d} (-> {d})\n", .{ op.name(), displacement, target });
            return offset + 4;
        },

        // 2 byte operand (depth, index)
        .load_upvalue, .store_upvalue => {
            const depth = chunk.readU8(offset + 2);
            const index = chunk.readU8(offset + 3);
            try writer.print("{s} depth={d} index={d}\n", .{ op.name(), depth, index });
            return offset + 4;
        },

        // 3 byte operand (code index + captures)
        .make_closure => {
            const code_idx = chunk.readU16(offset + 2);
            const num_captures = chunk.readU8(offset + 4);
            try writer.print("{s} code={d} captures={d}\n", .{ op.name(), code_idx, num_captures });
            return offset + 5;
        },

        // 4 byte operand
        .push_i32 => {
            const val = chunk.readI32(offset + 2);
            try writer.print("{s} {d}\n", .{ op.name(), val });
            return offset + 6;
        },

        // 4 byte operand (i16 exit offset + u16 name index)
        .push_block => {
            const displacement = chunk.readI16(offset + 2);
            const name_idx = chunk.readU16(offset + 4);
            const target = @as(i32, @intCast(offset)) + 6 + displacement;
            try writer.print("{s} name={d} exit={d} (-> {d})\n", .{ op.name(), name_idx, displacement, target });
            return offset + 6;
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

/// Disassemble a runtime chunk (different struct layout)
pub fn disassembleRuntime(chunk: *const @import("../runtime/objects.zig").Chunk, writer: anytype) !void {
    try writer.print("; Function: <anonymous>\n", .{});
    try writer.print("; Arity: {d}, Locals: {d}\n", .{ chunk.arity, chunk.num_locals });
    try writer.writeAll("; Constants:\n");

    const constants = chunk.getConstants();
    for (constants, 0..) |c, i| {
        try writer.print(";   [{d}] = 0x{x:0>16}\n", .{ i, c.raw });
    }

    try writer.writeAll("; Code:\n");

    const code = chunk.getCode();
    var offset: usize = 0;
    while (offset < code.len) {
        offset = try disassembleInstructionRuntime(chunk, offset, writer);
    }
}

fn disassembleInstructionRuntime(chunk: *const @import("../runtime/objects.zig").Chunk, offset: usize, writer: anytype) !usize {
    try writer.print("{d:0>4}  ", .{offset});

    const code = chunk.getCode();
    if (offset + 1 >= code.len) return error.UnexpectedEof;
    const low: u16 = code[offset];
    const high: u16 = code[offset + 1];
    const opcode = low | (high << 8);
    const op = try std.meta.intToEnum(Op, opcode);

    // Simplified: just print opcode name and advance by operand size
    const size = op.operandSize();
    if (size == 0) {
        try writer.print("{s}\n", .{op.name()});
        return offset + 2;
    } else {
        try writer.print("{s}", .{op.name()});
        for (0..size) |i| {
            const b = if (offset + 2 + i < code.len) code[offset + 2 + i] else 0;
            try writer.print(" {d}", .{b});
        }
        try writer.writeAll("\n");
        return offset + 2 + size;
    }
}

// ============================================================================
// Tests
// ============================================================================

test "disassemble simple" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const code = [_]u8{
        @truncate(@as(u16, @intFromEnum(Op.push_nil))),
        @truncate(@as(u16, @intFromEnum(Op.push_nil)) >> 8),
        @truncate(@as(u16, @intFromEnum(Op.push_t))),
        @truncate(@as(u16, @intFromEnum(Op.push_t)) >> 8),
        @truncate(@as(u16, @intFromEnum(Op.cons))),
        @truncate(@as(u16, @intFromEnum(Op.cons)) >> 8),
        @truncate(@as(u16, @intFromEnum(Op.ret))),
        @truncate(@as(u16, @intFromEnum(Op.ret)) >> 8),
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

test "disassemble invalid opcode errors" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const code = [_]u8{ 0xFF, 0xFF };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 0,
        .name = "bad",
    };

    try testing.expectError(error.InvalidEnumTag, disassembleToString(allocator, &chunk));
}

test "disassemble with operands" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const code = [_]u8{
        @truncate(@as(u16, @intFromEnum(Op.push_i32))),
        @truncate(@as(u16, @intFromEnum(Op.push_i32)) >> 8),
        42,
        0,
        0,
        0, // 42 as i32 LE
        @truncate(@as(u16, @intFromEnum(Op.load_local))),
        @truncate(@as(u16, @intFromEnum(Op.load_local)) >> 8),
        5, // local 5
        @truncate(@as(u16, @intFromEnum(Op.ret))),
        @truncate(@as(u16, @intFromEnum(Op.ret)) >> 8),
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
        @truncate(@as(u16, @intFromEnum(Op.jmp_nil))),
        @truncate(@as(u16, @intFromEnum(Op.jmp_nil)) >> 8),
        5,
        0, // displacement = 5
        @truncate(@as(u16, @intFromEnum(Op.push_t))),
        @truncate(@as(u16, @intFromEnum(Op.push_t)) >> 8),
        @truncate(@as(u16, @intFromEnum(Op.jmp))),
        @truncate(@as(u16, @intFromEnum(Op.jmp)) >> 8),
        3,
        0, // displacement = 3
        @truncate(@as(u16, @intFromEnum(Op.push_nil))),
        @truncate(@as(u16, @intFromEnum(Op.push_nil)) >> 8),
        @truncate(@as(u16, @intFromEnum(Op.ret))),
        @truncate(@as(u16, @intFromEnum(Op.ret)) >> 8),
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
