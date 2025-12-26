//! Bytecode opcode definitions for Habu VM
//!
//! Stack-based bytecode with 1-byte opcode + optional operands.
//! Designed for simplicity and portability (WASM target).
//!
//! Instruction format:
//! - Opcode: 1 byte
//! - Operand: 0-4 bytes depending on opcode
//!
//! Stack convention: (... before -- ... after)
//! - Values are pushed/popped from top of stack
//! - Binary ops: pop right, pop left, push result

const std = @import("std");

/// Bytecode opcodes
pub const Op = enum(u8) {
    // ========================================================================
    // Stack manipulation
    // ========================================================================

    /// Push nil onto stack
    /// ( -- nil )
    push_nil = 0x00,

    /// Push t (true) onto stack
    /// ( -- t )
    push_t = 0x01,

    /// Push a fixnum literal (operand: i32)
    /// ( -- fixnum )
    push_i32 = 0x02,

    /// Push constant from pool (operand: u16 index)
    /// ( -- const )
    push_const = 0x03,

    /// Duplicate top of stack
    /// ( x -- x x )
    dup = 0x04,

    /// Pop and discard top of stack
    /// ( x -- )
    pop = 0x05,

    /// Swap top two values
    /// ( x y -- y x )
    swap = 0x06,

    // ========================================================================
    // Variable access
    // ========================================================================

    /// Load local variable (operand: u8 index)
    /// ( -- val )
    load_local = 0x10,

    /// Store to local variable (operand: u8 index)
    /// ( val -- )
    store_local = 0x11,

    /// Load from closure capture (operand: u8 index)
    /// ( -- val )
    load_capture = 0x12,

    /// Load from upvalue (operand: u8 depth, u8 index)
    /// ( -- val )
    load_upvalue = 0x13,

    /// Store to upvalue (operand: u8 depth, u8 index)
    /// ( val -- )
    store_upvalue = 0x14,

    /// Load global by name index (operand: u16 name index)
    /// ( -- val )
    load_global = 0x15,

    /// Store to global (operand: u16 name index)
    /// ( val -- )
    store_global = 0x16,

    // ========================================================================
    // Arithmetic
    // ========================================================================

    /// Addition
    /// ( a b -- a+b )
    add = 0x20,

    /// Subtraction
    /// ( a b -- a-b )
    sub = 0x21,

    /// Multiplication
    /// ( a b -- a*b )
    mul = 0x22,

    /// Division
    /// ( a b -- a/b )
    div = 0x23,

    /// Modulo
    /// ( a b -- a%b )
    mod = 0x24,

    /// Negate
    /// ( a -- -a )
    neg = 0x25,

    // ========================================================================
    // Comparison
    // ========================================================================

    /// Equal (eq)
    /// ( a b -- t/nil )
    eq = 0x30,

    /// Less than
    /// ( a b -- t/nil )
    lt = 0x31,

    /// Greater than
    /// ( a b -- t/nil )
    gt = 0x32,

    /// Less than or equal
    /// ( a b -- t/nil )
    le = 0x33,

    /// Greater than or equal
    /// ( a b -- t/nil )
    ge = 0x34,

    /// Numeric equal (=)
    /// ( a b -- t/nil )
    num_eq = 0x35,

    // ========================================================================
    // Logical
    // ========================================================================

    /// Logical not
    /// ( a -- t/nil )
    not = 0x38,

    // ========================================================================
    // List operations
    // ========================================================================

    /// Cons
    /// ( car cdr -- cons )
    cons = 0x40,

    /// Car
    /// ( cons -- car )
    car = 0x41,

    /// Cdr
    /// ( cons -- cdr )
    cdr = 0x42,

    /// Make list from N elements (operand: u8 count)
    /// ( e1 e2 ... eN -- list )
    make_list = 0x43,

    /// Append two lists
    /// ( list1 list2 -- result )
    append_lists = 0x44,

    // ========================================================================
    // Type predicates
    // ========================================================================

    /// Check if cons
    /// ( x -- t/nil )
    consp = 0x50,

    /// Check if symbol
    /// ( x -- t/nil )
    symbolp = 0x51,

    /// Check if number (fixnum)
    /// ( x -- t/nil )
    numberp = 0x52,

    /// Check if string
    /// ( x -- t/nil )
    stringp = 0x53,

    /// Check if vector
    /// ( x -- t/nil )
    vectorp = 0x54,

    /// Check if closure
    /// ( x -- t/nil )
    closurep = 0x55,

    /// Check if keyword
    /// ( x -- t/nil )
    keywordp = 0x56,

    /// Check if nil
    /// ( x -- t/nil )
    nilp = 0x57,

    // ========================================================================
    // Vector operations
    // ========================================================================

    /// Create vector (operand: u16 size)
    /// ( -- vec )
    make_vec = 0x60,

    /// Vector ref
    /// ( vec idx -- val )
    vec_ref = 0x61,

    /// Vector set
    /// ( vec idx val -- )
    vec_set = 0x62,

    /// Vector length
    /// ( vec -- len )
    vec_len = 0x63,

    // ========================================================================
    // String operations
    // ========================================================================

    /// String ref
    /// ( str idx -- char )
    str_ref = 0x68,

    /// String length
    /// ( str -- len )
    str_len = 0x69,

    /// String concat
    /// ( s1 s2 -- s3 )
    str_concat = 0x6A,

    /// Intern - create symbol from string
    /// ( str -- sym )
    intern = 0x6B,

    /// Substring - extract part of a string
    /// ( str start end -- substr )
    substring = 0x6C,

    /// Symbol name - get name of symbol as string
    /// ( sym -- str )
    sym_name = 0x6D,

    /// String equal
    /// ( s1 s2 -- bool )
    str_eq = 0x6E,

    // ========================================================================
    // Control flow
    // ========================================================================

    /// Unconditional jump (operand: i16 offset)
    /// ( -- )
    jmp = 0x70,

    /// Jump if nil (operand: i16 offset)
    /// ( val -- )
    jmp_nil = 0x71,

    /// Jump if not nil (operand: i16 offset)
    /// ( val -- )
    jmp_not_nil = 0x72,

    // ========================================================================
    // Function calls
    // ========================================================================

    /// Call function (operand: u8 argc)
    /// ( fn arg1 ... argN -- result )
    call = 0x80,

    /// Tail call (operand: u8 argc)
    /// ( fn arg1 ... argN -- result )
    tail_call = 0x81,

    /// Return from function
    /// ( result -- )
    ret = 0x82,

    /// Make closure (operand: u16 code index, u8 num_captures)
    /// ( cap1 ... capN -- closure )
    make_closure = 0x83,

    /// Apply function to list of args
    /// ( fn args-list -- result )
    apply = 0x84,

    // ========================================================================
    // I/O
    // ========================================================================

    /// Print value
    /// ( val -- )
    print = 0x90,

    /// Random number [0, n)
    /// ( n -- rand )
    random = 0x91,

    /// Get type of value as symbol
    /// ( x -- type-sym )
    type_of = 0x92,

    // ========================================================================
    // Type assertions (gradual typing)
    // ========================================================================

    /// Assert value is fixnum, error if not
    /// ( x -- x )
    check_fixnum = 0xA0,

    /// Assert value is cons, error if not
    /// ( x -- x )
    check_cons = 0xA1,

    /// Assert value is symbol, error if not
    /// ( x -- x )
    check_symbol = 0xA2,

    /// Assert value is string, error if not
    /// ( x -- x )
    check_string = 0xA3,

    /// Assert value is vector, error if not
    /// ( x -- x )
    check_vector = 0xA4,

    /// Assert value is closure, error if not
    /// ( x -- x )
    check_closure = 0xA5,

    /// Assert value is non-nil, error if nil
    /// ( x -- x )
    check_non_nil = 0xA6,

    /// Assert value is list (nil or cons), error if not
    /// ( x -- x )
    check_list = 0xA7,

    // ========================================================================
    // Dynamic exception handling (catch/throw)
    // ========================================================================

    /// Push catch frame (operand: i16 offset to catch handler)
    /// Pops tag from stack, saves state for unwinding
    /// ( tag -- )
    push_catch = 0xB0,

    /// Pop catch frame on normal exit
    /// ( -- )
    pop_catch = 0xB1,

    /// Throw to matching catch (tag on stack, value on stack)
    /// ( tag value -- )
    throw = 0xB2,

    // ========================================================================
    // Special
    // ========================================================================

    /// Halt execution
    /// ( -- )
    halt = 0xFF,

    /// Get operand size in bytes
    pub fn operandSize(self: Op) u8 {
        return switch (self) {
            // No operand
            .push_nil, .push_t, .dup, .pop, .swap,
            .add, .sub, .mul, .div, .mod, .neg,
            .eq, .lt, .gt, .le, .ge, .num_eq, .not,
            .cons, .car, .cdr, .append_lists,
            .consp, .symbolp, .numberp, .stringp, .vectorp, .closurep, .keywordp, .nilp,
            .vec_ref, .vec_set, .vec_len,
            .str_ref, .str_len, .str_concat,
            .ret, .print, .random, .type_of, .intern, .substring, .sym_name, .str_eq, .halt,
            .check_fixnum, .check_cons, .check_symbol, .check_string,
            .check_vector, .check_closure, .check_non_nil, .check_list,
            .apply, .pop_catch, .throw,
            => 0,

            // 1 byte operand
            .load_local, .store_local, .load_capture,
            .call, .tail_call, .make_list,
            => 1,

            // 2 byte operand
            .push_const, .load_global, .store_global,
            .make_vec, .jmp, .jmp_nil, .jmp_not_nil,
            .load_upvalue, .store_upvalue, .push_catch,
            => 2,

            // 3 byte operand
            .make_closure, // u16 code index + u8 captures
            => 3,

            // 4 byte operand
            .push_i32,
            => 4,
        };
    }

    /// Get the name of the opcode for debugging
    pub fn name(self: Op) []const u8 {
        return @tagName(self);
    }
};

/// Bytecode chunk - compiled function
pub const Chunk = struct {
    /// Bytecode instructions
    code: []u8,
    /// Constant pool
    constants: []const u64, // Values stored as raw u64

    /// Arity (number of parameters)
    arity: u8,
    /// Number of local variables
    num_locals: u8,
    /// Function name (for debugging)
    name: []const u8,

    /// Read operand at offset
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

// ============================================================================
// Tests
// ============================================================================

test "opcode size" {
    const testing = std.testing;

    try testing.expectEqual(@as(u8, 0), Op.push_nil.operandSize());
    try testing.expectEqual(@as(u8, 4), Op.push_i32.operandSize());
    try testing.expectEqual(@as(u8, 1), Op.load_local.operandSize());
    try testing.expectEqual(@as(u8, 2), Op.jmp.operandSize());
    try testing.expectEqual(@as(u8, 3), Op.make_closure.operandSize());
}

test "opcode names" {
    const testing = std.testing;

    try testing.expectEqualStrings("push_nil", Op.push_nil.name());
    try testing.expectEqualStrings("add", Op.add.name());
    try testing.expectEqualStrings("call", Op.call.name());
}

test "chunk read" {
    const testing = std.testing;

    const code = [_]u8{ 0x12, 0x34, 0x56, 0x78 };
    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .num_locals = 0,
        .name = "test",
    };

    try testing.expectEqual(@as(u8, 0x12), chunk.readU8(0));
    try testing.expectEqual(@as(u16, 0x3412), chunk.readU16(0)); // Little endian
    try testing.expectEqual(@as(i32, 0x78563412), chunk.readI32(0));
}
