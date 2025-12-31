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

    /// Load argument count for current function call
    /// ( -- argc )
    load_argc = 0x17,

    /// Find keyword argument value (operand: u16 keyword constant index)
    /// Scans args from (arity + optional_count + key_count) to argc for keyword
    /// ( -- found_flag value )
    /// found_flag is t if keyword found, nil if not
    /// value is the keyword's value if found, nil if not
    find_key = 0x18,

    /// Enter a let scope (operand: u8 num_locals)
    /// Reserves slots for locals and pushes a scope frame
    /// ( -- [num_locals nil values] )
    enter_scope = 0x19,

    /// Exit a let scope (operand: u8 num_locals)
    /// Pops scope frame and removes reserved local slots, keeping result on top
    /// ( [locals...] result -- result )
    exit_scope = 0x1A,

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

    /// List length
    /// ( list -- fixnum )
    list_length = 0x45,

    /// Reverse a list
    /// ( list -- reversed )
    list_reverse = 0x46,

    /// Get nth element (0-indexed)
    /// ( list n -- element )
    list_nth = 0x47,

    /// Get last cons cell
    /// ( list -- last-cons )
    list_last = 0x48,

    /// Find element in list (using eq)
    /// ( item list -- tail-or-nil )
    list_member = 0x49,

    /// Get nthcdr (drop first n elements)
    /// ( list n -- tail )
    list_nthcdr = 0x4A,

    /// Mutate car of cons cell (destructive!)
    /// ( cons value -- value )
    rplaca = 0x4B,

    /// Mutate cdr of cons cell (destructive!)
    /// ( cons value -- value )
    rplacd = 0x4C,

    /// Signal user error with message
    /// ( message-string -- ) never returns
    error_user = 0x4D,

    /// Find element in list (using eql - compares numbers by value)
    /// ( item list -- tail-or-nil )
    list_member_eql = 0x4E,

    /// Find element in list (using equal - deep equality)
    /// ( item list -- tail-or-nil )
    list_member_equal = 0x4F,

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

    /// Check if rational number
    /// ( x -- t/nil )
    rationalp = 0x58,

    /// Check if complex number
    /// ( x -- t/nil )
    complexp = 0x59,

    /// Create complex number from real and imaginary parts
    /// ( real imag -- complex )
    make_complex = 0x5A,

    /// Get real part of complex number
    /// ( complex -- real )
    real_part = 0x5B,

    /// Get imaginary part of complex number
    /// ( complex -- imag )
    imag_part = 0x5C,

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

    /// Create box (mutable cell for closure capture)
    /// ( val -- box )
    make_box = 0x64,

    /// Read from box
    /// ( box -- val )
    box_ref = 0x65,

    /// Write to box (returns the value written)
    /// ( box val -- val )
    box_set = 0x66,

    /// Create vector from N stack elements (operand: u8 count)
    /// ( v0 v1 ... vN-1 -- vec )
    make_vec_n = 0x67,

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

    /// Format string with arguments (operand: u8 argc)
    /// ( dest control-str arg1 ... argN -- result )
    /// dest=nil: return string, dest=t: print and return nil
    format = 0x93,

    // ========================================================================
    // Hash table operations
    // ========================================================================

    /// Create hash table (operand: u16 initial capacity)
    /// ( -- hashtable )
    make_hash = 0x94,

    /// Get value from hash table
    /// ( hashtable key -- value )
    /// Returns nil if key not found
    hash_get = 0x95,

    /// Set value in hash table
    /// ( hashtable key value -- )
    hash_set = 0x96,

    /// Remove key from hash table
    /// ( hashtable key -- removed? )
    hash_rem = 0x97,

    /// Get count of entries in hash table
    /// ( hashtable -- count )
    hash_count = 0x98,

    /// Check if value is a hash table
    /// ( x -- t/nil )
    hashtablep = 0x99,

    // ========================================================================
    // Character operations
    // ========================================================================

    /// Check if value is a character
    /// ( x -- t/nil )
    characterp = 0x9A,

    /// Get character code point
    /// ( char -- fixnum )
    char_code = 0x9B,

    /// Make character from code point
    /// ( fixnum -- char )
    code_char = 0x9C,

    /// Character equality
    /// ( c1 c2 -- t/nil )
    char_eq = 0x9D,

    /// Character less than
    /// ( c1 c2 -- t/nil )
    char_lt = 0x9E,

    /// Character greater than
    /// ( c1 c2 -- t/nil )
    char_gt = 0x9F,

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

    /// Assert refinement predicate holds, error if not
    /// Stack: predicate result on top, original value below
    /// Pops predicate result, leaves original value if truthy, errors if not
    /// ( value predicate-result -- value )
    check_refine = 0xF6,

    // ========================================================================
    // Character I/O
    // ========================================================================

    /// Read a character from stdin
    /// ( -- char | fixnum(-1) )
    read_char = 0xA8,

    /// Peek at next character without consuming
    /// ( -- char | fixnum(-1) )
    peek_char = 0xA9,

    /// Push character back to input
    /// ( char -- )
    unread_char = 0xAA,

    /// Check if symbol has a global value binding
    /// ( symbol -- t/nil )
    boundp = 0xAB,

    /// Check if symbol has a function binding (same as boundp in Habu)
    /// ( symbol -- t/nil )
    fboundp = 0xAC,

    /// Get symbol's global value
    /// ( symbol -- value )
    symbol_value = 0xAD,

    /// Get symbol's function binding (same as symbol_value in Habu)
    /// ( symbol -- value )
    symbol_function = 0xAE,

    /// Check if object is of given type
    /// ( obj type-sym -- t/nil )
    typep = 0xAF,

    // ========================================================================
    // Numeric predicates
    // ========================================================================

    /// Absolute value
    /// ( n -- |n| )
    abs = 0xD0,

    /// Check if zero
    /// ( n -- t/nil )
    zerop = 0xD1,

    /// Check if positive
    /// ( n -- t/nil )
    plusp = 0xD2,

    /// Check if negative
    /// ( n -- t/nil )
    minusp = 0xD3,

    /// Check if even
    /// ( n -- t/nil )
    evenp = 0xD4,

    /// Check if odd
    /// ( n -- t/nil )
    oddp = 0xD5,

    /// Read S-expression from stdin
    /// ( -- value )
    read = 0xD6,

    /// Load a file: (load filename)
    /// ( filename -- result )
    load = 0xD7,

    /// Read S-expression from string
    /// ( string -- value )
    read_from_string = 0xD8,

    /// Evaluate expression at runtime
    /// ( expr -- result )
    eval = 0xD9,

    /// Generate unique symbol
    /// ( -- symbol )
    gensym = 0xDA,

    /// Expand macros in expression
    /// ( expr -- expanded )
    macroexpand = 0xDB,

    /// Print value without escaping (princ style)
    /// ( val -- val )
    princ = 0xDC,

    /// Print newline
    /// ( -- nil )
    terpri = 0xDD,

    /// Write single character to stdout
    /// ( char -- char )
    write_char = 0xDE,

    /// Convert character to uppercase
    /// ( char -- char )
    char_upcase = 0xDF,

    /// Convert character to lowercase
    /// ( char -- char )
    char_downcase = 0xE0,

    /// Check if character is a digit
    /// ( char -- t/nil )
    digit_char_p = 0xE1,

    /// Check if character is alphabetic
    /// ( char -- t/nil )
    alpha_char_p = 0xE2,

    /// Parse string to integer
    /// ( string -- fixnum )
    parse_integer = 0xE3,

    /// Convert value to string representation
    /// ( value -- string )
    write_to_string = 0xE4,

    // ========================================================================
    // Bitwise operations
    // ========================================================================

    /// Bitwise AND
    /// ( a b -- a&b )
    logand = 0xE5,

    /// Bitwise OR
    /// ( a b -- a|b )
    logior = 0xE6,

    /// Bitwise XOR
    /// ( a b -- a^b )
    logxor = 0xE7,

    /// Bitwise NOT
    /// ( a -- ~a )
    lognot = 0xE8,

    /// Arithmetic shift (positive=left, negative=right)
    /// ( n count -- shifted )
    ash = 0xE9,

    /// Read entire file contents to string
    /// ( path-string -- string )
    read_file = 0xEA,

    /// Write string to file
    /// ( path-string content-string -- nil )
    write_file = 0xEB,

    /// Create string of given length filled with character
    /// ( length char -- string )
    make_string = 0xEC,

    /// Convert string to list of character codes
    /// ( string -- list )
    string_to_list = 0xED,

    /// Convert list of character codes to string
    /// ( list -- string )
    list_to_string = 0xEE,

    /// Convert string to uppercase
    /// ( string -- string )
    string_upcase = 0xEF,

    /// Convert string to lowercase
    /// ( string -- string )
    string_downcase = 0xF0,

    /// Check if value is a list (nil or cons)
    /// ( val -- t/nil )
    listp = 0xF1,

    /// Check if value is an atom (not a cons)
    /// ( val -- t/nil )
    atom = 0xF2,

    /// Association list lookup (using eq)
    /// ( key alist -- cons-or-nil )
    assoc = 0xF3,

    /// Structural equality (recursive)
    /// ( a b -- t/nil )
    equal = 0xF4,

    /// Extended equality (eql)
    /// ( a b -- t/nil )
    eql = 0xF5,

    /// Association list lookup (using eql - compares numbers by value)
    /// ( key alist -- cons-or-nil )
    assoc_eql = 0xF7,

    /// Association list lookup (using equal - deep equality)
    /// ( key alist -- cons-or-nil )
    assoc_equal = 0xF8,

    /// Find element in sequence (using eql - default)
    /// ( item sequence -- item-or-nil )
    list_find = 0xF9,

    /// Find element in sequence (using eq - identity)
    /// ( item sequence -- item-or-nil )
    list_find_eq = 0xFA,

    /// Find element in sequence (using equal - structural)
    /// ( item sequence -- item-or-nil )
    list_find_equal = 0xFB,

    /// Find position of element in sequence (using eql - default)
    /// ( item sequence -- index-or-nil )
    list_position = 0xFC,

    /// Find position of element in sequence (using eq - identity)
    /// ( item sequence -- index-or-nil )
    list_position_eq = 0xFD,

    /// Find position of element in sequence (using equal - structural)
    /// ( item sequence -- index-or-nil )
    list_position_equal = 0xFE,

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

    /// Push unwind-protect frame (operand: i16 offset to cleanup code)
    /// ( -- )
    push_unwind = 0xB3,

    /// Pop unwind-protect frame on normal exit, jump to cleanup
    /// (operand: i16 offset past cleanup)
    /// ( -- )
    pop_unwind = 0xB4,

    /// Check if value is a float
    /// ( x -- t/nil )
    floatp = 0xB5,

    // ========================================================================
    // Restart handling
    // ========================================================================

    /// Push restart frame (operand: u16 handler offset)
    /// Stack contains restart name
    /// ( name -- )
    push_restart = 0xB6,

    /// Pop N restart frames on normal exit (operand: u8 count)
    /// ( -- )
    pop_restarts = 0xB7,

    /// Invoke restart by name - unwinds to restart handler
    /// ( name value -- ) value becomes result of restart-case
    invoke_restart = 0xB8,

    /// Find restart by name (for find-restart)
    /// ( name -- t/nil )
    find_restart = 0xB9,

    // ========================================================================
    // Streams
    // ========================================================================

    /// Check if value is a stream
    /// ( val -- t/nil )
    streamp = 0xBA,

    /// Check if stream is an input stream
    /// ( stream -- t/nil )
    input_stream_p = 0xBB,

    /// Check if stream is an output stream
    /// ( stream -- t/nil )
    output_stream_p = 0xBC,

    /// Make a string input stream
    /// ( string -- stream )
    make_string_input_stream = 0xBD,

    /// Make a string output stream
    /// ( -- stream )
    make_string_output_stream = 0xBE,

    /// Get the accumulated string from an output stream
    /// ( stream -- string )
    get_output_stream_string = 0xBF,

    // ========================================================================
    // Multiple values
    // ========================================================================

    /// Return multiple values (operand: u8 count)
    /// Pops count values, pushes first, stores rest in secondary values
    /// ( v1 v2 ... vN -- v1 )
    values = 0xC0,

    /// Bind multiple values to locals (operand: u8 count)
    /// Takes primary value from stack, secondaries from buffer
    /// Stores into consecutive locals starting at next operand
    /// ( primary -- )
    mv_bind = 0xC1,

    /// Gather multiple values into a list (multiple-value-list)
    /// Takes primary from stack, secondaries from buffer
    /// Pushes list of all values
    /// ( primary -- list )
    mv_list = 0xC2,

    /// Count occurrences of item in sequence (using eql - default)
    /// ( item sequence -- count )
    list_count = 0xC3,

    /// Count occurrences of item in sequence (using eq - identity)
    /// ( item sequence -- count )
    list_count_eq = 0xC4,

    /// Count occurrences of item in sequence (using equal - structural)
    /// ( item sequence -- count )
    list_count_equal = 0xC5,

    /// Remove all occurrences of item from sequence (using eql - default)
    /// ( item sequence -- new-sequence )
    list_remove = 0xC6,

    /// Remove all occurrences of item from sequence (using eq - identity)
    /// ( item sequence -- new-sequence )
    list_remove_eq = 0xC7,

    /// Remove all occurrences of item from sequence (using equal - structural)
    /// ( item sequence -- new-sequence )
    list_remove_equal = 0xC8,

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
            .push_nil, .push_t, .dup, .pop, .swap, .load_argc,
            .add, .sub, .mul, .div, .mod, .neg,
            .eq, .lt, .gt, .le, .ge, .num_eq, .not,
            .cons, .car, .cdr, .append_lists, .list_length, .list_reverse, .list_nth, .list_last, .list_member, .list_nthcdr,
            .consp, .symbolp, .numberp, .stringp, .vectorp, .closurep, .keywordp, .nilp,
            .vec_ref, .vec_set, .vec_len, .make_box, .box_ref, .box_set,
            .str_ref, .str_len, .str_concat,
            .ret, .print, .random, .type_of, .intern, .substring, .sym_name, .str_eq, .halt,
            .check_fixnum, .check_cons, .check_symbol, .check_string,
            .check_vector, .check_closure, .check_non_nil, .check_list, .check_refine,
            .apply, .pop_catch, .throw,
            .hash_get, .hash_set, .hash_rem, .hash_count, .hashtablep, .rationalp, .complexp,
            .make_complex, .real_part, .imag_part,
            .characterp, .floatp, .char_code, .code_char, .char_eq, .char_lt, .char_gt,
            .read_char, .peek_char, .unread_char, .boundp, .fboundp,
            .symbol_value, .symbol_function, .typep,
            .abs, .zerop, .plusp, .minusp, .evenp, .oddp,
            .mv_list, .read, .load, .read_from_string, .eval, .gensym, .macroexpand, .princ,
            .terpri, .write_char, .char_upcase, .char_downcase,
            .digit_char_p, .alpha_char_p, .parse_integer, .write_to_string,
            .logand, .logior, .logxor, .lognot, .ash,
            .read_file, .write_file, .make_string, .string_to_list, .list_to_string,
        .string_upcase, .string_downcase,
        .listp, .atom, .assoc, .equal, .eql, .rplaca, .rplacd, .error_user,
        .list_member_eql, .list_member_equal, .assoc_eql, .assoc_equal,
        .list_find, .list_find_eq, .list_find_equal,
        .list_position, .list_position_eq, .list_position_equal,
        .list_count, .list_count_eq, .list_count_equal,
        .list_remove, .list_remove_eq, .list_remove_equal,
        .invoke_restart, .find_restart,
        .streamp, .input_stream_p, .output_stream_p,
        .make_string_input_stream, .make_string_output_stream, .get_output_stream_string,
            => 0,

            // 1 byte operand
            .load_local, .store_local, .load_capture,
            .call, .tail_call, .make_list, .make_vec_n, .values, .mv_bind, .format,
            .enter_scope, .exit_scope,
            .pop_restarts,
            => 1,

            // 2 byte operand
            .push_const, .load_global, .store_global,
            .make_vec, .jmp, .jmp_nil, .jmp_not_nil,
            .load_upvalue, .store_upvalue, .push_catch,
            .push_unwind, .pop_unwind, .find_key,
            .push_restart,
            => 2,

            // 3 byte operand
            .make_closure, // u16 code index + u8 captures
            .make_hash, // u16 capacity + u8 test_type
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
    /// Constant pool (mutable for GC relocation)
    constants: []u64, // Values stored as raw u64

    /// Arity (number of required parameters)
    arity: u8,
    /// Number of optional parameters
    optional_count: u8,
    /// Number of keyword parameters
    key_count: u8,
    /// Whether function accepts rest parameter
    has_rest: bool,
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
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 0,
        .name = "test",
    };

    try testing.expectEqual(@as(u8, 0x12), chunk.readU8(0));
    try testing.expectEqual(@as(u16, 0x3412), chunk.readU16(0)); // Little endian
    try testing.expectEqual(@as(i32, 0x78563412), chunk.readI32(0));
}
