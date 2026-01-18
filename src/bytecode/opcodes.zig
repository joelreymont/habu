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

/// Extended math sub-opcodes (used with math_ext opcode 0xED)
pub const MathExtOp = enum(u8) {
    asin = 0x00,
    acos = 0x01,
    atan = 0x02,
    atan2 = 0x03,
    sinh = 0x04,
    cosh = 0x05,
    tanh = 0x06,
    asinh = 0x07,
    acosh = 0x08,
    atanh = 0x09,
};

/// Bytecode opcodes
pub const Op = enum(u16) {
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
    store_global = 0x2E,

    /// Load argument count for current function call
    /// ( -- argc )
    load_argc = 0x2F,

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

    /// Integer quotient (truncate toward zero)
    /// ( a b -- a/b )
    quot = 0x2D,

    /// Remainder (sign matches dividend)
    /// ( a b -- a rem b )
    rem = 0x16,

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
    /// Get numerator of rational (0x5D)
    numerator = 0x5D,
    /// Get denominator of rational (0x5E)
    denominator = 0x5E,
    /// Convert to rational (0x0E)
    rational = 0x0E,
    /// Convert float to rational (0x0E) - same as rational

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

    /// CLOS slot access
    /// ( obj slot-name-sym -- value )
    slot_value = 0x64,

    /// CLOS slot assignment
    /// ( obj slot-name-sym value -- value )
    set_slot_value = 0x65,

    /// Create box (mutable cell for closure capture)
    /// ( val -- box )
    make_box = 0x66,

    /// Read from box
    /// ( box -- val )
    box_ref = 0x67,

    /// Write to box (returns the value written)
    /// ( box val -- val )
    box_set = 0x68,

    /// Create vector from N stack elements (operand: u8 count)
    /// ( v0 v1 ... vN-1 -- vec )
    make_vec_n = 0x69,

    /// Get fill pointer
    /// ( vec -- fp|nil )
    vec_fill_ptr = 0x6A,

    /// Vector push
    /// ( vec val -- idx|-1 )
    vec_push = 0x6B,

    /// Vector push extend
    /// ( vec val ext -- idx )
    vec_push_ext = 0x6C,

    /// Vector pop
    /// ( vec -- val )
    vec_pop = 0x6D,

    // ========================================================================
    // String operations
    // ========================================================================

    /// String ref
    /// ( str idx -- char )
    str_ref = 0x70,

    /// String length
    /// ( str -- len )
    str_len = 0x71,

    /// String set
    /// ( str idx char -- str )
    str_set = 0x72,

    /// String concat
    /// ( s1 s2 -- s3 )
    str_concat = 0x73,

    /// Intern - create symbol from string
    /// ( str -- sym )
    intern = 0x74,

    /// Substring - extract part of a string
    /// ( str start end -- substr )
    substring = 0x75,

    /// Symbol name - get name of symbol as string
    /// ( sym -- str )
    sym_name = 0x100,
    /// Get symbol property (0x6F)
    get = 0x85,
    /// Put symbol property (0x70)
    put = 0x86,
    /// Remove symbol property (0x71)
    remprop = 0x87,

    /// String equal
    /// ( s1 s2 -- bool )
    str_eq = 0x76,

    /// String less than
    /// ( s1 s2 -- bool )
    str_lt = 0x77,

    /// String greater than
    /// ( s1 s2 -- bool )
    str_gt = 0x78,

    /// String less than or equal
    /// ( s1 s2 -- bool )
    str_le = 0x79,

    /// String greater than or equal
    /// ( s1 s2 -- bool )
    str_ge = 0x7A,

    // ========================================================================
    // Control flow
    // ========================================================================

    /// Unconditional jump (operand: i16 offset)
    /// ( -- )
    jmp = 0x80,

    /// Jump if nil (operand: i16 offset)
    /// ( val -- )
    jmp_nil = 0x81,

    /// Jump if not nil (operand: i16 offset)
    /// ( val -- )
    jmp_not_nil = 0x82,

    // ========================================================================
    // Function calls
    // ========================================================================

    /// Call function (operand: u8 argc)
    /// ( fn arg1 ... argN -- result )
    call = 0x90,

    /// Tail call (operand: u8 argc)
    /// ( fn arg1 ... argN -- result )
    tail_call = 0x91,

    /// Return from function
    /// ( result -- )
    ret = 0x92,

    /// Make closure (operand: u16 code index, u8 num_captures)
    /// ( cap1 ... capN -- closure )
    make_closure = 0x93,

    /// Apply function to list of args
    /// ( fn args-list -- result )
    apply = 0x94,

    // ========================================================================
    // I/O
    // ========================================================================

    /// Write value (with escaping)
    /// ( val -- val )
    write = 0x0106,

    /// Print value
    /// ( val -- val )
    print = 0xA0,

    /// Random number [0, n)
    /// ( n -- rand )
    random = 0xA1,

    /// Open file stream (operand: u8 direction: 0=input, 1=output)
    /// ( pathname -- stream )
    open = 0xCE,

    /// Close stream
    /// ( stream -- stream )
    close = 0xCF,

    /// Seed random number generator
    /// ( seed -- seed )
    random_seed = 0xD0,

    /// Make array (dimensions on stack, count in operand, optional initial-element)
    /// ( dim1 dim2 ... dimN [initial-element] -- array )
    /// Operand bits: [7:1] = dim count, [0] = has initial-element
    make_array = 0xD1,

    /// Array reference - access array element (operand: u8 subscript count)
    /// ( array sub1 sub2 ... subN -- value )
    aref = 0x1B,

    /// Array set - set array element (operand: u8 subscript count)
    /// ( array sub1 sub2 ... subN value -- value )
    aset = 0x1C,

    /// Get array dimension at axis
    /// ( array axis -- dimension )
    array_dimension = 0x1D,

    /// Get all array dimensions as list
    /// ( array -- dimensions-list )
    array_dimensions = 0x1E,

    /// Make pathname from components (operand: u8 flags)
    /// ( [components...] -- pathname )
    /// Flags indicate which components are on stack
    make_pathname = 0x1F,

    /// Coerce to pathname
    /// ( pathspec -- pathname )
    /// pathspec can be pathname (identity), string (parse), or stream (get pathname)
    pathname = 0x26,

    /// Parse namestring into pathname
    /// ( string -- pathname )
    /// Parse string into pathname components using platform rules
    parse_namestring = 0x27,

    /// Convert pathname to namestring
    /// ( pathname -- string )
    /// Reconstruct platform-appropriate string from pathname components
    namestring = 0x28,

    /// Merge pathnames
    /// ( pathname default-pathname -- pathname )
    /// Fill nil components from defaults
    merge_pathnames = 0x29,

    /// Reader macro operations
    /// ( char function -- nil )
    /// Set reader macro function for character
    set_macro_character = 0x0C,
    /// ( char -- function-or-nil non-terminating-p )
    /// Get reader macro function for character
    get_macro_character = 0x0D,
    /// ( disp-char sub-char function -- nil )
    /// Set dispatch macro character for #X syntax
    set_dispatch_macro_character = 0x2A,
    /// ( disp-char sub-char -- function-or-nil )
    /// Get dispatch macro function for #X syntax
    get_dispatch_macro_character = 0x2B,

    /// Get type of value as symbol
    /// ( x -- type-sym )
    type_of = 0xA2,

    /// Format string with arguments (operand: u8 argc)
    /// ( dest control-str arg1 ... argN -- result )
    /// dest=nil: return string, dest=t: print and return nil
    format = 0xA3,

    // ========================================================================
    // Hash table operations
    // ========================================================================

    /// Create hash table (operand: u16 initial capacity)
    /// ( -- hashtable )
    make_hash = 0xA4,

    /// Get value from hash table
    /// ( hashtable key -- value )
    /// Returns nil if key not found
    hash_get = 0xA5,

    /// Compute hash code for object
    /// ( object -- fixnum )
    sxhash = 0xCB,

    /// Set value in hash table
    /// ( hashtable key value -- )
    hash_set = 0x96,

    /// Remove key from hash table
    /// ( hashtable key -- removed? )
    hash_rem = 0x97,

    /// Get count of entries in hash table
    /// ( hashtable -- count )
    hash_count = 0x98,

    /// Get hash table capacity
    /// ( hashtable -- fixnum )
    hash_capacity = 0x10D,

    /// Get hash table test function
    /// ( hashtable -- test-symbol )
    hash_test = 0xCC,

    /// Clear hash table
    /// ( hashtable -- hashtable )
    hash_clear = 0xCD,

    /// Get list of keys from hash table
    /// ( hashtable -- keys-list )
    hash_keys = 0x88,

    /// Get alist from hash table
    /// ( hashtable -- alist )
    hash_alist = 0x89,

    // ========================================================================
    // Block/return-from (lexical non-local exit)
    // ========================================================================

    /// Push block frame (operand: i16 exit offset, u16 name constant index)
    /// Saves state for return-from to jump back to
    /// ( -- )
    push_block = 0x8A,

    /// Pop block frame on normal exit
    /// ( -- )
    pop_block = 0x8B,

    /// Return from block (operand: u16 name constant index)
    /// Searches for matching block and does non-local exit
    /// ( value -- )
    return_from = 0x8C,

    /// Write string to output stream
    /// ( string stream -- string )
    write_to_stream = 0x8D,

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
    check_fixnum = 0xB0,

    /// Assert value is cons, error if not
    /// ( x -- x )
    check_cons = 0xB1,

    /// Assert value is symbol, error if not
    /// ( x -- x )
    check_symbol = 0xB2,

    /// Assert value is string, error if not
    /// ( x -- x )
    check_string = 0xB3,

    /// Assert value is vector, error if not
    /// ( x -- x )
    check_vector = 0xB4,

    /// Assert value is closure, error if not
    /// ( x -- x )
    check_closure = 0xB5,

    /// Assert value is non-nil, error if nil
    /// ( x -- x )
    check_non_nil = 0xB6,

    /// Assert value is list (nil or cons), error if not
    /// ( x -- x )
    check_list = 0xA7,

    /// Assert refinement predicate holds, error if not
    /// Stack: predicate result on top, original value below
    /// Pops predicate result, leaves original value if truthy, errors if not
    /// ( value predicate-result -- value )
    check_refine = 0xF8,

    /// Assert value matches one of multiple types (union type)
    /// Operand: u16 constant pool index of vector containing type symbols
    /// ( x -- x )
    check_or = 0x2C,

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
    abs = 0xD2,

    /// Check if zero
    /// ( n -- t/nil )
    zerop = 0xD3,

    /// Check if positive
    /// ( n -- t/nil )
    plusp = 0xD4,

    /// Check if negative
    /// ( n -- t/nil )
    minusp = 0xD5,

    /// Check if even
    /// ( n -- t/nil )
    evenp = 0xD6,

    /// Check if odd
    /// ( n -- t/nil )
    oddp = 0xD7,

    // ========================================================================
    // Math functions
    // ========================================================================

    /// Square root
    /// ( n -- float )
    sqrt = 0xC0,

    /// Sine
    /// ( n -- float )
    sin = 0xC1,

    /// Cosine
    /// ( n -- float )
    cos = 0xC2,

    /// Tangent
    /// ( n -- float )
    tan = 0xC3,

    /// Exponential (e^x)
    /// ( n -- float )
    exp = 0xC4,

    /// Natural logarithm
    /// ( n -- float )
    log = 0xC5,

    /// Floor (round toward negative infinity)
    /// ( n -- fixnum )
    floor = 0xC6,

    /// Ceiling (round toward positive infinity)
    /// ( n -- fixnum )
    ceiling = 0x7B,

    /// Round to nearest integer
    /// ( n -- fixnum )
    round = 0x7C,

    /// Read S-expression from stdin
    /// ( -- value )
    read = 0xD8,

    /// Load a file: (load filename)
    /// ( filename -- result )
    load = 0xD9,

    /// Read S-expression from string
    /// ( string -- value )
    read_from_string = 0xDA,

    /// Evaluate expression at runtime
    /// ( expr -- result )
    eval = 0xDB,

    /// Generate unique symbol
    /// ( -- symbol )
    gensym = 0xDC,

    /// Expand macros in expression
    /// ( expr -- expanded )
    macroexpand = 0xDD,

    /// Print value without escaping (princ style)
    /// ( val -- val )
    princ = 0xDE,

    /// Print newline
    /// ( -- nil )
    terpri = 0xDF,

    /// Write single character to stdout
    /// ( char -- char )
    write_char = 0xE0,

    /// Convert character to uppercase
    /// ( char -- char )
    char_upcase = 0xE1,

    /// Convert character to lowercase
    /// ( char -- char )
    char_downcase = 0xE2,

    /// Check if character is a digit
    /// ( char -- t/nil )
    digit_char_p = 0xE3,

    /// Check if character is alphabetic
    /// ( char -- t/nil )
    alpha_char_p = 0xE4,

    /// Parse string to integer
    /// ( string -- fixnum )
    parse_integer = 0xE5,

    /// Convert value to string representation
    /// ( value -- string )
    write_to_string = 0xE6,

    // ========================================================================
    // Bitwise operations
    // ========================================================================

    /// Bitwise AND
    /// ( a b -- a&b )
    logand = 0xE7,

    /// Bitwise OR
    /// ( a b -- a|b )
    logior = 0xE8,

    /// Bitwise XOR
    /// ( a b -- a^b )
    logxor = 0xE9,

    /// Bitwise NOT
    /// ( a -- ~a )
    lognot = 0xEA,

    /// Arithmetic shift (positive=left, negative=right)
    /// ( n count -- shifted )
    ash = 0xEB,

    /// Test if bit at index is set
    /// ( index n -- bool )
    logbitp = 0x0F,

    /// Count number of 1 bits
    /// ( n -- count )
    logcount = 0x17,

    /// Number of bits needed to represent integer
    /// ( n -- length )
    integer_length = 0x3F,

    /// Bitwise NAND
    /// ( a b -- ~(a&b) )
    lognand = 0x5F,

    /// Bitwise NOR
    /// ( a b -- ~(a|b) )
    lognor = 0x7D,

    /// Bitwise AND with NOT of first arg
    /// ( a b -- (~a)&b )
    logandc1 = 0x7E,

    /// Bitwise AND with NOT of second arg
    /// ( a b -- a&(~b) )
    logandc2 = 0x7F,

    /// Bitwise equivalence (NOT XOR)
    /// ( a b -- ~(a^b) )
    logeqv = 0x8E,

    /// Read entire file contents to string
    /// ( path-string -- string )
    read_file = 0xEC,

    /// Write string to file
    /// ( path-string content-string -- nil )
    write_file = 0xED,

    /// Create string of given length filled with character
    /// ( length char -- string )
    make_string = 0xEE,

    /// Convert list of character codes to string
    /// ( list -- string )
    list_to_string = 0xEF,

    /// Convert string to uppercase
    /// ( string -- string )
    string_upcase = 0xF0,

    /// Extended math operations (sub-opcode in next byte)
    /// ( ... -- ... )
    math_ext = 0xF1,

    /// Convert string to lowercase
    /// ( string -- string )
    string_downcase = 0xF2,

    /// Check if value is a list (nil or cons)
    /// ( val -- t/nil )
    listp = 0xF3,

    /// Check if value is an atom (not a cons)
    /// ( val -- t/nil )
    atom = 0xF4,

    /// Association list lookup (using eq)
    /// ( key alist -- cons-or-nil )
    assoc = 0xF5,

    /// Structural equality (recursive)
    /// ( a b -- t/nil )
    equal = 0xF6,

    /// Extended equality (eql)
    /// ( a b -- t/nil )
    eql = 0xF7,

    /// Association list lookup (using eql - compares numbers by value)
    /// ( key alist -- cons-or-nil )
    assoc_eql = 0xF9,

    /// Association list lookup (using equal - deep equality)
    /// ( key alist -- cons-or-nil )
    assoc_equal = 0xFA,

    /// Find element in sequence (using eql - default)
    /// ( item sequence -- item-or-nil )
    list_find = 0xFB,

    /// Find element in sequence (using eq - identity)
    /// ( item sequence -- item-or-nil )
    list_find_eq = 0xFC,

    /// Find element in sequence (using equal - structural)
    /// ( item sequence -- item-or-nil )
    list_find_equal = 0xFD,

    /// Find position of element in sequence (using eql - default)
    /// ( item sequence -- index-or-nil )
    list_position = 0xFE,

    // ========================================================================
    // Dynamic exception handling (catch/throw)
    // ========================================================================

    /// Push catch frame (operand: i16 offset to catch handler)
    /// Pops tag from stack, saves state for unwinding
    /// ( tag -- )
    push_catch = 0x1F0,

    /// Pop catch frame on normal exit
    /// ( -- )
    pop_catch = 0x1F1,

    /// Throw to matching catch (tag on stack, value on stack)
    /// ( tag value -- )
    throw = 0x1F2,

    /// Push unwind-protect frame (operand: i16 offset to cleanup code)
    /// ( -- )
    push_unwind = 0x1F3,

    /// Pop unwind-protect frame on normal exit, jump to cleanup
    /// (operand: i16 offset past cleanup)
    /// ( -- )
    pop_unwind = 0x1F4,

    /// Check if value is a float
    /// ( x -- t/nil )
    floatp = 0x1F5,

    // ========================================================================
    // Restart handling
    // ========================================================================

    /// Push restart frame (operand: u16 handler offset)
    /// Stack contains restart name
    /// ( name -- )
    push_restart = 0x1F6,

    /// Pop N restart frames on normal exit (operand: u8 count)
    /// ( -- )
    pop_restarts = 0x1F7,

    /// Invoke restart by name - unwinds to restart handler
    /// ( name value -- ) value becomes result of restart-case
    invoke_restart = 0x1F8,

    /// Find restart by name (for find-restart)
    /// ( name -- t/nil )
    find_restart = 0x1F9,

    /// Push progv frame (symbols list and values list on stack)
    /// ( symbols values -- )
    push_progv = 0xBA,

    /// Pop progv frame, restoring previous bindings
    /// ( -- )
    pop_progv = 0xBB,

    // ========================================================================
    // Streams
    // ========================================================================

    /// Make a string input stream
    /// ( string -- stream )
    make_string_input_stream = 0xBC,

    /// Make a string output stream
    /// ( -- stream )
    make_string_output_stream = 0xBD,

    /// Get the accumulated string from an output stream
    /// ( stream -- string )
    get_output_stream_string = 0xBE,

    /// Check if value is a stream
    /// ( val -- t/nil )
    streamp = 0xBF,

    /// Check if stream is an input stream
    /// ( stream -- t/nil )
    input_stream_p = 0x1FA,

    /// Check if stream is an output stream
    /// ( stream -- t/nil )
    output_stream_p = 0x1FB,

    // ========================================================================
    // Multiple values
    // ========================================================================

    /// Return multiple values (operand: u8 count)
    /// Pops count values, pushes first, stores rest in secondary values
    /// ( v1 v2 ... vN -- v1 )
    values = 0x1FC,

    /// Bind multiple values to locals (operand: u8 count)
    /// Takes primary value from stack, secondaries from buffer
    /// Stores into consecutive locals starting at next operand
    /// ( primary -- )
    mv_bind = 0xC3,

    /// Gather multiple values into a list (multiple-value-list)
    /// Takes primary from stack, secondaries from buffer
    /// Pushes list of all values
    /// ( primary -- list )
    mv_list = 0xC4,

    /// Return list elements as multiple values (values-list)
    /// ( list -- first-element )
    /// Secondary values buffer gets remaining elements
    values_list = 0x8F,

    /// Count occurrences of item in sequence (using eql - default)
    /// ( item sequence -- count )
    list_count = 0xC5,

    /// Count occurrences of item in sequence (using eq - identity)
    /// ( item sequence -- count )
    list_count_eq = 0xC6,

    /// Count occurrences of item in sequence (using equal - structural)
    /// ( item sequence -- count )
    list_count_equal = 0xC7,

    /// Remove all occurrences of item from sequence (using eql - default)
    /// ( item sequence -- new-sequence )
    list_remove = 0xC8,

    /// Remove all occurrences of item from sequence (using eq - identity)
    /// ( item sequence -- new-sequence )
    list_remove_eq = 0xC9,

    /// Remove all occurrences of item from sequence (using equal - structural)
    /// ( item sequence -- new-sequence )
    list_remove_equal = 0xCA,

    /// Establish condition handler bindings
    /// ( body-fn handlers-alist -- result )
    /// handlers-alist: list of (condition-type . handler-fn)
    handler_bind = 0x07,

    // Stream I/O operations
    /// Read line from stream
    /// ( stream -- string | nil )
    read_line = 0x36,

    /// Write line to stream
    /// ( stream text -- t )
    write_line = 0x37,

    /// Read byte from stream
    /// ( stream -- fixnum | nil )
    read_byte = 0x39,

    /// Write byte to stream
    /// ( stream byte -- t )
    write_byte = 0x3A,

    /// Get file position
    /// ( stream -- fixnum )
    file_position = 0x3B,

    /// Get file length
    /// ( stream -- fixnum )
    file_length = 0x3C,

    /// Finish output (flush)
    /// ( stream -- nil )
    finish_output = 0x3D,

    /// Force output (flush)
    /// ( stream -- nil )
    force_output = 0x3E,

    // ========================================================================
    // Special
    // ========================================================================

    /// Halt execution
    /// ( -- )
    halt = 0xFF,

    // ========================================================================
    // Pathname operations (0x100+)
    // ========================================================================

    /// Get pathname host component
    /// ( pathname -- host )
    pathname_host = 0x107,

    /// Get pathname device component
    /// ( pathname -- device )
    pathname_device = 0x108,

    /// Get pathname directory component
    /// ( pathname -- directory )
    pathname_directory = 0x109,

    /// Get pathname name component
    /// ( pathname -- name )
    pathname_name = 0x10A,

    /// Get pathname type component
    /// ( pathname -- type )
    pathname_type = 0x10B,

    /// Get pathname version component
    /// ( pathname -- version )
    pathname_version = 0x10C,

    /// Get operand size in bytes
    pub fn operandSize(self: Op) u8 {
        return switch (self) {
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
            .sqrt,
            .sin,
            .cos,
            .tan,
            .exp,
            .log,
            .floor,
            .ceiling,
            .round,
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
            .set_slot_value,
            .make_box,
            .box_ref,
            .box_set,
            .str_ref,
            .str_len,
            .str_set,
            .str_concat,
            .str_eq,
            .str_lt,
            .str_gt,
            .str_le,
            .str_ge,
            .ret,
            .write,
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
            .set_macro_character,
            .get_macro_character,
            .set_dispatch_macro_character,
            .get_dispatch_macro_character,
            .read_line,
            .write_line,
            .read_byte,
            .write_byte,
            .file_position,
            .file_length,
            .finish_output,
            .force_output,
            .halt,
            .close,
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
            .values_list,
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
            .list_count,
            .list_count_eq,
            .list_count_equal,
            .list_remove,
            .list_remove_eq,
            .list_remove_equal,
            .invoke_restart,
            .find_restart,
            .push_progv,
            .pop_progv,
            .handler_bind,
            .streamp,
            .input_stream_p,
            .output_stream_p,
            .make_string_input_stream,
            .make_string_output_stream,
            .get_output_stream_string,
            .array_dimension,
            .array_dimensions,
            .pathname,
            .parse_namestring,
            .namestring,
            .merge_pathnames,
            .pathname_host,
            .pathname_device,
            .pathname_directory,
            .pathname_name,
            .pathname_type,
            .pathname_version,
            => 0,

            // 1 byte operand
            .load_local,
            .store_local,
            .load_capture,
            .call,
            .tail_call,
            .make_list,
            .make_vec_n,
            .values,
            .mv_bind,
            .format,
            .enter_scope,
            .exit_scope,
            .open,
            .pop_restarts,
            .make_array,
            .math_ext,
            .aref,
            .aset,
            .make_pathname,
            => 1,

            // 2 byte operand
            .push_const,
            .load_global,
            .store_global,
            .make_vec,
            .jmp,
            .jmp_nil,
            .jmp_not_nil,
            .load_upvalue,
            .store_upvalue,
            .push_catch,
            .push_unwind,
            .pop_unwind,
            .find_key,
            .push_restart,
            .check_or,
            .return_from,
            => 2,

            // 3 byte operand
            .make_closure, // u16 code index + u8 captures
            .make_hash, // u16 capacity + u8 test_type
            => 3,

            // 4 byte operand
            .push_i32,
            .push_block, // i16 exit offset + u16 name constant index
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
