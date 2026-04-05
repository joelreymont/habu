//! Bytecode opcode definitions for Habu VM
//!
//! Stack-based bytecode with 2-byte opcode (u16, little-endian) + optional operands.
//! Designed for simplicity and portability (WASM target).
//!
//! Instruction format:
//! - Opcode: 2 bytes (little-endian)
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

    /// Check if integer (fixnum or bignum)
    /// ( x -- t/nil )
    integerp = 0x1FD,

    /// Check if real number (integer, rational, or float)
    /// ( x -- t/nil )
    realp = 0x1FE,

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

    /// Get method qualifiers
    /// ( method -- qualifiers )
    method_qualifiers = 0x112,

    /// Get method specializers
    /// ( method -- specializers )
    method_specializers = 0x113,

    /// Get method function
    /// ( method -- function )
    method_function = 0x114,

    /// Get generic function methods
    /// ( gf -- methods )
    generic_function_methods = 0x115,

    /// Get generic function lambda list
    /// ( gf -- lambda-list )
    generic_function_lambda_list = 0x116,

    /// Get generic function name
    /// ( gf -- name )
    generic_function_name = 0x117,

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

    /// Get class of object
    /// ( obj -- class )
    class_of = 0x0B,

    /// Find class by name symbol
    /// ( name -- class )
    find_class = 0x101,
    /// Set class registry entry
    /// ( name class-or-nil -- class-or-nil )
    set_find_class = 0x12A,
    /// Return the name of a class
    /// ( class -- name )
    class_name = 0x102,
    /// Return direct superclasses list
    /// ( class -- list )
    class_direct_superclasses = 0x103,
    /// Return class precedence list
    /// ( class -- list )
    class_precedence_list = 0x104,
    /// Return direct slots list
    /// ( class -- list )
    class_direct_slots = 0x105,
    /// Return all slots list (use 0x11D to avoid collision with write=0x106)
    /// ( class -- list )
    class_slots = 0x11D,

    /// Slot definition accessors
    /// ( slot-def -- value )
    slot_definition_name = 0x11E,
    slot_definition_initform = 0x11F,
    slot_definition_initargs = 0x120,
    slot_definition_readers = 0x121,
    slot_definition_writers = 0x122,
    slot_definition_allocation = 0x123,
    slot_definition_type = 0x124,

    /// Allocate GenericFunction object
    /// ( name lambda-list -- gf )
    make_generic_function = 0xA6,

    /// Allocate Method object
    /// ( qualifiers specializers lambda-list function -- method )
    make_method = 0xA7,

    /// Return unbound slot marker
    /// ( -- unbound )
    make_unbound = 0x09,

    /// Set GF dispatcher function
    /// ( gf dispatcher -- gf )
    set_gf_dispatcher = 0xB9,

    /// Add method to generic function
    /// ( gf method -- gf )
    add_method = 0x125,

    /// Check if CLOS slot is bound
    /// ( obj slot-name-sym -- t/nil )
    slot_boundp = 0x95,

    /// Make CLOS slot unbound
    /// ( obj slot-name-sym -- obj )
    slot_makunbound = 0x11B,

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

    /// Set fill pointer
    /// ( vec fp -- t|nil )
    vec_set_fill_ptr = 0x172,

    /// Set adjustable flag
    /// ( vec bool -- t|nil )
    vec_set_adjustable = 0x173,

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

    /// Make uninterned symbol from string designator
    /// ( designator -- sym )
    make_symbol = 0x10F,

    /// Unintern - remove symbol from package
    /// ( sym pkg -- bool )
    unintern = 0x0A,

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

    /// Get array rank (number of dimensions)
    /// ( array -- rank )
    array_rank = 0x193,

    /// Get array total size (product of dimensions)
    /// ( array -- total-size )
    array_total_size = 0x194,

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

    /// Adjust array size
    /// ( vec new-size fill-val -- vec )
    vec_adjust = 0xB7,

    /// Assert value is list (nil or cons), error if not
    /// ( x -- x )
    check_list = 0xB8,

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

    /// Check if input is available on stream
    /// ( stream -- t/nil )
    listen = 0x153,

    /// Get upgraded complex part type
    /// ( typespec -- real )
    upgraded_complex_part_type = 0x154,

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

    /// Get symbol's property list
    /// ( symbol -- plist )
    symbol_plist = 0x190,

    /// Set symbol's function binding
    /// ( symbol function -- function )
    set_symbol_function = 0x192,

    /// Set symbol's property list
    /// ( symbol plist -- plist )
    set_symbol_plist = 0x191,

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
    tan = 0x83,

    /// Exponential (e^x)
    /// ( n -- float )
    exp = 0x84,

    /// Natural logarithm
    /// ( n -- float )
    log = 0x6E,

    /// Floor (round toward negative infinity)
    /// ( n -- fixnum )
    floor = 0x6F,

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

    /// Read S-expression from stream
    /// ( stream -- value ) + eofp secondary
    read_stream = 0x197,

    /// Evaluate expression at runtime
    /// ( expr -- result )
    eval = 0xDB,

    /// Generate unique symbol
    /// ( -- symbol )
    gensym = 0xDC,

    /// Expand macros fully
    /// ( expr -- expanded )
    macroexpand = 0xDD,

    /// Expand macros once
    /// ( expr -- expanded )
    macroexpand_1 = 0x13E,

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

    /// Extended structural equality (equalp)
    /// ( a b -- t/nil )
    equalp = 0x127,

    /// Copy structure (copy-structure)
    /// ( struct -- struct-copy )
    copy_structure = 0x128,

    /// FUNCTION-LAMBDA-EXPRESSION
    /// ( fn -- lambda-expr )
    /// Also sets secondary values: closure-p, name.
    function_lambda_expression = 0x129,

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

    /// Signal condition (condition-type and value on stack)
    /// Dispatches handlers/catches and returns nil when unhandled.
    /// ( condition-type value -- nil | transferred )
    signal = 0x1FF,

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

    /// Write string to stream (no newline)
    /// ( stream text -- t )
    write_string = 0x18C,

    /// Check subtype relationship
    /// ( type1 type2 -- (subtype-p certain-p) )
    subtypep = 0x18D,

    /// Read byte from stream
    /// ( stream -- fixnum | nil )
    read_byte = 0x39,

    /// Write byte to stream
    /// ( stream byte -- t )
    write_byte = 0x3A,

    /// Get file position
    /// ( stream -- fixnum )
    file_position = 0x3B,

    /// Set file position
    /// ( stream pos -- t )
    set_file_position = 0x196,

    /// Get file length
    /// ( stream -- fixnum )
    file_length = 0x3C,

    /// Finish output (flush)
    /// ( stream -- nil )
    finish_output = 0x3D,

    /// Force output (flush)
    /// ( stream -- nil )
    force_output = 0x3E,

    /// Clear input (discard buffered input)
    /// ( stream -- nil )
    clear_input = 0x14E,

    /// Clear output (discard buffered output)
    /// ( stream -- nil )
    clear_output = 0x14F,

    /// Sleep for N seconds
    /// ( seconds -- nil )
    sleep = 0x118,

    /// Delete file
    /// ( path-string -- nil )
    delete_file = 0x119,

    /// Rename file
    /// ( old-path new-path -- nil )
    rename_file = 0x11A,

    /// Probe file (check if exists)
    /// ( path-string -- truename-or-nil )
    probe_file = 0x130,

    /// File write date (modification time)
    /// ( path-string -- universal-time )
    file_write_date = 0x131,

    /// File author (nil on Unix-like systems)
    /// ( path-string -- author-string-or-nil )
    file_author = 0x150,

    /// Get string length in file (bytes)
    /// ( stream string -- length )
    file_string_length = 0x155,

    /// Package predicate
    /// ( x -- bool )
    packagep = 0x156,

    /// Get symbol's package
    /// ( symbol -- package-or-nil )
    symbol_package = 0x157,

    /// Get package name
    /// ( package -- name-string )
    package_name = 0x158,

    /// Get package nicknames
    /// ( package -- nickname-list )
    package_nicknames = 0x159,

    /// Get package use-list
    /// ( package -- package-list )
    package_use_list = 0x15A,

    /// Get packages that use this package
    /// ( package -- package-list )
    package_used_by_list = 0x15B,

    /// Get package shadowing symbols
    /// ( package -- symbol-list )
    package_shadowing_symbols = 0x15C,

    /// List all packages
    /// ( -- package-list )
    list_all_packages = 0x15D,

    /// Find a package by name
    /// ( name -- package-or-nil )
    find_package = 0x15E,

    /// Delete a package
    /// ( package -- t-or-nil )
    delete_package = 0x15F,

    /// Export symbols from package
    /// ( symbols package -- t )
    pkg_export = 0x18E,

    /// Import symbols into package
    /// ( symbols package -- t )
    pkg_import = 0x160,

    /// Add packages to package use-list
    /// ( packages package -- t )
    pkg_use_package = 0x18F,

    /// Unexport symbols from package
    /// ( symbols package -- t )
    pkg_unexport = 0x161,

    /// Shadow symbols in package
    /// ( names package -- t )
    pkg_shadow = 0x162,

    /// Shadowing import
    /// ( symbols package -- t )
    pkg_shadowing_import = 0x163,

    /// Unuse package
    /// ( packages package -- t )
    pkg_unuse_package = 0x164,

    /// Unintern symbol from package
    /// ( symbol package -- t-or-nil )
    pkg_unintern = 0x165,

    /// Find symbol in package
    /// ( name package -- symbol status )
    pkg_find_symbol = 0x166,

    /// Find all symbols by name
    /// ( name -- symbol-list )
    pkg_find_all_symbols = 0x167,

    /// Make a new package
    /// ( name nicknames use-list -- package )
    pkg_make_package = 0x168,

    /// Rename a package
    /// ( package new-name new-nicknames -- package )
    pkg_rename_package = 0x169,

    /// Encode calendar components to universal time
    /// Operand: u8 argc (6 or 7 - 7 includes timezone)
    /// ( second minute hour date month year [zone] -- universal-time )
    encode_universal_time = 0x16A,

    /// Find symbols containing substring
    /// ( string -- symbol-list )
    apropos_list = 0x16B,

    /// Read character if available (non-blocking)
    /// ( stream -- char | nil )
    read_char_no_hang = 0x16C,

    /// Compute list of active restarts
    /// ( -- restart-list )
    compute_restarts = 0x16D,

    /// Get restart name
    /// ( restart -- name )
    restart_name = 0x16E,

    /// List directory contents
    /// ( pathname -- list-of-pathnames )
    directory = 0x16F,

    /// Check if pathname matches wildcard
    /// ( pathname wildcard -- t/nil )
    pathname_match_p = 0x170,

    /// Get shortest pathname string
    /// ( pathname -- string )
    enough_namestring = 0x171,

    /// Decode float into (significand, exponent, sign) list
    /// ( float -- list )
    decode_float = 0x174,

    /// Integer decode float into (significand, exponent, sign) list
    /// ( float -- list )
    integer_decode_float = 0x175,

    /// Float radix (always 2)
    /// ( float -- 2 )
    float_radix = 0x176,

    /// Float digits (precision bits)
    /// ( float -- fixnum )
    float_digits = 0x177,

    /// Set element in sequence (polymorphic for vectors and lists)
    /// ( seq index value -- value )
    elt_set = 0x178,

    /// Create broadcast stream (writes to multiple streams) - varargs version
    /// ( streams... n -- stream )
    make_broadcast_stream = 0x179,

    /// Create concatenated stream (reads from sequence of streams) - varargs version
    /// ( streams... n -- stream )
    make_concatenated_stream = 0x17A,

    /// Create broadcast stream from list - list version
    /// ( streams-list -- stream )
    make_broadcast_stream_list = 0x185,

    /// Create concatenated stream from list - list version
    /// ( streams-list -- stream )
    make_concatenated_stream_list = 0x186,

    /// Create echo stream (reads from input, echoes to output)
    /// ( input-stream output-stream -- stream )
    make_echo_stream = 0x17B,

    /// Create synonym stream (delegates to symbol's value)
    /// ( symbol -- stream )
    make_synonym_stream = 0x17C,

    /// Create two-way stream (bidirectional input + output)
    /// ( input-stream output-stream -- stream )
    make_two_way_stream = 0x17D,

    /// Get broadcast stream's component streams
    /// ( stream -- list )
    broadcast_stream_streams = 0x17E,

    /// Get concatenated stream's remaining streams
    /// ( stream -- list )
    concatenated_stream_streams = 0x17F,

    /// Get echo stream's input stream
    /// ( stream -- stream )
    echo_stream_input_stream = 0x180,

    /// Get echo stream's output stream
    /// ( stream -- stream )
    echo_stream_output_stream = 0x181,

    /// Get synonym stream's symbol
    /// ( stream -- symbol )
    synonym_stream_symbol = 0x182,

    /// Get two-way stream's input stream
    /// ( stream -- stream )
    two_way_stream_input_stream = 0x183,

    /// Get two-way stream's output stream
    /// ( stream -- stream )
    two_way_stream_output_stream = 0x184,

    /// Disassemble a function and print bytecode
    /// ( function -- nil )
    disassemble = 0x187,

    /// Read a character from a stream
    /// ( stream -- char )
    read_char_stream = 0x188,

    /// Peek a character from a stream without consuming
    /// ( stream -- char )
    peek_char_stream = 0x189,

    /// Push a character back to a stream
    /// ( char stream -- nil )
    unread_char_stream = 0x195,

    /// Open a file stream
    /// ( filename direction -- stream )
    open_file = 0x18A,

    /// Close a stream
    /// ( stream -- nil )
    close_stream = 0x18B,

    /// Get current universal time
    /// ( -- universal-time )
    get_universal_time = 0x132,

    /// Get internal real time (microseconds)
    /// ( -- internal-time )
    get_internal_real_time = 0x133,

    /// Get internal run time (process CPU time, microseconds)
    /// ( -- internal-time )
    get_internal_run_time = 0x140,

    /// Copy a symbol (optionally with properties)
    /// ( symbol copy-props -- new-symbol )
    copy_symbol = 0x141,

    /// Make a symbol unbound
    /// ( symbol -- symbol )
    makunbound = 0x142,

    /// Set symbol's value (deprecated, use setq)
    /// ( symbol value -- value )
    set_sym_val = 0x143,

    /// Print memory usage information
    /// ( -- nil )
    room = 0x134,

    /// Get Lisp implementation type
    /// ( -- string )
    lisp_implementation_type = 0x135,

    /// Get Lisp implementation version
    /// ( -- string )
    lisp_implementation_version = 0x136,

    /// Get software type (OS)
    /// ( -- string )
    software_type = 0x137,

    /// Get machine type (architecture)
    /// ( -- string )
    machine_type = 0x138,

    /// Get machine instance (hostname)
    /// ( -- string-or-nil )
    machine_instance = 0x139,

    /// Get machine version
    /// ( -- string-or-nil )
    machine_version = 0x13A,

    /// Get software version
    /// ( -- string-or-nil )
    software_version = 0x13B,

    /// Get short site name
    /// ( -- string-or-nil )
    short_site_name = 0x13C,

    /// Get long site name
    /// ( -- string-or-nil )
    long_site_name = 0x13D,

    /// Get user home directory pathname
    /// ( -- pathname-or-nil )
    user_homedir_pathname = 0x13F,

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

    /// Get canonical pathname (truename)
    /// ( pathname -- truename-or-nil )
    truename = 0x144,

    /// Ensure directories exist for pathname
    /// ( pathname -- pathname )
    ensure_directories_exist = 0x145,

    /// Get current decoded time (9 values)
    /// ( -- second minute hour date month year dow dst-p zone )
    get_decoded_time = 0x146,

    /// Decode universal time to components
    /// ( universal-time -- second minute hour date month year dow dst-p zone )
    decode_universal_time = 0x147,

    /// Get directory portion of pathname as string
    /// ( pathname -- string )
    directory_namestring = 0x148,

    /// Get file portion of pathname as string
    /// ( pathname -- string )
    file_namestring = 0x149,

    /// Get host portion of pathname as string
    /// ( pathname -- string )
    host_namestring = 0x14A,

    /// Check if pathname contains wildcards
    /// ( pathname -- t/nil )
    wild_pathname_p = 0x14B,

    /// Check if stream is open (not closed)
    /// ( stream -- t/nil )
    open_stream_p = 0x14C,

    /// Check if stream is interactive
    /// ( stream -- t/nil )
    interactive_stream_p = 0x14D,

    /// Get stream element type
    /// ( stream -- type-symbol )
    stream_element_type = 0x151,

    /// Get stream external format
    /// ( stream -- format-keyword )
    stream_external_format = 0x152,

    /// Get package internal symbols hash table
    /// ( pkg -- hashtable )
    package_symbols_table = 0x110,

    /// Get package exports hash table
    /// ( pkg -- hashtable )
    package_exports_table = 0x111,

    /// Get list of all symbols in native package by name
    /// ( pkg-name -- symbol-list )
    package_symbols_list = 0x11C,

    /// Get list of exported symbols in native package
    /// ( pkg -- symbol-list )
    package_exports_list = 0x126,

    /// Find symbol in package
    /// ( name pkg -- (symbol . status) | (nil . nil) )
    /// Returns cons: (symbol . status) where status is :internal/:external/:inherited/nil
    find_symbol = 0x10E,

    // ========================================================================
    // Specialized (type-proven) operations
    // ========================================================================

    /// Unboxed fixnum addition (no tag check)
    /// ( fixnum fixnum -- fixnum )
    fixnum_add = 0x200,

    /// Unboxed fixnum subtraction (no tag check)
    /// ( fixnum fixnum -- fixnum )
    fixnum_sub = 0x201,

    /// Unboxed fixnum multiplication (no tag check)
    /// ( fixnum fixnum -- fixnum )
    fixnum_mul = 0x202,

    /// Unboxed fixnum comparisons (no tag check, result is t or nil)
    fixnum_le = 0x203,
    fixnum_lt = 0x204,
    fixnum_gt = 0x205,
    fixnum_ge = 0x206,
    fixnum_eq = 0x207,

    /// Cons car without nil check (proven cons)
    /// ( cons -- car )
    unsafe_car = 0x208,

    /// Cons cdr without nil check (proven cons)
    /// ( cons -- cdr )
    unsafe_cdr = 0x209,

    /// Array access without bounds check (proven valid)
    /// ( vec fixnum -- element )
    direct_aref = 0x210,

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
            .integerp,
            .realp,
            .stringp,
            .vectorp,
            .closurep,
            .keywordp,
            .method_qualifiers,
            .method_specializers,
            .method_function,
            .generic_function_methods,
            .generic_function_lambda_list,
            .generic_function_name,
            .nilp,
            .vec_ref,
            .vec_set,
            .vec_len,
            .slot_value,
            .set_slot_value,
            .class_of,
            .find_class,
            .set_find_class,
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
            .make_generic_function,
            .make_method,
            .make_unbound,
            .set_gf_dispatcher,
            .add_method,
            .slot_boundp,
            .slot_makunbound,
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
            .make_symbol,
            .unintern,
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
            .write_string,
            .read_byte,
            .write_byte,
            .file_position,
            .set_file_position,
            .file_length,
            .finish_output,
            .force_output,
            .clear_input,
            .clear_output,
            .sleep,
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
            .signal,
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
            .listen,
            .upgraded_complex_part_type,
            .boundp,
            .fboundp,
            .symbol_value,
            .symbol_function,
            .symbol_plist,
            .set_symbol_function,
            .set_symbol_plist,
            .function_lambda_expression,
            .typep,
            .subtypep,
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
            .read_stream,
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
            .elt_set,
            .get_universal_time,
            .get_internal_real_time,
            .get_internal_run_time,
            .get_decoded_time,
            .decode_universal_time,
            .copy_symbol,
            .makunbound,
            .set_sym_val,
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
            .make_string,
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
            .array_rank,
            .array_total_size,
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
            .vec_fill_ptr,
            .vec_set_fill_ptr,
            .vec_set_adjustable,
            .vec_push,
            .vec_push_ext,
            .vec_pop,
            .vec_adjust,
            // Compound stream accessors (no operand)
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
            .unread_char_stream,
            .open_file,
            .close_stream,
            .fixnum_add,
            .fixnum_sub,
            .fixnum_mul,
            .fixnum_le,
            .fixnum_lt,
            .fixnum_gt,
            .fixnum_ge,
            .fixnum_eq,
            .unsafe_car,
            .unsafe_cdr,
            .direct_aref,
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
            .encode_universal_time,
            // Compound stream constructors with count operand
            .make_broadcast_stream,
            .make_concatenated_stream,
            => 1,

            // 2 byte operand
            .push_const,
            .load_global,
            .store_global,
            .make_vec,
            .mv_bind,
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

pub const DecodeError = error{
    TruncatedInstruction,
    InvalidOpcode,
};

pub const DecodedInstruction = struct {
    op: Op,
    operand_off: usize,
    next_off: usize,
};

pub fn decodeInstruction(code: []const u8, offset: usize) DecodeError!DecodedInstruction {
    if (offset >= code.len) return error.TruncatedInstruction;
    if (code.len - offset < 2) return error.TruncatedInstruction;

    const opcode = std.mem.readInt(u16, code[offset..][0..2], .little);
    const op = std.meta.intToEnum(Op, opcode) catch return error.InvalidOpcode;
    const insn_len: usize = 2 + op.operandSize();
    if (code.len - offset < insn_len) return error.TruncatedInstruction;

    return .{
        .op = op,
        .operand_off = offset + 2,
        .next_off = offset + insn_len,
    };
}

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
