//! Intermediate Representation for Habu compiler
//!
//! The IR is a tree structure representing Habu programs after parsing
//! but before bytecode emission. It's designed to be:
//! - Easy to generate from S-expressions
//! - Easy to type-check (occurrence typing)
//! - Easy to compile to bytecode or JIT
//!
//! IR nodes use tagged unions for exhaustive matching.

const std = @import("std");
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
pub const HashTest = @import("../runtime/objects.zig").HashTest;
const types = @import("../types/types.zig");

/// Sentinel value for unresolved variable depth/index
/// Used by p03_lift, resolved by p04_resolve
pub const UNRESOLVED: u16 = 0xFFFF;

/// Single restart definition for restart-case
pub const Restart = struct {
    name: *const Ir,
    handler: *const Ir,
};

/// Single handler binding for handler-bind
pub const Handler = struct {
    condition_type: *const Ir,
    handler_fn: *const Ir,
};

/// IR node - represents a Habu expression
pub const Ir = union(enum) {
    // ========================================================================
    // Literals
    // ========================================================================

    /// Literal value (fixnum, nil, t)
    lit: Value,

    /// Symbol literal (for quote)
    quote_sym: []const u8,

    /// Quoted expression (for complex quoted data)
    quote: *const Ir,

    // ========================================================================
    // Variables
    // ========================================================================

    /// Variable reference
    @"var": struct {
        name: []const u8,
        /// Lexical depth (0 = current frame, 1 = parent, etc.)
        depth: u16,
        /// Slot index within frame
        index: u16,
    },

    /// Variable assignment
    set: struct {
        name: []const u8,
        depth: u16,
        index: u16,
        value: *const Ir,
    },

    /// Global variable reference
    global_ref: struct {
        name: []const u8,
        index: u16,
    },

    /// Global variable definition
    define: struct {
        name: []const u8,
        index: u16,
        value: *const Ir,
    },

    // ========================================================================
    // Binding forms
    // ========================================================================

    /// Let binding: (let ((x 1) (y 2)) body)
    let: struct {
        bindings: []const Binding,
        body: *const Ir,
    },

    /// Lambda: (lambda (x y) body)
    lambda: struct {
        params: []const []const u8,
        /// Optional parameters with defaults: (x &optional (y 10) (z nil))
        optional_params: []const OptionalParam,
        /// Keyword parameters with defaults: (x &key (y 10) z)
        key_params: []const KeyParam,
        /// Whether &allow-other-keys was present
        allow_other_keys: bool = false,
        /// Absolute local-slot index where temporary keyword/value pairs begin.
        key_temp_start: u16 = 0,
        /// Rest parameter name (for dotted param lists like (a b . rest))
        rest_param: ?[]const u8,
        /// Ordered special bindings activated as parameter slots become live.
        special_bindings: []const SpecialBinding = &.{},
        /// Free variables captured from enclosing scope
        captures: []const Capture,
        body: *const Ir,
        /// Original source (lambda ...) form, or nil if unavailable.
        /// Used by FUNCTION-LAMBDA-EXPRESSION.
        lambda_expr: Value = Value.nil,
        /// Stable retained index for lambda_expr during compile->emit GC windows.
        lambda_expr_idx: ?u32 = null,
        /// Function name symbol, or nil if anonymous.
        /// Used by FUNCTION-LAMBDA-EXPRESSION.
        name: Value = Value.nil,
        /// Stable retained index for name during compile->emit GC windows.
        name_idx: ?u32 = null,
        /// Per-lambda optimize speed (from declarations)
        speed: u8 = 1,
        /// Per-lambda optimize safety (from declarations)
        safety: u8 = 1,
    },

    // ========================================================================
    // Control flow
    // ========================================================================

    /// Conditional: (if cond then else)
    @"if": struct {
        cond: *const Ir,
        then_branch: *const Ir,
        else_branch: *const Ir,
    },

    /// Sequence: (progn e1 e2 ... en)
    progn: []const *const Ir,

    /// While loop: (while cond body...)
    loop: struct {
        cond: *const Ir,
        body: *const Ir,
    },

    /// Named block: (block name body...)
    block: struct {
        name: Value,
        body: *const Ir,
    },

    /// Return from block: (return-from name value)
    return_from: struct {
        name: Value,
        value: *const Ir,
    },

    /// Unwind-protect: (unwind-protect protected cleanup...)
    unwind_protect: struct {
        protected: *const Ir,
        cleanup: *const Ir,
    },

    /// Catch: (catch tag body...)
    @"catch": struct {
        tag: *const Ir,
        body: *const Ir,
    },

    /// Throw: (throw tag value)
    throw: struct {
        tag: *const Ir,
        value: *const Ir,
    },

    /// Signal condition: (signal condition-type value)
    /// Like throw on %condition% tag, but returns nil when unhandled.
    signal: struct {
        condition_type: *const Ir,
        value: *const Ir,
    },

    /// Handler-case: like catch but binds caught value to a variable
    /// (handler-case protected-expr (type (var) handler-body)...)
    handler_case: struct {
        tag: *const Ir, // The catch tag (usually %condition%)
        body: *const Ir, // The protected expression
        handler: *const Ir, // Handler dispatch code
        cond_var: []const u8, // Variable name for caught condition
        cond_idx: u16, // Local slot index for caught condition
    },

    /// Handler-bind: establishes condition handlers around a form
    /// (handler-bind ((condition-type handler-fn) ...) body)
    handler_bind: struct {
        body: *const Ir,
        handlers: []const Handler,
    },

    /// Restart-case: establishes restarts around a form
    /// (restart-case body (name (args) handler)...)
    restart_case: struct {
        body: *const Ir, // The protected expression
        restarts: []const Restart, // Restart definitions
    },

    /// Invoke a restart by name with a value
    /// (invoke-restart name value)
    invoke_restart: struct {
        name: *const Ir,
        value: *const Ir,
    },

    /// Find a restart by name (returns t or nil)
    /// (find-restart name)
    find_restart: UnaryOp,

    /// Progv: dynamic variable binding (progv symbols values body)
    progv: struct {
        symbols: *const Ir, // List of symbols to bind
        values: *const Ir, // List of values to bind them to
        body: *const Ir,
    },

    /// Tagbody: (tagbody tag1 form1 tag2 form2 ...)
    /// Tags are symbols or integers, forms are expressions
    tagbody: struct {
        /// Tag names (for go targets)
        tags: []const Value,
        /// Segments: code between tags (segments.len == tags.len + 1)
        /// segments[0] = code before first tag
        /// segments[i] = code after tags[i-1]
        segments: []const *const Ir,
    },

    /// Go: (go tag)
    go: struct {
        tag: Value,
    },

    /// Multiple values: (values v1 v2 ...)
    values: []const *const Ir,

    /// Values-list: (values-list list) - return list elements as multiple values
    values_list: UnaryOp,

    /// Multiple-value-bind: (multiple-value-bind (vars...) expr body...)
    mv_bind: struct {
        vars: []const []const u8,
        start_index: u16, // absolute local index of first variable
        expr: *const Ir,
        body: *const Ir,
        special_bindings: []const SpecialBinding = &.{},
    },

    /// Multiple-value-list: (multiple-value-list expr)
    /// Evaluates expr and gathers all values into a list
    mv_list: struct {
        expr: *const Ir,
    },

    /// Format: (format dest control-string args...)
    format: struct {
        dest: *const Ir,
        control: *const Ir,
        args: []const *const Ir,
    },

    // ========================================================================
    // Hash table operations
    // ========================================================================

    /// Make hash table: (make-hash-table &key size test)
    make_hash: struct {
        capacity: u16,
        test_type: HashTest,
    },

    /// Hash table get: (gethash key hashtable &optional default)
    hash_get: struct {
        table: *const Ir,
        key: *const Ir,
        default: ?*const Ir,
    },

    /// Hash table set: (sethash key hashtable value)
    hash_set: struct {
        table: *const Ir,
        key: *const Ir,
        value: *const Ir,
    },

    /// Hash table remove: (remhash key hashtable)
    hash_rem: struct {
        table: *const Ir,
        key: *const Ir,
    },

    /// Hash table count: (hash-table-count hashtable)
    hash_count: struct {
        operand: *const Ir,
    },

    /// Hash table capacity: (hash-table-capacity hashtable)
    hash_capacity: struct {
        operand: *const Ir,
    },

    /// Hash table predicate: (hash-table-p x)
    hashtablep: struct {
        operand: *const Ir,
    },

    /// Hash table clear: (clrhash hashtable)
    hash_clear: struct {
        operand: *const Ir,
    },

    /// Hash table test function query: (hash-table-test hashtable)
    hash_test: struct {
        operand: *const Ir,
    },

    /// Hash table keys: (hash-table-keys hashtable)
    hash_keys: struct {
        operand: *const Ir,
    },

    /// Hash table alist: (hash-table-alist hashtable)
    hash_alist: struct {
        operand: *const Ir,
    },

    /// Package predicate: (packagep x)
    packagep: struct {
        operand: *const Ir,
    },

    /// Symbol's package: (symbol-package symbol)
    symbol_package: struct {
        operand: *const Ir,
    },

    /// Package name: (package-name package)
    package_name: struct {
        operand: *const Ir,
    },

    /// Package nicknames: (package-nicknames package)
    package_nicknames: struct {
        operand: *const Ir,
    },

    /// Package use-list: (package-use-list package)
    package_use_list: struct {
        operand: *const Ir,
    },

    /// Package used-by-list: (package-used-by-list package)
    package_used_by_list: struct {
        operand: *const Ir,
    },

    /// Package shadowing symbols: (package-shadowing-symbols package)
    package_shadowing_symbols: struct {
        operand: *const Ir,
    },

    /// List all packages: (list-all-packages)
    list_all_packages: struct {},

    /// Compute list of active restarts: (compute-restarts)
    compute_restarts: struct {},

    /// Get restart name: (restart-name restart)
    restart_name: struct {
        operand: *const Ir,
    },

    /// List directory contents: (directory pathname)
    directory: struct {
        operand: *const Ir,
    },

    /// Check if pathname matches wildcard: (pathname-match-p pathname wildcard)
    pathname_match_p: BinaryOp,

    /// Get shortest sufficient pathname string: (enough-namestring pathname)
    enough_namestring: struct {
        operand: *const Ir,
    },

    /// Find a package by name: (find-package name)
    find_package: struct {
        operand: *const Ir,
    },

    /// Delete a package: (delete-package package)
    delete_package: struct {
        operand: *const Ir,
    },

    /// Export symbols from package: (%export symbols package)
    pkg_export: BinaryOp,

    /// Import symbols into package: (%import symbols package)
    pkg_import: BinaryOp,

    /// Use packages in target package: (%use-package packages package)
    pkg_use_package: BinaryOp,

    /// Unexport symbols from package: (%unexport symbols package)
    pkg_unexport: BinaryOp,

    /// Shadow symbols in package: (%shadow names package)
    pkg_shadow: BinaryOp,

    /// Shadowing import: (%shadowing-import symbols package)
    pkg_shadowing_import: BinaryOp,

    /// Unuse package: (%unuse-package packages package)
    pkg_unuse_package: BinaryOp,

    /// Unintern symbol from package: (%unintern symbol package)
    pkg_unintern: BinaryOp,

    /// Find symbol in package: (%find-symbol name package)
    pkg_find_symbol: BinaryOp,

    /// Find all symbols by name: (find-all-symbols name)
    pkg_find_all_symbols: struct {
        operand: *const Ir,
    },

    /// Find symbols containing substring: (apropos-list string)
    apropos_list: struct {
        operand: *const Ir,
    },

    /// Read character if available: (read-char-no-hang stream)
    read_char_no_hang: struct {
        operand: *const Ir,
    },

    /// Make a new package: (%make-package name nicknames use-list)
    pkg_make_package: TernaryOp,

    /// Rename a package: (%rename-package package new-name new-nicknames)
    pkg_rename_package: TernaryOp,

    sxhash: struct {
        operand: *const Ir,
    },

    // ========================================================================
    // Pathname operations
    // ========================================================================

    pathname_host: struct {
        operand: *const Ir,
    },
    pathname_device: struct {
        operand: *const Ir,
    },
    pathname_directory: struct {
        operand: *const Ir,
    },
    pathname_name: struct {
        operand: *const Ir,
    },
    pathname_type: struct {
        operand: *const Ir,
    },
    pathname_version: struct {
        operand: *const Ir,
    },
    truename: UnaryOp, // Get canonical pathname
    ensure_directories_exist: UnaryOp, // Create directories for pathname
    pathname: UnaryOp, // Convert pathspec to pathname
    parse_namestring: UnaryOp, // Parse string to pathname
    namestring: UnaryOp, // Convert pathname to string
    directory_namestring: UnaryOp, // Get directory portion as string
    file_namestring: UnaryOp, // Get file portion as string
    host_namestring: UnaryOp, // Get host portion as string
    wild_pathname_p: UnaryOp, // Check if pathname contains wildcards
    merge_pathnames: BinaryOp, // Merge two pathnames

    package_symbols_table: struct {
        operand: *const Ir,
    },

    package_exports_table: struct {
        operand: *const Ir,
    },

    package_symbols_list: struct {
        operand: *const Ir,
    },

    package_exports_list: struct {
        operand: *const Ir,
    },

    // ========================================================================
    // Function calls
    // ========================================================================

    /// Function call: (f arg1 arg2 ...)
    call: struct {
        func: *const Ir,
        args: []const *const Ir,
    },

    /// Tail call (same as call but in tail position)
    tailcall: struct {
        func: *const Ir,
        args: []const *const Ir,
    },

    /// Apply function to list of args
    apply: struct {
        func: *const Ir,
        args: *const Ir,
    },

    /// Multiple-value-call: (multiple-value-call fn form1 form2 ...)
    /// Gathers all values from each form and passes them as args to fn
    mv_call: struct {
        func: *const Ir,
        forms: []const *const Ir,
    },

    // ========================================================================
    // Primitives - Arithmetic
    // ========================================================================

    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,
    mod: BinaryOp,
    quot: BinaryOp, // integer quotient (truncate toward zero)
    rem: BinaryOp, // remainder (sign matches dividend)

    // ========================================================================
    // Primitives - Comparison
    // ========================================================================

    eq: BinaryOp, // eq (pointer/fixnum equality)
    lt: BinaryOp,
    gt: BinaryOp,
    le: BinaryOp,
    ge: BinaryOp,
    num_eq: BinaryOp, // = (numeric equality)
    equal: BinaryOp, // equal (structural equality)
    eql: BinaryOp, // eql (extended equality)
    equalp: BinaryOp, // equalp (extended structural equality)

    // ========================================================================
    // Primitives - Logic
    // ========================================================================

    not: UnaryOp,

    // ========================================================================
    // Primitives - List operations
    // ========================================================================

    cons: BinaryOp,
    car: UnaryOp,
    cdr: UnaryOp,
    list: []const *const Ir, // (list a b c)
    list_star: []const *const Ir, // (list* a b c) - last elem is tail
    append: BinaryOp, // (append list1 list2)
    length: UnaryOp, // (length list)
    reverse: UnaryOp, // (reverse list)
    nth: BinaryOp, // (nth n list)
    nthcdr: BinaryOp, // (nthcdr n list)
    last: UnaryOp, // (last list)
    member: BinaryOp, // (member item list) - uses eq
    member_eql: BinaryOp, // (member item list :test #'eql)
    member_equal: BinaryOp, // (member item list :test #'equal)
    assoc: BinaryOp, // (assoc key alist) - uses eq
    assoc_eql: BinaryOp, // (assoc key alist :test #'eql)
    assoc_equal: BinaryOp, // (assoc key alist :test #'equal)
    find: BinaryOp, // (find item sequence) - uses eql
    find_eq: BinaryOp, // (find item sequence :test #'eq)
    find_equal: BinaryOp, // (find item sequence :test #'equal)
    position: BinaryOp, // (position item sequence) - uses eql
    position_eq: BinaryOp, // (position item sequence :test #'eq)
    position_equal: BinaryOp, // (position item sequence :test #'equal)
    count: BinaryOp, // (count item sequence) - uses eql
    count_eq: BinaryOp, // (count item sequence :test #'eq)
    count_equal: BinaryOp, // (count item sequence :test #'equal)
    remove: BinaryOp, // (remove item sequence) - uses eql
    remove_eq: BinaryOp, // (remove item sequence :test #'eq)
    remove_equal: BinaryOp, // (remove item sequence :test #'equal)
    rplaca: BinaryOp, // (rplaca cons value) - destructive!
    rplacd: BinaryOp, // (rplacd cons value) - destructive!

    // ========================================================================
    // Primitives - Type predicates
    // ========================================================================

    consp: UnaryOp,
    symbolp: UnaryOp,
    numberp: UnaryOp,
    integerp: UnaryOp,
    realp: UnaryOp,
    stringp: UnaryOp,
    vectorp: UnaryOp,
    closurep: UnaryOp,
    keywordp: UnaryOp,
    method_qualifiers: UnaryOp,
    method_specializers: UnaryOp,
    method_function: UnaryOp,
    generic_function_methods: UnaryOp,
    generic_function_lambda_list: UnaryOp,
    generic_function_name: UnaryOp,
    nilp: UnaryOp,
    characterp: UnaryOp,
    floatp: UnaryOp,
    listp: UnaryOp, // nil or cons
    atom: UnaryOp, // not a cons
    rationalp: UnaryOp, // rational number
    complexp: UnaryOp, // complex number
    make_complex: BinaryOp, // create complex from real, imag
    real_part: UnaryOp, // get real part of complex
    imag_part: UnaryOp, // get imaginary part of complex
    numerator: UnaryOp, // get numerator of rational
    denominator: UnaryOp, // get denominator of rational
    rational: UnaryOp, // convert number to rational
    rationalize: UnaryOp, // convert float to rational
    /// Struct type predicate: checks if value is a specific struct type
    /// Used for occurrence typing to narrow to struct types
    struct_p: struct {
        operand: *const Ir,
        struct_name: []const u8,
        /// Reference to the struct Type for occurrence typing
        struct_type: *const types.Type,
    },

    // ========================================================================
    // Primitives - Stream operations
    // ========================================================================

    streamp: UnaryOp, // stream predicate
    input_stream_p: UnaryOp, // input stream predicate
    output_stream_p: UnaryOp, // output stream predicate
    open_stream_p: UnaryOp, // open (not closed) stream predicate
    interactive_stream_p: UnaryOp, // interactive stream predicate
    stream_element_type: UnaryOp, // get stream element type
    stream_external_format: UnaryOp, // get stream external format
    make_string_input_stream: UnaryOp, // create string input stream
    make_string_output_stream: void, // create string output stream
    get_output_stream_string: UnaryOp, // get string from output stream
    write_to_stream: BinaryOp, // write string to stream
    // Compound stream operations
    broadcast_stream_streams: UnaryOp, // get broadcast stream's component streams
    concatenated_stream_streams: UnaryOp, // get concatenated stream's remaining streams
    echo_stream_input_stream: UnaryOp, // get echo stream's input stream
    echo_stream_output_stream: UnaryOp, // get echo stream's output stream
    synonym_stream_symbol: UnaryOp, // get synonym stream's symbol
    two_way_stream_input_stream: UnaryOp, // get two-way stream's input stream
    two_way_stream_output_stream: UnaryOp, // get two-way stream's output stream
    make_synonym_stream: UnaryOp, // create synonym stream from symbol
    make_echo_stream: BinaryOp, // create echo stream (input, output)
    make_two_way_stream: BinaryOp, // create two-way stream (input, output)
    make_broadcast_stream: []const *const Ir, // create broadcast stream (&rest streams)
    make_concatenated_stream: []const *const Ir, // create concatenated stream (&rest streams)
    make_broadcast_stream_list: UnaryOp, // create broadcast stream from list
    make_concatenated_stream_list: UnaryOp, // create concatenated stream from list
    disassemble: UnaryOp, // disassemble a function
    read_char_stream: UnaryOp, // read character from stream
    peek_char_stream: UnaryOp, // peek character from stream
    unread_char_stream: BinaryOp, // push character back to a stream
    read_stream: UnaryOp, // read one form from stream
    open_file: BinaryOp, // open file (filename, direction)
    close_stream: UnaryOp, // close a stream

    // ========================================================================
    // Primitives - Character operations
    // ========================================================================

    char_code: UnaryOp,
    code_char: UnaryOp,
    char_eq: BinaryOp,
    char_lt: BinaryOp,
    char_gt: BinaryOp,
    read_char: void, // No operands - reads from stdin
    peek_char: void, // No operands - peeks at stdin
    unread_char: UnaryOp, // Push character back
    listen: UnaryOp, // Check if input available
    upgraded_complex_part_type: UnaryOp, // Get upgraded complex part type
    read: void, // Read S-expression from stdin
    read_from_string: UnaryOp, // Parse string to value
    load: UnaryOp, // Load and evaluate a file
    eval: UnaryOp, // Evaluate expression at runtime
    gensym: OptionalOp, // Generate unique symbol (nullary or with prefix)
    macroexpand: UnaryOp, // Expand macros fully
    macroexpand_1: UnaryOp, // Expand macros once
    princ: UnaryOp, // Print without escaping
    terpri: void, // Print newline
    write_char: UnaryOp, // Write character to stdout
    char_upcase: UnaryOp, // Convert char to uppercase
    char_downcase: UnaryOp, // Convert char to lowercase
    digit_char_p: UnaryOp, // Check if char is digit
    alpha_char_p: UnaryOp, // Check if char is alphabetic
    parse_integer: UnaryOp, // Parse string to integer
    write_to_string: UnaryOp, // Convert value to string
    logand: BinaryOp, // Bitwise AND
    logior: BinaryOp, // Bitwise OR
    logxor: BinaryOp, // Bitwise XOR
    lognot: UnaryOp, // Bitwise NOT
    ash: BinaryOp, // Arithmetic shift
    lognand: BinaryOp, // Bitwise NAND
    lognor: BinaryOp, // Bitwise NOR
    logandc1: BinaryOp, // AND with NOT of first
    logandc2: BinaryOp, // AND with NOT of second
    logeqv: BinaryOp, // Bitwise equivalence
    logbitp: BinaryOp, // Test if bit is set
    logcount: UnaryOp, // Count 1 bits
    integer_length: UnaryOp, // Bits needed to represent
    read_file: UnaryOp, // Read file to string
    write_file: BinaryOp, // Write string to file
    delete_file: UnaryOp, // Delete file
    rename_file: BinaryOp, // Rename file (old, new)
    probe_file: UnaryOp, // Check if file exists
    file_write_date: UnaryOp, // Get file modification time
    file_author: UnaryOp, // Get file author (nil on Unix)
    file_string_length: BinaryOp, // Get length of string in file (stream, string)
    get_universal_time: void, // Get current universal time
    get_internal_real_time: void, // Get internal real time (microseconds)
    get_internal_run_time: void, // Get internal run time (CPU microseconds)
    get_decoded_time: void, // Get current decoded time (9 values)
    decode_universal_time: UnaryOp, // Decode universal time to components
    encode_universal_time: struct { // Encode calendar components to universal time
        second: *const Ir,
        minute: *const Ir,
        hour: *const Ir,
        date: *const Ir,
        month: *const Ir,
        year: *const Ir,
        zone: ?*const Ir, // optional time zone
    },
    room: void, // Print memory statistics
    lisp_implementation_type: void, // Get implementation name
    lisp_implementation_version: void, // Get implementation version
    software_type: void, // Get OS type
    machine_type: void, // Get architecture
    machine_instance: void, // Get hostname
    machine_version: void, // Get hardware version
    software_version: void, // Get OS version
    short_site_name: void, // Get short site name
    long_site_name: void, // Get long site name
    user_homedir_pathname: void, // Get user home directory as pathname
    make_pathname: struct { // Create pathname from components
        host: *const Ir,
        device: *const Ir,
        directory: *const Ir,
        name: *const Ir,
        type: *const Ir,
        version: *const Ir,
    },
    make_string: BinaryOp, // Create string (length, char)
    list_to_string: UnaryOp, // List of chars to string
    string_upcase: UnaryOp, // Convert string to uppercase
    string_downcase: UnaryOp, // Convert string to lowercase
    boundp: UnaryOp, // Check if symbol has global binding
    fboundp: UnaryOp, // Check if symbol has function binding
    symbol_value: UnaryOp, // Get symbol's global value
    array_rank: UnaryOp, // Get array rank
    array_total_size: UnaryOp, // Get array total size
    array_dimensions: UnaryOp, // Get array dimensions as list
    symbol_function: UnaryOp, // Get symbol's function binding
    set_symbol_function: BinaryOp, // Set symbol's function binding
    symbol_plist: UnaryOp, // Get symbol's property list
    set_symbol_plist: BinaryOp, // Set symbol's property list
    function_lambda_expression: UnaryOp, // (function-lambda-expression fn) -> (values lambda-expr closure-p name)
    typep: BinaryOp, // Check if object is of given type
    subtypep: BinaryOp, // Check subtype relationship

    // ========================================================================
    // Primitives - Numeric predicates
    // ========================================================================

    abs: UnaryOp,
    zerop: UnaryOp,
    plusp: UnaryOp,
    minusp: UnaryOp,
    evenp: UnaryOp,
    oddp: UnaryOp,

    // ========================================================================
    // Primitives - Math functions
    // ========================================================================

    sqrt: UnaryOp,
    sin: UnaryOp,
    cos: UnaryOp,
    tan: UnaryOp,
    asin: UnaryOp,
    acos: UnaryOp,
    atan: UnaryOp,
    atan2: BinaryOp,
    sinh: UnaryOp,
    cosh: UnaryOp,
    tanh: UnaryOp,
    asinh: UnaryOp,
    acosh: UnaryOp,
    atanh: UnaryOp,
    exp: UnaryOp,
    log: UnaryOp,
    floor: UnaryOp,
    ceiling: UnaryOp,
    round: UnaryOp,
    decode_float: UnaryOp,
    integer_decode_float: UnaryOp,
    float_radix: UnaryOp,
    float_digits: UnaryOp,

    // ========================================================================
    // Primitives - Vector operations
    // ========================================================================

    vec_new: struct {
        size: *const Ir,
        init: ?*const Ir,
    },
    vec: []const *const Ir, // (vector a b c)
    vec_ref: BinaryOp,
    vec_set: struct {
        vec: *const Ir,
        index: *const Ir,
        value: *const Ir,
    },
    vec_len: UnaryOp,
    vec_fill_ptr: UnaryOp,
    vec_set_fill_ptr: BinaryOp,
    vec_set_adjustable: BinaryOp,
    vec_set_character: BinaryOp,
    vec_push: BinaryOp,
    vec_push_ext: TernaryOp,
    vec_pop: UnaryOp,
    vec_adjust: TernaryOp,
    copy_structure: UnaryOp, // (copy-structure struct)
    elt_set: struct { // (setf (elt seq idx) val) - polymorphic
        seq: *const Ir,
        index: *const Ir,
        value: *const Ir,
    },

    // Array operations (multi-dimensional)
    arr_new: struct {
        dimensions: []const *const Ir,
        init: ?*const Ir,
    },
    arr_new_dyn: struct {
        dimensions: *const Ir,
        init: ?*const Ir,
    },
    arr_ref: struct {
        array: *const Ir,
        subscripts: []const *const Ir,
    },
    arr_set: struct {
        array: *const Ir,
        subscripts: []const *const Ir,
        value: *const Ir,
    },

    // ========================================================================
    // Primitives - CLOS operations
    // ========================================================================

    slot_value: BinaryOp, // (slot-value obj 'slot-name)
    set_slot_value: TernaryOp, // (%set-slot-value obj 'slot-name value)
    class_of: UnaryOp, // (class-of obj)
    find_class: UnaryOp, // (find-class name)
    set_find_class: BinaryOp, // (%set-find-class name class-or-nil)
    class_name: UnaryOp, // (class-name class)
    class_direct_superclasses: UnaryOp, // (class-direct-superclasses class)
    class_precedence_list: UnaryOp, // (class-precedence-list class)
    class_direct_slots: UnaryOp, // (class-direct-slots class)
    class_slots: UnaryOp, // (class-slots class)
    slot_definition_name: UnaryOp, // (slot-definition-name slot-def)
    slot_definition_initform: UnaryOp, // (slot-definition-initform slot-def)
    slot_definition_initargs: UnaryOp, // (slot-definition-initargs slot-def)
    slot_definition_readers: UnaryOp, // (slot-definition-readers slot-def)
    slot_definition_writers: UnaryOp, // (slot-definition-writers slot-def)
    slot_definition_allocation: UnaryOp, // (slot-definition-allocation slot-def)
    slot_definition_type: UnaryOp, // (slot-definition-type slot-def)
    make_generic_function: BinaryOp, // (%make-generic-function name lambda-list)
    make_unbound: void, // (%make-unbound)
    make_method: QuaternaryOp, // (%make-method qualifiers specializers lambda-list function)
    set_gf_dispatcher: BinaryOp, // (%set-gf-dispatcher gf dispatcher)
    add_method: BinaryOp, // (%add-method gf method)
    slot_boundp: BinaryOp, // (slot-boundp obj 'slot-name)
    slot_makunbound: BinaryOp, // (slot-makunbound obj 'slot-name)

    // ========================================================================
    // Primitives - Box operations (mutable cells)
    // ========================================================================

    make_box: UnaryOp,
    box_ref: UnaryOp,
    box_set: BinaryOp,

    // ========================================================================
    // Primitives - String operations
    // ========================================================================

    str_ref: BinaryOp,
    str_len: UnaryOp,
    str_set: struct {
        str: *const Ir,
        index: *const Ir,
        value: *const Ir,
    },
    str_concat: BinaryOp,
    str_eq: BinaryOp,
    str_lt: BinaryOp,
    str_gt: BinaryOp,
    str_le: BinaryOp,
    str_ge: BinaryOp,
    substring: struct {
        str: *const Ir,
        start: *const Ir,
        end: *const Ir,
    },

    // ========================================================================
    // Primitives - I/O and Symbol
    // ========================================================================

    write: UnaryOp,
    print: UnaryOp,
    random: UnaryOp,
    random_seed: UnaryOp,
    intern: UnaryOp,
    make_symbol: UnaryOp,
    unintern: BinaryOp,
    find_symbol: BinaryOp,
    sym_name: UnaryOp,
    copy_symbol: BinaryOp, // (copy-symbol sym copy-props)
    makunbound: UnaryOp, // (makunbound sym)
    set_sym_val: BinaryOp, // (set sym val)
    get: BinaryOp,
    put: TernaryOp,
    remprop: BinaryOp,
    type_of: UnaryOp, // Get type of value as symbol
    error_user: UnaryOp, // Signal user error with message

    // Stream I/O operations
    open: BinaryOp, // (open path mode)
    close: UnaryOp, // (close stream)
    read_line: UnaryOp, // (read-line stream)
    write_line: BinaryOp, // (write-line stream text)
    write_string: BinaryOp, // (write-string stream text)
    read_byte: UnaryOp, // (read-byte stream)
    write_byte: BinaryOp, // (write-byte stream byte)
    file_position: UnaryOp, // (file-position stream)
    set_file_position: BinaryOp, // (file-position stream pos)
    file_length: UnaryOp, // (file-length stream)
    finish_output: UnaryOp, // (finish-output stream)
    force_output: UnaryOp, // (force-output stream)
    clear_input: UnaryOp, // (clear-input stream)
    clear_output: UnaryOp, // (clear-output stream)
    sleep: UnaryOp, // (sleep seconds)

    // Reader macros
    set_macro_character: TernaryOp, // char, function, non-terminating-p
    get_macro_character: UnaryOp, // char -> (values function non-terminating-p)
    set_dispatch_macro_character: TernaryOp, // disp-char, sub-char, function
    get_dispatch_macro_character: BinaryOp, // disp-char, sub-char -> function

    // ========================================================================
    // Type assertions (gradual typing)
    // ========================================================================

    /// Assert value is fixnum (error if not)
    /// ( x -- x ) but errors if not fixnum
    assert_fixnum: UnaryOp,

    /// Assert value is cons (error if not)
    assert_cons: UnaryOp,

    /// Assert value is symbol (error if not)
    assert_symbol: UnaryOp,

    /// Assert value is string (error if not)
    assert_string: UnaryOp,

    /// Assert value is vector (error if not)
    assert_vector: UnaryOp,

    /// Assert value is closure (error if not)
    assert_closure: UnaryOp,

    /// Assert value is non-nil (error if nil)
    assert_non_nil: UnaryOp,

    /// Assert value is list (nil or cons)
    assert_list: UnaryOp,

    /// Assert value matches one of multiple types (union type)
    /// (or T1 T2 ...) -> checks if value is T1 OR T2 OR ...
    assert_or: struct {
        /// The value to check
        operand: *const Ir,
        /// List of type symbols to check against
        type_symbols: []const runtime.Value,
    },

    /// Assert value satisfies a refinement predicate
    /// (the (refine T x P) expr) -> evaluates expr, applies predicate, errors if false
    assert_refine: struct {
        /// The value to check
        operand: *const Ir,
        /// The predicate function (a lambda that takes the value and returns bool)
        predicate: *const Ir,
        /// Base type assertion (applied before predicate)
        base_type: ?*const types.Type,
    },

    /// Dependent pair introduction: (dpair first second type)
    /// Creates a pair where the type of second can depend on first
    dpair: struct {
        first: *const Ir,
        second: *const Ir,
        /// The Sigma type for type checking (optional)
        sigma_type: ?*const types.Type,
    },

    /// Dependent pair first projection with type info
    dfst: struct {
        pair: *const Ir,
        /// The expected type of the first element
        type_info: ?*const types.Type,
    },

    /// Dependent pair second projection with type info
    dsnd: struct {
        pair: *const Ir,
        /// The expected type of the second element (may depend on first)
        type_info: ?*const types.Type,
    },

    // ========================================================================
    // Specialized (type-proven) operations
    // ========================================================================

    /// Unboxed fixnum addition (no tag check, proven fixnum operands)
    fixnum_add: BinaryOp,
    /// Unboxed fixnum subtraction (no tag check, proven fixnum operands)
    fixnum_sub: BinaryOp,
    /// Unboxed fixnum multiplication (no tag check, proven fixnum operands)
    fixnum_mul: BinaryOp,
    /// Unboxed fixnum comparisons (no tag check, result is t or nil)
    fixnum_le: BinaryOp,
    fixnum_lt: BinaryOp,
    fixnum_gt: BinaryOp,
    fixnum_ge: BinaryOp,
    fixnum_eq: BinaryOp,
    /// Cons car without nil check (proven cons operand)
    unsafe_car: UnaryOp,
    /// Cons cdr without nil check (proven cons operand)
    unsafe_cdr: UnaryOp,
    /// Array access without bounds check (proven valid index)
    direct_aref: BinaryOp,

    // ========================================================================
    // Helper types
    // ========================================================================

    pub const BinaryOp = struct {
        left: *const Ir,
        right: *const Ir,
    };

    pub const UnaryOp = struct {
        operand: *const Ir,
    };

    pub const OptionalOp = struct {
        operand: ?*const Ir,
    };

    pub const TernaryOp = struct {
        first: *const Ir,
        second: *const Ir,
        third: *const Ir,
    };

    pub const QuaternaryOp = struct {
        first: *const Ir,
        second: *const Ir,
        third: *const Ir,
        fourth: *const Ir,
    };

    pub const Binding = struct {
        name: []const u8,
        value: *const Ir,
        /// Stack slot index for this binding (for proper nested let handling)
        index: u16,
    };

    pub const Capture = struct {
        name: []const u8,
        /// Depth in enclosing scope chain
        depth: u16,
        /// Index in that scope
        index: u16,
    };

    /// Optional parameter with default value
    pub const OptionalParam = struct {
        name: []const u8,
        default: ?*const Ir, // null means nil default
        supplied_p: ?[]const u8, // supplied-p variable name, or null
        supplied_p_idx: ?u16 = null, // local slot index for supplied-p, set by compiler
    };

    /// Keyword parameter with default value
    pub const KeyParam = struct {
        /// The keyword name (without leading colon)
        keyword: []const u8,
        /// The parameter name (may differ from keyword)
        name: []const u8,
        default: ?*const Ir, // null means nil default
        supplied_p: ?[]const u8, // supplied-p variable name, or null
        supplied_p_idx: ?u16 = null, // local slot index for supplied-p, set by compiler
    };

    pub const SpecialBindingStage = enum(u8) {
        required,
        optional,
        rest,
        key,
    };

    pub const SpecialBinding = struct {
        sym: Value,
        idx: u16,
        stage: SpecialBindingStage,
    };

    // ========================================================================
    // Predicates
    // ========================================================================

    /// Check if this node is in tail position (for TCO)
    pub fn isTailPosition(self: Ir) bool {
        return switch (self) {
            .tailcall => true,
            else => false,
        };
    }

    /// Check if this is a primitive operation
    pub fn isPrimitive(self: Ir) bool {
        return switch (self) {
            .add,
            .sub,
            .mul,
            .div,
            .mod,
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
            .list,
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
            .listp,
            .atom,
            .rationalp,
            .complexp,
            .make_complex,
            .real_part,
            .imag_part,
            .numerator,
            .denominator,
            .hashtablep,
            .packagep,
            .symbol_package,
            .package_name,
            .package_nicknames,
            .package_use_list,
            .package_used_by_list,
            .package_shadowing_symbols,
            .find_package,
            .delete_package,
            .pkg_find_all_symbols,
            .streamp,
            .input_stream_p,
            .output_stream_p,
            .make_string_input_stream,
            .make_string_output_stream,
            .get_output_stream_string,
            .vec_new,
            .vec,
            .vec_ref,
            .vec_set,
            .vec_len,
            .vec_fill_ptr,
            .vec_set_fill_ptr,
            .vec_set_adjustable,
            .vec_set_character,
            .vec_push,
            .vec_push_ext,
            .vec_pop,
            .vec_adjust,
            .copy_structure,
            .arr_new,
            .arr_new_dyn,
            .arr_ref,
            .arr_set,
            .slot_value,
            .make_box,
            .box_ref,
            .box_set,
            .str_ref,
            .str_len,
            .str_concat,
            .str_eq,
            .str_lt,
            .str_gt,
            .str_le,
            .str_ge,
            .substring,
            .string_upcase,
            .string_downcase,
            .print,
            .random,
            .intern,
            .make_symbol,
            .sym_name,
            .get,
            .put,
            .remprop,
            .type_of,
            .function_lambda_expression,
            .assert_fixnum,
            .assert_cons,
            .assert_symbol,
            .assert_string,
            .assert_vector,
            .assert_closure,
            .assert_non_nil,
            .assert_list,
            => true,
            else => false,
        };
    }

    /// Get the active tag name for debugging
    pub fn tagName(self: Ir) []const u8 {
        return @tagName(self);
    }
};

// ============================================================================
// IR Builder - allocates IR nodes
// ============================================================================

pub const IrBuilder = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) IrBuilder {
        return .{ .allocator = allocator };
    }

    // Literals
    pub fn lit(self: IrBuilder, v: Value) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lit = v };
        return node;
    }

    pub fn quoteSym(self: IrBuilder, name: []const u8) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .quote_sym = name_copy };
        return node;
    }

    // Variables
    pub fn variable(self: IrBuilder, name: []const u8, depth: u16, index: u16) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .@"var" = .{ .name = name_copy, .depth = depth, .index = index } };
        return node;
    }

    pub fn set(self: IrBuilder, name: []const u8, depth: u16, index: u16, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .set = .{ .name = name_copy, .depth = depth, .index = index, .value = value } };
        return node;
    }

    pub fn globalRef(self: IrBuilder, name: []const u8, index: u16) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .global_ref = .{ .name = name_copy, .index = index } };
        return node;
    }

    pub fn define(self: IrBuilder, name: []const u8, index: u16, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .define = .{ .name = name_copy, .index = index, .value = value } };
        return node;
    }

    // Binding forms
    pub fn letExpr(self: IrBuilder, bindings: []const Ir.Binding, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const bindings_copy = try self.allocator.dupe(Ir.Binding, bindings);
        node.* = .{ .let = .{ .bindings = bindings_copy, .body = body } };
        return node;
    }

    pub fn lambda(self: IrBuilder, params: []const []const u8, optional_params: []const Ir.OptionalParam, key_params: []const Ir.KeyParam, allow_other_keys: bool, key_temp_start: u16, rest_param: ?[]const u8, captures: []const Ir.Capture, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        // Copy params
        var params_copy = try self.allocator.alloc([]const u8, params.len);
        for (params, 0..) |p, i| {
            params_copy[i] = try self.allocator.dupe(u8, p);
        }
        // Copy optional params
        var opt_copy = try self.allocator.alloc(Ir.OptionalParam, optional_params.len);
        for (optional_params, 0..) |op, i| {
            opt_copy[i] = .{
                .name = try self.allocator.dupe(u8, op.name),
                .default = op.default,
                .supplied_p = if (op.supplied_p) |sp| try self.allocator.dupe(u8, sp) else null,
                .supplied_p_idx = op.supplied_p_idx,
            };
        }
        // Copy key params
        var key_copy = try self.allocator.alloc(Ir.KeyParam, key_params.len);
        for (key_params, 0..) |kp, i| {
            key_copy[i] = .{
                .keyword = try self.allocator.dupe(u8, kp.keyword),
                .name = try self.allocator.dupe(u8, kp.name),
                .default = kp.default,
                .supplied_p = if (kp.supplied_p) |sp| try self.allocator.dupe(u8, sp) else null,
                .supplied_p_idx = kp.supplied_p_idx,
            };
        }
        const captures_copy = try self.allocator.dupe(Ir.Capture, captures);
        const rest_copy = if (rest_param) |rp| try self.allocator.dupe(u8, rp) else null;
        node.* = .{ .lambda = .{
            .params = params_copy,
            .optional_params = opt_copy,
            .key_params = key_copy,
            .allow_other_keys = allow_other_keys,
            .key_temp_start = key_temp_start,
            .rest_param = rest_copy,
            .special_bindings = &.{},
            .captures = captures_copy,
            .body = body,
        } };
        return node;
    }

    // Control flow
    pub fn ifExpr(self: IrBuilder, cond: *const Ir, then_branch: *const Ir, else_branch: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .@"if" = .{ .cond = cond, .then_branch = then_branch, .else_branch = else_branch } };
        return node;
    }

    pub fn progn(self: IrBuilder, exprs: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const exprs_copy = try self.allocator.dupe(*const Ir, exprs);
        node.* = .{ .progn = exprs_copy };
        return node;
    }

    pub fn loop(self: IrBuilder, cond: *const Ir, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .loop = .{ .cond = cond, .body = body } };
        return node;
    }

    pub fn block(self: IrBuilder, name: Value, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .block = .{ .name = name, .body = body } };
        return node;
    }

    pub fn returnFrom(self: IrBuilder, name: Value, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .return_from = .{ .name = name, .value = value } };
        return node;
    }

    pub fn unwindProtect(self: IrBuilder, protected: *const Ir, cleanup: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .unwind_protect = .{ .protected = protected, .cleanup = cleanup } };
        return node;
    }

    pub fn @"catch"(self: IrBuilder, tag: *const Ir, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .@"catch" = .{ .tag = tag, .body = body } };
        return node;
    }

    pub fn throw(self: IrBuilder, tag: *const Ir, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .throw = .{ .tag = tag, .value = value } };
        return node;
    }

    pub fn signal(self: IrBuilder, condition_type: *const Ir, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .signal = .{ .condition_type = condition_type, .value = value } };
        return node;
    }

    pub fn progv(self: IrBuilder, symbols: *const Ir, vals: *const Ir, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .progv = .{ .symbols = symbols, .values = vals, .body = body } };
        return node;
    }

    pub fn handlerCase(self: IrBuilder, tag: *const Ir, body: *const Ir, handler: *const Ir, cond_var: []const u8, cond_idx: u16) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .handler_case = .{
            .tag = tag,
            .body = body,
            .handler = handler,
            .cond_var = cond_var,
            .cond_idx = cond_idx,
        } };
        return node;
    }

    /// Single-binding let for handler-case variable binding
    pub fn let1(self: IrBuilder, name: []const u8, index: u16, initializer: *const Ir, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const bindings = try self.allocator.alloc(Ir.Binding, 1);
        bindings[0] = .{ .name = name, .value = initializer, .index = index };
        node.* = .{ .let = .{ .bindings = bindings, .body = body } };
        return node;
    }

    pub fn tagbody(self: IrBuilder, tags: []const Value, segments: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const tags_copy = try self.allocator.dupe(Value, tags);
        const segments_copy = try self.allocator.dupe(*const Ir, segments);
        node.* = .{ .tagbody = .{ .tags = tags_copy, .segments = segments_copy } };
        return node;
    }

    pub fn go(self: IrBuilder, tag: Value) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .go = .{ .tag = tag } };
        return node;
    }

    pub fn values(self: IrBuilder, vals: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const vals_copy = try self.allocator.dupe(*const Ir, vals);
        node.* = .{ .values = vals_copy };
        return node;
    }

    pub fn valuesList(self: IrBuilder, lst: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .values_list = .{ .operand = lst } };
        return node;
    }

    pub fn mvBind(self: IrBuilder, vars: []const []const u8, start_index: u16, expr: *const Ir, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const vars_copy = try self.allocator.dupe([]const u8, vars);
        node.* = .{ .mv_bind = .{ .vars = vars_copy, .start_index = start_index, .expr = expr, .body = body, .special_bindings = &.{} } };
        return node;
    }

    pub fn mvCall(self: IrBuilder, func: *const Ir, forms: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const forms_copy = try self.allocator.dupe(*const Ir, forms);
        node.* = .{ .mv_call = .{ .func = func, .forms = forms_copy } };
        return node;
    }

    pub fn mvList(self: IrBuilder, expr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .mv_list = .{ .expr = expr } };
        return node;
    }

    pub fn format(self: IrBuilder, dest: *const Ir, control: *const Ir, args: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const args_copy = try self.allocator.dupe(*const Ir, args);
        node.* = .{ .format = .{ .dest = dest, .control = control, .args = args_copy } };
        return node;
    }

    // Function calls
    pub fn call(self: IrBuilder, func: *const Ir, args: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const args_copy = try self.allocator.dupe(*const Ir, args);
        node.* = .{ .call = .{ .func = func, .args = args_copy } };
        return node;
    }

    pub fn tailcall(self: IrBuilder, func: *const Ir, args: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const args_copy = try self.allocator.dupe(*const Ir, args);
        node.* = .{ .tailcall = .{ .func = func, .args = args_copy } };
        return node;
    }

    // Arithmetic
    pub fn add(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .add = .{ .left = left, .right = right } };
        return node;
    }

    pub fn sub(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .sub = .{ .left = left, .right = right } };
        return node;
    }

    pub fn mul(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .mul = .{ .left = left, .right = right } };
        return node;
    }

    pub fn div(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .div = .{ .left = left, .right = right } };
        return node;
    }

    pub fn quot(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .quot = .{ .left = left, .right = right } };
        return node;
    }

    pub fn rem(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .rem = .{ .left = left, .right = right } };
        return node;
    }

    // Comparison
    pub fn eq(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .eq = .{ .left = left, .right = right } };
        return node;
    }

    pub fn lt(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lt = .{ .left = left, .right = right } };
        return node;
    }

    pub fn gt(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .gt = .{ .left = left, .right = right } };
        return node;
    }

    pub fn equal(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .equal = .{ .left = left, .right = right } };
        return node;
    }

    pub fn eql(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .eql = .{ .left = left, .right = right } };
        return node;
    }

    pub fn equalp(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .equalp = .{ .left = left, .right = right } };
        return node;
    }

    pub fn numEq(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .num_eq = .{ .left = left, .right = right } };
        return node;
    }

    pub fn le(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .le = .{ .left = left, .right = right } };
        return node;
    }

    pub fn ge(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .ge = .{ .left = left, .right = right } };
        return node;
    }

    // List operations
    pub fn cons(self: IrBuilder, car_val: *const Ir, cdr_val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .cons = .{ .left = car_val, .right = cdr_val } };
        return node;
    }

    pub fn car(self: IrBuilder, pair_val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .car = .{ .operand = pair_val } };
        return node;
    }

    pub fn cdr(self: IrBuilder, pair_val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .cdr = .{ .operand = pair_val } };
        return node;
    }

    pub fn list(self: IrBuilder, elements: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const elems_copy = try self.allocator.dupe(*const Ir, elements);
        node.* = .{ .list = elems_copy };
        return node;
    }

    pub fn listStar(self: IrBuilder, elements: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const elems_copy = try self.allocator.dupe(*const Ir, elements);
        node.* = .{ .list_star = elems_copy };
        return node;
    }

    pub fn append(self: IrBuilder, list1: *const Ir, list2: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .append = .{ .left = list1, .right = list2 } };
        return node;
    }

    // Type predicates
    pub fn consp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .consp = .{ .operand = operand } };
        return node;
    }

    pub fn symbolp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .symbolp = .{ .operand = operand } };
        return node;
    }

    pub fn numberp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .numberp = .{ .operand = operand } };
        return node;
    }

    pub fn integerp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .integerp = .{ .operand = operand } };
        return node;
    }

    pub fn realp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .realp = .{ .operand = operand } };
        return node;
    }

    pub fn nilp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .nilp = .{ .operand = operand } };
        return node;
    }

    pub fn characterp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .characterp = .{ .operand = operand } };
        return node;
    }

    pub fn floatp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .floatp = .{ .operand = operand } };
        return node;
    }

    pub fn listp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .listp = .{ .operand = operand } };
        return node;
    }

    pub fn atomp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .atom = .{ .operand = operand } };
        return node;
    }

    pub fn rationalp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .rationalp = .{ .operand = operand } };
        return node;
    }

    pub fn complexp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .complexp = .{ .operand = operand } };
        return node;
    }

    pub fn makeComplex(self: IrBuilder, real: *const Ir, imag: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_complex = .{ .left = real, .right = imag } };
        return node;
    }

    pub fn realPart(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .real_part = .{ .operand = operand } };
        return node;
    }

    pub fn imagPart(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .imag_part = .{ .operand = operand } };
        return node;
    }

    pub fn numerator(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .numerator = .{ .operand = operand } };
        return node;
    }

    pub fn denominator(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .denominator = .{ .operand = operand } };
        return node;
    }

    // Stream operations
    pub fn streamp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .streamp = .{ .operand = operand } };
        return node;
    }

    pub fn inputStreamP(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .input_stream_p = .{ .operand = operand } };
        return node;
    }

    pub fn outputStreamP(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .output_stream_p = .{ .operand = operand } };
        return node;
    }

    pub fn openStreamP(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .open_stream_p = .{ .operand = operand } };
        return node;
    }

    pub fn interactiveStreamP(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .interactive_stream_p = .{ .operand = operand } };
        return node;
    }

    pub fn streamElementType(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .stream_element_type = .{ .operand = operand } };
        return node;
    }

    pub fn streamExternalFormat(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .stream_external_format = .{ .operand = operand } };
        return node;
    }

    pub fn makeStringInputStream(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_string_input_stream = .{ .operand = operand } };
        return node;
    }

    pub fn makeStringOutputStream(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_string_output_stream = {} };
        return node;
    }

    pub fn getOutputStreamString(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .get_output_stream_string = .{ .operand = operand } };
        return node;
    }

    pub fn writeToStream(self: IrBuilder, str: *const Ir, stream: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .write_to_stream = .{ .left = str, .right = stream } };
        return node;
    }

    pub fn hashtablep(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .hashtablep = .{ .operand = operand } };
        return node;
    }

    pub fn packagep(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .packagep = .{ .operand = operand } };
        return node;
    }

    pub fn symbolPackage(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .symbol_package = .{ .operand = operand } };
        return node;
    }

    pub fn packageName(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .package_name = .{ .operand = operand } };
        return node;
    }

    pub fn packageNicknames(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .package_nicknames = .{ .operand = operand } };
        return node;
    }

    pub fn packageUseList(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .package_use_list = .{ .operand = operand } };
        return node;
    }

    pub fn packageUsedByList(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .package_used_by_list = .{ .operand = operand } };
        return node;
    }

    pub fn packageShadowingSymbols(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .package_shadowing_symbols = .{ .operand = operand } };
        return node;
    }

    pub fn packageSymbolsList(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .package_symbols_list = .{ .operand = operand } };
        return node;
    }

    pub fn packageExportsList(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .package_exports_list = .{ .operand = operand } };
        return node;
    }

    pub fn listAllPackages(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .list_all_packages = .{} };
        return node;
    }

    pub fn computeRestarts(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .compute_restarts = .{} };
        return node;
    }

    pub fn restartName(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .restart_name = .{ .operand = operand } };
        return node;
    }

    pub fn directory(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .directory = .{ .operand = operand } };
        return node;
    }

    pub fn enoughNamestring(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .enough_namestring = .{ .operand = operand } };
        return node;
    }

    pub fn findPackage(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .find_package = .{ .operand = operand } };
        return node;
    }

    pub fn deletePackage(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .delete_package = .{ .operand = operand } };
        return node;
    }

    pub fn findAllSymbols(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .pkg_find_all_symbols = .{ .operand = operand } };
        return node;
    }

    pub fn aproposList(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .apropos_list = .{ .operand = operand } };
        return node;
    }

    pub fn readCharNoHang(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read_char_no_hang = .{ .operand = operand } };
        return node;
    }

    pub fn closurep(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .closurep = .{ .operand = operand } };
        return node;
    }

    pub fn keywordp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .keywordp = .{ .operand = operand } };
        return node;
    }

    pub fn methodQualifiers(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .method_qualifiers = .{ .operand = operand } };
        return node;
    }

    pub fn methodSpecializers(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .method_specializers = .{ .operand = operand } };
        return node;
    }

    pub fn methodFunction(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .method_function = .{ .operand = operand } };
        return node;
    }

    pub fn genericFunctionMethods(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .generic_function_methods = .{ .operand = operand } };
        return node;
    }

    pub fn genericFunctionLambdaList(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .generic_function_lambda_list = .{ .operand = operand } };
        return node;
    }

    pub fn genericFunctionName(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .generic_function_name = .{ .operand = operand } };
        return node;
    }

    pub fn stringp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .stringp = .{ .operand = operand } };
        return node;
    }

    pub fn vectorp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .vectorp = .{ .operand = operand } };
        return node;
    }

    pub fn structp(self: IrBuilder, operand: *const Ir, struct_name: []const u8, struct_type: *const types.Type) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, struct_name);
        node.* = .{ .struct_p = .{
            .operand = operand,
            .struct_name = name_copy,
            .struct_type = struct_type,
        } };
        return node;
    }

    pub fn assoc(self: IrBuilder, key: *const Ir, alist: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assoc = .{ .left = key, .right = alist } };
        return node;
    }

    pub fn rplaca(self: IrBuilder, cons_ir: *const Ir, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .rplaca = .{ .left = cons_ir, .right = value } };
        return node;
    }

    pub fn rplacd(self: IrBuilder, cons_ir: *const Ir, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .rplacd = .{ .left = cons_ir, .right = value } };
        return node;
    }

    pub fn not(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .not = .{ .operand = operand } };
        return node;
    }

    // Character operations
    pub fn charCode(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_code = .{ .operand = operand } };
        return node;
    }

    pub fn codeChar(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .code_char = .{ .operand = operand } };
        return node;
    }

    pub fn charEq(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_eq = .{ .left = left, .right = right } };
        return node;
    }

    pub fn charLt(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_lt = .{ .left = left, .right = right } };
        return node;
    }

    pub fn charGt(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_gt = .{ .left = left, .right = right } };
        return node;
    }

    pub fn readChar(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read_char = {} };
        return node;
    }

    pub fn peekChar(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .peek_char = {} };
        return node;
    }

    pub fn readSexp(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read = {} };
        return node;
    }

    pub fn load(self: IrBuilder, filename: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .load = .{ .operand = filename } };
        return node;
    }

    pub fn readFromString(self: IrBuilder, str: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read_from_string = .{ .operand = str } };
        return node;
    }

    pub fn readStream(self: IrBuilder, stream: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read_stream = .{ .operand = stream } };
        return node;
    }

    pub fn unreadChar(self: IrBuilder, char: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .unread_char = .{ .operand = char } };
        return node;
    }

    pub fn unreadCharStream(self: IrBuilder, char: *const Ir, stream: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .unread_char_stream = .{ .left = char, .right = stream } };
        return node;
    }

    pub fn listen(self: IrBuilder, stream: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .listen = .{ .operand = stream } };
        return node;
    }

    pub fn eval(self: IrBuilder, expr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .eval = .{ .operand = expr } };
        return node;
    }

    pub fn gensym(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .gensym = .{ .operand = null } };
        return node;
    }

    pub fn macroexpand(self: IrBuilder, expr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .macroexpand = .{ .operand = expr } };
        return node;
    }

    pub fn macroexpand1(self: IrBuilder, expr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .macroexpand_1 = .{ .operand = expr } };
        return node;
    }

    pub fn princ(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .princ = .{ .operand = val } };
        return node;
    }

    pub fn terpri(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .terpri = {} };
        return node;
    }

    pub fn writeChar(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .write_char = .{ .operand = val } };
        return node;
    }

    pub fn charUpcase(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_upcase = .{ .operand = val } };
        return node;
    }

    pub fn charDowncase(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_downcase = .{ .operand = val } };
        return node;
    }

    pub fn digitCharP(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .digit_char_p = .{ .operand = val } };
        return node;
    }

    pub fn alphaCharP(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .alpha_char_p = .{ .operand = val } };
        return node;
    }

    pub fn parseInteger(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .parse_integer = .{ .operand = val } };
        return node;
    }

    pub fn writeToString(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .write_to_string = .{ .operand = val } };
        return node;
    }

    pub fn logand(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logand = .{ .left = left, .right = right } };
        return node;
    }

    pub fn logior(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logior = .{ .left = left, .right = right } };
        return node;
    }

    pub fn logxor(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logxor = .{ .left = left, .right = right } };
        return node;
    }

    pub fn lognot(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lognot = .{ .operand = val } };
        return node;
    }

    pub fn ash(self: IrBuilder, n: *const Ir, count: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .ash = .{ .left = n, .right = count } };
        return node;
    }

    pub fn lognand(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lognand = .{ .left = left, .right = right } };
        return node;
    }

    pub fn lognor(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lognor = .{ .left = left, .right = right } };
        return node;
    }

    pub fn logandc1(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logandc1 = .{ .left = left, .right = right } };
        return node;
    }

    pub fn logandc2(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logandc2 = .{ .left = left, .right = right } };
        return node;
    }

    pub fn logeqv(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logeqv = .{ .left = left, .right = right } };
        return node;
    }

    pub fn logbitp(self: IrBuilder, index: *const Ir, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logbitp = .{ .left = index, .right = n } };
        return node;
    }

    pub fn logcount(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logcount = .{ .operand = val } };
        return node;
    }

    pub fn integerLength(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .integer_length = .{ .operand = val } };
        return node;
    }

    pub fn readFile(self: IrBuilder, path: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read_file = .{ .operand = path } };
        return node;
    }

    pub fn writeFile(self: IrBuilder, path: *const Ir, content: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .write_file = .{ .left = path, .right = content } };
        return node;
    }

    pub fn deleteFile(self: IrBuilder, path: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .delete_file = .{ .operand = path } };
        return node;
    }

    pub fn renameFile(self: IrBuilder, old_path: *const Ir, new_path: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .rename_file = .{ .left = old_path, .right = new_path } };
        return node;
    }

    pub fn probeFile(self: IrBuilder, path: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .probe_file = .{ .operand = path } };
        return node;
    }

    pub fn fileWriteDate(self: IrBuilder, path: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .file_write_date = .{ .operand = path } };
        return node;
    }

    pub fn fileAuthor(self: IrBuilder, path: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .file_author = .{ .operand = path } };
        return node;
    }

    pub fn getUniversalTime(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .get_universal_time = {} };
        return node;
    }

    pub fn getInternalRealTime(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .get_internal_real_time = {} };
        return node;
    }

    pub fn getInternalRunTime(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .get_internal_run_time = {} };
        return node;
    }

    pub fn getDecodedTime(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .get_decoded_time = {} };
        return node;
    }

    pub fn room(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .room = {} };
        return node;
    }

    pub fn lispImplementationType(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lisp_implementation_type = {} };
        return node;
    }

    pub fn lispImplementationVersion(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lisp_implementation_version = {} };
        return node;
    }

    pub fn softwareType(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .software_type = {} };
        return node;
    }

    pub fn machineType(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .machine_type = {} };
        return node;
    }

    pub fn machineInstance(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .machine_instance = {} };
        return node;
    }

    pub fn machineVersion(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .machine_version = {} };
        return node;
    }

    pub fn softwareVersion(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .software_version = {} };
        return node;
    }

    pub fn shortSiteName(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .short_site_name = {} };
        return node;
    }

    pub fn longSiteName(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .long_site_name = {} };
        return node;
    }

    pub fn userHomedirPathname(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .user_homedir_pathname = {} };
        return node;
    }

    pub fn makePathname(self: IrBuilder, h: *const Ir, dev: *const Ir, dir: *const Ir, n: *const Ir, ty: *const Ir, ver: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_pathname = .{
            .host = h,
            .device = dev,
            .directory = dir,
            .name = n,
            .type = ty,
            .version = ver,
        } };
        return node;
    }

    pub fn makeString(self: IrBuilder, len: *const Ir, char: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_string = .{ .left = len, .right = char } };
        return node;
    }

    pub fn stringToList(self: IrBuilder, str: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .string_to_list = .{ .operand = str } };
        return node;
    }

    pub fn listToString(self: IrBuilder, lst: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .list_to_string = .{ .operand = lst } };
        return node;
    }

    pub fn stringUpcase(self: IrBuilder, str: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .string_upcase = .{ .operand = str } };
        return node;
    }

    pub fn stringDowncase(self: IrBuilder, str: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .string_downcase = .{ .operand = str } };
        return node;
    }

    pub fn boundp(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .boundp = .{ .operand = sym } };
        return node;
    }

    pub fn fboundp(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .fboundp = .{ .operand = sym } };
        return node;
    }

    pub fn symbolValue(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .symbol_value = .{ .operand = sym } };
        return node;
    }

    pub fn arrayRank(self: IrBuilder, arr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .array_rank = .{ .operand = arr } };
        return node;
    }

    pub fn arrayTotalSize(self: IrBuilder, arr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .array_total_size = .{ .operand = arr } };
        return node;
    }

    pub fn arrayDimensions(self: IrBuilder, arr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .array_dimensions = .{ .operand = arr } };
        return node;
    }

    pub fn symbolFunction(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .symbol_function = .{ .operand = sym } };
        return node;
    }

    pub fn symbolPlist(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .symbol_plist = .{ .operand = sym } };
        return node;
    }

    pub fn setSymbolFunction(self: IrBuilder, sym: *const Ir, func: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .set_symbol_function = .{ .left = sym, .right = func } };
        return node;
    }

    pub fn setSymbolPlist(self: IrBuilder, sym: *const Ir, plist: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .set_symbol_plist = .{ .left = sym, .right = plist } };
        return node;
    }

    pub fn functionLambdaExpression(self: IrBuilder, fn_val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .function_lambda_expression = .{ .operand = fn_val } };
        return node;
    }

    pub fn typep(self: IrBuilder, obj: *const Ir, type_spec: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .typep = .{ .left = obj, .right = type_spec } };
        return node;
    }

    pub fn subtypep(self: IrBuilder, type1: *const Ir, type2: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .subtypep = .{ .left = type1, .right = type2 } };
        return node;
    }

    // Numeric predicates
    pub fn abs(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .abs = .{ .operand = n } };
        return node;
    }

    pub fn zerop(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .zerop = .{ .operand = n } };
        return node;
    }

    pub fn plusp(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .plusp = .{ .operand = n } };
        return node;
    }

    pub fn minusp(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .minusp = .{ .operand = n } };
        return node;
    }

    pub fn evenp(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .evenp = .{ .operand = n } };
        return node;
    }

    pub fn oddp(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .oddp = .{ .operand = n } };
        return node;
    }

    // Math functions
    pub fn sqrt(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .sqrt = .{ .operand = n } };
        return node;
    }

    pub fn sin(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .sin = .{ .operand = n } };
        return node;
    }

    pub fn cos(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .cos = .{ .operand = n } };
        return node;
    }

    pub fn tan(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .tan = .{ .operand = n } };
        return node;
    }

    pub fn asin(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .asin = .{ .operand = n } };
        return node;
    }

    pub fn acos(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .acos = .{ .operand = n } };
        return node;
    }

    pub fn atan(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .atan = .{ .operand = n } };
        return node;
    }

    pub fn atan2(self: IrBuilder, y: *const Ir, x: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .atan2 = .{ .left = y, .right = x } };
        return node;
    }

    pub fn sinh(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .sinh = .{ .operand = n } };
        return node;
    }

    pub fn cosh(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .cosh = .{ .operand = n } };
        return node;
    }

    pub fn tanh(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .tanh = .{ .operand = n } };
        return node;
    }

    pub fn asinh(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .asinh = .{ .operand = n } };
        return node;
    }

    pub fn acosh(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .acosh = .{ .operand = n } };
        return node;
    }

    pub fn atanh(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .atanh = .{ .operand = n } };
        return node;
    }

    pub fn exp_fn(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .exp = .{ .operand = n } };
        return node;
    }

    pub fn log_fn(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .log = .{ .operand = n } };
        return node;
    }

    pub fn floor_fn(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .floor = .{ .operand = n } };
        return node;
    }

    pub fn ceiling(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .ceiling = .{ .operand = n } };
        return node;
    }

    pub fn round_fn(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .round = .{ .operand = n } };
        return node;
    }

    // Vector operations
    pub fn vecNew(self: IrBuilder, size: *const Ir, init_val: ?*const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .vec_new = .{ .size = size, .init = init_val } };
        return node;
    }

    pub fn vec(self: IrBuilder, elements: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const elems_copy = try self.allocator.dupe(*const Ir, elements);
        node.* = .{ .vec = elems_copy };
        return node;
    }

    pub fn vecRef(self: IrBuilder, v: *const Ir, index: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .vec_ref = .{ .left = v, .right = index } };
        return node;
    }

    pub fn vecLen(self: IrBuilder, v: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .vec_len = .{ .operand = v } };
        return node;
    }

    pub fn copyStructure(self: IrBuilder, v: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .copy_structure = .{ .operand = v } };
        return node;
    }

    // Array operations
    pub fn arrNew(self: IrBuilder, dimensions: []const *const Ir, init_val: ?*const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const dims_copy = try self.allocator.dupe(*const Ir, dimensions);
        node.* = .{ .arr_new = .{ .dimensions = dims_copy, .init = init_val } };
        return node;
    }

    pub fn arrNewDynamic(self: IrBuilder, dimensions: *const Ir, init_val: ?*const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .arr_new_dyn = .{ .dimensions = dimensions, .init = init_val } };
        return node;
    }

    pub fn arrRef(self: IrBuilder, array: *const Ir, subscripts: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const subs_copy = try self.allocator.dupe(*const Ir, subscripts);
        node.* = .{ .arr_ref = .{ .array = array, .subscripts = subs_copy } };
        return node;
    }

    pub fn arrSet(self: IrBuilder, array: *const Ir, subscripts: []const *const Ir, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const subs_copy = try self.allocator.dupe(*const Ir, subscripts);
        node.* = .{ .arr_set = .{ .array = array, .subscripts = subs_copy, .value = value } };
        return node;
    }

    // CLOS operations
    pub fn slotValue(self: IrBuilder, obj: *const Ir, slot_name: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .slot_value = .{ .left = obj, .right = slot_name } };
        return node;
    }

    pub fn classOf(self: IrBuilder, obj: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .class_of = .{ .operand = obj } };
        return node;
    }

    pub fn makeGenericFunction(self: IrBuilder, name: *const Ir, lambda_list: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_generic_function = .{ .left = name, .right = lambda_list } };
        return node;
    }

    pub fn setGfDispatcher(self: IrBuilder, gf: *const Ir, dispatcher: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .set_gf_dispatcher = .{ .left = gf, .right = dispatcher } };
        return node;
    }

    pub fn addMethod(self: IrBuilder, gf: *const Ir, method: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .add_method = .{ .left = gf, .right = method } };
        return node;
    }

    pub fn slotBoundp(self: IrBuilder, obj: *const Ir, slot_name: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .slot_boundp = .{ .left = obj, .right = slot_name } };
        return node;
    }

    pub fn slotMakunbound(self: IrBuilder, obj: *const Ir, slot_name: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .slot_makunbound = .{ .left = obj, .right = slot_name } };
        return node;
    }

    pub fn makeUnbound(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_unbound = {} };
        return node;
    }

    pub fn makeMethod(self: IrBuilder, qualifiers: *const Ir, specializers: *const Ir, lambda_list: *const Ir, function: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_method = .{ .first = qualifiers, .second = specializers, .third = lambda_list, .fourth = function } };
        return node;
    }

    pub fn setSlotValue(self: IrBuilder, obj: *const Ir, slot_name: *const Ir, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .set_slot_value = .{ .first = obj, .second = slot_name, .third = value } };
        return node;
    }

    pub fn vecSet(self: IrBuilder, v: *const Ir, index: *const Ir, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .vec_set = .{ .vec = v, .index = index, .value = val } };
        return node;
    }

    // String operations
    pub fn strLen(self: IrBuilder, s: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .str_len = .{ .operand = s } };
        return node;
    }

    pub fn strSet(self: IrBuilder, s: *const Ir, index: *const Ir, char: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .str_set = .{ .str = s, .index = index, .value = char } };
        return node;
    }

    pub fn strConcat(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .str_concat = .{ .left = left, .right = right } };
        return node;
    }

    // I/O
    pub fn write(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .write = .{ .operand = operand } };
        return node;
    }

    pub fn print(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .print = .{ .operand = operand } };
        return node;
    }

    pub fn typeOf(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .type_of = .{ .operand = operand } };
        return node;
    }

    pub fn errorUser(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .error_user = .{ .operand = operand } };
        return node;
    }

    // Type assertions
    pub fn assertFixnum(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_fixnum = .{ .operand = operand } };
        return node;
    }

    pub fn assertCons(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_cons = .{ .operand = operand } };
        return node;
    }

    pub fn assertSymbol(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_symbol = .{ .operand = operand } };
        return node;
    }

    pub fn assertString(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_string = .{ .operand = operand } };
        return node;
    }

    pub fn assertVector(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_vector = .{ .operand = operand } };
        return node;
    }

    pub fn assertClosure(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_closure = .{ .operand = operand } };
        return node;
    }

    pub fn assertNonNil(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_non_nil = .{ .operand = operand } };
        return node;
    }

    pub fn assertList(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_list = .{ .operand = operand } };
        return node;
    }

    /// Assert value matches one of multiple types (union type)
    pub fn assertOr(self: IrBuilder, operand: *const Ir, type_symbols: []const runtime.Value) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_or = .{
            .operand = operand,
            .type_symbols = type_symbols,
        } };
        return node;
    }

    // ========================================================================
    // Dependent type operations
    // ========================================================================

    /// Assert value satisfies a refinement type predicate
    pub fn assertRefine(self: IrBuilder, operand: *const Ir, predicate: *const Ir, base_type: ?*const types.Type) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_refine = .{
            .operand = operand,
            .predicate = predicate,
            .base_type = base_type,
        } };
        return node;
    }

    /// Create a dependent pair (Sigma type introduction)
    pub fn dpair(self: IrBuilder, first: *const Ir, second: *const Ir, sigma_type: ?*const types.Type) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .dpair = .{
            .first = first,
            .second = second,
            .sigma_type = sigma_type,
        } };
        return node;
    }

    /// Project first element of dependent pair
    pub fn dfst(self: IrBuilder, pair_ir: *const Ir, type_info: ?*const types.Type) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .dfst = .{
            .pair = pair_ir,
            .type_info = type_info,
        } };
        return node;
    }

    /// Project second element of dependent pair
    pub fn dsnd(self: IrBuilder, pair_ir: *const Ir, type_info: ?*const types.Type) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .dsnd = .{
            .pair = pair_ir,
            .type_info = type_info,
        } };
        return node;
    }
};

// ============================================================================
// Deep copy (for JIT inlining — keeps callee IR alive after arena is freed)
// ============================================================================

/// Deep-copy an IR tree to a new allocator. Handles the subset of nodes
/// that pass IrTranslator.canTranslate. Returns null for unsupported nodes.
fn deepCopyOptionalIr(allocator: std.mem.Allocator, src: ?*const Ir) anyerror!?*const Ir {
    if (src) |node| return try deepCopyIr(allocator, node);
    return null;
}

fn deepCopyIrSlice(allocator: std.mem.Allocator, src: []const *const Ir) anyerror![]const *const Ir {
    const dst = try allocator.alloc(*const Ir, src.len);
    for (src, 0..) |node, i| dst[i] = try deepCopyIr(allocator, node);
    return dst;
}

pub fn deepCopyIr(allocator: std.mem.Allocator, src: *const Ir) anyerror!*const Ir {
    const node = try allocator.create(Ir);
    node.* = switch (src.*) {
        .lit => |v| .{ .lit = v },
        .@"var" => |v| .{ .@"var" = .{
            .name = try allocator.dupe(u8, v.name),
            .depth = v.depth,
            .index = v.index,
        } },
        .global_ref => |g| .{ .global_ref = .{
            .name = try allocator.dupe(u8, g.name),
            .index = g.index,
        } },
        .set => |s| .{ .set = .{
            .name = try allocator.dupe(u8, s.name),
            .depth = s.depth,
            .index = s.index,
            .value = try deepCopyIr(allocator, s.value),
        } },
        .block => |b| .{ .block = .{
            .name = b.name,
            .body = try deepCopyIr(allocator, b.body),
        } },
        .@"if" => |f| .{ .@"if" = .{
            .cond = try deepCopyIr(allocator, f.cond),
            .then_branch = try deepCopyIr(allocator, f.then_branch),
            .else_branch = try deepCopyIr(allocator, f.else_branch),
        } },
        .progn => |exprs| blk: {
            const new_exprs = try deepCopyIrSlice(allocator, exprs);
            break :blk .{ .progn = new_exprs };
        },
        .let => |l| blk: {
            const new_bindings = try allocator.alloc(Ir.Binding, l.bindings.len);
            for (l.bindings, 0..) |b, i| {
                new_bindings[i] = .{
                    .name = try allocator.dupe(u8, b.name),
                    .value = try deepCopyIr(allocator, b.value),
                    .index = b.index,
                };
            }
            break :blk .{ .let = .{
                .bindings = new_bindings,
                .body = try deepCopyIr(allocator, l.body),
            } };
        },
        .loop => |l| .{ .loop = .{
            .cond = try deepCopyIr(allocator, l.cond),
            .body = try deepCopyIr(allocator, l.body),
        } },
        .progv => |p| .{ .progv = .{
            .symbols = try deepCopyIr(allocator, p.symbols),
            .values = try deepCopyIr(allocator, p.values),
            .body = try deepCopyIr(allocator, p.body),
        } },
        .call => |c| blk: {
            const new_args = try deepCopyIrSlice(allocator, c.args);
            break :blk .{ .call = .{
                .func = try deepCopyIr(allocator, c.func),
                .args = new_args,
            } };
        },
        .tailcall => |tc| blk: {
            const new_args = try deepCopyIrSlice(allocator, tc.args);
            break :blk .{ .tailcall = .{
                .func = try deepCopyIr(allocator, tc.func),
                .args = new_args,
            } };
        },
        .lambda => |lam| blk: {
            const new_params = try allocator.alloc([]const u8, lam.params.len);
            for (lam.params, 0..) |param, i| {
                new_params[i] = try allocator.dupe(u8, param);
            }

            const new_optional = try allocator.alloc(Ir.OptionalParam, lam.optional_params.len);
            for (lam.optional_params, 0..) |param, i| {
                new_optional[i] = .{
                    .name = try allocator.dupe(u8, param.name),
                    .default = try deepCopyOptionalIr(allocator, param.default),
                    .supplied_p = if (param.supplied_p) |sp| try allocator.dupe(u8, sp) else null,
                    .supplied_p_idx = param.supplied_p_idx,
                };
            }

            const new_key = try allocator.alloc(Ir.KeyParam, lam.key_params.len);
            for (lam.key_params, 0..) |param, i| {
                new_key[i] = .{
                    .keyword = try allocator.dupe(u8, param.keyword),
                    .name = try allocator.dupe(u8, param.name),
                    .default = try deepCopyOptionalIr(allocator, param.default),
                    .supplied_p = if (param.supplied_p) |sp| try allocator.dupe(u8, sp) else null,
                    .supplied_p_idx = param.supplied_p_idx,
                };
            }

            const new_captures = try allocator.alloc(Ir.Capture, lam.captures.len);
            for (lam.captures, 0..) |capture, i| {
                new_captures[i] = .{
                    .name = try allocator.dupe(u8, capture.name),
                    .depth = capture.depth,
                    .index = capture.index,
                };
            }

            const new_specials = try allocator.alloc(Ir.SpecialBinding, lam.special_bindings.len);
            for (lam.special_bindings, 0..) |binding, i| {
                new_specials[i] = .{
                    .sym = binding.sym,
                    .idx = binding.idx,
                    .stage = binding.stage,
                };
            }

            break :blk .{ .lambda = .{
                .params = new_params,
                .optional_params = new_optional,
                .key_params = new_key,
                .allow_other_keys = lam.allow_other_keys,
                .key_temp_start = lam.key_temp_start,
                .rest_param = if (lam.rest_param) |rest_param| try allocator.dupe(u8, rest_param) else null,
                .special_bindings = new_specials,
                .captures = new_captures,
                .body = try deepCopyIr(allocator, lam.body),
                .lambda_expr = lam.lambda_expr,
                .lambda_expr_idx = lam.lambda_expr_idx,
                .name = lam.name,
                .name_idx = lam.name_idx,
                .speed = lam.speed,
                .safety = lam.safety,
            } };
        },
        .cons => |op| .{ .cons = .{
            .left = try deepCopyIr(allocator, op.left),
            .right = try deepCopyIr(allocator, op.right),
        } },
        // All binary ops
        inline .fixnum_add,
        .fixnum_sub,
        .add,
        .sub,
        .fixnum_le,
        .fixnum_lt,
        .fixnum_gt,
        .fixnum_ge,
        .fixnum_eq,
        .le,
        .lt,
        .gt,
        .ge,
        .num_eq,
        .fixnum_mul,
        .mul,
        .eq,
        .vec_ref,
        .str_ref,
        .str_concat,
        .make_string,
        .position,
        .position_eq,
        .position_equal,
        .logand,
        .mod,
        .rem,
        .append,
        .assoc,
        => |op, tag| @unionInit(Ir, @tagName(tag), .{
            .left = try deepCopyIr(allocator, op.left),
            .right = try deepCopyIr(allocator, op.right),
        }),
        // All unary ops
        inline .assert_fixnum,
        .nilp,
        .not,
        .consp,
        .abs,
        .zerop,
        .oddp,
        .evenp,
        .car,
        .cdr,
        .unsafe_car,
        .unsafe_cdr,
        .length,
        .sqrt,
        .round,
        .intern,
        .vec_len,
        .str_len,
        .hash_count,
        .hash_capacity,
        .hash_clear,
        .hash_test,
        .hash_keys,
        .hash_alist,
        => |op, tag| @unionInit(Ir, @tagName(tag), .{
            .operand = try deepCopyIr(allocator, op.operand),
        }),
        .vec_new => |v| .{ .vec_new = .{
            .size = try deepCopyIr(allocator, v.size),
            .init = try deepCopyOptionalIr(allocator, v.init),
        } },
        .vec_set => |v| .{ .vec_set = .{
            .vec = try deepCopyIr(allocator, v.vec),
            .index = try deepCopyIr(allocator, v.index),
            .value = try deepCopyIr(allocator, v.value),
        } },
        .make_hash => |h| .{ .make_hash = h },
        .hash_get => |h| .{ .hash_get = .{
            .table = try deepCopyIr(allocator, h.table),
            .key = try deepCopyIr(allocator, h.key),
            .default = try deepCopyOptionalIr(allocator, h.default),
        } },
        .hash_set => |h| .{ .hash_set = .{
            .table = try deepCopyIr(allocator, h.table),
            .key = try deepCopyIr(allocator, h.key),
            .value = try deepCopyIr(allocator, h.value),
        } },
        .hash_rem => |h| .{ .hash_rem = .{
            .table = try deepCopyIr(allocator, h.table),
            .key = try deepCopyIr(allocator, h.key),
        } },
        .format => |f| .{ .format = .{
            .dest = try deepCopyIr(allocator, f.dest),
            .control = try deepCopyIr(allocator, f.control),
            .args = try deepCopyIrSlice(allocator, f.args),
        } },
        .str_set => |s| .{ .str_set = .{
            .str = try deepCopyIr(allocator, s.str),
            .index = try deepCopyIr(allocator, s.index),
            .value = try deepCopyIr(allocator, s.value),
        } },
        .substring => |s| .{ .substring = .{
            .str = try deepCopyIr(allocator, s.str),
            .start = try deepCopyIr(allocator, s.start),
            .end = try deepCopyIr(allocator, s.end),
        } },
        .arr_new => |a| .{ .arr_new = .{
            .dimensions = try deepCopyIrSlice(allocator, a.dimensions),
            .init = try deepCopyOptionalIr(allocator, a.init),
        } },
        .arr_new_dyn => |a| .{ .arr_new_dyn = .{
            .dimensions = try deepCopyIr(allocator, a.dimensions),
            .init = try deepCopyOptionalIr(allocator, a.init),
        } },
        .arr_ref => |a| .{ .arr_ref = .{
            .array = try deepCopyIr(allocator, a.array),
            .subscripts = try deepCopyIrSlice(allocator, a.subscripts),
        } },
        .arr_set => |a| .{ .arr_set = .{
            .array = try deepCopyIr(allocator, a.array),
            .subscripts = try deepCopyIrSlice(allocator, a.subscripts),
            .value = try deepCopyIr(allocator, a.value),
        } },
        else => return error.UnsupportedIrNode,
    };
    return node;
}

// ============================================================================
// Tests
// ============================================================================

test "ir literal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(arena.allocator());
    const node = try builder.lit(Value.makeFixnum(42));

    try std.testing.expectEqual(Ir.lit, std.meta.activeTag(node.*));
    try std.testing.expectEqual(@as(i64, 42), node.lit.toFixnum());
}

test "ir binary op" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(arena.allocator());
    const left = try builder.lit(Value.makeFixnum(1));
    const right = try builder.lit(Value.makeFixnum(2));
    const sum = try builder.add(left, right);

    try std.testing.expectEqual(Ir.add, std.meta.activeTag(sum.*));
    try std.testing.expect(sum.isPrimitive());
}

test "ir if expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(arena.allocator());
    const test_expr = try builder.lit(Value.t);
    const then_expr = try builder.lit(Value.makeFixnum(1));
    const else_expr = try builder.lit(Value.makeFixnum(0));
    const if_node = try builder.ifExpr(test_expr, then_expr, else_expr);

    try std.testing.expectEqual(Ir.@"if", std.meta.activeTag(if_node.*));
    try std.testing.expect(!if_node.isPrimitive());
}

test "ir lambda" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(arena.allocator());
    const body = try builder.lit(Value.nil);
    const params = [_][]const u8{ "x", "y" };
    const captures = [_]Ir.Capture{};
    const lam = try builder.lambda(&params, &.{}, &.{}, false, 0, null, &captures, body);

    try std.testing.expectEqual(Ir.lambda, std.meta.activeTag(lam.*));
    try std.testing.expectEqual(@as(usize, 2), lam.lambda.params.len);
    try std.testing.expectEqualStrings("x", lam.lambda.params[0]);
}

test "ir variable" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(arena.allocator());
    const v = try builder.variable("foo", 0, 3);

    try std.testing.expectEqual(Ir.@"var", std.meta.activeTag(v.*));
    try std.testing.expectEqualStrings("foo", v.@"var".name);
    try std.testing.expectEqual(@as(u16, 0), v.@"var".depth);
    try std.testing.expectEqual(@as(u16, 3), v.@"var".index);
}

test "ir tag name" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const builder = IrBuilder.init(arena.allocator());
    const node = try builder.lit(Value.nil);

    try std.testing.expectEqualStrings("lit", node.tagName());
}

test "deepCopyIr copies block-wrapped recursive shape" {
    var src_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer src_arena.deinit();

    const builder = IrBuilder.init(src_arena.allocator());

    const col = try builder.variable("col", 0, 0);
    const placed = try builder.variable("placed", 0, 1);
    const row = try builder.variable("row", 0, 2);

    const cond_nil = try builder.nilp(placed);
    const c = try builder.car(placed);
    const eq_col = try builder.eq(c, col);
    const diff = try builder.sub(c, col);
    const diag = try builder.abs(diff);
    const eq_diag = try builder.eq(diag, row);
    const not_diag = try builder.not(eq_diag);
    const rest = try builder.cdr(placed);
    const one = try builder.lit(Value.makeFixnum(1));
    const row_next = try builder.add(row, one);
    const self_fn = try builder.globalRef("NQUEENS-SAFE-P", 0);
    const recurse_args = [_]*const Ir{ col, rest, row_next };
    const recurse = try builder.tailcall(self_fn, &recurse_args);
    const nil_lit = try builder.lit(Value.nil);
    const t_lit = try builder.lit(Value.t);
    const inner_if = try builder.ifExpr(not_diag, recurse, nil_lit);
    const not_same_col = try builder.not(eq_col);
    const let_body = try builder.ifExpr(not_same_col, inner_if, nil_lit);
    const bindings = [_]Ir.Binding{
        .{ .name = "c", .value = c, .index = 3 },
    };
    const let_node = try builder.letExpr(&bindings, let_body);
    const body = try builder.ifExpr(cond_nil, t_lit, let_node);
    const wrapped = try builder.block(Value.nil, body);

    var dst_arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer dst_arena.deinit();

    const copied = try deepCopyIr(dst_arena.allocator(), wrapped);
    try std.testing.expectEqual(Ir.block, std.meta.activeTag(copied.*));
    try std.testing.expect(@intFromPtr(copied) != @intFromPtr(wrapped));

    const outer_if = copied.block.body;
    try std.testing.expectEqual(Ir.@"if", std.meta.activeTag(outer_if.*));

    const copied_let = outer_if.@"if".else_branch;
    try std.testing.expectEqual(Ir.let, std.meta.activeTag(copied_let.*));

    const cmp_if = copied_let.let.body;
    try std.testing.expectEqual(Ir.@"if", std.meta.activeTag(cmp_if.*));

    const recurse_if = cmp_if.@"if".then_branch;
    try std.testing.expectEqual(Ir.@"if", std.meta.activeTag(recurse_if.*));

    const recurse_call = recurse_if.@"if".then_branch;
    try std.testing.expectEqual(Ir.tailcall, std.meta.activeTag(recurse_call.*));
    try std.testing.expectEqual(Ir.global_ref, std.meta.activeTag(recurse_call.tailcall.func.*));
    try std.testing.expectEqualStrings("NQUEENS-SAFE-P", recurse_call.tailcall.func.global_ref.name);
}
