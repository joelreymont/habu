//! S-expression to IR compiler
//!
//! Compiles parsed Habu expressions (cons trees) to IR nodes.
//! Handles:
//! - Special forms: if, lambda, let, set!, quote, progn, while
//! - Primitive operations: +, -, *, /, cons, car, cdr, etc.
//! - Function calls
//! - Variable references with lexical scoping
//!
//! Type integration:
//! - Tracks types during compilation (TypeEnv)
//! - Occurrence typing: narrows types after predicates
//! - Inserts contracts at typed/untyped boundaries

const std = @import("std");
const ir = @import("ir.zig");
const Ir = ir.Ir;
const IrBuilder = ir.IrBuilder;
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;
const Heap = runtime.Heap;
const Closure = runtime.Closure;
const types = @import("../types/types.zig");
const Type = types.Type;
const TypeEnv = types.TypeEnv;
const OccurrenceCtx = types.OccurrenceCtx;
const TypeChecker = types.TypeChecker;
const vm_mod = @import("../interp/vm.zig");
const Vm = vm_mod.Vm;
const bytecode = @import("../bytecode/bytecode.zig");
const Emitter = bytecode.Emitter;
const Chunk = bytecode.Chunk;

pub const CompileError = error{
    InvalidSyntax,
    UnboundVariable,
    InvalidLambda,
    InvalidLet,
    InvalidIf,
    InvalidSet,
    OutOfMemory,
};

/// Pre-interned special form symbols for identity comparison
pub const Builtins = struct {
    // Control flow
    @"if": Value,
    cond: Value,
    @"and": Value,
    @"or": Value,

    // Binding forms
    let: Value,
    @"let*": Value,
    letrec: Value,
    flet: Value,
    labels: Value,
    lambda: Value,
    @"fn": Value,
    define: Value,
    defvar: Value,
    defun: Value,
    @"set!": Value,
    setq: Value,

    // Sequencing
    progn: Value,
    begin: Value,
    @"while": Value,

    // Quoting
    quote: Value,
    quasiquote: Value,
    unquote: Value,
    @"unquote-splicing": Value,
    function: Value,

    // Function application
    funcall: Value,
    apply: Value,

    // Macros
    defmacro: Value,
    macroexpand: Value,

    // Type assertions
    the: Value,

    // Non-local exits
    block: Value,
    @"return-from": Value,
    @"unwind-protect": Value,
    @"catch": Value,
    throw: Value,
    tagbody: Value,
    go: Value,
    values: Value,
    @"multiple-value-bind": Value,
    @"multiple-value-call": Value,
    @"multiple-value-list": Value,

    // Primitives - Arithmetic
    @"+": Value,
    @"-": Value,
    @"*": Value,
    @"/": Value,
    mod: Value,
    @"%": Value,

    // Primitives - Comparison
    eq: Value,
    equal: Value,
    eql: Value,
    @"<": Value,
    @">": Value,
    @"<=": Value,
    @">=": Value,
    @"=": Value,

    // Primitives - List operations
    cons: Value,
    car: Value,
    cdr: Value,
    first: Value, // CL alias for car
    rest: Value, // CL alias for cdr
    // Composed accessors (2-level)
    caar: Value,
    cadr: Value,
    cdar: Value,
    cddr: Value,
    // Composed accessors (3-level)
    caaar: Value,
    caadr: Value,
    cadar: Value,
    caddr: Value,
    cdaar: Value,
    cdadr: Value,
    cddar: Value,
    cdddr: Value,
    // Positional accessors
    second: Value,
    third: Value,
    fourth: Value,
    append: Value,
    length: Value,
    reverse: Value,
    nth: Value,
    nthcdr: Value,
    last: Value,
    member: Value,
    assoc: Value,
    list: Value,
    rplaca: Value,
    rplacd: Value,

    // Primitives - Type predicates
    consp: Value,
    @"cons?": Value,
    symbolp: Value,
    @"symbol?": Value,
    numberp: Value,
    @"number?": Value,
    stringp: Value,
    @"string?": Value,
    vectorp: Value,
    @"vector?": Value,
    closurep: Value,
    @"closure?": Value,
    keywordp: Value,
    @"keyword?": Value,
    null: Value,
    @"null?": Value,
    not: Value,
    characterp: Value,
    @"character?": Value,
    floatp: Value,
    @"float?": Value,
    listp: Value,
    @"list?": Value,
    atom: Value,

    // Primitives - Character operations
    @"char-code": Value,
    @"code-char": Value,
    @"char=": Value,
    @"char<": Value,
    @"char>": Value,
    @"read-char": Value,
    @"peek-char": Value,
    read: Value,
    @"read-from-string": Value,
    load: Value,
    @"unread-char": Value,
    eval: Value,
    gensym: Value,

    // Primitives - Symbol operations
    boundp: Value,
    fboundp: Value,
    @"symbol-value": Value,
    @"symbol-function": Value,
    typep: Value,
    @"type-of": Value,
    intern: Value,
    @"symbol-name": Value,
    @"error": Value,
    // Type specifier symbols for concatenate/coerce
    string: Value,
    character: Value,

    // Primitives - Numeric
    abs: Value,
    zerop: Value,
    plusp: Value,
    minusp: Value,
    evenp: Value,
    oddp: Value,

    // Primitives - Vector operations (CL names)
    aref: Value, // CL: array element access
    svref: Value, // CL: simple-vector element access
    @"%svset": Value, // internal: (setf (svref ...)) expands to this
    @"vector-length": Value,
    @"make-vector": Value,
    vector: Value,

    // Primitives - String operations (CL names)
    char: Value, // CL: character at index
    schar: Value, // CL: simple-string character access
    @"string-length": Value,
    @"string-concat": Value,
    @"string=": Value,
    substring: Value,
    subseq: Value,

    // Primitives - I/O and misc
    print: Value,
    princ: Value,
    terpri: Value,
    @"write-char": Value,
    random: Value,
    format: Value,

    // Primitives - Character functions
    @"char-upcase": Value,
    @"char-downcase": Value,
    @"digit-char-p": Value,
    @"alpha-char-p": Value,

    // Primitives - String/number conversion
    @"parse-integer": Value,
    @"write-to-string": Value,

    // Primitives - Bitwise operations
    logand: Value,
    logior: Value,
    logxor: Value,
    lognot: Value,
    ash: Value,

    // Primitives - File I/O
    @"read-file": Value,
    @"write-file": Value,

    // Primitives - String construction
    @"make-string": Value,
    @"string-to-list": Value,
    @"list-to-string": Value,
    @"string-upcase": Value,
    @"string-downcase": Value,
    concatenate: Value,
    coerce: Value,

    // Primitives - Hash tables
    @"make-hash-table": Value,
    gethash: Value,
    puthash: Value,
    remhash: Value,
    @"hash-table-count": Value,
    @"hash-table-p": Value,

    /// Initialize all builtin symbols from heap
    pub fn init(heap: *Heap) ?Builtins {
        return .{
            .@"if" = heap.intern("if") orelse return null,
            .cond = heap.intern("cond") orelse return null,
            .@"and" = heap.intern("and") orelse return null,
            .@"or" = heap.intern("or") orelse return null,
            .let = heap.intern("let") orelse return null,
            .@"let*" = heap.intern("let*") orelse return null,
            .letrec = heap.intern("letrec") orelse return null,
            .lambda = heap.intern("lambda") orelse return null,
            .@"fn" = heap.intern("fn") orelse return null,
            .define = heap.intern("define") orelse return null,
            .defvar = heap.intern("defvar") orelse return null,
            .defun = heap.intern("defun") orelse return null,
            .@"set!" = heap.intern("set!") orelse return null,
            .setq = heap.intern("setq") orelse return null,
            .progn = heap.intern("progn") orelse return null,
            .begin = heap.intern("begin") orelse return null,
            .@"while" = heap.intern("while") orelse return null,
            .quote = heap.intern("quote") orelse return null,
            .quasiquote = heap.intern("quasiquote") orelse return null,
            .unquote = heap.intern("unquote") orelse return null,
            .@"unquote-splicing" = heap.intern("unquote-splicing") orelse return null,
            .function = heap.intern("function") orelse return null,
            .funcall = heap.intern("funcall") orelse return null,
            .apply = heap.intern("apply") orelse return null,
            .defmacro = heap.intern("defmacro") orelse return null,
            .macroexpand = heap.intern("macroexpand") orelse return null,
            .the = heap.intern("the") orelse return null,
            .flet = heap.intern("flet") orelse return null,
            .labels = heap.intern("labels") orelse return null,
            .block = heap.intern("block") orelse return null,
            .@"return-from" = heap.intern("return-from") orelse return null,
            .@"unwind-protect" = heap.intern("unwind-protect") orelse return null,
            .@"catch" = heap.intern("catch") orelse return null,
            .throw = heap.intern("throw") orelse return null,
            .tagbody = heap.intern("tagbody") orelse return null,
            .go = heap.intern("go") orelse return null,
            .values = heap.intern("values") orelse return null,
            .@"multiple-value-bind" = heap.intern("multiple-value-bind") orelse return null,
            .@"multiple-value-call" = heap.intern("multiple-value-call") orelse return null,
            .@"multiple-value-list" = heap.intern("multiple-value-list") orelse return null,
            // Primitives - Arithmetic
            .@"+" = heap.intern("+") orelse return null,
            .@"-" = heap.intern("-") orelse return null,
            .@"*" = heap.intern("*") orelse return null,
            .@"/" = heap.intern("/") orelse return null,
            .mod = heap.intern("mod") orelse return null,
            .@"%" = heap.intern("%") orelse return null,
            // Primitives - Comparison
            .eq = heap.intern("eq") orelse return null,
            .equal = heap.intern("equal") orelse return null,
            .eql = heap.intern("eql") orelse return null,
            .@"<" = heap.intern("<") orelse return null,
            .@">" = heap.intern(">") orelse return null,
            .@"<=" = heap.intern("<=") orelse return null,
            .@">=" = heap.intern(">=") orelse return null,
            .@"=" = heap.intern("=") orelse return null,
            // Primitives - List operations
            .cons = heap.intern("cons") orelse return null,
            .car = heap.intern("car") orelse return null,
            .cdr = heap.intern("cdr") orelse return null,
            .first = heap.intern("first") orelse return null,
            .rest = heap.intern("rest") orelse return null,
            // Composed accessors (2-level)
            .caar = heap.intern("caar") orelse return null,
            .cadr = heap.intern("cadr") orelse return null,
            .cdar = heap.intern("cdar") orelse return null,
            .cddr = heap.intern("cddr") orelse return null,
            // Composed accessors (3-level)
            .caaar = heap.intern("caaar") orelse return null,
            .caadr = heap.intern("caadr") orelse return null,
            .cadar = heap.intern("cadar") orelse return null,
            .caddr = heap.intern("caddr") orelse return null,
            .cdaar = heap.intern("cdaar") orelse return null,
            .cdadr = heap.intern("cdadr") orelse return null,
            .cddar = heap.intern("cddar") orelse return null,
            .cdddr = heap.intern("cdddr") orelse return null,
            // Positional accessors
            .second = heap.intern("second") orelse return null,
            .third = heap.intern("third") orelse return null,
            .fourth = heap.intern("fourth") orelse return null,
            .append = heap.intern("append") orelse return null,
            .length = heap.intern("length") orelse return null,
            .reverse = heap.intern("reverse") orelse return null,
            .nth = heap.intern("nth") orelse return null,
            .nthcdr = heap.intern("nthcdr") orelse return null,
            .last = heap.intern("last") orelse return null,
            .member = heap.intern("member") orelse return null,
            .assoc = heap.intern("assoc") orelse return null,
            .list = heap.intern("list") orelse return null,
            .rplaca = heap.intern("rplaca") orelse return null,
            .rplacd = heap.intern("rplacd") orelse return null,
            // Primitives - Type predicates
            .consp = heap.intern("consp") orelse return null,
            .@"cons?" = heap.intern("cons?") orelse return null,
            .symbolp = heap.intern("symbolp") orelse return null,
            .@"symbol?" = heap.intern("symbol?") orelse return null,
            .numberp = heap.intern("numberp") orelse return null,
            .@"number?" = heap.intern("number?") orelse return null,
            .stringp = heap.intern("stringp") orelse return null,
            .@"string?" = heap.intern("string?") orelse return null,
            .vectorp = heap.intern("vectorp") orelse return null,
            .@"vector?" = heap.intern("vector?") orelse return null,
            .closurep = heap.intern("closurep") orelse return null,
            .@"closure?" = heap.intern("closure?") orelse return null,
            .keywordp = heap.intern("keywordp") orelse return null,
            .@"keyword?" = heap.intern("keyword?") orelse return null,
            .null = heap.intern("null") orelse return null,
            .@"null?" = heap.intern("null?") orelse return null,
            .not = heap.intern("not") orelse return null,
            .characterp = heap.intern("characterp") orelse return null,
            .@"character?" = heap.intern("character?") orelse return null,
            .floatp = heap.intern("floatp") orelse return null,
            .@"float?" = heap.intern("float?") orelse return null,
            .listp = heap.intern("listp") orelse return null,
            .@"list?" = heap.intern("list?") orelse return null,
            .atom = heap.intern("atom") orelse return null,
            // Primitives - Character operations
            .@"char-code" = heap.intern("char-code") orelse return null,
            .@"code-char" = heap.intern("code-char") orelse return null,
            .@"char=" = heap.intern("char=") orelse return null,
            .@"char<" = heap.intern("char<") orelse return null,
            .@"char>" = heap.intern("char>") orelse return null,
            .@"read-char" = heap.intern("read-char") orelse return null,
            .@"peek-char" = heap.intern("peek-char") orelse return null,
            .read = heap.intern("read") orelse return null,
            .@"read-from-string" = heap.intern("read-from-string") orelse return null,
            .load = heap.intern("load") orelse return null,
            .@"unread-char" = heap.intern("unread-char") orelse return null,
            .eval = heap.intern("eval") orelse return null,
            .gensym = heap.intern("gensym") orelse return null,
            // Primitives - Symbol operations
            .boundp = heap.intern("boundp") orelse return null,
            .fboundp = heap.intern("fboundp") orelse return null,
            .@"symbol-value" = heap.intern("symbol-value") orelse return null,
            .@"symbol-function" = heap.intern("symbol-function") orelse return null,
            .typep = heap.intern("typep") orelse return null,
            .@"type-of" = heap.intern("type-of") orelse return null,
            .intern = heap.intern("intern") orelse return null,
            .@"symbol-name" = heap.intern("symbol-name") orelse return null,
            .@"error" = heap.intern("error") orelse return null,
            // Type specifier symbols for concatenate/coerce
            .string = heap.intern("string") orelse return null,
            .character = heap.intern("character") orelse return null,
            // Primitives - Numeric
            .abs = heap.intern("abs") orelse return null,
            .zerop = heap.intern("zerop") orelse return null,
            .plusp = heap.intern("plusp") orelse return null,
            .minusp = heap.intern("minusp") orelse return null,
            .evenp = heap.intern("evenp") orelse return null,
            .oddp = heap.intern("oddp") orelse return null,
            // Primitives - Vector operations (CL names)
            .aref = heap.intern("aref") orelse return null,
            .svref = heap.intern("svref") orelse return null,
            .@"%svset" = heap.intern("%svset") orelse return null,
            .@"vector-length" = heap.intern("vector-length") orelse return null,
            .@"make-vector" = heap.intern("make-vector") orelse return null,
            .vector = heap.intern("vector") orelse return null,
            // Primitives - String operations (CL names)
            .char = heap.intern("char") orelse return null,
            .schar = heap.intern("schar") orelse return null,
            .@"string-length" = heap.intern("string-length") orelse return null,
            .@"string-concat" = heap.intern("string-concat") orelse return null,
            .@"string=" = heap.intern("string=") orelse return null,
            .substring = heap.intern("substring") orelse return null,
            .subseq = heap.intern("subseq") orelse return null,
            // Primitives - I/O and misc
            .print = heap.intern("print") orelse return null,
            .princ = heap.intern("princ") orelse return null,
            .terpri = heap.intern("terpri") orelse return null,
            .@"write-char" = heap.intern("write-char") orelse return null,
            .random = heap.intern("random") orelse return null,
            .format = heap.intern("format") orelse return null,
            // Primitives - Character functions
            .@"char-upcase" = heap.intern("char-upcase") orelse return null,
            .@"char-downcase" = heap.intern("char-downcase") orelse return null,
            .@"digit-char-p" = heap.intern("digit-char-p") orelse return null,
            .@"alpha-char-p" = heap.intern("alpha-char-p") orelse return null,
            // Primitives - String/number conversion
            .@"parse-integer" = heap.intern("parse-integer") orelse return null,
            .@"write-to-string" = heap.intern("write-to-string") orelse return null,
            // Primitives - Bitwise operations
            .logand = heap.intern("logand") orelse return null,
            .logior = heap.intern("logior") orelse return null,
            .logxor = heap.intern("logxor") orelse return null,
            .lognot = heap.intern("lognot") orelse return null,
            .ash = heap.intern("ash") orelse return null,
            // Primitives - File I/O
            .@"read-file" = heap.intern("read-file") orelse return null,
            .@"write-file" = heap.intern("write-file") orelse return null,
            // Primitives - String construction
            .@"make-string" = heap.intern("make-string") orelse return null,
            .@"string-to-list" = heap.intern("string-to-list") orelse return null,
            .@"list-to-string" = heap.intern("list-to-string") orelse return null,
            .@"string-upcase" = heap.intern("string-upcase") orelse return null,
            .@"string-downcase" = heap.intern("string-downcase") orelse return null,
            .concatenate = heap.intern("concatenate") orelse return null,
            .coerce = heap.intern("coerce") orelse return null,
            // Primitives - Hash tables
            .@"make-hash-table" = heap.intern("make-hash-table") orelse return null,
            .gethash = heap.intern("gethash") orelse return null,
            .puthash = heap.intern("puthash") orelse return null,
            .remhash = heap.intern("remhash") orelse return null,
            .@"hash-table-count" = heap.intern("hash-table-count") orelse return null,
            .@"hash-table-p" = heap.intern("hash-table-p") orelse return null,
        };
    }
};

/// Lexical environment for variable resolution
pub const Env = struct {
    /// Variable bindings at this level
    bindings: std.StringHashMap(u16),
    /// Parent environment (for closures)
    parent: ?*const Env,
    /// Depth from root (0 = top level)
    depth: u16,
    /// Whether this is a new frame (lambda) or same frame (let)
    new_frame: bool,
    /// Base index for bindings (for let, continues from parent)
    base_index: u16,
    /// Allocator for bindings
    allocator: std.mem.Allocator,

    /// Create a new frame environment (for lambda)
    pub fn init(allocator: std.mem.Allocator, parent: ?*const Env) Env {
        return .{
            .bindings = std.StringHashMap(u16).init(allocator),
            .parent = parent,
            .depth = if (parent) |p| p.depth + 1 else 0,
            .new_frame = true,
            .base_index = 0,
            .allocator = allocator,
        };
    }

    /// Create a same-frame environment (for let)
    pub fn initLet(allocator: std.mem.Allocator, parent: *const Env) Env {
        return .{
            .bindings = std.StringHashMap(u16).init(allocator),
            .parent = parent,
            .depth = parent.depth, // Same depth - same frame
            .new_frame = false,
            .base_index = parent.localCount(), // Continue from parent's count
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Env) void {
        self.bindings.deinit();
    }

    /// Get total local count in this frame
    pub fn localCount(self: *const Env) u16 {
        const own_count: u16 = @intCast(self.bindings.count());
        if (!self.new_frame) {
            // For let envs, add parent's count
            if (self.parent) |p| {
                return p.localCount() + own_count;
            }
        }
        return self.base_index + own_count;
    }

    /// Add a binding, returns the absolute index
    pub fn bind(self: *Env, name: []const u8) !u16 {
        const local_index: u16 = @intCast(self.bindings.count());
        const abs_index = self.base_index + local_index;
        try self.bindings.put(name, abs_index);
        return abs_index;
    }

    /// Look up a variable, returns (depth, index) or null
    pub fn lookup(self: *const Env, name: []const u8) ?struct { depth: u16, index: u16 } {
        if (self.bindings.get(name)) |index| {
            return .{ .depth = 0, .index = index };
        }
        if (self.parent) |parent| {
            if (parent.lookup(name)) |result| {
                if (self.new_frame) {
                    // Cross frame boundary - increment depth
                    return .{ .depth = result.depth + 1, .index = result.index };
                } else {
                    // Same frame (let) - keep same depth
                    return result;
                }
            }
        }
        return null;
    }
};

/// Typed compilation result
pub const TypedIr = struct {
    ir: *Ir,
    ty: *const Type,
};

/// Set of variable names that need boxing (mutable + captured)
pub const BoxingSet = struct {
    names: std.StringHashMap(void),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) BoxingSet {
        return .{
            .names = std.StringHashMap(void).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *BoxingSet) void {
        self.names.deinit();
    }

    pub fn add(self: *BoxingSet, name: []const u8) !void {
        try self.names.put(name, {});
    }

    pub fn contains(self: *const BoxingSet, name: []const u8) bool {
        return self.names.contains(name);
    }
};

/// Capture analysis result
pub const CaptureSet = struct {
    /// Free variables that need to be captured
    captures: std.ArrayList(Ir.Capture),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CaptureSet {
        return .{
            .captures = std.ArrayList(Ir.Capture){},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CaptureSet) void {
        self.captures.deinit(self.allocator);
    }

    /// Add a capture if not already present
    pub fn addCapture(self: *CaptureSet, name: []const u8, depth: u16, index: u16) !void {
        // Check if already captured
        for (self.captures.items) |cap| {
            if (std.mem.eql(u8, cap.name, name)) return;
        }
        try self.captures.append(self.allocator, .{
            .name = name,
            .depth = depth,
            .index = index,
        });
    }
};

/// Global environment for top-level definitions
pub const GlobalEnv = struct {
    /// Map from name to global index
    bindings: std.StringHashMap(u16),
    /// Next available index
    next_index: u16,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) GlobalEnv {
        return .{
            .bindings = std.StringHashMap(u16).init(allocator),
            .next_index = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GlobalEnv) void {
        // Free all allocated name strings
        var iter = self.bindings.keyIterator();
        while (iter.next()) |key| {
            self.allocator.free(key.*);
        }
        self.bindings.deinit();
    }

    /// Define a global, returns its index
    pub fn define(self: *GlobalEnv, name: []const u8) !u16 {
        if (self.bindings.get(name)) |idx| {
            return idx; // Already defined, return existing index
        }
        const idx = self.next_index;
        // Dupe name - callers may pass transient slices (e.g. from symbol storage)
        const name_copy = try self.allocator.dupe(u8, name);
        try self.bindings.put(name_copy, idx);
        self.next_index += 1;
        return idx;
    }

    /// Lookup a global, returns index or null
    pub fn lookup(self: *const GlobalEnv, name: []const u8) ?u16 {
        return self.bindings.get(name);
    }
};

/// Compiler state
pub const Compiler = struct {
    builder: IrBuilder,
    allocator: std.mem.Allocator,
    /// Type checker for type errors and subtype checking
    type_checker: TypeChecker,
    /// Whether to enable type checking (gradual typing)
    type_checking_enabled: bool,
    /// Global environment for top-level definitions
    globals: GlobalEnv,
    /// Pre-interned builtin symbols for identity comparison
    builtins: ?Builtins,
    /// Current occurrence context for type narrowing (set during if compilation)
    occ: ?*const OccurrenceCtx,
    /// Variables that need boxing (mutable + captured) - set during let compilation
    boxed_vars: ?*const BoxingSet,
    /// Defined ADT types for match exhaustiveness checking
    defined_types: std.StringHashMap([]const Variant),
    /// Macro table: maps macro name to closure (expander function)
    /// When a form (macro-name args...) is compiled, the macro is expanded first
    macro_table: std.StringHashMap(Value),
    /// Optional VM for compile-time macro expansion
    vm: ?*Vm,
    /// Heap for creating runtime values during macro expansion
    heap: ?*Heap,

    /// ADT variant definition
    pub const Variant = struct {
        name: []const u8,
        fields: []const []const u8,
    };

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .type_checker = TypeChecker.init(allocator),
            .type_checking_enabled = false, // Off by default for gradual typing
            .globals = GlobalEnv.init(allocator),
            .builtins = null, // Lazily initialized when heap is available
            .occ = null,
            .boxed_vars = null,
            .defined_types = std.StringHashMap([]const Variant).init(allocator),
            .macro_table = std.StringHashMap(Value).init(allocator),
            .vm = null,
            .heap = null,
        };
    }

    /// Initialize with heap for symbol interning
    pub fn initWithHeap(allocator: std.mem.Allocator, heap: *Heap) Compiler {
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .type_checker = TypeChecker.init(allocator),
            .type_checking_enabled = false,
            .globals = GlobalEnv.init(allocator),
            .builtins = Builtins.init(heap),
            .occ = null,
            .boxed_vars = null,
            .defined_types = std.StringHashMap([]const Variant).init(allocator),
            .macro_table = std.StringHashMap(Value).init(allocator),
            .vm = null,
            .heap = heap,
        };
    }

    /// Set VM for compile-time macro expansion
    pub fn setVm(self: *Compiler, vm: *Vm) void {
        self.vm = vm;
    }

    pub fn deinit(self: *Compiler) void {
        self.type_checker.deinit();
        self.globals.deinit();
        self.macro_table.deinit();
    }

    /// Enable type checking mode
    pub fn enableTypeChecking(self: *Compiler) void {
        self.type_checking_enabled = true;
    }

    /// Check if type checker has errors
    pub fn hasTypeErrors(self: *const Compiler) bool {
        return self.type_checker.hasErrors();
    }

    /// Compile with type inference
    /// Returns both IR and inferred type
    pub fn compileTyped(
        self: *Compiler,
        expr: Value,
        env: *const Env,
        type_env: *const TypeEnv,
        occ: *const OccurrenceCtx,
    ) CompileError!TypedIr {
        const ir_node = try self.compile(expr, env);

        // Infer type based on IR node
        const ty = self.inferType(ir_node, type_env, occ);

        return .{ .ir = ir_node, .ty = ty };
    }

    /// Infer type of an IR node
    fn inferType(self: *Compiler, node: *const Ir, type_env: *const TypeEnv, occ: *const OccurrenceCtx) *const Type {
        _ = self;
        return switch (node.*) {
            .lit => |val| return switch (val.typeKind()) {
                .nil => &types.t_nil,
                .fixnum => &types.t_fixnum,
                .float => &types.t_float,
                .char => &types.t_char,
                .string => &types.t_string,
                .symbol => &types.t_symbol,
                .cons => &types.t_cons,
                .vector => &types.t_vector,
                .keyword => &types.t_keyword,
                .closure => &types.t_closure,
                .hashtable => &types.t_any,
            },
            .@"var" => |v| {
                // Check occurrence typing first (narrowed types)
                if (occ.getNarrowed(v.name)) |narrowed| {
                    return narrowed;
                }
                // Then check type environment
                if (type_env.lookup(v.name)) |ty| {
                    return ty;
                }
                return &types.t_any;
            },
            .add, .sub, .mul, .div, .mod => &types.t_fixnum,
            .eq, .lt, .gt, .le, .ge, .num_eq => &types.t_any, // Returns t or nil
            .cons => &types.t_cons,
            .car, .cdr => &types.t_any, // Could be anything
            .consp, .symbolp, .numberp, .nilp, .not, .stringp, .vectorp => &types.t_any,
            .quote_sym => &types.t_symbol,
            .@"if" => &types.t_any, // Would need union of branches
            .lambda => &types.t_closure,
            .let => &types.t_any, // Type of body
            .progn => &types.t_any, // Type of last expr
            .call => &types.t_any, // Need function return type
            else => &types.t_any,
        };
    }

    /// Predicate narrowing information for occurrence typing
    const PredicateInfo = struct {
        /// Variable name being tested
        var_name: []const u8,
        /// Type to narrow to in the then-branch
        narrowed_type: *const Type,
        /// Type to narrow to in the else-branch (complement)
        else_type: ?*const Type,
    };

    /// Predicate to narrowed type mapping
    /// then_ty: type to narrow to in then-branch
    /// else_ty: type to narrow to in else-branch (null if unknown)
    const predicate_types = [_]struct {
        tag: std.meta.Tag(Ir),
        then_ty: *const Type,
        else_ty: ?*const Type,
    }{
        .{ .tag = .consp, .then_ty = &types.t_cons, .else_ty = null },
        .{ .tag = .symbolp, .then_ty = &types.t_symbol, .else_ty = null },
        .{ .tag = .numberp, .then_ty = &types.t_fixnum, .else_ty = null },
        .{ .tag = .stringp, .then_ty = &types.t_string, .else_ty = null },
        .{ .tag = .vectorp, .then_ty = &types.t_vector, .else_ty = null },
        .{ .tag = .closurep, .then_ty = &types.t_closure, .else_ty = null },
        .{ .tag = .keywordp, .then_ty = &types.t_keyword, .else_ty = null },
        .{ .tag = .characterp, .then_ty = &types.t_char, .else_ty = null },
        .{ .tag = .floatp, .then_ty = &types.t_float, .else_ty = null },
        // nilp: if nil then x is nil, else x is non-nil
        .{ .tag = .nilp, .then_ty = &types.t_nil, .else_ty = &types.t_non_nil },
        // not: if (not x) is true then x is nil, else x is non-nil
        .{ .tag = .not, .then_ty = &types.t_nil, .else_ty = &types.t_non_nil },
    };

    /// Get operand from a unary predicate IR node
    fn getPredicateOperand(node: *const Ir) ?*const Ir {
        return switch (node.*) {
            .consp => |p| p.operand,
            .symbolp => |p| p.operand,
            .numberp => |p| p.operand,
            .stringp => |p| p.operand,
            .vectorp => |p| p.operand,
            .closurep => |p| p.operand,
            .keywordp => |p| p.operand,
            .characterp => |p| p.operand,
            .floatp => |p| p.operand,
            .nilp => |p| p.operand,
            .not => |p| p.operand,
            else => null,
        };
    }

    /// Extract predicate narrowing info from an IR node
    /// For (consp x), returns info to narrow x to cons in then-branch
    /// For (nilp x), returns info to narrow x to nil in then, non-nil in else
    fn extractPredicateInfo(node: *const Ir) ?PredicateInfo {
        const tag = std.meta.activeTag(node.*);

        for (predicate_types) |entry| {
            if (tag == entry.tag) {
                if (getPredicateOperand(node)) |operand| {
                    if (operand.* == .@"var") {
                        return .{
                            .var_name = operand.@"var".name,
                            .narrowed_type = entry.then_ty,
                            .else_type = entry.else_ty,
                        };
                    }
                }
                break;
            }
        }
        return null;
    }

    /// Compile if with occurrence typing support
    pub fn compileIfTyped(
        self: *Compiler,
        args: Value,
        env: *const Env,
        type_env: *const TypeEnv,
        occ: *OccurrenceCtx,
    ) CompileError!TypedIr {
        // (if test then else?)
        if (!args.isCons()) return error.InvalidIf;

        const cons1 = args.toPtr(Cons);
        const test_expr = cons1.car;
        const rest1 = cons1.cdr;

        if (!rest1.isCons()) return error.InvalidIf;
        const cons2 = rest1.toPtr(Cons);
        const then_expr = cons2.car;
        const rest2 = cons2.cdr;

        const else_expr = if (rest2.isCons())
            rest2.toPtr(Cons).car
        else
            Value.nil;

        // Compile test expression
        const test_ir = try self.compile(test_expr, env);

        // Check if test is a type predicate for occurrence typing
        const pred_info = extractPredicateInfo(test_ir);

        // Compile then-branch with narrowed type context
        var then_occ = OccurrenceCtx.init(self.allocator);
        defer then_occ.deinit();

        if (pred_info) |info| {
            // In then-branch, the variable has the narrowed type
            then_occ.narrowed.put(info.var_name, info.narrowed_type) catch
                return error.OutOfMemory;
        }

        // Copy existing narrowings
        var occ_iter = occ.narrowed.iterator();
        while (occ_iter.next()) |entry| {
            then_occ.narrowed.put(entry.key_ptr.*, entry.value_ptr.*) catch
                return error.OutOfMemory;
        }

        const then_ir = try self.compileTyped(then_expr, env, type_env, &then_occ);

        // Compile else-branch (could narrow to complement type)
        const else_ir = try self.compileTyped(else_expr, env, type_env, occ);

        const if_ir = self.builder.ifExpr(test_ir, then_ir.ir, else_ir.ir) catch
            return error.OutOfMemory;

        // Result type is union of branch types (simplified to any for now)
        return .{ .ir = if_ir, .ty = &types.t_any };
    }

    /// Compile a single expression
    pub fn compile(self: *Compiler, expr: Value, env: *const Env) CompileError!*Ir {
        return self.compileWithTail(expr, env, false);
    }

    /// Compile with tail position tracking
    fn compileWithTail(self: *Compiler, expr: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // Nil
        if (expr.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        // Fixnum
        if (expr.isFixnum()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // Float
        if (expr.isFloat()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // String
        if (expr.isString()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // Character
        if (expr.isCharacter()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // Symbol (variable reference)
        if (expr.isSymbol()) {
            const sym = expr.toPtr(Symbol);
            const name = sym.getName();

            if (env.lookup(name)) |binding| {
                const var_ir = self.builder.variable(name, binding.depth, binding.index) catch
                    return error.OutOfMemory;

                // If this variable is boxed, wrap with box-ref
                if (self.boxed_vars) |bv| {
                    if (bv.contains(name)) {
                        const box_ref = self.allocator.create(Ir) catch return error.OutOfMemory;
                        box_ref.* = .{ .box_ref = .{ .operand = var_ir } };
                        return box_ref;
                    }
                }
                return var_ir;
            }
            // Check globals - if not found, assume it will be defined later (late binding)
            const idx = self.globals.lookup(name) orelse
                (self.globals.define(name) catch return error.OutOfMemory);
            return self.builder.globalRef(name, idx) catch
                return error.OutOfMemory;
        }

        // List (special form or function call)
        if (expr.isCons()) {
            return self.compileListWithTail(expr, env, in_tail);
        }

        // Keyword - just return as literal
        if (expr.isKeyword()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        return error.InvalidSyntax;
    }

    fn compileList(self: *Compiler, expr: Value, env: *const Env) CompileError!*Ir {
        return self.compileListWithTail(expr, env, false);
    }

    /// Special form types for dispatch
    const SpecialForm = enum {
        // Tail-position aware forms
        @"if",
        let,
        letrec,
        @"let*",
        cond,
        progn,
        begin,
        flet,
        labels,
        block,
        // Non-tail forms
        lambda,
        @"and",
        @"or",
        funcall,
        apply,
        @"set!",
        setq,
        quote,
        function,
        quasiquote,
        @"while",
        define,
        defvar,
        defun,
        the,
        @"return-from",
        @"unwind-protect",
        @"catch",
        throw,
        tagbody,
        go,
        values,
        @"multiple-value-bind",
        @"multiple-value-call",
        @"multiple-value-list",
        // ADT support
        deftype,
        match,
        // Macro support
        defmacro,
        // Compile-time evaluation
        @"eval-when",
    };

    /// Comptime dispatch table for special forms
    const special_forms = std.StaticStringMap(SpecialForm).initComptime(.{
        .{ "if", .@"if" },
        .{ "let", .let },
        .{ "letrec", .letrec },
        .{ "let*", .@"let*" },
        .{ "cond", .cond },
        .{ "progn", .progn },
        .{ "begin", .begin },
        .{ "flet", .flet },
        .{ "labels", .labels },
        .{ "lambda", .lambda },
        .{ "and", .@"and" },
        .{ "or", .@"or" },
        .{ "funcall", .funcall },
        .{ "apply", .apply },
        .{ "set!", .@"set!" },
        .{ "setq", .setq },
        .{ "quote", .quote },
        .{ "function", .function },
        .{ "quasiquote", .quasiquote },
        .{ "while", .@"while" },
        .{ "define", .define },
        .{ "defvar", .defvar },
        .{ "defun", .defun },
        .{ "the", .the },
        .{ "block", .block },
        .{ "return-from", .@"return-from" },
        .{ "unwind-protect", .@"unwind-protect" },
        .{ "catch", .@"catch" },
        .{ "throw", .throw },
        .{ "tagbody", .tagbody },
        .{ "go", .go },
        .{ "values", .values },
        .{ "multiple-value-bind", .@"multiple-value-bind" },
        .{ "multiple-value-call", .@"multiple-value-call" },
        .{ "multiple-value-list", .@"multiple-value-list" },
        // ADT support
        .{ "deftype", .deftype },
        .{ "match", .match },
        // Macro support
        .{ "defmacro", .defmacro },
        // Compile-time evaluation
        .{ "eval-when", .@"eval-when" },
    });

    fn compileListWithTail(self: *Compiler, expr: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check for special forms via StaticStringMap (O(1) perfect hash lookup)
        if (head.isSymbol()) {
            const sym = head.toPtr(Symbol);
            const name = sym.getName();

            if (special_forms.get(name)) |form| {
                return switch (form) {
                    // Tail-position aware forms
                    .@"if" => self.compileIfWithTail(tail, env, in_tail),
                    .let => self.compileLetWithTail(tail, env, in_tail),
                    .letrec => self.compileLetrecWithTail(tail, env, in_tail),
                    .@"let*" => self.compileLetStarWithTail(tail, env, in_tail),
                    .cond => self.compileCondWithTail(tail, env, in_tail),
                    .progn, .begin => self.compilePrognWithTail(tail, env, in_tail),
                    .flet => self.compileFletWithTail(tail, env, in_tail),
                    .labels => self.compileLabelsWithTail(tail, env, in_tail),
                    .block => self.compileBlockWithTail(tail, env, in_tail),
                    .@"unwind-protect" => self.compileUnwindProtectWithTail(tail, env, in_tail),
                    .@"catch" => self.compileCatchWithTail(tail, env, in_tail),
                    // Non-tail forms
                    .lambda => self.compileLambda(tail, env),
                    .@"and" => self.compileAnd(tail, env),
                    .@"or" => self.compileOr(tail, env),
                    .funcall => self.compileFuncall(tail, env),
                    .apply => self.compileApply(tail, env),
                    .@"set!", .setq => self.compileSet(tail, env),
                    .quote => self.compileQuote(tail),
                    .function => self.compileFunction(tail, env),
                    .quasiquote => self.compileQuasiquote(tail, env),
                    .@"while" => self.compileWhile(tail, env),
                    .define, .defvar => self.compileDefine(tail, env),
                    .defun => self.compileDefun(tail, env),
                    .the => self.compileThe(tail, env),
                    .@"return-from" => self.compileReturnFrom(tail, env),
                    .throw => self.compileThrow(tail, env),
                    .tagbody => self.compileTagbody(tail, env),
                    .go => self.compileGo(tail),
                    .values => self.compileValues(tail, env),
                    .@"multiple-value-bind" => self.compileMvBind(tail, env),
                    .@"multiple-value-call" => self.compileMvCall(tail, env),
                    .@"multiple-value-list" => self.compileMvList(tail, env),
                    // ADT support
                    .deftype => self.compileDeftype(tail, env),
                    .match => self.compileMatch(tail, env),
                    // Macro support
                    .defmacro => self.compileDefmacro(tail, env),
                    // Compile-time evaluation
                    .@"eval-when" => self.compileEvalWhen(tail, env),
                };
            }

            // Check for macros - expand at compile time if VM is available
            if (self.macro_table.get(name)) |macro_def| {
                if (self.vm) |vm| {
                    const expanded = self.expandMacro(macro_def, tail, vm) catch return error.InvalidSyntax;
                    return self.compileWithTail(expanded, env, in_tail);
                }
                // No VM - can't expand macro, treat as function call
            }

            // Check for primitives
            if (self.compilePrimitive(head, tail, env)) |prim| {
                return prim;
            } else |_| {
                // Fall through to function call
            }
        }

        // Function call - pass tail position
        return self.compileCallWithTail(head, tail, env, in_tail);
    }

    /// Expand a macro by calling its expander function with the arguments
    fn expandMacro(self: *Compiler, macro_def: Value, args: Value, vm: *Vm) !Value {
        const heap = self.heap orelse return error.InvalidSyntax;

        // macro_def is ((params...) body...)
        // Create a lambda: (lambda (params...) body...)
        if (!macro_def.isCons()) return error.InvalidSyntax;
        const def_cons = macro_def.toPtr(Cons);
        const params = def_cons.car;
        const body_list = def_cons.cdr;

        // Get first body form (for simple single-body macros)
        if (!body_list.isCons()) return error.InvalidSyntax;
        const body_cons = body_list.toPtr(Cons);
        const body = body_cons.car;

        // Build (lambda (params...) body) and compile it
        const lambda_sym = heap.intern("lambda") orelse return error.OutOfMemory;
        // Build: (lambda params body)
        const body_cell = heap.allocCons(body, Value.nil) orelse return error.OutOfMemory;
        const params_body = heap.allocCons(params, body_cell) orelse return error.OutOfMemory;
        const lambda_list = heap.allocCons(lambda_sym, params_body) orelse return error.OutOfMemory;

        // Compile the lambda to get a closure
        var macro_compiler = Compiler.initWithHeap(self.allocator, heap);
        defer macro_compiler.deinit();
        macro_compiler.vm = vm;

        var empty_env = Env.init(self.allocator, null);
        const lambda_ir = macro_compiler.compile(lambda_list, &empty_env) catch
            return error.InvalidSyntax;

        // Emit to bytecode
        var emitter = Emitter.initWithHeap(self.allocator, heap);
        emitter.emit(lambda_ir) catch {
            emitter.deinit();
            return error.InvalidSyntax;
        };

        // Get child chunks and main chunk
        const child_chunks = emitter.getChildChunks() catch {
            emitter.deinit();
            return error.OutOfMemory;
        };
        var chunk = emitter.finalize() catch {
            self.allocator.free(child_chunks);
            emitter.deinit();
            return error.OutOfMemory;
        };
        emitter.deinit();

        // Create temporary chunk pool for macro execution
        var chunk_ptrs = self.allocator.alloc(*Chunk, child_chunks.len) catch {
            self.allocator.free(child_chunks);
            return error.OutOfMemory;
        };
        defer self.allocator.free(chunk_ptrs);

        for (child_chunks, 0..) |*child_chunk, i| {
            const chunk_ptr = self.allocator.create(Chunk) catch {
                // Clean up already allocated
                for (chunk_ptrs[0..i]) |ptr| {
                    self.allocator.destroy(ptr);
                }
                self.allocator.free(child_chunks);
                return error.OutOfMemory;
            };
            chunk_ptr.* = child_chunk.*;
            chunk_ptrs[i] = chunk_ptr;
        }
        self.allocator.free(child_chunks);

        // Set chunk pool and run
        vm.setChunkPool(chunk_ptrs);
        const closure_val = vm.run(&chunk) catch {
            for (chunk_ptrs) |ptr| {
                self.allocator.destroy(ptr);
            }
            return error.InvalidSyntax;
        };

        if (!closure_val.isClosure()) {
            for (chunk_ptrs) |ptr| {
                self.allocator.destroy(ptr);
            }
            return error.InvalidSyntax;
        }
        const closure = closure_val.toPtr(Closure);

        // Now call the closure with the macro arguments
        // Push args onto stack, then call
        var arg_count: u8 = 0;
        var arg_list = args;
        while (arg_list.isCons()) {
            const arg_cons = arg_list.toPtr(Cons);
            vm.push(arg_cons.car) catch {
                for (chunk_ptrs) |ptr| {
                    self.allocator.destroy(ptr);
                }
                return error.InvalidSyntax;
            };
            arg_count += 1;
            arg_list = arg_cons.cdr;
        }

        // Call the macro expander
        const result = vm.callClosure(closure, arg_count) catch {
            for (chunk_ptrs) |ptr| {
                self.allocator.destroy(ptr);
            }
            return error.InvalidSyntax;
        };

        // Clean up temporary chunks (closure is no longer needed)
        for (chunk_ptrs) |ptr| {
            self.allocator.destroy(ptr);
        }

        return result;
    }

    fn compileIf(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        return self.compileIfWithTail(args, env, false);
    }

    fn compileIfWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (if test then else?)
        if (!args.isCons()) return error.InvalidIf;

        const cons1 = args.toPtr(Cons);
        const test_expr = cons1.car;
        const rest1 = cons1.cdr;

        if (!rest1.isCons()) return error.InvalidIf;
        const cons2 = rest1.toPtr(Cons);
        const then_expr = cons2.car;
        const rest2 = cons2.cdr;

        // else is optional, defaults to nil
        const else_expr = if (rest2.isCons())
            rest2.toPtr(Cons).car
        else
            Value.nil;

        const test_ir = try self.compile(test_expr, env);

        // Check for type predicate to enable occurrence typing
        const pred_info = extractPredicateInfo(test_ir);

        // Compile then-branch with narrowed type context if predicate detected
        const then_ir = blk: {
            if (pred_info) |info| {
                // Create occurrence context for then-branch
                var then_occ = OccurrenceCtx.init(self.allocator);
                defer then_occ.deinit();
                then_occ.narrowed.put(info.var_name, info.narrowed_type) catch
                    return error.OutOfMemory;

                // Save and restore outer occ context
                const saved_occ = self.occ;
                self.occ = &then_occ;
                defer self.occ = saved_occ;

                break :blk try self.compileWithTail(then_expr, env, in_tail);
            } else {
                break :blk try self.compileWithTail(then_expr, env, in_tail);
            }
        };

        // Compile else-branch with else-type narrowing if available
        const else_ir = blk: {
            if (pred_info) |info| {
                if (info.else_type) |else_ty| {
                    // Create occurrence context for else-branch
                    var else_occ = OccurrenceCtx.init(self.allocator);
                    defer else_occ.deinit();
                    else_occ.narrowed.put(info.var_name, else_ty) catch
                        return error.OutOfMemory;

                    // Save and restore outer occ context
                    const saved_occ = self.occ;
                    self.occ = &else_occ;
                    defer self.occ = saved_occ;

                    break :blk try self.compileWithTail(else_expr, env, in_tail);
                }
            }
            break :blk try self.compileWithTail(else_expr, env, in_tail);
        };

        return self.builder.ifExpr(test_ir, then_ir, else_ir) catch return error.OutOfMemory;
    }

    /// Typed parameter info for function declarations
    const TypedParam = struct {
        name: []const u8,
        type_name: ?[]const u8, // null for untyped
    };

    fn compileLambda(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        return self.compileLambdaCore(args, env, null);
    }

    fn compileLambdaCore(self: *Compiler, args: Value, env: *const Env, return_type: ?[]const u8) CompileError!*Ir {
        // (lambda (params...) body)
        // Params can be: symbol for untyped, (symbol type) for typed
        if (!args.isCons()) return error.InvalidLambda;

        const cons = args.toPtr(Cons);
        const params_expr = cons.car;
        const body_exprs = cons.cdr;

        // Parse parameters (supports typed and untyped)
        var params = std.ArrayList([]const u8){};
        defer params.deinit(self.allocator);

        var typed_params = std.ArrayList(TypedParam){};
        defer typed_params.deinit(self.allocator);

        var param_list = params_expr;
        while (param_list.isCons()) {
            const param_cons = param_list.toPtr(Cons);
            const param_item = param_cons.car;

            if (param_item.isSymbol()) {
                // Untyped parameter: just a symbol
                const param_sym = param_item.toPtr(Symbol);
                const name = param_sym.getName();
                params.append(self.allocator, name) catch return error.OutOfMemory;
                typed_params.append(self.allocator, .{ .name = name, .type_name = null }) catch return error.OutOfMemory;
            } else if (param_item.isCons()) {
                // Typed parameter: (name type)
                const typed = param_item.toPtr(Cons);
                if (!typed.car.isSymbol()) return error.InvalidLambda;
                const name_sym = typed.car.toPtr(Symbol);
                const name = name_sym.getName();

                if (!typed.cdr.isCons()) return error.InvalidLambda;
                const type_cons = typed.cdr.toPtr(Cons);
                if (!type_cons.car.isSymbol()) return error.InvalidLambda;
                const type_sym = type_cons.car.toPtr(Symbol);
                const type_name = type_sym.getName();

                params.append(self.allocator, name) catch return error.OutOfMemory;
                typed_params.append(self.allocator, .{ .name = name, .type_name = type_name }) catch return error.OutOfMemory;
            } else {
                return error.InvalidLambda;
            }

            param_list = param_cons.cdr;
        }

        // Check for rest parameter (dotted list: (a b . rest))
        var rest_param: ?[]const u8 = null;
        if (!param_list.isNil()) {
            if (param_list.isSymbol()) {
                const rest_sym = param_list.toPtr(Symbol);
                rest_param = rest_sym.getName();
            } else {
                return error.InvalidLambda;
            }
        }

        // Create new environment with parameters
        var lambda_env = Env.init(self.allocator, env);
        defer lambda_env.deinit();

        for (params.items) |param| {
            _ = lambda_env.bind(param) catch return error.OutOfMemory;
        }

        // Bind rest parameter if present
        if (rest_param) |rp| {
            _ = lambda_env.bind(rp) catch return error.OutOfMemory;
        }

        // Capture analysis: collect free variables before compiling body
        var capture_set = CaptureSet.init(self.allocator);
        defer capture_set.deinit();

        self.collectFreeVars(body_exprs, &lambda_env, &capture_set) catch
            return error.OutOfMemory;

        // Compile body (implicit progn) - body is in tail position
        var body_ir = try self.compileBodyWithTail(body_exprs, &lambda_env, true);

        // Prepend type assertions for typed parameters
        var assertions = std.ArrayList(*Ir){};
        defer assertions.deinit(self.allocator);

        for (typed_params.items) |tp| {
            if (tp.type_name) |type_name| {
                // Create variable reference for parameter
                const binding = lambda_env.lookup(tp.name) orelse return error.OutOfMemory;
                const var_ir = self.builder.variable(tp.name, binding.depth, binding.index) catch
                    return error.OutOfMemory;

                // Create assertion based on type name
                const assert_ir = self.makeTypeAssertion(var_ir, type_name) catch
                    return error.InvalidSyntax;
                if (assert_ir) |assert_node| {
                    assertions.append(self.allocator, assert_node) catch return error.OutOfMemory;
                }
            }
        }

        // If we have assertions, wrap body in progn with assertions first
        if (assertions.items.len > 0) {
            assertions.append(self.allocator, body_ir) catch return error.OutOfMemory;
            const items = self.allocator.dupe(*const Ir, assertions.items) catch
                return error.OutOfMemory;
            body_ir = self.builder.progn(items) catch return error.OutOfMemory;
        }

        // Wrap body in return type assertion if specified
        if (return_type) |ret_type| {
            const assert_ir = self.makeTypeAssertion(body_ir, ret_type) catch
                return error.InvalidSyntax;
            if (assert_ir) |wrapped| {
                body_ir = wrapped;
            }
        }

        // Convert captures to slice
        const captures = self.allocator.dupe(Ir.Capture, capture_set.captures.items) catch
            return error.OutOfMemory;

        return self.builder.lambda(params.items, rest_param, captures, body_ir) catch
            return error.OutOfMemory;
    }

    /// Create a type assertion IR node for a given type name
    fn makeTypeAssertion(self: *Compiler, expr_ir: *Ir, type_name: []const u8) !?*Ir {
        if (std.mem.eql(u8, type_name, "fixnum")) {
            return self.builder.assertFixnum(expr_ir);
        }
        if (std.mem.eql(u8, type_name, "cons")) {
            return self.builder.assertCons(expr_ir);
        }
        if (std.mem.eql(u8, type_name, "symbol")) {
            return self.builder.assertSymbol(expr_ir);
        }
        if (std.mem.eql(u8, type_name, "string")) {
            return self.builder.assertString(expr_ir);
        }
        if (std.mem.eql(u8, type_name, "vector")) {
            return self.builder.assertVector(expr_ir);
        }
        if (std.mem.eql(u8, type_name, "closure")) {
            return self.builder.assertClosure(expr_ir);
        }
        if (std.mem.eql(u8, type_name, "list")) {
            return self.builder.assertList(expr_ir);
        }
        if (std.mem.eql(u8, type_name, "non-nil")) {
            return self.builder.assertNonNil(expr_ir);
        }
        // "any" means no check needed
        if (std.mem.eql(u8, type_name, "any")) {
            return null;
        }
        return error.InvalidSyntax;
    }

    /// Collect free variables in an expression
    fn collectFreeVars(self: *Compiler, expr: Value, env: *const Env, captures: *CaptureSet) error{OutOfMemory}!void {
        if (expr.isNil() or expr.isFixnum() or expr.isString() or expr.isKeyword() or expr.isCharacter()) {
            return; // Literals have no free variables
        }

        if (expr.isSymbol()) {
            const sym = expr.toPtr(Symbol);
            const name = sym.getName();

            // Check if bound locally
            if (env.bindings.get(name) != null) return;

            // Check if in outer scope (free variable)
            if (env.parent) |parent| {
                if (parent.lookup(name)) |binding| {
                    // This is a free variable - needs to be captured
                    // Store original depth for loading, emitVar adjusts for matching
                    try captures.addCapture(name, binding.depth, binding.index);
                }
            }
            return;
        }

        if (expr.isCons()) {
            const cons = expr.toPtr(Cons);
            const head = cons.car;
            const tail = cons.cdr;

            // Check for special forms that introduce bindings
            if (head.isSymbol()) {
                const b = self.builtins orelse return;

                if (head.raw == b.lambda.raw or head.raw == b.@"fn".raw) {
                    // Lambda creates new scope - handled recursively by compileLambda
                    return;
                }

                if (head.raw == b.let.raw) {
                    // Let introduces bindings - need to handle carefully
                    if (tail.isCons()) {
                        const let_cons = tail.toPtr(Cons);
                        const bindings_expr = let_cons.car;
                        const body_expr = let_cons.cdr;

                        // Collect free vars in binding values (before let scope)
                        var binding_list = bindings_expr;
                        while (binding_list.isCons()) {
                            const binding_cons = binding_list.toPtr(Cons);
                            const binding = binding_cons.car;
                            if (binding.isCons()) {
                                const binding_pair = binding.toPtr(Cons);
                                if (binding_pair.cdr.isCons()) {
                                    const val_cons = binding_pair.cdr.toPtr(Cons);
                                    try self.collectFreeVars(val_cons.car, env, captures);
                                }
                            }
                            binding_list = binding_cons.cdr;
                        }

                        // Create temp env for let body (same frame)
                        var let_env = Env.initLet(self.allocator, env);
                        defer let_env.deinit();

                        binding_list = bindings_expr;
                        while (binding_list.isCons()) {
                            const binding_cons = binding_list.toPtr(Cons);
                            const binding = binding_cons.car;
                            if (binding.isCons()) {
                                const binding_pair = binding.toPtr(Cons);
                                if (binding_pair.car.isSymbol()) {
                                    const bname_sym = binding_pair.car.toPtr(Symbol);
                                    _ = let_env.bind(bname_sym.getName()) catch return;
                                }
                            }
                            binding_list = binding_cons.cdr;
                        }

                        // Collect from body with extended env
                        try self.collectFreeVarsInList(body_expr, &let_env, captures);
                    }
                    return;
                }

                if (head.raw == b.quote.raw) {
                    return; // Quoted expressions have no free variables
                }
            }

            // Recurse on head and tail
            try self.collectFreeVars(head, env, captures);
            try self.collectFreeVarsInList(tail, env, captures);
        }
    }

    /// Collect free variables in a list of expressions
    fn collectFreeVarsInList(self: *Compiler, list: Value, env: *const Env, captures: *CaptureSet) error{OutOfMemory}!void {
        var current = list;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            try self.collectFreeVars(cons.car, env, captures);
            current = cons.cdr;
        }
    }

    /// Find variables that need boxing: both mutated via set! AND captured by a lambda
    /// This pre-scans a let body to determine which bindings need automatic boxing
    fn findBoxedVars(self: *Compiler, body: Value, binding_names: []const []const u8) error{OutOfMemory}!BoxingSet {
        var mutated = std.StringHashMap(void).init(self.allocator);
        defer mutated.deinit();

        var captured = std.StringHashMap(void).init(self.allocator);
        defer captured.deinit();

        // Collect mutations and captures from body
        try self.collectMutationsAndCaptures(body, binding_names, &mutated, &captured);

        // Intersection: names that are both mutated AND captured need boxing
        var result = BoxingSet.init(self.allocator);
        var iter = mutated.keyIterator();
        while (iter.next()) |name| {
            if (captured.contains(name.*)) {
                try result.add(name.*);
            }
        }
        return result;
    }

    /// Recursively collect mutations (set!) and lambda captures in an expression
    fn collectMutationsAndCaptures(
        self: *Compiler,
        expr: Value,
        binding_names: []const []const u8,
        mutated: *std.StringHashMap(void),
        captured: *std.StringHashMap(void),
    ) error{OutOfMemory}!void {
        if (expr.isNil() or expr.isFixnum() or expr.isString() or expr.isKeyword() or expr.isCharacter()) {
            return;
        }

        if (!expr.isCons()) return;

        // Builtins required for symbol identity comparison
        const b = self.builtins orelse return;

        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        if (head.isSymbol()) {
            // Check for (set! var ...)
            if (head.raw == b.@"set!".raw) {
                if (tail.isCons()) {
                    const set_cons = tail.toPtr(Cons);
                    if (set_cons.car.isSymbol()) {
                        const var_sym = set_cons.car.toPtr(Symbol);
                        const var_name = var_sym.getName();
                        // Check if this is one of our bindings
                        for (binding_names) |bn| {
                            if (std.mem.eql(u8, var_name, bn)) {
                                try mutated.put(var_name, {});
                                break;
                            }
                        }
                    }
                    // Recurse into the value expression
                    if (set_cons.cdr.isCons()) {
                        const val_cons = set_cons.cdr.toPtr(Cons);
                        try self.collectMutationsAndCaptures(val_cons.car, binding_names, mutated, captured);
                    }
                }
                return;
            }

            // Check for (lambda ...) - collect free vars that are our bindings
            // AND mutations inside the lambda body
            if (head.raw == b.lambda.raw or head.raw == b.@"fn".raw) {
                try self.collectLambdaCaptures(tail, binding_names, captured);
                // Also look for mutations inside the lambda body
                if (tail.isCons()) {
                    const lam_cons = tail.toPtr(Cons);
                    // Skip params, recurse into body
                    try self.collectMutationsAndCapturesInList(lam_cons.cdr, binding_names, mutated, captured);
                }
                return;
            }

            // Skip quote - don't analyze quoted expressions
            if (head.raw == b.quote.raw) {
                return;
            }
        }

        // Recurse into all elements of the list
        try self.collectMutationsAndCaptures(head, binding_names, mutated, captured);
        try self.collectMutationsAndCapturesInList(tail, binding_names, mutated, captured);
    }

    fn collectMutationsAndCapturesInList(
        self: *Compiler,
        list: Value,
        binding_names: []const []const u8,
        mutated: *std.StringHashMap(void),
        captured: *std.StringHashMap(void),
    ) error{OutOfMemory}!void {
        var current = list;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            try self.collectMutationsAndCaptures(cons.car, binding_names, mutated, captured);
            current = cons.cdr;
        }
    }

    /// Collect which of our bindings are captured by a lambda
    fn collectLambdaCaptures(self: *Compiler, lambda_args: Value, binding_names: []const []const u8, captured: *std.StringHashMap(void)) error{OutOfMemory}!void {
        if (!lambda_args.isCons()) return;

        const args_cons = lambda_args.toPtr(Cons);
        const params_expr = args_cons.car;
        const body = args_cons.cdr;

        // Get lambda parameter names to exclude from captures
        var param_names = std.StringHashMap(void).init(self.allocator);
        defer param_names.deinit();

        var param_list = params_expr;
        while (param_list.isCons()) {
            const param_cons = param_list.toPtr(Cons);
            if (param_cons.car.isSymbol()) {
                const param_sym = param_cons.car.toPtr(Symbol);
                try param_names.put(param_sym.getName(), {});
            }
            param_list = param_cons.cdr;
        }

        // Find free variable references in body that are our bindings
        try self.collectFreeVarRefs(body, binding_names, &param_names, captured);
    }

    /// Find references to binding names in expression (excluding params)
    fn collectFreeVarRefs(
        self: *Compiler,
        expr: Value,
        binding_names: []const []const u8,
        params: *std.StringHashMap(void),
        captured: *std.StringHashMap(void),
    ) error{OutOfMemory}!void {
        if (expr.isNil() or expr.isFixnum() or expr.isString() or expr.isKeyword() or expr.isCharacter()) {
            return;
        }

        if (expr.isSymbol()) {
            const sym = expr.toPtr(Symbol);
            const name = sym.getName();
            // If it's not a param and is one of our bindings, it's captured
            if (!params.contains(name)) {
                for (binding_names) |bn| {
                    if (std.mem.eql(u8, name, bn)) {
                        try captured.put(name, {});
                        break;
                    }
                }
            }
            return;
        }

        if (!expr.isCons()) return;

        const cons = expr.toPtr(Cons);

        // Handle quote specially - don't look inside
        if (cons.car.isSymbol()) {
            const b = self.builtins orelse return;
            const head = cons.car;
            if (head.raw == b.quote.raw) return;

            // Handle nested lambda - need to add its params to exclusion
            if (head.raw == b.lambda.raw or head.raw == b.@"fn".raw) {
                if (cons.cdr.isCons()) {
                    const lam_cons = cons.cdr.toPtr(Cons);
                    const lam_params = lam_cons.car;
                    const lam_body = lam_cons.cdr;

                    // Collect nested lambda params
                    var nested_params = std.StringHashMap(void).init(self.allocator);
                    defer nested_params.deinit();

                    // Copy existing params
                    var iter = params.keyIterator();
                    while (iter.next()) |k| {
                        try nested_params.put(k.*, {});
                    }

                    // Add lambda params
                    var pl = lam_params;
                    while (pl.isCons()) {
                        const pc = pl.toPtr(Cons);
                        if (pc.car.isSymbol()) {
                            const ps = pc.car.toPtr(Symbol);
                            try nested_params.put(ps.getName(), {});
                        }
                        pl = pc.cdr;
                    }

                    // Recurse with extended params
                    try self.collectFreeVarRefsInList(lam_body, binding_names, &nested_params, captured);
                }
                return;
            }
        }

        try self.collectFreeVarRefs(cons.car, binding_names, params, captured);
        try self.collectFreeVarRefsInList(cons.cdr, binding_names, params, captured);
    }

    fn collectFreeVarRefsInList(
        self: *Compiler,
        list: Value,
        binding_names: []const []const u8,
        params: *std.StringHashMap(void),
        captured: *std.StringHashMap(void),
    ) error{OutOfMemory}!void {
        var current = list;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            try self.collectFreeVarRefs(cons.car, binding_names, params, captured);
            current = cons.cdr;
        }
    }

    fn compileLet(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        return self.compileLetWithTail(args, env, false);
    }

    fn compileLetWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (let ((x 1) (y 2)) body)
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // First pass: collect binding names for boxing analysis
        var binding_names = std.ArrayList([]const u8){};
        defer binding_names.deinit(self.allocator);

        var binding_list = bindings_expr;
        while (binding_list.isCons()) {
            const binding_cons = binding_list.toPtr(Cons);
            const binding = binding_cons.car;

            if (!binding.isCons()) return error.InvalidLet;
            const b = binding.toPtr(Cons);

            if (!b.car.isSymbol()) return error.InvalidLet;
            const name_sym = b.car.toPtr(Symbol);
            binding_names.append(self.allocator, name_sym.getName()) catch return error.OutOfMemory;

            binding_list = binding_cons.cdr;
        }

        // Find variables that need boxing (mutable + captured by lambda)
        var boxed = self.findBoxedVars(body_exprs, binding_names.items) catch return error.OutOfMemory;
        defer boxed.deinit();

        // Second pass: compile bindings, wrapping boxed ones with make-box
        var bindings = std.ArrayList(Ir.Binding){};
        defer bindings.deinit(self.allocator);

        binding_list = bindings_expr;
        var name_idx: usize = 0;
        while (binding_list.isCons()) : (name_idx += 1) {
            const binding_cons = binding_list.toPtr(Cons);
            const binding = binding_cons.car;

            const b = binding.toPtr(Cons);
            const name = binding_names.items[name_idx];

            // Get value expression
            if (!b.cdr.isCons()) return error.InvalidLet;
            const val_cons = b.cdr.toPtr(Cons);
            var val_ir = try self.compile(val_cons.car, env);

            // If this variable needs boxing, wrap value in make-box
            if (boxed.contains(name)) {
                const box_ir = self.allocator.create(Ir) catch return error.OutOfMemory;
                box_ir.* = .{ .make_box = .{ .operand = val_ir } };
                val_ir = box_ir;
            }

            bindings.append(self.allocator, .{ .name = name, .value = val_ir }) catch
                return error.OutOfMemory;

            binding_list = binding_cons.cdr;
        }

        // Create same-frame environment with bindings (let doesn't create new frame)
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();

        for (bindings.items) |b| {
            _ = let_env.bind(b.name) catch return error.OutOfMemory;
        }

        // Set boxed_vars so that variable refs and set! use box operations
        const saved_boxed = self.boxed_vars;
        if (boxed.names.count() > 0) {
            self.boxed_vars = &boxed;
        }

        // Compile body - body is in tail position if let is
        const body_ir = try self.compileBodyWithTail(body_exprs, &let_env, in_tail);

        // Restore previous boxed_vars
        self.boxed_vars = saved_boxed;

        return self.builder.letExpr(bindings.items, body_ir) catch return error.OutOfMemory;
    }

    fn compileLetrecWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (letrec ((f (lambda ...)) (g (lambda ...))) body)
        // Compile as: pre-register globals, define each, then body
        // This allows recursive/mutual recursion via global references
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // First pass: pre-register all globals (like defun does)
        var names = std.ArrayList([]const u8){};
        defer names.deinit(self.allocator);

        var val_exprs = std.ArrayList(Value){};
        defer val_exprs.deinit(self.allocator);

        var indices = std.ArrayList(u16){};
        defer indices.deinit(self.allocator);

        var binding_list = bindings_expr;
        while (binding_list.isCons()) {
            const binding_cons = binding_list.toPtr(Cons);
            const binding = binding_cons.car;

            if (!binding.isCons()) return error.InvalidLet;
            const b = binding.toPtr(Cons);

            if (!b.car.isSymbol()) return error.InvalidLet;
            const name_sym = b.car.toPtr(Symbol);
            const name = name_sym.getName();

            if (!b.cdr.isCons()) return error.InvalidLet;
            const val_cons = b.cdr.toPtr(Cons);

            // Pre-register global for recursive visibility
            const idx = self.globals.define(name) catch return error.OutOfMemory;

            names.append(self.allocator, name) catch return error.OutOfMemory;
            val_exprs.append(self.allocator, val_cons.car) catch return error.OutOfMemory;
            indices.append(self.allocator, idx) catch return error.OutOfMemory;

            binding_list = binding_cons.cdr;
        }

        // Second pass: compile values and create defines
        var exprs = std.ArrayList(*const Ir){};
        defer exprs.deinit(self.allocator);

        for (names.items, val_exprs.items, indices.items) |name, val_expr, idx| {
            const val_ir = try self.compile(val_expr, env);
            const define_ir = self.builder.define(name, idx, val_ir) catch return error.OutOfMemory;
            exprs.append(self.allocator, define_ir) catch return error.OutOfMemory;
        }

        // Compile body (in tail position if letrec is)
        const body_ir = try self.compileBodyWithTail(body_exprs, env, in_tail);
        exprs.append(self.allocator, body_ir) catch return error.OutOfMemory;

        // Return progn of defines + body
        const items = self.allocator.dupe(*const Ir, exprs.items) catch return error.OutOfMemory;
        return self.builder.progn(items) catch return error.OutOfMemory;
    }

    fn compileFletWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (flet ((fname (args) body...) ...) body)
        // Desugars to let with lambdas - functions don't see each other
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // Parse function definitions and create lambda bindings
        var bindings = std.ArrayList(Ir.Binding){};
        defer bindings.deinit(self.allocator);

        var binding_list = bindings_expr;
        while (binding_list.isCons()) {
            const binding_cons = binding_list.toPtr(Cons);
            const fdef = binding_cons.car;

            // Each fdef is (fname (params) body...)
            if (!fdef.isCons()) return error.InvalidLet;
            const f = fdef.toPtr(Cons);

            if (!f.car.isSymbol()) return error.InvalidLet;
            const name_sym = f.car.toPtr(Symbol);
            const name = name_sym.getName();

            // Build lambda from rest: ((params) body...) -> compile as lambda
            if (!f.cdr.isCons()) return error.InvalidLet;
            const lambda_ir = try self.compileLambda(f.cdr, env);

            bindings.append(self.allocator, .{ .name = name, .value = lambda_ir }) catch
                return error.OutOfMemory;

            binding_list = binding_cons.cdr;
        }

        // Create same-frame environment with bindings
        var flet_env = Env.initLet(self.allocator, env);
        defer flet_env.deinit();

        for (bindings.items) |b| {
            _ = flet_env.bind(b.name) catch return error.OutOfMemory;
        }

        // Compile body in new environment
        const body_ir = try self.compileBodyWithTail(body_exprs, &flet_env, in_tail);

        return self.builder.letExpr(bindings.items, body_ir) catch return error.OutOfMemory;
    }

    fn compileLabelsWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (labels ((fname (args) body...) ...) body)
        // Like letrec - functions can see each other and themselves
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // First pass: pre-register all globals for mutual visibility
        var names = std.ArrayList([]const u8){};
        defer names.deinit(self.allocator);

        var lambda_args = std.ArrayList(Value){};
        defer lambda_args.deinit(self.allocator);

        var indices = std.ArrayList(u16){};
        defer indices.deinit(self.allocator);

        var binding_list = bindings_expr;
        while (binding_list.isCons()) {
            const binding_cons = binding_list.toPtr(Cons);
            const fdef = binding_cons.car;

            // Each fdef is (fname (params) body...)
            if (!fdef.isCons()) return error.InvalidLet;
            const f = fdef.toPtr(Cons);

            if (!f.car.isSymbol()) return error.InvalidLet;
            const name_sym = f.car.toPtr(Symbol);
            const name = name_sym.getName();

            // Pre-register global for recursive visibility
            const idx = self.globals.define(name) catch return error.OutOfMemory;

            if (!f.cdr.isCons()) return error.InvalidLet;

            names.append(self.allocator, name) catch return error.OutOfMemory;
            lambda_args.append(self.allocator, f.cdr) catch return error.OutOfMemory;
            indices.append(self.allocator, idx) catch return error.OutOfMemory;

            binding_list = binding_cons.cdr;
        }

        // Second pass: compile lambdas and create defines
        var exprs = std.ArrayList(*const Ir){};
        defer exprs.deinit(self.allocator);

        for (names.items, lambda_args.items, indices.items) |name, largs, idx| {
            const lambda_ir = try self.compileLambda(largs, env);
            const define_ir = self.builder.define(name, idx, lambda_ir) catch return error.OutOfMemory;
            exprs.append(self.allocator, define_ir) catch return error.OutOfMemory;
        }

        // Compile body
        const body_ir = try self.compileBodyWithTail(body_exprs, env, in_tail);
        exprs.append(self.allocator, body_ir) catch return error.OutOfMemory;

        // Return progn of defines + body
        const items = self.allocator.dupe(*const Ir, exprs.items) catch return error.OutOfMemory;
        return self.builder.progn(items) catch return error.OutOfMemory;
    }

    fn compileLetStarWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (let* ((x 1) (y (+ x 1))) body)
        // Compiles to nested lets: (let ((x 1)) (let ((y (+ x 1))) body))
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // If no bindings, just compile body
        if (bindings_expr.isNil()) {
            return self.compileBodyWithTail(body_exprs, env, in_tail);
        }

        if (!bindings_expr.isCons()) return error.InvalidLet;

        // Recursively compile as nested lets
        return self.compileLetStarBindings(bindings_expr, body_exprs, env, in_tail);
    }

    fn compileLetStarBindings(self: *Compiler, bindings_list: Value, body_exprs: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        if (!bindings_list.isCons()) return error.InvalidLet;

        const binding_cons = bindings_list.toPtr(Cons);
        const binding = binding_cons.car;
        const rest = binding_cons.cdr;

        if (!binding.isCons()) return error.InvalidLet;
        const b = binding.toPtr(Cons);

        if (!b.car.isSymbol()) return error.InvalidLet;
        const name_sym = b.car.toPtr(Symbol);
        const name = name_sym.getName();

        if (!b.cdr.isCons()) return error.InvalidLet;
        const val_cons = b.cdr.toPtr(Cons);

        // Compile value in current environment
        const val_ir = try self.compile(val_cons.car, env);

        // Create single-binding array
        const binding_array = [_]ir.Ir.Binding{.{ .name = name, .value = val_ir }};

        // Create extended environment for rest
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();
        _ = try let_env.bind(name);

        // Compile rest or body
        const inner_ir = if (rest.isNil())
            try self.compileBodyWithTail(body_exprs, &let_env, in_tail)
        else
            try self.compileLetStarBindings(rest, body_exprs, &let_env, in_tail);

        return self.builder.letExpr(&binding_array, inner_ir) catch return error.OutOfMemory;
    }

    fn compileCondWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (cond (test1 expr1...) (test2 expr2...) ... [(t exprN...)])
        // Transform to nested ifs
        if (args.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const clause = cons.car;
        const rest_clauses = cons.cdr;

        if (!clause.isCons()) return error.InvalidSyntax;
        const clause_cons = clause.toPtr(Cons);
        const test_expr = clause_cons.car;
        const body_exprs = clause_cons.cdr;

        // Check for default clause (t or else)
        const is_default = if (test_expr.isSymbol()) blk: {
            const sym = test_expr.toPtr(Symbol);
            const name = sym.getName();
            break :blk std.mem.eql(u8, name, "t") or std.mem.eql(u8, name, "else");
        } else false;

        if (is_default) {
            return self.compileBodyWithTail(body_exprs, env, in_tail);
        }

        const test_ir = try self.compile(test_expr, env);
        const then_ir = try self.compileBodyWithTail(body_exprs, env, in_tail);
        const else_ir = try self.compileCondWithTail(rest_clauses, env, in_tail);

        return self.builder.ifExpr(test_ir, then_ir, else_ir) catch return error.OutOfMemory;
    }

    fn compileAnd(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (and a b) -> (if a b nil)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const first = cons1.car;
        const rest = cons1.cdr;

        if (rest.isNil()) {
            return self.compile(first, env);
        }

        if (!rest.isCons()) return error.InvalidSyntax;
        const cons2 = rest.toPtr(Cons);
        const second = cons2.car;

        // Handle variadic: (and a b c ...) -> (and a (and b c ...))
        if (!cons2.cdr.isNil()) {
            const nested_and = try self.compileAnd(rest, env);
            const first_ir = try self.compile(first, env);
            const nil_ir = try self.builder.lit(Value.nil);
            return self.builder.ifExpr(first_ir, nested_and, nil_ir) catch return error.OutOfMemory;
        }

        const first_ir = try self.compile(first, env);
        const second_ir = try self.compile(second, env);
        const nil_ir = try self.builder.lit(Value.nil);

        return self.builder.ifExpr(first_ir, second_ir, nil_ir) catch return error.OutOfMemory;
    }

    fn compileOr(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (or a b) -> (let ((tmp a)) (if tmp tmp b))
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const first = cons1.car;
        const rest = cons1.cdr;

        if (rest.isNil()) {
            return self.compile(first, env);
        }

        if (!rest.isCons()) return error.InvalidSyntax;

        // Compile rest for else branch
        const else_ir = try self.compileOr(rest, env);
        const first_ir = try self.compile(first, env);

        // Create let binding for tmp
        const tmp_name = "__or_tmp";
        const bindings = self.allocator.alloc(ir.Ir.Binding, 1) catch return error.OutOfMemory;
        bindings[0] = .{ .name = tmp_name, .value = first_ir };

        var tmp_env = Env.initLet(self.allocator, env);
        defer tmp_env.deinit();
        const tmp_idx = try tmp_env.bind(tmp_name);

        const tmp_var1 = try self.builder.variable(tmp_name, 0, tmp_idx);
        const tmp_var2 = try self.builder.variable(tmp_name, 0, tmp_idx);

        const body = try self.builder.ifExpr(tmp_var1, tmp_var2, else_ir);
        return self.builder.letExpr(bindings, body) catch return error.OutOfMemory;
    }

    fn compileFuncall(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (funcall fn arg1 arg2 ...) - same as regular call with computed function
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        return self.compileCallWithTail(cons1.car, cons1.cdr, env, false);
    }

    fn compileApply(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (apply fn args-list)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const fn_expr = cons1.car;
        const rest = cons1.cdr;

        if (!rest.isCons()) return error.InvalidSyntax;
        const cons2 = rest.toPtr(Cons);
        const args_list_expr = cons2.car;

        const fn_ir = try self.compile(fn_expr, env);
        const args_ir = try self.compile(args_list_expr, env);

        const node = self.allocator.create(ir.Ir) catch return error.OutOfMemory;
        node.* = .{ .apply = .{ .func = fn_ir, .args = args_ir } };
        return node;
    }

    fn compileSet(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (set! var value) or (setq var value)
        if (!args.isCons()) return error.InvalidSet;

        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSet;
        const var_sym = cons1.car.toPtr(Symbol);
        const name = var_sym.getName();

        if (!cons1.cdr.isCons()) return error.InvalidSet;
        const cons2 = cons1.cdr.toPtr(Cons);
        const val_ir = try self.compile(cons2.car, env);

        // First check local environment
        if (env.lookup(name)) |binding| {
            // If this variable is boxed, use box-set! instead
            if (self.boxed_vars) |bv| {
                if (bv.contains(name)) {
                    // Compile (box-set! var val) instead of (set! var val)
                    const var_ir = self.builder.variable(name, binding.depth, binding.index) catch
                        return error.OutOfMemory;
                    const box_set = self.allocator.create(Ir) catch return error.OutOfMemory;
                    box_set.* = .{ .box_set = .{ .left = var_ir, .right = val_ir } };
                    return box_set;
                }
            }
            return self.builder.set(name, binding.depth, binding.index, val_ir) catch
                return error.OutOfMemory;
        }

        // Check globals - if defined, use define to update
        if (self.globals.lookup(name)) |_| {
            // Re-define the global with the new value
            return self.builder.define(name, self.globals.lookup(name).?, val_ir) catch
                return error.OutOfMemory;
        }

        return error.UnboundVariable;
    }

    fn compileQuote(self: *Compiler, args: Value) CompileError!*Ir {
        // (quote expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const quoted = cons.car;

        // For symbols, use quote_sym
        if (quoted.isSymbol()) {
            const sym = quoted.toPtr(Symbol);
            return self.builder.quoteSym(sym.getName()) catch return error.OutOfMemory;
        }

        // For other values, return as literal
        return self.builder.lit(quoted) catch return error.OutOfMemory;
    }

    /// Compile function special form: (function name) or (function (lambda ...))
    /// #'name is reader syntax that expands to (function name)
    fn compileFunction(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (function expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const func_spec = cons.car;

        // (function symbol) - look up function binding as variable reference
        if (func_spec.isSymbol()) {
            // Compile symbol as a variable reference (will look up in env/globals)
            return self.compile(func_spec, env);
        }

        // (function (lambda ...)) - compile the lambda
        if (func_spec.isCons()) {
            const inner = func_spec.toPtr(Cons);
            if (inner.car.isSymbol()) {
                if (self.builtins) |b| {
                    if (inner.car.raw == b.lambda.raw or inner.car.raw == b.@"fn".raw) {
                        return self.compileLambda(inner.cdr, env);
                    }
                }
            }
        }

        return error.InvalidSyntax;
    }

    /// Compile quasiquote (backquote)
    /// Handles unquote (,) and unquote-splicing (,@)
    fn compileQuasiquote(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (quasiquote expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const expr = cons.car;

        return self.quasiquoteExpr(expr, env);
    }

    /// Process an expression inside quasiquote
    fn quasiquoteExpr(self: *Compiler, expr: Value, env: *const Env) CompileError!*Ir {
        // Non-list: return as quoted literal
        if (!expr.isCons()) {
            if (expr.isSymbol()) {
                const sym = expr.toPtr(Symbol);
                return self.builder.quoteSym(sym.getName()) catch return error.OutOfMemory;
            }
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        const cons = expr.toPtr(Cons);
        const head = cons.car;

        // Check for (unquote x) - evaluate x
        if (head.isSymbol()) {
            if (self.builtins) |b| {
                if (head.raw == b.unquote.raw) {
                    // (unquote x) -> compile x
                    if (!cons.cdr.isCons()) return error.InvalidSyntax;
                    const unquoted = cons.cdr.toPtr(Cons).car;
                    return self.compile(unquoted, env);
                }
                if (head.raw == b.@"unquote-splicing".raw) {
                    // unquote-splicing outside of list context is an error
                    return error.InvalidSyntax;
                }
            } else {
                // Fallback: string comparison
                const sym = head.toPtr(Symbol);
                const name = sym.getName();
                if (std.mem.eql(u8, name, "unquote")) {
                    if (!cons.cdr.isCons()) return error.InvalidSyntax;
                    const unquoted = cons.cdr.toPtr(Cons).car;
                    return self.compile(unquoted, env);
                }
                if (std.mem.eql(u8, name, "unquote-splicing")) {
                    return error.InvalidSyntax;
                }
            }
        }

        // Regular list: build with cons at runtime
        return self.quasiquoteList(expr, env);
    }

    /// Build a list from quasiquoted elements using cons/append
    fn quasiquoteList(self: *Compiler, list: Value, env: *const Env) CompileError!*Ir {
        if (list.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        if (!list.isCons()) {
            // Improper list tail - just quote it
            if (list.isSymbol()) {
                const sym = list.toPtr(Symbol);
                return self.builder.quoteSym(sym.getName()) catch return error.OutOfMemory;
            }
            return self.builder.lit(list) catch return error.OutOfMemory;
        }

        const cons = list.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check for (unquote-splicing x) - splice x into result
        if (head.isCons()) {
            const head_cons = head.toPtr(Cons);
            if (head_cons.car.isSymbol()) {
                const is_splice = if (self.builtins) |b|
                    head_cons.car.raw == b.@"unquote-splicing".raw
                else blk: {
                    const sym = head_cons.car.toPtr(Symbol);
                    break :blk std.mem.eql(u8, sym.getName(), "unquote-splicing");
                };

                if (is_splice) {
                    // (,@x ...) -> (append x (quasiquote-list ...))
                    if (!head_cons.cdr.isCons()) return error.InvalidSyntax;
                    const spliced = head_cons.cdr.toPtr(Cons).car;
                    const spliced_ir = try self.compile(spliced, env);
                    const rest_ir = try self.quasiquoteList(tail, env);

                    // Build (append spliced rest)
                    return self.builder.append(spliced_ir, rest_ir) catch return error.OutOfMemory;
                }
            }
        }

        // Regular element: (cons (quasiquote head) (quasiquote-list tail))
        const head_ir = try self.quasiquoteExpr(head, env);
        const tail_ir = try self.quasiquoteList(tail, env);

        return self.builder.cons(head_ir, tail_ir) catch return error.OutOfMemory;
    }

    fn compileProgn(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        return self.compileBody(args, env);
    }

    fn compilePrognWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        return self.compileBodyWithTail(args, env, in_tail);
    }

    fn compileWhile(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (while test body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const test_expr = cons.car;
        const body_exprs = cons.cdr;

        const test_ir = try self.compile(test_expr, env);
        const body_ir = try self.compileBody(body_exprs, env);

        return self.builder.loop(test_ir, body_ir) catch return error.OutOfMemory;
    }

    fn compileBlockWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (block name body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        if (!cons.car.isSymbol()) return error.InvalidSyntax;
        const name_sym = cons.car.toPtr(Symbol);
        const name = name_sym.getName();

        // Compile body
        const body_ir = try self.compileBodyWithTail(cons.cdr, env, in_tail);

        return self.builder.block(name, body_ir) catch return error.OutOfMemory;
    }

    fn compileReturnFrom(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (return-from name value)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        if (!cons.car.isSymbol()) return error.InvalidSyntax;
        const name_sym = cons.car.toPtr(Symbol);
        const name = name_sym.getName();

        // Get value (defaults to nil if not provided)
        const value = if (cons.cdr.isCons())
            cons.cdr.toPtr(Cons).car
        else
            Value.nil;

        const value_ir = try self.compile(value, env);

        return self.builder.returnFrom(name, value_ir) catch return error.OutOfMemory;
    }

    fn compileUnwindProtectWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (unwind-protect protected cleanup...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const protected = cons.car;

        // Protected form can be in tail position
        const protected_ir = try self.compileWithTail(protected, env, in_tail);

        // Cleanup forms are never in tail position (value discarded)
        const cleanup_ir = try self.compileBody(cons.cdr, env);

        return self.builder.unwindProtect(protected_ir, cleanup_ir) catch return error.OutOfMemory;
    }

    fn compileCatchWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (catch tag body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const tag = cons.car;

        // Tag is evaluated at runtime
        const tag_ir = try self.compile(tag, env);

        // Body can be in tail position
        const body_ir = try self.compileBodyWithTail(cons.cdr, env, in_tail);

        return self.builder.@"catch"(tag_ir, body_ir) catch return error.OutOfMemory;
    }

    fn compileThrow(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (throw tag value)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const tag = cons.car;

        // Tag is evaluated
        const tag_ir = try self.compile(tag, env);

        // Value defaults to nil
        const value = if (cons.cdr.isCons()) cons.cdr.toPtr(Cons).car else Value.nil;
        const value_ir = try self.compile(value, env);

        return self.builder.throw(tag_ir, value_ir) catch return error.OutOfMemory;
    }

    fn compileTagbody(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (tagbody [tag | form]...)
        // Parse body into tags and segments
        var tags = std.ArrayList([]const u8){};
        defer tags.deinit(self.allocator);

        var segments = std.ArrayList(*const Ir){};
        defer segments.deinit(self.allocator);

        var current_forms = std.ArrayList(Value){};
        defer current_forms.deinit(self.allocator);

        // Walk through body
        var rest = args;
        while (rest.isCons()) {
            const cons = rest.toPtr(Cons);
            const elem = cons.car;
            rest = cons.cdr;

            if (elem.isSymbol()) {
                // This is a tag - close current segment and start new one
                const segment_ir = try self.compileFormsToProgn(current_forms.items, env);
                segments.append(self.allocator, segment_ir) catch return error.OutOfMemory;
                current_forms.clearRetainingCapacity();

                const sym = elem.toPtr(Symbol);
                tags.append(self.allocator, sym.getName()) catch return error.OutOfMemory;
            } else {
                // This is a form - add to current segment
                current_forms.append(self.allocator, elem) catch return error.OutOfMemory;
            }
        }

        // Close final segment
        const final_segment = try self.compileFormsToProgn(current_forms.items, env);
        segments.append(self.allocator, final_segment) catch return error.OutOfMemory;

        return self.builder.tagbody(
            tags.items,
            segments.items,
        ) catch return error.OutOfMemory;
    }

    fn compileFormsToProgn(self: *Compiler, forms: []const Value, env: *const Env) CompileError!*Ir {
        if (forms.len == 0) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }
        if (forms.len == 1) {
            return self.compile(forms[0], env);
        }
        var exprs = std.ArrayList(*const Ir){};
        defer exprs.deinit(self.allocator);
        for (forms) |form| {
            const form_ir = try self.compile(form, env);
            exprs.append(self.allocator, form_ir) catch return error.OutOfMemory;
        }
        return self.builder.progn(exprs.items) catch return error.OutOfMemory;
    }

    fn compileGo(self: *Compiler, args: Value) CompileError!*Ir {
        // (go tag)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        if (!cons.car.isSymbol()) return error.InvalidSyntax;

        const sym = cons.car.toPtr(Symbol);
        const name = sym.getName();

        return self.builder.go(name) catch return error.OutOfMemory;
    }

    fn compileValues(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (values v1 v2 ...)
        var vals = std.ArrayList(*const Ir){};
        defer vals.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const val_ir = try self.compile(cons.car, env);
            vals.append(self.allocator, val_ir) catch return error.OutOfMemory;
            current = cons.cdr;
        }

        return self.builder.values(vals.items) catch return error.OutOfMemory;
    }

    fn compileMvBind(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (multiple-value-bind (var1 var2 ...) expr body...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);

        // Parse variable list
        var vars = std.ArrayList([]const u8){};
        defer vars.deinit(self.allocator);

        var var_list = cons1.car;
        while (var_list.isCons()) {
            const var_cons = var_list.toPtr(Cons);
            if (!var_cons.car.isSymbol()) return error.InvalidSyntax;
            const sym = var_cons.car.toPtr(Symbol);
            vars.append(self.allocator, sym.getName()) catch return error.OutOfMemory;
            var_list = var_cons.cdr;
        }

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);

        // Compile the expression that produces multiple values
        const expr_ir = try self.compile(cons2.car, env);

        // Create environment with bindings for vars
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();

        for (vars.items) |var_name| {
            _ = let_env.bind(var_name) catch return error.OutOfMemory;
        }

        // Compile body forms
        const body_ir = try self.compileBody(cons2.cdr, &let_env);

        return self.builder.mvBind(vars.items, expr_ir, body_ir) catch return error.OutOfMemory;
    }

    fn compileMvCall(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (multiple-value-call fn form1 form2 ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);

        // Compile function expression
        const func_ir = try self.compile(cons1.car, env);

        // Compile forms
        var forms = std.ArrayList(*const Ir){};
        defer forms.deinit(self.allocator);

        var current = cons1.cdr;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const form_ir = try self.compile(cons.car, env);
            forms.append(self.allocator, form_ir) catch return error.OutOfMemory;
            current = cons.cdr;
        }

        return self.builder.mvCall(func_ir, forms.items) catch return error.OutOfMemory;
    }

    fn compileMvList(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (multiple-value-list expr)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);

        const expr_ir = try self.compile(cons.car, env);
        return self.builder.mvList(expr_ir) catch return error.OutOfMemory;
    }

    fn compileDefine(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (define name value)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSyntax;
        const name_sym = cons1.car.toPtr(Symbol);
        const name = name_sym.getName();

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const value_ir = try self.compile(cons2.car, env);

        // Register global
        const idx = self.globals.define(name) catch return error.OutOfMemory;

        return self.builder.define(name, idx, value_ir) catch return error.OutOfMemory;
    }

    fn compileDefun(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (defun name (params...) body...) -> (define name (lambda (params...) body...))
        // (defun (name -> type) (params...) body...) -> with return type assertion
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const name_spec = cons1.car;

        var name: []const u8 = undefined;
        var return_type: ?[]const u8 = null;

        if (name_spec.isSymbol()) {
            // Simple: (defun name ...)
            const name_sym = name_spec.toPtr(Symbol);
            name = name_sym.getName();
        } else if (name_spec.isCons()) {
            // Typed: (defun (name -> type) ...)
            const spec_cons = name_spec.toPtr(Cons);
            if (!spec_cons.car.isSymbol()) return error.InvalidSyntax;
            const name_sym = spec_cons.car.toPtr(Symbol);
            name = name_sym.getName();

            // Check for -> arrow
            if (!spec_cons.cdr.isCons()) return error.InvalidSyntax;
            const arrow_cons = spec_cons.cdr.toPtr(Cons);
            if (!arrow_cons.car.isSymbol()) return error.InvalidSyntax;
            const arrow_sym = arrow_cons.car.toPtr(Symbol);
            if (!std.mem.eql(u8, arrow_sym.getName(), "->")) return error.InvalidSyntax;

            // Get return type
            if (!arrow_cons.cdr.isCons()) return error.InvalidSyntax;
            const type_cons = arrow_cons.cdr.toPtr(Cons);
            if (!type_cons.car.isSymbol()) return error.InvalidSyntax;
            const type_sym = type_cons.car.toPtr(Symbol);
            return_type = type_sym.getName();
        } else {
            return error.InvalidSyntax;
        }

        // Pre-register the global so recursive calls work
        const idx = self.globals.define(name) catch return error.OutOfMemory;

        // Rest is (params...) body...
        const lambda_args = cons1.cdr;
        const lambda_ir = try self.compileLambdaWithReturnType(lambda_args, env, return_type);

        return self.builder.define(name, idx, lambda_ir) catch return error.OutOfMemory;
    }

    /// Compile defmacro: (defmacro name (params...) body...)
    /// Stores the lambda-args in macro_table for expansion during macro calls.
    /// Returns nil since defmacro has no runtime effect.
    fn compileDefmacro(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        _ = env;
        // Parse: (name (params...) body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const name_val = cons1.car;
        if (!name_val.isSymbol()) return error.InvalidSyntax;

        const name_sym = name_val.toPtr(Symbol);
        const name = name_sym.getName();

        // Rest is ((params...) body...) - store as lambda-args for later expansion
        const lambda_args = cons1.cdr;
        if (!lambda_args.isCons()) return error.InvalidSyntax;

        // Store in macro_table: name -> lambda-args
        self.macro_table.put(name, lambda_args) catch return error.OutOfMemory;

        // defmacro has no runtime effect - return nil
        return self.builder.lit(Value.nil) catch return error.OutOfMemory;
    }

    /// Compile eval-when: (eval-when (situations...) body...)
    /// The REPL handles compile-time evaluation; compiler just handles :execute
    fn compileEvalWhen(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // Parse: (situations... body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const situations = cons1.car;
        const body = cons1.cdr;

        // Check situations for :execute (or :load-toplevel)
        var execute = false;
        var sit = situations;
        while (sit.isCons()) {
            const sit_cons = sit.toPtr(Cons);
            const situation = sit_cons.car;

            if (situation.isKeyword()) {
                const kw = situation.toPtr(runtime.Keyword);
                const kw_name = kw.getName();
                if (std.mem.eql(u8, kw_name, "execute") or std.mem.eql(u8, kw_name, "load-toplevel")) {
                    execute = true;
                    break;
                }
            }
            sit = sit_cons.cdr;
        }

        // If :execute, compile body as progn
        if (execute) {
            return self.compileProgn(body, env);
        }

        // Otherwise return nil (compile-time only)
        return self.builder.lit(Value.nil) catch return error.OutOfMemory;
    }

    // ========================================================================
    // ADT Support: deftype and match
    // ========================================================================

    /// Compile deftype: (deftype type-name (variant1 field1 field2) (variant2 field3) ...)
    /// Generates: constructors, type predicate, variant predicates, field accessors
    /// Runtime representation: #(:variant-tag field1 field2 ...)
    fn compileDeftype(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        _ = env;
        // Parse: (type-name (variant1 f1 f2) (variant2 f3) ...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const type_name_val = cons1.car;
        if (!type_name_val.isSymbol()) return error.InvalidSyntax;
        const type_name_raw = type_name_val.toPtr(Symbol).getName();
        // Dupe type name - symbol internal storage may be invalidated by GC
        const type_name = self.allocator.dupe(u8, type_name_raw) catch return error.OutOfMemory;

        // Collect variants
        var variants = std.ArrayList(Variant){};
        var current = cons1.cdr;
        while (current.isCons()) {
            const variant_cons = current.toPtr(Cons);
            const variant = try self.parseVariant(variant_cons.car);
            variants.append(self.allocator, variant) catch return error.OutOfMemory;
            current = variant_cons.cdr;
        }

        if (variants.items.len == 0) return error.InvalidSyntax;

        // Store type definition for match exhaustiveness checking
        self.defined_types.put(type_name, variants.items) catch return error.OutOfMemory;

        // Generate definitions as a progn:
        // 1. Constructor for each variant: (defun variant-name (fields...) (vector :variant-name fields...))
        // 2. Type predicate: (defun type-name? (x) (and (vectorp x) (member (aref x 0) '(:v1 :v2 ...))))
        // 3. Variant predicates: (defun variant-name? (x) (and (vectorp x) (eq (aref x 0) :variant-name)))
        // 4. Field accessors: (defun variant-name-field (x) (aref x field-index))

        var defs = std.ArrayList(*const Ir){};

        for (variants.items) |variant| {
            // Constructor: (variant-name f1 f2) -> #(:variant-name f1 f2)
            const ctor = try self.generateAdtConstructor(variant);
            defs.append(self.allocator, ctor) catch return error.OutOfMemory;

            // Variant predicate: (variant-name? x) -> (and (vectorp x) (eq (aref x 0) :variant-name))
            const pred = try self.generateVariantPredicate(variant);
            defs.append(self.allocator, pred) catch return error.OutOfMemory;

            // Field accessors: (variant-name-field x) -> (aref x index)
            for (variant.fields, 1..) |field, idx| {
                const accessor = try self.generateFieldAccessor(variant.name, field, @intCast(idx));
                defs.append(self.allocator, accessor) catch return error.OutOfMemory;
            }
        }

        // Type predicate: (type-name? x) -> checks if any variant matches
        const type_pred = try self.generateTypePredicate(type_name, variants.items);
        defs.append(self.allocator, type_pred) catch return error.OutOfMemory;

        // Return progn of all definitions
        const slice = defs.toOwnedSlice(self.allocator) catch return error.OutOfMemory;
        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .progn = slice };
        return node;
    }

    fn parseVariant(self: *Compiler, expr: Value) CompileError!Variant {
        // (variant-name field1 field2 ...)
        if (!expr.isCons()) return error.InvalidSyntax;

        const cons1 = expr.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSyntax;
        const sym_name = cons1.car.toPtr(Symbol).getName();
        // Dupe name to heap - symbol internal storage may be invalidated by GC
        const name = self.allocator.dupe(u8, sym_name) catch return error.OutOfMemory;

        var fields = std.ArrayList([]const u8){};
        var current = cons1.cdr;
        while (current.isCons()) {
            const field_cons = current.toPtr(Cons);
            if (!field_cons.car.isSymbol()) return error.InvalidSyntax;
            const field_sym_name = field_cons.car.toPtr(Symbol).getName();
            // Dupe field name to heap
            const field_name = self.allocator.dupe(u8, field_sym_name) catch return error.OutOfMemory;
            fields.append(self.allocator, field_name) catch return error.OutOfMemory;
            current = field_cons.cdr;
        }

        return .{
            .name = name,
            .fields = fields.toOwnedSlice(self.allocator) catch return error.OutOfMemory,
        };
    }

    fn generateAdtConstructor(self: *Compiler, variant: Variant) CompileError!*Ir {
        // Creates: (defun variant-name (f1 f2 ...) (vector :variant-name f1 f2 ...))
        const idx = self.globals.define(variant.name) catch return error.OutOfMemory;

        // Build lambda body: (vector :variant-name f1 f2 ...)
        const num_elems = variant.fields.len + 1; // tag + fields
        var elems = self.allocator.alloc(*const Ir, num_elems) catch return error.OutOfMemory;

        // First element: keyword tag
        const tag_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        tag_node.* = .{ .quote_sym = variant.name };
        elems[0] = tag_node;

        // Rest: variable references to parameters
        for (variant.fields, 0..) |field, i| {
            const var_node = self.builder.variable(field, 0, @intCast(i)) catch return error.OutOfMemory;
            elems[i + 1] = var_node;
        }

        const vec_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        vec_node.* = .{ .vec = elems };

        // Build lambda
        const params = self.allocator.dupe([]const u8, variant.fields) catch return error.OutOfMemory;
        const lambda_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        lambda_node.* = .{ .lambda = .{
            .params = params,
            .rest_param = null,
            .captures = &[_]Ir.Capture{},
            .body = vec_node,
        } };

        return self.builder.define(variant.name, idx, lambda_node) catch return error.OutOfMemory;
    }

    fn generateVariantPredicate(self: *Compiler, variant: Variant) CompileError!*Ir {
        // Creates: (defun variant-name? (x) (and (vectorp x) (eq (aref x 0) :variant-name)))
        var name_buf: [256]u8 = undefined;
        const pred_name = std.fmt.bufPrint(&name_buf, "{s}?", .{variant.name}) catch return error.OutOfMemory;
        const pred_name_copy = self.allocator.dupe(u8, pred_name) catch return error.OutOfMemory;

        const idx = self.globals.define(pred_name_copy) catch return error.OutOfMemory;

        // Build: (and (vectorp x) (eq (aref x 0) :variant-name))
        // Param x at index 0
        const x_var = self.builder.variable("x", 0, 0) catch return error.OutOfMemory;

        // (vectorp x)
        const vectorp_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        vectorp_node.* = .{ .vectorp = .{ .operand = x_var } };

        // (aref x 0)
        const x_var2 = self.builder.variable("x", 0, 0) catch return error.OutOfMemory;
        const zero = self.builder.lit(Value.makeFixnum(0)) catch return error.OutOfMemory;
        const aref_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        aref_node.* = .{ .vec_ref = .{ .left = x_var2, .right = zero } };

        // :variant-name (as quoted symbol)
        const tag_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        tag_node.* = .{ .quote_sym = variant.name };

        // (eq (aref x 0) :variant-name)
        const eq_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        eq_node.* = .{ .eq = .{ .left = aref_node, .right = tag_node } };

        // (if (vectorp x) (eq ...) nil)
        const nil_node = self.builder.lit(Value.nil) catch return error.OutOfMemory;
        const if_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        if_node.* = .{ .@"if" = .{
            .cond = vectorp_node,
            .then_branch = eq_node,
            .else_branch = nil_node,
        } };

        // Lambda wrapper
        const params = self.allocator.alloc([]const u8, 1) catch return error.OutOfMemory;
        params[0] = "x";
        const lambda_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        lambda_node.* = .{ .lambda = .{
            .params = params,
            .rest_param = null,
            .captures = &[_]Ir.Capture{},
            .body = if_node,
        } };

        return self.builder.define(pred_name_copy, idx, lambda_node) catch return error.OutOfMemory;
    }

    fn generateFieldAccessor(self: *Compiler, variant_name: []const u8, field_name: []const u8, field_idx: u16) CompileError!*Ir {
        // Creates: (defun variant-name-field (x) (aref x field-idx))
        var name_buf: [256]u8 = undefined;
        const accessor_name = std.fmt.bufPrint(&name_buf, "{s}-{s}", .{ variant_name, field_name }) catch return error.OutOfMemory;
        const accessor_name_copy = self.allocator.dupe(u8, accessor_name) catch return error.OutOfMemory;

        const idx = self.globals.define(accessor_name_copy) catch return error.OutOfMemory;

        // Build: (aref x field-idx)
        const x_var = self.builder.variable("x", 0, 0) catch return error.OutOfMemory;
        const idx_lit = self.builder.lit(Value.makeFixnum(@intCast(field_idx))) catch return error.OutOfMemory;
        const aref_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        aref_node.* = .{ .vec_ref = .{ .left = x_var, .right = idx_lit } };

        // Lambda wrapper
        const params = self.allocator.alloc([]const u8, 1) catch return error.OutOfMemory;
        params[0] = "x";
        const lambda_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        lambda_node.* = .{ .lambda = .{
            .params = params,
            .rest_param = null,
            .captures = &[_]Ir.Capture{},
            .body = aref_node,
        } };

        return self.builder.define(accessor_name_copy, idx, lambda_node) catch return error.OutOfMemory;
    }

    fn generateTypePredicate(self: *Compiler, type_name: []const u8, variants: []const Variant) CompileError!*Ir {
        // Creates: (defun type-name? (x) (or (variant1? x) (variant2? x) ...))
        var name_buf: [256]u8 = undefined;
        const pred_name = std.fmt.bufPrint(&name_buf, "{s}?", .{type_name}) catch return error.OutOfMemory;
        const pred_name_copy = self.allocator.dupe(u8, pred_name) catch return error.OutOfMemory;

        const idx = self.globals.define(pred_name_copy) catch return error.OutOfMemory;

        // Build body: chain of or's checking each variant
        // Start from the last variant and work backwards
        var body: *Ir = self.builder.lit(Value.nil) catch return error.OutOfMemory;

        var i: usize = variants.len;
        while (i > 0) {
            i -= 1;
            const variant = variants[i];

            // Build variant check inline (like variant-name? but without function call)
            const x_var = self.builder.variable("x", 0, 0) catch return error.OutOfMemory;
            const vectorp_node = self.allocator.create(Ir) catch return error.OutOfMemory;
            vectorp_node.* = .{ .vectorp = .{ .operand = x_var } };

            const x_var2 = self.builder.variable("x", 0, 0) catch return error.OutOfMemory;
            const zero = self.builder.lit(Value.makeFixnum(0)) catch return error.OutOfMemory;
            const aref_node = self.allocator.create(Ir) catch return error.OutOfMemory;
            aref_node.* = .{ .vec_ref = .{ .left = x_var2, .right = zero } };

            const tag_node = self.allocator.create(Ir) catch return error.OutOfMemory;
            tag_node.* = .{ .quote_sym = variant.name };

            const eq_node = self.allocator.create(Ir) catch return error.OutOfMemory;
            eq_node.* = .{ .eq = .{ .left = aref_node, .right = tag_node } };

            const nil_node = self.builder.lit(Value.nil) catch return error.OutOfMemory;
            const check_node = self.allocator.create(Ir) catch return error.OutOfMemory;
            check_node.* = .{ .@"if" = .{
                .cond = vectorp_node,
                .then_branch = eq_node,
                .else_branch = nil_node,
            } };

            // (if check_node t body)
            const t_val = self.builder.lit(Value.t) catch return error.OutOfMemory;
            const or_node = self.allocator.create(Ir) catch return error.OutOfMemory;
            or_node.* = .{ .@"if" = .{
                .cond = check_node,
                .then_branch = t_val,
                .else_branch = body,
            } };
            body = or_node;
        }

        // Lambda wrapper
        const params = self.allocator.alloc([]const u8, 1) catch return error.OutOfMemory;
        params[0] = "x";
        const lambda_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        lambda_node.* = .{ .lambda = .{
            .params = params,
            .rest_param = null,
            .captures = &[_]Ir.Capture{},
            .body = body,
        } };

        return self.builder.define(pred_name_copy, idx, lambda_node) catch return error.OutOfMemory;
    }

    /// Compile match: (match expr ((variant1 f1 f2) body1) ((variant2 f3) body2) (_ default))
    fn compileMatch(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // Parse: (expr clause1 clause2 ...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const scrutinee = try self.compile(cons1.car, env);

        const clauses = cons1.cdr;

        // Exhaustiveness checking: collect variant names from clauses
        self.checkMatchExhaustiveness(clauses);

        return self.compileMatchClauses(scrutinee, clauses, env);
    }

    /// Check if match covers all variants of the ADT (warning only, doesn't fail)
    fn checkMatchExhaustiveness(self: *Compiler, clauses: Value) void {
        var has_wildcard = false;
        var covered = std.StringHashMap(void).init(self.allocator);
        defer covered.deinit();

        // Scan clauses to find variant names and wildcards
        var current = clauses;
        while (current.isCons()) {
            const clause_cons = current.toPtr(Cons);
            const clause = clause_cons.car;

            if (!clause.isCons()) {
                current = clause_cons.cdr;
                continue;
            }

            const pattern_cons = clause.toPtr(Cons);
            const pattern = pattern_cons.car;

            // Check for wildcard
            if (pattern.isSymbol()) {
                const sym = pattern.toPtr(Symbol);
                if (std.mem.eql(u8, sym.getName(), "_")) {
                    has_wildcard = true;
                    break;
                }
            }

            // Extract variant name from pattern (variant-name field1 field2 ...)
            if (pattern.isCons()) {
                const variant_cons = pattern.toPtr(Cons);
                if (variant_cons.car.isSymbol()) {
                    const variant_name = variant_cons.car.toPtr(Symbol).getName();
                    covered.put(variant_name, {}) catch {};
                }
            }

            current = clause_cons.cdr;
        }

        if (has_wildcard) return; // Wildcard covers everything

        // Find the ADT type from the first variant
        var type_variants: ?[]const Variant = null;
        var iter = covered.keyIterator();
        while (iter.next()) |variant_name| {
            // Search all defined types for this variant
            var type_iter = self.defined_types.iterator();
            while (type_iter.next()) |entry| {
                for (entry.value_ptr.*) |v| {
                    if (std.mem.eql(u8, v.name, variant_name.*)) {
                        type_variants = entry.value_ptr.*;
                        break;
                    }
                }
                if (type_variants != null) break;
            }
            if (type_variants != null) break;
        }

        if (type_variants) |variants| {
            // Check which variants are missing
            for (variants) |v| {
                if (!covered.contains(v.name)) {
                    std.log.warn("match: missing case for variant '{s}'", .{v.name});
                }
            }
        }
    }

    fn compileMatchClauses(self: *Compiler, scrutinee: *const Ir, clauses: Value, env: *const Env) CompileError!*Ir {
        if (!clauses.isCons()) {
            // No more clauses - return nil (or could be error for non-exhaustive)
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        const clause_cons = clauses.toPtr(Cons);
        const clause = clause_cons.car;

        if (!clause.isCons()) return error.InvalidSyntax;
        const pattern_cons = clause.toPtr(Cons);
        const pattern = pattern_cons.car;
        const body_list = pattern_cons.cdr;

        // Check for wildcard pattern: _
        if (pattern.isSymbol()) {
            const sym = pattern.toPtr(Symbol);
            if (std.mem.eql(u8, sym.getName(), "_")) {
                // Wildcard - compile body as progn
                return self.compileProgn(body_list, env);
            }
        }

        // Pattern: (variant-name field1 field2 ...)
        if (!pattern.isCons()) return error.InvalidSyntax;
        const variant_cons = pattern.toPtr(Cons);
        if (!variant_cons.car.isSymbol()) return error.InvalidSyntax;
        const variant_name = variant_cons.car.toPtr(Symbol).getName();

        // Collect field bindings
        var field_names = std.ArrayList([]const u8){};
        var field_current = variant_cons.cdr;
        while (field_current.isCons()) {
            const fc = field_current.toPtr(Cons);
            if (!fc.car.isSymbol()) return error.InvalidSyntax;
            const field_name = fc.car.toPtr(Symbol).getName();
            field_names.append(self.allocator, field_name) catch return error.OutOfMemory;
            field_current = fc.cdr;
        }

        // Build condition: (and (vectorp scrutinee) (eq (aref scrutinee 0) :variant-name))
        const vectorp_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        vectorp_node.* = .{ .vectorp = .{ .operand = scrutinee } };

        const zero = self.builder.lit(Value.makeFixnum(0)) catch return error.OutOfMemory;
        const aref_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        aref_node.* = .{ .vec_ref = .{ .left = scrutinee, .right = zero } };

        const tag_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        tag_node.* = .{ .quote_sym = variant_name };

        const eq_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        eq_node.* = .{ .eq = .{ .left = aref_node, .right = tag_node } };

        const nil_lit = self.builder.lit(Value.nil) catch return error.OutOfMemory;
        const cond_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        cond_node.* = .{ .@"if" = .{
            .cond = vectorp_node,
            .then_branch = eq_node,
            .else_branch = nil_lit,
        } };

        // Build then branch: (let ((f1 (aref scrutinee 1)) (f2 (aref scrutinee 2)) ...) body...)
        var bindings = self.allocator.alloc(Ir.Binding, field_names.items.len) catch return error.OutOfMemory;
        for (field_names.items, 0..) |field_name, i| {
            const idx_lit = self.builder.lit(Value.makeFixnum(@intCast(i + 1))) catch return error.OutOfMemory;
            const field_aref = self.allocator.create(Ir) catch return error.OutOfMemory;
            field_aref.* = .{ .vec_ref = .{ .left = scrutinee, .right = idx_lit } };

            bindings[i] = .{
                .name = field_name,
                .value = field_aref,
            };
        }

        // Compile body in extended environment
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();
        for (field_names.items) |field_name| {
            _ = let_env.bind(field_name) catch return error.OutOfMemory;
        }
        const body_ir = try self.compileProgn(body_list, &let_env);

        var then_ir: *Ir = undefined;
        if (bindings.len > 0) {
            then_ir = self.allocator.create(Ir) catch return error.OutOfMemory;
            then_ir.* = .{ .let = .{
                .bindings = bindings,
                .body = body_ir,
            } };
        } else {
            then_ir = body_ir;
        }

        // Compile else branch (remaining clauses)
        const else_ir = try self.compileMatchClauses(scrutinee, clause_cons.cdr, env);

        // Build if node
        const if_node = self.allocator.create(Ir) catch return error.OutOfMemory;
        if_node.* = .{ .@"if" = .{
            .cond = cond_node,
            .then_branch = then_ir,
            .else_branch = else_ir,
        } };

        return if_node;
    }

    fn compileLambdaWithReturnType(self: *Compiler, args: Value, env: *const Env, return_type: ?[]const u8) CompileError!*Ir {
        // Delegate to compileLambda but wrap result if return type specified
        const lambda_ir = try self.compileLambdaCore(args, env, return_type);
        return lambda_ir;
    }

    /// Compile type assertion: (the type expr)
    /// Supported types: fixnum, cons, symbol, string, vector, closure, non-nil
    /// Uses occurrence typing: skips check if variable already narrowed to type
    fn compileThe(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (the type expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const type_spec = cons1.car;
        const rest = cons1.cdr;

        if (!rest.isCons()) return error.InvalidSyntax;
        const cons2 = rest.toPtr(Cons);
        const expr = cons2.car;

        // Compile the expression first
        const expr_ir = try self.compile(expr, env);

        // Handle compound type specs: (or type1 type2 ...)
        if (type_spec.isCons()) {
            return self.compileCompoundTypeCheck(type_spec, expr_ir);
        }

        // Type must be a symbol for simple types
        if (!type_spec.isSymbol()) return error.InvalidSyntax;
        const type_sym = type_spec.toPtr(Symbol);
        const type_name = type_sym.getName();

        // Check occurrence typing: if expr is a variable narrowed to this type, skip check
        if (expr.isSymbol()) {
            const var_sym = expr.toPtr(Symbol);
            const var_name = var_sym.getName();

            if (self.occ) |occ| {
                if (occ.getNarrowed(var_name)) |narrowed_type| {
                    // Check if narrowed type matches requested type
                    if (self.typeMatchesName(narrowed_type, type_name)) {
                        // Already narrowed - just compile the expression, skip the check
                        return expr_ir;
                    }
                }
            }
        }

        return self.compileSimpleTypeCheck(type_name, expr_ir);
    }

    /// Compile a simple type check for a single type name
    fn compileSimpleTypeCheck(self: *Compiler, type_name: []const u8, expr_ir: *const Ir) CompileError!*Ir {
        // Map type name to assertion IR
        if (std.mem.eql(u8, type_name, "fixnum")) {
            return self.builder.assertFixnum(expr_ir) catch return error.OutOfMemory;
        }
        if (std.mem.eql(u8, type_name, "cons")) {
            return self.builder.assertCons(expr_ir) catch return error.OutOfMemory;
        }
        if (std.mem.eql(u8, type_name, "symbol")) {
            return self.builder.assertSymbol(expr_ir) catch return error.OutOfMemory;
        }
        if (std.mem.eql(u8, type_name, "string")) {
            return self.builder.assertString(expr_ir) catch return error.OutOfMemory;
        }
        if (std.mem.eql(u8, type_name, "vector")) {
            return self.builder.assertVector(expr_ir) catch return error.OutOfMemory;
        }
        if (std.mem.eql(u8, type_name, "closure")) {
            return self.builder.assertClosure(expr_ir) catch return error.OutOfMemory;
        }
        if (std.mem.eql(u8, type_name, "non-nil")) {
            return self.builder.assertNonNil(expr_ir) catch return error.OutOfMemory;
        }
        if (std.mem.eql(u8, type_name, "list")) {
            return self.builder.assertList(expr_ir) catch return error.OutOfMemory;
        }
        if (std.mem.eql(u8, type_name, "any")) {
            // No check needed for any type
            return @constCast(expr_ir);
        }

        // Unknown type
        return error.InvalidSyntax;
    }

    /// Compile a compound type check: (or type1 type2 ...)
    fn compileCompoundTypeCheck(self: *Compiler, type_spec: Value, expr_ir: *const Ir) CompileError!*Ir {
        const cons = type_spec.toPtr(Cons);
        if (!cons.car.isSymbol()) return error.InvalidSyntax;

        const head_sym = cons.car.toPtr(Symbol);
        const head_name = head_sym.getName();

        if (std.mem.eql(u8, head_name, "or")) {
            return self.compileOrTypeCheck(cons.cdr, expr_ir);
        }

        // Unknown compound type
        return error.InvalidSyntax;
    }

    /// Compile (or type1 type2 ...) check
    /// Expands to: check_list if (or cons nil), else check each type
    fn compileOrTypeCheck(self: *Compiler, type_list: Value, expr_ir: *const Ir) CompileError!*Ir {
        // Collect type names (symbols are names, nil value means "nil" type)
        var type_names = std.ArrayList([]const u8){};
        defer type_names.deinit(self.allocator);

        var list = type_list;
        while (list.isCons()) {
            const c = list.toPtr(Cons);
            if (c.car.isSymbol()) {
                const type_sym = c.car.toPtr(Symbol);
                type_names.append(self.allocator, type_sym.getName()) catch return error.OutOfMemory;
            } else if (c.car.isNil()) {
                // nil value in type position means the nil type
                type_names.append(self.allocator, "nil") catch return error.OutOfMemory;
            } else {
                return error.InvalidSyntax;
            }
            list = c.cdr;
        }

        if (type_names.items.len == 0) return error.InvalidSyntax;

        // Special case: (or cons nil) or (or nil cons) -> list
        if (type_names.items.len == 2) {
            const has_cons = for (type_names.items) |n| {
                if (std.mem.eql(u8, n, "cons")) break true;
            } else false;
            const has_nil = for (type_names.items) |n| {
                if (std.mem.eql(u8, n, "nil")) break true;
            } else false;
            if (has_cons and has_nil) {
                return self.builder.assertList(expr_ir) catch return error.OutOfMemory;
            }
        }

        // For other or-types, we need runtime checks with predicates
        // This is more complex - for now, return the expression without check
        // TODO: Implement full or-type checking
        return @constCast(expr_ir);
    }

    /// Check if a Type matches a type name string
    fn typeMatchesName(self: *Compiler, ty: *const Type, name: []const u8) bool {
        _ = self;
        switch (ty.*) {
            .primitive => |p| return std.mem.eql(u8, p.name(), name),
            .non_nil => return std.mem.eql(u8, name, "non-nil"),
            else => return false,
        }
    }

    fn compileBody(self: *Compiler, exprs: Value, env: *const Env) CompileError!*Ir {
        return self.compileBodyWithTail(exprs, env, false);
    }

    fn compileBodyWithTail(self: *Compiler, exprs: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        if (exprs.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        // Count expressions first to know which is last
        var count: usize = 0;
        var tmp = exprs;
        while (tmp.isCons()) {
            count += 1;
            tmp = tmp.toPtr(Cons).cdr;
        }

        var expr_list = std.ArrayList(*Ir){};
        defer expr_list.deinit(self.allocator);

        var list = exprs;
        var idx: usize = 0;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const is_last = idx == count - 1;
            // Only last expression is in tail position
            const expr_ir = try self.compileWithTail(cons.car, env, in_tail and is_last);
            expr_list.append(self.allocator, expr_ir) catch return error.OutOfMemory;
            list = cons.cdr;
            idx += 1;
        }

        if (expr_list.items.len == 1) {
            return expr_list.items[0];
        }

        // Convert to const slice for progn
        const items = self.allocator.dupe(*const Ir, expr_list.items) catch
            return error.OutOfMemory;
        return self.builder.progn(items) catch return error.OutOfMemory;
    }

    fn compilePrimitive(self: *Compiler, sym: Value, args: Value, env: *const Env) CompileError!*Ir {
        const s = sym.raw;
        const b = self.builtins orelse return error.InvalidSyntax;

        // Variadic arithmetic (+, -, *, /)
        if (s == b.@"+".raw) return self.compileVariadicArith(args, env, .add, 0);
        if (s == b.@"-".raw) return self.compileVariadicArith(args, env, .sub, null);
        if (s == b.@"*".raw) return self.compileVariadicArith(args, env, .mul, 1);
        if (s == b.@"/".raw) return self.compileVariadicArith(args, env, .div, null);
        if (s == b.mod.raw or s == b.@"%".raw) return self.compileBinaryPrim(args, env, .mod);

        // Comparison
        if (s == b.eq.raw) return self.compileBinaryPrim(args, env, .eq);
        if (s == b.equal.raw) return self.compileBinaryPrim(args, env, .equal);
        if (s == b.eql.raw) return self.compileBinaryPrim(args, env, .eql);
        if (s == b.@"<".raw) return self.compileBinaryPrim(args, env, .lt);
        if (s == b.@">".raw) return self.compileBinaryPrim(args, env, .gt);
        if (s == b.@"<=".raw) return self.compileBinaryPrim(args, env, .le);
        if (s == b.@">=".raw) return self.compileBinaryPrim(args, env, .ge);
        if (s == b.@"=".raw) return self.compileBinaryPrim(args, env, .num_eq);

        // List operations
        if (s == b.cons.raw) return self.compileBinaryPrim(args, env, .cons);
        if (s == b.car.raw or s == b.first.raw) return self.compileUnaryPrim(args, env, .car);
        if (s == b.cdr.raw or s == b.rest.raw) return self.compileUnaryPrim(args, env, .cdr);
        // Composed accessors (2-level)
        if (s == b.caar.raw) return self.compileComposedAccessor(args, env, "aa");
        if (s == b.cadr.raw or s == b.second.raw) return self.compileComposedAccessor(args, env, "ad");
        if (s == b.cdar.raw) return self.compileComposedAccessor(args, env, "da");
        if (s == b.cddr.raw) return self.compileComposedAccessor(args, env, "dd");
        // Composed accessors (3-level)
        if (s == b.caaar.raw) return self.compileComposedAccessor(args, env, "aaa");
        if (s == b.caadr.raw) return self.compileComposedAccessor(args, env, "aad");
        if (s == b.cadar.raw) return self.compileComposedAccessor(args, env, "ada");
        if (s == b.caddr.raw or s == b.third.raw) return self.compileComposedAccessor(args, env, "add");
        if (s == b.cdaar.raw) return self.compileComposedAccessor(args, env, "daa");
        if (s == b.cdadr.raw) return self.compileComposedAccessor(args, env, "dad");
        if (s == b.cddar.raw) return self.compileComposedAccessor(args, env, "dda");
        if (s == b.cdddr.raw) return self.compileComposedAccessor(args, env, "ddd");
        // fourth = (car (cdr (cdr (cdr x)))) = cadddr
        if (s == b.fourth.raw) return self.compileComposedAccessor(args, env, "addd");
        if (s == b.append.raw) return self.compileBinaryPrim(args, env, .append);
        if (s == b.length.raw) return self.compileUnaryPrim(args, env, .length);
        if (s == b.reverse.raw) return self.compileUnaryPrim(args, env, .reverse);
        if (s == b.nth.raw) return self.compileBinaryPrim(args, env, .nth);
        if (s == b.nthcdr.raw) return self.compileBinaryPrim(args, env, .nthcdr);
        if (s == b.last.raw) return self.compileUnaryPrim(args, env, .last);
        if (s == b.member.raw) return self.compileBinaryPrim(args, env, .member);
        if (s == b.assoc.raw) return self.compileBinaryPrim(args, env, .assoc);
        if (s == b.list.raw) return self.compileListPrim(args, env);
        if (s == b.rplaca.raw) return self.compileBinaryPrim(args, env, .rplaca);
        if (s == b.rplacd.raw) return self.compileBinaryPrim(args, env, .rplacd);

        // Type predicates
        if (s == b.consp.raw or s == b.@"cons?".raw) return self.compileUnaryPrim(args, env, .consp);
        if (s == b.symbolp.raw or s == b.@"symbol?".raw) return self.compileUnaryPrim(args, env, .symbolp);
        if (s == b.numberp.raw or s == b.@"number?".raw) return self.compileUnaryPrim(args, env, .numberp);
        if (s == b.stringp.raw or s == b.@"string?".raw) return self.compileUnaryPrim(args, env, .stringp);
        if (s == b.vectorp.raw or s == b.@"vector?".raw) return self.compileUnaryPrim(args, env, .vectorp);
        if (s == b.closurep.raw or s == b.@"closure?".raw) return self.compileUnaryPrim(args, env, .closurep);
        if (s == b.keywordp.raw or s == b.@"keyword?".raw) return self.compileUnaryPrim(args, env, .keywordp);
        if (s == b.null.raw or s == b.@"null?".raw) return self.compileUnaryPrim(args, env, .nilp);
        if (s == b.not.raw) return self.compileUnaryPrim(args, env, .not);
        if (s == b.characterp.raw or s == b.@"character?".raw) return self.compileUnaryPrim(args, env, .characterp);
        if (s == b.floatp.raw or s == b.@"float?".raw) return self.compileUnaryPrim(args, env, .floatp);
        if (s == b.listp.raw or s == b.@"list?".raw) return self.compileUnaryPrim(args, env, .listp);
        if (s == b.atom.raw) return self.compileUnaryPrim(args, env, .atom);

        // Character operations
        if (s == b.@"char-code".raw) return self.compileUnaryPrim(args, env, .char_code);
        if (s == b.@"code-char".raw) return self.compileUnaryPrim(args, env, .code_char);
        if (s == b.@"char=".raw) return self.compileBinaryPrim(args, env, .char_eq);
        if (s == b.@"char<".raw) return self.compileBinaryPrim(args, env, .char_lt);
        if (s == b.@"char>".raw) return self.compileBinaryPrim(args, env, .char_gt);
        if (s == b.@"read-char".raw) return self.compileNullaryPrim(.read_char);
        if (s == b.@"peek-char".raw) return self.compileNullaryPrim(.peek_char);
        if (s == b.read.raw) return self.compileNullaryPrim(.read);
        if (s == b.@"read-from-string".raw) return self.compileUnaryPrim(args, env, .read_from_string);
        if (s == b.load.raw) return self.compileUnaryPrim(args, env, .load);
        if (s == b.@"unread-char".raw) return self.compileUnaryPrim(args, env, .unread_char);
        if (s == b.eval.raw) return self.compileUnaryPrim(args, env, .eval);
        if (s == b.gensym.raw) return self.compileNullaryPrim(.gensym);
        if (s == b.macroexpand.raw) return self.compileUnaryPrim(args, env, .macroexpand);

        // Symbol operations
        if (s == b.boundp.raw) return self.compileUnaryPrim(args, env, .boundp);
        if (s == b.fboundp.raw) return self.compileUnaryPrim(args, env, .fboundp);
        if (s == b.@"symbol-value".raw) return self.compileUnaryPrim(args, env, .symbol_value);
        if (s == b.@"symbol-function".raw) return self.compileUnaryPrim(args, env, .symbol_function);
        if (s == b.typep.raw) return self.compileBinaryPrim(args, env, .typep);
        if (s == b.@"type-of".raw) return self.compileUnaryPrim(args, env, .type_of);
        if (s == b.intern.raw) return self.compileUnaryPrim(args, env, .intern);
        if (s == b.@"symbol-name".raw) return self.compileUnaryPrim(args, env, .sym_name);
        if (s == b.@"error".raw) return self.compileUnaryPrim(args, env, .error_user);

        // Numeric predicates
        if (s == b.abs.raw) return self.compileUnaryPrim(args, env, .abs);
        if (s == b.zerop.raw) return self.compileUnaryPrim(args, env, .zerop);
        if (s == b.plusp.raw) return self.compileUnaryPrim(args, env, .plusp);
        if (s == b.minusp.raw) return self.compileUnaryPrim(args, env, .minusp);
        if (s == b.evenp.raw) return self.compileUnaryPrim(args, env, .evenp);
        if (s == b.oddp.raw) return self.compileUnaryPrim(args, env, .oddp);

        // Vector operations (CL names: aref, svref, %svset)
        if (s == b.aref.raw or s == b.svref.raw) return self.compileBinaryPrim(args, env, .vec_ref);
        if (s == b.@"vector-length".raw) return self.compileUnaryPrim(args, env, .vec_len);
        if (s == b.@"make-vector".raw) return self.compileMakeVector(args, env);
        if (s == b.@"%svset".raw) return self.compileSvset(args, env);
        if (s == b.vector.raw) return self.compileVectorPrim(args, env);

        // String operations (CL names: char, schar)
        if (s == b.char.raw or s == b.schar.raw) return self.compileBinaryPrim(args, env, .str_ref);
        if (s == b.@"string-length".raw) return self.compileUnaryPrim(args, env, .str_len);
        if (s == b.@"string=".raw) return self.compileBinaryPrim(args, env, .str_eq);
        if (s == b.@"string-concat".raw) return self.compileBinaryPrim(args, env, .str_concat);
        if (s == b.substring.raw) return self.compileSubstring(args, env);
        if (s == b.subseq.raw) return self.compileSubseq(args, env);

        // I/O
        if (s == b.print.raw) return self.compileUnaryPrim(args, env, .print);
        if (s == b.princ.raw) return self.compileUnaryPrim(args, env, .princ);
        if (s == b.terpri.raw) return self.compileNullaryPrim(.terpri);
        if (s == b.@"write-char".raw) return self.compileUnaryPrim(args, env, .write_char);
        if (s == b.format.raw) return self.compileFormat(args, env);

        // Character functions
        if (s == b.@"char-upcase".raw) return self.compileUnaryPrim(args, env, .char_upcase);
        if (s == b.@"char-downcase".raw) return self.compileUnaryPrim(args, env, .char_downcase);
        if (s == b.@"digit-char-p".raw) return self.compileUnaryPrim(args, env, .digit_char_p);
        if (s == b.@"alpha-char-p".raw) return self.compileUnaryPrim(args, env, .alpha_char_p);

        // String/number conversion
        if (s == b.@"parse-integer".raw) return self.compileUnaryPrim(args, env, .parse_integer);
        if (s == b.@"write-to-string".raw) return self.compileUnaryPrim(args, env, .write_to_string);

        // Bitwise operations
        if (s == b.logand.raw) return self.compileBinaryPrim(args, env, .logand);
        if (s == b.logior.raw) return self.compileBinaryPrim(args, env, .logior);
        if (s == b.logxor.raw) return self.compileBinaryPrim(args, env, .logxor);
        if (s == b.lognot.raw) return self.compileUnaryPrim(args, env, .lognot);
        if (s == b.ash.raw) return self.compileBinaryPrim(args, env, .ash);

        // File I/O
        if (s == b.@"read-file".raw) return self.compileUnaryPrim(args, env, .read_file);
        if (s == b.@"write-file".raw) return self.compileBinaryPrim(args, env, .write_file);

        // String construction
        if (s == b.@"make-string".raw) return self.compileBinaryPrim(args, env, .make_string);
        if (s == b.@"string-to-list".raw) return self.compileUnaryPrim(args, env, .string_to_list);
        if (s == b.@"list-to-string".raw) return self.compileUnaryPrim(args, env, .list_to_string);
        if (s == b.@"string-upcase".raw) return self.compileUnaryPrim(args, env, .string_upcase);
        if (s == b.@"string-downcase".raw) return self.compileUnaryPrim(args, env, .string_downcase);
        if (s == b.concatenate.raw) return self.compileConcatenate(args, env);
        if (s == b.coerce.raw) return self.compileCoerce(args, env);

        // Hash tables
        if (s == b.@"make-hash-table".raw) return self.compileMakeHash(args);
        if (s == b.gethash.raw) return self.compileGethash(args, env);
        if (s == b.puthash.raw) return self.compileSethash(args, env);
        if (s == b.remhash.raw) return self.compileRemhash(args, env);
        if (s == b.@"hash-table-count".raw) return self.compileHashTableCount(args, env);
        if (s == b.@"hash-table-p".raw) return self.compileHashTableP(args, env);

        // Random
        if (s == b.random.raw) return self.compileUnaryPrim(args, env, .random);

        return error.InvalidSyntax; // Not a known primitive
    }

    const PrimTag = enum { add, sub, mul, div, mod, eq, equal, eql, lt, gt, le, ge, num_eq, cons, car, cdr, append, length, reverse, nth, nthcdr, last, member, assoc, rplaca, rplacd, consp, symbolp, numberp, stringp, vectorp, closurep, keywordp, nilp, not, vec_ref, vec_len, make_box, box_ref, box_set, str_ref, str_len, str_eq, str_concat, print, princ, terpri, write_char, random, intern, sym_name, type_of, error_user, characterp, floatp, listp, atom, char_code, code_char, char_eq, char_lt, char_gt, char_upcase, char_downcase, digit_char_p, alpha_char_p, read_char, peek_char, read, read_from_string, load, unread_char, eval, gensym, macroexpand, parse_integer, write_to_string, logand, logior, logxor, lognot, ash, read_file, write_file, make_string, string_to_list, list_to_string, string_upcase, string_downcase, boundp, fboundp, symbol_value, symbol_function, typep, abs, zerop, plusp, minusp, evenp, oddp };

    /// Compile variadic arithmetic: +, -, *, /
    /// identity: for + (0), * (1). null means no identity (- and / need args)
    fn compileVariadicArith(self: *Compiler, args: Value, env: *const Env, op: PrimTag, identity: ?i64) CompileError!*Ir {
        // Collect args
        var arg_list = std.ArrayList(*Ir){};
        defer arg_list.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const c = current.toPtr(Cons);
            const compiled = try self.compile(c.car, env);
            arg_list.append(self.allocator, compiled) catch return error.OutOfMemory;
            current = c.cdr;
        }

        const arg_count = arg_list.items.len;

        // Handle different arities
        if (arg_count == 0) {
            // (+) -> 0, (*) -> 1, (-) and (/) are errors
            if (identity) |id| {
                return self.builder.lit(Value.makeFixnum(id)) catch return error.OutOfMemory;
            }
            return error.InvalidSyntax;
        }

        if (arg_count == 1) {
            // (+ x) -> x, (* x) -> x
            // (- x) -> (- 0 x), (/ x) -> (/ 1 x)
            if (op == .sub) {
                const zero = self.builder.lit(Value.makeFixnum(0)) catch return error.OutOfMemory;
                return self.builder.sub(zero, arg_list.items[0]) catch return error.OutOfMemory;
            }
            if (op == .div) {
                const one = self.builder.lit(Value.makeFixnum(1)) catch return error.OutOfMemory;
                return self.builder.div(one, arg_list.items[0]) catch return error.OutOfMemory;
            }
            return arg_list.items[0];
        }

        // 2+ args: fold left
        var result = arg_list.items[0];
        for (arg_list.items[1..]) |arg| {
            result = switch (op) {
                .add => self.builder.add(result, arg),
                .sub => self.builder.sub(result, arg),
                .mul => self.builder.mul(result, arg),
                .div => self.builder.div(result, arg),
                else => unreachable,
            } catch return error.OutOfMemory;
        }
        return result;
    }

    fn compileBinaryPrim(self: *Compiler, args: Value, env: *const Env, prim: PrimTag) CompileError!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const left = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const right = try self.compile(cons2.car, env);

        return switch (prim) {
            .add => self.builder.add(left, right),
            .sub => self.builder.sub(left, right),
            .mul => self.builder.mul(left, right),
            .div => self.builder.div(left, right),
            .mod => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .mod = .{ .left = left, .right = right } };
                break :blk node;
            },
            .eq => self.builder.eq(left, right),
            .equal => self.builder.equal(left, right),
            .eql => self.builder.eql(left, right),
            .lt => self.builder.lt(left, right),
            .gt => self.builder.gt(left, right),
            .le => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .le = .{ .left = left, .right = right } };
                break :blk node;
            },
            .ge => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .ge = .{ .left = left, .right = right } };
                break :blk node;
            },
            .num_eq => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .num_eq = .{ .left = left, .right = right } };
                break :blk node;
            },
            .cons => self.builder.cons(left, right),
            .vec_ref => self.builder.vecRef(left, right),
            .box_set => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .box_set = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_ref => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .str_ref = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_eq => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .str_eq = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_concat => self.builder.strConcat(left, right),
            .char_eq => self.builder.charEq(left, right),
            .char_lt => self.builder.charLt(left, right),
            .char_gt => self.builder.charGt(left, right),
            .typep => self.builder.typep(left, right),
            .append => self.builder.append(left, right),
            .nth => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .nth = .{ .left = left, .right = right } };
                break :blk node;
            },
            .nthcdr => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .nthcdr = .{ .left = left, .right = right } };
                break :blk node;
            },
            .member => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .member = .{ .left = left, .right = right } };
                break :blk node;
            },
            .assoc => self.builder.assoc(left, right),
            .logand => self.builder.logand(left, right),
            .logior => self.builder.logior(left, right),
            .logxor => self.builder.logxor(left, right),
            .ash => self.builder.ash(left, right),
            .write_file => self.builder.writeFile(left, right),
            .make_string => self.builder.makeString(left, right),
            .rplaca => self.builder.rplaca(left, right),
            .rplacd => self.builder.rplacd(left, right),
            else => error.InvalidSyntax,
        } catch return error.OutOfMemory;
    }

    fn compileUnaryPrim(self: *Compiler, args: Value, env: *const Env, prim: PrimTag) CompileError!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const operand = try self.compile(cons.car, env);

        return switch (prim) {
            .car => self.builder.car(operand),
            .cdr => self.builder.cdr(operand),
            .consp => self.builder.consp(operand),
            .symbolp => self.builder.symbolp(operand),
            .numberp => self.builder.numberp(operand),
            .nilp => self.builder.nilp(operand),
            .not => self.builder.not(operand),
            .vec_len => self.builder.vecLen(operand),
            .make_box => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .make_box = .{ .operand = operand } };
                break :blk node;
            },
            .box_ref => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .box_ref = .{ .operand = operand } };
                break :blk node;
            },
            .str_len => self.builder.strLen(operand),
            .print => self.builder.print(operand),
            .princ => self.builder.princ(operand),
            .write_char => self.builder.writeChar(operand),
            .char_upcase => self.builder.charUpcase(operand),
            .char_downcase => self.builder.charDowncase(operand),
            .digit_char_p => self.builder.digitCharP(operand),
            .alpha_char_p => self.builder.alphaCharP(operand),
            .parse_integer => self.builder.parseInteger(operand),
            .write_to_string => self.builder.writeToString(operand),
            .lognot => self.builder.lognot(operand),
            .read_file => self.builder.readFile(operand),
            .string_to_list => self.builder.stringToList(operand),
            .list_to_string => self.builder.listToString(operand),
            .string_upcase => self.builder.stringUpcase(operand),
            .string_downcase => self.builder.stringDowncase(operand),
            .stringp => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .stringp = .{ .operand = operand } };
                break :blk node;
            },
            .vectorp => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .vectorp = .{ .operand = operand } };
                break :blk node;
            },
            .closurep => self.builder.closurep(operand),
            .keywordp => self.builder.keywordp(operand),
            .random => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .random = .{ .operand = operand } };
                break :blk node;
            },
            .intern => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .intern = .{ .operand = operand } };
                break :blk node;
            },
            .sym_name => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .sym_name = .{ .operand = operand } };
                break :blk node;
            },
            .type_of => self.builder.typeOf(operand),
            .error_user => self.builder.errorUser(operand),
            .characterp => self.builder.characterp(operand),
            .floatp => self.builder.floatp(operand),
            .listp => self.builder.listp(operand),
            .atom => self.builder.atomp(operand),
            .char_code => self.builder.charCode(operand),
            .code_char => self.builder.codeChar(operand),
            .unread_char => self.builder.unreadChar(operand),
            .load => self.builder.load(operand),
            .read_from_string => self.builder.readFromString(operand),
            .eval => self.builder.eval(operand),
            .macroexpand => self.builder.macroexpand(operand),
            .boundp => self.builder.boundp(operand),
            .fboundp => self.builder.fboundp(operand),
            .symbol_value => self.builder.symbolValue(operand),
            .symbol_function => self.builder.symbolFunction(operand),
            .abs => self.builder.abs(operand),
            .zerop => self.builder.zerop(operand),
            .plusp => self.builder.plusp(operand),
            .minusp => self.builder.minusp(operand),
            .evenp => self.builder.evenp(operand),
            .oddp => self.builder.oddp(operand),
            .length => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .length = .{ .operand = operand } };
                break :blk node;
            },
            .reverse => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .reverse = .{ .operand = operand } };
                break :blk node;
            },
            .last => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .last = .{ .operand = operand } };
                break :blk node;
            },
            else => error.InvalidSyntax,
        } catch return error.OutOfMemory;
    }

    fn compileNullaryPrim(self: *Compiler, prim: PrimTag) CompileError!*Ir {
        return switch (prim) {
            .read_char => self.builder.readChar(),
            .peek_char => self.builder.peekChar(),
            .read => self.builder.readSexp(),
            .gensym => self.builder.gensym(),
            .terpri => self.builder.terpri(),
            else => error.InvalidSyntax,
        } catch return error.OutOfMemory;
    }

    fn compileListPrim(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (list a b c ...) -> variadic
        var elements = std.ArrayList(*const Ir){};
        defer elements.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const elem_ir = try self.compile(cons.car, env);
            elements.append(self.allocator, elem_ir) catch return error.OutOfMemory;
            current = cons.cdr;
        }

        return self.builder.list(elements.items) catch return error.OutOfMemory;
    }

    fn compileSubstring(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (substring str start end) - 3 arguments
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const str_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const start_ir = try self.compile(cons2.car, env);

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const end_ir = try self.compile(cons3.car, env);

        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .substring = .{ .str = str_ir, .start = start_ir, .end = end_ir } };
        return node;
    }

    fn compileSubseq(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (subseq seq start &optional end)
        // For now, just works like substring for strings
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const seq_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const start_ir = try self.compile(cons2.car, env);

        // End is optional - if not provided, use string-length
        const end_ir = if (cons2.cdr.isCons()) blk: {
            const cons3 = cons2.cdr.toPtr(Cons);
            break :blk try self.compile(cons3.car, env);
        } else blk: {
            // Use string-length as default end
            break :blk self.builder.strLen(seq_ir) catch return error.OutOfMemory;
        };

        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .substring = .{ .str = seq_ir, .start = start_ir, .end = end_ir } };
        return node;
    }

    fn compileConcatenate(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (concatenate 'string str1 str2 ...)
        // (concatenate 'list list1 list2 ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const b = self.builtins orelse return error.InvalidSyntax;

        // First arg should be a quoted type: (quote string) or (quote list)
        var type_sym: Value = undefined;
        if (cons1.car.isCons()) {
            const quote_cons = cons1.car.toPtr(Cons);
            // Check if it's (quote xxx)
            if (quote_cons.car.isSymbol()) {
                const sym = quote_cons.car.toPtr(Symbol);
                if (std.mem.eql(u8, sym.getName(), "quote") and quote_cons.cdr.isCons()) {
                    type_sym = quote_cons.cdr.toPtr(Cons).car;
                } else {
                    return error.InvalidSyntax;
                }
            } else {
                return error.InvalidSyntax;
            }
        } else {
            return error.InvalidSyntax;
        }

        // Get the sequences
        var rest = cons1.cdr;
        if (!rest.isCons()) {
            // No sequences, return empty string or nil
            if (type_sym.raw == b.string.raw) {
                return self.builder.lit(Value.nil) catch return error.OutOfMemory;
            } else {
                return self.builder.lit(Value.nil) catch return error.OutOfMemory;
            }
        }

        // Compile first sequence
        const first_cons = rest.toPtr(Cons);
        var result_ir = try self.compile(first_cons.car, env);
        rest = first_cons.cdr;

        // Concatenate remaining sequences based on type
        if (type_sym.raw == b.string.raw) {
            while (rest.isCons()) {
                const cons = rest.toPtr(Cons);
                const next_ir = try self.compile(cons.car, env);
                result_ir = self.builder.strConcat(result_ir, next_ir) catch return error.OutOfMemory;
                rest = cons.cdr;
            }
        } else if (type_sym.raw == b.list.raw) {
            while (rest.isCons()) {
                const cons = rest.toPtr(Cons);
                const next_ir = try self.compile(cons.car, env);
                result_ir = self.builder.append(result_ir, next_ir) catch return error.OutOfMemory;
                rest = cons.cdr;
            }
        } else {
            return error.InvalidSyntax;
        }

        return result_ir;
    }

    fn compileCoerce(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (coerce obj 'type)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const obj_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const b = self.builtins orelse return error.InvalidSyntax;

        // Second arg should be a quoted type
        var type_sym: Value = undefined;
        if (cons2.car.isCons()) {
            const quote_cons = cons2.car.toPtr(Cons);
            if (quote_cons.car.isSymbol()) {
                const sym = quote_cons.car.toPtr(Symbol);
                if (std.mem.eql(u8, sym.getName(), "quote") and quote_cons.cdr.isCons()) {
                    type_sym = quote_cons.cdr.toPtr(Cons).car;
                } else {
                    return error.InvalidSyntax;
                }
            } else {
                return error.InvalidSyntax;
            }
        } else {
            return error.InvalidSyntax;
        }

        // Generate conversion based on target type
        if (type_sym.raw == b.list.raw) {
            // (coerce x 'list) - convert sequence to list
            // For strings, use string-to-list
            return self.builder.stringToList(obj_ir) catch return error.OutOfMemory;
        } else if (type_sym.raw == b.string.raw) {
            // (coerce x 'string) - convert sequence to string
            // For lists, use list-to-string
            return self.builder.listToString(obj_ir) catch return error.OutOfMemory;
        } else if (type_sym.raw == b.character.raw) {
            // (coerce x 'character) - convert to character
            return self.builder.codeChar(obj_ir) catch return error.OutOfMemory;
        } else {
            return error.InvalidSyntax;
        }
    }

    fn compileFormat(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (format dest control-string args...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const dest_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const control_ir = try self.compile(cons2.car, env);

        // Collect remaining args
        var arg_list = std.ArrayList(*const Ir){};
        defer arg_list.deinit(self.allocator);

        var rest = cons2.cdr;
        while (rest.isCons()) {
            const cons = rest.toPtr(Cons);
            const arg_ir = try self.compile(cons.car, env);
            arg_list.append(self.allocator, arg_ir) catch return error.OutOfMemory;
            rest = cons.cdr;
        }

        return self.builder.format(dest_ir, control_ir, arg_list.items) catch return error.OutOfMemory;
    }

    fn compileMakeHash(self: *Compiler, args: Value) CompileError!*Ir {
        // (make-hash-table) or (make-hash-table :size n)
        // For now, just use default capacity
        _ = args;
        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .make_hash = .{ .capacity = 16 } };
        return node;
    }

    /// Compile composed car/cdr accessor like cadr, caddr, etc.
    /// Pattern string: 'a' = car, 'd' = cdr, applied right-to-left
    /// e.g., "ad" for cadr means (car (cdr x))
    fn compileComposedAccessor(self: *Compiler, args: Value, env: *const Env, pattern: []const u8) CompileError!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        var result = try self.compile(cons.car, env);

        // Apply pattern right-to-left (innermost to outermost)
        var i: usize = pattern.len;
        while (i > 0) {
            i -= 1;
            result = switch (pattern[i]) {
                'a' => self.builder.car(result) catch return error.OutOfMemory,
                'd' => self.builder.cdr(result) catch return error.OutOfMemory,
                else => return error.InvalidSyntax,
            };
        }
        return result;
    }

    fn compileMakeVector(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (make-vector size &optional init)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const size_ir = try self.compile(cons1.car, env);

        var init_ir: ?*const Ir = null;
        if (cons1.cdr.isCons()) {
            const cons2 = cons1.cdr.toPtr(Cons);
            init_ir = try self.compile(cons2.car, env);
        }

        return self.builder.vecNew(size_ir, init_ir) catch return error.OutOfMemory;
    }

    fn compileVectorPrim(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (vector a b c ...) -> create vector from elements
        var elements = std.ArrayList(*const Ir){};
        defer elements.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const elem_ir = try self.compile(cons.car, env);
            elements.append(self.allocator, elem_ir) catch return error.OutOfMemory;
            current = cons.cdr;
        }

        return self.builder.vec(elements.items) catch return error.OutOfMemory;
    }

    fn compileSvset(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (%svset vec index value) - internal setter for (setf (svref ...))
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const vec_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const idx_ir = try self.compile(cons2.car, env);

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const val_ir = try self.compile(cons3.car, env);

        return self.builder.vecSet(vec_ir, idx_ir, val_ir) catch return error.OutOfMemory;
    }

    fn compileGethash(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (gethash key hashtable)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const key_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const table_ir = try self.compile(cons2.car, env);

        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .hash_get = .{ .table = table_ir, .key = key_ir } };
        return node;
    }

    fn compileSethash(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (puthash key value hash-table) - CL convention
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const key_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const value_ir = try self.compile(cons2.car, env);

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const table_ir = try self.compile(cons3.car, env);

        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .hash_set = .{ .table = table_ir, .key = key_ir, .value = value_ir } };
        return node;
    }

    fn compileRemhash(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (remhash key hashtable)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const key_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const table_ir = try self.compile(cons2.car, env);

        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .hash_rem = .{ .table = table_ir, .key = key_ir } };
        return node;
    }

    fn compileHashTableCount(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (hash-table-count hashtable)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const table_ir = try self.compile(cons.car, env);

        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .hash_count = .{ .operand = table_ir } };
        return node;
    }

    fn compileHashTableP(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (hash-table-p x)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const operand_ir = try self.compile(cons.car, env);

        const node = self.allocator.create(Ir) catch return error.OutOfMemory;
        node.* = .{ .hashtablep = .{ .operand = operand_ir } };
        return node;
    }

    fn compileCall(self: *Compiler, func_expr: Value, args_expr: Value, env: *const Env) CompileError!*Ir {
        return self.compileCallWithTail(func_expr, args_expr, env, false);
    }

    fn compileCallWithTail(self: *Compiler, func_expr: Value, args_expr: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        const func_ir = try self.compile(func_expr, env);

        var args = std.ArrayList(*Ir){};
        defer args.deinit(self.allocator);

        var list = args_expr;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const arg_ir = try self.compile(cons.car, env);
            args.append(self.allocator, arg_ir) catch return error.OutOfMemory;
            list = cons.cdr;
        }

        // Convert to const slice
        const items = self.allocator.dupe(*const Ir, args.items) catch
            return error.OutOfMemory;

        if (in_tail) {
            return self.builder.tailcall(func_ir, items) catch return error.OutOfMemory;
        } else {
            return self.builder.call(func_ir, items) catch return error.OutOfMemory;
        }
    }

    // ========================================================================
    // Type Inference
    // ========================================================================

    /// Type inference error types
    pub const InferError = error{
        TypeMismatch,
        ArityMismatch,
        InfiniteType,
        OutOfMemory,
        InferenceFailed,
    };

    /// Run type inference on an IR tree
    /// Returns the inferred type, or an error with a descriptive message
    pub fn typeInfer(self: *Compiler, ir_node: *const Ir) InferError!*const types.InferType {
        // Create inference context
        var ctx = types.InferCtx.init(self.allocator);
        defer ctx.deinit();

        // Create type environment
        var type_env = types.infer.InferCtx.TypeEnv.init(self.allocator);
        defer type_env.deinit();

        // Infer types and collect constraints
        const inferred = ctx.infer(ir_node, &type_env) catch |err| {
            return switch (err) {
                error.OutOfMemory => error.OutOfMemory,
                else => error.InferenceFailed,
            };
        };

        // Solve constraints via unification
        ctx.solve() catch |err| {
            return switch (err) {
                error.TypeMismatch => error.TypeMismatch,
                error.ArityMismatch => error.ArityMismatch,
                error.InfiniteType => error.InfiniteType,
                error.OutOfMemory => error.OutOfMemory,
            };
        };

        // Return the resolved type
        return ctx.resolve(inferred);
    }

    /// Type-check an IR tree, returning an error if it's ill-typed
    /// This is a simplified check - just runs inference and solve
    pub fn typeCheck(self: *Compiler, ir_node: *const Ir) InferError!void {
        _ = try self.typeInfer(ir_node);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "compile fixnum" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    const result = try compiler.compile(Value.makeFixnum(42), &env);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(result.*));
    try testing.expectEqual(@as(i64, 42), result.lit.toFixnum());

    allocator.destroy(result);
}

test "compile nil" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    const result = try compiler.compile(Value.nil, &env);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(result.*));
    try testing.expect(result.lit.isNil());

    allocator.destroy(result);
}

test "env lookup" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var outer = Env.init(allocator, null);
    defer outer.deinit();
    _ = try outer.bind("x");
    _ = try outer.bind("y");

    var inner = Env.init(allocator, &outer);
    defer inner.deinit();
    _ = try inner.bind("z");

    // z is at depth 0, index 0
    const z_lookup = inner.lookup("z");
    try testing.expect(z_lookup != null);
    try testing.expectEqual(@as(u16, 0), z_lookup.?.depth);
    try testing.expectEqual(@as(u16, 0), z_lookup.?.index);

    // x is at depth 1, index 0
    const x_lookup = inner.lookup("x");
    try testing.expect(x_lookup != null);
    try testing.expectEqual(@as(u16, 1), x_lookup.?.depth);
    try testing.expectEqual(@as(u16, 0), x_lookup.?.index);

    // w doesn't exist
    try testing.expect(inner.lookup("w") == null);
}

test "type inference for literals" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    var type_env = TypeEnv.init(allocator);
    defer type_env.deinit();

    var occ = OccurrenceCtx.init(allocator);
    defer occ.deinit();

    // Fixnum has type fixnum
    const fixnum_result = try compiler.compileTyped(Value.makeFixnum(42), &env, &type_env, &occ);
    try testing.expectEqual(&types.t_fixnum, fixnum_result.ty);
    allocator.destroy(fixnum_result.ir);

    // Nil has type nil
    const nil_result = try compiler.compileTyped(Value.nil, &env, &type_env, &occ);
    try testing.expectEqual(&types.t_nil, nil_result.ty);
    allocator.destroy(nil_result.ir);
}

test "occurrence typing with type env" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Test that type environment lookup works
    var type_env = TypeEnv.init(allocator);
    defer type_env.deinit();

    try type_env.bind("x", &types.t_fixnum);
    try testing.expectEqual(&types.t_fixnum, type_env.lookup("x").?);

    // Test with parent env
    var child_env = TypeEnv.initWithParent(allocator, &type_env);
    defer child_env.deinit();

    try child_env.bind("y", &types.t_cons);

    // Child can see parent bindings
    try testing.expectEqual(&types.t_fixnum, child_env.lookup("x").?);
    try testing.expectEqual(&types.t_cons, child_env.lookup("y").?);
}

test "extract predicate info" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Build IR for (consp x)
    var builder = IrBuilder.init(allocator);

    const var_x = try builder.variable("x", 0, 0);
    const consp_ir = try builder.consp(var_x);

    // Should extract predicate info
    const info = Compiler.extractPredicateInfo(consp_ir);
    try testing.expect(info != null);
    try testing.expectEqualStrings("x", info.?.var_name);
    try testing.expectEqual(&types.t_cons, info.?.narrowed_type);

    // Free name copy from variable
    allocator.free(var_x.@"var".name);
    allocator.destroy(var_x);
    allocator.destroy(consp_ir);
}
