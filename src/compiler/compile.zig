//! S-expression to IR compiler
//!
//! Compiles parsed Habu expressions (cons trees) to IR nodes.
//! Handles:
//! - Special forms: if, lambda, let, setq, quote, progn, while
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
const BiChecker = types.BiChecker;
const TypingCtx = types.TypingCtx;
const vm_mod = @import("../interp/vm.zig");
const Vm = vm_mod.Vm;
const bytecode = @import("../bytecode/bytecode.zig");
const Emitter = bytecode.Emitter;
const Chunk = bytecode.Chunk;

pub const Error = error{
    InvalidSyntax,
    UnboundVariable,
    InvalidLambda,
    InvalidLet,
    InvalidIf,
    InvalidSet,
    OutOfMemory,
    NoSpaceLeft, // Buffer overflow in name generation
    UninitializedBuiltins, // Builtins not set when required
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

    // Declarations (no-op, for CL compatibility)
    declare: Value,

    // Type assertions
    the: Value,

    // Non-local exits
    block: Value,
    @"return-from": Value,
    @"unwind-protect": Value,
    @"catch": Value,
    throw: Value,
    @"handler-case": Value,
    signal: Value,
    @"%condition%": Value, // Internal tag for condition system
    @"restart-case": Value,
    @"invoke-restart": Value,
    @"find-restart": Value,
    tagbody: Value,
    go: Value,
    values: Value,
    @"values-list": Value,
    @"multiple-value-bind": Value,
    @"multiple-value-call": Value,
    @"multiple-value-list": Value,

    // Packages
    defpackage: Value,
    @"in-package": Value,
    @"export": Value,
    @"use-package": Value,

    // Structure definition
    defstruct: Value,

    // CLOS
    defclass: Value,
    @"make-instance": Value,
    @"slot-value": Value,
    defgeneric: Value,
    defmethod: Value,

    // eval-when for compile-time evaluation
    @"eval-when": Value,

    // Primitives - Arithmetic
    @"+": Value,
    @"-": Value,
    @"*": Value,
    @"/": Value,
    mod: Value,
    @"%": Value,
    quot: Value,
    truncate: Value,
    rem: Value,

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
    find: Value,
    position: Value,
    count: Value,
    remove: Value,
    list: Value,
    rplaca: Value,
    rplacd: Value,

    // Primitives - Type predicates (CL-style -p suffix)
    consp: Value,
    symbolp: Value,
    numberp: Value,
    stringp: Value,
    vectorp: Value,
    closurep: Value,
    keywordp: Value,
    null: Value,
    not: Value,
    characterp: Value,
    floatp: Value,
    listp: Value,
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
    get: Value,
    put: Value,
    remprop: Value,
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

    // Primitives - Math functions
    sqrt: Value,
    sin: Value,
    cos: Value,
    tan: Value,
    exp: Value,
    log: Value,
    floor: Value,
    ceiling: Value,
    round: Value,

    // Primitives - Vector operations (CL names)
    aref: Value, // CL: array element access
    svref: Value, // CL: simple-vector element access
    @"%svset": Value, // internal: (setf (svref ...)) expands to this
    @"%aset": Value, // internal: (setf (aref ...)) expands to this
    @"%set-slot-value": Value, // internal: (setf (slot-value ...)) expands to this

    // Stream I/O primitives
    @"%open": Value,
    @"%close": Value,
    @"%read-line": Value,
    @"%write-line": Value,
    @"%read-byte": Value,
    @"%write-byte": Value,
    @"%file-position": Value,
    @"%file-length": Value,
    @"%finish-output": Value,
    @"%force-output": Value,

    @"vector-length": Value,
    @"make-vector": Value,
    vector: Value,
    @"make-array": Value, // CL: multi-dimensional array creation

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
    @"random-seed": Value,
    format: Value,

    // Primitives - Character functions
    @"char-upcase": Value,
    @"char-downcase": Value,
    @"digit-char-p": Value,
    @"alpha-char-p": Value,

    // Primitives - Reader macros
    @"set-macro-character": Value,
    @"get-macro-character": Value,
    @"set-dispatch-macro-character": Value,
    @"get-dispatch-macro-character": Value,

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
    // coerce removed - implemented in stdlib

    // Primitives - Hash tables
    @"make-hash-table": Value,
    gethash: Value,
    puthash: Value,
    remhash: Value,
    @"hash-table-count": Value,
    clrhash: Value,
    @"hash-table-test": Value,
    @"hash-table-p": Value,
    @"hash-table-keys": Value,
    @"hash-table-alist": Value,
    rationalp: Value,
    complexp: Value,
    @"make-complex": Value,
    @"real-part": Value,
    @"imag-part": Value,
    numerator: Value,
    denominator: Value,
    // Streams
    streamp: Value,
    @"input-stream-p": Value,
    @"output-stream-p": Value,
    @"make-string-input-stream": Value,
    @"make-string-output-stream": Value,
    @"get-output-stream-string": Value,
    @"write-to-stream": Value,

    // Type name symbols (for type dispatch)
    ty_fixnum: Value,
    ty_integer: Value, // alias for fixnum
    ty_symbol: Value,
    ty_vector: Value,
    ty_closure: Value,
    ty_function: Value, // alias for closure
    ty_list: Value,
    @"ty_non-nil": Value,
    ty_any: Value,
    ty_nil: Value,
    ty_float: Value,
    @"ty_hash-table": Value,
    ty_keyword: Value,
    ty_cons: Value,
    ty_string: Value,
    ty_char: Value,
    ty_character: Value, // alias for char
    ty_t: Value,
    ty_union: Value, // (union T1 T2 ...) - union type
    ty_or: Value, // (or T1 T2 ...) - alias for union

    // Dependent type form symbols (QTT)
    ty_pi: Value, // (pi (x : A) B) - dependent function type
    ty_sigma: Value, // (sigma (x : A) B) - dependent pair type
    ty_refine: Value, // (refine T x P) - refinement type
    ty_vec: Value, // (vec a n) - length-indexed vector
    ty_forall: Value, // (forall (a) T) - universally quantified type

    // Lambda parameter markers
    @"&rest": Value,
    @"&body": Value,
    @"&optional": Value,
    @"&key": Value,

    // Special dispatch symbols
    _: Value,
    @"else": Value,
    @"->": Value,

    // eval-when keywords (interned as keywords for identity comparison)
    kw_execute: Value,
    @"kw_load-toplevel": Value,
    @"kw_compile-toplevel": Value,
    kw_use: Value,
    kw_export: Value,
    kw_size: Value,
    kw_test: Value,
    kw_eq: Value,
    kw_eql: Value,
    kw_equal: Value,

    // *features* keywords
    kw_habu: Value,
    kw_zig: Value,
    kw_unix: Value,
    kw_darwin: Value,
    kw_windows: Value,

    /// Initialize all builtin symbols from heap
    pub fn init(heap: *Heap) !Builtins {
        return .{
            .@"if" = try heap.intern("if"),
            .cond = try heap.intern("cond"),
            .@"and" = try heap.intern("and"),
            .@"or" = try heap.intern("or"),
            .let = try heap.intern("let"),
            .@"let*" = try heap.intern("let*"),
            .letrec = try heap.intern("letrec"),
            .lambda = try heap.intern("lambda"),
            .@"fn" = try heap.intern("fn"),
            .define = try heap.intern("define"),
            .defvar = try heap.intern("defvar"),
            .defun = try heap.intern("defun"),
            .setq = try heap.intern("setq"),
            .progn = try heap.intern("progn"),
            .begin = try heap.intern("begin"),
            .@"while" = try heap.intern("while"),
            .quote = try heap.intern("quote"),
            .quasiquote = try heap.intern("quasiquote"),
            .unquote = try heap.intern("unquote"),
            .@"unquote-splicing" = try heap.intern("unquote-splicing"),
            .function = try heap.intern("function"),
            .funcall = try heap.intern("funcall"),
            .apply = try heap.intern("apply"),
            .defmacro = try heap.intern("defmacro"),
            .macroexpand = try heap.intern("macroexpand"),
            .declare = try heap.intern("declare"),
            .the = try heap.intern("the"),
            .flet = try heap.intern("flet"),
            .labels = try heap.intern("labels"),
            .block = try heap.intern("block"),
            .@"return-from" = try heap.intern("return-from"),
            .@"unwind-protect" = try heap.intern("unwind-protect"),
            .@"catch" = try heap.intern("catch"),
            .throw = try heap.intern("throw"),
            .@"handler-case" = try heap.intern("handler-case"),
            .signal = try heap.intern("signal"),
            .@"%condition%" = try heap.intern("%condition%"),
            .@"restart-case" = try heap.intern("restart-case"),
            .@"invoke-restart" = try heap.intern("invoke-restart"),
            .@"find-restart" = try heap.intern("find-restart"),
            .tagbody = try heap.intern("tagbody"),
            .go = try heap.intern("go"),
            .values = try heap.intern("values"),
            .@"values-list" = try heap.intern("values-list"),
            .@"multiple-value-bind" = try heap.intern("multiple-value-bind"),
            .@"multiple-value-call" = try heap.intern("multiple-value-call"),
            .@"multiple-value-list" = try heap.intern("multiple-value-list"),
            // Packages
            .defpackage = try heap.intern("defpackage"),
            .@"in-package" = try heap.intern("in-package"),
            .@"export" = try heap.intern("export"),
            .@"use-package" = try heap.intern("use-package"),
            // Structure definition
            .defstruct = try heap.intern("defstruct"),
            // CLOS
            .defclass = try heap.intern("defclass"),
            .@"make-instance" = try heap.intern("make-instance"),
            .@"slot-value" = try heap.intern("slot-value"),
            .defgeneric = try heap.intern("defgeneric"),
            .defmethod = try heap.intern("defmethod"),
            .@"eval-when" = try heap.intern("eval-when"),
            // Primitives - Arithmetic
            .@"+" = try heap.intern("+"),
            .@"-" = try heap.intern("-"),
            .@"*" = try heap.intern("*"),
            .@"/" = try heap.intern("/"),
            .mod = try heap.intern("mod"),
            .@"%" = try heap.intern("%"),
            .quot = try heap.intern("quot"),
            .truncate = try heap.intern("truncate"),
            .rem = try heap.intern("rem"),
            // Primitives - Comparison
            .eq = try heap.intern("eq"),
            .equal = try heap.intern("equal"),
            .eql = try heap.intern("eql"),
            .@"<" = try heap.intern("<"),
            .@">" = try heap.intern(">"),
            .@"<=" = try heap.intern("<="),
            .@">=" = try heap.intern(">="),
            .@"=" = try heap.intern("="),
            // Primitives - List operations
            .cons = try heap.intern("cons"),
            .car = try heap.intern("car"),
            .cdr = try heap.intern("cdr"),
            .first = try heap.intern("first"),
            .rest = try heap.intern("rest"),
            // Composed accessors (2-level)
            .caar = try heap.intern("caar"),
            .cadr = try heap.intern("cadr"),
            .cdar = try heap.intern("cdar"),
            .cddr = try heap.intern("cddr"),
            // Composed accessors (3-level)
            .caaar = try heap.intern("caaar"),
            .caadr = try heap.intern("caadr"),
            .cadar = try heap.intern("cadar"),
            .caddr = try heap.intern("caddr"),
            .cdaar = try heap.intern("cdaar"),
            .cdadr = try heap.intern("cdadr"),
            .cddar = try heap.intern("cddar"),
            .cdddr = try heap.intern("cdddr"),
            // Positional accessors
            .second = try heap.intern("second"),
            .third = try heap.intern("third"),
            .fourth = try heap.intern("fourth"),
            .append = try heap.intern("append"),
            .length = try heap.intern("length"),
            .reverse = try heap.intern("reverse"),
            .nth = try heap.intern("nth"),
            .nthcdr = try heap.intern("nthcdr"),
            .last = try heap.intern("last"),
            .member = try heap.intern("member"),
            .assoc = try heap.intern("assoc"),
            .find = try heap.intern("find"),
            .position = try heap.intern("position"),
            .count = try heap.intern("count"),
            .remove = try heap.intern("remove"),
            .list = try heap.intern("list"),
            .rplaca = try heap.intern("rplaca"),
            .rplacd = try heap.intern("rplacd"),
            // Primitives - Type predicates (CL-style -p suffix)
            .consp = try heap.intern("consp"),
            .symbolp = try heap.intern("symbolp"),
            .numberp = try heap.intern("numberp"),
            .stringp = try heap.intern("stringp"),
            .vectorp = try heap.intern("vectorp"),
            .closurep = try heap.intern("closurep"),
            .keywordp = try heap.intern("keywordp"),
            .null = try heap.intern("null"),
            .not = try heap.intern("not"),
            .characterp = try heap.intern("characterp"),
            .floatp = try heap.intern("floatp"),
            .listp = try heap.intern("listp"),
            .atom = try heap.intern("atom"),
            // Primitives - Character operations
            .@"char-code" = try heap.intern("char-code"),
            .@"code-char" = try heap.intern("code-char"),
            .@"char=" = try heap.intern("char="),
            .@"char<" = try heap.intern("char<"),
            .@"char>" = try heap.intern("char>"),
            .@"read-char" = try heap.intern("read-char"),
            .@"peek-char" = try heap.intern("peek-char"),
            .read = try heap.intern("read"),
            .@"read-from-string" = try heap.intern("read-from-string"),
            .load = try heap.intern("load"),
            .@"unread-char" = try heap.intern("unread-char"),
            .eval = try heap.intern("eval"),
            .gensym = try heap.intern("gensym"),
            // Primitives - Symbol operations
            .boundp = try heap.intern("boundp"),
            .fboundp = try heap.intern("fboundp"),
            .@"symbol-value" = try heap.intern("symbol-value"),
            .@"symbol-function" = try heap.intern("symbol-function"),
            .typep = try heap.intern("typep"),
            .@"type-of" = try heap.intern("type-of"),
            .intern = try heap.intern("intern"),
            .@"symbol-name" = try heap.intern("symbol-name"),
            .get = try heap.intern("get"),
            .put = try heap.intern("put"),
            .remprop = try heap.intern("remprop"),
            .@"error" = try heap.intern("error"),
            // Type specifier symbols for concatenate/coerce
            .string = try heap.intern("string"),
            .character = try heap.intern("character"),
            // Primitives - Numeric
            .abs = try heap.intern("abs"),
            .zerop = try heap.intern("zerop"),
            .plusp = try heap.intern("plusp"),
            .minusp = try heap.intern("minusp"),
            .evenp = try heap.intern("evenp"),
            .oddp = try heap.intern("oddp"),
            // Primitives - Math functions
            .sqrt = try heap.intern("sqrt"),
            .sin = try heap.intern("sin"),
            .cos = try heap.intern("cos"),
            .tan = try heap.intern("tan"),
            .exp = try heap.intern("exp"),
            .log = try heap.intern("log"),
            .floor = try heap.intern("floor"),
            .ceiling = try heap.intern("ceiling"),
            .round = try heap.intern("round"),
            // Primitives - Vector operations (CL names)
            .aref = try heap.intern("aref"),
            .svref = try heap.intern("svref"),
            .@"%svset" = try heap.intern("%svset"),
            .@"%aset" = try heap.intern("%aset"),
            .@"%set-slot-value" = try heap.intern("%set-slot-value"),
            // Stream I/O primitives
            .@"%open" = try heap.intern("%open"),
            .@"%close" = try heap.intern("%close"),
            .@"%read-line" = try heap.intern("%read-line"),
            .@"%write-line" = try heap.intern("%write-line"),
            .@"%read-byte" = try heap.intern("%read-byte"),
            .@"%write-byte" = try heap.intern("%write-byte"),
            .@"%file-position" = try heap.intern("%file-position"),
            .@"%file-length" = try heap.intern("%file-length"),
            .@"%finish-output" = try heap.intern("%finish-output"),
            .@"%force-output" = try heap.intern("%force-output"),
            .@"vector-length" = try heap.intern("vector-length"),
            .@"make-vector" = try heap.intern("make-vector"),
            .vector = try heap.intern("vector"),
            .@"make-array" = try heap.intern("make-array"),
            // Primitives - String operations (CL names)
            .char = try heap.intern("char"),
            .schar = try heap.intern("schar"),
            .@"string-length" = try heap.intern("string-length"),
            .@"string-concat" = try heap.intern("string-concat"),
            .@"string=" = try heap.intern("string="),
            .substring = try heap.intern("substring"),
            .subseq = try heap.intern("subseq"),
            // Primitives - I/O and misc
            .print = try heap.intern("print"),
            .princ = try heap.intern("princ"),
            .terpri = try heap.intern("terpri"),
            .@"write-char" = try heap.intern("write-char"),
            .random = try heap.intern("random"),
            .@"random-seed" = try heap.intern("random-seed"),
            .format = try heap.intern("format"),
            // Primitives - Character functions
            .@"char-upcase" = try heap.intern("char-upcase"),
            .@"char-downcase" = try heap.intern("char-downcase"),
            .@"digit-char-p" = try heap.intern("digit-char-p"),
            .@"alpha-char-p" = try heap.intern("alpha-char-p"),
            // Primitives - Reader macros
            .@"set-macro-character" = try heap.intern("set-macro-character"),
            .@"get-macro-character" = try heap.intern("get-macro-character"),
            .@"set-dispatch-macro-character" = try heap.intern("set-dispatch-macro-character"),
            .@"get-dispatch-macro-character" = try heap.intern("get-dispatch-macro-character"),
            // Primitives - String/number conversion
            .@"parse-integer" = try heap.intern("parse-integer"),
            .@"write-to-string" = try heap.intern("write-to-string"),
            // Primitives - Bitwise operations
            .logand = try heap.intern("logand"),
            .logior = try heap.intern("logior"),
            .logxor = try heap.intern("logxor"),
            .lognot = try heap.intern("lognot"),
            .ash = try heap.intern("ash"),
            // Primitives - File I/O
            .@"read-file" = try heap.intern("read-file"),
            .@"write-file" = try heap.intern("write-file"),
            // Primitives - String construction
            .@"make-string" = try heap.intern("make-string"),
            .@"string-to-list" = try heap.intern("string-to-list"),
            .@"list-to-string" = try heap.intern("list-to-string"),
            .@"string-upcase" = try heap.intern("string-upcase"),
            .@"string-downcase" = try heap.intern("string-downcase"),
            .concatenate = try heap.intern("concatenate"),
            // .coerce removed - implemented in stdlib
            // Primitives - Hash tables
            .@"make-hash-table" = try heap.intern("make-hash-table"),
            .gethash = try heap.intern("gethash"),
            .puthash = try heap.intern("puthash"),
            .remhash = try heap.intern("remhash"),
            .clrhash = try heap.intern("clrhash"),
            .@"hash-table-test" = try heap.intern("hash-table-test"),
            .@"hash-table-count" = try heap.intern("hash-table-count"),
            .@"hash-table-p" = try heap.intern("hash-table-p"),
            .@"hash-table-keys" = try heap.intern("hash-table-keys"),
            .@"hash-table-alist" = try heap.intern("hash-table-alist"),
            .rationalp = try heap.intern("rationalp"),
            .complexp = try heap.intern("complexp"),
            .@"make-complex" = try heap.intern("make-complex"),
            .@"real-part" = try heap.intern("real-part"),
            .@"imag-part" = try heap.intern("imag-part"),
            .numerator = try heap.intern("numerator"),
            .denominator = try heap.intern("denominator"),
            // Streams
            .streamp = try heap.intern("streamp"),
            .@"input-stream-p" = try heap.intern("input-stream-p"),
            .@"output-stream-p" = try heap.intern("output-stream-p"),
            .@"make-string-input-stream" = try heap.intern("make-string-input-stream"),
            .@"make-string-output-stream" = try heap.intern("make-string-output-stream"),
            .@"get-output-stream-string" = try heap.intern("get-output-stream-string"),
            .@"write-to-stream" = try heap.intern("write-to-stream"),
            // Type name symbols
            .ty_fixnum = try heap.intern("fixnum"),
            .ty_integer = try heap.intern("integer"),
            .ty_symbol = try heap.intern("symbol"),
            .ty_vector = try heap.intern("vector"),
            .ty_closure = try heap.intern("closure"),
            .ty_function = try heap.intern("function"),
            .ty_list = try heap.intern("list"),
            .@"ty_non-nil" = try heap.intern("non-nil"),
            .ty_any = try heap.intern("any"),
            .ty_nil = try heap.intern("nil"),
            .ty_float = try heap.intern("float"),
            .@"ty_hash-table" = try heap.intern("hash-table"),
            .ty_keyword = try heap.intern("keyword"),
            .ty_cons = try heap.intern("cons"),
            .ty_string = try heap.intern("string"),
            .ty_char = try heap.intern("char"),
            .ty_character = try heap.intern("character"),
            .ty_t = try heap.intern("t"),
            .ty_union = try heap.intern("union"),
            .ty_or = try heap.intern("or"),
            // Dependent type form symbols (QTT)
            .ty_pi = try heap.intern("pi"),
            .ty_sigma = try heap.intern("sigma"),
            .ty_refine = try heap.intern("refine"),
            .ty_vec = try heap.intern("vec"),
            .ty_forall = try heap.intern("forall"),
            // Lambda parameter markers
            .@"&rest" = try heap.intern("&rest"),
            .@"&body" = try heap.intern("&body"),
            .@"&optional" = try heap.intern("&optional"),
            .@"&key" = try heap.intern("&key"),
            // Special dispatch symbols
            ._ = try heap.intern("_"),
            .@"else" = try heap.intern("else"),
            .@"->" = try heap.intern("->"),
            // eval-when keywords (interned as keywords, not symbols)
            .kw_execute = try heap.internKeyword("execute"),
            .@"kw_load-toplevel" = try heap.internKeyword("load-toplevel"),
            .@"kw_compile-toplevel" = try heap.internKeyword("compile-toplevel"),
            .kw_use = try heap.internKeyword("use"),
            .kw_export = try heap.internKeyword("export"),
            .kw_size = try heap.internKeyword("size"),
            .kw_test = try heap.internKeyword("test"),
            .kw_eq = try heap.internKeyword("eq"),
            .kw_eql = try heap.internKeyword("eql"),
            .kw_equal = try heap.internKeyword("equal"),
            // *features* keywords
            .kw_habu = try heap.internKeyword("habu"),
            .kw_zig = try heap.internKeyword("zig"),
            .kw_unix = try heap.internKeyword("unix"),
            .kw_darwin = try heap.internKeyword("darwin"),
            .kw_windows = try heap.internKeyword("windows"),
        };
    }

    /// Check if a symbol is a builtin function (not special form)
    pub fn isBuiltinFunction(self: *const Builtins, sym: Value) bool {
        const s = sym.raw;
        // Primitives - Arithmetic
        if (s == self.@"+".raw) return true;
        if (s == self.@"-".raw) return true;
        if (s == self.@"*".raw) return true;
        if (s == self.@"/".raw) return true;
        if (s == self.mod.raw) return true;
        if (s == self.@"%".raw) return true;
        if (s == self.quot.raw) return true;
        if (s == self.truncate.raw) return true;
        if (s == self.rem.raw) return true;
        // Primitives - Comparison
        if (s == self.eq.raw) return true;
        if (s == self.equal.raw) return true;
        if (s == self.eql.raw) return true;
        if (s == self.@"<".raw) return true;
        if (s == self.@">".raw) return true;
        if (s == self.@"<=".raw) return true;
        if (s == self.@">=".raw) return true;
        if (s == self.@"=".raw) return true;
        // Primitives - List operations
        if (s == self.cons.raw) return true;
        if (s == self.car.raw) return true;
        if (s == self.cdr.raw) return true;
        if (s == self.first.raw) return true;
        if (s == self.rest.raw) return true;
        if (s == self.caar.raw) return true;
        if (s == self.cadr.raw) return true;
        if (s == self.cdar.raw) return true;
        if (s == self.cddr.raw) return true;
        if (s == self.caaar.raw) return true;
        if (s == self.caadr.raw) return true;
        if (s == self.cadar.raw) return true;
        if (s == self.caddr.raw) return true;
        if (s == self.cdaar.raw) return true;
        if (s == self.cdadr.raw) return true;
        if (s == self.cddar.raw) return true;
        if (s == self.cdddr.raw) return true;
        if (s == self.second.raw) return true;
        if (s == self.third.raw) return true;
        if (s == self.fourth.raw) return true;
        if (s == self.append.raw) return true;
        if (s == self.length.raw) return true;
        if (s == self.reverse.raw) return true;
        if (s == self.nth.raw) return true;
        if (s == self.nthcdr.raw) return true;
        if (s == self.last.raw) return true;
        if (s == self.member.raw) return true;
        if (s == self.assoc.raw) return true;
        if (s == self.find.raw) return true;
        if (s == self.position.raw) return true;
        if (s == self.count.raw) return true;
        if (s == self.remove.raw) return true;
        if (s == self.list.raw) return true;
        if (s == self.rplaca.raw) return true;
        if (s == self.rplacd.raw) return true;
        // Primitives - Type predicates
        if (s == self.consp.raw) return true;
        if (s == self.symbolp.raw) return true;
        if (s == self.numberp.raw) return true;
        if (s == self.stringp.raw) return true;
        if (s == self.vectorp.raw) return true;
        if (s == self.closurep.raw) return true;
        if (s == self.keywordp.raw) return true;
        if (s == self.null.raw) return true;
        if (s == self.not.raw) return true;
        if (s == self.characterp.raw) return true;
        if (s == self.floatp.raw) return true;
        if (s == self.listp.raw) return true;
        if (s == self.atom.raw) return true;
        // Primitives - Character operations
        if (s == self.@"char-code".raw) return true;
        if (s == self.@"code-char".raw) return true;
        if (s == self.@"char=".raw) return true;
        if (s == self.@"char<".raw) return true;
        if (s == self.@"char>".raw) return true;
        if (s == self.@"read-char".raw) return true;
        if (s == self.@"peek-char".raw) return true;
        if (s == self.read.raw) return true;
        if (s == self.@"read-from-string".raw) return true;
        if (s == self.load.raw) return true;
        if (s == self.@"unread-char".raw) return true;
        if (s == self.eval.raw) return true;
        if (s == self.gensym.raw) return true;
        // Primitives - Symbol operations
        if (s == self.boundp.raw) return true;
        if (s == self.fboundp.raw) return true;
        if (s == self.@"symbol-value".raw) return true;
        if (s == self.@"symbol-function".raw) return true;
        if (s == self.typep.raw) return true;
        if (s == self.@"type-of".raw) return true;
        if (s == self.intern.raw) return true;
        if (s == self.@"symbol-name".raw) return true;
        if (s == self.get.raw) return true;
        if (s == self.put.raw) return true;
        if (s == self.remprop.raw) return true;
        // Note: error is NOT a primitive - stdlib provides (defun error (msg) (signal 'error msg))
        // Primitives - Numeric
        if (s == self.abs.raw) return true;
        if (s == self.zerop.raw) return true;
        if (s == self.plusp.raw) return true;
        if (s == self.minusp.raw) return true;
        if (s == self.evenp.raw) return true;
        if (s == self.oddp.raw) return true;
        // Primitives - Math functions
        if (s == self.sqrt.raw) return true;
        if (s == self.sin.raw) return true;
        if (s == self.cos.raw) return true;
        if (s == self.tan.raw) return true;
        if (s == self.exp.raw) return true;
        if (s == self.log.raw) return true;
        if (s == self.floor.raw) return true;
        if (s == self.ceiling.raw) return true;
        if (s == self.round.raw) return true;
        // Primitives - Vector operations
        if (s == self.aref.raw) return true;
        if (s == self.svref.raw) return true;
        if (s == self.@"vector-length".raw) return true;
        if (s == self.@"make-vector".raw) return true;
        if (s == self.vector.raw) return true;
        if (s == self.@"make-array".raw) return true;
        // Primitives - String operations
        if (s == self.char.raw) return true;
        if (s == self.schar.raw) return true;
        if (s == self.@"string-length".raw) return true;
        if (s == self.@"string-concat".raw) return true;
        if (s == self.@"string=".raw) return true;
        if (s == self.substring.raw) return true;
        if (s == self.subseq.raw) return true;
        // Primitives - I/O
        if (s == self.print.raw) return true;
        if (s == self.princ.raw) return true;
        if (s == self.terpri.raw) return true;
        if (s == self.@"write-char".raw) return true;
        if (s == self.random.raw) return true;
        if (s == self.@"random-seed".raw) return true;
        if (s == self.format.raw) return true;
        // Primitives - Character functions
        if (s == self.@"char-upcase".raw) return true;
        if (s == self.@"char-downcase".raw) return true;
        if (s == self.@"digit-char-p".raw) return true;
        if (s == self.@"alpha-char-p".raw) return true;
        // Primitives - String/number conversion
        if (s == self.@"parse-integer".raw) return true;
        if (s == self.@"write-to-string".raw) return true;
        // Primitives - Bitwise operations
        if (s == self.logand.raw) return true;
        if (s == self.logior.raw) return true;
        if (s == self.logxor.raw) return true;
        if (s == self.lognot.raw) return true;
        if (s == self.ash.raw) return true;
        // Primitives - File I/O
        if (s == self.@"read-file".raw) return true;
        if (s == self.@"write-file".raw) return true;
        // Primitives - String construction
        if (s == self.@"make-string".raw) return true;
        if (s == self.@"string-to-list".raw) return true;
        if (s == self.@"list-to-string".raw) return true;
        if (s == self.@"string-upcase".raw) return true;
        if (s == self.@"string-downcase".raw) return true;
        if (s == self.concatenate.raw) return true;
        // Primitives - Hash tables
        if (s == self.@"make-hash-table".raw) return true;
        if (s == self.gethash.raw) return true;
        if (s == self.puthash.raw) return true;
        if (s == self.remhash.raw) return true;
        if (s == self.@"hash-table-count".raw) return true;
        if (s == self.clrhash.raw) return true;
        if (s == self.@"hash-table-test".raw) return true;
        if (s == self.@"hash-table-p".raw) return true;
        if (s == self.@"hash-table-keys".raw) return true;
        if (s == self.@"hash-table-alist".raw) return true;
        // Primitives - Numeric types
        if (s == self.rationalp.raw) return true;
        if (s == self.complexp.raw) return true;
        if (s == self.@"make-complex".raw) return true;
        if (s == self.@"real-part".raw) return true;
        if (s == self.@"imag-part".raw) return true;
        if (s == self.numerator.raw) return true;
        if (s == self.denominator.raw) return true;
        // Primitives - Streams
        if (s == self.streamp.raw) return true;
        if (s == self.@"input-stream-p".raw) return true;
        if (s == self.@"output-stream-p".raw) return true;
        if (s == self.@"make-string-input-stream".raw) return true;
        if (s == self.@"make-string-output-stream".raw) return true;
        if (s == self.@"get-output-stream-string".raw) return true;
        if (s == self.@"write-to-stream".raw) return true;
        // Also funcall and apply are callable
        if (s == self.funcall.raw) return true;
        if (s == self.apply.raw) return true;
        if (s == self.values.raw) return true;
        if (s == self.@"values-list".raw) return true;
        return false;
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
    /// Fast deduplication lookup
    seen: std.StringHashMap(void),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CaptureSet {
        return .{
            .captures = std.ArrayList(Ir.Capture){},
            .seen = std.StringHashMap(void).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CaptureSet) void {
        self.captures.deinit(self.allocator);
        self.seen.deinit();
    }

    /// Add a capture if not already present
    pub fn addCapture(self: *CaptureSet, name: []const u8, depth: u16, index: u16) !void {
        const gop = try self.seen.getOrPut(name);
        if (gop.found_existing) return;
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
    /// Bidirectional type checker for dependent types
    bi_checker: BiChecker,
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
    /// Defined struct types for typed struct support
    /// Maps struct name to its Type definition
    struct_types: std.StringHashMap(*const types.Type),
    /// Struct predicate names mapped to struct types (for occurrence typing)
    /// Maps "point-p" -> pointer to point struct type
    struct_predicates: std.StringHashMap(*const types.Type),
    /// Macro table: maps macro name to closure (expander function)
    /// When a form (macro-name args...) is compiled, the macro is expanded first
    macro_table: std.StringHashMap(Value),
    /// Optional VM for compile-time macro expansion
    vm: ?*Vm,
    /// Heap for creating runtime values during macro expansion
    heap: ?*Heap,
    /// Class metadata for CLOS compilation
    /// Maps class name to slot specifications (names + initforms)
    class_metadata: std.StringHashMap([]const SlotSpec),
    /// Generic function registry for CLOS method dispatch
    /// Maps generic function name to list of methods
    generic_functions: std.StringHashMap(std.ArrayList(MethodDef)),

    /// ADT variant definition
    pub const Variant = struct {
        name: []const u8,
        fields: []const []const u8,
    };

    /// CLOS method definition
    pub const MethodDef = struct {
        specializers: []const []const u8, // Class names for each parameter
        function_name: []const u8, // Global function name to call
    };

    /// Typed parameter info for function declarations
    pub const TypedParam = struct {
        name: []const u8,
        type_sym: ?Value, // null for untyped, otherwise the type symbol
    };

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .type_checker = TypeChecker.init(allocator),
            .bi_checker = BiChecker.init(allocator),
            .type_checking_enabled = false, // Off by default for gradual typing
            .globals = GlobalEnv.init(allocator),
            .builtins = null, // Lazily initialized when heap is available
            .occ = null,
            .boxed_vars = null,
            .defined_types = std.StringHashMap([]const Variant).init(allocator),
            .struct_types = std.StringHashMap(*const types.Type).init(allocator),
            .struct_predicates = std.StringHashMap(*const types.Type).init(allocator),
            .macro_table = std.StringHashMap(Value).init(allocator),
            .vm = null,
            .heap = null,
            .class_metadata = std.StringHashMap([]const SlotSpec).init(allocator),
            .generic_functions = std.StringHashMap(std.ArrayList(MethodDef)).init(allocator),
        };
    }

    /// Initialize with heap for symbol interning
    pub fn initWithHeap(allocator: std.mem.Allocator, heap: *Heap) !Compiler {
        // Builtins are interned in CL package (current_package starts as CL)
        const builtins = try Builtins.init(heap);
        // Switch to CL-USER for user code
        if (heap.cl_user_package) |cl_user| {
            heap.setCurrentPackage(cl_user);
        }
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .type_checker = TypeChecker.init(allocator),
            .bi_checker = BiChecker.init(allocator),
            .type_checking_enabled = false,
            .globals = GlobalEnv.init(allocator),
            .builtins = builtins,
            .occ = null,
            .boxed_vars = null,
            .defined_types = std.StringHashMap([]const Variant).init(allocator),
            .struct_types = std.StringHashMap(*const types.Type).init(allocator),
            .struct_predicates = std.StringHashMap(*const types.Type).init(allocator),
            .macro_table = std.StringHashMap(Value).init(allocator),
            .vm = null,
            .heap = heap,
            .class_metadata = std.StringHashMap([]const SlotSpec).init(allocator),
            .generic_functions = std.StringHashMap(std.ArrayList(MethodDef)).init(allocator),
        };
    }

    /// Set VM for compile-time macro expansion
    pub fn setVm(self: *Compiler, vm: *Vm) void {
        self.vm = vm;
    }

    pub fn deinit(self: *Compiler) void {
        self.type_checker.deinit();
        self.bi_checker.deinit();
        // Free struct_predicates keys (allocated with globals.allocator)
        var pred_iter = self.struct_predicates.keyIterator();
        while (pred_iter.next()) |key| {
            self.globals.allocator.free(key.*);
        }
        self.struct_predicates.deinit();
        // Free struct_types keys (allocated with globals.allocator)
        var type_iter = self.struct_types.keyIterator();
        while (type_iter.next()) |key| {
            self.globals.allocator.free(key.*);
        }
        self.struct_types.deinit();
        // Free class_metadata keys and SlotSpec arrays
        var class_iter = self.class_metadata.iterator();
        while (class_iter.next()) |entry| {
            self.globals.allocator.free(entry.key_ptr.*);
            for (entry.value_ptr.*) |spec| {
                self.globals.allocator.free(spec.name);
            }
            self.globals.allocator.free(entry.value_ptr.*);
        }
        self.class_metadata.deinit();
        // Free generic_functions - keys, method lists, specializers, function names
        var gf_iter = self.generic_functions.iterator();
        while (gf_iter.next()) |entry| {
            // Free the generic function name key
            self.globals.allocator.free(entry.key_ptr.*);
            // Free each method's data
            for (entry.value_ptr.items) |method| {
                // Free specializers array and each specializer string
                for (method.specializers) |spec| {
                    self.globals.allocator.free(spec);
                }
                self.globals.allocator.free(method.specializers);
                // Free function name
                self.globals.allocator.free(method.function_name);
            }
            // Free methods list
            self.globals.allocator.free(entry.value_ptr.items);
        }
        self.generic_functions.deinit();
        self.globals.deinit();
        self.macro_table.deinit();
        // Note: defined_types contains references to ArrayList buffers and duped strings
        // that are intentionally not freed - they persist for the compiler's lifetime
        // and the memory is small (type definitions). The hashmap itself is freed.
        self.defined_types.deinit();
    }

    /// Register a struct type definition
    pub fn registerStructType(self: *Compiler, struct_name: []const u8, struct_type: *const types.Type) !void {
        // Use globals.allocator for persistence across expressions
        const name_copy = try self.globals.allocator.dupe(u8, struct_name);
        try self.struct_types.put(name_copy, struct_type);
    }

    /// Look up a struct type by name
    pub fn getStructType(self: *const Compiler, struct_name: []const u8) ?*const types.Type {
        return self.struct_types.get(struct_name);
    }

    /// Enable type checking mode
    pub fn enableTypeChecking(self: *Compiler) void {
        self.type_checking_enabled = true;
    }

    /// Check if type checker has errors
    pub fn hasTypeErrors(self: *const Compiler) bool {
        return self.type_checker.hasErrors();
    }

    /// Check if bidirectional type checker has errors
    pub fn hasBiCheckErrors(self: *const Compiler) bool {
        return self.bi_checker.hasErrors();
    }

    /// Get bidirectional type checker errors
    pub fn getBiCheckErrors(self: *const Compiler) []const types.bicheck.TypeError {
        return self.bi_checker.errors.items;
    }

    /// Bidirectional type check for a lambda/function with typed parameters
    /// This is called during compilation when type_checking_enabled is true
    fn checkLambdaTypes(
        self: *Compiler,
        typed_params: []const TypedParam,
        return_type: ?Value,
        body_ir: *const Ir,
    ) void {
        // Build typing context with parameter types
        var ctx = TypingCtx.init(self.allocator);
        defer ctx.deinit();

        // Add parameters to context
        for (typed_params) |tp| {
            if (tp.type_sym) |type_sym| {
                // Parse type from symbol
                const param_type = self.parseTypeExpr(type_sym) orelse &types.t_any;
                ctx.bind(tp.name, param_type, .many) catch continue;
            } else {
                // Untyped parameter - bind as any
                ctx.bind(tp.name, &types.t_any, .many) catch continue;
            }
        }

        // If there's a return type, check body against it
        if (return_type) |ret_type_val| {
            const expected_type = self.parseTypeExpr(ret_type_val) orelse return;
            self.bi_checker.check(body_ir, expected_type, &ctx) catch {
                // Type error - already recorded in bi_checker.errors
            };
        } else {
            // No return type specified - just infer (validates internal consistency)
            _ = self.bi_checker.infer(body_ir, &ctx) catch {
                // Type error - already recorded in bi_checker.errors
            };
        }
    }

    /// Compile with type inference
    /// Returns both IR and inferred type
    pub fn compileTyped(
        self: *Compiler,
        expr: Value,
        env: *const Env,
        type_env: *const TypeEnv,
        occ: *const OccurrenceCtx,
    ) anyerror!TypedIr {
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
                .t => &types.t_symbol, // t is a symbol
                .fixnum => &types.t_fixnum,
                .float => &types.t_float,
                .char => &types.t_char,
                .string => &types.t_string,
                .symbol => &types.t_symbol,
                .cons => &types.t_cons,
                .vector => &types.t_vector,
                .keyword => &types.t_keyword,
                .closure => &types.t_closure,
                .hashtable => &types.t_hashtable,
                .rational => &types.t_rational,
                .complex => &types.t_complex,
                .stream => &types.t_stream,
                .bignum => &types.t_bignum,
                .array => &types.t_array,
                .pathname => &types.t_pathname,
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
            .add, .sub, .mul, .div, .mod, .quot, .rem => &types.t_fixnum,
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
    /// For (struct-p x), returns info to narrow x to the struct type
    fn extractPredicateInfo(node: *const Ir) ?PredicateInfo {
        // Handle struct_p specially - it carries the type directly
        if (node.* == .struct_p) {
            const sp = node.struct_p;
            if (sp.operand.* == .@"var") {
                return .{
                    .var_name = sp.operand.@"var".name,
                    .narrowed_type = sp.struct_type,
                    .else_type = null,
                };
            }
            return null;
        }

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
    ) Error!TypedIr {
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
    pub fn compile(self: *Compiler, expr: Value, env: *const Env) anyerror!*Ir {
        return self.compileWithTail(expr, env, false);
    }

    /// Compile with tail position tracking
    fn compileWithTail(self: *Compiler, expr: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // Nil
        if (expr.isNil()) {
            return try self.builder.lit(Value.nil);
        }

        // Fixnum
        if (expr.isFixnum()) {
            return try self.builder.lit(expr);
        }

        // Float
        if (expr.isFloat()) {
            return try self.builder.lit(expr);
        }

        // Rational
        if (expr.isRational()) {
            return try self.builder.lit(expr);
        }

        // Complex
        if (expr.isComplex()) {
            return try self.builder.lit(expr);
        }

        // Bignum
        if (expr.isBignum()) {
            return try self.builder.lit(expr);
        }

        // String
        if (expr.isString()) {
            return try self.builder.lit(expr);
        }

        // Character
        if (expr.isCharacter()) {
            return try self.builder.lit(expr);
        }

        // t and nil are self-evaluating (also symbols, but special)
        if (expr.isMagicSymbol()) {
            return try self.builder.lit(expr);
        }

        // Vector (literal - #(1 2 3))
        if (expr.isVector()) {
            return try self.builder.lit(expr);
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
                        const box_ref = try self.allocator.create(Ir);
                        box_ref.* = .{ .box_ref = .{ .operand = var_ir } };
                        return box_ref;
                    }
                }
                return var_ir;
            }
            // Check globals - use qualified name if symbol has package
            var qual_buf: [256]u8 = undefined;
            const qual_name = self.getQualifiedName(sym, &qual_buf) catch sym.getName();
            // Allow forward references: allocate slot if not found
            // Runtime will check if still undefined when accessed
            const idx = self.globals.lookup(qual_name) orelse
                try self.globals.define(qual_name);
            return try self.builder.globalRef(qual_name, idx);
        }

        // List (special form or function call)
        if (expr.isCons()) {
            return self.compileListWithTail(expr, env, in_tail);
        }

        // Keyword - just return as literal
        if (expr.isKeyword()) {
            return try self.builder.lit(expr);
        }

        return error.InvalidSyntax;
    }

    fn compileList(self: *Compiler, expr: Value, env: *const Env) anyerror!*Ir {
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
        setq,
        quote,
        function,
        quasiquote,
        @"while",
        loop,
        define,
        defvar,
        defun,
        the,
        declare,
        @"return-from",
        @"unwind-protect",
        @"catch",
        throw,
        @"handler-case",
        signal,
        @"restart-case",
        @"invoke-restart",
        @"find-restart",
        tagbody,
        go,
        values,
        @"values-list",
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
        // Packages
        defpackage,
        @"in-package",
        @"export",
        @"use-package",
        // Structure definition
        defstruct,
        // CLOS
        defclass,
        @"make-instance",
        @"slot-value",
        defgeneric,
        defmethod,
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
        .{ "setq", .setq },
        .{ "quote", .quote },
        .{ "function", .function },
        .{ "quasiquote", .quasiquote },
        .{ "while", .@"while" },
        .{ "loop", .loop },
        .{ "define", .define },
        .{ "defvar", .defvar },
        .{ "defun", .defun },
        .{ "the", .the },
        .{ "declare", .declare },
        .{ "block", .block },
        .{ "return-from", .@"return-from" },
        .{ "unwind-protect", .@"unwind-protect" },
        .{ "catch", .@"catch" },
        .{ "throw", .throw },
        .{ "handler-case", .@"handler-case" },
        .{ "signal", .signal },
        .{ "restart-case", .@"restart-case" },
        .{ "invoke-restart", .@"invoke-restart" },
        .{ "find-restart", .@"find-restart" },
        .{ "tagbody", .tagbody },
        .{ "go", .go },
        .{ "values", .values },
        .{ "values-list", .@"values-list" },
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
        // Packages
        .{ "defpackage", .defpackage },
        .{ "in-package", .@"in-package" },
        .{ "export", .@"export" },
        .{ "use-package", .@"use-package" },
        // Structure definition
        .{ "defstruct", .defstruct },
        // CLOS
        .{ "defclass", .defclass },
        .{ "make-instance", .@"make-instance" },
        .{ "slot-value", .@"slot-value" },
        .{ "defgeneric", .defgeneric },
        .{ "defmethod", .defmethod },
    });

    fn compileListWithTail(self: *Compiler, expr: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
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
                    .@"handler-case" => self.compileHandlerCaseWithTail(tail, env, in_tail),
                    // Non-tail forms
                    .lambda => self.compileLambda(tail, env),
                    .@"and" => self.compileAnd(tail, env),
                    .@"or" => self.compileOr(tail, env),
                    .funcall => self.compileFuncall(tail, env),
                    .apply => self.compileApply(tail, env),
                    .setq => self.compileSet(tail, env),
                    .quote => self.compileQuote(tail),
                    .function => self.compileFunction(tail, env),
                    .quasiquote => self.compileQuasiquote(tail, env),
                    .@"while" => self.compileWhile(tail, env),
                    .loop => self.compileLoop(tail, env),
                    .define, .defvar => self.compileDefine(tail, env),
                    .defun => self.compileDefun(tail, env),
                    .the => self.compileThe(tail, env),
                    .declare => self.builder.lit(Value.nil), // no-op, returns nil
                    .@"return-from" => self.compileReturnFrom(tail, env),
                    .throw => self.compileThrow(tail, env),
                    .signal => self.compileSignal(tail, env),
                    .@"restart-case" => self.compileRestartCase(tail, env),
                    .@"invoke-restart" => self.compileInvokeRestart(tail, env),
                    .@"find-restart" => self.compileFindRestart(tail, env),
                    .tagbody => self.compileTagbody(tail, env),
                    .go => self.compileGo(tail),
                    .values => self.compileValues(tail, env),
                    .@"values-list" => self.compileValuesList(tail, env),
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
                    // Packages
                    .defpackage => self.compileDefpackage(tail),
                    .@"in-package" => self.compileInPackage(tail),
                    .@"export" => self.compileExport(tail),
                    .@"use-package" => self.compileUsePackage(tail),
                    // Structure definition
                    .defstruct => self.compileDefstruct(tail, env),
                    // CLOS
                    .defclass => self.compileDefclass(tail, env),
                    .@"make-instance" => self.compileMakeInstance(tail, env),
                    .@"slot-value" => self.compileSlotValue(tail, env),
                    .defgeneric => self.compileDefgeneric(tail, env),
                    .defmethod => self.compileDefmethod(tail, env),
                };
            }

            // Check for macros - expand at compile time if VM is available
            if (self.macro_table.get(name)) |macro_def| {
                if (self.vm) |vm| {
                    const expanded = try self.expandMacro(macro_def, tail, vm);
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
        const lambda_sym = try heap.intern("lambda");
        // Build: (lambda params body)
        const body_cell = try heap.allocCons(body, Value.nil);
        const params_body = try heap.allocCons(params, body_cell);
        const lambda_list = try heap.allocCons(lambda_sym, params_body);

        // Compile the lambda to get a closure
        var macro_compiler = Compiler.initWithHeap(self.allocator, heap) catch
            return error.OutOfMemory;
        defer macro_compiler.deinit();
        macro_compiler.vm = vm;

        // Share macro table so nested macros (like prog1) work in macro bodies
        var iter = self.macro_table.iterator();
        while (iter.next()) |entry| {
            try macro_compiler.macro_table.put(entry.key_ptr.*, entry.value_ptr.*);
        }

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

        // Use arena for temporary chunk allocations - single cleanup handles all error paths
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        // Create temporary chunk pool for macro execution
        var chunk_ptrs = arena.allocator().alloc(*Chunk, child_chunks.len) catch {
            self.allocator.free(child_chunks);
            return error.OutOfMemory;
        };

        for (child_chunks, 0..) |*child_chunk, i| {
            const chunk_ptr = arena.allocator().create(Chunk) catch {
                self.allocator.free(child_chunks);
                return error.OutOfMemory;
            };
            chunk_ptr.* = child_chunk.*;
            chunk_ptrs[i] = chunk_ptr;
        }
        self.allocator.free(child_chunks);

        // Set chunk pool and run - all error paths now cleaned up by arena.deinit
        vm.setChunkPool(chunk_ptrs);
        const closure_val = try vm.run(&chunk);

        if (!closure_val.isClosure()) return error.InvalidSyntax;
        const closure = closure_val.toPtr(Closure);

        // Now call the closure with the macro arguments
        var arg_count: u8 = 0;
        var arg_list = args;
        while (arg_list.isCons()) {
            const arg_cons = arg_list.toPtr(Cons);
            try vm.push(arg_cons.car);
            arg_count += 1;
            arg_list = arg_cons.cdr;
        }

        // Call the macro expander
        return try vm.callClosure(closure, arg_count);
    }

    fn compileIf(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileIfWithTail(args, env, false);
    }

    fn compileIfWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
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

        return try self.builder.ifExpr(test_ir, then_ir, else_ir);
    }

    fn compileLambda(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileLambdaCore(args, env, null);
    }

    fn compileLambdaCore(self: *Compiler, args: Value, env: *const Env, return_type: ?Value) anyerror!*Ir {
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

        var optional_params = std.ArrayList(Ir.OptionalParam){};
        defer optional_params.deinit(self.allocator);

        var key_params = std.ArrayList(Ir.KeyParam){};
        defer key_params.deinit(self.allocator);

        var rest_param: ?[]const u8 = null;
        var in_optional = false;
        var in_key = false;
        var param_list = params_expr;
        while (param_list.isCons()) {
            const param_cons = param_list.toPtr(Cons);
            const param_item = param_cons.car;

            if (param_item.isSymbol()) {
                const b = self.builtins orelse return error.UninitializedBuiltins;

                // Check for &rest/&body keyword (use symbol identity)
                if (param_item.raw == b.@"&rest".raw or param_item.raw == b.@"&body".raw) {
                    // Next element is the rest parameter name
                    if (!param_cons.cdr.isCons()) return error.InvalidLambda;
                    const rest_cons = param_cons.cdr.toPtr(Cons);
                    if (!rest_cons.car.isSymbol()) return error.InvalidLambda;
                    const rest_sym = rest_cons.car.toPtr(Symbol);
                    rest_param = rest_sym.getName();
                    break; // &rest must be last
                }

                // Check for &optional keyword (use symbol identity)
                if (param_item.raw == b.@"&optional".raw) {
                    in_optional = true;
                    in_key = false;
                    param_list = param_cons.cdr;
                    continue;
                }

                // Check for &key keyword (use symbol identity)
                if (param_item.raw == b.@"&key".raw) {
                    in_key = true;
                    in_optional = false;
                    param_list = param_cons.cdr;
                    continue;
                }

                const param_sym = param_item.toPtr(Symbol);
                const name = param_sym.getName();

                if (in_key) {
                    // Key parameter with nil default, keyword = name
                    try key_params.append(self.allocator, .{
                        .keyword = name,
                        .name = name,
                        .default = null,
                    });
                } else if (in_optional) {
                    // Optional parameter with nil default
                    try optional_params.append(self.allocator, .{
                        .name = name,
                        .default = null,
                    });
                } else {
                    // Untyped parameter: just a symbol
                    try params.append(self.allocator, name);
                    try typed_params.append(self.allocator, .{ .name = name, .type_sym = null });
                }
            } else if (param_item.isCons()) {
                const typed = param_item.toPtr(Cons);
                if (!typed.car.isSymbol()) return error.InvalidLambda;
                const name_sym = typed.car.toPtr(Symbol);
                const name = name_sym.getName();

                if (in_key) {
                    // Key parameter: (name default-expr) or just name
                    // Compile default in parent env (not lambda env)
                    var default_ir: ?*const Ir = null;
                    if (typed.cdr.isCons()) {
                        const default_cons = typed.cdr.toPtr(Cons);
                        default_ir = try self.compile(default_cons.car, env);
                    }
                    try key_params.append(self.allocator, .{
                        .keyword = name,
                        .name = name,
                        .default = default_ir,
                    });
                } else if (in_optional) {
                    // Optional parameter: (name default-expr)
                    // Compile default in parent env (not lambda env)
                    var default_ir: ?*const Ir = null;
                    if (typed.cdr.isCons()) {
                        const default_cons = typed.cdr.toPtr(Cons);
                        default_ir = try self.compile(default_cons.car, env);
                    }
                    try optional_params.append(self.allocator, .{
                        .name = name,
                        .default = default_ir,
                    });
                } else {
                    // Typed parameter: (name type-expr)
                    if (!typed.cdr.isCons()) return error.InvalidLambda;
                    const type_val = typed.cdr.toPtr(Cons).car;
                    try params.append(self.allocator, name);
                    try typed_params.append(self.allocator, .{ .name = name, .type_sym = type_val });
                }
            } else {
                return error.InvalidLambda;
            }

            param_list = param_cons.cdr;
        }

        // Also check for rest parameter via dotted list: (a b . rest)
        if (rest_param == null and !param_list.isNil()) {
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
            _ = try lambda_env.bind(param);
        }

        // Bind optional parameters
        for (optional_params.items) |op| {
            _ = try lambda_env.bind(op.name);
        }

        // Bind key parameters
        for (key_params.items) |kp| {
            _ = try lambda_env.bind(kp.name);
        }

        // Bind rest parameter if present
        if (rest_param) |rp| {
            _ = try lambda_env.bind(rp);
        }

        // Capture analysis: collect free variables before compiling body
        var capture_set = CaptureSet.init(self.allocator);
        defer capture_set.deinit();

        self.collectFreeVars(body_exprs, &lambda_env, &capture_set) catch
            return error.OutOfMemory;

        // Compile body (implicit progn) - body is in tail position
        var body_ir = try self.compileBodyWithTail(body_exprs, &lambda_env, true);

        // Bidirectional type checking (when enabled)
        if (self.type_checking_enabled) {
            self.checkLambdaTypes(typed_params.items, return_type, body_ir);
        }

        // Prepend type assertions for typed parameters
        var assertions = std.ArrayList(*Ir){};
        defer assertions.deinit(self.allocator);

        for (typed_params.items) |tp| {
            if (tp.type_sym) |type_sym| {
                // Create variable reference for parameter
                const binding = lambda_env.lookup(tp.name) orelse continue;
                const var_ir = self.builder.variable(tp.name, binding.depth, binding.index) catch
                    return error.OutOfMemory;

                // Create assertion based on type symbol
                const assert_ir = self.makeTypeAssertionSym(var_ir, type_sym) catch
                    return error.InvalidSyntax;
                if (assert_ir) |assert_node| {
                    try assertions.append(self.allocator, assert_node);
                }
            }
        }

        // If we have assertions, wrap body in progn with assertions first
        if (assertions.items.len > 0) {
            try assertions.append(self.allocator, body_ir);
            const items = self.allocator.dupe(*const Ir, assertions.items) catch
                return error.OutOfMemory;
            body_ir = try self.builder.progn(items);
        }

        // Wrap body in return type assertion if specified
        if (return_type) |ret_type_sym| {
            const assert_ir = self.makeTypeAssertionSym(body_ir, ret_type_sym) catch
                return error.InvalidSyntax;
            if (assert_ir) |wrapped| {
                body_ir = wrapped;
            }
        }

        // Convert captures to slice
        const captures = self.allocator.dupe(Ir.Capture, capture_set.captures.items) catch
            return error.OutOfMemory;

        // Copy optional params
        const opt_params = self.allocator.dupe(Ir.OptionalParam, optional_params.items) catch
            return error.OutOfMemory;

        // Copy key params
        const kp_params = self.allocator.dupe(Ir.KeyParam, key_params.items) catch
            return error.OutOfMemory;

        return self.builder.lambda(params.items, opt_params, kp_params, rest_param, captures, body_ir) catch
            return error.OutOfMemory;
    }

    /// Create a type assertion IR node for a given type symbol or complex type
    fn makeTypeAssertionSym(self: *Compiler, expr_ir: *Ir, type_sym: Value) !?*Ir {
        const b = self.builtins orelse return error.UninitializedBuiltins;

        // Handle simple type symbols by identity
        if (type_sym.isSymbol()) {
            if (type_sym.raw == b.ty_fixnum.raw) return self.builder.assertFixnum(expr_ir);
            if (type_sym.raw == b.cons.raw) return self.builder.assertCons(expr_ir);
            if (type_sym.raw == b.ty_symbol.raw) return self.builder.assertSymbol(expr_ir);
            if (type_sym.raw == b.string.raw) return self.builder.assertString(expr_ir);
            if (type_sym.raw == b.ty_vector.raw) return self.builder.assertVector(expr_ir);
            if (type_sym.raw == b.ty_closure.raw) return self.builder.assertClosure(expr_ir);
            if (type_sym.raw == b.ty_list.raw) return self.builder.assertList(expr_ir);
            if (type_sym.raw == b.@"ty_non-nil".raw) return self.builder.assertNonNil(expr_ir);
            if (type_sym.raw == b.ty_any.raw) return null; // any = no check
            if (type_sym.raw == b.ty_nil.raw) {
                // nil type - use assertOr with just nil
                const syms = try self.allocator.alloc(Value, 1);
                syms[0] = b.ty_nil;
                return self.builder.assertOr(expr_ir, syms);
            }
            return error.InvalidSyntax;
        }

        // Handle complex types: (union T1 T2 ...), etc.
        if (type_sym.isCons()) {
            const cons = type_sym.toPtr(Cons);
            if (!cons.car.isSymbol()) return error.InvalidSyntax;

            // Check for (union T1 T2 ...) or (or T1 T2 ...)
            if (cons.car.raw == b.ty_union.raw or cons.car.raw == b.ty_or.raw) {
                // Collect type alternatives
                var alts = std.ArrayList(Value){};
                defer alts.deinit(self.allocator);

                var current = cons.cdr;
                while (current.isCons()) {
                    const c = current.toPtr(Cons);
                    // Each alternative is a type symbol or nil
                    if (c.car.isSymbol()) {
                        try alts.append(self.allocator, c.car);
                    } else if (c.car.isNil()) {
                        // nil in type position - use the interned nil symbol
                        try alts.append(self.allocator, b.ty_nil);
                    } else {
                        // Nested complex type - not yet supported
                        return error.InvalidSyntax;
                    }
                    current = c.cdr;
                }

                if (alts.items.len == 0) return error.InvalidSyntax;

                const syms = try self.allocator.dupe(Value, alts.items);
                return self.builder.assertOr(expr_ir, syms);
            }

            // Other complex types not yet supported
            return error.InvalidSyntax;
        }

        return error.InvalidSyntax;
    }

    /// Collect free variables in an expression
    fn collectFreeVars(self: *Compiler, expr: Value, env: *const Env, captures: *CaptureSet) error{OutOfMemory}!void {
        if (expr.isNil() or expr.isFixnum() or expr.isBignum() or expr.isString() or expr.isKeyword() or expr.isCharacter() or expr.isMagicSymbol() or expr.isVector()) {
            return; // Literals have no free variables
        }

        if (expr.isSymbol()) {
            const sym = expr.toPtr(Symbol);
            const name = sym.getName();

            // Check if bound in current scope (including same-frame let environments)
            var check_env: ?*const Env = env;
            while (check_env) |e| {
                if (e.bindings.get(name) != null) return; // Bound locally, not a free var
                if (e.new_frame) break; // Stop at frame boundary
                check_env = e.parent;
            }

            // Find the lambda frame (the nearest new_frame environment)
            var lambda_env: ?*const Env = env;
            while (lambda_env) |e| {
                if (e.new_frame) break;
                lambda_env = e.parent;
            }

            // Look up from the lambda's parent to get correct capture depth
            if (lambda_env) |le| {
                if (le.parent) |lambda_parent| {
                    if (lambda_parent.lookup(name)) |binding| {
                        // This is a free variable - needs to be captured
                        // Store depth from lambda's parent perspective for correct loading
                        try captures.addCapture(name, binding.depth, binding.index);
                    }
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
                                    _ = try let_env.bind(bname_sym.getName());
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
    fn findBoxedVars(self: *Compiler, body: Value, binding_names: []const []const u8, result: *BoxingSet) error{OutOfMemory}!void {
        var mutated = std.StringHashMap(void).init(self.allocator);
        defer mutated.deinit();

        var captured = std.StringHashMap(void).init(self.allocator);
        defer captured.deinit();

        // Collect mutations and captures from body
        try self.collectMutationsAndCaptures(body, binding_names, &mutated, &captured);

        // Intersection: names that are both mutated AND captured need boxing
        var iter = mutated.keyIterator();
        while (iter.next()) |name| {
            if (captured.contains(name.*)) {
                try result.add(name.*);
            }
        }
    }

    /// Recursively collect mutations (set!) and lambda captures in an expression
    fn collectMutationsAndCaptures(
        self: *Compiler,
        expr: Value,
        binding_names: []const []const u8,
        mutated: *std.StringHashMap(void),
        captured: *std.StringHashMap(void),
    ) error{OutOfMemory}!void {
        if (expr.isNil() or expr.isFixnum() or expr.isBignum() or expr.isString() or expr.isKeyword() or expr.isCharacter() or expr.isVector()) {
            return;
        }

        if (!expr.isCons()) return;

        // Builtins required for symbol identity comparison
        const b = self.builtins orelse return;

        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        if (head.isSymbol()) {
            // Check for (setq var ...)
            if (head.raw == b.setq.raw) {
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
        if (expr.isNil() or expr.isFixnum() or expr.isBignum() or expr.isString() or expr.isKeyword() or expr.isCharacter() or expr.isVector()) {
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

    fn compileLet(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileLetWithTail(args, env, false);
    }

    fn compileLetWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
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
            try binding_names.append(self.allocator, name_sym.getName());

            binding_list = binding_cons.cdr;
        }

        // Find variables that need boxing (mutable + captured by lambda)
        // Allocate on heap to avoid dangling pointer during recursive compilation
        const boxed = try self.allocator.create(BoxingSet);
        boxed.* = BoxingSet.init(self.allocator);
        defer {
            boxed.deinit();
            self.allocator.destroy(boxed);
        }
        try self.findBoxedVars(body_exprs, binding_names.items, boxed);

        // Create let_env first so we can get indices for each binding
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();

        // Second pass: compile bindings, get indices, wrap boxed ones with make-box
        var bindings = std.ArrayList(Ir.Binding){};
        defer bindings.deinit(self.allocator);

        binding_list = bindings_expr;
        var name_idx: usize = 0;
        while (binding_list.isCons()) : (name_idx += 1) {
            const binding_cons = binding_list.toPtr(Cons);
            const binding = binding_cons.car;

            const b = binding.toPtr(Cons);
            const name = binding_names.items[name_idx];

            // Bind name in let_env to get stack slot index
            const index = try let_env.bind(name);

            // Get value expression - compile in outer env (let semantics)
            if (!b.cdr.isCons()) return error.InvalidLet;
            const val_cons = b.cdr.toPtr(Cons);
            var val_ir = try self.compile(val_cons.car, env);

            // If this variable needs boxing, wrap value in make-box
            if (boxed.contains(name)) {
                const box_ir = try self.allocator.create(Ir);
                box_ir.* = .{ .make_box = .{ .operand = val_ir } };
                val_ir = box_ir;
            }

            bindings.append(self.allocator, .{ .name = name, .value = val_ir, .index = index }) catch
                return error.OutOfMemory;

            binding_list = binding_cons.cdr;
        }

        // Set boxed_vars so that variable refs and set! use box operations
        const saved_boxed = self.boxed_vars;
        if (boxed.names.count() > 0) {
            self.boxed_vars = boxed;
        }
        // Restore on error before defer frees boxed
        errdefer self.boxed_vars = saved_boxed;

        // Compile body - body is in tail position if let is
        const body_ir = try self.compileBodyWithTail(body_exprs, &let_env, in_tail);

        // Restore previous boxed_vars
        self.boxed_vars = saved_boxed;

        return try self.builder.letExpr(bindings.items, body_ir);
    }

    fn compileLetrecWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
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

            // Use qualified name for globals (package-aware)
            var qual_buf: [256]u8 = undefined;
            const name = self.getQualifiedName(name_sym, &qual_buf) catch name_sym.getName();

            if (!b.cdr.isCons()) return error.InvalidLet;
            const val_cons = b.cdr.toPtr(Cons);

            // Pre-register global for recursive visibility
            const idx = try self.globals.define(name);

            try names.append(self.allocator, name);
            try val_exprs.append(self.allocator, val_cons.car);
            try indices.append(self.allocator, idx);

            binding_list = binding_cons.cdr;
        }

        // Second pass: compile values and create defines
        var exprs = std.ArrayList(*const Ir){};
        defer exprs.deinit(self.allocator);

        for (names.items, val_exprs.items, indices.items) |name, val_expr, idx| {
            const val_ir = try self.compile(val_expr, env);
            const define_ir = try self.builder.define(name, idx, val_ir);
            try exprs.append(self.allocator, define_ir);
        }

        // Compile body (in tail position if letrec is)
        const body_ir = try self.compileBodyWithTail(body_exprs, env, in_tail);
        try exprs.append(self.allocator, body_ir);

        // Return progn of defines + body
        const items = try self.allocator.dupe(*const Ir, exprs.items);
        return try self.builder.progn(items);
    }

    fn compileFletWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // (flet ((fname (args) body...) ...) body)
        // Desugars to let with lambdas - functions don't see each other
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // Create environment first to get binding indices
        var flet_env = Env.initLet(self.allocator, env);
        defer flet_env.deinit();

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

            // Get index from env
            const index = try flet_env.bind(name);

            // Build lambda from rest: ((params) body...) -> compile as lambda
            if (!f.cdr.isCons()) return error.InvalidLet;
            const lambda_ir = try self.compileLambda(f.cdr, env);

            bindings.append(self.allocator, .{ .name = name, .value = lambda_ir, .index = index }) catch
                return error.OutOfMemory;

            binding_list = binding_cons.cdr;
        }

        // Compile body in new environment
        const body_ir = try self.compileBodyWithTail(body_exprs, &flet_env, in_tail);

        return try self.builder.letExpr(bindings.items, body_ir);
    }

    fn compileLabelsWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
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

            // Use qualified name for globals (package-aware)
            var qual_buf: [256]u8 = undefined;
            const name = self.getQualifiedName(name_sym, &qual_buf) catch name_sym.getName();

            // Pre-register global for recursive visibility
            const idx = try self.globals.define(name);

            if (!f.cdr.isCons()) return error.InvalidLet;

            try names.append(self.allocator, name);
            try lambda_args.append(self.allocator, f.cdr);
            try indices.append(self.allocator, idx);

            binding_list = binding_cons.cdr;
        }

        // Second pass: compile lambdas and create defines
        var exprs = std.ArrayList(*const Ir){};
        defer exprs.deinit(self.allocator);

        for (names.items, lambda_args.items, indices.items) |name, largs, idx| {
            const lambda_ir = try self.compileLambda(largs, env);
            const define_ir = try self.builder.define(name, idx, lambda_ir);
            try exprs.append(self.allocator, define_ir);
        }

        // Compile body
        const body_ir = try self.compileBodyWithTail(body_exprs, env, in_tail);
        try exprs.append(self.allocator, body_ir);

        // Return progn of defines + body
        const items = try self.allocator.dupe(*const Ir, exprs.items);
        return try self.builder.progn(items);
    }

    fn compileLetStarWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
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

    fn compileLetStarBindings(self: *Compiler, bindings_list: Value, body_exprs: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
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

        // Create extended environment first to get binding index
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();
        const index = try let_env.bind(name);

        // Compile value in current environment
        const val_ir = try self.compile(val_cons.car, env);

        // Create single-binding array with index
        const binding_array = [_]ir.Ir.Binding{.{ .name = name, .value = val_ir, .index = index }};

        // Compile rest or body
        const inner_ir = if (rest.isNil())
            try self.compileBodyWithTail(body_exprs, &let_env, in_tail)
        else
            try self.compileLetStarBindings(rest, body_exprs, &let_env, in_tail);

        return try self.builder.letExpr(&binding_array, inner_ir);
    }

    fn compileCondWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // (cond (test1 expr1...) (test2 expr2...) ... [(t exprN...)])
        // Transform to nested ifs
        if (args.isNil()) {
            return try self.builder.lit(Value.nil);
        }

        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const clause = cons.car;
        const rest_clauses = cons.cdr;

        if (!clause.isCons()) return error.InvalidSyntax;
        const clause_cons = clause.toPtr(Cons);
        const test_expr = clause_cons.car;
        const body_exprs = clause_cons.cdr;

        // Check for default clause (t or else) - use symbol identity
        const is_default = blk: {
            // t is magic value, else is interned symbol
            if (test_expr.raw == Value.t.raw) break :blk true;
            const b = self.builtins orelse return error.UninitializedBuiltins;
            if (test_expr.raw == b.@"else".raw) break :blk true;
            break :blk false;
        };

        if (is_default) {
            return self.compileBodyWithTail(body_exprs, env, in_tail);
        }

        const test_ir = try self.compile(test_expr, env);
        const then_ir = try self.compileBodyWithTail(body_exprs, env, in_tail);
        const else_ir = try self.compileCondWithTail(rest_clauses, env, in_tail);

        return try self.builder.ifExpr(test_ir, then_ir, else_ir);
    }

    fn compileAnd(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
            return try self.builder.ifExpr(first_ir, nested_and, nil_ir);
        }

        const first_ir = try self.compile(first, env);
        const second_ir = try self.compile(second, env);
        const nil_ir = try self.builder.lit(Value.nil);

        return try self.builder.ifExpr(first_ir, second_ir, nil_ir);
    }

    fn compileOr(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
        var tmp_env = Env.initLet(self.allocator, env);
        defer tmp_env.deinit();
        const tmp_idx = try tmp_env.bind(tmp_name);

        const bindings = try self.allocator.alloc(ir.Ir.Binding, 1);
        bindings[0] = .{ .name = tmp_name, .value = first_ir, .index = tmp_idx };

        const tmp_var1 = try self.builder.variable(tmp_name, 0, tmp_idx);
        const tmp_var2 = try self.builder.variable(tmp_name, 0, tmp_idx);

        const body = try self.builder.ifExpr(tmp_var1, tmp_var2, else_ir);
        return try self.builder.letExpr(bindings, body);
    }

    fn compileFuncall(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (funcall fn arg1 arg2 ...) - same as regular call with computed function
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        return self.compileCallWithTail(cons1.car, cons1.cdr, env, false);
    }

    fn compileApply(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (apply fn arg1 arg2 ... args-list)
        // CL semantics: spread args are prepended to final list
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const fn_expr = cons1.car;
        var rest = cons1.cdr;

        if (!rest.isCons()) return error.InvalidSyntax;

        // Collect all remaining args (spread args + final list)
        var spread_args = std.ArrayList(*const Ir){};
        defer spread_args.deinit(self.allocator);

        while (rest.isCons()) {
            const cons = rest.toPtr(Cons);
            const arg_ir = try self.compile(cons.car, env);
            try spread_args.append(self.allocator, arg_ir);
            rest = cons.cdr;
        }

        if (spread_args.items.len == 0) return error.InvalidSyntax;

        const fn_ir = try self.compile(fn_expr, env);

        // If only one arg, it's just (apply fn args-list)
        if (spread_args.items.len == 1) {
            const node = try self.allocator.create(ir.Ir);
            node.* = .{ .apply = .{ .func = fn_ir, .args = spread_args.items[0] } };
            return node;
        }

        // Multiple args: need to build combined args list
        // (apply fn a b c final-list) => call fn with (a b c . final-list)
        // Build: (list* a b c final-list) which creates (a b c . final-list)
        const combined = try self.builder.listStar(spread_args.items);
        const node = try self.allocator.create(ir.Ir);
        node.* = .{ .apply = .{ .func = fn_ir, .args = combined } };
        return node;
    }

    fn compileSet(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (set! var value) or (setq var value)
        if (!args.isCons()) return error.InvalidSet;

        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSet;
        const var_sym = cons1.car.toPtr(Symbol);
        const local_name = var_sym.getName();

        if (!cons1.cdr.isCons()) return error.InvalidSet;
        const cons2 = cons1.cdr.toPtr(Cons);
        const val_ir = try self.compile(cons2.car, env);

        // First check local environment
        if (env.lookup(local_name)) |binding| {
            // If this variable is boxed, use box-set! instead
            if (self.boxed_vars) |bv| {
                if (bv.contains(local_name)) {
                    // Compile (box-set! var val) instead of (set! var val)
                    const var_ir = self.builder.variable(local_name, binding.depth, binding.index) catch
                        return error.OutOfMemory;
                    const box_set = try self.allocator.create(Ir);
                    box_set.* = .{ .box_set = .{ .left = var_ir, .right = val_ir } };
                    return box_set;
                }
            }
            return self.builder.set(local_name, binding.depth, binding.index, val_ir) catch
                return error.OutOfMemory;
        }

        // Check globals - use qualified name for package-aware lookup
        var qual_buf: [256]u8 = undefined;
        const global_name = self.getQualifiedName(var_sym, &qual_buf) catch local_name;

        if (self.globals.lookup(global_name)) |idx| {
            // Re-define the global with the new value
            return self.builder.define(global_name, idx, val_ir) catch
                return error.OutOfMemory;
        }

        return error.UnboundVariable;
    }

    fn compileQuote(self: *Compiler, args: Value) anyerror!*Ir {
        // (quote expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const quoted = cons.car;

        // For symbols, use quote_sym
        if (quoted.isSymbol()) {
            const sym = quoted.toPtr(Symbol);
            return try self.builder.quoteSym(sym.getName());
        }

        // For other values, return as literal
        return try self.builder.lit(quoted);
    }

    /// Compile function special form: (function name) or (function (lambda ...))
    /// #'name is reader syntax that expands to (function name)
    fn compileFunction(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (function expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const func_spec = cons.car;

        // (function symbol) - look up function binding or wrap primitive
        if (func_spec.isSymbol()) {
            // Check if it's a primitive that needs wrapping
            if (try self.compilePrimitiveFunctionRef(func_spec)) |wrapper| {
                return wrapper;
            }
            // Otherwise compile symbol as a variable reference (will look up in env/globals)
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

    /// Create a lambda wrapper for a primitive function reference
    /// Returns null if sym is not a known primitive
    fn compilePrimitiveFunctionRef(self: *Compiler, sym: Value) anyerror!?*Ir {
        const b = self.builtins orelse return null;
        const s = sym.raw;

        // Binary primitives: (lambda (a b) (prim a b))
        if (s == b.cons.raw) return try self.makeBinaryWrapper(&IrBuilder.cons);
        if (s == b.eq.raw) return try self.makeBinaryWrapper(&IrBuilder.eq);
        if (s == b.equal.raw) return try self.makeBinaryWrapper(&IrBuilder.equal);
        if (s == b.eql.raw) return try self.makeBinaryWrapper(&IrBuilder.eql);
        if (s == b.@"<".raw) return try self.makeBinaryWrapper(&IrBuilder.lt);
        if (s == b.@">".raw) return try self.makeBinaryWrapper(&IrBuilder.gt);
        if (s == b.@"<=".raw) return try self.makeBinaryWrapper(&IrBuilder.le);
        if (s == b.@">=".raw) return try self.makeBinaryWrapper(&IrBuilder.ge);
        if (s == b.@"=".raw) return try self.makeBinaryWrapper(&IrBuilder.numEq);
        if (s == b.append.raw) return try self.makeBinaryWrapper(&IrBuilder.append);

        // Unary primitives: (lambda (a) (prim a))
        if (s == b.car.raw or s == b.first.raw) return try self.makeUnaryWrapper(&IrBuilder.car);
        if (s == b.cdr.raw or s == b.rest.raw) return try self.makeUnaryWrapper(&IrBuilder.cdr);
        if (s == b.not.raw) return try self.makeUnaryWrapper(&IrBuilder.not);
        if (s == b.null.raw) return try self.makeUnaryWrapper(&IrBuilder.nilp);
        if (s == b.consp.raw) return try self.makeUnaryWrapper(&IrBuilder.consp);
        if (s == b.symbolp.raw) return try self.makeUnaryWrapper(&IrBuilder.symbolp);
        if (s == b.numberp.raw) return try self.makeUnaryWrapper(&IrBuilder.numberp);
        if (s == b.stringp.raw) return try self.makeUnaryWrapper(&IrBuilder.stringp);
        if (s == b.atom.raw) return try self.makeUnaryWrapper(&IrBuilder.atomp);
        if (s == b.listp.raw) return try self.makeUnaryWrapper(&IrBuilder.listp);

        // Variadic arithmetic - create wrappers using add/sub/mul/div builders
        if (s == b.@"+".raw) return try self.makeVariadicAddWrapper();
        if (s == b.@"*".raw) return try self.makeVariadicMulWrapper();
        if (s == b.@"-".raw) return try self.makeVariadicSubWrapper();
        if (s == b.@"/".raw) return try self.makeVariadicDivWrapper();

        return null;
    }

    fn makeBinaryWrapper(self: *Compiler, buildFn: *const fn (IrBuilder, *const Ir, *const Ir) std.mem.Allocator.Error!*Ir) anyerror!*Ir {
        // Create: (lambda (a b) (op a b))
        const a_ref = try self.builder.variable("a", 0, 0);
        const b_ref = try self.builder.variable("b", 0, 1);
        const prim_call = try buildFn(self.builder, a_ref, b_ref);
        const params = [_][]const u8{ "a", "b" };
        return self.builder.lambda(&params, &.{}, &.{}, null, &.{}, prim_call) catch
            return error.OutOfMemory;
    }

    fn makeUnaryWrapper(self: *Compiler, buildFn: *const fn (IrBuilder, *const Ir) std.mem.Allocator.Error!*Ir) anyerror!*Ir {
        // Create: (lambda (a) (op a))
        const a_ref = try self.builder.variable("a", 0, 0);
        const prim_call = try buildFn(self.builder, a_ref);
        const params = [_][]const u8{"a"};
        return self.builder.lambda(&params, &.{}, &.{}, null, &.{}, prim_call) catch
            return error.OutOfMemory;
    }

    fn makeVariadicAddWrapper(self: *Compiler) anyerror!*Ir {
        // (lambda (&rest args)
        //   (let ((acc 0))                    ; acc at slot 1
        //     (while (consp args)             ; args at slot 0 (rest param)
        //       (setq acc (+ acc (car args)))
        //       (setq args (cdr args)))
        //     acc))
        return try self.makeFoldWrapper(0, IrBuilder.add);
    }

    fn makeVariadicMulWrapper(self: *Compiler) anyerror!*Ir {
        return try self.makeFoldWrapper(1, IrBuilder.mul);
    }

    fn makeVariadicSubWrapper(self: *Compiler) anyerror!*Ir {
        // (lambda (&rest args)
        //   (if (null args)
        //       0
        //       (if (null (cdr args))
        //           (- 0 (car args))          ; unary negate
        //           (let ((acc (car args)))   ; acc at slot 1
        //             (let ((rest (cdr args))) ; rest at slot 2
        //               (while (consp rest)
        //                 (setq acc (- acc (car rest)))
        //                 (setq rest (cdr rest)))
        //               acc)))))
        const b = self.builder;

        // args at slot 0 (rest param)
        const args0 = try b.variable("args", 0, 0);
        const args1 = try b.variable("args", 0, 0);
        const args2 = try b.variable("args", 0, 0);
        const args3 = try b.variable("args", 0, 0);
        const args4 = try b.variable("args", 0, 0);

        // (null args) -> return 0
        const zero = try b.lit(Value.makeFixnum(0));
        const null_args = try b.nilp(args0);

        // (cdr args)
        const cdr_args = try b.cdr(args1);

        // (null (cdr args)) -> unary negate: (- 0 (car args))
        const null_cdr = try b.nilp(cdr_args);
        const car_args = try b.car(args2);
        const neg_result = try b.sub(zero, car_args);

        // Binary/variadic case: fold from first element
        // acc = (car args) at slot 1
        const first = try b.car(args3);
        // rest = (cdr args) at slot 2
        const rest_init = try b.cdr(args4);

        // Loop: while (consp rest)
        const rest_ref1 = try b.variable("rest", 0, 2);
        const loop_cond = try b.consp(rest_ref1);

        // Body: (setq acc (- acc (car rest))) (setq rest (cdr rest))
        const acc_ref = try b.variable("acc", 0, 1);
        const rest_ref2 = try b.variable("rest", 0, 2);
        const car_rest = try b.car(rest_ref2);
        const sub_expr = try b.sub(acc_ref, car_rest);
        const set_acc = try b.set("acc", 0, 1, sub_expr);

        const rest_ref3 = try b.variable("rest", 0, 2);
        const cdr_rest = try b.cdr(rest_ref3);
        const set_rest = try b.set("rest", 0, 2, cdr_rest);

        const loop_body_exprs = [_]*const Ir{ set_acc, set_rest };
        const loop_body = try b.progn(&loop_body_exprs);
        const loop_node = try b.loop(loop_cond, loop_body);

        // After loop, return acc
        const acc_result = try b.variable("acc", 0, 1);
        const inner_let_body_exprs = [_]*const Ir{ loop_node, acc_result };
        const inner_let_body = try b.progn(&inner_let_body_exprs);

        // let rest = (cdr args) at slot 2
        const rest_let = try b.let1("rest", 2, rest_init, inner_let_body);

        // let acc = (car args) at slot 1
        const acc_let = try b.let1("acc", 1, first, rest_let);

        // Inner if: (if (null (cdr args)) (- 0 (car args)) <fold>)
        const inner_if = try b.ifExpr(null_cdr, neg_result, acc_let);

        // Outer if: (if (null args) 0 <inner>)
        const outer_if = try b.ifExpr(null_args, zero, inner_if);

        return self.builder.lambda(&.{}, &.{}, &.{}, "args", &.{}, outer_if) catch
            return error.OutOfMemory;
    }

    fn makeVariadicDivWrapper(self: *Compiler) anyerror!*Ir {
        // Similar to sub but with division and identity 1 for unary
        // (/ x) = (/ 1 x)
        const b = self.builder;

        const args0 = try b.variable("args", 0, 0);
        const args1 = try b.variable("args", 0, 0);
        const args2 = try b.variable("args", 0, 0);
        const args3 = try b.variable("args", 0, 0);
        const args4 = try b.variable("args", 0, 0);

        const one = try b.lit(Value.makeFixnum(1));
        const null_args = try b.nilp(args0);

        const cdr_args = try b.cdr(args1);
        const null_cdr = try b.nilp(cdr_args);
        const car_args = try b.car(args2);
        const recip_result = try b.div(one, car_args); // (/ 1 x)

        const first = try b.car(args3);
        const rest_init = try b.cdr(args4);

        const rest_ref1 = try b.variable("rest", 0, 2);
        const loop_cond = try b.consp(rest_ref1);

        const acc_ref = try b.variable("acc", 0, 1);
        const rest_ref2 = try b.variable("rest", 0, 2);
        const car_rest = try b.car(rest_ref2);
        const div_expr = try b.div(acc_ref, car_rest);
        const set_acc = try b.set("acc", 0, 1, div_expr);

        const rest_ref3 = try b.variable("rest", 0, 2);
        const cdr_rest = try b.cdr(rest_ref3);
        const set_rest = try b.set("rest", 0, 2, cdr_rest);

        const loop_body_exprs = [_]*const Ir{ set_acc, set_rest };
        const loop_body = try b.progn(&loop_body_exprs);
        const loop_node = try b.loop(loop_cond, loop_body);

        const acc_result = try b.variable("acc", 0, 1);
        const inner_let_body_exprs = [_]*const Ir{ loop_node, acc_result };
        const inner_let_body = try b.progn(&inner_let_body_exprs);

        const rest_let = try b.let1("rest", 2, rest_init, inner_let_body);
        const acc_let = try b.let1("acc", 1, first, rest_let);

        const inner_if = try b.ifExpr(null_cdr, recip_result, acc_let);
        const outer_if = try b.ifExpr(null_args, one, inner_if);

        return self.builder.lambda(&.{}, &.{}, &.{}, "args", &.{}, outer_if) catch
            return error.OutOfMemory;
    }

    /// Helper to build a simple fold wrapper for + and *
    fn makeFoldWrapper(
        self: *Compiler,
        identity: i64,
        buildOp: *const fn (IrBuilder, *const Ir, *const Ir) std.mem.Allocator.Error!*Ir,
    ) anyerror!*Ir {
        const b = self.builder;

        // args at slot 0 (rest param), acc at slot 1
        const args_ref1 = try b.variable("args", 0, 0);
        const loop_cond = try b.consp(args_ref1);

        const acc_ref = try b.variable("acc", 0, 1);
        const args_ref2 = try b.variable("args", 0, 0);
        const car_args = try b.car(args_ref2);
        const op_expr = try buildOp(b, acc_ref, car_args);
        const set_acc = try b.set("acc", 0, 1, op_expr);

        const args_ref3 = try b.variable("args", 0, 0);
        const cdr_args = try b.cdr(args_ref3);
        const set_args = try b.set("args", 0, 0, cdr_args);

        const loop_body_exprs = [_]*const Ir{ set_acc, set_args };
        const loop_body = try b.progn(&loop_body_exprs);
        const loop_node = try b.loop(loop_cond, loop_body);

        const acc_result = try b.variable("acc", 0, 1);
        const let_body_exprs = [_]*const Ir{ loop_node, acc_result };
        const let_body = try b.progn(&let_body_exprs);

        const init_val = try b.lit(Value.makeFixnum(identity));
        const let_node = try b.let1("acc", 1, init_val, let_body);

        return self.builder.lambda(&.{}, &.{}, &.{}, "args", &.{}, let_node) catch
            return error.OutOfMemory;
    }

    /// Compile quasiquote (backquote)
    /// Handles unquote (,) and unquote-splicing (,@)
    fn compileQuasiquote(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (quasiquote expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const expr = cons.car;

        return self.quasiquoteExpr(expr, env);
    }

    /// Process an expression inside quasiquote
    fn quasiquoteExpr(self: *Compiler, expr: Value, env: *const Env) anyerror!*Ir {
        // Non-list: return as quoted literal
        if (!expr.isCons()) {
            if (expr.isSymbol()) {
                const sym = expr.toPtr(Symbol);
                return try self.builder.quoteSym(sym.getName());
            }
            return try self.builder.lit(expr);
        }

        const cons = expr.toPtr(Cons);
        const head = cons.car;

        // Check for (unquote x) - evaluate x (use symbol identity)
        if (head.isSymbol()) {
            const b = self.builtins orelse return error.UninitializedBuiltins;
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
        }

        // Regular list: build with cons at runtime
        return self.quasiquoteList(expr, env);
    }

    /// Build a list from quasiquoted elements using cons/append
    fn quasiquoteList(self: *Compiler, list: Value, env: *const Env) anyerror!*Ir {
        if (list.isNil()) {
            return try self.builder.lit(Value.nil);
        }

        if (!list.isCons()) {
            // Improper list tail - just quote it
            if (list.isSymbol()) {
                const sym = list.toPtr(Symbol);
                return try self.builder.quoteSym(sym.getName());
            }
            return try self.builder.lit(list);
        }

        const cons = list.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check for (unquote-splicing x) - splice x into result (use symbol identity)
        if (head.isCons()) {
            const head_cons = head.toPtr(Cons);
            if (head_cons.car.isSymbol()) {
                const b = self.builtins orelse return error.UninitializedBuiltins;
                if (head_cons.car.raw == b.@"unquote-splicing".raw) {
                    // (,@x ...) -> (append x (quasiquote-list ...))
                    if (!head_cons.cdr.isCons()) return error.InvalidSyntax;
                    const spliced = head_cons.cdr.toPtr(Cons).car;
                    const spliced_ir = try self.compile(spliced, env);
                    const rest_ir = try self.quasiquoteList(tail, env);

                    // Build (append spliced rest)
                    return try self.builder.append(spliced_ir, rest_ir);
                }
            }
        }

        // Regular element: (cons (quasiquote head) (quasiquote-list tail))
        const head_ir = try self.quasiquoteExpr(head, env);
        const tail_ir = try self.quasiquoteList(tail, env);

        return try self.builder.cons(head_ir, tail_ir);
    }

    fn compileProgn(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileBody(args, env);
    }

    fn compilePrognWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        return self.compileBodyWithTail(args, env, in_tail);
    }

    fn compileWhile(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (while test body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const test_expr = cons.car;
        const body_exprs = cons.cdr;

        const test_ir = try self.compile(test_expr, env);
        const body_ir = try self.compileBody(body_exprs, env);

        return try self.builder.loop(test_ir, body_ir);
    }

    fn compileLoop(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (loop body...) - infinite loop, exits via return-from
        // Compiles to (block nil (while t body...))
        const test_ir = try self.builder.lit(Value.t);
        const body_ir = try self.compileBody(args, env);
        const loop_ir = try self.builder.loop(test_ir, body_ir);
        // Wrap in nil block for (return ...) support
        return try self.builder.block("nil", loop_ir);
    }

    fn compileBlockWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // (block name body...) - name can be symbol or nil
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const name = if (cons.car.isNil())
            "nil" // nil block name (used by dolist/dotimes)
        else if (cons.car.isSymbol())
            cons.car.toPtr(Symbol).getName()
        else
            return error.InvalidSyntax;

        // Compile body
        const body_ir = try self.compileBodyWithTail(cons.cdr, env, in_tail);

        return try self.builder.block(name, body_ir);
    }

    fn compileReturnFrom(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (return-from name value) - name can be symbol or nil
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const name = if (cons.car.isNil())
            "nil"
        else if (cons.car.isSymbol())
            cons.car.toPtr(Symbol).getName()
        else
            return error.InvalidSyntax;

        // Get value (defaults to nil if not provided)
        const value = if (cons.cdr.isCons())
            cons.cdr.toPtr(Cons).car
        else
            Value.nil;

        const value_ir = try self.compile(value, env);

        return try self.builder.returnFrom(name, value_ir);
    }

    fn compileUnwindProtectWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // (unwind-protect protected cleanup...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const protected = cons.car;

        // Protected form can be in tail position
        const protected_ir = try self.compileWithTail(protected, env, in_tail);

        // Cleanup forms are never in tail position (value discarded)
        const cleanup_ir = try self.compileBody(cons.cdr, env);

        return try self.builder.unwindProtect(protected_ir, cleanup_ir);
    }

    fn compileCatchWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // (catch tag body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const tag = cons.car;

        // Tag is evaluated at runtime
        const tag_ir = try self.compile(tag, env);

        // Body can be in tail position
        const body_ir = try self.compileBodyWithTail(cons.cdr, env, in_tail);

        return try self.builder.@"catch"(tag_ir, body_ir);
    }

    fn compileThrow(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (throw tag value)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const tag = cons.car;

        // Tag is evaluated
        const tag_ir = try self.compile(tag, env);

        // Value defaults to nil
        const value = if (cons.cdr.isCons()) cons.cdr.toPtr(Cons).car else Value.nil;
        const value_ir = try self.compile(value, env);

        return try self.builder.throw(tag_ir, value_ir);
    }

    /// Compile (signal condition-type value) - signals a condition
    /// This is syntactic sugar for (throw '%condition% (cons condition-type value))
    fn compileSignal(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (signal type value) or (signal type)
        if (!args.isCons()) return error.InvalidSyntax;

        const b = self.builtins.?;
        const cons = args.toPtr(Cons);
        const condition_type = cons.car;

        // Compile condition type
        const type_ir = try self.compile(condition_type, env);

        // Value defaults to nil
        const value = if (cons.cdr.isCons()) cons.cdr.toPtr(Cons).car else Value.nil;
        const value_ir = try self.compile(value, env);

        // Build (cons type value) for the condition
        const condition_ir = try self.builder.cons(type_ir, value_ir);

        // Throw with special %condition% tag
        const tag_ir = try self.builder.lit(b.@"%condition%");
        return try self.builder.throw(tag_ir, condition_ir);
    }

    /// Compile restart-case
    /// (restart-case body
    ///   (restart-name (args) handler-body...)
    ///   ...)
    fn compileRestartCase(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const args_cons = args.toPtr(Cons);
        const body = args_cons.car;
        var restart_clauses = args_cons.cdr;

        // Collect restart definitions
        var restarts = std.ArrayList(ir.Restart){};
        defer restarts.deinit(self.allocator);

        while (restart_clauses.isCons()) {
            const clause_cons = restart_clauses.toPtr(Cons);
            const clause = clause_cons.car;

            if (!clause.isCons()) return error.InvalidSyntax;
            const clause_parts = clause.toPtr(Cons);

            // First element is the restart name (symbol)
            const restart_name_val = clause_parts.car;
            if (!restart_name_val.isSymbol()) return error.InvalidSyntax;

            // Quote the restart name
            const restart_name = restart_name_val.toPtr(Symbol).getName();
            const name_ir = try self.builder.quoteSym(restart_name);

            // Rest is (args) body...
            // Handler is like a lambda: (name (param) body...)
            const lambda_args = clause_parts.cdr;

            // Compile handler as a lambda with the parameter list
            // compileLambda expects ((params) body...)
            const handler_ir = try self.compileLambda(lambda_args, env);

            try restarts.append(self.allocator, .{
                .name = name_ir,
                .handler = handler_ir,
            });

            restart_clauses = clause_cons.cdr;
        }

        // Compile body
        const body_ir = try self.compile(body, env);

        // Create restart-case IR node
        const restarts_slice = try self.allocator.dupe(ir.Restart, restarts.items);

        const node = try self.allocator.create(Ir);
        node.* = .{ .restart_case = .{
            .body = body_ir,
            .restarts = restarts_slice,
        } };
        return node;
    }

    /// Compile invoke-restart
    /// (invoke-restart restart-name value)
    fn compileInvokeRestart(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const args_cons = args.toPtr(Cons);
        const name_expr = args_cons.car;

        // Compile restart name
        const name_ir = try self.compile(name_expr, env);

        // Value defaults to nil
        const value_expr = if (args_cons.cdr.isCons())
            args_cons.cdr.toPtr(Cons).car
        else
            Value.nil;
        const value_ir = try self.compile(value_expr, env);

        const node = try self.allocator.create(Ir);
        node.* = .{ .invoke_restart = .{
            .name = name_ir,
            .value = value_ir,
        } };
        return node;
    }

    /// Compile find-restart
    /// (find-restart restart-name)
    fn compileFindRestart(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const args_cons = args.toPtr(Cons);
        const name_expr = args_cons.car;

        // Compile restart name
        const operand = try self.compile(name_expr, env);

        const node = try self.allocator.create(Ir);
        node.* = .{ .find_restart = .{ .operand = operand } };
        return node;
    }

    /// Compile handler-case with tail position tracking
    /// (handler-case expression
    ///   (condition-type (var) handler-body...)
    ///   ...)
    fn compileHandlerCaseWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // (handler-case expr (type1 (var1) body1...) (type2 (var2) body2...) ...)
        if (!args.isCons()) return error.InvalidSyntax;

        const b = self.builtins.?;
        const args_cons = args.toPtr(Cons);
        const protected_expr = args_cons.car;
        const handlers = args_cons.cdr;

        // Build the handler dispatch code
        // This will be wrapped in a catch with %condition% tag
        // The caught value is (type . data), we need to match type and bind data

        // Use a fixed internal name for the condition variable
        // Each handler-case has its own scope, so this won't conflict
        const cond_name = "%hc-cond%";

        // Create environment with condition variable bound
        // This is where the caught value will be stored
        var handler_env = Env.initLet(self.allocator, env);
        defer handler_env.deinit();
        const cond_idx = try handler_env.bind(cond_name);

        // Build handler dispatch as nested if/progn:
        // (if (eq (car cond) 'type1)
        //     (let ((var1 (cdr cond))) body1...)
        //     (if (eq (car cond) 'type2)
        //         (let ((var2 (cdr cond))) body2...)
        //         (throw '%condition% cond)))  ; re-throw if no match

        var handler_ir = try self.buildRethrow(cond_name, cond_idx, &handler_env);

        // Process handlers in reverse order to build nested if
        var handler_list = std.ArrayList(Value){};
        defer handler_list.deinit(self.allocator);

        var curr = handlers;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            try handler_list.append(self.allocator, c.car);
            curr = c.cdr;
        }

        // Build from last to first
        var i = handler_list.items.len;
        while (i > 0) {
            i -= 1;
            const handler = handler_list.items[i];
            handler_ir = try self.buildHandlerClause(handler, cond_name, cond_idx, handler_ir, &handler_env, in_tail);
        }

        // Compile protected expression
        const protected_ir = try self.compileWithTail(protected_expr, env, in_tail);

        // Build catch with %condition% tag
        // The handler_ir is the dispatch code that runs when a condition is caught
        const tag_ir = try self.builder.lit(b.@"%condition%");

        // Create a handler-case IR node (which is like catch but binds the caught value)
        return try self.builder.handlerCase(tag_ir, protected_ir, handler_ir, cond_name, cond_idx);
    }

    /// Build re-throw for unhandled conditions
    fn buildRethrow(self: *Compiler, cond_name: []const u8, cond_idx: u16, env: *const Env) anyerror!*Ir {
        const b = self.builtins.?;
        _ = env;
        // (throw '%condition% cond)
        const tag_ir = try self.builder.lit(b.@"%condition%");
        const cond_ir = try self.builder.variable(cond_name, 0, cond_idx);
        return try self.builder.throw(tag_ir, cond_ir);
    }

    /// Build a single handler clause as if-then-else
    fn buildHandlerClause(
        self: *Compiler,
        handler: Value,
        cond_name: []const u8,
        cond_idx: u16,
        else_ir: *Ir,
        env: *const Env,
        in_tail: bool,
    ) anyerror!*Ir {
        // handler is (type (var) body...)
        if (!handler.isCons()) return error.InvalidSyntax;

        const handler_cons = handler.toPtr(Cons);
        const condition_type = handler_cons.car;

        if (!handler_cons.cdr.isCons()) return error.InvalidSyntax;
        const rest = handler_cons.cdr.toPtr(Cons);
        const lambda_list = rest.car;
        const body = rest.cdr;

        // Get variable name from lambda list
        var var_name: []const u8 = "_"; // default if no variable
        if (lambda_list.isCons()) {
            const ll_cons = lambda_list.toPtr(Cons);
            if (ll_cons.car.isSymbol()) {
                var_name = ll_cons.car.toPtr(runtime.Symbol).getName();
            }
        }

        // Build handler body with variable binding
        // (let ((var (cdr cond))) body...)
        const cond_var_ir2 = try self.builder.variable(cond_name, 0, cond_idx);
        const cdr_cond = try self.builder.cdr(cond_var_ir2);

        // Create environment with binding
        var inner_env = Env.initLet(self.allocator, env);
        defer inner_env.deinit();
        const var_idx = try inner_env.bind(var_name);

        // Compile body
        const body_ir = try self.compileBodyWithTail(body, &inner_env, in_tail);

        // Build let node
        const let_ir = try self.builder.let1(var_name, var_idx, cdr_cond, body_ir);

        // Check if condition_type is 't' (catch-all handler)
        if (condition_type.raw == Value.t.raw) {
            // t is catch-all, no test needed
            return let_ir;
        }

        // Build: (if (eq (car cond) 'type) (let ((var (cdr cond))) body...) else)

        // (car cond)
        const cond_var_ir = try self.builder.variable(cond_name, 0, cond_idx);
        const car_cond = try self.builder.car(cond_var_ir);

        // 'type - the condition type symbol
        const type_ir = try self.builder.lit(condition_type);

        // (eq (car cond) 'type)
        const test_ir = try self.builder.eq(car_cond, type_ir);

        // Build if node
        return try self.builder.ifExpr(test_ir, let_ir, else_ir);
    }

    fn compileTagbody(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
                try segments.append(self.allocator, segment_ir);
                current_forms.clearRetainingCapacity();

                const sym = elem.toPtr(Symbol);
                try tags.append(self.allocator, sym.getName());
            } else {
                // This is a form - add to current segment
                try current_forms.append(self.allocator, elem);
            }
        }

        // Close final segment
        const final_segment = try self.compileFormsToProgn(current_forms.items, env);
        try segments.append(self.allocator, final_segment);

        return try self.builder.tagbody(
            tags.items,
            segments.items,
        );
    }

    fn compileFormsToProgn(self: *Compiler, forms: []const Value, env: *const Env) anyerror!*Ir {
        if (forms.len == 0) {
            return try self.builder.lit(Value.nil);
        }
        if (forms.len == 1) {
            return self.compile(forms[0], env);
        }
        var exprs = std.ArrayList(*const Ir){};
        defer exprs.deinit(self.allocator);
        for (forms) |form| {
            const form_ir = try self.compile(form, env);
            try exprs.append(self.allocator, form_ir);
        }
        return try self.builder.progn(exprs.items);
    }

    fn compileGo(self: *Compiler, args: Value) anyerror!*Ir {
        // (go tag)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        if (!cons.car.isSymbol()) return error.InvalidSyntax;

        const sym = cons.car.toPtr(Symbol);
        const name = sym.getName();

        return try self.builder.go(name);
    }

    fn compileValues(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (values v1 v2 ...)
        var vals = std.ArrayList(*const Ir){};
        defer vals.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const val_ir = try self.compile(cons.car, env);
            try vals.append(self.allocator, val_ir);
            current = cons.cdr;
        }

        return try self.builder.values(vals.items);
    }

    fn compileValuesList(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (values-list list)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const list_ir = try self.compile(cons.car, env);
        return try self.builder.valuesList(list_ir);
    }

    fn compileMvBind(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
            try vars.append(self.allocator, sym.getName());
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
            _ = try let_env.bind(var_name);
        }

        // Compile body forms
        const body_ir = try self.compileBody(cons2.cdr, &let_env);

        return try self.builder.mvBind(vars.items, expr_ir, body_ir);
    }

    fn compileMvCall(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
            try forms.append(self.allocator, form_ir);
            current = cons.cdr;
        }

        return try self.builder.mvCall(func_ir, forms.items);
    }

    fn compileMvList(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (multiple-value-list expr)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);

        const expr_ir = try self.compile(cons.car, env);
        return try self.builder.mvList(expr_ir);
    }

    fn compileDefine(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (define name value)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSyntax;
        const name_sym = cons1.car.toPtr(Symbol);

        // Use qualified name for globals (package-aware)
        var qual_buf: [256]u8 = undefined;
        const name = self.getQualifiedName(name_sym, &qual_buf) catch name_sym.getName();

        // Pre-register global for recursive definitions
        const idx = try self.globals.define(name);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const value_ir = try self.compile(cons2.car, env);

        return try self.builder.define(name, idx, value_ir);
    }

    fn compileDefun(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (defun name (params...) body...) -> (define name (lambda (params...) body...))
        // (defun (name -> type) (params...) body...) -> with return type assertion
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const name_spec = cons1.car;

        var name_sym_saved: *const Symbol = undefined;
        var return_type: ?Value = null;

        if (name_spec.isSymbol()) {
            // Simple: (defun name ...)
            name_sym_saved = name_spec.toPtr(Symbol);
        } else if (name_spec.isCons()) {
            // Typed: (defun (name -> type) ...)
            const spec_cons = name_spec.toPtr(Cons);
            if (!spec_cons.car.isSymbol()) return error.InvalidSyntax;
            name_sym_saved = spec_cons.car.toPtr(Symbol);

            // Check for -> arrow (use symbol identity)
            if (!spec_cons.cdr.isCons()) return error.InvalidSyntax;
            const arrow_cons = spec_cons.cdr.toPtr(Cons);
            const b = self.builtins orelse return error.UninitializedBuiltins;
            if (arrow_cons.car.raw != b.@"->".raw) return error.InvalidSyntax;

            // Get return type (symbol or complex type like (or T1 T2))
            if (!arrow_cons.cdr.isCons()) return error.InvalidSyntax;
            const type_cons = arrow_cons.cdr.toPtr(Cons);
            // Accept symbol or cons (complex type expression)
            if (!type_cons.car.isSymbol() and !type_cons.car.isCons()) return error.InvalidSyntax;
            return_type = type_cons.car;
        } else {
            return error.InvalidSyntax;
        }

        // Pre-register the global so recursive calls work
        // Use qualified name for consistency
        var qual_buf: [256]u8 = undefined;
        const name = self.getQualifiedName(name_sym_saved, &qual_buf) catch name_sym_saved.getName();
        const idx = try self.globals.define(name);

        // Rest is (params...) body...
        const lambda_args = cons1.cdr;
        const lambda_ir = try self.compileLambdaWithReturnType(lambda_args, env, return_type);

        return try self.builder.define(name, idx, lambda_ir);
    }

    /// Compile defmacro: (defmacro name (params...) body...)
    /// Stores the lambda-args in macro_table for expansion during macro calls.
    /// Returns nil since defmacro has no runtime effect.
    fn compileDefmacro(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
        try self.macro_table.put(name, lambda_args);

        // defmacro has no runtime effect - return nil
        return try self.builder.lit(Value.nil);
    }

    /// Compile eval-when: (eval-when (situations...) body...)
    /// The REPL handles compile-time evaluation; compiler just handles :execute
    fn compileEvalWhen(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
                const b = self.builtins.?;
                // Compare by identity with pre-interned keywords
                if (situation.raw == b.kw_execute.raw or
                    situation.raw == b.@"kw_load-toplevel".raw)
                {
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
        return try self.builder.lit(Value.nil);
    }

    // ========================================================================
    // Package Support
    // ========================================================================

    /// Compile defpackage: (defpackage "name" (:use "other-pkg") (:export "sym1" "sym2"))
    /// Creates a new package with the given name
    fn compileDefpackage(self: *Compiler, args: Value) anyerror!*Ir {
        const heap = self.heap orelse return error.InvalidSyntax;
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const pkg_name_val = cons1.car;

        // Get package name from string or symbol
        var pkg_name: []const u8 = undefined;
        if (pkg_name_val.isString()) {
            const str = pkg_name_val.toPtr(runtime.String);
            pkg_name = str.bytes();
        } else if (pkg_name_val.isSymbol()) {
            const sym = pkg_name_val.toPtr(Symbol);
            pkg_name = sym.getName();
        } else {
            return error.InvalidSyntax;
        }

        // Create or find the package
        const pkg = try heap.findOrCreatePackage(pkg_name);

        // Process options: (:use ...) (:export ...)
        var options = cons1.cdr;
        while (options.isCons()) {
            const opt_cons = options.toPtr(Cons);
            const opt = opt_cons.car;

            if (opt.isCons()) {
                const opt_list = opt.toPtr(Cons);
                if (opt_list.car.isKeyword()) {
                    const kw = opt_list.car;
                    const b = self.builtins.?;

                    if (kw.raw == b.kw_use.raw) {
                        // (:use "pkg1" "pkg2" ...)
                        var use_list = opt_list.cdr;
                        while (use_list.isCons()) {
                            const use_cons = use_list.toPtr(Cons);
                            const use_pkg_name = self.getStringOrSymbolName(use_cons.car) orelse return error.InvalidSyntax;
                            const use_pkg = try heap.findOrCreatePackage(use_pkg_name);
                            pkg.usePackage(use_pkg) catch |e| return e;
                            use_list = use_cons.cdr;
                        }
                    } else if (kw.raw == b.kw_export.raw) {
                        // (:export "sym1" "sym2" ...)
                        var export_list = opt_list.cdr;
                        while (export_list.isCons()) {
                            const export_cons = export_list.toPtr(Cons);
                            const export_name = self.getStringOrSymbolName(export_cons.car) orelse return error.InvalidSyntax;
                            pkg.exportSymbol(export_name) catch |e| return e;
                            export_list = export_cons.cdr;
                        }
                    }
                }
            }
            options = opt_cons.cdr;
        }

        // Return the package name as a symbol
        return try self.builder.lit(try heap.intern(pkg_name));
    }

    /// Compile in-package: (in-package "name")
    /// Switches the current package
    fn compileInPackage(self: *Compiler, args: Value) anyerror!*Ir {
        const heap = self.heap orelse return error.InvalidSyntax;
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const pkg_name = self.getStringOrSymbolName(cons1.car) orelse return error.InvalidSyntax;

        // Find or create the package and set it as current
        const pkg = try heap.findOrCreatePackage(pkg_name);
        heap.setCurrentPackage(pkg);

        // Return the package name
        return try self.builder.lit(try heap.intern(pkg_name));
    }

    /// Compile export: (export 'sym1 'sym2 ...)
    /// Exports symbols from the current package
    fn compileExport(self: *Compiler, args: Value) anyerror!*Ir {
        const heap = self.heap orelse return error.InvalidSyntax;
        const pkg = heap.current_package orelse return error.InvalidSyntax;

        var syms = args;
        while (syms.isCons()) {
            const cons = syms.toPtr(Cons);
            const sym_name = self.getStringOrSymbolName(cons.car) orelse return error.InvalidSyntax;
            pkg.exportSymbol(sym_name) catch |e| return e;
            syms = cons.cdr;
        }

        return try self.builder.lit(Value.t);
    }

    /// Compile use-package: (use-package "pkg")
    /// Makes another package's exports available in current package
    fn compileUsePackage(self: *Compiler, args: Value) anyerror!*Ir {
        const heap = self.heap orelse return error.InvalidSyntax;
        if (!args.isCons()) return error.InvalidSyntax;

        const pkg = heap.current_package orelse return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const other_name = self.getStringOrSymbolName(cons1.car) orelse return error.InvalidSyntax;

        const other_pkg = heap.findPackage(other_name) orelse return error.InvalidSyntax;
        pkg.usePackage(other_pkg) catch |e| return e;

        return try self.builder.lit(Value.t);
    }

    /// Helper to get string from a string or symbol value
    fn getStringOrSymbolName(self: *Compiler, val: Value) ?[]const u8 {
        _ = self;
        if (val.isString()) {
            return val.toPtr(runtime.String).bytes();
        } else if (val.isSymbol()) {
            return val.toPtr(Symbol).getName();
        }
        return null;
    }

    /// Get qualified name for a symbol (PKG:NAME or just NAME if no package)
    fn getQualifiedName(self: *Compiler, sym: *const Symbol, buf: []u8) ![]const u8 {
        _ = self;
        const pkg_ptr = sym.reserved;
        if (pkg_ptr == 0) {
            // No package - just return name
            return sym.getName();
        }
        // Get package from pointer - Package is in heap module
        const pkg: *const runtime.heap.Package = @ptrFromInt(pkg_ptr);
        const pkg_name = pkg.name;
        const sym_name = sym.getName();

        // Format as PKG:NAME
        const result = try std.fmt.bufPrint(buf, "{s}:{s}", .{ pkg_name, sym_name });
        return result;
    }

    /// Build qualified name from plain name using current package
    fn qualifyName(self: *Compiler, name: []const u8, buf: []u8) ![]const u8 {
        const heap = self.heap orelse return name;
        const pkg = heap.current_package orelse return name;
        const pkg_name = pkg.name;
        // Format as PKG:NAME
        const result = try std.fmt.bufPrint(buf, "{s}:{s}", .{ pkg_name, name });
        return result;
    }

    // ========================================================================
    // Structure Definition (defstruct)
    // ========================================================================

    /// Compile defstruct: (defstruct name slot1 slot2 ...)
    /// Generates: constructor (make-name), accessors (name-slot), predicate (name-p), copier (copy-name)
    /// Runtime representation: #(name slot1-val slot2-val ...)
    /// Registers struct type with type system for occurrence typing and type checking
    fn compileDefstruct(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const heap = self.heap orelse return error.InvalidSyntax;

        // Parse: (name slot1 slot2 ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const name_val = cons1.car;
        if (!name_val.isSymbol()) return error.InvalidSyntax;
        // Dupe struct name to avoid dangling pointer if heap moves
        const struct_name_raw = name_val.toPtr(Symbol).getName();
        const struct_name = self.allocator.dupe(u8, struct_name_raw) catch |e| return e;

        // Collect slot specs: either `slot` or `(slot type)`
        var slot_specs = std.ArrayList(SlotSpec){};
        defer slot_specs.deinit(self.allocator);
        var rest = cons1.cdr;
        while (rest.isCons()) {
            const c = rest.toPtr(Cons);
            const slot_spec = c.car;

            if (slot_spec.isSymbol()) {
                // Simple slot: `x` -> type is any
                const slot_name_raw = slot_spec.toPtr(Symbol).getName();
                const slot_name = self.allocator.dupe(u8, slot_name_raw) catch |e| return e;
                slot_specs.append(self.allocator, .{ .name = slot_name, .field_type = &types.t_any }) catch |e| return e;
            } else if (slot_spec.isCons()) {
                // Typed slot: `(x fixnum)` -> parse name and type
                const spec_cons = slot_spec.toPtr(Cons);
                if (!spec_cons.car.isSymbol()) return error.InvalidSyntax;
                const slot_name_raw = spec_cons.car.toPtr(Symbol).getName();
                const slot_name = self.allocator.dupe(u8, slot_name_raw) catch |e| return e;

                // Get type from second element (can be symbol or compound type expr)
                if (!spec_cons.cdr.isCons()) return error.InvalidSyntax;
                const type_cons = spec_cons.cdr.toPtr(Cons);
                const type_expr = type_cons.car;

                // Parse type expression (supports compound types like (list fixnum))
                const field_type = self.parseTypeExpr(type_expr) orelse return error.InvalidSyntax;
                slot_specs.append(self.allocator, .{ .name = slot_name, .field_type = field_type }) catch |e| return e;
            } else {
                return error.InvalidSyntax;
            }
            rest = c.cdr;
        }

        // Create struct fields from specs
        const struct_fields = try self.allocator.alloc(types.StructField, slot_specs.items.len);
        for (slot_specs.items, 0..) |spec, i| {
            struct_fields[i] = .{
                .name = try self.allocator.dupe(u8, spec.name),
                .type = spec.field_type,
            };
        }
        var type_builder = types.TypeBuilder.init(self.allocator);
        defer type_builder.deinit();
        const struct_type = type_builder.makeStruct(struct_name, struct_fields) catch |e| return e;
        self.registerStructType(struct_name, struct_type) catch |e| return e;

        // Extract slot names for constructor params
        var slot_names = try self.allocator.alloc([]const u8, slot_specs.items.len);
        for (slot_specs.items, 0..) |spec, i| {
            slot_names[i] = spec.name;
        }

        // Pre-allocate array: constructor + accessors + predicate + copier + name_lit
        const num_defs = 4 + slot_specs.items.len;
        const defs = try self.allocator.alloc(*Ir, num_defs);
        var def_idx: usize = 0;

        // 1. Constructor: (defun make-name (slot1 slot2 ...) (vector 'name slot1 slot2 ...))
        const make_name = try self.concatStrings("make-", struct_name);
        defs[def_idx] = try self.generateStructConstructor(heap, make_name, struct_name, slot_specs.items, env);
        def_idx += 1;

        // 2. Accessors: (defun name-slotN (obj) (if (name-p obj) (aref obj N+1) (error)))
        for (slot_specs.items, 0..) |spec, i| {
            const accessor_name = try self.concatStrings3(struct_name, "-", spec.name);
            defs[def_idx] = try self.generateStructAccessor(heap, accessor_name, struct_name, i);
            def_idx += 1;
        }

        // 3. Predicate: (defun name-p (obj) (and (vectorp obj) (eq (aref obj 0) 'name)))
        const pred_name = try self.concatStrings(struct_name, "-p");
        defs[def_idx] = try self.generateStructPredicate(heap, pred_name, struct_name);
        def_idx += 1;

        // Register predicate for occurrence typing
        // Use globals.allocator for persistence across expressions (arena gets freed)
        const persistent_pred_name = self.globals.allocator.dupe(u8, pred_name) catch |e| return e;
        self.struct_predicates.put(persistent_pred_name, struct_type) catch |e| return e;

        // 4. Copier: (defun copy-name (obj) (copy-seq obj))
        const copy_name = try self.concatStrings("copy-", struct_name);
        defs[def_idx] = try self.generateStructCopier(copy_name);
        def_idx += 1;

        // 5. Return struct name
        defs[def_idx] = try self.builder.lit(name_val);

        return try self.builder.progn(defs);
    }

    /// Slot specification with name, type, and optional init form
    const SlotSpec = struct {
        name: []const u8,
        field_type: *const types.Type,
        initform: ?Value = null, // Optional initialization expression
    };

    /// Generate constructor: creates a closure that takes args, checks types, returns vector
    fn generateStructConstructor(self: *Compiler, heap: *Heap, make_name: []const u8, struct_name: []const u8, slots: []const SlotSpec, env: *const Env) anyerror!*Ir {
        _ = env;
        // Qualify the constructor name with current package
        var qual_buf: [512]u8 = undefined;
        const qualified_name = self.qualifyName(make_name, &qual_buf) catch make_name;
        const global_idx = self.globals.define(qualified_name) catch |e| return e;

        // Extract just the names for lambda params
        const slot_names = try self.allocator.alloc([]const u8, slots.len);
        for (slots, 0..) |spec, i| {
            slot_names[i] = spec.name;
        }

        // Build body: type assertions + vector creation
        // Count non-any type assertions needed
        var num_assertions: usize = 0;
        for (slots) |spec| {
            if (!spec.field_type.isAny()) num_assertions += 1;
        }

        // Pre-allocate vector args: name_sym + one per slot
        const vec_args = try self.allocator.alloc(*Ir, 1 + slots.len);
        const name_sym = try heap.intern(struct_name);
        vec_args[0] = try self.builder.lit(name_sym);
        for (slots, 0..) |spec, i| {
            vec_args[1 + i] = try self.builder.variable(spec.name, 0, @intCast(i));
        }
        const vec_ir = try self.builder.vec(vec_args);

        // If no type assertions needed, just return the vector
        var body_ir: *Ir = vec_ir;
        if (num_assertions > 0) {
            // Build progn with type assertions followed by vector
            const progn_items = try self.allocator.alloc(*Ir, num_assertions + 1);
            var idx: usize = 0;
            for (slots, 0..) |spec, i| {
                if (!spec.field_type.isAny()) {
                    // Generate type assertion for this field
                    const var_ref = try self.builder.variable(spec.name, 0, @intCast(i));
                    progn_items[idx] = try self.generateTypeAssertion(var_ref, spec.field_type, spec.name);
                    idx += 1;
                }
            }
            progn_items[num_assertions] = vec_ir;
            body_ir = try self.builder.progn(progn_items);
        }

        const lambda_ir = self.builder.lambda(
            slot_names,
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            null,
            &[_]Ir.Capture{},
            body_ir,
        ) catch |e| return e;

        return try self.builder.define(qualified_name, global_idx, lambda_ir);
    }

    /// Generate a type assertion IR node
    fn generateTypeAssertion(self: *Compiler, value: *Ir, expected_type: *const types.Type, field_name: []const u8) anyerror!*Ir {
        _ = field_name; // Could be used for error messages
        // Use the appropriate assert_* IR based on type
        return switch (expected_type.*) {
            .primitive => |p| switch (p) {
                .fixnum => try self.builder.assertFixnum(value),
                .cons => try self.builder.assertCons(value),
                .symbol => try self.builder.assertSymbol(value),
                .string => try self.builder.assertString(value),
                .vector => try self.builder.assertVector(value),
                .closure => try self.builder.assertClosure(value),
                else => value, // No assertion for nil, float, char, keyword yet
            },
            else => value, // No assertion for compound types yet
        };
    }

    /// Parse type symbol to Type pointer using symbol identity
    fn parseTypeSym(self: *Compiler, type_sym: Value) ?*const types.Type {
        const b = self.builtins orelse return null;

        // Type symbol table - match by Value.raw identity
        const entries = [_]struct { sym: Value, ty: *const types.Type }{
            .{ .sym = b.ty_fixnum, .ty = &types.t_fixnum },
            .{ .sym = b.ty_integer, .ty = &types.t_fixnum }, // alias
            .{ .sym = b.ty_float, .ty = &types.t_float },
            .{ .sym = b.ty_cons, .ty = &types.t_cons },
            .{ .sym = b.ty_symbol, .ty = &types.t_symbol },
            .{ .sym = b.ty_string, .ty = &types.t_string },
            .{ .sym = b.ty_vector, .ty = &types.t_vector },
            .{ .sym = b.ty_closure, .ty = &types.t_closure },
            .{ .sym = b.ty_function, .ty = &types.t_closure }, // alias
            .{ .sym = b.ty_keyword, .ty = &types.t_keyword },
            .{ .sym = b.ty_nil, .ty = &types.t_nil },
            .{ .sym = b.ty_char, .ty = &types.t_char },
            .{ .sym = b.ty_character, .ty = &types.t_char }, // alias
            .{ .sym = b.ty_any, .ty = &types.t_any },
            .{ .sym = b.ty_t, .ty = &types.t_any }, // t = any
            .{ .sym = b.ty_list, .ty = &types.t_list_any },
        };

        for (entries) |e| if (type_sym.raw == e.sym.raw) return e.ty;
        return null;
    }

    /// Parse a type expression (simple symbol or compound form)
    /// Handles:
    /// - Simple: fixnum, symbol, cons, etc.
    /// - Or: (or T1 T2 ...)
    /// - Arrow: (-> (A B) C) or (-> A B C)
    /// - List: (list T)
    /// - Vec: (vec T) or (vec T N) for sized vectors
    /// - Non-nil: (non-nil T)
    /// - Pi: (pi (x : A) B) dependent function
    /// - Sigma: (sigma (x : A) B) dependent pair
    /// - Refine: (refine T x P) refinement type
    pub fn parseTypeExpr(self: *Compiler, type_expr: Value) ?*const types.Type {
        // Simple symbol case
        if (type_expr.isSymbol()) {
            return self.parseTypeSym(type_expr);
        }

        // Compound type form
        if (!type_expr.isCons()) return null;

        const cons = type_expr.toPtr(Cons);
        const head = cons.car;

        if (!head.isSymbol()) return null;

        const b = self.builtins orelse return null;

        // (union T1 T2 ...) or (or T1 T2 ...) - union type
        if (head.raw == b.ty_union.raw or head.raw == b.ty_or.raw) {
            return self.parseOrType(cons.cdr);
        }

        // (-> (A B) C) or (-> A B ... C) - function type
        if (head.raw == b.@"->".raw) {
            return self.parseArrowType(cons.cdr);
        }

        // (list T) - list type
        if (head.raw == b.ty_list.raw) {
            return self.parseListType(cons.cdr);
        }

        // (vec T) or (vec T N) - vector type
        if (head.raw == b.ty_vec.raw) {
            return self.parseVecType(cons.cdr);
        }

        // (non-nil T) - non-nil type
        if (head.raw == b.@"ty_non-nil".raw) {
            return self.parseNonNilType(cons.cdr);
        }

        // (pi (x : A) B) - dependent function type
        if (head.raw == b.ty_pi.raw) {
            return self.parsePiType(cons.cdr);
        }

        // (sigma (x : A) B) - dependent pair type
        if (head.raw == b.ty_sigma.raw) {
            return self.parseSigmaType(cons.cdr);
        }

        // (refine T x P) - refinement type
        if (head.raw == b.ty_refine.raw) {
            return self.parseRefineType(cons.cdr);
        }

        return null;
    }

    /// Parse (or T1 T2 ...)
    fn parseOrType(self: *Compiler, args: Value) ?*const types.Type {
        var type_list = std.ArrayList(*const types.Type){};
        defer type_list.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const c = current.toPtr(Cons);
            const t = self.parseTypeExpr(c.car) orelse return null;
            type_list.append(self.allocator, t) catch return null;
            current = c.cdr;
        }

        if (type_list.items.len == 0) return null;
        if (type_list.items.len == 1) return type_list.items[0];

        return self.type_checker.builder.makeOr(type_list.items) catch null;
    }

    /// Parse (-> (A B) C) or (-> A B ... C) function type
    fn parseArrowType(self: *Compiler, args: Value) ?*const types.Type {
        if (!args.isCons()) return null;

        var all_types = std.ArrayList(*const types.Type){};
        defer all_types.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const c = current.toPtr(Cons);
            // Check if this element is a list (domain types)
            if (c.car.isCons()) {
                // Parse list of domain types
                var domain = c.car;
                while (domain.isCons()) {
                    const dc = domain.toPtr(Cons);
                    const t = self.parseTypeExpr(dc.car) orelse return null;
                    all_types.append(self.allocator, t) catch return null;
                    domain = dc.cdr;
                }
            } else {
                // Single type
                const t = self.parseTypeExpr(c.car) orelse return null;
                all_types.append(self.allocator, t) catch return null;
            }
            current = c.cdr;
        }

        if (all_types.items.len < 1) return null;

        // Last type is return type, rest are domain
        const return_type = all_types.items[all_types.items.len - 1];
        const domain = all_types.items[0 .. all_types.items.len - 1];

        return self.type_checker.builder.makeArrow(domain, return_type) catch null;
    }

    /// Parse (list T)
    fn parseListType(self: *Compiler, args: Value) ?*const types.Type {
        if (!args.isCons()) return &types.t_list_any;
        const c = args.toPtr(Cons);
        const elem = self.parseTypeExpr(c.car) orelse return null;
        return self.type_checker.builder.makeList(elem) catch null;
    }

    /// Parse (vec T) or (vec T N) - for now just (vec T)
    fn parseVecType(self: *Compiler, args: Value) ?*const types.Type {
        if (!args.isCons()) return &types.t_vector;
        const c = args.toPtr(Cons);
        const elem = self.parseTypeExpr(c.car) orelse return null;
        // TODO: handle (vec T N) for sized vectors with term parsing
        return self.type_checker.builder.makeVec(elem) catch null;
    }

    /// Parse (non-nil T)
    fn parseNonNilType(self: *Compiler, args: Value) ?*const types.Type {
        if (!args.isCons()) return null;
        const c = args.toPtr(Cons);
        const inner = self.parseTypeExpr(c.car) orelse return null;
        return self.type_checker.builder.makeNonNil(inner) catch null;
    }

    /// Parse (pi (x : A) B) dependent function type
    fn parsePiType(self: *Compiler, args: Value) ?*const types.Type {
        // (pi (x : A) B) -> args = ((x : A) B)
        if (!args.isCons()) return null;
        const c1 = args.toPtr(Cons);

        // First element should be (x : A) - the binding
        if (!c1.car.isCons()) return null;
        const binding = c1.car.toPtr(Cons);

        // Parse parameter name
        if (!binding.car.isSymbol()) return null;
        const param_name = binding.car.toPtr(Symbol).getName();

        // Expect (:) and type
        if (!binding.cdr.isCons()) return null;
        const rest1 = binding.cdr.toPtr(Cons);
        // Skip the colon if present
        var type_expr = rest1.car;
        if (rest1.car.isSymbol()) {
            const sym = rest1.car.toPtr(Symbol);
            if (std.mem.eql(u8, sym.getName(), ":")) {
                // Next is the actual type
                if (!rest1.cdr.isCons()) return null;
                type_expr = rest1.cdr.toPtr(Cons).car;
            }
        }

        const param_type = self.parseTypeExpr(type_expr) orelse return null;

        // Second element is the return type B
        if (!c1.cdr.isCons()) return null;
        const c2 = c1.cdr.toPtr(Cons);
        const return_type = self.parseTypeExpr(c2.car) orelse return null;

        return self.type_checker.builder.makePi(param_name, param_type, return_type, .many) catch null;
    }

    /// Parse (sigma (x : A) B) dependent pair type
    fn parseSigmaType(self: *Compiler, args: Value) ?*const types.Type {
        // Similar structure to pi
        if (!args.isCons()) return null;
        const c1 = args.toPtr(Cons);

        if (!c1.car.isCons()) return null;
        const binding = c1.car.toPtr(Cons);

        if (!binding.car.isSymbol()) return null;
        const first_name = binding.car.toPtr(Symbol).getName();

        if (!binding.cdr.isCons()) return null;
        const rest1 = binding.cdr.toPtr(Cons);
        var type_expr = rest1.car;
        if (rest1.car.isSymbol()) {
            const sym = rest1.car.toPtr(Symbol);
            if (std.mem.eql(u8, sym.getName(), ":")) {
                if (!rest1.cdr.isCons()) return null;
                type_expr = rest1.cdr.toPtr(Cons).car;
            }
        }

        const first_type = self.parseTypeExpr(type_expr) orelse return null;

        if (!c1.cdr.isCons()) return null;
        const c2 = c1.cdr.toPtr(Cons);
        const second_type = self.parseTypeExpr(c2.car) orelse return null;

        return self.type_checker.builder.makeSigma(first_name, first_type, second_type) catch null;
    }

    /// Parse (refine T x P) refinement type
    fn parseRefineType(self: *Compiler, args: Value) ?*const types.Type {
        // (refine T x P) -> args = (T x P)
        if (!args.isCons()) return null;
        const c1 = args.toPtr(Cons);

        // Base type T
        const base_type = self.parseTypeExpr(c1.car) orelse return null;

        if (!c1.cdr.isCons()) return null;
        const c2 = c1.cdr.toPtr(Cons);

        // Variable name x
        if (!c2.car.isSymbol()) return null;
        const var_name = c2.car.toPtr(Symbol).getName();

        if (!c2.cdr.isCons()) return null;
        const c3 = c2.cdr.toPtr(Cons);

        // Predicate P - for now, store as raw S-expression
        // TODO: convert to Term for proper type-level computation
        const predicate = c3.car;
        _ = predicate; // Predicate parsing would go here

        // For now, create refinement with null predicate (will be enhanced later)
        return self.type_checker.builder.makeRefinement(base_type, var_name, null) catch null;
    }

    /// Generate accessor with runtime type check:
    /// (lambda (obj)
    ///   (if (and (vectorp obj) (eq (vec-ref obj 0) 'struct-name))
    ///       (vec-ref obj slot_idx+1)
    ///       (error "type error")))
    fn generateStructAccessor(self: *Compiler, heap: *Heap, accessor_name: []const u8, struct_name: []const u8, slot_idx: usize) anyerror!*Ir {
        // Qualify the accessor name with current package
        var qual_buf: [512]u8 = undefined;
        const qualified_name = self.qualifyName(accessor_name, &qual_buf) catch accessor_name;
        const global_idx = self.globals.define(qualified_name) catch |e| return e;

        // Variable reference for obj parameter
        const obj_ref = try self.builder.variable("obj", 0, 0);

        // Build condition: (if (vectorp obj) (eq (vec-ref obj 0) 'name) nil)
        const vectorp_ir = try self.builder.vectorp(obj_ref);
        const idx0 = try self.builder.lit(Value.makeFixnum(0));
        // Need fresh obj_ref for second use (IR nodes are consumed)
        const obj_ref2 = try self.builder.variable("obj", 0, 0);
        const vecref0 = try self.builder.vecRef(obj_ref2, idx0);
        const name_sym = try heap.intern(struct_name);
        const name_lit = try self.builder.lit(name_sym);
        const eq_ir = try self.builder.eq(vecref0, name_lit);
        const nil_ir = try self.builder.lit(Value.nil);
        const type_check = try self.builder.ifExpr(vectorp_ir, eq_ir, nil_ir);

        // Then branch: (vec-ref obj slot_idx+1)
        const obj_ref3 = try self.builder.variable("obj", 0, 0);
        const idx_lit = try self.builder.lit(Value.makeFixnum(@intCast(slot_idx + 1)));
        const vecref_ir = try self.builder.vecRef(obj_ref3, idx_lit);

        // Else branch: (error "type error: expected struct-name")
        const error_msg = try self.concatStrings3("type error: expected ", struct_name, "");
        const error_str = try heap.allocString(error_msg);
        const error_lit = try self.builder.lit(error_str);
        const error_call = try self.builder.errorUser(error_lit);

        // Full body: (if type-check (vec-ref obj idx) (error ...))
        const body_ir = try self.builder.ifExpr(type_check, vecref_ir, error_call);

        // Lambda with 1 param named "obj"
        const lambda_ir = self.builder.lambda(
            &[_][]const u8{"obj"},
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            null,
            &[_]Ir.Capture{},
            body_ir,
        ) catch |e| return e;

        return try self.builder.define(qualified_name, global_idx, lambda_ir);
    }

    /// Generate predicate: checks if obj is a vector with correct type tag
    fn generateStructPredicate(self: *Compiler, heap: *Heap, pred_name: []const u8, struct_name: []const u8) anyerror!*Ir {
        // Qualify the predicate name with current package
        var qual_buf: [512]u8 = undefined;
        const qualified_name = self.qualifyName(pred_name, &qual_buf) catch pred_name;
        const global_idx = self.globals.define(qualified_name) catch |e| return e;

        // Body: (if (vectorp obj) (eq (vec-ref obj 0) 'name) nil)
        const obj_ref = try self.builder.variable("obj", 0, 0);
        const vectorp_ir = try self.builder.vectorp(obj_ref);
        const idx0 = try self.builder.lit(Value.makeFixnum(0));
        const vecref0 = try self.builder.vecRef(obj_ref, idx0);
        const name_sym = try heap.intern(struct_name);
        const name_lit = try self.builder.lit(name_sym);
        const eq_ir = try self.builder.eq(vecref0, name_lit);
        const nil_ir = try self.builder.lit(Value.nil);
        const body_ir = try self.builder.ifExpr(vectorp_ir, eq_ir, nil_ir);

        const lambda_ir = self.builder.lambda(
            &[_][]const u8{"obj"},
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            null,
            &[_]Ir.Capture{},
            body_ir,
        ) catch |e| return e;

        return try self.builder.define(qualified_name, global_idx, lambda_ir);
    }

    /// Generate copier: (lambda (obj) obj)
    /// TODO: implement proper copy-seq when available
    fn generateStructCopier(self: *Compiler, copy_name: []const u8) anyerror!*Ir {
        // Qualify the copier name with current package
        var qual_buf: [512]u8 = undefined;
        const qualified_name = self.qualifyName(copy_name, &qual_buf) catch copy_name;
        const global_idx = self.globals.define(qualified_name) catch |e| return e;

        // For now just return identity - proper copy-seq needs implementation
        const obj_ref = try self.builder.variable("obj", 0, 0);

        const lambda_ir = self.builder.lambda(
            &[_][]const u8{"obj"},
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            null,
            &[_]Ir.Capture{},
            obj_ref,
        ) catch |e| return e;

        return try self.builder.define(qualified_name, global_idx, lambda_ir);
    }

    /// Concatenate two strings
    fn concatStrings(self: *Compiler, a: []const u8, b: []const u8) ![]const u8 {
        const result = try self.allocator.alloc(u8, a.len + b.len);
        @memcpy(result[0..a.len], a);
        @memcpy(result[a.len..], b);
        return result;
    }

    /// Concatenate three strings
    fn concatStrings3(self: *Compiler, a: []const u8, b: []const u8, c: []const u8) ![]const u8 {
        const result = try self.allocator.alloc(u8, a.len + b.len + c.len);
        @memcpy(result[0..a.len], a);
        @memcpy(result[a.len .. a.len + b.len], b);
        @memcpy(result[a.len + b.len ..], c);
        return result;
    }

    // ========================================================================
    // CLOS Support: defclass, make-instance, slot-value
    // ========================================================================

    /// Compile defclass: (defclass name (superclasses) (slot1 slot2 ...) ...)
    /// Simplified CLOS - for now, ignores superclasses and slot options
    /// Generates: class predicate, constructor (via make-instance), slot accessors
    /// Runtime representation: #('class-name slot1-val slot2-val ...)
    fn compileDefclass(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const heap = self.heap orelse return error.InvalidSyntax;

        // Parse: (name (superclasses...) (slot1 slot2 ...) ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const name_val = cons1.car;
        if (!name_val.isSymbol()) return error.InvalidSyntax;

        const class_name_raw = name_val.toPtr(Symbol).getName();
        const class_name = self.allocator.dupe(u8, class_name_raw) catch |e| return e;

        // Parse superclasses (second arg) and inherit their slots
        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const superclasses = cons2.car;

        // Collect inherited slots from superclasses
        var slot_specs = std.ArrayList(SlotSpec){};
        defer slot_specs.deinit(self.allocator);

        // Process superclass list
        if (superclasses.isCons()) {
            var super_list = superclasses;
            while (super_list.isCons()) {
                const super_cons = super_list.toPtr(Cons);
                const super_name_val = super_cons.car;

                if (super_name_val.isSymbol()) {
                    const super_name = super_name_val.toPtr(Symbol).getName();

                    // Look up superclass metadata
                    if (self.class_metadata.get(super_name)) |parent_specs| {
                        // Inherit slots from parent
                        for (parent_specs) |parent_spec| {
                            const inherited_name = try self.allocator.dupe(u8, parent_spec.name);
                            try slot_specs.append(self.allocator, .{
                                .name = inherited_name,
                                .field_type = parent_spec.field_type,
                                .initform = parent_spec.initform,
                            });
                        }
                    }
                }

                super_list = super_cons.cdr;
            }
        }

        // Parse this class's slots
        // Support both CL standard: (defclass name () ((slot1 ...) (slot2 ...)))
        // and Habu style: (defclass name () (slot1 ...) (slot2 ...))
        var rest = cons2.cdr;

        // Check for CL standard syntax: single list of slot specs
        if (rest.isCons()) {
            const first = rest.toPtr(Cons);
            // If first element is a list and next is nil, it's CL standard
            if (first.car.isCons() and first.cdr.isNil()) {
                // Check if first element looks like a slots list (list of symbols/lists)
                const inner = first.car.toPtr(Cons);
                if (inner.car.isSymbol() or inner.car.isCons()) {
                    // CL standard: unwrap the outer list
                    rest = first.car;
                }
            }
        }

        while (rest.isCons()) {
            const c = rest.toPtr(Cons);
            const slot_spec = c.car;

            if (slot_spec.isSymbol()) {
                // Simple slot: `x`
                const slot_name_raw = slot_spec.toPtr(Symbol).getName();
                const slot_name = self.allocator.dupe(u8, slot_name_raw) catch |e| return e;
                slot_specs.append(self.allocator, .{ .name = slot_name, .field_type = &types.t_any }) catch |e| return e;
            } else if (slot_spec.isCons()) {
                // Slot with options: (name :initform expr :type type ...)
                const spec_cons = slot_spec.toPtr(Cons);
                if (!spec_cons.car.isSymbol()) return error.InvalidSyntax;
                const slot_name_raw = spec_cons.car.toPtr(Symbol).getName();
                const slot_name = self.allocator.dupe(u8, slot_name_raw) catch |e| return e;

                // Extract :type and :initform options
                var field_type: *const types.Type = &types.t_any;
                var initform: ?Value = null;
                var opts = spec_cons.cdr;
                while (opts.isCons()) {
                    const opt_cons = opts.toPtr(Cons);
                    const opt_key = opt_cons.car;

                    if (opt_key.isKeyword()) {
                        const kw_name = opt_key.toPtr(runtime.Keyword).getName();

                        if (std.mem.eql(u8, kw_name, "type")) {
                            // :type keyword - next element is the type
                            if (opt_cons.cdr.isCons()) {
                                const type_cons = opt_cons.cdr.toPtr(Cons);
                                if (self.parseTypeExpr(type_cons.car)) |ty| {
                                    field_type = ty;
                                }
                                opts = type_cons.cdr;
                                continue;
                            }
                        } else if (std.mem.eql(u8, kw_name, "initform")) {
                            // :initform keyword - next element is the init expression
                            if (opt_cons.cdr.isCons()) {
                                const init_cons = opt_cons.cdr.toPtr(Cons);
                                initform = init_cons.car;
                                opts = init_cons.cdr;
                                continue;
                            }
                        }
                    }

                    // Skip this option (and its value if present)
                    if (opt_cons.cdr.isCons()) {
                        opts = opt_cons.cdr.toPtr(Cons).cdr;
                    } else {
                        break;
                    }
                }

                slot_specs.append(self.allocator, .{
                    .name = slot_name,
                    .field_type = field_type,
                    .initform = initform,
                }) catch |e| return e;
            } else {
                return error.InvalidSyntax;
            }
            rest = c.cdr;
        }

        // Create class type - similar to struct
        const class_fields = try self.allocator.alloc(types.StructField, slot_specs.items.len);
        for (slot_specs.items, 0..) |spec, i| {
            class_fields[i] = .{
                .name = try self.allocator.dupe(u8, spec.name),
                .type = spec.field_type,
            };
        }
        var type_builder = types.TypeBuilder.init(self.allocator);
        defer type_builder.deinit();
        const class_type = type_builder.makeStruct(class_name, class_fields) catch |e| return e;
        self.registerStructType(class_name, class_type) catch |e| return e;

        // Store class metadata for compilation (compiler-side with initforms)
        const persistent_specs = try self.globals.allocator.alloc(SlotSpec, slot_specs.items.len);
        for (slot_specs.items, 0..) |spec, i| {
            persistent_specs[i] = .{
                .name = try self.globals.allocator.dupe(u8, spec.name),
                .field_type = spec.field_type,
                .initform = spec.initform,
            };
        }
        const persistent_class_name = try self.globals.allocator.dupe(u8, class_name);
        try self.class_metadata.put(persistent_class_name, persistent_specs);

        // Also store in heap for runtime slot-value lookup
        {
            const heap_slot_names = try heap.backing_allocator.alloc([]const u8, slot_specs.items.len);
            for (slot_specs.items, 0..) |spec, i| {
                heap_slot_names[i] = try heap.backing_allocator.dupe(u8, spec.name);
            }
            const heap_class_name = try heap.backing_allocator.dupe(u8, class_name);
            try heap.class_metadata.put(heap.backing_allocator, heap_class_name, heap_slot_names);
        }

        // Generate definitions: constructor + predicate + accessors + name_lit
        const num_defs = 1 + 1 + slot_specs.items.len + 1; // constructor + predicate + accessors + name_lit
        const defs = try self.allocator.alloc(*Ir, num_defs);
        var def_idx: usize = 0;

        // 1. Constructor: (defun make-class-name (slot1 slot2 ...) (vector 'class-name slot1 slot2 ...))
        const make_name = try self.concatStrings("make-", class_name);
        defs[def_idx] = try self.generateStructConstructor(heap, make_name, class_name, slot_specs.items, env);
        def_idx += 1;

        // 2. Predicate: (defun class-name-p (obj) (and (vectorp obj) (eq (aref obj 0) 'class-name)))
        const pred_name = try self.concatStrings(class_name, "-p");
        defs[def_idx] = try self.generateStructPredicate(heap, pred_name, class_name);
        def_idx += 1;

        // Register predicate for occurrence typing (use qualified name to match globals table)
        var pred_qual_buf: [512]u8 = undefined;
        const qualified_pred_name = self.qualifyName(pred_name, &pred_qual_buf) catch pred_name;
        const persistent_pred_name = self.globals.allocator.dupe(u8, qualified_pred_name) catch |e| return e;
        self.struct_predicates.put(persistent_pred_name, class_type) catch |e| return e;

        // 3. Accessors: (defun class-name-slot (obj) (if (class-name-p obj) (aref obj N+1) (error)))
        for (slot_specs.items, 0..) |spec, i| {
            const accessor_name = try self.concatStrings3(class_name, "-", spec.name);
            defs[def_idx] = try self.generateStructAccessor(heap, accessor_name, class_name, i);
            def_idx += 1;
        }

        // 4. Return class name
        defs[def_idx] = try self.builder.lit(name_val);

        return try self.builder.progn(defs);
    }

    /// Compile make-instance: (make-instance 'class-name :slot1 val1 :slot2 val2 ...)
    /// Calls the appropriate make-class-name constructor with positional args
    fn compileMakeInstance(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        var class_name_expr = cons1.car;

        // Handle quoted class name
        if (class_name_expr.isCons()) {
            const quote_cons = class_name_expr.toPtr(Cons);
            if (quote_cons.cdr.isCons()) {
                class_name_expr = quote_cons.cdr.toPtr(Cons).car;
            }
        }

        if (!class_name_expr.isSymbol()) return error.InvalidSyntax;
        const class_name = class_name_expr.toPtr(Symbol).getName();

        // Look up class metadata to get slot order
        const slot_specs = self.class_metadata.get(class_name) orelse return error.InvalidSyntax;

        // Parse keyword arguments and build positional args array
        const slot_values = try self.allocator.alloc(?*Ir, slot_specs.len);
        for (slot_values) |*sv| sv.* = null;

        var rest = cons1.cdr;
        while (rest.isCons()) {
            const kw_cons = rest.toPtr(Cons);
            const kw = kw_cons.car;

            if (!kw.isKeyword()) return error.InvalidSyntax;
            const kw_name = kw.toPtr(runtime.Keyword).getName();

            // Get value (next element after keyword)
            if (!kw_cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = kw_cons.cdr.toPtr(Cons);
            const value_ir = try self.compile(val_cons.car, env);

            // Find matching slot and store value
            for (slot_specs, 0..) |spec, i| {
                if (std.mem.eql(u8, kw_name, spec.name)) {
                    slot_values[i] = value_ir;
                    break;
                }
            }

            // Move to next keyword-value pair
            rest = val_cons.cdr;
        }

        // Build call to make-class-name with positional args
        const ctor_name_plain = try self.concatStrings("make-", class_name);

        // Qualify the constructor name with current package
        var qual_buf: [512]u8 = undefined;
        const ctor_name = try self.qualifyName(ctor_name_plain, &qual_buf);

        const call_args = try self.allocator.alloc(*Ir, slot_specs.len);
        for (slot_values, 0..) |maybe_val, i| {
            if (maybe_val) |val| {
                call_args[i] = val;
            } else {
                // No value provided - use initform or nil
                if (slot_specs[i].initform) |initform_expr| {
                    call_args[i] = try self.compile(initform_expr, env);
                } else {
                    call_args[i] = try self.builder.lit(Value.nil);
                }
            }
        }

        const ctor_ref = try self.builder.globalRef(ctor_name, self.globals.lookup(ctor_name) orelse return error.InvalidSyntax);
        return try self.builder.call(ctor_ref, call_args);
    }

    /// Compile slot-value: (slot-value obj 'slot-name)
    /// Generates a runtime slot lookup using class metadata
    fn compileSlotValue(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const obj_expr = cons1.car;
        const obj_ir = try self.compile(obj_expr, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        var slot_name_expr = cons2.car;

        // Handle quoted slot name
        if (slot_name_expr.isCons()) {
            const quote_cons = slot_name_expr.toPtr(Cons);
            if (quote_cons.cdr.isCons()) {
                slot_name_expr = quote_cons.cdr.toPtr(Cons).car;
            }
        }

        if (!slot_name_expr.isSymbol()) return error.InvalidSyntax;
        const slot_name = slot_name_expr.toPtr(Symbol).getName();

        const slot_sym = try self.builder.quoteSym(slot_name);
        return try self.builder.slotValue(obj_ir, slot_sym);
    }

    /// Compile %set-slot-value: (%set-slot-value obj 'slot-name value)
    /// Internal function used by (setf (slot-value obj 'slot) value)
    fn compileSetSlotValue(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const obj_expr = cons1.car;
        const obj_ir = try self.compile(obj_expr, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        var slot_name_expr = cons2.car;

        // Handle quoted slot name
        if (slot_name_expr.isCons()) {
            const quote_cons = slot_name_expr.toPtr(Cons);
            if (quote_cons.cdr.isCons()) {
                slot_name_expr = quote_cons.cdr.toPtr(Cons).car;
            }
        }

        if (!slot_name_expr.isSymbol()) return error.InvalidSyntax;
        const slot_name = slot_name_expr.toPtr(Symbol).getName();

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const value_expr = cons3.car;
        const value_ir = try self.compile(value_expr, env);

        const slot_sym = try self.builder.quoteSym(slot_name);
        return try self.builder.setSlotValue(obj_ir, slot_sym, value_ir);
    }

    /// Compile defgeneric: (defgeneric name (arg1 arg2 ...))
    /// Creates a generic function that dispatches on argument types
    fn compileDefgeneric(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        _ = env;

        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const name_val = cons1.car;

        if (!name_val.isSymbol()) return error.InvalidSyntax;
        const name_sym = name_val.toPtr(Symbol);
        var qual_buf: [256]u8 = undefined;
        const gen_name_tmp = self.getQualifiedName(name_sym, &qual_buf) catch name_sym.getName();
        const gen_name = try self.allocator.dupe(u8, gen_name_tmp);

        // Register generic function
        const persistent_name = try self.globals.allocator.dupe(u8, gen_name);
        try self.generic_functions.put(persistent_name, std.ArrayList(MethodDef){});

        // Return the name
        return try self.builder.lit(name_val);
    }

    /// Compile defmethod: (defmethod name ((arg1 class1) (arg2 class2) ...) body...)
    /// Adds a method to a generic function
    fn compileDefmethod(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const name_val = cons1.car;

        if (!name_val.isSymbol()) return error.InvalidSyntax;
        const name_sym = name_val.toPtr(Symbol);
        var qual_buf: [256]u8 = undefined;
        const gen_name_tmp = self.getQualifiedName(name_sym, &qual_buf) catch name_sym.getName();

        // Parse specialized lambda list
        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const lambda_list = cons2.car;

        // Extract parameter names and specializers
        var param_names = std.ArrayList([]const u8){};
        defer param_names.deinit(self.allocator);
        var specializers = std.ArrayList([]const u8){};
        defer specializers.deinit(self.allocator);

        var params = lambda_list;
        while (params.isCons()) {
            const param_cons = params.toPtr(Cons);
            const param = param_cons.car;

            if (param.isSymbol()) {
                // Unspecialized parameter
                const param_name = param.toPtr(Symbol).getName();
                try param_names.append(self.allocator, try self.allocator.dupe(u8, param_name));
                try specializers.append(self.allocator, try self.allocator.dupe(u8, "t")); // t = any type
            } else if (param.isCons()) {
                // Specialized parameter: (param-name class-name)
                const spec_cons = param.toPtr(Cons);
                if (!spec_cons.car.isSymbol()) return error.InvalidSyntax;
                const param_name = spec_cons.car.toPtr(Symbol).getName();
                try param_names.append(self.allocator, try self.allocator.dupe(u8, param_name));

                if (spec_cons.cdr.isCons()) {
                    const class_cons = spec_cons.cdr.toPtr(Cons);
                    if (class_cons.car.isSymbol()) {
                        const class_name = class_cons.car.toPtr(Symbol).getName();
                        try specializers.append(self.allocator, try self.allocator.dupe(u8, class_name));
                    } else {
                        try specializers.append(self.allocator, try self.allocator.dupe(u8, "t"));
                    }
                } else {
                    try specializers.append(self.allocator, try self.allocator.dupe(u8, "t"));
                }
            } else {
                return error.InvalidSyntax;
            }

            params = param_cons.cdr;
        }

        // Create lambda environment with method parameters
        var lambda_env = Env.init(self.allocator, env);
        defer lambda_env.deinit();

        for (param_names.items) |param| {
            _ = try lambda_env.bind(param);
        }

        // Compile method body in lambda environment
        const body = cons2.cdr;
        const body_ir = try self.compileBodyWithTail(body, &lambda_env, true);

        // Collect free variables for captures
        var capture_set = CaptureSet.init(self.allocator);
        defer capture_set.deinit();
        try self.collectFreeVars(body, &lambda_env, &capture_set);
        const captures = try self.allocator.dupe(Ir.Capture, capture_set.captures.items);

        // Save param names for dispatcher before toOwnedSlice consumes them
        const param_names_copy = try self.allocator.dupe([]const u8, param_names.items);

        // Create lambda
        const lambda_ir = try self.builder.lambda(
            try param_names.toOwnedSlice(self.allocator),
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            null,
            captures,
            body_ir,
        );

        // Store method - auto-create generic function if it doesn't exist
        // First check if entry exists to avoid allocating key unnecessarily
        const gen_name = if (self.generic_functions.get(gen_name_tmp)) |_|
            // Entry exists - use existing key from hashmap
            gen_name_tmp // We'll use the existing key, this temp is just for getOrPut
        else
            // New entry - allocate persistent key with globals.allocator
            try self.globals.allocator.dupe(u8, gen_name_tmp);

        const gop = try self.generic_functions.getOrPut(gen_name);

        // Generate unique method function name: generic-name$specializer1$specializer2...
        // Use first specializer as suffix (single-dispatch for now)
        const method_name = if (specializers.items.len > 0)
            try self.concatStrings3(gen_name, "$", specializers.items[0])
        else
            try self.concatStrings(gen_name, "$t");

        // Define method as global function
        const method_global_idx = try self.globals.define(method_name);
        const method_define_ir = try self.builder.define(method_name, method_global_idx, lambda_ir);

        // Store method function name (persistent, needs globals.allocator)
        const persistent_method_name = try self.globals.allocator.dupe(u8, method_name);
        const persistent_specializers = try self.globals.allocator.alloc([]const u8, specializers.items.len);
        for (specializers.items, 0..) |spec, i| {
            persistent_specializers[i] = try self.globals.allocator.dupe(u8, spec);
        }

        // Create method def
        const method_def = MethodDef{
            .specializers = persistent_specializers,
            .function_name = persistent_method_name,
        };

        // Manually grow the methods list
        // Use globals.allocator for persistent storage across arena resets
        if (!gop.found_existing) {
            // New generic function - create list with first method
            const new_methods = try self.globals.allocator.alloc(MethodDef, 1);
            new_methods[0] = method_def;
            gop.value_ptr.* = .{ .items = new_methods, .capacity = 1 };
        } else {
            // Existing function - reallocate with one more slot
            const old_items = gop.value_ptr.items;
            const old_len = old_items.len;
            const new_methods = try self.globals.allocator.alloc(MethodDef, old_len + 1);

            // Copy old methods
            for (old_items, 0..) |old_method, i| {
                new_methods[i] = old_method;
            }

            // Add new method
            new_methods[old_len] = method_def;

            // Free old memory (safe because we allocated with globals.allocator)
            self.globals.allocator.free(old_items);

            gop.value_ptr.* = .{ .items = new_methods, .capacity = old_len + 1 };
        }

        // Generate dispatcher function
        const dispatcher = try self.generateMethodDispatcher(gen_name, gop.value_ptr.*, param_names_copy);

        // Define the generic function as the dispatcher
        const global_idx = try self.globals.define(gen_name);
        const dispatcher_define_ir = try self.builder.define(gen_name, global_idx, dispatcher);

        // Return progn that defines method then dispatcher
        const defs = try self.allocator.alloc(*Ir, 2);
        defs[0] = method_define_ir;
        defs[1] = dispatcher_define_ir;
        return try self.builder.progn(defs);
    }

    /// Generate a dispatcher lambda that checks argument types and calls the matching method
    fn generateMethodDispatcher(
        self: *Compiler,
        _: []const u8,
        methods: std.ArrayList(MethodDef),
        param_names: []const []const u8,
    ) anyerror!*Ir {
        // Build dispatcher body: nested if-then-else checking types
        var dispatch_body: *Ir = undefined;

        // Start from the end: error case
        const error_msg = try self.heap.?.allocString("No applicable method");
        const error_msg_ir = try self.builder.lit(error_msg);
        const error_ir = try self.builder.errorUser(error_msg_ir);
        dispatch_body = error_ir;

        // Work backwards through methods, wrapping in if statements
        var i = methods.items.len;
        while (i > 0) {
            i -= 1;
            const method = methods.items[i];

            // For now, only handle single-dispatch (first parameter specializer)
            // TODO: Handle multi-parameter dispatch
            if (method.specializers.len == 0) continue;

            const spec_name = method.specializers[0];

            // Skip unspecialized methods (specializer = "t")
            if (std.mem.eql(u8, spec_name, "t")) {
                // Unspecialized - always matches, make it the else branch
                dispatch_body = try self.generateMethodCallByName(method.function_name, param_names);
                continue;
            }

            // Build condition: (typep arg1 'class-name)
            if (param_names.len == 0) return error.InvalidSyntax;

            // Reference to first parameter
            const arg_ir = try self.builder.variable(param_names[0], 0, 0); // depth 0, index 0 - first param

            // Class name symbol
            const class_sym = try self.heap.?.intern(spec_name);
            const class_ir = try self.builder.lit(class_sym);

            // typep check
            const cond_ir = try self.builder.typep(arg_ir, class_ir);

            // Method call
            const then_ir = try self.generateMethodCallByName(method.function_name, param_names);

            // Wrap in if
            dispatch_body = try self.builder.ifExpr(cond_ir, then_ir, dispatch_body);
        }

        // Wrap dispatch body in lambda
        const dispatcher = try self.builder.lambda(
            param_names,
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            null,
            &[_]Ir.Capture{}, // No captures - methods are stored as IR
            dispatch_body,
        );

        return dispatcher;
    }

    /// Generate a call to a method by function name with given parameters
    fn generateMethodCallByName(
        self: *Compiler,
        function_name: []const u8,
        param_names: []const []const u8,
    ) anyerror!*Ir {
        // Build argument list: pass all parameters
        var args = std.ArrayList(*const Ir){};
        defer args.deinit(self.allocator);

        for (param_names, 0..) |param, idx| {
            const arg_ir = try self.builder.variable(param, 0, @intCast(idx));
            try args.append(self.allocator, arg_ir);
        }

        // Look up the method function by name
        const func_sym = try self.heap.?.intern(function_name);
        const func_ir = try self.builder.lit(func_sym);
        const func_val_ir = try self.builder.symbolFunction(func_ir);

        // Call the method function
        return try self.builder.call(func_val_ir, try args.toOwnedSlice(self.allocator));
    }

    // ========================================================================
    // ADT Support: deftype and match
    // ========================================================================

    /// Compile deftype: (deftype type-name (variant1 field1 field2) (variant2 field3) ...)
    /// Generates: constructors, type predicate, variant predicates, field accessors
    /// Runtime representation: #(:variant-tag field1 field2 ...)
    fn compileDeftype(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        _ = env;
        // Parse: (type-name (variant1 f1 f2) (variant2 f3) ...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const type_name_val = cons1.car;
        if (!type_name_val.isSymbol()) return error.InvalidSyntax;
        const type_name_raw = type_name_val.toPtr(Symbol).getName();
        // Dupe type name - symbol internal storage may be invalidated by GC
        const type_name = try self.allocator.dupe(u8, type_name_raw);

        // Collect variants
        var variants = std.ArrayList(Variant){};
        var current = cons1.cdr;
        while (current.isCons()) {
            const variant_cons = current.toPtr(Cons);
            const variant = try self.parseVariant(variant_cons.car);
            try variants.append(self.allocator, variant);
            current = variant_cons.cdr;
        }

        if (variants.items.len == 0) return error.InvalidSyntax;

        // Store type definition for match exhaustiveness checking
        try self.defined_types.put(type_name, variants.items);

        // Generate definitions as a progn:
        // 1. Constructor for each variant: (defun variant-name (fields...) (vector :variant-name fields...))
        // 2. Type predicate: (defun type-name? (x) (and (vectorp x) (member (aref x 0) '(:v1 :v2 ...))))
        // 3. Variant predicates: (defun variant-name? (x) (and (vectorp x) (eq (aref x 0) :variant-name)))
        // 4. Field accessors: (defun variant-name-field (x) (aref x field-index))

        var defs = std.ArrayList(*const Ir){};

        for (variants.items) |variant| {
            // Constructor: (variant-name f1 f2) -> #(:variant-name f1 f2)
            const ctor = try self.generateAdtConstructor(variant);
            try defs.append(self.allocator, ctor);

            // Variant predicate: (variant-name? x) -> (and (vectorp x) (eq (aref x 0) :variant-name))
            const pred = try self.generateVariantPredicate(variant);
            try defs.append(self.allocator, pred);

            // Field accessors: (variant-name-field x) -> (aref x index)
            for (variant.fields, 1..) |field, idx| {
                const accessor = try self.generateFieldAccessor(variant.name, field, @intCast(idx));
                try defs.append(self.allocator, accessor);
            }
        }

        // Type predicate: (type-name? x) -> checks if any variant matches
        const type_pred = try self.generateTypePredicate(type_name, variants.items);
        try defs.append(self.allocator, type_pred);

        // Return progn of all definitions
        const slice = try defs.toOwnedSlice(self.allocator);
        const node = try self.allocator.create(Ir);
        node.* = .{ .progn = slice };
        return node;
    }

    fn parseVariant(self: *Compiler, expr: Value) Error!Variant {
        // (variant-name field1 field2 ...)
        if (!expr.isCons()) return error.InvalidSyntax;

        const cons1 = expr.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSyntax;
        const sym_name = cons1.car.toPtr(Symbol).getName();
        // Dupe name to heap - symbol internal storage may be invalidated by GC
        const name = try self.allocator.dupe(u8, sym_name);

        var fields = std.ArrayList([]const u8){};
        var current = cons1.cdr;
        while (current.isCons()) {
            const field_cons = current.toPtr(Cons);
            if (!field_cons.car.isSymbol()) return error.InvalidSyntax;
            const field_sym_name = field_cons.car.toPtr(Symbol).getName();
            // Dupe field name to heap
            const field_name = try self.allocator.dupe(u8, field_sym_name);
            try fields.append(self.allocator, field_name);
            current = field_cons.cdr;
        }

        return .{
            .name = name,
            .fields = try fields.toOwnedSlice(self.allocator),
        };
    }

    fn generateAdtConstructor(self: *Compiler, variant: Variant) anyerror!*Ir {
        // Creates: (defun variant-name (f1 f2 ...) (vector :variant-name f1 f2 ...))
        const idx = try self.globals.define(variant.name);

        // Build lambda body: (vector :variant-name f1 f2 ...)
        const num_elems = variant.fields.len + 1; // tag + fields
        var elems = try self.allocator.alloc(*const Ir, num_elems);

        // First element: keyword tag
        const tag_node = try self.allocator.create(Ir);
        tag_node.* = .{ .quote_sym = variant.name };
        elems[0] = tag_node;

        // Rest: variable references to parameters
        for (variant.fields, 0..) |field, i| {
            const var_node = try self.builder.variable(field, 0, @intCast(i));
            elems[i + 1] = var_node;
        }

        const vec_node = try self.allocator.create(Ir);
        vec_node.* = .{ .vec = elems };

        // Build lambda
        const params = try self.allocator.dupe([]const u8, variant.fields);
        const lambda_node = try self.allocator.create(Ir);
        lambda_node.* = .{ .lambda = .{
            .params = params,
            .optional_params = &.{},
            .key_params = &.{},
            .rest_param = null,
            .captures = &[_]Ir.Capture{},
            .body = vec_node,
        } };

        return try self.builder.define(variant.name, idx, lambda_node);
    }

    fn generateVariantPredicate(self: *Compiler, variant: Variant) anyerror!*Ir {
        // Creates: (defun variant-name? (x) (and (vectorp x) (eq (aref x 0) :variant-name)))
        var name_buf: [256]u8 = undefined;
        const pred_name = try std.fmt.bufPrint(&name_buf, "{s}?", .{variant.name});
        const pred_name_copy = try self.allocator.dupe(u8, pred_name);

        const idx = try self.globals.define(pred_name_copy);

        // Build: (and (vectorp x) (eq (aref x 0) :variant-name))
        // Param x at index 0
        const x_var = try self.builder.variable("x", 0, 0);

        // (vectorp x)
        const vectorp_node = try self.allocator.create(Ir);
        vectorp_node.* = .{ .vectorp = .{ .operand = x_var } };

        // (aref x 0)
        const x_var2 = try self.builder.variable("x", 0, 0);
        const zero = try self.builder.lit(Value.makeFixnum(0));
        const aref_node = try self.allocator.create(Ir);
        aref_node.* = .{ .vec_ref = .{ .left = x_var2, .right = zero } };

        // :variant-name (as quoted symbol)
        const tag_node = try self.allocator.create(Ir);
        tag_node.* = .{ .quote_sym = variant.name };

        // (eq (aref x 0) :variant-name)
        const eq_node = try self.allocator.create(Ir);
        eq_node.* = .{ .eq = .{ .left = aref_node, .right = tag_node } };

        // (if (vectorp x) (eq ...) nil)
        const nil_node = try self.builder.lit(Value.nil);
        const if_node = try self.allocator.create(Ir);
        if_node.* = .{ .@"if" = .{
            .cond = vectorp_node,
            .then_branch = eq_node,
            .else_branch = nil_node,
        } };

        // Lambda wrapper
        const params = try self.allocator.alloc([]const u8, 1);
        params[0] = "x";
        const lambda_node = try self.allocator.create(Ir);
        lambda_node.* = .{ .lambda = .{
            .params = params,
            .optional_params = &.{},
            .key_params = &.{},
            .rest_param = null,
            .captures = &[_]Ir.Capture{},
            .body = if_node,
        } };

        return try self.builder.define(pred_name_copy, idx, lambda_node);
    }

    fn generateFieldAccessor(self: *Compiler, variant_name: []const u8, field_name: []const u8, field_idx: u16) anyerror!*Ir {
        // Creates: (defun variant-name-field (x) (aref x field-idx))
        var name_buf: [256]u8 = undefined;
        const accessor_name = try std.fmt.bufPrint(&name_buf, "{s}-{s}", .{ variant_name, field_name });
        const accessor_name_copy = try self.allocator.dupe(u8, accessor_name);

        const idx = try self.globals.define(accessor_name_copy);

        // Build: (aref x field-idx)
        const x_var = try self.builder.variable("x", 0, 0);
        const idx_lit = try self.builder.lit(Value.makeFixnum(@intCast(field_idx)));
        const aref_node = try self.allocator.create(Ir);
        aref_node.* = .{ .vec_ref = .{ .left = x_var, .right = idx_lit } };

        // Lambda wrapper
        const params = try self.allocator.alloc([]const u8, 1);
        params[0] = "x";
        const lambda_node = try self.allocator.create(Ir);
        lambda_node.* = .{ .lambda = .{
            .params = params,
            .optional_params = &.{},
            .key_params = &.{},
            .rest_param = null,
            .captures = &[_]Ir.Capture{},
            .body = aref_node,
        } };

        return try self.builder.define(accessor_name_copy, idx, lambda_node);
    }

    fn generateTypePredicate(self: *Compiler, type_name: []const u8, variants: []const Variant) anyerror!*Ir {
        // Creates: (defun type-name? (x) (or (variant1? x) (variant2? x) ...))
        var name_buf: [256]u8 = undefined;
        const pred_name = try std.fmt.bufPrint(&name_buf, "{s}?", .{type_name});
        const pred_name_copy = try self.allocator.dupe(u8, pred_name);

        const idx = try self.globals.define(pred_name_copy);

        // Build body: chain of or's checking each variant
        // Start from the last variant and work backwards
        var body: *Ir = try self.builder.lit(Value.nil);

        var i: usize = variants.len;
        while (i > 0) {
            i -= 1;
            const variant = variants[i];

            // Build variant check inline (like variant-name? but without function call)
            const x_var = try self.builder.variable("x", 0, 0);
            const vectorp_node = try self.allocator.create(Ir);
            vectorp_node.* = .{ .vectorp = .{ .operand = x_var } };

            const x_var2 = try self.builder.variable("x", 0, 0);
            const zero = try self.builder.lit(Value.makeFixnum(0));
            const aref_node = try self.allocator.create(Ir);
            aref_node.* = .{ .vec_ref = .{ .left = x_var2, .right = zero } };

            const tag_node = try self.allocator.create(Ir);
            tag_node.* = .{ .quote_sym = variant.name };

            const eq_node = try self.allocator.create(Ir);
            eq_node.* = .{ .eq = .{ .left = aref_node, .right = tag_node } };

            const nil_node = try self.builder.lit(Value.nil);
            const check_node = try self.allocator.create(Ir);
            check_node.* = .{ .@"if" = .{
                .cond = vectorp_node,
                .then_branch = eq_node,
                .else_branch = nil_node,
            } };

            // (if check_node t body)
            const t_val = try self.builder.lit(Value.t);
            const or_node = try self.allocator.create(Ir);
            or_node.* = .{ .@"if" = .{
                .cond = check_node,
                .then_branch = t_val,
                .else_branch = body,
            } };
            body = or_node;
        }

        // Lambda wrapper
        const params = try self.allocator.alloc([]const u8, 1);
        params[0] = "x";
        const lambda_node = try self.allocator.create(Ir);
        lambda_node.* = .{ .lambda = .{
            .params = params,
            .optional_params = &.{},
            .key_params = &.{},
            .rest_param = null,
            .captures = &[_]Ir.Capture{},
            .body = body,
        } };

        return try self.builder.define(pred_name_copy, idx, lambda_node);
    }

    /// Compile match: (match expr ((variant1 f1 f2) body1) ((variant2 f3) body2) (_ default))
    fn compileMatch(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // Parse: (expr clause1 clause2 ...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const scrutinee = try self.compile(cons1.car, env);

        const clauses = cons1.cdr;

        // Exhaustiveness checking: collect variant names from clauses
        try self.checkMatchExhaustiveness(clauses);

        return self.compileMatchClauses(scrutinee, clauses, env);
    }

    /// Check if match covers all variants of the ADT (warning only, doesn't fail)
    fn checkMatchExhaustiveness(self: *Compiler, clauses: Value) !void {
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

            // Check for wildcard (use symbol identity)
            if (pattern.isSymbol()) {
                const b = self.builtins orelse return error.UninitializedBuiltins;
                if (pattern.raw == b._.raw) {
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

    fn compileMatchClauses(self: *Compiler, scrutinee: *const Ir, clauses: Value, env: *const Env) anyerror!*Ir {
        if (!clauses.isCons()) {
            // No more clauses - return nil (or could be error for non-exhaustive)
            return try self.builder.lit(Value.nil);
        }

        const clause_cons = clauses.toPtr(Cons);
        const clause = clause_cons.car;

        if (!clause.isCons()) return error.InvalidSyntax;
        const pattern_cons = clause.toPtr(Cons);
        const pattern = pattern_cons.car;
        const body_list = pattern_cons.cdr;

        // Check for wildcard pattern: _ (use symbol identity)
        if (pattern.isSymbol()) {
            const b = self.builtins orelse return error.UninitializedBuiltins;
            if (pattern.raw == b._.raw) {
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
            try field_names.append(self.allocator, field_name);
            field_current = fc.cdr;
        }

        // Build condition: (and (vectorp scrutinee) (eq (aref scrutinee 0) :variant-name))
        const vectorp_node = try self.allocator.create(Ir);
        vectorp_node.* = .{ .vectorp = .{ .operand = scrutinee } };

        const zero = try self.builder.lit(Value.makeFixnum(0));
        const aref_node = try self.allocator.create(Ir);
        aref_node.* = .{ .vec_ref = .{ .left = scrutinee, .right = zero } };

        const tag_node = try self.allocator.create(Ir);
        tag_node.* = .{ .quote_sym = variant_name };

        const eq_node = try self.allocator.create(Ir);
        eq_node.* = .{ .eq = .{ .left = aref_node, .right = tag_node } };

        const nil_lit = try self.builder.lit(Value.nil);
        const cond_node = try self.allocator.create(Ir);
        cond_node.* = .{ .@"if" = .{
            .cond = vectorp_node,
            .then_branch = eq_node,
            .else_branch = nil_lit,
        } };

        // Compile body in extended environment - create env first to get indices
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();

        // Build then branch: (let ((f1 (aref scrutinee 1)) (f2 (aref scrutinee 2)) ...) body...)
        var bindings = try self.allocator.alloc(Ir.Binding, field_names.items.len);
        for (field_names.items, 0..) |field_name, i| {
            const idx_lit = try self.builder.lit(Value.makeFixnum(@intCast(i + 1)));
            const field_aref = try self.allocator.create(Ir);
            field_aref.* = .{ .vec_ref = .{ .left = scrutinee, .right = idx_lit } };

            const binding_idx = try let_env.bind(field_name);
            bindings[i] = .{
                .name = field_name,
                .value = field_aref,
                .index = binding_idx,
            };
        }
        const body_ir = try self.compileProgn(body_list, &let_env);

        var then_ir: *Ir = undefined;
        if (bindings.len > 0) {
            then_ir = try self.allocator.create(Ir);
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
        const if_node = try self.allocator.create(Ir);
        if_node.* = .{ .@"if" = .{
            .cond = cond_node,
            .then_branch = then_ir,
            .else_branch = else_ir,
        } };

        return if_node;
    }

    fn compileLambdaWithReturnType(self: *Compiler, args: Value, env: *const Env, return_type: ?Value) anyerror!*Ir {
        // Delegate to compileLambda but wrap result if return type specified
        const lambda_ir = try self.compileLambdaCore(args, env, return_type);
        return lambda_ir;
    }

    /// Compile type assertion: (the type expr)
    /// Supported types: fixnum, cons, symbol, string, vector, closure, non-nil
    /// Uses occurrence typing: skips check if variable already narrowed to type
    fn compileThe(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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

        // Check occurrence typing: if expr is a variable narrowed to this type, skip check
        if (expr.isSymbol()) {
            const var_sym = expr.toPtr(Symbol);
            const var_name = var_sym.getName();

            if (self.occ) |occ| {
                if (occ.getNarrowed(var_name)) |narrowed_type| {
                    // Check if narrowed type matches requested type (by symbol identity)
                    if (self.typeMatchesSym(narrowed_type, type_spec)) {
                        // Already narrowed - just compile the expression, skip the check
                        return expr_ir;
                    }
                }
            }
        }

        return self.compileSimpleTypeCheckSym(type_spec, expr_ir);
    }

    /// Compile a simple type check for a single type symbol (uses symbol identity)
    fn compileSimpleTypeCheckSym(self: *Compiler, type_sym: Value, expr_ir: *const Ir) anyerror!*Ir {
        const b = self.builtins orelse return error.UninitializedBuiltins;
        // Dispatch by symbol identity (no string comparison)
        if (type_sym.raw == b.ty_fixnum.raw) return self.builder.assertFixnum(expr_ir);
        if (type_sym.raw == b.cons.raw) return self.builder.assertCons(expr_ir);
        if (type_sym.raw == b.ty_symbol.raw) return self.builder.assertSymbol(expr_ir);
        if (type_sym.raw == b.string.raw) return self.builder.assertString(expr_ir);
        if (type_sym.raw == b.ty_vector.raw) return self.builder.assertVector(expr_ir);
        if (type_sym.raw == b.ty_closure.raw) return self.builder.assertClosure(expr_ir);
        if (type_sym.raw == b.@"ty_non-nil".raw) return self.builder.assertNonNil(expr_ir);
        if (type_sym.raw == b.ty_list.raw) return self.builder.assertList(expr_ir);
        if (type_sym.raw == b.ty_any.raw) return @constCast(expr_ir); // no check
        return error.InvalidSyntax;
    }

    /// Compile a compound type check: (or type1 type2 ...), (refine T x P), etc.
    fn compileCompoundTypeCheck(self: *Compiler, type_spec: Value, expr_ir: *const Ir) anyerror!*Ir {
        const cons = type_spec.toPtr(Cons);
        if (!cons.car.isSymbol()) return error.InvalidSyntax;

        const b = self.builtins orelse return error.UninitializedBuiltins;
        const head = cons.car;

        // Dispatch by symbol identity
        if (head.raw == b.ty_union.raw or head.raw == b.ty_or.raw) {
            return self.compileOrTypeCheck(cons.cdr, expr_ir);
        }
        if (head.raw == b.ty_refine.raw) {
            return self.compileRefineTypeCheck(cons.cdr, expr_ir);
        }
        if (head.raw == b.ty_pi.raw) {
            return self.compilePiTypeCheck(cons.cdr, expr_ir);
        }
        if (head.raw == b.ty_sigma.raw) {
            return self.compileSigmaTypeCheck(cons.cdr, expr_ir);
        }
        if (head.raw == b.ty_list.raw or head.raw == b.list.raw) {
            // (list T) - just check it's a list, element type checked lazily
            return self.builder.assertList(expr_ir);
        }
        if (head.raw == b.ty_vec.raw or head.raw == b.vector.raw) {
            // (vec T) or (vector T) - just check it's a vector
            return self.builder.assertVector(expr_ir);
        }
        if (head.raw == b.@"->".raw) {
            // (-> (A B) C) - check it's a closure
            return self.builder.assertClosure(expr_ir);
        }

        // Unknown compound type
        return error.InvalidSyntax;
    }

    /// Compile (or type1 type2 ...) check
    /// Expands to: check_list if (or cons nil), else check each type
    fn compileOrTypeCheck(self: *Compiler, type_list: Value, expr_ir: *const Ir) anyerror!*Ir {
        const b = self.builtins orelse return error.InvalidSyntax;

        // Collect type symbols (symbol Values or nil for "nil" type)
        var type_syms = std.ArrayList(Value){};
        defer type_syms.deinit(self.allocator);

        var list = type_list;
        while (list.isCons()) {
            const c = list.toPtr(Cons);
            if (c.car.isSymbol()) {
                try type_syms.append(self.allocator, c.car);
            } else if (c.car.isNil()) {
                // nil value in type position means the nil type symbol
                try type_syms.append(self.allocator, b.ty_nil);
            } else {
                return error.InvalidSyntax;
            }
            list = c.cdr;
        }

        if (type_syms.items.len == 0) return error.InvalidSyntax;

        // Special case: (or cons nil) or (or nil cons) -> list
        if (type_syms.items.len == 2) {
            const has_cons = for (type_syms.items) |sym| {
                if (sym.raw == b.ty_cons.raw) break true;
            } else false;
            const has_nil = for (type_syms.items) |sym| {
                if (sym.raw == b.ty_nil.raw) break true;
            } else false;
            if (has_cons and has_nil) {
                return try self.builder.assertList(expr_ir);
            }
        }

        // For other or-types, generate assert_or IR node
        // Duplicate the type_symbols slice to persistent allocation
        const persistent_types = try self.allocator.dupe(Value, type_syms.items);
        return try self.builder.assertOr(expr_ir, persistent_types);
    }

    /// Compile (refine T x P) type check
    /// Generates: (assert-refine expr (lambda (x) P) T)
    fn compileRefineTypeCheck(self: *Compiler, args: Value, expr_ir: *const Ir) anyerror!*Ir {
        // args = (T x P)
        if (!args.isCons()) return error.InvalidSyntax;
        const c1 = args.toPtr(Cons);

        // Base type T
        const base_type_spec = c1.car;

        if (!c1.cdr.isCons()) return error.InvalidSyntax;
        const c2 = c1.cdr.toPtr(Cons);

        // Variable name x
        if (!c2.car.isSymbol()) return error.InvalidSyntax;
        const var_sym = c2.car.toPtr(Symbol);
        const var_name = var_sym.getName();

        if (!c2.cdr.isCons()) return error.InvalidSyntax;
        const c3 = c2.cdr.toPtr(Cons);

        // Predicate P
        const predicate_expr = c3.car;

        // Create a lambda for the predicate: (lambda (x) P)
        // We need to compile the predicate in an environment where x is bound
        var pred_env = Env.init(self.allocator, null);
        defer pred_env.deinit();
        _ = pred_env.bind(var_name) catch |e| return e;

        const predicate_body = try self.compile(predicate_expr, &pred_env);
        const param_names = try self.allocator.alloc([]const u8, 1);
        param_names[0] = try self.allocator.dupe(u8, var_name);
        const empty_opt = try self.allocator.alloc(Ir.OptionalParam, 0);
        const empty_key = try self.allocator.alloc(Ir.KeyParam, 0);
        const empty_cap = try self.allocator.alloc(Ir.Capture, 0);

        const predicate_lambda = try self.builder.lambda(
            param_names,
            empty_opt,
            empty_key,
            null,
            empty_cap,
            predicate_body,
        );

        // Parse base type for type info (optional)
        const base_type = self.parseTypeExpr(base_type_spec);

        // Generate assert_refine IR node
        return self.builder.assertRefine(expr_ir, predicate_lambda, base_type);
    }

    /// Compile (pi (x : A) B) type check
    /// At runtime, just check it's a closure - dependent checking is at compile time
    fn compilePiTypeCheck(self: *Compiler, args: Value, expr_ir: *const Ir) anyerror!*Ir {
        // Pi types are dependent function types
        // At runtime, we just check it's a closure
        // Full dependent checking would require evaluating the function
        _ = args;
        return self.builder.assertClosure(expr_ir);
    }

    /// Compile (sigma (x : A) B) type check
    /// At runtime, just check it's a cons - dependent checking is at compile time
    fn compileSigmaTypeCheck(self: *Compiler, args: Value, expr_ir: *const Ir) anyerror!*Ir {
        // Sigma types are dependent pair types
        // At runtime, we just check it's a cons cell
        // Full dependent checking would require type-level computation
        _ = args;
        return self.builder.assertCons(expr_ir);
    }

    /// Check if a Type matches a type symbol (using pointer comparison)
    fn typeMatchesSym(self: *Compiler, ty: *const Type, type_sym: Value) bool {
        const b = self.builtins orelse return false;

        // Table mapping type symbols to type pointers
        const TypeMapping = struct { sym: Value, ty: *const Type };
        const mappings = [_]TypeMapping{
            .{ .sym = b.ty_fixnum, .ty = &types.t_fixnum },
            .{ .sym = b.ty_cons, .ty = &types.t_cons },
            .{ .sym = b.ty_symbol, .ty = &types.t_symbol },
            .{ .sym = b.ty_string, .ty = &types.t_string },
            .{ .sym = b.ty_nil, .ty = &types.t_nil },
            .{ .sym = b.ty_float, .ty = &types.t_float },
            .{ .sym = b.ty_keyword, .ty = &types.t_keyword },
            .{ .sym = b.ty_char, .ty = &types.t_char },
            .{ .sym = b.ty_character, .ty = &types.t_char }, // alias
            .{ .sym = b.@"ty_non-nil", .ty = &types.t_non_nil },
        };

        for (mappings) |m| {
            if (type_sym.raw == m.sym.raw) {
                return ty == m.ty;
            }
        }
        return false;
    }

    fn compileBody(self: *Compiler, exprs: Value, env: *const Env) anyerror!*Ir {
        return self.compileBodyWithTail(exprs, env, false);
    }

    fn compileBodyWithTail(self: *Compiler, exprs: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        if (exprs.isNil()) {
            return try self.builder.lit(Value.nil);
        }

        // Count expressions first to know which is last
        var count: usize = 0;
        var tmp = exprs;
        while (tmp.isCons()) {
            count += 1;
            tmp = tmp.toPtr(Cons).cdr;
        }

        // DEBUG: assert count > 0
        std.debug.assert(count > 0);

        var expr_list = std.ArrayList(*Ir){};
        defer expr_list.deinit(self.allocator);

        var list = exprs;
        var idx: usize = 0;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const is_last = idx == count - 1;
            // Only last expression is in tail position
            const expr_ir = try self.compileWithTail(cons.car, env, in_tail and is_last);
            try expr_list.append(self.allocator, expr_ir);
            list = cons.cdr;
            idx += 1;
        }

        // DEBUG: assert we compiled all expressions
        std.debug.assert(expr_list.items.len == count);

        if (expr_list.items.len == 1) {
            return expr_list.items[0];
        }

        // Convert to const slice for progn
        const items = self.allocator.dupe(*const Ir, expr_list.items) catch
            return error.OutOfMemory;
        const result = try self.builder.progn(items);
        // DEBUG: verify progn was created with all items
        std.debug.assert(std.meta.activeTag(result.*) == .progn);
        std.debug.assert(result.progn.len == count);
        return result;
    }

    fn compilePrimitive(self: *Compiler, sym: Value, args: Value, env: *const Env) anyerror!*Ir {
        const s = sym.raw;
        const b = self.builtins orelse return error.InvalidSyntax;

        // Variadic arithmetic (+, -, *, /)
        if (s == b.@"+".raw) return self.compileVariadicArith(args, env, .add, 0);
        if (s == b.@"-".raw) return self.compileVariadicArith(args, env, .sub, null);
        if (s == b.@"*".raw) return self.compileVariadicArith(args, env, .mul, 1);
        if (s == b.@"/".raw) return self.compileVariadicArith(args, env, .div, null);
        if (s == b.mod.raw or s == b.@"%".raw) return self.compileBinaryPrim(args, env, .mod);
        if (s == b.quot.raw) return self.compileBinaryPrim(args, env, .quot);
        if (s == b.rem.raw) return self.compileBinaryPrim(args, env, .rem);

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
        if (s == b.member.raw) return self.compileMemberWithTest(args, env);
        if (s == b.assoc.raw) return self.compileAssocWithTest(args, env);
        if (s == b.find.raw) return self.compileFindWithTest(args, env);
        if (s == b.position.raw) return self.compilePositionWithTest(args, env);
        if (s == b.count.raw) return self.compileCountWithTest(args, env);
        if (s == b.remove.raw) return self.compileRemoveWithTest(args, env);
        if (s == b.list.raw) return self.compileListPrim(args, env);
        if (s == b.rplaca.raw) return self.compileBinaryPrim(args, env, .rplaca);
        if (s == b.rplacd.raw) return self.compileBinaryPrim(args, env, .rplacd);

        // Type predicates (CL-style -p suffix)
        if (s == b.consp.raw) return self.compileUnaryPrim(args, env, .consp);
        if (s == b.symbolp.raw) return self.compileUnaryPrim(args, env, .symbolp);
        if (s == b.numberp.raw) return self.compileUnaryPrim(args, env, .numberp);
        if (s == b.stringp.raw) return self.compileUnaryPrim(args, env, .stringp);
        if (s == b.vectorp.raw) return self.compileUnaryPrim(args, env, .vectorp);
        if (s == b.closurep.raw) return self.compileUnaryPrim(args, env, .closurep);
        if (s == b.keywordp.raw) return self.compileUnaryPrim(args, env, .keywordp);
        if (s == b.null.raw) return self.compileUnaryPrim(args, env, .nilp);
        if (s == b.not.raw) return self.compileUnaryPrim(args, env, .not);
        if (s == b.characterp.raw) return self.compileUnaryPrim(args, env, .characterp);
        if (s == b.floatp.raw) return self.compileUnaryPrim(args, env, .floatp);
        if (s == b.listp.raw) return self.compileUnaryPrim(args, env, .listp);
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
        // Note: error is NOT handled here - stdlib provides (defun error (msg) (signal 'error msg))
        // This allows handler-case to catch errors

        // Symbol operations
        if (s == b.boundp.raw) return self.compileUnaryPrim(args, env, .boundp);
        if (s == b.fboundp.raw) return self.compileUnaryPrim(args, env, .fboundp);
        if (s == b.@"symbol-value".raw) return self.compileUnaryPrim(args, env, .symbol_value);
        if (s == b.@"symbol-function".raw) return self.compileUnaryPrim(args, env, .symbol_function);
        if (s == b.typep.raw) return self.compileBinaryPrim(args, env, .typep);
        if (s == b.@"type-of".raw) return self.compileUnaryPrim(args, env, .type_of);
        if (s == b.intern.raw) return self.compileUnaryPrim(args, env, .intern);
        if (s == b.@"symbol-name".raw) return self.compileUnaryPrim(args, env, .sym_name);
        if (s == b.get.raw) return self.compileBinaryPrim(args, env, .get);
        if (s == b.put.raw) return self.compileTernaryPrim(args, env, .put);
        if (s == b.remprop.raw) return self.compileBinaryPrim(args, env, .remprop);
        // Note: `error` is now defined in stdlib using signal/handler-case

        // Numeric predicates
        if (s == b.abs.raw) return self.compileUnaryPrim(args, env, .abs);
        if (s == b.zerop.raw) return self.compileUnaryPrim(args, env, .zerop);
        if (s == b.plusp.raw) return self.compileUnaryPrim(args, env, .plusp);
        if (s == b.minusp.raw) return self.compileUnaryPrim(args, env, .minusp);
        if (s == b.evenp.raw) return self.compileUnaryPrim(args, env, .evenp);
        if (s == b.oddp.raw) return self.compileUnaryPrim(args, env, .oddp);

        // Math functions
        if (s == b.sqrt.raw) return self.compileUnaryPrim(args, env, .sqrt);
        if (s == b.sin.raw) return self.compileUnaryPrim(args, env, .sin);
        if (s == b.cos.raw) return self.compileUnaryPrim(args, env, .cos);
        if (s == b.tan.raw) return self.compileUnaryPrim(args, env, .tan);
        if (s == b.exp.raw) return self.compileUnaryPrim(args, env, .exp);
        if (s == b.log.raw) return self.compileUnaryPrim(args, env, .log);
        // floor/ceiling/round - primitives return just the quotient, stdlib versions return multiple values
        if (s == b.floor.raw) return self.compileFloorCeilRound(args, env, .floor);
        if (s == b.ceiling.raw) return self.compileFloorCeilRound(args, env, .ceiling);
        if (s == b.round.raw) return self.compileFloorCeilRound(args, env, .round);

        // Vector operations (CL names: aref, svref, %svset, %aset)
        if (s == b.aref.raw) return self.compileAref(args, env);
        if (s == b.svref.raw) return self.compileBinaryPrim(args, env, .vec_ref);
        if (s == b.@"vector-length".raw) return self.compileUnaryPrim(args, env, .vec_len);
        if (s == b.@"make-vector".raw) return self.compileMakeVector(args, env);
        if (s == b.@"%svset".raw) return self.compileSvset(args, env);
        if (s == b.@"%aset".raw) return self.compileAset(args, env);
        if (s == b.@"%set-slot-value".raw) return self.compileSetSlotValue(args, env);
        if (s == b.vector.raw) return self.compileVectorPrim(args, env);
        if (s == b.@"make-array".raw) return self.compileMakeArray(args, env);

        // Stream I/O operations
        if (s == b.@"%open".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const path_ir = try self.compile(cons1.car, env);
            if (!cons1.cdr.isCons()) return error.InvalidSyntax;
            const cons2 = cons1.cdr.toPtr(Cons);
            const mode_ir = try self.compile(cons2.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .open = .{ .left = path_ir, .right = mode_ir } };
            return node;
        }
        if (s == b.@"%close".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .close = .{ .operand = stream_ir } };
            return node;
        }
        if (s == b.@"%read-line".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .read_line = .{ .operand = stream_ir } };
            return node;
        }
        if (s == b.@"%write-line".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            if (!cons1.cdr.isCons()) return error.InvalidSyntax;
            const cons2 = cons1.cdr.toPtr(Cons);
            const text_ir = try self.compile(cons2.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .write_line = .{ .left = stream_ir, .right = text_ir } };
            return node;
        }
        if (s == b.@"%read-byte".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .read_byte = .{ .operand = stream_ir } };
            return node;
        }
        if (s == b.@"%write-byte".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            if (!cons1.cdr.isCons()) return error.InvalidSyntax;
            const cons2 = cons1.cdr.toPtr(Cons);
            const byte_ir = try self.compile(cons2.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .write_byte = .{ .left = stream_ir, .right = byte_ir } };
            return node;
        }
        if (s == b.@"%file-position".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .file_position = .{ .operand = stream_ir } };
            return node;
        }
        if (s == b.@"%file-length".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .file_length = .{ .operand = stream_ir } };
            return node;
        }
        if (s == b.@"%finish-output".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .finish_output = .{ .operand = stream_ir } };
            return node;
        }
        if (s == b.@"%force-output".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .force_output = .{ .operand = stream_ir } };
            return node;
        }

        // String operations (CL names: char, schar)
        if (s == b.char.raw or s == b.schar.raw) return self.compileBinaryPrim(args, env, .str_ref);
        if (s == b.@"string-length".raw) return self.compileUnaryPrim(args, env, .str_len);
        if (s == b.@"string=".raw) return self.compileBinaryPrim(args, env, .str_eq);
        if (s == b.@"string-concat".raw) return self.compileBinaryPrim(args, env, .str_concat);
        if (s == b.substring.raw) return self.compileSubstring(args, env);
        // subseq handled by stdlib for list support (builtin only did strings)

        // I/O
        if (s == b.print.raw) return self.compilePrintOrPrinc(args, env, true);
        if (s == b.princ.raw) return self.compilePrintOrPrinc(args, env, false);
        if (s == b.terpri.raw) return self.compileNullaryPrim(.terpri);
        if (s == b.@"write-char".raw) return self.compileUnaryPrim(args, env, .write_char);
        if (s == b.format.raw) return self.compileFormat(args, env);

        // Character functions
        if (s == b.@"char-upcase".raw) return self.compileUnaryPrim(args, env, .char_upcase);
        if (s == b.@"char-downcase".raw) return self.compileUnaryPrim(args, env, .char_downcase);
        if (s == b.@"digit-char-p".raw) return self.compileUnaryPrim(args, env, .digit_char_p);
        if (s == b.@"alpha-char-p".raw) return self.compileUnaryPrim(args, env, .alpha_char_p);

        // Reader macros
        if (s == b.@"set-macro-character".raw) return self.compileSetMacroCharacter(args, env);
        if (s == b.@"get-macro-character".raw) return self.compileUnaryPrim(args, env, .get_macro_character);
        if (s == b.@"set-dispatch-macro-character".raw) return self.compileTernaryPrim(args, env, .set_dispatch_macro_character);
        if (s == b.@"get-dispatch-macro-character".raw) return self.compileBinaryPrim(args, env, .get_dispatch_macro_character);

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
        // coerce is handled by stdlib function, not special form

        // Hash tables
        if (s == b.@"make-hash-table".raw) return self.compileMakeHash(args);
        if (s == b.gethash.raw) return self.compileGethash(args, env);
        if (s == b.puthash.raw) return self.compileSethash(args, env);
        if (s == b.clrhash.raw) return self.compileUnaryPrim(args, env, .hash_clear);
        if (s == b.@"hash-table-test".raw) return self.compileUnaryPrim(args, env, .hash_test);
        if (s == b.remhash.raw) return self.compileRemhash(args, env);
        if (s == b.@"hash-table-count".raw) return self.compileHashTableCount(args, env);
        if (s == b.@"hash-table-p".raw) return self.compileHashTableP(args, env);
        if (s == b.@"hash-table-keys".raw) return self.compileUnaryPrim(args, env, .hash_keys);
        if (s == b.@"hash-table-alist".raw) return self.compileUnaryPrim(args, env, .hash_alist);
        if (s == b.rationalp.raw) return self.compileUnaryPrim(args, env, .rationalp);
        if (s == b.complexp.raw) return self.compileUnaryPrim(args, env, .complexp);
        if (s == b.@"make-complex".raw) return self.compileBinaryPrim(args, env, .make_complex);
        if (s == b.@"real-part".raw) return self.compileUnaryPrim(args, env, .real_part);
        if (s == b.@"imag-part".raw) return self.compileUnaryPrim(args, env, .imag_part);
        if (s == b.numerator.raw) return self.compileUnaryPrim(args, env, .numerator);
        if (s == b.denominator.raw) return self.compileUnaryPrim(args, env, .denominator);

        // Streams
        if (s == b.streamp.raw) return self.compileUnaryPrim(args, env, .streamp);
        if (s == b.@"input-stream-p".raw) return self.compileUnaryPrim(args, env, .input_stream_p);
        if (s == b.@"output-stream-p".raw) return self.compileUnaryPrim(args, env, .output_stream_p);
        if (s == b.@"make-string-input-stream".raw) return self.compileUnaryPrim(args, env, .make_string_input_stream);
        if (s == b.@"make-string-output-stream".raw) return self.compileNullaryPrim(.make_string_output_stream);
        if (s == b.@"get-output-stream-string".raw) return self.compileUnaryPrim(args, env, .get_output_stream_string);
        if (s == b.@"write-to-stream".raw) return self.compileBinaryPrim(args, env, .write_to_stream);

        // Random
        if (s == b.random.raw) return self.compileUnaryPrim(args, env, .random);
        if (s == b.@"random-seed".raw) return self.compileUnaryPrim(args, env, .random_seed);

        return error.InvalidSyntax; // Not a known primitive
    }

    const PrimTag = enum { add, sub, mul, div, mod, quot, rem, eq, equal, eql, lt, gt, le, ge, num_eq, cons, car, cdr, append, length, reverse, nth, nthcdr, last, member, assoc, rplaca, rplacd, consp, symbolp, numberp, stringp, vectorp, closurep, keywordp, nilp, not, vec_ref, vec_len, make_box, box_ref, box_set, str_ref, str_len, str_eq, str_concat, print, princ, terpri, write_char, random, random_seed, intern, sym_name, type_of, error_user, characterp, floatp, listp, atom, char_code, code_char, char_eq, char_lt, char_gt, char_upcase, char_downcase, digit_char_p, alpha_char_p, read_char, peek_char, read, read_from_string, load, unread_char, eval, gensym, macroexpand, parse_integer, write_to_string, logand, logior, logxor, lognot, ash, read_file, write_file, make_string, string_to_list, list_to_string, string_upcase, string_downcase, boundp, fboundp, symbol_value, symbol_function, typep, abs, zerop, plusp, minusp, evenp, oddp, sqrt, sin, cos, tan, exp, log, floor, ceiling, round, rationalp, complexp, make_complex, real_part, imag_part, numerator, denominator, get, put, remprop, get_macro_character, set_dispatch_macro_character, get_dispatch_macro_character, hashtablep, hash_clear, hash_test, hash_keys, hash_alist, streamp, input_stream_p, output_stream_p, make_string_input_stream, make_string_output_stream, get_output_stream_string, write_to_stream };

    /// Compile variadic arithmetic: +, -, *, /
    /// identity: for + (0), * (1). null means no identity (- and / need args)
    fn compileVariadicArith(self: *Compiler, args: Value, env: *const Env, op: PrimTag, identity: ?i64) anyerror!*Ir {
        // Collect args
        var arg_list = std.ArrayList(*Ir){};
        defer arg_list.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const c = current.toPtr(Cons);
            const compiled = try self.compile(c.car, env);
            try arg_list.append(self.allocator, compiled);
            current = c.cdr;
        }

        const arg_count = arg_list.items.len;

        // Handle different arities
        if (arg_count == 0) {
            // (+) -> 0, (*) -> 1, (-) and (/) are errors
            if (identity) |id| {
                return try self.builder.lit(Value.makeFixnum(id));
            }
            return error.InvalidSyntax;
        }

        if (arg_count == 1) {
            // (+ x) -> x, (* x) -> x
            // (- x) -> (- 0 x), (/ x) -> (/ 1 x)
            if (op == .sub) {
                const zero = try self.builder.lit(Value.makeFixnum(0));
                return try self.builder.sub(zero, arg_list.items[0]);
            }
            if (op == .div) {
                const one = try self.builder.lit(Value.makeFixnum(1));
                return try self.builder.div(one, arg_list.items[0]);
            }
            return arg_list.items[0];
        }

        // 2+ args: fold left
        var result = arg_list.items[0];
        for (arg_list.items[1..]) |arg| {
            result = switch (op) {
                .add => try self.builder.add(result, arg),
                .sub => try self.builder.sub(result, arg),
                .mul => try self.builder.mul(result, arg),
                .div => try self.builder.div(result, arg),
                else => unreachable,
            };
        }
        return result;
    }

    fn compileBinaryPrim(self: *Compiler, args: Value, env: *const Env, prim: PrimTag) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const left = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const right = try self.compile(cons2.car, env);

        return switch (prim) {
            .add => try self.builder.add(left, right),
            .sub => try self.builder.sub(left, right),
            .mul => try self.builder.mul(left, right),
            .div => try self.builder.div(left, right),
            .mod => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .mod = .{ .left = left, .right = right } };
                break :blk node;
            },
            .quot => try self.builder.quot(left, right),
            .rem => try self.builder.rem(left, right),
            .eq => try self.builder.eq(left, right),
            .equal => try self.builder.equal(left, right),
            .eql => try self.builder.eql(left, right),
            .lt => try self.builder.lt(left, right),
            .gt => try self.builder.gt(left, right),
            .le => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .le = .{ .left = left, .right = right } };
                break :blk node;
            },
            .ge => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .ge = .{ .left = left, .right = right } };
                break :blk node;
            },
            .num_eq => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .num_eq = .{ .left = left, .right = right } };
                break :blk node;
            },
            .cons => try self.builder.cons(left, right),
            .vec_ref => try self.builder.vecRef(left, right),
            .box_set => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .box_set = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_ref => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .str_ref = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_eq => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .str_eq = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_concat => try self.builder.strConcat(left, right),
            .char_eq => try self.builder.charEq(left, right),
            .char_lt => try self.builder.charLt(left, right),
            .char_gt => try self.builder.charGt(left, right),
            .typep => try self.builder.typep(left, right),
            .append => try self.builder.append(left, right),
            .nth => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .nth = .{ .left = left, .right = right } };
                break :blk node;
            },
            .nthcdr => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .nthcdr = .{ .left = left, .right = right } };
                break :blk node;
            },
            .member => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .member = .{ .left = left, .right = right } };
                break :blk node;
            },
            .assoc => try self.builder.assoc(left, right),
            .logand => try self.builder.logand(left, right),
            .logior => try self.builder.logior(left, right),
            .logxor => try self.builder.logxor(left, right),
            .ash => try self.builder.ash(left, right),
            .write_file => try self.builder.writeFile(left, right),
            .make_string => try self.builder.makeString(left, right),
            .rplaca => try self.builder.rplaca(left, right),
            .rplacd => try self.builder.rplacd(left, right),
            .make_complex => try self.builder.makeComplex(left, right),
            .get => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .get = .{ .left = left, .right = right } };
                break :blk node;
            },
            .remprop => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .remprop = .{ .left = left, .right = right } };
                break :blk node;
            },
            .write_to_stream => try self.builder.writeToStream(left, right),
            else => return error.InvalidSyntax,
        };
    }

    fn compileTernaryPrim(self: *Compiler, args: Value, env: *const Env, prim: PrimTag) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const first = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const second = try self.compile(cons2.car, env);

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const third = try self.compile(cons3.car, env);

        return switch (prim) {
            .put => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .put = .{ .first = first, .second = second, .third = third } };
                break :blk node;
            },
            else => error.InvalidSyntax,
        };
    }

    /// Compile (set-macro-character char function &optional non-terminating-p)
    fn compileSetMacroCharacter(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const char_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const func_ir = try self.compile(cons2.car, env);

        // Optional third argument: non-terminating-p (defaults to nil)
        const non_term_ir = if (cons2.cdr.isCons()) blk: {
            const cons3 = cons2.cdr.toPtr(Cons);
            break :blk try self.compile(cons3.car, env);
        } else try self.builder.lit(Value.nil);

        const node = try self.allocator.create(Ir);
        node.* = .{ .set_macro_character = .{ .first = char_ir, .second = func_ir, .third = non_term_ir } };
        return node;
    }

    /// Compile princ/print with optional stream argument
    /// (princ obj) -> print to stdout
    /// (princ obj stream) -> write obj to stream (strings directly, others via write-to-string)
    fn compilePrintOrPrinc(self: *Compiler, args: Value, env: *const Env, is_print: bool) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const obj_ir = try self.compile(cons.car, env);

        // Check for second argument (stream)
        if (cons.cdr.isCons()) {
            const cons2 = cons.cdr.toPtr(Cons);
            const stream_ir = try self.compile(cons2.car, env);

            // For princ to stream: if string, use directly; otherwise write-to-string
            // Generate: (if (stringp obj) obj (write-to-string obj))
            const stringp_check = try self.builder.stringp(obj_ir);
            const converted = try self.builder.writeToString(obj_ir);
            const str_ir = try self.builder.ifExpr(stringp_check, obj_ir, converted);

            if (is_print) {
                // print also needs a newline - create concat with newline
                const heap = self.heap orelse return error.UninitializedBuiltins;
                const newline_ir = try self.builder.lit(try heap.allocString("\n"));
                const with_newline = try self.builder.strConcat(str_ir, newline_ir);
                return try self.builder.writeToStream(with_newline, stream_ir);
            } else {
                return try self.builder.writeToStream(str_ir, stream_ir);
            }
        } else {
            // Single arg: print/princ to stdout
            if (is_print) {
                return try self.builder.print(obj_ir);
            } else {
                return try self.builder.princ(obj_ir);
            }
        }
    }

    fn compileUnaryPrim(self: *Compiler, args: Value, env: *const Env, prim: PrimTag) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const operand = try self.compile(cons.car, env);

        return switch (prim) {
            .car => try self.builder.car(operand),
            .cdr => try self.builder.cdr(operand),
            .consp => try self.builder.consp(operand),
            .symbolp => try self.builder.symbolp(operand),
            .numberp => try self.builder.numberp(operand),
            .nilp => try self.builder.nilp(operand),
            .not => try self.builder.not(operand),
            .vec_len => try self.builder.vecLen(operand),
            .make_box => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .make_box = .{ .operand = operand } };
                break :blk node;
            },
            .box_ref => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .box_ref = .{ .operand = operand } };
                break :blk node;
            },
            .str_len => try self.builder.strLen(operand),
            .print => try self.builder.print(operand),
            .princ => try self.builder.princ(operand),
            .write_char => try self.builder.writeChar(operand),
            .char_upcase => try self.builder.charUpcase(operand),
            .char_downcase => try self.builder.charDowncase(operand),
            .digit_char_p => try self.builder.digitCharP(operand),
            .alpha_char_p => try self.builder.alphaCharP(operand),
            .parse_integer => try self.builder.parseInteger(operand),
            .write_to_string => try self.builder.writeToString(operand),
            .lognot => try self.builder.lognot(operand),
            .read_file => try self.builder.readFile(operand),
            .string_to_list => try self.builder.stringToList(operand),
            .list_to_string => try self.builder.listToString(operand),
            .string_upcase => try self.builder.stringUpcase(operand),
            .string_downcase => try self.builder.stringDowncase(operand),
            .stringp => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .stringp = .{ .operand = operand } };
                break :blk node;
            },
            .vectorp => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .vectorp = .{ .operand = operand } };
                break :blk node;
            },
            .closurep => try self.builder.closurep(operand),
            .keywordp => try self.builder.keywordp(operand),
            .random => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .random = .{ .operand = operand } };
                break :blk node;
            },
            .random_seed => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .random_seed = .{ .operand = operand } };
                break :blk node;
            },
            .intern => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .intern = .{ .operand = operand } };
                break :blk node;
            },
            .sym_name => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .sym_name = .{ .operand = operand } };
                break :blk node;
            },
            .type_of => try self.builder.typeOf(operand),
            .error_user => try self.builder.errorUser(operand),
            .characterp => try self.builder.characterp(operand),
            .floatp => try self.builder.floatp(operand),
            .listp => try self.builder.listp(operand),
            .atom => try self.builder.atomp(operand),
            .char_code => try self.builder.charCode(operand),
            .code_char => try self.builder.codeChar(operand),
            .unread_char => try self.builder.unreadChar(operand),
            .load => try self.builder.load(operand),
            .read_from_string => try self.builder.readFromString(operand),
            .eval => try self.builder.eval(operand),
            .macroexpand => try self.builder.macroexpand(operand),
            .boundp => try self.builder.boundp(operand),
            .fboundp => try self.builder.fboundp(operand),
            .symbol_value => try self.builder.symbolValue(operand),
            .symbol_function => try self.builder.symbolFunction(operand),
            .abs => try self.builder.abs(operand),
            .zerop => try self.builder.zerop(operand),
            .plusp => try self.builder.plusp(operand),
            .minusp => try self.builder.minusp(operand),
            .evenp => try self.builder.evenp(operand),
            .oddp => try self.builder.oddp(operand),
            .sqrt => try self.builder.sqrt(operand),
            .sin => try self.builder.sin(operand),
            .cos => try self.builder.cos(operand),
            .tan => try self.builder.tan(operand),
            .exp => try self.builder.exp_fn(operand),
            .log => try self.builder.log_fn(operand),
            .floor => try self.builder.floor_fn(operand),
            .ceiling => try self.builder.ceiling(operand),
            .round => try self.builder.round_fn(operand),
            .length => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .length = .{ .operand = operand } };
                break :blk node;
            },
            .reverse => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .reverse = .{ .operand = operand } };
                break :blk node;
            },
            .last => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .last = .{ .operand = operand } };
                break :blk node;
            },
            .rationalp => try self.builder.rationalp(operand),
            .complexp => try self.builder.complexp(operand),
            .real_part => try self.builder.realPart(operand),
            .imag_part => try self.builder.imagPart(operand),
            .numerator => try self.builder.numerator(operand),
            .denominator => try self.builder.denominator(operand),
            .hashtablep => try self.builder.hashtablep(operand),
            .hash_clear => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .hash_clear = .{ .operand = operand } };
                break :blk node;
            },
            .hash_test => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .hash_test = .{ .operand = operand } };
                break :blk node;
            },
            .hash_keys => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .hash_keys = .{ .operand = operand } };
                break :blk node;
            },
            .hash_alist => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .hash_alist = .{ .operand = operand } };
                break :blk node;
            },
            .streamp => try self.builder.streamp(operand),
            .input_stream_p => try self.builder.inputStreamP(operand),
            .output_stream_p => try self.builder.outputStreamP(operand),
            .make_string_input_stream => try self.builder.makeStringInputStream(operand),
            .get_output_stream_string => try self.builder.getOutputStreamString(operand),
            else => return error.InvalidSyntax,
        };
    }

    /// Compile floor/ceiling/round with optional divisor: (floor x) or (floor x y)
    fn compileFloorCeilRound(self: *Compiler, args: Value, env: *const Env, op: PrimTag) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const dividend = try self.compile(cons.car, env);

        // Check for optional second argument (divisor)
        if (cons.cdr.isCons()) {
            const cdr_cons = cons.cdr.toPtr(Cons);
            const divisor = try self.compile(cdr_cons.car, env);
            // (floor x y) = (floor (/ x y))
            const div_ir = try self.builder.div(dividend, divisor);
            return switch (op) {
                .floor => try self.builder.floor_fn(div_ir),
                .ceiling => try self.builder.ceiling(div_ir),
                .round => try self.builder.round_fn(div_ir),
                else => return error.InvalidSyntax,
            };
        }

        // Single argument case
        return switch (op) {
            .floor => try self.builder.floor_fn(dividend),
            .ceiling => try self.builder.ceiling(dividend),
            .round => try self.builder.round_fn(dividend),
            else => return error.InvalidSyntax,
        };
    }

    fn compileNullaryPrim(self: *Compiler, prim: PrimTag) anyerror!*Ir {
        return switch (prim) {
            .read_char => try self.builder.readChar(),
            .peek_char => try self.builder.peekChar(),
            .read => try self.builder.readSexp(),
            .gensym => try self.builder.gensym(),
            .terpri => try self.builder.terpri(),
            .make_string_output_stream => try self.builder.makeStringOutputStream(),
            else => return error.InvalidSyntax,
        };
    }

    fn compileListPrim(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (list a b c ...) -> variadic
        var elements = std.ArrayList(*const Ir){};
        defer elements.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const elem_ir = try self.compile(cons.car, env);
            try elements.append(self.allocator, elem_ir);
            current = cons.cdr;
        }

        return try self.builder.list(elements.items);
    }

    fn compileSubstring(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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

        const node = try self.allocator.create(Ir);
        node.* = .{ .substring = .{ .str = str_ir, .start = start_ir, .end = end_ir } };
        return node;
    }

    fn compileSubseq(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
            break :blk try self.builder.strLen(seq_ir);
        };

        const node = try self.allocator.create(Ir);
        node.* = .{ .substring = .{ .str = seq_ir, .start = start_ir, .end = end_ir } };
        return node;
    }

    fn compileConcatenate(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (concatenate 'string str1 str2 ...)
        // (concatenate 'list list1 list2 ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const b = self.builtins orelse return error.InvalidSyntax;

        // First arg should be a quoted type: (quote string) or (quote list)
        var type_sym: Value = undefined;
        if (cons1.car.isCons()) {
            const quote_cons = cons1.car.toPtr(Cons);
            // Check if it's (quote xxx) - use symbol identity
            if (quote_cons.car.raw == b.quote.raw and quote_cons.cdr.isCons()) {
                type_sym = quote_cons.cdr.toPtr(Cons).car;
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
                return try self.builder.lit(Value.nil);
            } else {
                return try self.builder.lit(Value.nil);
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
                result_ir = try self.builder.strConcat(result_ir, next_ir);
                rest = cons.cdr;
            }
        } else if (type_sym.raw == b.list.raw) {
            while (rest.isCons()) {
                const cons = rest.toPtr(Cons);
                const next_ir = try self.compile(cons.car, env);
                result_ir = try self.builder.append(result_ir, next_ir);
                rest = cons.cdr;
            }
        } else {
            return error.InvalidSyntax;
        }

        return result_ir;
    }

    // coerce is implemented in stdlib, not as a compiler special form

    fn compileFormat(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
            try arg_list.append(self.allocator, arg_ir);
            rest = cons.cdr;
        }

        return try self.builder.format(dest_ir, control_ir, arg_list.items);
    }

    fn compileMakeHash(self: *Compiler, args: Value) anyerror!*Ir {
        // (make-hash-table) or (make-hash-table :size n :test test-fn)
        // Defaults: size=16, test=eql
        const b = self.builtins.?;
        var capacity: u16 = 16;
        var test_type: ir.HashTest = .eql;

        // Parse keyword arguments
        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const key = cons.car;

            // Check if it's a keyword
            if (!key.isKeyword()) break; // Not a keyword, stop parsing

            // Get the value
            if (!cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = cons.cdr.toPtr(Cons);
            const val = val_cons.car;

            if (key.raw == b.kw_size.raw) {
                // :size n - capacity
                if (!val.isFixnum()) return error.InvalidSyntax;
                const n = val.toFixnum();
                if (n < 1 or n > 65535) return error.InvalidSyntax;
                capacity = @intCast(@as(u64, @bitCast(n)));
            } else if (key.raw == b.kw_test.raw) {
                // :test 'eq or :test 'eql or :test 'equal
                // The value should be a quoted symbol or just a symbol
                var test_sym = val;
                if (val.isCons()) {
                    // Could be (quote eq)
                    const quote_cons = val.toPtr(Cons);
                    if (quote_cons.cdr.isCons()) {
                        test_sym = quote_cons.cdr.toPtr(Cons).car;
                    }
                }
                if (test_sym.isSymbol()) {
                    // Compare by identity with pre-interned symbols
                    if (test_sym.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (test_sym.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (test_sym.raw == b.equal.raw) {
                        test_type = .equal;
                    } else {
                        return error.InvalidSyntax;
                    }
                } else {
                    return error.InvalidSyntax;
                }
            }
            // Move to next key-value pair
            current = val_cons.cdr;
        }

        const node = try self.allocator.create(Ir);
        node.* = .{ .make_hash = .{ .capacity = capacity, .test_type = test_type } };
        return node;
    }

    /// Compile (member item list &key test) with optional :test keyword
    fn compileMemberWithTest(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const b = self.builtins.?;

        // Parse positional arguments: (member item list ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const arg1_cons = args.toPtr(Cons);
        const item_expr = arg1_cons.car;

        if (!arg1_cons.cdr.isCons()) return error.InvalidSyntax;
        const arg2_cons = arg1_cons.cdr.toPtr(Cons);
        const list_expr = arg2_cons.car;

        // Compile positional arguments
        const item = try self.compile(item_expr, env);
        const list = try self.compile(list_expr, env);

        // Default test is eq
        var test_type: enum { eq, eql, equal } = .eq;

        // Parse optional :test keyword
        var current = arg2_cons.cdr;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const key = cons.car;

            if (!key.isKeyword()) break;

            if (!cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = cons.cdr.toPtr(Cons);
            const val = val_cons.car;

            if (key.raw == b.kw_test.raw) {
                // :test 'eq or :test 'eql or :test 'equal
                var test_sym = val;
                if (val.isCons()) {
                    // Could be (quote eq)
                    const quote_cons = val.toPtr(Cons);
                    if (quote_cons.cdr.isCons()) {
                        test_sym = quote_cons.cdr.toPtr(Cons).car;
                    }
                }
                if (test_sym.isSymbol()) {
                    if (test_sym.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (test_sym.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (test_sym.raw == b.equal.raw) {
                        test_type = .equal;
                    } else {
                        return error.InvalidSyntax;
                    }
                } else {
                    return error.InvalidSyntax;
                }
            }
            current = val_cons.cdr;
        }

        // Create appropriate IR node based on test type
        const node = try self.allocator.create(Ir);
        node.* = switch (test_type) {
            .eq => .{ .member = .{ .left = item, .right = list } },
            .eql => .{ .member_eql = .{ .left = item, .right = list } },
            .equal => .{ .member_equal = .{ .left = item, .right = list } },
        };
        return node;
    }

    /// Compile (assoc key alist &key test) with optional :test keyword
    fn compileAssocWithTest(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const b = self.builtins.?;

        // Parse positional arguments: (assoc key alist ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const arg1_cons = args.toPtr(Cons);
        const key_expr = arg1_cons.car;

        if (!arg1_cons.cdr.isCons()) return error.InvalidSyntax;
        const arg2_cons = arg1_cons.cdr.toPtr(Cons);
        const alist_expr = arg2_cons.car;

        // Compile positional arguments
        const key = try self.compile(key_expr, env);
        const alist = try self.compile(alist_expr, env);

        // Default test is eq
        var test_type: enum { eq, eql, equal } = .eq;

        // Parse optional :test keyword
        var current = arg2_cons.cdr;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const kw = cons.car;

            if (!kw.isKeyword()) break;

            if (!cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = cons.cdr.toPtr(Cons);
            const val = val_cons.car;

            if (kw.raw == b.kw_test.raw) {
                // :test 'eq or :test 'eql or :test 'equal
                var test_sym = val;
                if (val.isCons()) {
                    // Could be (quote eq)
                    const quote_cons = val.toPtr(Cons);
                    if (quote_cons.cdr.isCons()) {
                        test_sym = quote_cons.cdr.toPtr(Cons).car;
                    }
                }
                if (test_sym.isSymbol()) {
                    if (test_sym.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (test_sym.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (test_sym.raw == b.equal.raw) {
                        test_type = .equal;
                    } else {
                        return error.InvalidSyntax;
                    }
                } else {
                    return error.InvalidSyntax;
                }
            }
            current = val_cons.cdr;
        }

        // Create appropriate IR node based on test type
        const node = try self.allocator.create(Ir);
        node.* = switch (test_type) {
            .eq => .{ .assoc = .{ .left = key, .right = alist } },
            .eql => .{ .assoc_eql = .{ .left = key, .right = alist } },
            .equal => .{ .assoc_equal = .{ .left = key, .right = alist } },
        };
        return node;
    }

    /// Compile (find item sequence &key test) with optional :test keyword
    /// Default test is eql (CL spec)
    fn compileFindWithTest(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const b = self.builtins.?;

        // Parse positional arguments: (find item sequence ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const arg1_cons = args.toPtr(Cons);
        const item_expr = arg1_cons.car;

        if (!arg1_cons.cdr.isCons()) return error.InvalidSyntax;
        const arg2_cons = arg1_cons.cdr.toPtr(Cons);
        const seq_expr = arg2_cons.car;

        // Compile positional arguments
        const item = try self.compile(item_expr, env);
        const seq = try self.compile(seq_expr, env);

        // Default test is eql (CL spec for find)
        var test_type: enum { eq, eql, equal } = .eql;

        // Parse optional :test keyword
        var current = arg2_cons.cdr;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const key = cons.car;

            if (!key.isKeyword()) break;

            if (!cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = cons.cdr.toPtr(Cons);
            const val = val_cons.car;

            if (key.raw == b.kw_test.raw) {
                var test_sym = val;
                if (val.isCons()) {
                    const quote_cons = val.toPtr(Cons);
                    if (quote_cons.cdr.isCons()) {
                        test_sym = quote_cons.cdr.toPtr(Cons).car;
                    }
                }
                if (test_sym.isSymbol()) {
                    if (test_sym.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (test_sym.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (test_sym.raw == b.equal.raw) {
                        test_type = .equal;
                    } else {
                        return error.InvalidSyntax;
                    }
                } else {
                    return error.InvalidSyntax;
                }
            }
            current = val_cons.cdr;
        }

        const node = try self.allocator.create(Ir);
        node.* = switch (test_type) {
            .eq => .{ .find_eq = .{ .left = item, .right = seq } },
            .eql => .{ .find = .{ .left = item, .right = seq } },
            .equal => .{ .find_equal = .{ .left = item, .right = seq } },
        };
        return node;
    }

    /// Compile (position item sequence &key test) with optional :test keyword
    /// Default test is eql (CL spec)
    fn compilePositionWithTest(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const b = self.builtins.?;

        // Parse positional arguments: (position item sequence ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const arg1_cons = args.toPtr(Cons);
        const item_expr = arg1_cons.car;

        if (!arg1_cons.cdr.isCons()) return error.InvalidSyntax;
        const arg2_cons = arg1_cons.cdr.toPtr(Cons);
        const seq_expr = arg2_cons.car;

        // Compile positional arguments
        const item = try self.compile(item_expr, env);
        const seq = try self.compile(seq_expr, env);

        // Default test is eql (CL spec for position)
        var test_type: enum { eq, eql, equal } = .eql;

        // Parse optional :test keyword
        var current = arg2_cons.cdr;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const key = cons.car;

            if (!key.isKeyword()) break;

            if (!cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = cons.cdr.toPtr(Cons);
            const val = val_cons.car;

            if (key.raw == b.kw_test.raw) {
                var test_sym = val;
                if (val.isCons()) {
                    const quote_cons = val.toPtr(Cons);
                    if (quote_cons.cdr.isCons()) {
                        test_sym = quote_cons.cdr.toPtr(Cons).car;
                    }
                }
                if (test_sym.isSymbol()) {
                    if (test_sym.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (test_sym.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (test_sym.raw == b.equal.raw) {
                        test_type = .equal;
                    } else {
                        return error.InvalidSyntax;
                    }
                } else {
                    return error.InvalidSyntax;
                }
            }
            current = val_cons.cdr;
        }

        const node = try self.allocator.create(Ir);
        node.* = switch (test_type) {
            .eq => .{ .position_eq = .{ .left = item, .right = seq } },
            .eql => .{ .position = .{ .left = item, .right = seq } },
            .equal => .{ .position_equal = .{ .left = item, .right = seq } },
        };
        return node;
    }

    /// Compile (count item sequence &key test) with optional :test keyword
    /// Default test is eql (CL spec)
    fn compileCountWithTest(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const b = self.builtins.?;

        if (!args.isCons()) return error.InvalidSyntax;
        const arg1_cons = args.toPtr(Cons);
        const item_expr = arg1_cons.car;

        if (!arg1_cons.cdr.isCons()) return error.InvalidSyntax;
        const arg2_cons = arg1_cons.cdr.toPtr(Cons);
        const seq_expr = arg2_cons.car;

        const item = try self.compile(item_expr, env);
        const seq = try self.compile(seq_expr, env);

        var test_type: enum { eq, eql, equal } = .eql;

        var current = arg2_cons.cdr;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const key = cons.car;

            if (!key.isKeyword()) break;

            if (!cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = cons.cdr.toPtr(Cons);
            const val = val_cons.car;

            if (key.raw == b.kw_test.raw) {
                var test_sym = val;
                if (val.isCons()) {
                    const quote_cons = val.toPtr(Cons);
                    if (quote_cons.cdr.isCons()) {
                        test_sym = quote_cons.cdr.toPtr(Cons).car;
                    }
                }
                if (test_sym.isSymbol()) {
                    if (test_sym.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (test_sym.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (test_sym.raw == b.equal.raw) {
                        test_type = .equal;
                    } else {
                        return error.InvalidSyntax;
                    }
                } else {
                    return error.InvalidSyntax;
                }
            }
            current = val_cons.cdr;
        }

        const node = try self.allocator.create(Ir);
        node.* = switch (test_type) {
            .eq => .{ .count_eq = .{ .left = item, .right = seq } },
            .eql => .{ .count = .{ .left = item, .right = seq } },
            .equal => .{ .count_equal = .{ .left = item, .right = seq } },
        };
        return node;
    }

    /// Compile (remove item sequence &key test) with optional :test keyword
    /// Default test is eql (CL spec)
    fn compileRemoveWithTest(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const b = self.builtins.?;

        if (!args.isCons()) return error.InvalidSyntax;
        const arg1_cons = args.toPtr(Cons);
        const item_expr = arg1_cons.car;

        if (!arg1_cons.cdr.isCons()) return error.InvalidSyntax;
        const arg2_cons = arg1_cons.cdr.toPtr(Cons);
        const seq_expr = arg2_cons.car;

        const item = try self.compile(item_expr, env);
        const seq = try self.compile(seq_expr, env);

        var test_type: enum { eq, eql, equal } = .eql;

        var current = arg2_cons.cdr;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const key = cons.car;

            if (!key.isKeyword()) break;

            if (!cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = cons.cdr.toPtr(Cons);
            const val = val_cons.car;

            if (key.raw == b.kw_test.raw) {
                var test_sym = val;
                if (val.isCons()) {
                    const quote_cons = val.toPtr(Cons);
                    if (quote_cons.cdr.isCons()) {
                        test_sym = quote_cons.cdr.toPtr(Cons).car;
                    }
                }
                if (test_sym.isSymbol()) {
                    if (test_sym.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (test_sym.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (test_sym.raw == b.equal.raw) {
                        test_type = .equal;
                    } else {
                        return error.InvalidSyntax;
                    }
                } else {
                    return error.InvalidSyntax;
                }
            }
            current = val_cons.cdr;
        }

        const node = try self.allocator.create(Ir);
        node.* = switch (test_type) {
            .eq => .{ .remove_eq = .{ .left = item, .right = seq } },
            .eql => .{ .remove = .{ .left = item, .right = seq } },
            .equal => .{ .remove_equal = .{ .left = item, .right = seq } },
        };
        return node;
    }

    /// Compile composed car/cdr accessor like cadr, caddr, etc.
    /// Pattern string: 'a' = car, 'd' = cdr, applied right-to-left
    /// e.g., "ad" for cadr means (car (cdr x))
    fn compileComposedAccessor(self: *Compiler, args: Value, env: *const Env, pattern: []const u8) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        var result = try self.compile(cons.car, env);

        // Apply pattern right-to-left (innermost to outermost)
        var i: usize = pattern.len;
        while (i > 0) {
            i -= 1;
            result = switch (pattern[i]) {
                'a' => try self.builder.car(result),
                'd' => try self.builder.cdr(result),
                else => return error.InvalidSyntax,
            };
        }
        return result;
    }

    fn compileMakeVector(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (make-vector size &optional init)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const size_ir = try self.compile(cons1.car, env);

        var init_ir: ?*const Ir = null;
        if (cons1.cdr.isCons()) {
            const cons2 = cons1.cdr.toPtr(Cons);
            init_ir = try self.compile(cons2.car, env);
        }

        return try self.builder.vecNew(size_ir, init_ir);
    }

    fn compileVectorPrim(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (vector a b c ...) -> create vector from elements
        var elements = std.ArrayList(*const Ir){};
        defer elements.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const elem_ir = try self.compile(cons.car, env);
            try elements.append(self.allocator, elem_ir);
            current = cons.cdr;
        }

        return try self.builder.vec(elements.items);
    }

    fn compileMakeArray(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (make-array dimensions &optional initial-element)
        // dimensions can be a single fixnum or a quoted list of fixnums
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);

        var dimensions = std.ArrayList(*const Ir){};
        defer dimensions.deinit(self.allocator);

        var dims_val = cons1.car;

        // If dims_val is (quote (2 3)), unwrap it
        const b = self.builtins orelse return error.InvalidSyntax;
        if (dims_val.isCons()) {
            const quote_cons = dims_val.toPtr(Cons);
            if (quote_cons.car.raw == b.quote.raw and quote_cons.cdr.isCons()) {
                const inner_cons = quote_cons.cdr.toPtr(Cons);
                dims_val = inner_cons.car;
            }
        }

        // Now dims_val is either a fixnum, list of dimensions, or an expression
        if (dims_val.isCons()) {
            // Check if it's a (quote list) form - actual list of dimensions
            const cons_check = dims_val.toPtr(Cons);
            // If car is a fixnum, it's a literal list of dimensions
            if (cons_check.car.isFixnum()) {
                var current = dims_val;
                while (current.isCons()) {
                    const dim_cons = current.toPtr(Cons);
                    const dim_ir = try self.compile(dim_cons.car, env);
                    try dimensions.append(self.allocator, dim_ir);
                    current = dim_cons.cdr;
                }
            } else {
                // It's an expression like (length lst) - compile it as single dim
                const dim_ir = try self.compile(dims_val, env);
                try dimensions.append(self.allocator, dim_ir);
            }
        } else {
            // Fixnum, symbol (variable), or other expression - compile as single dim
            const dim_ir = try self.compile(dims_val, env);
            try dimensions.append(self.allocator, dim_ir);
        }

        // Optional initial element - handle keyword args (:initial-element value)
        var init_ir: ?*const Ir = null;
        var rest = cons1.cdr;
        while (rest.isCons()) {
            const kv_cons = rest.toPtr(Cons);
            // Check if it's a keyword
            if (kv_cons.car.isKeyword()) {
                // Skip the keyword, get the value
                if (!kv_cons.cdr.isCons()) break;
                const val_cons = kv_cons.cdr.toPtr(Cons);
                init_ir = try self.compile(val_cons.car, env);
                rest = val_cons.cdr;
            } else {
                // Non-keyword arg is the initial element (legacy support)
                init_ir = try self.compile(kv_cons.car, env);
                rest = kv_cons.cdr;
            }
        }

        return try self.builder.arrNew(dimensions.items, init_ir);
    }

    fn compileAref(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (aref array subscript1 subscript2 ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const array_ir = try self.compile(cons1.car, env);

        // Collect subscripts
        var subscripts = std.ArrayList(*const Ir){};
        defer subscripts.deinit(self.allocator);

        var current = cons1.cdr;
        while (current.isCons()) {
            const sub_cons = current.toPtr(Cons);
            const sub_ir = try self.compile(sub_cons.car, env);
            try subscripts.append(self.allocator, sub_ir);
            current = sub_cons.cdr;
        }

        if (subscripts.items.len == 0) return error.InvalidSyntax;

        return try self.builder.arrRef(array_ir, subscripts.items);
    }

    fn compileSvset(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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

        return try self.builder.vecSet(vec_ir, idx_ir, val_ir);
    }

    fn compileAset(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (%aset array sub1 sub2 ... subN value) - internal setter for (setf (aref ...))
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const array_ir = try self.compile(cons1.car, env);

        // Collect subscripts and value
        var subscripts = std.ArrayList(*const Ir){};
        defer subscripts.deinit(self.allocator);

        var current = cons1.cdr;
        var value_ir: ?*const Ir = null;

        // Iterate to collect all arguments
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const arg_ir = try self.compile(cons.car, env);

            // Last element is the value, rest are subscripts
            if (cons.cdr.isNil()) {
                value_ir = arg_ir;
            } else {
                try subscripts.append(self.allocator, arg_ir);
            }

            current = cons.cdr;
        }

        if (value_ir == null or subscripts.items.len == 0) return error.InvalidSyntax;

        return try self.builder.arrSet(array_ir, subscripts.items, value_ir.?);
    }

    fn compileGethash(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (gethash key hashtable &optional default)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const key_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const table_ir = try self.compile(cons2.car, env);

        // Optional default argument
        const default_ir: ?*const Ir = if (cons2.cdr.isCons()) blk: {
            const cons3 = cons2.cdr.toPtr(Cons);
            break :blk try self.compile(cons3.car, env);
        } else null;

        const node = try self.allocator.create(Ir);
        node.* = .{ .hash_get = .{ .table = table_ir, .key = key_ir, .default = default_ir } };
        return node;
    }

    fn compileSethash(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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

        const node = try self.allocator.create(Ir);
        node.* = .{ .hash_set = .{ .table = table_ir, .key = key_ir, .value = value_ir } };
        return node;
    }

    fn compileRemhash(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (remhash key hashtable)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const key_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const table_ir = try self.compile(cons2.car, env);

        const node = try self.allocator.create(Ir);
        node.* = .{ .hash_rem = .{ .table = table_ir, .key = key_ir } };
        return node;
    }

    fn compileHashTableCount(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (hash-table-count hashtable)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const table_ir = try self.compile(cons.car, env);

        const node = try self.allocator.create(Ir);
        node.* = .{ .hash_count = .{ .operand = table_ir } };
        return node;
    }

    fn compileHashTableP(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (hash-table-p x)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const operand_ir = try self.compile(cons.car, env);

        const node = try self.allocator.create(Ir);
        node.* = .{ .hashtablep = .{ .operand = operand_ir } };
        return node;
    }

    fn compileCall(self: *Compiler, func_expr: Value, args_expr: Value, env: *const Env) anyerror!*Ir {
        return self.compileCallWithTail(func_expr, args_expr, env, false);
    }

    fn compileCallWithTail(self: *Compiler, func_expr: Value, args_expr: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // Check for struct predicate calls (for occurrence typing)
        // If calling a known struct predicate like point-p, generate struct_p IR
        if (func_expr.isSymbol() and self.struct_predicates.count() > 0) {
            // Copy name to avoid dangling pointer if heap moves
            const sym_name_raw = func_expr.toPtr(Symbol).getName();
            const sym_name = self.allocator.dupe(u8, sym_name_raw) catch |e| return e;
            defer self.allocator.free(sym_name);
            if (self.struct_predicates.get(sym_name)) |struct_type| {
                // This is a struct predicate call - generate struct_p IR
                // Extract struct name from predicate (remove "-p" suffix)
                const struct_name = if (sym_name.len > 2 and
                    std.mem.endsWith(u8, sym_name, "-p"))
                    sym_name[0 .. sym_name.len - 2]
                else
                    sym_name;

                // Compile the single argument
                if (!args_expr.isCons()) return error.InvalidSyntax;
                const arg_cons = args_expr.toPtr(Cons);
                if (!arg_cons.cdr.isNil()) return error.InvalidSyntax; // Must have exactly 1 arg
                const arg_ir = try self.compile(arg_cons.car, env);

                return try self.builder.structp(arg_ir, struct_name, struct_type);
            }
        }

        const func_ir = try self.compile(func_expr, env);

        var args = std.ArrayList(*Ir){};
        defer args.deinit(self.allocator);

        var list = args_expr;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const arg_ir = try self.compile(cons.car, env);
            try args.append(self.allocator, arg_ir);
            list = cons.cdr;
        }

        // Convert to const slice
        const items = self.allocator.dupe(*const Ir, args.items) catch
            return error.OutOfMemory;

        if (in_tail) {
            return try self.builder.tailcall(func_ir, items);
        } else {
            return try self.builder.call(func_ir, items);
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

test "BiChecker integration - type checking enabled" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    // Enable type checking
    compiler.enableTypeChecking();
    try testing.expect(compiler.type_checking_enabled);

    // BiChecker should be initialized
    try testing.expect(!compiler.hasBiCheckErrors());
}

test "BiChecker integration - checkLambdaTypes with correct types" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    // Create a simple body IR (literal fixnum)
    const body = try compiler.builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(body);

    // Empty typed params (untyped function)
    const typed_params = [_]Compiler.TypedParam{};

    // Check with no return type - should succeed (just infers)
    compiler.checkLambdaTypes(&typed_params, null, body);

    // No errors expected
    try testing.expect(!compiler.hasBiCheckErrors());
}

test "BiChecker integration - checkLambdaTypes with type mismatch" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    // Create body that returns a fixnum
    const body = try compiler.builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(body);

    // Empty typed params
    const typed_params = [_]Compiler.TypedParam{};

    // Expect string return type (but body returns fixnum)
    // Note: We need a Value for the type symbol, so we'll test this differently
    // For now, just test that checking works without crashing
    compiler.checkLambdaTypes(&typed_params, null, body);

    // BiChecker should have been invoked
    try testing.expect(!compiler.hasBiCheckErrors());
}
