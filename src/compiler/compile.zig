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
const primitives = runtime.primitives;
const qual_name = @import("../runtime/qual_name.zig");
const types = @import("../types/types.zig");
const Type = types.Type;
const TypeEnv = types.TypeEnv;
const OccurrenceCtx = types.OccurrenceCtx;
const TypeChecker = types.TypeChecker;
const BiChecker = types.BiChecker;
const TypingCtx = types.TypingCtx;
const vm_mod = @import("../interp/vm.zig");
const Vm = vm_mod.Vm;
const reader = @import("../reader/reader.zig");
const Parser = reader.Parser;
const bytecode = @import("../bytecode/bytecode.zig");
const Emitter = bytecode.Emitter;
const Chunk = bytecode.Chunk;
const prims = @import("../runtime/primitives/symbol.zig");
const io = @import("../runtime/primitives/io.zig");

pub const Error = error{
    InvalidSyntax,
    UnboundVariable,
    InvalidLambda,
    InvalidLet,
    InvalidIf,
    InvalidSet,
    UndefinedClass,
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
    @"LET*": Value,
    letrec: Value,
    flet: Value,
    labels: Value,
    lambda: Value,
    @"fn": Value,
    define: Value,
    defvar: Value,
    defun: Value,
    setq: Value,
    setf: Value,
    push: Value,
    incf: Value,
    decf: Value,

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
    @"macroexpand-1": Value,

    // Declarations
    declare: Value,
    declaim: Value,
    proclaim: Value,

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
    @"handler-bind": Value,
    @"restart-case": Value,
    @"invoke-restart": Value,
    @"find-restart": Value,
    tagbody: Value,
    go: Value,
    progv: Value,
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
    @"class-of": Value,
    defgeneric: Value,
    defmethod: Value,
    @"call-next-method": Value,
    @"next-method-p": Value,
    @"define-method-combination": Value,
    @"method-qualifiers": Value,
    @"method-specializers": Value,
    @"method-function": Value,
    @"generic-function-methods": Value,
    @"generic-function-lambda-list": Value,
    @"generic-function-name": Value,
    @"slot-boundp": Value,
    @"slot-makunbound": Value,
    @"class-name": Value,
    @"find-class": Value,
    @"class-direct-superclasses": Value,
    @"class-precedence-list": Value,
    @"class-direct-slots": Value,
    @"class-slots": Value,
    @"slot-definition-name": Value,
    @"slot-definition-initform": Value,
    @"slot-definition-initargs": Value,
    @"slot-definition-readers": Value,
    @"slot-definition-writers": Value,
    @"slot-definition-allocation": Value,
    @"slot-definition-type": Value,
    // Method qualifier keywords
    kw_before: Value,
    kw_after: Value,
    kw_around: Value,

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
    equalp: Value,
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
    integerp: Value,
    realp: Value,
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
    @"%read-char": Value,
    @"%peek-char": Value,
    @"unread-char": Value,
    listen: Value,
    @"upgraded-complex-part-type": Value,
    read: Value,
    @"%read": Value,
    @"read-from-string": Value,
    load: Value,
    eval: Value,
    gensym: Value,

    // Primitives - Symbol operations
    boundp: Value,
    fboundp: Value,
    @"symbol-value": Value,
    @"symbol-function": Value,
    @"symbol-plist": Value,
    @"%set-symbol-value": Value,
    @"%set-symbol-plist": Value,
    @"function-lambda-expression": Value,
    fdefinition: Value,
    typep: Value,
    subtypep: Value,
    @"type-of": Value,
    intern: Value,
    @"%make-symbol": Value,
    @"%error": Value,
    @"%floor": Value,
    @"%ceiling": Value,
    @"%round": Value,
    @"%truncate": Value,
    unintern: Value,
    @"find-symbol": Value,
    @"symbol-name": Value,
    @"copy-symbol": Value,
    @"copy-structure": Value,
    makunbound: Value,
    set: Value,
    get: Value,
    put: Value,
    remprop: Value,
    @"error": Value,
    condition: Value,
    @"serious-condition": Value,
    @"simple-error": Value,
    @"simple-condition": Value,
    @"type-error": Value,
    @"program-error": Value,
    @"control-error": Value,
    @"arithmetic-error": Value,
    @"division-by-zero": Value,
    @"cell-error": Value,
    @"unbound-variable": Value,
    @"undefined-function": Value,
    @"package-error": Value,
    @"stream-error": Value,
    @"file-error": Value,
    @"parse-error": Value,
    @"end-of-file": Value,
    warning: Value,
    @"simple-warning": Value,
    // Type specifier symbols for concatenate/coerce
    string: Value,
    character: Value,
    t: Value,

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
    asin: Value,
    acos: Value,
    atan: Value,
    sinh: Value,
    cosh: Value,
    tanh: Value,
    asinh: Value,
    acosh: Value,
    atanh: Value,
    exp: Value,
    log: Value,
    floor: Value,
    ceiling: Value,
    round: Value,
    @"decode-float": Value,
    @"integer-decode-float": Value,
    @"float-radix": Value,
    @"float-digits": Value,

    // Primitives - Vector operations (CL names)
    aref: Value, // CL: array element access
    svref: Value, // CL: simple-vector element access
    bit: Value, // CL: bit-array element access
    sbit: Value, // CL: simple-bit-vector element access
    elt: Value, // CL: generic sequence element access
    @"%svset": Value, // internal: (setf (svref ...)) expands to this
    @"%aset": Value, // internal: (setf (aref ...)) expands to this
    @"%set-slot-value": Value, // internal: (setf (slot-value ...)) expands to this
    @"%sset": Value, // internal: (setf (char ...)) expands to this
    @"%make-unbound": Value, // internal: returns unbound marker
    @"%class-of": Value, // internal: class-of primitive
    @"%find-class": Value, // internal: find-class primitive
    @"%class-name": Value, // internal: class-name primitive

    // Stream I/O primitives
    @"%open": Value,
    @"%close": Value,
    close: Value,
    @"%read-line": Value,
    @"%write-line": Value,
    @"%write-string": Value,
    @"%read-byte": Value,
    @"%write-byte": Value,
    @"%file-position": Value,
    @"%file-length": Value,
    @"%finish-output": Value,
    @"%force-output": Value,
    @"%clear-input": Value,
    @"%clear-output": Value,
    @"%sleep": Value,

    @"vector-length": Value,
    @"make-vector": Value,
    vector: Value,
    @"make-array": Value, // CL: multi-dimensional array creation
    @"%fill-pointer": Value,
    @"%set-fill-pointer": Value,
    @"%set-adjustable": Value,
    @"%vector-push": Value,
    @"%vector-push-extend": Value,
    @"%vector-pop": Value,
    @"%adjust-array": Value,

    // Primitives - String operations (CL names)
    char: Value, // CL: character at index
    schar: Value, // CL: simple-string character access
    @"string-length": Value,
    @"string-concat": Value,
    @"string=": Value,
    @"string<": Value,
    @"string>": Value,
    @"string<=": Value,
    @"string>=": Value,
    substring: Value,
    subseq: Value,

    // Primitives - I/O and misc
    write: Value,
    prin1: Value,
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
    lognand: Value,
    lognor: Value,
    logandc1: Value,
    logandc2: Value,
    logorc1: Value,
    logorc2: Value,
    logeqv: Value,
    logbitp: Value,
    logcount: Value,
    @"integer-length": Value,

    // Primitives - File I/O
    @"read-file": Value,
    @"write-file": Value,
    @"delete-file": Value,
    @"rename-file": Value,
    @"probe-file": Value,
    @"file-write-date": Value,
    @"file-author": Value,
    @"file-string-length": Value,
    @"get-universal-time": Value,
    @"get-internal-real-time": Value,
    @"get-internal-run-time": Value,
    @"get-decoded-time": Value,
    @"decode-universal-time": Value,
    @"encode-universal-time": Value,
    room: Value,
    @"lisp-implementation-type": Value,
    @"lisp-implementation-version": Value,
    @"software-type": Value,
    @"machine-type": Value,
    @"machine-instance": Value,
    @"machine-version": Value,
    @"software-version": Value,
    @"short-site-name": Value,
    @"long-site-name": Value,
    @"user-homedir-pathname": Value,
    @"%make-pathname": Value,
    @"%make-array-contents": Value,

    // Primitives - String construction
    @"make-string": Value,
    @"string-to-list": Value,
    @"list-to-string": Value,
    @"string-upcase": Value,
    @"%string-upcase": Value,
    @"string-downcase": Value,
    @"%string-downcase": Value,
    concatenate: Value,
    // coerce removed - implemented in stdlib

    // Primitives - Hash tables
    @"make-hash-table": Value,
    gethash: Value,
    puthash: Value,
    remhash: Value,
    @"hash-table-count": Value,
    @"hash-table-capacity": Value,
    clrhash: Value,
    @"hash-table-test": Value,
    @"hash-table-p": Value,
    @"hash-table-keys": Value,
    @"hash-table-alist": Value,
    sxhash: Value,
    rationalp: Value,
    complexp: Value,
    @"make-complex": Value,
    @"real-part": Value,
    @"imag-part": Value,
    numerator: Value,
    denominator: Value,
    rational: Value,
    rationalize: Value,
    // Streams
    streamp: Value,
    @"input-stream-p": Value,
    @"output-stream-p": Value,
    @"open-stream-p": Value,
    @"interactive-stream-p": Value,
    @"stream-element-type": Value,
    @"stream-external-format": Value,
    @"make-string-input-stream": Value,
    @"make-string-output-stream": Value,
    @"get-output-stream-string": Value,
    @"write-to-stream": Value,
    // Compound streams
    @"broadcast-stream-streams": Value,
    @"concatenated-stream-streams": Value,
    @"echo-stream-input-stream": Value,
    @"echo-stream-output-stream": Value,
    @"synonym-stream-symbol": Value,
    @"two-way-stream-input-stream": Value,
    @"two-way-stream-output-stream": Value,
    @"%make-synonym-stream": Value,
    @"%make-echo-stream": Value,
    @"%make-two-way-stream": Value,
    @"%make-broadcast-stream": Value,
    @"%make-concatenated-stream": Value,
    @"%make-broadcast-stream-list": Value,
    @"%make-concatenated-stream-list": Value,
    @"%disassemble": Value,
    @"%read-char-from-stream": Value,
    @"%peek-char-from-stream": Value,
    @"%open-file": Value,
    @"%close-stream": Value,

    // Pathname primitives
    @"%pathname-host": Value,
    @"%pathname-device": Value,
    @"%pathname-directory": Value,
    @"%pathname-name": Value,
    @"%pathname-type": Value,
    @"%pathname-version": Value,
    @"pathname-host": Value,
    @"pathname-device": Value,
    @"pathname-directory": Value,
    @"pathname-name": Value,
    @"pathname-type": Value,
    @"pathname-version": Value,
    truename: Value,
    @"ensure-directories-exist": Value,
    pathname: Value,
    @"parse-namestring": Value,
    namestring: Value,
    @"merge-pathnames": Value,
    @"directory-namestring": Value,
    @"file-namestring": Value,
    @"host-namestring": Value,
    @"wild-pathname-p": Value,
    @"package-symbols-table": Value,
    @"package-exports-table": Value,
    @"%package-symbols-list": Value,
    @"%package-exports-list": Value,
    @"package-name": Value,
    @"package-nicknames": Value,
    @"package-use-list": Value,
    @"package-used-by-list": Value,
    @"package-shadowing-symbols": Value,
    packagep: Value,
    @"symbol-package": Value,
    @"list-all-packages": Value,
    @"find-package": Value,
    @"delete-package": Value,
    @"%import": Value,
    @"%use-package": Value,
    @"%unexport": Value,
    @"%shadow": Value,
    @"%shadowing-import": Value,
    @"%unuse-package": Value,
    @"%unintern": Value,
    @"%find-symbol": Value,
    @"find-all-symbols": Value,
    @"apropos-list": Value,
    @"read-char-no-hang": Value,
    @"compute-restarts": Value,
    @"restart-name": Value,
    directory: Value,
    @"pathname-match-p": Value,
    @"enough-namestring": Value,
    @"%make-package": Value,
    @"%rename-package": Value,

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
    ty_and: Value, // (and T1 T2 ...) - intersection type
    ty_not: Value, // (not T) - negation type
    ty_member: Value, // (member obj1 obj2 ...) - member type
    ty_eql: Value, // (eql obj) - eql type

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
    @"&allow-other-keys": Value,
    @"&aux": Value,
    @"&whole": Value,
    @"&environment": Value,

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
    kw_key: Value,
    kw_count: Value,
    kw_eq: Value,
    kw_eql: Value,
    kw_equal: Value,
    @"kw_initial-element": Value,
    @"kw_element-type": Value,
    @"kw_allow-other-keys": Value,
    kw_colon: Value,
    kw_type: Value,
    kw_initform: Value,
    kw_allocation: Value,
    kw_instance: Value,
    kw_class: Value,
    kw_initarg: Value,
    kw_reader: Value,
    kw_writer: Value,
    kw_accessor: Value,

    // *features* keywords
    kw_habu: Value,
    kw_zig: Value,
    kw_unix: Value,
    kw_darwin: Value,
    kw_windows: Value,
    kw_absolute: Value,
    kw_relative: Value,

    /// Initialize all builtin symbols from heap
    pub fn init(heap: *Heap) !Builtins {
        return .{
            .@"if" = try heap.intern("if"),
            .cond = try heap.intern("cond"),
            .@"and" = try heap.intern("and"),
            .@"or" = try heap.intern("or"),
            .let = try heap.intern("let"),
            .@"LET*" = try heap.intern("LET*"),
            .letrec = try heap.intern("letrec"),
            .lambda = try heap.intern("lambda"),
            .@"fn" = try heap.intern("fn"),
            .define = try heap.intern("define"),
            .defvar = try heap.intern("defvar"),
            .defun = try heap.intern("defun"),
            .setq = try heap.intern("setq"),
            .setf = try heap.intern("setf"),
            .push = try heap.intern("push"),
            .incf = try heap.intern("incf"),
            .decf = try heap.intern("decf"),
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
            .@"macroexpand-1" = try heap.intern("macroexpand-1"),
            .declare = try heap.intern("declare"),
            .declaim = try heap.intern("declaim"),
            .proclaim = try heap.intern("proclaim"),
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
            .@"handler-bind" = try heap.intern("handler-bind"),
            .@"restart-case" = try heap.intern("restart-case"),
            .@"invoke-restart" = try heap.intern("invoke-restart"),
            .@"find-restart" = try heap.intern("find-restart"),
            .tagbody = try heap.intern("tagbody"),
            .go = try heap.intern("go"),
            .progv = try heap.intern("progv"),
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
            .@"class-of" = try heap.intern("class-of"),
            .defgeneric = try heap.intern("defgeneric"),
            .defmethod = try heap.intern("defmethod"),
            .@"call-next-method" = try heap.intern("call-next-method"),
            .@"next-method-p" = try heap.intern("next-method-p"),
            .@"define-method-combination" = try heap.intern("define-method-combination"),
            .@"method-qualifiers" = try heap.intern("method-qualifiers"),
            .@"method-specializers" = try heap.intern("method-specializers"),
            .@"method-function" = try heap.intern("method-function"),
            .@"generic-function-methods" = try heap.intern("generic-function-methods"),
            .@"generic-function-lambda-list" = try heap.intern("generic-function-lambda-list"),
            .@"generic-function-name" = try heap.intern("generic-function-name"),
            .@"slot-boundp" = try heap.intern("slot-boundp"),
            .@"slot-makunbound" = try heap.intern("slot-makunbound"),
            .@"class-name" = try heap.intern("class-name"),
            .@"find-class" = try heap.intern("find-class"),
            .@"class-direct-superclasses" = try heap.intern("class-direct-superclasses"),
            .@"class-precedence-list" = try heap.intern("class-precedence-list"),
            .@"class-direct-slots" = try heap.intern("class-direct-slots"),
            .@"class-slots" = try heap.intern("class-slots"),
            .@"slot-definition-name" = try heap.intern("slot-definition-name"),
            .@"slot-definition-initform" = try heap.intern("slot-definition-initform"),
            .@"slot-definition-initargs" = try heap.intern("slot-definition-initargs"),
            .@"slot-definition-readers" = try heap.intern("slot-definition-readers"),
            .@"slot-definition-writers" = try heap.intern("slot-definition-writers"),
            .@"slot-definition-allocation" = try heap.intern("slot-definition-allocation"),
            .@"slot-definition-type" = try heap.intern("slot-definition-type"),
            .kw_before = try heap.internKeyword("before"),
            .kw_after = try heap.internKeyword("after"),
            .kw_around = try heap.internKeyword("around"),
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
            .equalp = try heap.intern("equalp"),
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
            .integerp = try heap.intern("integerp"),
            .realp = try heap.intern("realp"),
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
            .@"%read-char" = try heap.intern("%read-char"),
            .@"%peek-char" = try heap.intern("%peek-char"),
            .@"unread-char" = try heap.intern("unread-char"),
            .listen = try heap.intern("listen"),
            .@"upgraded-complex-part-type" = try heap.intern("upgraded-complex-part-type"),
            .read = try heap.intern("read"),
            .@"%read" = try heap.intern("%read"),
            .@"read-from-string" = try heap.intern("read-from-string"),
            .load = try heap.intern("load"),
            .eval = try heap.intern("eval"),
            .gensym = try heap.intern("gensym"),
            // Primitives - Symbol operations
            .boundp = try heap.intern("boundp"),
            .fboundp = try heap.intern("fboundp"),
            .@"symbol-value" = try heap.intern("symbol-value"),
            .@"symbol-function" = try heap.intern("symbol-function"),
            .@"symbol-plist" = try heap.intern("symbol-plist"),
            .@"%set-symbol-value" = try heap.intern("%set-symbol-value"),
            .@"%set-symbol-plist" = try heap.intern("%set-symbol-plist"),
            .@"function-lambda-expression" = try heap.intern("function-lambda-expression"),
            .fdefinition = try heap.intern("fdefinition"),
            .typep = try heap.intern("typep"),
            .subtypep = try heap.intern("subtypep"),
            .@"type-of" = try heap.intern("type-of"),
            .intern = try heap.intern("intern"),
            .@"%make-symbol" = try heap.intern("%make-symbol"),
            .@"%error" = try heap.intern("%error"),
            .@"%floor" = try heap.intern("%floor"),
            .@"%ceiling" = try heap.intern("%ceiling"),
            .@"%round" = try heap.intern("%round"),
            .@"%truncate" = try heap.intern("%truncate"),
            .unintern = try heap.intern("unintern"),
            .@"find-symbol" = try heap.intern("find-symbol"),
            .@"symbol-name" = try heap.intern("symbol-name"),
            .@"copy-symbol" = try heap.intern("copy-symbol"),
            .@"copy-structure" = try heap.intern("copy-structure"),
            .makunbound = try heap.intern("makunbound"),
            .set = try heap.intern("set"),
            .get = try heap.intern("get"),
            .put = try heap.intern("put"),
            .remprop = try heap.intern("remprop"),
            .@"error" = try heap.intern("error"),
            .condition = try heap.intern("condition"),
            .@"serious-condition" = try heap.intern("serious-condition"),
            .@"simple-error" = try heap.intern("simple-error"),
            .@"simple-condition" = try heap.intern("simple-condition"),
            .@"type-error" = try heap.intern("type-error"),
            .@"program-error" = try heap.intern("program-error"),
            .@"control-error" = try heap.intern("control-error"),
            .@"arithmetic-error" = try heap.intern("arithmetic-error"),
            .@"division-by-zero" = try heap.intern("division-by-zero"),
            .@"cell-error" = try heap.intern("cell-error"),
            .@"unbound-variable" = try heap.intern("unbound-variable"),
            .@"undefined-function" = try heap.intern("undefined-function"),
            .@"package-error" = try heap.intern("package-error"),
            .@"stream-error" = try heap.intern("stream-error"),
            .@"file-error" = try heap.intern("file-error"),
            .@"parse-error" = try heap.intern("parse-error"),
            .@"end-of-file" = try heap.intern("end-of-file"),
            .warning = try heap.intern("warning"),
            .@"simple-warning" = try heap.intern("simple-warning"),
            // Type specifier symbols for concatenate/coerce
            .string = try heap.intern("string"),
            .character = try heap.intern("character"),
            .t = Value.t,
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
            .asin = try heap.intern("asin"),
            .acos = try heap.intern("acos"),
            .atan = try heap.intern("atan"),
            .sinh = try heap.intern("sinh"),
            .cosh = try heap.intern("cosh"),
            .tanh = try heap.intern("tanh"),
            .asinh = try heap.intern("asinh"),
            .acosh = try heap.intern("acosh"),
            .atanh = try heap.intern("atanh"),
            .exp = try heap.intern("exp"),
            .log = try heap.intern("log"),
            .floor = try heap.intern("floor"),
            .ceiling = try heap.intern("ceiling"),
            .round = try heap.intern("round"),
            .@"decode-float" = try heap.intern("decode-float"),
            .@"integer-decode-float" = try heap.intern("integer-decode-float"),
            .@"float-radix" = try heap.intern("float-radix"),
            .@"float-digits" = try heap.intern("float-digits"),
            // Primitives - Vector operations (CL names)
            .aref = try heap.intern("aref"),
            .svref = try heap.intern("svref"),
            .bit = try heap.intern("bit"),
            .sbit = try heap.intern("sbit"),
            .elt = try heap.intern("elt"),
            .@"%svset" = try heap.intern("%svset"),
            .@"%aset" = try heap.intern("%aset"),
            .@"%set-slot-value" = try heap.intern("%set-slot-value"),
            .@"%sset" = try heap.intern("%sset"),
            .@"%make-unbound" = try heap.intern("%make-unbound"),
            .@"%class-of" = try heap.intern("%class-of"),
            .@"%find-class" = try heap.intern("%find-class"),
            .@"%class-name" = try heap.intern("%class-name"),
            // Stream I/O primitives
            .@"%open" = try heap.intern("%open"),
            .@"%close" = try heap.intern("%close"),
            .close = try heap.intern("close"),
            .@"%read-line" = try heap.intern("%read-line"),
            .@"%write-line" = try heap.intern("%write-line"),
            .@"%write-string" = try heap.intern("%write-string"),
            .@"%read-byte" = try heap.intern("%read-byte"),
            .@"%write-byte" = try heap.intern("%write-byte"),
            .@"%file-position" = try heap.intern("%file-position"),
            .@"%file-length" = try heap.intern("%file-length"),
            .@"%finish-output" = try heap.intern("%finish-output"),
            .@"%force-output" = try heap.intern("%force-output"),
            .@"%clear-input" = try heap.intern("%clear-input"),
            .@"%clear-output" = try heap.intern("%clear-output"),
            .@"%sleep" = try heap.intern("%sleep"),
            .@"vector-length" = try heap.intern("vector-length"),
            .@"make-vector" = try heap.intern("make-vector"),
            .vector = try heap.intern("vector"),
            .@"make-array" = try heap.intern("make-array"),
            .@"%fill-pointer" = try heap.intern("%fill-pointer"),
            .@"%set-fill-pointer" = try heap.intern("%set-fill-pointer"),
            .@"%set-adjustable" = try heap.intern("%set-adjustable"),
            .@"%vector-push" = try heap.intern("%vector-push"),
            .@"%vector-push-extend" = try heap.intern("%vector-push-extend"),
            .@"%vector-pop" = try heap.intern("%vector-pop"),
            .@"%adjust-array" = try heap.intern("%adjust-array"),
            // Primitives - String operations (CL names)
            .char = try heap.intern("char"),
            .schar = try heap.intern("schar"),
            .@"string-length" = try heap.intern("string-length"),
            .@"string-concat" = try heap.intern("string-concat"),
            .@"string=" = try heap.intern("string="),
            .@"string<" = try heap.intern("string<"),
            .@"string>" = try heap.intern("string>"),
            .@"string<=" = try heap.intern("string<="),
            .@"string>=" = try heap.intern("string>="),
            .substring = try heap.intern("substring"),
            .subseq = try heap.intern("subseq"),
            // Primitives - I/O and misc
            .write = try heap.intern("write"),
            .prin1 = try heap.intern("prin1"),
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
            .lognand = try heap.intern("lognand"),
            .lognor = try heap.intern("lognor"),
            .logandc1 = try heap.intern("logandc1"),
            .logandc2 = try heap.intern("logandc2"),
            .logorc1 = try heap.intern("logorc1"),
            .logorc2 = try heap.intern("logorc2"),
            .logeqv = try heap.intern("logeqv"),
            .logbitp = try heap.intern("logbitp"),
            .logcount = try heap.intern("logcount"),
            .@"integer-length" = try heap.intern("integer-length"),
            // Primitives - File I/O
            .@"read-file" = try heap.intern("read-file"),
            .@"write-file" = try heap.intern("write-file"),
            .@"delete-file" = try heap.intern("delete-file"),
            .@"rename-file" = try heap.intern("rename-file"),
            .@"probe-file" = try heap.intern("probe-file"),
            .@"file-write-date" = try heap.intern("file-write-date"),
            .@"file-author" = try heap.intern("file-author"),
            .@"file-string-length" = try heap.intern("file-string-length"),
            .@"get-universal-time" = try heap.intern("get-universal-time"),
            .@"get-internal-real-time" = try heap.intern("get-internal-real-time"),
            .@"get-internal-run-time" = try heap.intern("get-internal-run-time"),
            .@"get-decoded-time" = try heap.intern("get-decoded-time"),
            .@"decode-universal-time" = try heap.intern("decode-universal-time"),
            .@"encode-universal-time" = try heap.intern("encode-universal-time"),
            .room = try heap.intern("room"),
            .@"lisp-implementation-type" = try heap.intern("lisp-implementation-type"),
            .@"lisp-implementation-version" = try heap.intern("lisp-implementation-version"),
            .@"software-type" = try heap.intern("software-type"),
            .@"machine-type" = try heap.intern("machine-type"),
            .@"machine-instance" = try heap.intern("machine-instance"),
            .@"machine-version" = try heap.intern("machine-version"),
            .@"software-version" = try heap.intern("software-version"),
            .@"short-site-name" = try heap.intern("short-site-name"),
            .@"long-site-name" = try heap.intern("long-site-name"),
            .@"user-homedir-pathname" = try heap.intern("user-homedir-pathname"),
            .@"%make-pathname" = try heap.intern("%make-pathname"),
            .@"%make-array-contents" = try heap.intern("%make-array-contents"),
            // Primitives - String construction
            .@"make-string" = try heap.intern("make-string"),
            .@"string-to-list" = try heap.intern("string-to-list"),
            .@"list-to-string" = try heap.intern("list-to-string"),
            .@"string-upcase" = try heap.intern("string-upcase"),
            .@"%string-upcase" = try heap.intern("%string-upcase"),
            .@"string-downcase" = try heap.intern("string-downcase"),
            .@"%string-downcase" = try heap.intern("%string-downcase"),
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
            .@"hash-table-capacity" = try heap.intern("hash-table-capacity"),
            .@"hash-table-p" = try heap.intern("hash-table-p"),
            .@"hash-table-keys" = try heap.intern("hash-table-keys"),
            .@"hash-table-alist" = try heap.intern("hash-table-alist"),
            .sxhash = try heap.intern("sxhash"),
            .rationalp = try heap.intern("rationalp"),
            .complexp = try heap.intern("complexp"),
            .@"make-complex" = try heap.intern("make-complex"),
            .@"real-part" = try heap.intern("real-part"),
            .@"imag-part" = try heap.intern("imag-part"),
            .numerator = try heap.intern("numerator"),
            .denominator = try heap.intern("denominator"),
            .rational = try heap.intern("rational"),
            .rationalize = try heap.intern("rationalize"),
            // Streams
            .streamp = try heap.intern("streamp"),
            .@"input-stream-p" = try heap.intern("input-stream-p"),
            .@"output-stream-p" = try heap.intern("output-stream-p"),
            .@"open-stream-p" = try heap.intern("open-stream-p"),
            .@"interactive-stream-p" = try heap.intern("interactive-stream-p"),
            .@"stream-element-type" = try heap.intern("stream-element-type"),
            .@"stream-external-format" = try heap.intern("stream-external-format"),
            .@"make-string-input-stream" = try heap.intern("make-string-input-stream"),
            .@"make-string-output-stream" = try heap.intern("make-string-output-stream"),
            .@"get-output-stream-string" = try heap.intern("get-output-stream-string"),
            .@"write-to-stream" = try heap.intern("write-to-stream"),
            // Compound streams
            .@"broadcast-stream-streams" = try heap.intern("broadcast-stream-streams"),
            .@"concatenated-stream-streams" = try heap.intern("concatenated-stream-streams"),
            .@"echo-stream-input-stream" = try heap.intern("echo-stream-input-stream"),
            .@"echo-stream-output-stream" = try heap.intern("echo-stream-output-stream"),
            .@"synonym-stream-symbol" = try heap.intern("synonym-stream-symbol"),
            .@"two-way-stream-input-stream" = try heap.intern("two-way-stream-input-stream"),
            .@"two-way-stream-output-stream" = try heap.intern("two-way-stream-output-stream"),
            .@"%make-synonym-stream" = try heap.intern("%make-synonym-stream"),
            .@"%make-echo-stream" = try heap.intern("%make-echo-stream"),
            .@"%make-two-way-stream" = try heap.intern("%make-two-way-stream"),
            .@"%make-broadcast-stream" = try heap.intern("%make-broadcast-stream"),
            .@"%make-concatenated-stream" = try heap.intern("%make-concatenated-stream"),
            .@"%make-broadcast-stream-list" = try heap.intern("%make-broadcast-stream-list"),
            .@"%make-concatenated-stream-list" = try heap.intern("%make-concatenated-stream-list"),
            .@"%disassemble" = try heap.intern("%disassemble"),
            .@"%read-char-from-stream" = try heap.intern("%read-char-from-stream"),
            .@"%peek-char-from-stream" = try heap.intern("%peek-char-from-stream"),
            .@"%open-file" = try heap.intern("%open-file"),
            .@"%close-stream" = try heap.intern("%close-stream"),
            // Pathname primitives
            .@"%pathname-host" = try heap.intern("%pathname-host"),
            .@"%pathname-device" = try heap.intern("%pathname-device"),
            .@"%pathname-directory" = try heap.intern("%pathname-directory"),
            .@"%pathname-name" = try heap.intern("%pathname-name"),
            .@"%pathname-type" = try heap.intern("%pathname-type"),
            .@"%pathname-version" = try heap.intern("%pathname-version"),
            .@"pathname-host" = try heap.intern("pathname-host"),
            .@"pathname-device" = try heap.intern("pathname-device"),
            .@"pathname-directory" = try heap.intern("pathname-directory"),
            .@"pathname-name" = try heap.intern("pathname-name"),
            .@"pathname-type" = try heap.intern("pathname-type"),
            .@"pathname-version" = try heap.intern("pathname-version"),
            .truename = try heap.intern("truename"),
            .@"ensure-directories-exist" = try heap.intern("ensure-directories-exist"),
            .pathname = try heap.intern("pathname"),
            .@"parse-namestring" = try heap.intern("parse-namestring"),
            .namestring = try heap.intern("namestring"),
            .@"merge-pathnames" = try heap.intern("merge-pathnames"),
            .@"directory-namestring" = try heap.intern("directory-namestring"),
            .@"file-namestring" = try heap.intern("file-namestring"),
            .@"host-namestring" = try heap.intern("host-namestring"),
            .@"wild-pathname-p" = try heap.intern("wild-pathname-p"),
            .@"package-symbols-table" = try heap.intern("package-symbols-table"),
            .@"package-exports-table" = try heap.intern("package-exports-table"),
            .@"%package-symbols-list" = try heap.intern("%package-symbols-list"),
            .@"%package-exports-list" = try heap.intern("%package-exports-list"),
            .@"package-name" = try heap.intern("package-name"),
            .@"package-nicknames" = try heap.intern("package-nicknames"),
            .@"package-use-list" = try heap.intern("package-use-list"),
            .@"package-used-by-list" = try heap.intern("package-used-by-list"),
            .@"package-shadowing-symbols" = try heap.intern("package-shadowing-symbols"),
            .packagep = try heap.intern("packagep"),
            .@"symbol-package" = try heap.intern("symbol-package"),
            .@"list-all-packages" = try heap.intern("list-all-packages"),
            .@"find-package" = try heap.intern("find-package"),
            .@"delete-package" = try heap.intern("delete-package"),
            .@"%import" = try heap.intern("%import"),
            .@"%use-package" = try heap.intern("%use-package"),
            .@"%unexport" = try heap.intern("%unexport"),
            .@"%shadow" = try heap.intern("%shadow"),
            .@"%shadowing-import" = try heap.intern("%shadowing-import"),
            .@"%unuse-package" = try heap.intern("%unuse-package"),
            .@"%unintern" = try heap.intern("%unintern"),
            .@"%find-symbol" = try heap.intern("%find-symbol"),
            .@"find-all-symbols" = try heap.intern("find-all-symbols"),
            .@"apropos-list" = try heap.intern("apropos-list"),
            .@"read-char-no-hang" = try heap.intern("read-char-no-hang"),
            .@"compute-restarts" = try heap.intern("compute-restarts"),
            .@"restart-name" = try heap.intern("restart-name"),
            .directory = try heap.intern("directory"),
            .@"pathname-match-p" = try heap.intern("pathname-match-p"),
            .@"enough-namestring" = try heap.intern("enough-namestring"),
            .@"%make-package" = try heap.intern("%make-package"),
            .@"%rename-package" = try heap.intern("%rename-package"),
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
            .ty_and = try heap.intern("and"),
            .ty_not = try heap.intern("not"),
            .ty_member = try heap.intern("member"),
            .ty_eql = try heap.intern("eql"),
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
            .@"&allow-other-keys" = try heap.intern("&allow-other-keys"),
            .@"&aux" = try heap.intern("&aux"),
            .@"&whole" = try heap.intern("&whole"),
            .@"&environment" = try heap.intern("&environment"),
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
            .kw_key = try heap.internKeyword("key"),
            .kw_count = try heap.internKeyword("count"),
            .kw_eq = try heap.internKeyword("eq"),
            .kw_eql = try heap.internKeyword("eql"),
            .kw_equal = try heap.internKeyword("equal"),
            .@"kw_initial-element" = try heap.internKeyword("initial-element"),
            .@"kw_element-type" = try heap.internKeyword("element-type"),
            .@"kw_allow-other-keys" = try heap.internKeyword("allow-other-keys"),
            .kw_colon = try heap.intern(":"),
            .kw_type = try heap.internKeyword("type"),
            .kw_initform = try heap.internKeyword("initform"),
            .kw_allocation = try heap.internKeyword("allocation"),
            .kw_instance = try heap.internKeyword("instance"),
            .kw_class = try heap.internKeyword("class"),
            .kw_initarg = try heap.internKeyword("initarg"),
            .kw_reader = try heap.internKeyword("reader"),
            .kw_writer = try heap.internKeyword("writer"),
            .kw_accessor = try heap.internKeyword("accessor"),
            // *features* keywords
            .kw_habu = try heap.internKeyword("habu"),
            .kw_zig = try heap.internKeyword("zig"),
            .kw_unix = try heap.internKeyword("unix"),
            .kw_darwin = try heap.internKeyword("darwin"),
            .kw_windows = try heap.internKeyword("windows"),
            .kw_absolute = try heap.internKeyword("absolute"),
            .kw_relative = try heap.internKeyword("relative"),
        };
    }

    /// Comptime list of primitive function field names (not special forms)
    const primitive_fields = [_][]const u8{
        // Arithmetic
        "+",                      "-",                        "*",                         "/",                        "mod",                      "%",                           "quot",               "truncate",               "rem",
        // Comparison
        "eq",                     "equal",                    "eql",                       "equalp",                   "<",                        ">",                           "<=",                 ">=",                     "=",
        // List operations
        "cons",                   "car",                      "cdr",                       "first",                    "rest",                     "caar",                        "cadr",               "cdar",                   "cddr",
        "caaar",                  "caadr",                    "cadar",                     "caddr",                    "cdaar",                    "cdadr",                       "cddar",              "cdddr",                  "second",
        "third",                  "fourth",                   "append",                    "length",                   "reverse",                  "nth",                         "nthcdr",             "last",                   "member",
        "assoc",                  "find",                     "position",                  "count",                    "remove",                   "list",                        "rplaca",             "rplacd",
        // Type predicates
                        "consp",
        "symbolp",                "numberp",                  "integerp",                  "realp",                    "stringp",                  "vectorp",                     "closurep",           "keywordp",               "null",
        "not",                    "characterp",               "floatp",                    "listp",                    "atom",
        // Character operations
                            "char-code",                   "code-char",          "char=",                  "char<",
        "char>",                  "%read-char",               "%peek-char",                "read", "%read",                    "read-from-string",         "load",                        "unread-char",        "listen",                 "upgraded-complex-part-type",
        "eval",                   "gensym",                   "macroexpand",               "macroexpand-1",
        // Symbol operations
                   "boundp",                   "fboundp",                     "symbol-value",       "symbol-function",        "symbol-plist",            "function-lambda-expression",
        "typep",                  "type-of",                  "intern",                    "symbol-name",              "symbol-package",           "copy-symbol",              "makunbound",                  "set",                "copy-structure",         "get",
        "put",                    "remprop",                   "%set-symbol-value",         "%set-symbol-plist",
        // Numeric
                         "abs",                       "zerop",                    "plusp",                    "minusp",                      "evenp",              "oddp",
        // Math functions
                          "sqrt",
        "sin",                    "cos",                      "tan",                       "exp",                      "log",                      "floor",                       "ceiling",            "round",
        // Vector operations
                         "aref",
        "svref",                  "vector-length",            "make-vector",               "vector",                   "make-array",
        // String operations
                      "char",                        "schar",              "string-length",          "string-concat",
        "string=",                "string<",                  "string>",                   "string<=",                 "string>=",                 "substring",                   "subseq",
        // I/O
                    "write",                  "print",
        "princ",                  "terpri",                   "write-char",                "random",                   "random-seed",              "format",
        // Character functions
                             "char-upcase",        "char-downcase",          "digit-char-p",
        "alpha-char-p",
        // String/number conversion
                  "parse-integer",            "write-to-string",
        // Bitwise operations
        "logand",                   "logior",                   "logxor",                      "lognot",             "ash",                    "lognand",
        "lognor",                 "logandc1",                 "logandc2",                  "logorc1",                  "logorc2",                  "logeqv",                      "logbitp",                  "logcount",                    "integer-length",
        // File I/O
            "read-file",              "write-file",
        "delete-file",            "rename-file",              "probe-file",                "file-write-date",          "file-author",              "file-string-length",          "get-universal-time", "get-internal-real-time", "get-internal-run-time",
        "get-decoded-time",       "decode-universal-time",    "encode-universal-time",     "room",                     "lisp-implementation-type", "lisp-implementation-version", "software-type",      "machine-type",           "machine-instance",
        "machine-version",        "software-version",         "short-site-name",           "long-site-name",           "user-homedir-pathname",
        // String construction
           "make-string",                 "string-to-list",     "list-to-string",         "string-upcase",
        "string-downcase",        "concatenate",   "%string-upcase",  "%string-downcase",
        // Hash tables
                     "make-hash-table",           "gethash",                  "puthash",                  "remhash",                     "hash-table-count",   "hash-table-capacity",    "clrhash",
        "hash-table-test",        "hash-table-p",             "hash-table-keys",           "hash-table-alist",         "sxhash",
        // Numeric types
                          "rationalp",                   "complexp",           "make-complex",           "real-part",
        "imag-part",              "numerator",                "denominator",
        // Streams
                      "streamp",                  "input-stream-p",           "output-stream-p",             "open-stream-p",      "interactive-stream-p",   "stream-element-type",
        "stream-external-format", "make-string-input-stream", "make-string-output-stream", "get-output-stream-string", "write-to-stream",
        // Pathname primitives
                 "pathname-host",               "pathname-device",    "pathname-directory",     "pathname-name",
        "pathname-type",          "pathname-version",         "truename",                  "ensure-directories-exist", "pathname",                 "parse-namestring",            "namestring",         "merge-pathnames",        "directory-namestring",
        "file-namestring",        "host-namestring",          "wild-pathname-p",
        // Also callable
                  "funcall",                  "apply",                    "values",                      "values-list",
    };

    /// Check if a symbol is a builtin function (not special form)
    pub fn isBuiltinFunction(self: *const Builtins, sym: Value) bool {
        const s = sym.raw;
        inline for (primitive_fields) |field| {
            if (s == @field(self, field).raw) return true;
        }
        return false;
    }
};

/// Lexical environment for variable resolution
const VarKey = struct {
    /// Pointer to home package (Zig Package), 0 for uninterned or compiler temps.
    pkg_ptr: usize,
    /// Uninterned stable id, 0 for interned symbols and compiler temps.
    uid: u64,
    /// Symbol name (owned for uid==0 keys; empty for uid!=0 keys).
    name: []const u8,
};

const VarKeyCtx = struct {
    pub fn hash(_: VarKeyCtx, key: VarKey) u64 {
        var h = std.hash.Wyhash.hash(0, std.mem.asBytes(&key.pkg_ptr));
        h = std.hash.Wyhash.hash(h, std.mem.asBytes(&key.uid));
        return std.hash.Wyhash.hash(h, key.name);
    }

    pub fn eql(_: VarKeyCtx, a: VarKey, b: VarKey) bool {
        return a.pkg_ptr == b.pkg_ptr and a.uid == b.uid and std.mem.eql(u8, a.name, b.name);
    }
};

const VarMap = std.HashMap(VarKey, u16, VarKeyCtx, std.hash_map.default_max_load_percentage);

pub const OptimizeSettings = struct {
    speed: u8 = 1,
    safety: u8 = 1,
    debug: u8 = 1,
    space: u8 = 1,
    compilation_speed: u8 = 1,
};

pub const Env = struct {
    pub const Binding = struct {
        depth: u16,
        index: u16,
    };

    /// Variable bindings at this level
    bindings: VarMap,
    /// Function bindings at this level (Lisp-2 function namespace)
    fn_bindings: VarMap,
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
    /// Effective optimize settings for this lexical scope
    optimize: OptimizeSettings,

    /// Create a new frame environment (for lambda)
    pub fn init(allocator: std.mem.Allocator, parent: ?*const Env) Env {
        return .{
            .bindings = VarMap.init(allocator),
            .fn_bindings = VarMap.init(allocator),
            .parent = parent,
            .depth = if (parent) |p| p.depth + 1 else 0,
            .new_frame = true,
            .base_index = 0,
            .allocator = allocator,
            .optimize = if (parent) |p| p.optimize else .{},
        };
    }

    /// Create a same-frame environment (for let)
    pub fn initLet(allocator: std.mem.Allocator, parent: *const Env) Env {
        return .{
            .bindings = VarMap.init(allocator),
            .fn_bindings = VarMap.init(allocator),
            .parent = parent,
            .depth = parent.depth, // Same depth - same frame
            .new_frame = false,
            .base_index = parent.localCount(), // Continue from parent's count
            .allocator = allocator,
            .optimize = parent.optimize,
        };
    }

    pub fn deinit(self: *Env) void {
        var it = self.bindings.keyIterator();
        while (it.next()) |key| {
            // Only uid==0 keys allocate name storage.
            if (key.uid == 0) self.allocator.free(key.name);
        }
        self.bindings.deinit();

        var fn_it = self.fn_bindings.keyIterator();
        while (fn_it.next()) |key| {
            // Only uid==0 keys allocate name storage.
            if (key.uid == 0) self.allocator.free(key.name);
        }
        self.fn_bindings.deinit();
    }

    /// Get total local count in this frame
    pub fn localCount(self: *const Env) u16 {
        const own_count: u16 = @intCast(self.bindings.count());
        // base_index is absolute for both lambda and let scopes.
        // Nested let scopes set base_index from parent.localCount().
        return self.base_index + own_count;
    }

    fn keyFromSym(sym_val: Value) ?VarKey {
        if (sym_val.typeKind() != .symbol) return null;
        const sym = sym_val.toPtr(Symbol);
        const bits: u64 = sym.reserved;
        if (bits != 0 and (bits & 1) == 0) {
            return .{
                .pkg_ptr = @intCast(bits),
                .uid = 0,
                .name = sym.getName(),
            };
        }
        if ((bits & 1) != 0) {
            return .{
                .pkg_ptr = 0,
                .uid = bits >> 1,
                .name = "",
            };
        }
        // Legacy/unannotated symbol: treat as temp name key.
        return .{
            .pkg_ptr = 0,
            .uid = 0,
            .name = sym.getName(),
        };
    }

    fn keyOwnedFromSym(allocator: std.mem.Allocator, sym_val: Value) error{OutOfMemory}!VarKey {
        const k = keyFromSym(sym_val) orelse return error.OutOfMemory;
        if (k.uid != 0) return .{ .pkg_ptr = 0, .uid = k.uid, .name = "" };
        const name_copy = try allocator.dupe(u8, k.name);
        return .{ .pkg_ptr = k.pkg_ptr, .uid = 0, .name = name_copy };
    }

    /// Add a binding for a symbol, returns the absolute index
    pub fn bindSym(self: *Env, sym_val: Value) error{OutOfMemory}!u16 {
        const local_index: u16 = @intCast(self.bindings.count());
        const abs_index = self.base_index + local_index;
        const key = try keyOwnedFromSym(self.allocator, sym_val);
        try self.bindings.put(key, abs_index);
        return abs_index;
    }

    /// Add a binding for a compiler-generated name, returns the absolute index
    pub fn bindName(self: *Env, name: []const u8) error{OutOfMemory}!u16 {
        const local_index: u16 = @intCast(self.bindings.count());
        const abs_index = self.base_index + local_index;
        const name_copy = try self.allocator.dupe(u8, name);
        try self.bindings.put(.{ .pkg_ptr = 0, .uid = 0, .name = name_copy }, abs_index);
        return abs_index;
    }

    fn bindHiddenSlot(self: *Env) error{OutOfMemory}!u16 {
        const local_index: u16 = @intCast(self.bindings.count());
        const abs_index = self.base_index + local_index;
        const hidden_name = try std.fmt.allocPrint(self.allocator, "__fn_slot_{d}", .{abs_index});
        try self.bindings.put(.{ .pkg_ptr = 0, .uid = 0, .name = hidden_name }, abs_index);
        return abs_index;
    }

    /// Add a function binding for a symbol in the function namespace.
    pub fn bindFunctionSym(self: *Env, sym_val: Value) error{OutOfMemory}!u16 {
        const abs_index = try self.bindHiddenSlot();
        const key = try keyOwnedFromSym(self.allocator, sym_val);
        try self.fn_bindings.put(key, abs_index);
        return abs_index;
    }

    fn lookupKey(self: *const Env, key: VarKey) ?Binding {
        if (self.bindings.get(key)) |index| {
            return .{ .depth = 0, .index = index };
        }
        if (self.parent) |parent| {
            if (parent.lookupKey(key)) |result| {
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

    /// Look up a symbol variable, returns (depth, index) or null
    pub fn lookupSym(self: *const Env, sym_val: Value) ?Binding {
        const key = keyFromSym(sym_val) orelse return null;
        return self.lookupKey(key);
    }

    /// Look up a compiler-generated name, returns (depth, index) or null
    pub fn lookupName(self: *const Env, name: []const u8) ?Binding {
        return self.lookupKey(.{ .pkg_ptr = 0, .uid = 0, .name = name });
    }

    fn lookupFunctionKey(self: *const Env, key: VarKey) ?Binding {
        if (self.fn_bindings.get(key)) |index| {
            return .{ .depth = 0, .index = index };
        }
        if (self.parent) |parent| {
            if (parent.lookupFunctionKey(key)) |result| {
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

    fn lookupFunctionName(self: *const Env, name: []const u8) ?Binding {
        var it = self.fn_bindings.iterator();
        while (it.next()) |entry| {
            if (std.ascii.eqlIgnoreCase(entry.key_ptr.name, name)) {
                return .{ .depth = 0, .index = entry.value_ptr.* };
            }
        }

        if (self.parent) |parent| {
            if (parent.lookupFunctionName(name)) |result| {
                if (self.new_frame) {
                    return .{ .depth = result.depth + 1, .index = result.index };
                } else {
                    return result;
                }
            }
        }
        return null;
    }

    /// Look up a function symbol, returns (depth, index) or null.
    pub fn lookupFunctionSym(self: *const Env, sym_val: Value) ?Binding {
        if (keyFromSym(sym_val)) |key| {
            if (self.lookupFunctionKey(key)) |binding| return binding;
        }

        if (sym_val.isSymbol()) {
            return self.lookupFunctionName(sym_val.toPtr(Symbol).getName());
        }

        return null;
    }

    /// Look up any lexical symbol by case-insensitive symbol name.
    /// This is used for special vars like *PACKAGE* where package-qualified
    /// symbol identity may differ across read/compile contexts.
    pub fn lookupSymbolName(self: *const Env, sym_name: []const u8) ?Binding {
        var it = self.bindings.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            if (key.uid != 0) continue;
            if (key.name.len == 0) continue;
            if (!std.ascii.eqlIgnoreCase(key.name, sym_name)) continue;
            return .{ .depth = 0, .index = entry.value_ptr.* };
        }

        if (self.parent) |parent| {
            if (parent.lookupSymbolName(sym_name)) |result| {
                if (self.new_frame) {
                    return .{ .depth = result.depth + 1, .index = result.index };
                }
                return result;
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
    names: std.AutoHashMap(Value, void),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) BoxingSet {
        return .{
            .names = std.AutoHashMap(Value, void).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *BoxingSet) void {
        self.names.deinit();
    }

    pub fn add(self: *BoxingSet, sym: Value) !void {
        try self.names.put(sym, {});
    }

    pub fn contains(self: *const BoxingSet, sym: Value) bool {
        return self.names.contains(sym);
    }
};

/// Capture analysis result
pub const CaptureSet = struct {
    /// Free variables that need to be captured
    captures: std.ArrayList(Ir.Capture),
    /// Fast deduplication lookup
    seen: std.AutoHashMap(u32, void),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) CaptureSet {
        return .{
            .captures = std.ArrayList(Ir.Capture){},
            .seen = std.AutoHashMap(u32, void).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CaptureSet) void {
        self.captures.deinit(self.allocator);
        self.seen.deinit();
    }

    /// Add a capture if not already present
    pub fn addCapture(self: *CaptureSet, name: []const u8, depth: u16, index: u16) !void {
        const key: u32 = (@as(u32, depth) << 16) | @as(u32, index);
        const gop = try self.seen.getOrPut(key);
        if (gop.found_existing) return;
        const name_copy = try self.allocator.dupe(u8, name);
        try self.captures.append(self.allocator, .{
            .name = name_copy,
            .depth = depth,
            .index = index,
        });
    }
};

/// Declaration specifier types
pub const DeclSpec = enum {
    type_decl,
    ftype,
    declaration,
    inline_decl,
    notinline,
    ignore,
    ignorable,
    special,
    dynamic_extent,
};

/// Declaration information for a variable
pub const DeclInfo = struct {
    spec: DeclSpec,
    /// For type/ftype: type expression value
    type_expr: ?Value = null,
};

/// Declaration environment - tracks declarations in current scope
pub const DeclEnv = struct {
    /// Map from variable name to declaration info
    decls: std.StringArrayHashMapUnmanaged(std.ArrayList(DeclInfo)),
    allocator: std.mem.Allocator,

    pub fn init(self: *DeclEnv, allocator: std.mem.Allocator) void {
        self.* = .{
            .decls = .{},
            .allocator = allocator,
        };
    }

    pub fn create(allocator: std.mem.Allocator) DeclEnv {
        return .{
            .decls = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DeclEnv) void {
        var iter = self.decls.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.allocator);
        }
        self.decls.deinit(self.allocator);
    }

    /// Add a declaration for a variable
    pub fn addDecl(self: *DeclEnv, name: []const u8, info: DeclInfo) !void {
        const existing = self.decls.getPtr(name);
        if (existing) |list| {
            try list.append(self.allocator, info);
        } else {
            const owned_name = try self.allocator.dupe(u8, name);
            var list = std.ArrayList(DeclInfo){};
            try list.append(self.allocator, info);
            try self.decls.put(self.allocator, owned_name, list);
        }
    }

    /// Check if variable has a specific declaration
    pub fn hasDecl(self: *const DeclEnv, name: []const u8, spec: DeclSpec) bool {
        if (self.decls.get(name)) |infos| {
            for (infos.items) |info| {
                if (info.spec == spec) return true;
            }
        }
        return false;
    }

    /// Get type declaration for variable if present
    pub fn getTypeDecl(self: *const DeclEnv, name: []const u8) ?Value {
        if (self.decls.get(name)) |infos| {
            for (infos.items) |info| {
                if (info.spec == .type_decl) return info.type_expr;
            }
        }
        return null;
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
    /// Function symbols that are boxed (used by LABELS letrec lowering).
    boxed_fn_syms: ?*const BoxingSet,
    /// Defined ADT types for match exhaustiveness checking
    defined_types: std.StringHashMap([]const Variant),
    /// Defined struct types for typed struct support
    /// Maps struct name to its Type definition
    struct_types: std.StringHashMap(*const types.Type),
    /// Struct predicate names mapped to struct types (for occurrence typing)
    /// Maps "point-p" -> pointer to point struct type
    struct_predicates: std.StringHashMap(*const types.Type),
    /// Struct/class accessor names mapped to slot index for setf compilation.
    /// Maps "foo-bar" / "PKG:foo-bar" -> slot index (0-based field index).
    struct_accessors: std.StringHashMap(usize),
    /// Macro table: maps macro name to closure (expander function)
    /// When a form (macro-name args...) is compiled, the macro is expanded first
    macro_table: std.AutoHashMap(Value, Value),
    /// Symbol macro table: maps symbol to expansion form
    /// When a symbol is compiled, if in this table, the expansion is compiled instead
    symbol_macros: std.AutoHashMap(Value, Value),
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
    /// Current method params for call-next-method (set during method body compilation)
    method_params: ?[]const []const u8 = null,
    /// Type abbreviations for CL deftype
    /// Maps type name to expansion function (Value closure)
    type_aliases: std.StringHashMap(Value),
    /// Global declaration environment
    global_decls: DeclEnv,
    /// Persistent optimize declarations from DECLAIM/PROCLAIM
    optimize_global: OptimizeSettings,
    /// Effective optimize settings for current compile scope
    optimize_current: OptimizeSettings,
    /// Diagnostic prints for compile errors
    diag: bool,

    /// ADT variant definition
    pub const Variant = struct {
        name: []const u8,
        sym: Value,
        fields: []const []const u8,
    };

    /// CLOS method qualifier for standard method combination
    pub const MethodQualifier = enum {
        primary,
        before,
        after,
        around,
    };

    /// CLOS method definition
    pub const MethodDef = struct {
        specializers: []const Value, // Interned class name symbols for each parameter
        function_name: []const u8, // Global function name to call
        qualifier: MethodQualifier = .primary,
    };

    /// Typed parameter info for function declarations
    pub const TypedParam = struct {
        name: []const u8,
        type_sym: ?Value, // null for untyped, otherwise the type symbol
        /// Local slot index in the lambda frame
        idx: u16,
    };

    pub fn init(allocator: std.mem.Allocator, vm: *Vm) Compiler {
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .type_checker = TypeChecker.init(allocator),
            .bi_checker = BiChecker.init(allocator, &vm.builtins),
            .type_checking_enabled = false, // Off by default for gradual typing
            .globals = GlobalEnv.init(allocator),
            .builtins = null, // Lazily initialized when heap is available
            .occ = null,
            .boxed_vars = null,
            .boxed_fn_syms = null,
            .defined_types = std.StringHashMap([]const Variant).init(allocator),
            .struct_types = std.StringHashMap(*const types.Type).init(allocator),
            .struct_predicates = std.StringHashMap(*const types.Type).init(allocator),
            .struct_accessors = std.StringHashMap(usize).init(allocator),
            .macro_table = std.AutoHashMap(Value, Value).init(allocator),
            .symbol_macros = std.AutoHashMap(Value, Value).init(allocator),
            .vm = vm,
            .heap = vm.heap,
            .class_metadata = std.StringHashMap([]const SlotSpec).init(allocator),
            .generic_functions = std.StringHashMap(std.ArrayList(MethodDef)).init(allocator),
            .type_aliases = std.StringHashMap(Value).init(allocator),
            .global_decls = DeclEnv.create(allocator),
            .optimize_global = .{},
            .optimize_current = .{},
            .diag = false,
        };
    }

    /// Initialize with heap for symbol interning
    pub fn initWithHeap(allocator: std.mem.Allocator, vm: *Vm) !Compiler {
        const builtins = try initBuiltinsCanonical(vm.heap);
        if (vm.heap.cl_user_package) |cl_user| {
            vm.heap.setCurrentPackage(cl_user);
        }
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .type_checker = TypeChecker.init(allocator),
            .bi_checker = BiChecker.init(allocator, &vm.builtins),
            .type_checking_enabled = false,
            .globals = GlobalEnv.init(allocator),
            .builtins = builtins,
            .occ = null,
            .boxed_vars = null,
            .boxed_fn_syms = null,
            .defined_types = std.StringHashMap([]const Variant).init(allocator),
            .struct_types = std.StringHashMap(*const types.Type).init(allocator),
            .struct_predicates = std.StringHashMap(*const types.Type).init(allocator),
            .struct_accessors = std.StringHashMap(usize).init(allocator),
            .macro_table = std.AutoHashMap(Value, Value).init(allocator),
            .symbol_macros = std.AutoHashMap(Value, Value).init(allocator),
            .vm = vm,
            .heap = vm.heap,
            .class_metadata = std.StringHashMap([]const SlotSpec).init(allocator),
            .generic_functions = std.StringHashMap(std.ArrayList(MethodDef)).init(allocator),
            .type_aliases = std.StringHashMap(Value).init(allocator),
            .global_decls = DeclEnv.create(allocator),
            .optimize_global = .{},
            .optimize_current = .{},
            .diag = false,
        };
    }

    /// Initialize with heap while preserving current package selection.
    pub fn initWithHeapPreservePackage(allocator: std.mem.Allocator, vm: *Vm) !Compiler {
        const builtins = try initBuiltinsCanonical(vm.heap);
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .type_checker = TypeChecker.init(allocator),
            .bi_checker = BiChecker.init(allocator, &vm.builtins),
            .type_checking_enabled = false,
            .globals = GlobalEnv.init(allocator),
            .builtins = builtins,
            .occ = null,
            .boxed_vars = null,
            .boxed_fn_syms = null,
            .defined_types = std.StringHashMap([]const Variant).init(allocator),
            .struct_types = std.StringHashMap(*const types.Type).init(allocator),
            .struct_predicates = std.StringHashMap(*const types.Type).init(allocator),
            .struct_accessors = std.StringHashMap(usize).init(allocator),
            .macro_table = std.AutoHashMap(Value, Value).init(allocator),
            .symbol_macros = std.AutoHashMap(Value, Value).init(allocator),
            .vm = vm,
            .heap = vm.heap,
            .class_metadata = std.StringHashMap([]const SlotSpec).init(allocator),
            .generic_functions = std.StringHashMap(std.ArrayList(MethodDef)).init(allocator),
            .type_aliases = std.StringHashMap(Value).init(allocator),
            .global_decls = DeclEnv.create(allocator),
            .optimize_global = .{},
            .optimize_current = .{},
            .diag = false,
        };
    }

    /// Set VM for compile-time macro expansion
    pub fn setVm(self: *Compiler, vm: *Vm) void {
        self.vm = vm;
        self.heap = vm.heap;
    }

    fn initBuiltinsCanonical(heap: *Heap) !Builtins {
        const saved_pkg = heap.current_package;
        if (heap.cl_package) |cl_pkg| {
            heap.setCurrentPackage(cl_pkg);
        }
        defer if (saved_pkg) |pkg| {
            heap.setCurrentPackage(pkg);
        };
        return try Builtins.init(heap);
    }

    /// Refresh interned builtin symbol identities after GC may have moved objects.
    pub fn refreshBuiltins(self: *Compiler) !void {
        const heap = self.heap orelse return;
        self.builtins = try initBuiltinsCanonical(heap);
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
        // Free struct_accessors keys (allocated with globals.allocator)
        var accessor_iter = self.struct_accessors.keyIterator();
        while (accessor_iter.next()) |key| {
            self.globals.allocator.free(key.*);
        }
        self.struct_accessors.deinit();
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
                var mut_initargs = spec.initargs;
                var mut_readers = spec.readers;
                var mut_writers = spec.writers;
                mut_initargs.deinit(self.globals.allocator);
                mut_readers.deinit(self.globals.allocator);
                mut_writers.deinit(self.globals.allocator);
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
                // Specializers are interned Values (no individual free needed)
                self.globals.allocator.free(method.specializers);
                // Free function name
                self.globals.allocator.free(method.function_name);
            }
            // Free methods list
            entry.value_ptr.deinit(self.globals.allocator);
        }
        self.generic_functions.deinit();
        self.globals.deinit();
        self.macro_table.deinit();
        self.symbol_macros.deinit();
        // Free type_aliases keys
        var alias_iter = self.type_aliases.keyIterator();
        while (alias_iter.next()) |key| {
            self.globals.allocator.free(key.*);
        }
        self.type_aliases.deinit();
        // Free global_decls
        self.global_decls.deinit();
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

    fn registerStructAccessor(self: *Compiler, accessor_name: []const u8, slot_idx: usize) !void {
        if (self.struct_accessors.getPtr(accessor_name)) |idx_ptr| {
            idx_ptr.* = slot_idx;
        } else {
            const key_copy = try self.globals.allocator.dupe(u8, accessor_name);
            try self.struct_accessors.put(key_copy, slot_idx);
        }

        var qual_buf: [512]u8 = undefined;
        const q = try self.qualifyName(accessor_name, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        if (self.struct_accessors.getPtr(q.name)) |idx_ptr| {
            idx_ptr.* = slot_idx;
        } else {
            const q_copy = try self.globals.allocator.dupe(u8, q.name);
            try self.struct_accessors.put(q_copy, slot_idx);
        }
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
    ) anyerror!void {
        // Build typing context with parameter types
        var ctx = TypingCtx.init(self.allocator);
        defer ctx.deinit();

        // Add parameters to context
        for (typed_params) |tp| {
            if (tp.type_sym) |type_sym| {
                // Parse type from symbol
                const param_type = (try self.parseTypeExpr(type_sym)) orelse &types.t_any;
                try ctx.bind(tp.name, param_type, .many);
            } else {
                // Untyped parameter - bind as any
                try ctx.bind(tp.name, &types.t_any, .many);
            }
        }

        // If there's a return type, check body against it
        if (return_type) |ret_type_val| {
            const expected_type = if (try self.parseTypeExpr(ret_type_val)) |ty| ty else return error.InvalidSyntax;
            try self.bi_checker.check(body_ir, expected_type, &ctx);
        } else {
            // No return type specified - just infer (validates internal consistency)
            _ = try self.bi_checker.infer(body_ir, &ctx);
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
                .unbound => &types.t_symbol, // unbound marker is symbol-like
                .fixnum => &types.t_fixnum,
                .float => &types.t_float,
                .char => &types.t_char,
                .string => &types.t_string,
                .string32 => &types.t_string,
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
                .package => &types.t_any, // Packages are rare as literals
                .chunk => &types.t_any, // Chunks are internal
                .condition => &types.t_any, // Condition objects
                .class => &types.t_any, // Class objects
                .slotdef => &types.t_any, // SlotDefinition objects
                .generic_function => &types.t_any, // Generic function objects
                .method => &types.t_any, // Method objects
                .native_code => &types.t_any, // Native code handles are internal
                .macro_env => &types.t_any, // Macro environment objects
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
            .consp, .symbolp, .numberp, .integerp, .realp, .nilp, .not, .stringp, .vectorp => &types.t_any,
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
            var operand = sp.operand;
            // Unwrap box_ref for closure variables
            if (operand.* == .box_ref) {
                operand = operand.box_ref.operand;
            }
            if (operand.* == .@"var") {
                return .{
                    .var_name = operand.@"var".name,
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
                    var actual_operand = operand;
                    // Unwrap box_ref for closure variables
                    if (actual_operand.* == .box_ref) {
                        actual_operand = actual_operand.box_ref.operand;
                    }
                    if (actual_operand.* == .@"var") {
                        return .{
                            .var_name = actual_operand.@"var".name,
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
            try then_occ.narrowed.put(info.var_name, info.narrowed_type);
        }

        // Copy existing narrowings
        var occ_iter = occ.narrowed.iterator();
        while (occ_iter.next()) |entry| {
            try then_occ.narrowed.put(entry.key_ptr.*, entry.value_ptr.*);
        }

        const then_ir = try self.compileTyped(then_expr, env, type_env, &then_occ);

        // Compile else-branch (could narrow to complement type)
        const else_ir = try self.compileTyped(else_expr, env, type_env, occ);

        const if_ir = try self.builder.ifExpr(test_ir, then_ir.ir, else_ir.ir);

        // Result type is union of branch types (simplified to any for now)
        return .{ .ir = if_ir, .ty = &types.t_any };
    }

    /// Compile a single expression
    pub fn compile(self: *Compiler, expr: Value, env: *const Env) anyerror!*Ir {
        const result = self.compileWithTail(expr, env, false);
        return if (result) |node| node else |err| {
            if (self.diag) {
                std.debug.print("COMPILE FAILED with {}\n", .{err});
                if (expr.isCons()) {
                    const cons = expr.toPtr(Cons);
                    if (cons.car.isSymbol()) {
                        const sym = cons.car.toPtr(Symbol);
                        std.debug.print("  Failed form head: {s}\n", .{sym.getName()});
                    }
                }
            }
            return err;
        };
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

        // String (base-string or String32)
        if (expr.isString() or expr.isString32()) {
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

        // Array literal - #2A(...)
        if (expr.isArray()) {
            return try self.builder.lit(expr);
        }

        // Symbol (variable reference or symbol macro)
        if (expr.isSymbol()) {
            // Check for symbol macros first
            if (self.lookupSymbolMacro(expr)) |expansion| {
                return self.compileWithTail(expansion, env, in_tail);
            }

            const sym_val = expr;
            const sym = sym_val.toPtr(Symbol);
            const name = sym.getName();

            if (env.lookupSym(sym_val)) |binding| {
                const result_ir = try self.builder.variable(name, binding.depth, binding.index);

                // If this variable is boxed, wrap with box-ref
                if (self.boxed_vars) |bv| {
                    if (bv.contains(sym_val)) {
                        const box_ref = try self.allocator.create(Ir);
                        box_ref.* = .{ .box_ref = .{ .operand = result_ir } };
                        return box_ref;
                    }
                }

                // If variable has a type declaration, wrap with assert for specialization
                if (self.builtins) |b| {
                    if (self.global_decls.getTypeDecl(name)) |type_expr| {
                        if (type_expr.raw == b.ty_fixnum.raw) {
                            return self.builder.assertFixnum(result_ir);
                        }
                    }
                }

                return result_ir;
            }
            return self.compileGlobalSymbolRef(sym_val);
        }

        // List (special form or function call)
        if (expr.isCons()) {
            return self.compileListWithTail(expr, env, in_tail);
        }

        // Keyword - just return as literal
        if (expr.isKeyword()) {
            return try self.builder.lit(expr);
        }

        // Closure - can appear as macro expansion result
        // Treat as literal (will be used by funcall at runtime)
        if (expr.isClosure()) {
            return try self.builder.lit(expr);
        }

        if (self.diag) {
            std.debug.print("Invalid syntax: typeKind={}, raw=0x{x}\n", .{ expr.typeKind(), expr.raw });
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
        @"LET*",
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
        setf,
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
        declaim,
        proclaim,
        @"return-from",
        @"unwind-protect",
        @"catch",
        throw,
        @"handler-case",
        signal,
        @"handler-bind",
        @"restart-case",
        @"invoke-restart",
        @"find-restart",
        tagbody,
        go,
        progv,
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
        macrolet,
        @"symbol-macrolet",
        @"destructuring-bind",
        // Compile-time evaluation
        @"eval-when",
        @"load-time-value",
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
        @"call-next-method",
        @"next-method-p",
        @"define-method-combination",
        @"method-qualifiers",
        @"method-specializers",
        @"method-function",
        @"generic-function-methods",
        @"generic-function-lambda-list",
        @"generic-function-name",
        @"slot-boundp",
        @"slot-makunbound",
        @"class-name",
        @"find-class",
        @"class-direct-superclasses",
        @"class-precedence-list",
        @"class-direct-slots",
        @"class-slots",
        @"slot-definition-name",
        @"slot-definition-initform",
        @"slot-definition-initargs",
        @"slot-definition-readers",
        @"slot-definition-writers",
        @"slot-definition-allocation",
        @"slot-definition-type",
    };

    /// Comptime dispatch table for special forms
    pub const special_forms = std.StaticStringMap(SpecialForm).initComptime(.{
        .{ "IF", .@"if" },
        .{ "LET", .let },
        .{ "LETREC", .letrec },
        .{ "LET*", .@"LET*" },
        .{ "COND", .cond },
        .{ "PROGN", .progn },
        .{ "BEGIN", .begin },
        .{ "FLET", .flet },
        .{ "LABELS", .labels },
        .{ "LAMBDA", .lambda },
        .{ "AND", .@"and" },
        .{ "OR", .@"or" },
        .{ "FUNCALL", .funcall },
        .{ "APPLY", .apply },
        .{ "SETQ", .setq },
        .{ "SETF", .setf },
        .{ "QUOTE", .quote },
        .{ "FUNCTION", .function },
        .{ "QUASIQUOTE", .quasiquote },
        .{ "WHILE", .@"while" },
        .{ "LOOP", .loop },
        .{ "DEFINE", .define },
        .{ "DEFVAR", .defvar },
        .{ "DEFUN", .defun },
        .{ "THE", .the },
        .{ "DECLARE", .declare },
        .{ "DECLAIM", .declaim },
        .{ "PROCLAIM", .proclaim },
        .{ "BLOCK", .block },
        .{ "RETURN-FROM", .@"return-from" },
        .{ "UNWIND-PROTECT", .@"unwind-protect" },
        .{ "CATCH", .@"catch" },
        .{ "THROW", .throw },
        .{ "HANDLER-CASE", .@"handler-case" },
        .{ "SIGNAL", .signal },
        .{ "HANDLER-BIND", .@"handler-bind" },
        .{ "RESTART-CASE", .@"restart-case" },
        .{ "INVOKE-RESTART", .@"invoke-restart" },
        .{ "FIND-RESTART", .@"find-restart" },
        .{ "TAGBODY", .tagbody },
        .{ "GO", .go },
        .{ "PROGV", .progv },
        .{ "VALUES", .values },
        .{ "VALUES-LIST", .@"values-list" },
        .{ "MULTIPLE-VALUE-BIND", .@"multiple-value-bind" },
        .{ "MULTIPLE-VALUE-CALL", .@"multiple-value-call" },
        .{ "MULTIPLE-VALUE-LIST", .@"multiple-value-list" },
        // ADT support
        .{ "DEFTYPE", .deftype },
        .{ "MATCH", .match },
        // Macro support
        .{ "DEFMACRO", .defmacro },
        .{ "MACROLET", .macrolet },
        .{ "SYMBOL-MACROLET", .@"symbol-macrolet" },
        .{ "DESTRUCTURING-BIND", .@"destructuring-bind" },
        // Compile-time evaluation
        .{ "EVAL-WHEN", .@"eval-when" },
        .{ "LOAD-TIME-VALUE", .@"load-time-value" },
        // Packages
        .{ "DEFPACKAGE", .defpackage },
        .{ "IN-PACKAGE", .@"in-package" },
        .{ "EXPORT", .@"export" },
        .{ "USE-PACKAGE", .@"use-package" },
        // Structure definition
        .{ "DEFSTRUCT", .defstruct },
        // CLOS
        .{ "DEFCLASS", .defclass },
        .{ "MAKE-INSTANCE", .@"make-instance" },
        .{ "SLOT-VALUE", .@"slot-value" },
        .{ "DEFGENERIC", .defgeneric },
        .{ "DEFMETHOD", .defmethod },
        .{ "CALL-NEXT-METHOD", .@"call-next-method" },
        .{ "NEXT-METHOD-P", .@"next-method-p" },
        .{ "DEFINE-METHOD-COMBINATION", .@"define-method-combination" },
        .{ "METHOD-QUALIFIERS", .@"method-qualifiers" },
        .{ "METHOD-SPECIALIZERS", .@"method-specializers" },
        .{ "METHOD-FUNCTION", .@"method-function" },
        .{ "GENERIC-FUNCTION-METHODS", .@"generic-function-methods" },
        .{ "GENERIC-FUNCTION-LAMBDA-LIST", .@"generic-function-lambda-list" },
        .{ "GENERIC-FUNCTION-NAME", .@"generic-function-name" },
        .{ "SLOT-BOUNDP", .@"slot-boundp" },
        .{ "SLOT-MAKUNBOUND", .@"slot-makunbound" },
        .{ "CLASS-NAME", .@"class-name" },
        .{ "FIND-CLASS", .@"find-class" },
        .{ "CLASS-DIRECT-SUPERCLASSES", .@"class-direct-superclasses" },
        .{ "CLASS-PRECEDENCE-LIST", .@"class-precedence-list" },
        .{ "CLASS-DIRECT-SLOTS", .@"class-direct-slots" },
        .{ "CLASS-SLOTS", .@"class-slots" },
        .{ "SLOT-DEFINITION-NAME", .@"slot-definition-name" },
        .{ "SLOT-DEFINITION-INITFORM", .@"slot-definition-initform" },
        .{ "SLOT-DEFINITION-INITARGS", .@"slot-definition-initargs" },
        .{ "SLOT-DEFINITION-READERS", .@"slot-definition-readers" },
        .{ "SLOT-DEFINITION-WRITERS", .@"slot-definition-writers" },
        .{ "SLOT-DEFINITION-ALLOCATION", .@"slot-definition-allocation" },
        .{ "SLOT-DEFINITION-TYPE", .@"slot-definition-type" },
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
                    .@"LET*" => self.compileLetStarWithTail(tail, env, in_tail),
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
                    .setf => self.compileSetf(tail, env),
                    .quote => self.compileQuote(tail),
                    .function => self.compileFunction(tail, env),
                    .quasiquote => self.compileQuasiquote(tail, env),
                    .@"while" => self.compileWhile(tail, env),
                    .loop => self.compileLoopSpecial(expr, head, tail, env, in_tail),
                    .define => self.compileDefine(tail, env),
                    .defvar => self.compileDefvar(tail, env),
                    .defun => self.compileDefun(tail, env),
                    .the => self.compileThe(tail, env),
                    .declare => self.compileDeclare(tail),
                    .declaim => self.compileDeclaim(tail),
                    .proclaim => self.compileProclaim(tail, env),
                    .@"return-from" => self.compileReturnFrom(tail, env),
                    .throw => self.compileThrow(tail, env),
                    .signal => self.compileSignal(tail, env),
                    .@"handler-bind" => self.compileHandlerBind(tail, env),
                    .@"restart-case" => self.compileRestartCase(tail, env),
                    .@"invoke-restart" => self.compileInvokeRestart(tail, env),
                    .@"find-restart" => self.compileFindRestart(tail, env),
                    .tagbody => self.compileTagbody(tail, env),
                    .go => self.compileGo(tail),
                    .progv => self.compileProgv(tail, env),
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
                    .macrolet => self.compileMacrolet(tail, env),
                    .@"symbol-macrolet" => self.compileSymbolMacrolet(tail, env),
                    .@"destructuring-bind" => self.compileDestructuringBind(tail, env),
                    // Compile-time evaluation
                    .@"eval-when" => self.compileEvalWhen(tail, env),
                    .@"load-time-value" => self.compileLoadTimeValue(tail, env),
                    // Packages
                    .defpackage => self.compileDefpackage(tail),
                    .@"in-package" => self.compileInPackage(tail, env),
                    .@"export" => self.compileExport(tail, env),
                    .@"use-package" => self.compileUsePackage(tail, env),
                    // Structure definition
                    .defstruct => self.compileDefstruct(tail, env),
                    // CLOS
                    .defclass => self.compileDefclass(tail, env),
                    .@"make-instance" => self.compileMakeInstance(tail, env),
                    .@"slot-value" => self.compileSlotValue(tail, env),
                    .defgeneric => self.compileDefgeneric(tail, env),
                    .defmethod => self.compileDefmethod(tail, env),
                    .@"call-next-method" => self.compileCallNextMethod(tail, env),
                    .@"next-method-p" => self.compileNextMethodP(env),
                    .@"define-method-combination" => self.compileDefineMethodCombination(tail, env),
                    .@"method-qualifiers" => self.compileMethodQualifiers(tail, env),
                    .@"method-specializers" => self.compileMethodSpecializers(tail, env),
                    .@"method-function" => self.compileMethodFunction(tail, env),
                    .@"generic-function-methods" => self.compileGenericFunctionMethods(tail, env),
                    .@"generic-function-lambda-list" => self.compileGenericFunctionLambdaList(tail, env),
                    .@"generic-function-name" => self.compileGenericFunctionName(tail, env),
                    .@"slot-boundp" => self.compileSlotBoundp(tail, env),
                    .@"slot-makunbound" => self.compileSlotMakunbound(tail, env),
                    .@"class-name" => self.compileUnaryPrim(tail, env, .class_name),
                    .@"find-class" => self.compileFindClass(tail, env),
                    .@"class-direct-superclasses" => self.compileUnaryPrim(tail, env, .class_direct_superclasses),
                    .@"class-precedence-list" => self.compileUnaryPrim(tail, env, .class_precedence_list),
                    .@"class-direct-slots" => self.compileUnaryPrim(tail, env, .class_direct_slots),
                    .@"class-slots" => self.compileUnaryPrim(tail, env, .class_slots),
                    .@"slot-definition-name" => self.compileUnaryPrim(tail, env, .slot_definition_name),
                    .@"slot-definition-initform" => self.compileUnaryPrim(tail, env, .slot_definition_initform),
                    .@"slot-definition-initargs" => self.compileUnaryPrim(tail, env, .slot_definition_initargs),
                    .@"slot-definition-readers" => self.compileUnaryPrim(tail, env, .slot_definition_readers),
                    .@"slot-definition-writers" => self.compileUnaryPrim(tail, env, .slot_definition_writers),
                    .@"slot-definition-allocation" => self.compileUnaryPrim(tail, env, .slot_definition_allocation),
                    .@"slot-definition-type" => self.compileUnaryPrim(tail, env, .slot_definition_type),
                };
            }

            // Check for macros - expand at compile time if VM is available
            if (self.lookupMacroDef(head)) |macro_def| {
                if (self.vm) |vm| {
                    const expanded = self.expandMacro(macro_def, tail, expr, vm) catch |err| return err;
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

    fn compileGlobalSymbolRef(self: *Compiler, sym_val: Value) anyerror!*Ir {
        if (!sym_val.isSymbol()) return error.InvalidSyntax;

        const sym = sym_val.toPtr(Symbol);
        const name = sym.getName();

        // Check globals - use qualified name if symbol has package
        var qual_buf: [256]u8 = undefined;
        const q = try self.getQualifiedName(sym, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const qname = q.name;

        // Resolve existing globals before creating a forward-reference slot.
        // This avoids creating package-local NIL slots for imported CL symbols.
        if (self.globals.lookup(qname)) |idx| {
            return try self.builder.globalRef(qname, idx);
        }
        if (self.globals.lookup(name)) |idx| {
            return try self.builder.globalRef(name, idx);
        }

        const prefixes = [_][]const u8{ "COMMON-LISP:", "CL:", "CL-USER:", "COMMON-LISP-USER:" };
        var full_buf: [640]u8 = undefined;
        for (prefixes) |prefix| {
            if (prefix.len + name.len > full_buf.len) continue;
            @memcpy(full_buf[0..prefix.len], prefix);
            @memcpy(full_buf[prefix.len .. prefix.len + name.len], name);
            const candidate = full_buf[0 .. prefix.len + name.len];
            if (self.globals.lookup(candidate)) |idx| {
                return try self.builder.globalRef(candidate, idx);
            }
        }

        // Allow forward references: allocate slot if still not found.
        const idx = try self.globals.define(qname);
        return try self.builder.globalRef(qname, idx);
    }

    fn patchChunkClosureIndices(chunk: *Chunk, base: u16) void {
        const code = chunk.getCode();
        var i: usize = 0;
        while (i + 1 < code.len) {
            const low: u16 = code[i];
            const high: u16 = code[i + 1];
            const opcode: u16 = low | (high << 8);
            const op: bytecode.Op = @enumFromInt(opcode);
            const size = op.operandSize();

            if (op == .make_closure) {
                const rel_idx = std.mem.readInt(u16, code[i + 2 ..][0..2], .little);
                std.mem.writeInt(u16, code[i + 2 ..][0..2], rel_idx + base, .little);
            }

            i += 2 + size;
        }
    }

    /// Expand a macro by calling its expander function with the arguments
    fn expandMacro(self: *Compiler, macro_def: Value, args: Value, whole_form: Value, vm: *Vm) !Value {
        const heap = if (self.heap) |val| val else return error.InvalidSyntax;
        try self.refreshBuiltins();
        const b = if (self.builtins) |val| val else return error.InvalidSyntax;

        // If macro_def is a closure (set via (setf (macro-function ...) fn)),
        // call it directly with (whole-form nil) per CL spec
        if (macro_def.isClosure()) {
            const call_args = [_]Value{ whole_form, Value.nil };
            return vm.callFromStack(macro_def, &call_args) catch |err| return err;
        }

        // macro_def is ((params...) body...)
        // Transform destructured params before creating lambda
        if (!macro_def.isCons()) return error.InvalidSyntax;
        const transformed = try self.transformDestructuredParams(macro_def);

        const def_cons = transformed.toPtr(Cons);
        var params = def_cons.car;
        var wrapped_body = def_cons.cdr;

        // Check for &whole at the beginning of params and extract the var name
        var whole_var: ?Value = null;
        var whole_pattern: ?Value = null;
        if (params.isCons()) {
            const first_cons = params.toPtr(Cons);
            if (first_cons.car.raw == b.@"&whole".raw) {
                // &whole var - extract var name and skip both
                if (first_cons.cdr.isCons()) {
                    const rest = first_cons.cdr.toPtr(Cons);
                    switch (rest.car.typeKind()) {
                        .symbol => whole_var = rest.car,
                        .cons => {
                            whole_pattern = rest.car;
                            whole_var = try prims.gensym(heap, null);
                        },
                        else => return error.InvalidSyntax,
                    }
                    params = rest.cdr; // Skip &whole and var
                } else {
                    return error.InvalidSyntax;
                }
            }
        }

        // Check for &environment in params, extract var name, and remove it
        var env_var: ?Value = null;
        var new_params = Value.nil;
        var param_tail: ?*Cons = null;
        var p = params;
        while (p.isCons()) {
            const pc = p.toPtr(Cons);
            if (pc.car.raw == b.@"&environment".raw) {
                // Extract var name following &environment and skip both
                if (pc.cdr.isCons()) {
                    const env_rest = pc.cdr.toPtr(Cons);
                    env_var = env_rest.car; // The variable name
                    p = env_rest.cdr; // Skip &environment and var
                } else {
                    p = pc.cdr;
                }
                continue;
            }
            // Keep this param
            const new_cell = try heap.allocCons(pc.car, Value.nil);
            const new_cons = new_cell.toPtr(Cons);
            if (param_tail) |t| {
                t.cdr = new_cell;
            } else {
                new_params = new_cell;
            }
            param_tail = new_cons;
            p = pc.cdr;
        }
        params = new_params;

        // Build lambda params: (whole-var env-var regular-params...) if needed
        var final_params = params;
        if (env_var) |ev| {
            // Add env var at the beginning (will receive nil)
            final_params = try heap.allocCons(ev, final_params);
        }
        if (whole_var) |wv| {
            // Add whole var at the beginning
            final_params = try heap.allocCons(wv, final_params);
        }

        if (whole_pattern) |pat| {
            const wv = whole_var orelse return error.InvalidSyntax;
            const db_sym = try heap.intern("destructuring-bind");
            const progn_sym = try heap.intern("progn");
            const progn_body = try heap.allocCons(progn_sym, wrapped_body);
            const progn_cell = try heap.allocCons(progn_body, Value.nil);
            const whole_cell = try heap.allocCons(wv, progn_cell);
            const pat_cell = try heap.allocCons(pat, whole_cell);
            const db_form = try heap.allocCons(db_sym, pat_cell);
            wrapped_body = try heap.allocCons(db_form, Value.nil);
        }

        // Build (lambda (params...) body...) with all body forms
        const lambda_sym = try heap.intern("lambda");
        const params_body = try heap.allocCons(final_params, wrapped_body);
        const lambda_list = try heap.allocCons(lambda_sym, params_body);

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Compile the lambda to get a closure
        var macro_compiler = try Compiler.initWithHeapPreservePackage(arena_alloc, vm);
        defer macro_compiler.deinit();
        // Share macro table so nested macros (like prog1) work in macro bodies
        var iter = self.macro_table.iterator();
        while (iter.next()) |entry| {
            try macro_compiler.macro_table.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        // Share global bindings so builtins like 'list' are accessible
        // CRITICAL: Copy both name AND index so references resolve correctly
        var giter = self.globals.bindings.iterator();
        while (giter.next()) |entry| {
            const name = entry.key_ptr.*;
            const idx = entry.value_ptr.*;
            const name_copy = try macro_compiler.globals.allocator.dupe(u8, name);
            try macro_compiler.globals.bindings.put(name_copy, idx);
            if (idx >= macro_compiler.globals.next_index) {
                macro_compiler.globals.next_index = idx + 1;
            }
        }

        var empty_env = Env.init(arena_alloc, null);
        defer empty_env.deinit();
        const lambda_ir = try macro_compiler.compile(lambda_list, &empty_env);

        // Emit to bytecode
        var emitter = Emitter.initWithHeap(arena_alloc, heap);
        emitter.speed = self.optimize_current.speed;
        emitter.safety = self.optimize_current.safety;
        defer emitter.deinit();
        try emitter.emit(lambda_ir);

        // Get child chunks and main chunk (all GC-managed Values)
        const child_chunks = try emitter.getChildChunks();

        const chunk_val = try emitter.finalize();

        const saved_state = vm_mod.State.save(vm);
        const saved_env = vm.global_env;
        const saved_ext = vm.ext_roots;
        const saved_pool = vm.chunk_pool;

        const RootPair = struct { key: Value, val: Value };
        var macro_entries = std.ArrayList(RootPair){};
        defer macro_entries.deinit(self.allocator);
        var macro_iter = self.macro_table.iterator();
        while (macro_iter.next()) |entry| {
            try macro_entries.append(self.allocator, .{
                .key = entry.key_ptr.*,
                .val = entry.value_ptr.*,
            });
        }

        var symbol_macro_entries = std.ArrayList(RootPair){};
        defer symbol_macro_entries.deinit(self.allocator);
        var sym_iter = self.symbol_macros.iterator();
        while (sym_iter.next()) |entry| {
            try symbol_macro_entries.append(self.allocator, .{
                .key = entry.key_ptr.*,
                .val = entry.value_ptr.*,
            });
        }

        const root_chunk_idx = saved_pool.len;
        const root_closure_idx = saved_pool.len + 1;
        const root_args_idx = saved_pool.len + 2;
        const root_whole_idx = saved_pool.len + 3;
        const root_env_idx = saved_pool.len + 4;
        const root_macro_start = saved_pool.len + 5;
        const root_symbol_start = root_macro_start + (macro_entries.items.len * 2);
        const macro_roots = try self.allocator.alloc(
            Value,
            saved_pool.len + 5 + (macro_entries.items.len * 2) + (symbol_macro_entries.items.len * 2),
        );
        defer self.allocator.free(macro_roots);
        const chunk_ptrs = try self.allocator.alloc(*Chunk, saved_pool.len + child_chunks.len);
        defer self.allocator.free(chunk_ptrs);
        for (saved_pool, 0..) |ptr, i| {
            macro_roots[i] = Value.makeChunk(ptr);
            chunk_ptrs[i] = ptr;
        }
        macro_roots[root_chunk_idx] = chunk_val;
        macro_roots[root_closure_idx] = Value.nil;
        macro_roots[root_args_idx] = args;
        macro_roots[root_whole_idx] = whole_form;
        macro_roots[root_env_idx] = Value.nil;
        var root_idx = root_macro_start;
        for (macro_entries.items) |entry| {
            macro_roots[root_idx] = entry.key;
            macro_roots[root_idx + 1] = entry.val;
            root_idx += 2;
        }
        for (symbol_macro_entries.items) |entry| {
            macro_roots[root_idx] = entry.key;
            macro_roots[root_idx + 1] = entry.val;
            root_idx += 2;
        }
        const macro_chunk_base: u16 = @intCast(saved_pool.len);
        if (macro_chunk_base > 0) {
            patchChunkClosureIndices(macro_roots[root_chunk_idx].toPtr(Chunk), macro_chunk_base);
        }

        for (child_chunks, 0..) |cv, i| {
            const child_ptr = cv.toPtr(Chunk);
            if (macro_chunk_base > 0) {
                patchChunkClosureIndices(child_ptr, macro_chunk_base);
            }
            chunk_ptrs[saved_pool.len + i] = child_ptr;
        }

        vm.setExtRoots(macro_roots);
        vm.setGlobalEnv(&self.globals);
        vm.setChunkPool(chunk_ptrs);
        defer {
            // Keep the previous chunk pool slice up-to-date across GC that may run
            // while we temporarily replace vm.chunk_pool for macro expansion.
            for (saved_pool, 0..) |*slot, i| {
                const v = macro_roots[i];
                if (!v.isNil()) {
                    slot.* = v.toPtr(Chunk);
                }
            }

            vm.setExtRoots(saved_ext);
            vm.global_env = saved_env;
            saved_state.restore(vm);
        }

        const chunk_ptr = macro_roots[root_chunk_idx].toPtr(Chunk);
        const compile_closure = try heap.allocClosure(
            macro_roots[root_chunk_idx],
            chunk_ptr.arity,
            &[_]Value{},
        );
        macro_roots[root_closure_idx] = try vm.callFromStackAt(vm.sp, compile_closure, &[_]Value{});

        const closure_val = macro_roots[root_closure_idx];
        if (!closure_val.isClosure()) return error.InvalidSyntax;

        if (env_var != null) {
            macro_roots[root_env_idx] = try heap.allocMacroEnv();
        }

        var call_args = std.ArrayList(Value){};
        defer call_args.deinit(self.allocator);

        if (whole_var != null) {
            try call_args.append(self.allocator, macro_roots[root_whole_idx]);
        }
        if (env_var != null) {
            try call_args.append(self.allocator, macro_roots[root_env_idx]);
        }

        var arg_list = macro_roots[root_args_idx];
        while (arg_list.isCons()) {
            const arg_cons = arg_list.toPtr(Cons);
            try call_args.append(self.allocator, arg_cons.car);
            arg_list = arg_cons.cdr;
        }

        const call_result = vm.callFromStack(closure_val, call_args.items) catch |err| {
            try self.restoreMacroTablesFromRoots(
                macro_roots,
                root_macro_start,
                macro_entries.items.len,
                root_symbol_start,
                symbol_macro_entries.items.len,
            );
            try self.refreshBuiltins();
            return err;
        };

        try self.restoreMacroTablesFromRoots(
            macro_roots,
            root_macro_start,
            macro_entries.items.len,
            root_symbol_start,
            symbol_macro_entries.items.len,
        );
        try self.refreshBuiltins();
        return call_result;
    }

    fn restoreMacroTablesFromRoots(
        self: *Compiler,
        roots: []const Value,
        macro_start: usize,
        macro_count: usize,
        symbol_start: usize,
        symbol_count: usize,
    ) !void {
        self.macro_table.clearRetainingCapacity();
        var i: usize = 0;
        while (i < macro_count) : (i += 1) {
            const base = macro_start + (i * 2);
            try self.macro_table.put(roots[base], roots[base + 1]);
        }

        self.symbol_macros.clearRetainingCapacity();
        i = 0;
        while (i < symbol_count) : (i += 1) {
            const base = symbol_start + (i * 2);
            try self.symbol_macros.put(roots[base], roots[base + 1]);
        }
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
                try then_occ.narrowed.put(info.var_name, info.narrowed_type);

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
                    try else_occ.narrowed.put(info.var_name, else_ty);

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

    fn symLikeName(val: Value) ?[]const u8 {
        return switch (val.typeKind()) {
            .symbol => val.toPtr(Symbol).getName(),
            .keyword => val.toPtr(runtime.Keyword).getName(),
            .t => "t",
            .nil => "nil",
            else => null,
        };
    }

    const KeyParamSpec = struct {
        keyword_name: []const u8,
        param_name: []const u8,
        param_sym: ?Value,
        default_expr: ?Value,
    };

    fn parseKeyParamSpec(item: Value) ?KeyParamSpec {
        switch (item.typeKind()) {
            .symbol, .keyword, .t, .nil => {
                const name = symLikeName(item) orelse return null;
                return .{
                    .keyword_name = name,
                    .param_name = name,
                    .param_sym = if (item.typeKind() == .symbol) item else null,
                    .default_expr = null,
                };
            },
            .cons => {
                const item_cons = item.toPtr(Cons);
                const param_spec = item_cons.car;
                const default_expr = if (item_cons.cdr.isCons())
                    item_cons.cdr.toPtr(Cons).car
                else
                    null;

                switch (param_spec.typeKind()) {
                    .symbol, .keyword, .t, .nil => {
                        const name = symLikeName(param_spec) orelse return null;
                        return .{
                            .keyword_name = name,
                            .param_name = name,
                            .param_sym = if (param_spec.typeKind() == .symbol) param_spec else null,
                            .default_expr = default_expr,
                        };
                    },
                    .cons => {
                        const spec_cons = param_spec.toPtr(Cons);
                        if (!spec_cons.cdr.isCons()) return null;
                        const var_cons = spec_cons.cdr.toPtr(Cons);
                        const kw_designator = spec_cons.car;
                        const var_designator = var_cons.car;

                        const keyword_name = switch (kw_designator.typeKind()) {
                            .keyword => kw_designator.toPtr(runtime.Keyword).getName(),
                            .symbol, .t, .nil => symLikeName(kw_designator) orelse return null,
                            else => return null,
                        };
                        const param_name = symLikeName(var_designator) orelse return null;
                        return .{
                            .keyword_name = keyword_name,
                            .param_name = param_name,
                            .param_sym = if (var_designator.typeKind() == .symbol) var_designator else null,
                            .default_expr = default_expr,
                        };
                    },
                    else => return null,
                }
            },
            else => return null,
        }
    }

    fn compileLambdaCore(self: *Compiler, args: Value, env: *const Env, return_type: ?Value) anyerror!*Ir {
        // (lambda (params...) body)
        // Params can be: symbol for untyped, (symbol type) for typed
        const static = struct {
            var counter: u32 = 0;
        };
        static.counter += 1;
        // std.debug.print("compileLambdaCore #{d}\n", .{static.counter});
        if (!args.isCons()) {
            // std.debug.print("compileLambdaCore: args is not cons\n", .{});
            return error.InvalidLambda;
        }

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

        var aux_bindings = std.ArrayList(Ir.Binding){};
        defer aux_bindings.deinit(self.allocator);

        // Bind params as we parse to preserve symbol identity (package) and avoid
        // stashing GC-movable pointers into auxiliary arrays/maps.
        var lambda_env = Env.init(self.allocator, env);
        defer lambda_env.deinit();
        const saved_optimize = self.optimize_current;
        self.optimize_current = self.effectiveOptimizeForEnv(env);
        defer self.optimize_current = saved_optimize;
        lambda_env.optimize = self.optimize_current;

        var rest_param: ?[]const u8 = null;
        var allow_other_keys = false;
        var in_optional = false;
        var in_key = false;
        var in_aux = false;
        var param_list = params_expr;
        while (param_list.isCons()) {
            const param_cons = param_list.toPtr(Cons);
            const param_item = param_cons.car;

            switch (param_item.typeKind()) {
                .symbol, .keyword, .t, .nil => {
                    const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
                    const marker = self.canonicalBuiltinSymbol(param_item);

                    // Check for &rest/&body keyword (use symbol identity)
                    if (marker.raw == b.@"&rest".raw or marker.raw == b.@"&body".raw) {
                        // Next element is the rest parameter name
                        if (!param_cons.cdr.isCons()) return error.InvalidLambda;
                        const rest_cons = param_cons.cdr.toPtr(Cons);
                        const rest_name_raw = if (symLikeName(rest_cons.car)) |name| name else return error.InvalidLambda;
                        const rest_name = try self.allocator.dupe(u8, rest_name_raw);
                        rest_param = rest_name;
                        if (rest_cons.car.typeKind() == .symbol) {
                            _ = try lambda_env.bindSym(rest_cons.car);
                        } else {
                            _ = try lambda_env.bindName(rest_name);
                        }
                        break; // &rest must be last
                    }

                    // Check for &optional keyword (use symbol identity)
                    if (marker.raw == b.@"&optional".raw) {
                        in_optional = true;
                        in_key = false;
                        param_list = param_cons.cdr;
                        continue;
                    }

                    // Check for &key keyword (use symbol identity)
                    if (marker.raw == b.@"&key".raw) {
                        in_key = true;
                        in_optional = false;
                        in_aux = false;
                        param_list = param_cons.cdr;
                        continue;
                    }

                    // Check for &allow-other-keys keyword (use symbol identity)
                    if (marker.raw == b.@"&allow-other-keys".raw) {
                        allow_other_keys = true;
                        param_list = param_cons.cdr;
                        continue;
                    }

                    // Check for &aux keyword (use symbol identity)
                    if (marker.raw == b.@"&aux".raw) {
                        in_aux = true;
                        in_key = false;
                        in_optional = false;
                        param_list = param_cons.cdr;
                        continue;
                    }

                    const name_raw = if (symLikeName(param_item)) |sym_name| sym_name else return error.InvalidLambda;
                    const name = try self.allocator.dupe(u8, name_raw);

                    if (in_aux) {
                        // Aux variable with nil default
                        const nil_ir = try self.builder.lit(Value.nil);
                        const idx: u16 = @intCast(aux_bindings.items.len);
                        try aux_bindings.append(self.allocator, .{
                            .name = name,
                            .value = nil_ir,
                            .index = idx,
                        });
                        if (param_item.typeKind() == .symbol) {
                            _ = try lambda_env.bindSym(param_item);
                        } else {
                            _ = try lambda_env.bindName(name);
                        }
                    } else if (in_key) {
                        // Key parameter with nil default, keyword = name
                        try key_params.append(self.allocator, .{
                            .keyword = name,
                            .name = name,
                            .default = null,
                        });
                        if (param_item.typeKind() == .symbol) {
                            _ = try lambda_env.bindSym(param_item);
                        } else {
                            _ = try lambda_env.bindName(name);
                        }
                    } else if (in_optional) {
                        // Optional parameter with nil default
                        try optional_params.append(self.allocator, .{
                            .name = name,
                            .default = null,
                        });
                        if (param_item.typeKind() == .symbol) {
                            _ = try lambda_env.bindSym(param_item);
                        } else {
                            _ = try lambda_env.bindName(name);
                        }
                    } else {
                        // Untyped parameter: just a symbol
                        const idx = if (param_item.typeKind() == .symbol)
                            try lambda_env.bindSym(param_item)
                        else
                            try lambda_env.bindName(name);
                        try params.append(self.allocator, name);
                        try typed_params.append(self.allocator, .{ .name = name, .type_sym = null, .idx = idx });
                    }
                },
                .cons => {
                    const typed = param_item.toPtr(Cons);
                    const typed_name = symLikeName(typed.car);
                    if (!in_key and typed_name == null) return error.InvalidLambda;
                    const name = if (typed_name) |sym_name| try self.allocator.dupe(u8, sym_name) else "";

                    if (in_aux) {
                        // Aux variable: (name init-expr)
                        // Compile init in parent env (not lambda env)
                        var init_ir: *const Ir = try self.builder.lit(Value.nil);
                        if (typed.cdr.isCons()) {
                            const init_cons = typed.cdr.toPtr(Cons);
                            init_ir = try self.compile(init_cons.car, env);
                        }
                        const idx: u16 = @intCast(aux_bindings.items.len);
                        try aux_bindings.append(self.allocator, .{
                            .name = name,
                            .value = init_ir,
                            .index = idx,
                        });
                        if (typed.car.typeKind() == .symbol) {
                            _ = try lambda_env.bindSym(typed.car);
                        } else {
                            _ = try lambda_env.bindName(name);
                        }
                    } else if (in_key) {
                        // Key parameter supports both (var default) and
                        // ((:keyword var) default) CL lambda-list syntax.
                        const spec = parseKeyParamSpec(param_item) orelse return error.InvalidLambda;
                        const keyword_name = try self.allocator.dupe(u8, spec.keyword_name);
                        const param_name = try self.allocator.dupe(u8, spec.param_name);
                        var default_ir: ?*const Ir = null;
                        if (spec.default_expr) |default_expr| {
                            default_ir = try self.compile(default_expr, env);
                        }
                        try key_params.append(self.allocator, .{
                            .keyword = keyword_name,
                            .name = param_name,
                            .default = default_ir,
                        });
                        if (spec.param_sym) |param_sym| {
                            _ = try lambda_env.bindSym(param_sym);
                        } else {
                            _ = try lambda_env.bindName(param_name);
                        }
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
                        if (typed.car.typeKind() == .symbol) {
                            _ = try lambda_env.bindSym(typed.car);
                        } else {
                            _ = try lambda_env.bindName(name);
                        }
                    } else {
                        // Typed parameter: (name type-expr)
                        if (!typed.cdr.isCons()) return error.InvalidLambda;
                        const type_val = typed.cdr.toPtr(Cons).car;
                        const idx = if (typed.car.typeKind() == .symbol)
                            try lambda_env.bindSym(typed.car)
                        else
                            try lambda_env.bindName(name);
                        try params.append(self.allocator, name);
                        try typed_params.append(self.allocator, .{ .name = name, .type_sym = type_val, .idx = idx });
                    }
                },
                else => return error.InvalidLambda,
            }

            param_list = param_cons.cdr;
        }

        // Also check for rest parameter via dotted list: (a b . rest)
        if (rest_param == null and !param_list.isNil()) {
            const rest_name_raw = if (symLikeName(param_list)) |sym_name| sym_name else return error.InvalidLambda;
            const rest_name = try self.allocator.dupe(u8, rest_name_raw);
            rest_param = rest_name;
            if (param_list.typeKind() == .symbol) {
                _ = try lambda_env.bindSym(param_list);
            } else {
                _ = try lambda_env.bindName(rest_name);
            }
        }

        // Filter out declare forms from body
        const filtered_body = try self.filterDeclares(body_exprs, &lambda_env);
        lambda_env.optimize = self.optimize_current;
        // Capture analysis: collect free variables before compiling body
        var capture_set = CaptureSet.init(self.allocator);
        defer capture_set.deinit();

        try self.collectFreeVars(filtered_body, &lambda_env, &capture_set);

        // Compile body (implicit progn) - body is in tail position
        var body_ir = try self.compileBodyWithTail(filtered_body, &lambda_env, true);

        // Bidirectional type checking (when enabled)
        if (self.type_checking_enabled) {
            try self.checkLambdaTypes(typed_params.items, return_type, body_ir);
        }

        // Prepend type assertions for typed parameters
        var assertions = std.ArrayList(*Ir){};
        defer assertions.deinit(self.allocator);

        for (typed_params.items) |tp| {
            const param_name = tp.name;
            var type_sym_to_check: ?Value = null;

            if (tp.type_sym) |type_sym| {
                type_sym_to_check = type_sym;
            } else if (self.global_decls.getTypeDecl(param_name)) |decl_type| {
                type_sym_to_check = decl_type;
            }

            if (type_sym_to_check) |type_sym| {
                // At safety=0, variable references are already wrapped with
                // assert_fixnum via getTypeDecl, so entry assertions are
                // redundant. At safety>0, emit entry checks for early error.
                if (self.optimize_current.safety > 0) {
                    const var_ir = try self.builder.variable(param_name, 0, tp.idx);
                    const assert_ir = try self.makeTypeAssertionSym(var_ir, type_sym);
                    if (assert_ir) |assert_node| {
                        try assertions.append(self.allocator, assert_node);
                    }
                }
            }
        }

        // If we have assertions, wrap body in progn with assertions first
        if (assertions.items.len > 0) {
            try assertions.append(self.allocator, body_ir);
            const items = try self.allocator.dupe(*const Ir, assertions.items);
            body_ir = try self.builder.progn(items);
        }

        // Wrap body in return type assertion if specified.
        // Always generate for specialization; emitter skips checks at safety=0.
        if (return_type) |ret_type_sym| {
            const assert_ir = try self.makeTypeAssertionSym(body_ir, ret_type_sym);
            if (assert_ir) |wrapped| {
                body_ir = wrapped;
            }
        }

        // Convert captures to slice
        const captures = try self.allocator.dupe(Ir.Capture, capture_set.captures.items);

        // Copy optional params
        const opt_params = try self.allocator.dupe(Ir.OptionalParam, optional_params.items);

        // Copy key params
        const kp_params = try self.allocator.dupe(Ir.KeyParam, key_params.items);

        // If we have aux bindings, wrap body in a let
        if (aux_bindings.items.len > 0) {
            const aux_slice = try self.allocator.dupe(Ir.Binding, aux_bindings.items);
            body_ir = try self.builder.letExpr(aux_slice, body_ir);
        }

        const lam_ir = try self.builder.lambda(params.items, opt_params, kp_params, allow_other_keys, rest_param, captures, body_ir);

        // Propagate per-lambda optimize declarations into IR.
        lam_ir.lambda.speed = self.optimize_current.speed;
        lam_ir.lambda.safety = self.optimize_current.safety;

        // Preserve source lambda expression for FUNCTION-LAMBDA-EXPRESSION.
        const heap = if (self.heap) |val| val else return error.UninitializedBuiltins;
        const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
        lam_ir.lambda.lambda_expr = try heap.allocCons(b.lambda, args);
        return lam_ir;
    }

    /// Create a type assertion IR node for a given type symbol or complex type
    fn makeTypeAssertionSym(self: *Compiler, expr_ir: *Ir, type_sym: Value) !?*Ir {
        const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;

        // Handle simple type symbols by identity
        if (type_sym.isSymbol()) {
            if (type_sym.raw == b.ty_fixnum.raw or type_sym.raw == b.ty_integer.raw) return self.builder.assertFixnum(expr_ir);
            if (type_sym.raw == b.cons.raw or type_sym.raw == b.ty_cons.raw) return self.builder.assertCons(expr_ir);
            if (type_sym.raw == b.ty_symbol.raw) return self.builder.assertSymbol(expr_ir);
            if (type_sym.raw == b.string.raw or type_sym.raw == b.ty_string.raw) return self.builder.assertString(expr_ir);
            if (type_sym.raw == b.ty_vector.raw or type_sym.raw == b.vector.raw) return self.builder.assertVector(expr_ir);
            if (type_sym.raw == b.ty_closure.raw or type_sym.raw == b.ty_function.raw or type_sym.raw == b.function.raw) return self.builder.assertClosure(expr_ir);
            if (type_sym.raw == b.ty_list.raw or type_sym.raw == b.list.raw) return self.builder.assertList(expr_ir);
            if (type_sym.raw == b.@"ty_non-nil".raw) return self.builder.assertNonNil(expr_ir);
            if (type_sym.raw == b.ty_any.raw or type_sym.raw == b.ty_t.raw) return null; // any = no check
            if (type_sym.raw == b.ty_nil.raw or type_sym.raw == b.null.raw) {
                // nil type - use assertOr with just nil
                const syms = try self.allocator.alloc(Value, 1);
                syms[0] = b.ty_nil;
                return self.builder.assertOr(expr_ir, syms);
            }
            // Unknown type declarations are advisory.
            return null;
        }

        // Handle complex types: (union T1 T2 ...), etc.
        if (type_sym.isCons()) {
            const cons = type_sym.toPtr(Cons);
            if (!cons.car.isSymbol()) return null;

            // Check for (union T1 T2 ...) or (or T1 T2 ...)
            if (cons.car.raw == b.ty_union.raw or cons.car.raw == b.ty_or.raw) {
                // Collect type alternatives
                var alts = std.ArrayList(Value){};
                defer alts.deinit(self.allocator);

                var current = cons.cdr;
                while (current.isCons()) {
                    const c = current.toPtr(Cons);
                    // Each alternative is a type symbol or nil
                    switch (c.car.typeKind()) {
                        .symbol => try alts.append(self.allocator, c.car),
                        .nil => {
                            // nil in type position - use the interned nil symbol
                            try alts.append(self.allocator, b.ty_nil);
                        },
                        else => {
                            // Nested complex type declarations are advisory.
                            return null;
                        },
                    }
                    current = c.cdr;
                }

                if (alts.items.len == 0) return null;

                const syms = try self.allocator.dupe(Value, alts.items);
                return self.builder.assertOr(expr_ir, syms);
            }

            // Other complex declarations are advisory.
            return null;
        }

        return null;
    }

    /// Collect free variables in an expression
    fn bindLambdaParamForCapture(_: *Compiler, lambda_env: *Env, val: Value) error{OutOfMemory}!void {
        if (val.typeKind() == .symbol) {
            _ = try lambda_env.bindSym(val);
            return;
        }
        const name = symLikeName(val) orelse return;
        _ = try lambda_env.bindName(name);
    }

    fn isLambdaListMarker(self: *Compiler, val: Value) bool {
        const b = if (self.builtins) |builtins| builtins else return false;
        const marker = self.canonicalBuiltinSymbol(val);
        return marker.raw == b.@"&optional".raw or
            marker.raw == b.@"&rest".raw or
            marker.raw == b.@"&body".raw or
            marker.raw == b.@"&key".raw or
            marker.raw == b.@"&allow-other-keys".raw or
            marker.raw == b.@"&aux".raw or
            marker.raw == b.@"&whole".raw or
            marker.raw == b.@"&environment".raw;
    }

    fn bindLambdaParamsForCapture(self: *Compiler, params_expr: Value, lambda_env: *Env) error{OutOfMemory}!void {
        var in_optional = false;
        var in_key = false;
        var in_aux = false;

        var param_list = params_expr;
        while (param_list.isCons()) {
            const param_cons = param_list.toPtr(Cons);
            const item = param_cons.car;

            switch (item.typeKind()) {
                .symbol, .keyword, .t, .nil => {
                    const marker = self.canonicalBuiltinSymbol(item);
                    const b = if (self.builtins) |builtins| builtins else return;
                    if (marker.raw == b.@"&optional".raw) {
                        in_optional = true;
                        in_key = false;
                        in_aux = false;
                        param_list = param_cons.cdr;
                        continue;
                    }
                    if (marker.raw == b.@"&key".raw) {
                        in_optional = false;
                        in_key = true;
                        in_aux = false;
                        param_list = param_cons.cdr;
                        continue;
                    }
                    if (marker.raw == b.@"&aux".raw) {
                        in_optional = false;
                        in_key = false;
                        in_aux = true;
                        param_list = param_cons.cdr;
                        continue;
                    }
                    if (marker.raw == b.@"&allow-other-keys".raw) {
                        param_list = param_cons.cdr;
                        continue;
                    }
                    if (marker.raw == b.@"&rest".raw or marker.raw == b.@"&body".raw) {
                        if (param_cons.cdr.isCons()) {
                            const rest_cons = param_cons.cdr.toPtr(Cons);
                            try self.bindLambdaParamForCapture(lambda_env, rest_cons.car);
                        }
                        return;
                    }
                    if (!self.isLambdaListMarker(item)) {
                        try self.bindLambdaParamForCapture(lambda_env, item);
                    }
                },
                .cons => {
                    if (in_key) {
                        if (parseKeyParamSpec(item)) |spec| {
                            if (spec.param_sym) |param_sym| {
                                _ = try lambda_env.bindSym(param_sym);
                            } else {
                                _ = try lambda_env.bindName(spec.param_name);
                            }
                        }
                    } else {
                        const typed = item.toPtr(Cons);
                        if (typed.car.isSymbolLike()) {
                            try self.bindLambdaParamForCapture(lambda_env, typed.car);
                        }
                    }
                },
                else => {},
            }

            param_list = param_cons.cdr;
        }

        // Dotted rest parameter: (a b . rest)
        if (!param_list.isNil()) {
            try self.bindLambdaParamForCapture(lambda_env, param_list);
        }
    }

    fn collectNestedLambdaTransitiveCaptures(self: *Compiler, lambda_tail: Value, env: *const Env, captures: *CaptureSet) error{OutOfMemory}!void {
        if (!lambda_tail.isCons()) return;
        const lam_cons = lambda_tail.toPtr(Cons);
        const params_expr = lam_cons.car;
        const body_expr = lam_cons.cdr;

        // Build nested lambda environment so capture analysis ignores nested params.
        var nested_env = Env.init(self.allocator, env);
        defer nested_env.deinit();
        try self.bindLambdaParamsForCapture(params_expr, &nested_env);

        // Collect nested captures relative to current env, then rebase one frame out.
        var nested_captures = CaptureSet.init(self.allocator);
        defer nested_captures.deinit();
        try self.collectFreeVarsInList(body_expr, &nested_env, &nested_captures);

        for (nested_captures.captures.items) |cap| {
            if (cap.depth == 0) continue;
            try captures.addCapture(cap.name, cap.depth - 1, cap.index);
        }
    }

    fn collectFreeFunctionCapture(_: *Compiler, sym: Value, env: *const Env, captures: *CaptureSet) error{OutOfMemory}!void {
        if (!sym.isSymbol()) return;
        const sym_name = sym.toPtr(Symbol).getName();
        const fn_binding = env.lookupFunctionSym(sym) orelse {
            if (std.posix.getenv("HABU_TRACE_CAPTURE") != null) {
                std.debug.print("TRACE capture fn miss sym={s}\n", .{sym_name});
            }
            return;
        };
        if (std.posix.getenv("HABU_TRACE_CAPTURE") != null) {
            std.debug.print(
                "TRACE capture fn sym={s} depth={d} index={d}\n",
                .{ sym_name, fn_binding.depth, fn_binding.index },
            );
        }
        if (fn_binding.depth == 0) return;

        var lambda_env: ?*const Env = env;
        while (lambda_env) |e| {
            if (e.new_frame) break;
            lambda_env = e.parent;
        }
        if (lambda_env) |le| {
            if (le.parent) |lambda_parent| {
                if (lambda_parent.lookupFunctionSym(sym)) |parent_binding| {
                    if (std.posix.getenv("HABU_TRACE_CAPTURE") != null) {
                        std.debug.print(
                            "TRACE capture fn add sym={s} depth={d} index={d}\n",
                            .{ sym_name, parent_binding.depth, parent_binding.index },
                        );
                    }
                    try captures.addCapture(sym_name, parent_binding.depth, parent_binding.index);
                } else if (std.posix.getenv("HABU_TRACE_CAPTURE") != null) {
                    std.debug.print("TRACE capture fn parent miss sym={s}\n", .{sym_name});
                }
            }
        }
    }

    fn collectFreeVars(self: *Compiler, expr: Value, env: *const Env, captures: *CaptureSet) error{OutOfMemory}!void {
        if (expr.isNil() or expr.isFixnum() or expr.isBignum() or expr.isString() or expr.isString32() or expr.isKeyword() or expr.isCharacter() or expr.isMagicSymbol() or expr.isVector()) {
            return; // Literals have no free variables
        }

        if (expr.isSymbol()) {
            const sym = expr.toPtr(Symbol);
            const name = sym.getName();

            // If bound in this lambda frame (including same-frame let envs), it's not free.
            if (env.lookupSym(expr)) |binding| {
                if (binding.depth == 0) return;
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
                    if (lambda_parent.lookupSym(expr)) |binding| {
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
                const b = if (self.builtins) |val| val else return;

                if (head.raw == b.lambda.raw or head.raw == b.@"fn".raw) {
                    // Nested lambda can force this lambda to capture transitive upvalues.
                    try self.collectNestedLambdaTransitiveCaptures(tail, env, captures);
                    return;
                }

                if (head.raw == b.function.raw) {
                    // (function foo): foo can be a lexical function binding.
                    if (tail.isCons()) {
                        const fun_cons = tail.toPtr(Cons);
                        if (fun_cons.car.isSymbol()) {
                            try self.collectFreeFunctionCapture(fun_cons.car, env, captures);
                        } else {
                            try self.collectFreeVars(fun_cons.car, env, captures);
                        }
                    }
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
                                    _ = try let_env.bindSym(binding_pair.car);
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

                if (head.raw == b.quasiquote.raw) {
                    // Quasiquote: walk looking for unquoted expressions
                    if (tail.isCons()) {
                        try self.collectFreeVarsInQuasiquote(tail.toPtr(Cons).car, env, captures, 0);
                    }
                    return;
                }

                // Function position can reference lexical function namespace bindings.
                try self.collectFreeFunctionCapture(head, env, captures);
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

    /// Walk a quasiquoted form looking for unquoted expressions that contain free variables.
    /// At depth 0, (unquote x) evaluates x and (unquote-splicing x) evaluates x.
    /// At depth > 0, nested quasiquotes increment depth and unquotes decrement.
    fn collectFreeVarsInQuasiquote(self: *Compiler, expr: Value, env: *const Env, captures: *CaptureSet, depth: u32) error{OutOfMemory}!void {
        if (!expr.isCons()) return;

        const cons = expr.toPtr(Cons);
        const head = cons.car;

        if (head.isSymbol()) {
            const b = if (self.builtins) |val| val else return;
            const dh = self.canonicalBuiltinSymbol(head);

            if (dh.raw == b.unquote.raw or dh.raw == b.@"unquote-splicing".raw) {
                if (depth == 0) {
                    // At outermost level: the unquoted expression is evaluated
                    if (cons.cdr.isCons()) {
                        try self.collectFreeVars(cons.cdr.toPtr(Cons).car, env, captures);
                    }
                } else {
                    // Nested: process at depth-1
                    if (cons.cdr.isCons()) {
                        try self.collectFreeVarsInQuasiquote(cons.cdr.toPtr(Cons).car, env, captures, depth - 1);
                    }
                }
                return;
            }

            if (dh.raw == b.quasiquote.raw) {
                // Nested quasiquote: increment depth
                if (cons.cdr.isCons()) {
                    try self.collectFreeVarsInQuasiquote(cons.cdr.toPtr(Cons).car, env, captures, depth + 1);
                }
                return;
            }

            if (dh.raw == b.quote.raw) {
                // (quote ...) inside quasiquote: still need to look inside
                // because (quote (unquote x)) = ',x is a valid pattern
                if (cons.cdr.isCons()) {
                    try self.collectFreeVarsInQuasiquote(cons.cdr.toPtr(Cons).car, env, captures, depth);
                }
                return;
            }
        }

        // General list: recurse on all elements
        var current: Value = expr;
        while (current.isCons()) {
            const c = current.toPtr(Cons);
            try self.collectFreeVarsInQuasiquote(c.car, env, captures, depth);
            current = c.cdr;
        }
    }

    /// Find variables that need boxing: both mutated via set! AND captured by a lambda
    /// This pre-scans a let body to determine which bindings need automatic boxing
    fn findBoxedVars(self: *Compiler, body: Value, binding_syms: []const Value, result: *BoxingSet) error{OutOfMemory}!void {
        var mutated = std.AutoHashMap(Value, void).init(self.allocator);
        defer mutated.deinit();

        var captured = std.AutoHashMap(Value, void).init(self.allocator);
        defer captured.deinit();

        // Collect mutations and captures from body
        try self.collectMutationsAndCaptures(body, binding_syms, &mutated, &captured);

        // Intersection: names that are both mutated AND captured need boxing
        var iter = mutated.keyIterator();
        while (iter.next()) |sym| {
            if (captured.contains(sym.*)) {
                try result.add(sym.*);
            }
        }
    }

    /// Recursively collect mutations (set!) and lambda captures in an expression
    fn collectMutationsAndCaptures(
        self: *Compiler,
        expr: Value,
        binding_syms: []const Value,
        mutated: *std.AutoHashMap(Value, void),
        captured: *std.AutoHashMap(Value, void),
    ) error{OutOfMemory}!void {
        if (expr.isNil() or expr.isFixnum() or expr.isBignum() or expr.isString() or expr.isString32() or expr.isKeyword() or expr.isCharacter() or expr.isVector()) {
            return;
        }

        if (!expr.isCons()) return;

        // Builtins required for symbol identity comparison
        const b = if (self.builtins) |val| val else return;

        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        if (head.isSymbol()) {
            // Check for (setq var ...) or (setf var ...) — both mutate variables
            if (head.raw == b.setq.raw or head.raw == b.setf.raw) {
                if (tail.isCons()) {
                    const set_cons = tail.toPtr(Cons);
                    if (set_cons.car.isSymbol()) {
                        const var_sym = set_cons.car;
                        // Check if this is one of our bindings
                        for (binding_syms) |bn| {
                            if (var_sym.eq(bn)) {
                                try mutated.put(var_sym, {});
                                break;
                            }
                        }
                    }
                    // Recurse into the value expression
                    if (set_cons.cdr.isCons()) {
                        const val_cons = set_cons.cdr.toPtr(Cons);
                        try self.collectMutationsAndCaptures(val_cons.car, binding_syms, mutated, captured);
                    }
                }
                return;
            }

            // Check for (push val var) — mutation via macro
            if (head.raw == b.push.raw) {
                if (tail.isCons()) {
                    const push_cons = tail.toPtr(Cons);
                    if (push_cons.cdr.isCons()) {
                        const place_cons = push_cons.cdr.toPtr(Cons);
                        if (place_cons.car.isSymbol()) {
                            const var_sym = place_cons.car;
                            for (binding_syms) |bn| {
                                if (var_sym.eq(bn)) {
                                    try mutated.put(var_sym, {});
                                    break;
                                }
                            }
                        }
                        // Recurse into value
                        try self.collectMutationsAndCaptures(push_cons.car, binding_syms, mutated, captured);
                    }
                }
                return;
            }

            // Check for (incf var) / (decf var) — mutation via macro
            if (head.raw == b.incf.raw or head.raw == b.decf.raw) {
                if (tail.isCons()) {
                    const inc_cons = tail.toPtr(Cons);
                    if (inc_cons.car.isSymbol()) {
                        const var_sym = inc_cons.car;
                        for (binding_syms) |bn| {
                            if (var_sym.eq(bn)) {
                                try mutated.put(var_sym, {});
                                break;
                            }
                        }
                    }
                    // Recurse into delta if present
                    try self.collectMutationsAndCapturesInList(inc_cons.cdr, binding_syms, mutated, captured);
                }
                return;
            }

            // Check for (lambda ...) - collect free vars that are our bindings
            // AND mutations inside the lambda body
            if (head.raw == b.lambda.raw or head.raw == b.@"fn".raw) {
                try self.collectLambdaCaptures(tail, binding_syms, captured);
                // Also look for mutations inside the lambda body
                if (tail.isCons()) {
                    const lam_cons = tail.toPtr(Cons);
                    // Skip params, recurse into body
                    try self.collectMutationsAndCapturesInList(lam_cons.cdr, binding_syms, mutated, captured);
                }
                return;
            }

            // Skip quote - don't analyze quoted expressions
            if (head.raw == b.quote.raw) {
                return;
            }
        }

        // Recurse into all elements of the list
        try self.collectMutationsAndCaptures(head, binding_syms, mutated, captured);
        try self.collectMutationsAndCapturesInList(tail, binding_syms, mutated, captured);
    }

    fn collectMutationsAndCapturesInList(
        self: *Compiler,
        list: Value,
        binding_syms: []const Value,
        mutated: *std.AutoHashMap(Value, void),
        captured: *std.AutoHashMap(Value, void),
    ) error{OutOfMemory}!void {
        var current = list;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            try self.collectMutationsAndCaptures(cons.car, binding_syms, mutated, captured);
            current = cons.cdr;
        }
    }

    /// Collect which of our bindings are captured by a lambda
    fn collectLambdaCaptures(self: *Compiler, lambda_args: Value, binding_syms: []const Value, captured: *std.AutoHashMap(Value, void)) error{OutOfMemory}!void {
        if (!lambda_args.isCons()) return;

        const args_cons = lambda_args.toPtr(Cons);
        const params_expr = args_cons.car;
        const body = args_cons.cdr;

        // Get lambda parameter names to exclude from captures
        var dispatch_params = std.AutoHashMap(Value, void).init(self.allocator);
        defer dispatch_params.deinit();

        var param_list = params_expr;
        while (param_list.isCons()) {
            const param_cons = param_list.toPtr(Cons);
            if (param_cons.car.isSymbol()) {
                const param_sym = param_cons.car;
                try dispatch_params.put(param_sym, {});
            }
            param_list = param_cons.cdr;
        }

        // Find free variable references in body that are our bindings
        try self.collectFreeVarRefs(body, binding_syms, &dispatch_params, captured);
    }

    /// Find references to binding names in expression (excluding params)
    fn collectFreeVarRefs(
        self: *Compiler,
        expr: Value,
        binding_syms: []const Value,
        params: *std.AutoHashMap(Value, void),
        captured: *std.AutoHashMap(Value, void),
    ) error{OutOfMemory}!void {
        if (expr.isNil() or expr.isFixnum() or expr.isBignum() or expr.isString() or expr.isString32() or expr.isKeyword() or expr.isCharacter() or expr.isVector()) {
            return;
        }

        if (expr.isSymbol()) {
            const sym = expr;
            // If it's not a param and is one of our bindings, it's captured
            if (!params.contains(sym)) {
                for (binding_syms) |bn| {
                    if (sym.eq(bn)) {
                        try captured.put(sym, {});
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
            const b = if (self.builtins) |val| val else return;
            const head = cons.car;
            if (head.raw == b.quote.raw) return;

            // Handle nested lambda - need to add its params to exclusion
            if (head.raw == b.lambda.raw or head.raw == b.@"fn".raw) {
                if (cons.cdr.isCons()) {
                    const lam_cons = cons.cdr.toPtr(Cons);
                    const lam_params = lam_cons.car;
                    const lam_body = lam_cons.cdr;

                    // Collect nested lambda params
                    var nested_params = std.AutoHashMap(Value, void).init(self.allocator);
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
                            const ps = pc.car;
                            try nested_params.put(ps, {});
                        }
                        pl = pc.cdr;
                    }

                    // Recurse with extended params
                    try self.collectFreeVarRefsInList(lam_body, binding_syms, &nested_params, captured);
                }
                return;
            }
        }

        try self.collectFreeVarRefs(cons.car, binding_syms, params, captured);
        try self.collectFreeVarRefsInList(cons.cdr, binding_syms, params, captured);
    }

    fn collectFreeVarRefsInList(
        self: *Compiler,
        list: Value,
        binding_syms: []const Value,
        params: *std.AutoHashMap(Value, void),
        captured: *std.AutoHashMap(Value, void),
    ) error{OutOfMemory}!void {
        var current = list;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            try self.collectFreeVarRefs(cons.car, binding_syms, params, captured);
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

        // Special-variable dynamic binding path.
        // Lower (let ((*var* init) ...) body) to progv so callees observe dynamic values.
        if (try self.tryCompileSpecialLet(bindings_expr, body_exprs, env, in_tail)) |special_ir| {
            return special_ir;
        }

        // First pass: collect binding names for boxing analysis
        var binding_names = std.ArrayList([]const u8){};
        defer binding_names.deinit(self.allocator);
        var binding_syms = std.ArrayList(Value){};
        defer binding_syms.deinit(self.allocator);

        var binding_list = bindings_expr;
        while (binding_list.isCons()) {
            const binding_cons = binding_list.toPtr(Cons);
            const binding = binding_cons.car;

            const bind_sym = if (binding.isSymbolLike()) blk: {
                break :blk binding;
            } else if (binding.isCons()) blk: {
                const b = binding.toPtr(Cons);
                if (!b.car.isSymbolLike()) return error.InvalidLet;
                break :blk b.car;
            } else {
                return error.InvalidLet;
            };
            const name_raw = bind_sym.getSymbolName();
            const name_copy = try self.allocator.dupe(u8, name_raw);
            try binding_names.append(self.allocator, name_copy);
            try binding_syms.append(self.allocator, bind_sym);

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
        try self.findBoxedVars(body_exprs, binding_syms.items, boxed);

        // Create let_env first so we can get indices for each binding
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();

        // First, reserve ALL slots by binding all names (so nested exprs like 'or' know
        // to use higher indices). This fixes a bug where (or ...) in a let binding
        // would reuse slot 0, overwriting earlier bindings.
        for (binding_syms.items, 0..) |sym, i| {
            const name = binding_names.items[i];
            if (sym.typeKind() == .symbol) {
                _ = try let_env.bindSym(sym);
            } else {
                _ = try let_env.bindName(name);
            }
        }

        // Create a slot-reserving environment for compiling value expressions.
        // This env has the same localCount as let_env but must NOT see let_env bindings.
        // LET value forms are evaluated in the outer lexical environment.
        var value_env = Env{
            .bindings = VarMap.init(self.allocator),
            .fn_bindings = VarMap.init(self.allocator),
            .parent = env,
            .depth = env.depth,
            .new_frame = false,
            .base_index = let_env.localCount(),
            .allocator = self.allocator,
            .optimize = env.optimize,
        };
        defer value_env.deinit();

        // Second pass: compile bindings using pre-assigned indices
        var bindings = std.ArrayList(Ir.Binding){};
        defer bindings.deinit(self.allocator);

        binding_list = bindings_expr;
        var name_idx: usize = 0;
        while (binding_list.isCons()) : (name_idx += 1) {
            const binding_cons = binding_list.toPtr(Cons);
            const binding = binding_cons.car;
            const name = binding_names.items[name_idx];
            const sym = binding_syms.items[name_idx];

            // Get the already-assigned index
            const index = blk: {
                if (sym.typeKind() == .symbol) {
                    const found = let_env.lookupSym(sym) orelse return error.InvalidLet;
                    break :blk found.index;
                }
                const found = let_env.lookupName(name) orelse return error.InvalidLet;
                break :blk found.index;
            };

            // Get value expression - compile in value_env (has reserved slots)
            var val_ir = blk: {
                if (binding.isSymbolLike()) break :blk try self.builder.lit(Value.nil);
                if (!binding.isCons()) return error.InvalidLet;
                const b = binding.toPtr(Cons);
                if (b.cdr.isNil()) break :blk try self.builder.lit(Value.nil);
                if (!b.cdr.isCons()) return error.InvalidLet;
                const val_cons = b.cdr.toPtr(Cons);
                break :blk try self.compile(val_cons.car, &value_env);
            };

            // Check for type declaration and add type assertion
            var type_sym_to_check: ?Value = null;
            if (self.global_decls.getTypeDecl(name)) |decl_type| {
                type_sym_to_check = decl_type;
            }

            if (type_sym_to_check) |type_sym| {
                // Always generate for specialization; emitter skips checks at safety=0.
                const assert_ir = try self.makeTypeAssertionSym(val_ir, type_sym);
                if (assert_ir) |wrapped| {
                    val_ir = wrapped;
                }
            }

            // If this variable needs boxing, wrap value in make-box
            if (boxed.contains(sym)) {
                const box_ir = try self.allocator.create(Ir);
                box_ir.* = .{ .make_box = .{ .operand = val_ir } };
                val_ir = box_ir;
            }

            try bindings.append(self.allocator, .{ .name = name, .value = val_ir, .index = index });

            binding_list = binding_cons.cdr;
        }

        // Set boxed_vars so that variable refs and set! use box operations
        const saved_boxed = self.boxed_vars;
        if (boxed.names.count() > 0) {
            self.boxed_vars = boxed;
        }
        // Restore on error before defer frees boxed
        errdefer self.boxed_vars = saved_boxed;

        // Process declarations from body (declare forms)
        const filtered_body = try self.filterDeclares(body_exprs, &let_env);

        // Compile body - body is in tail position if let is
        const body_ir = try self.compileBodyWithTail(filtered_body, &let_env, in_tail);

        // Restore previous boxed_vars
        self.boxed_vars = saved_boxed;

        return try self.builder.letExpr(bindings.items, body_ir);
    }

    fn isSpecialBindingSym(self: *Compiler, sym: Value) bool {
        if (!sym.isSymbolLike()) return false;
        const name = sym.getSymbolName();
        if (self.global_decls.hasDecl(name, .special)) return true;
        return name.len >= 2 and name[0] == '*' and name[name.len - 1] == '*';
    }

    fn buildIrList(self: *Compiler, items: []const *const Ir) !*Ir {
        var out = try self.builder.lit(Value.nil);
        var i = items.len;
        while (i > 0) {
            i -= 1;
            out = try self.builder.cons(items[i], out);
        }
        return out;
    }

    fn tryCompileSpecialLet(
        self: *Compiler,
        bindings_expr: Value,
        body_exprs: Value,
        env: *const Env,
        in_tail: bool,
    ) anyerror!?*Ir {
        if (!bindings_expr.isCons()) return null;

        var syms = std.ArrayList(Value){};
        defer syms.deinit(self.allocator);
        var vals = std.ArrayList(*const Ir){};
        defer vals.deinit(self.allocator);

        var bindings = bindings_expr;
        while (bindings.isCons()) {
            const bind_cons = bindings.toPtr(Cons);
            const binding = bind_cons.car;

            const sym = if (binding.isSymbolLike()) blk: {
                break :blk binding;
            } else if (binding.isCons()) blk: {
                const b = binding.toPtr(Cons);
                if (!b.car.isSymbolLike()) return null;
                break :blk b.car;
            } else {
                return null;
            };

            if (!self.isSpecialBindingSym(sym)) return null;

            const init_ir = if (binding.isSymbolLike()) blk: {
                break :blk try self.builder.lit(Value.nil);
            } else blk: {
                const b = binding.toPtr(Cons);
                if (b.cdr.isNil()) break :blk try self.builder.lit(Value.nil);
                if (!b.cdr.isCons()) return error.InvalidLet;
                break :blk try self.compile(b.cdr.toPtr(Cons).car, env);
            };

            try syms.append(self.allocator, sym);
            try vals.append(self.allocator, init_ir);
            bindings = bind_cons.cdr;
        }

        if (!bindings.isNil()) return error.InvalidLet;
        if (syms.items.len == 0) return null;

        const sym_list = try self.listFromSlice(syms.items);
        const symbols_ir = try self.builder.lit(sym_list);
        const values_ir = try self.buildIrList(vals.items);
        const body_ir = try self.compileBodyWithTail(body_exprs, env, in_tail);
        return try self.builder.progv(symbols_ir, values_ir, body_ir);
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
            const q = try self.getQualifiedName(name_sym, &qual_buf);
            defer if (q.owned) self.allocator.free(q.name);
            const name = try self.allocator.dupe(u8, q.name);

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
        for (names.items) |name| {
            self.allocator.free(name);
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
            const name = try self.allocator.dupe(u8, name_sym.getName());

            // Bind in function namespace and reserve a hidden local slot.
            const index = try flet_env.bindFunctionSym(f.car);

            // Build lambda from rest: ((params) body...) -> compile as lambda
            if (!f.cdr.isCons()) return error.InvalidLet;
            const lambda_ir = try self.compileLambda(f.cdr, env);
            if (lambda_ir.* == .lambda) {
                lambda_ir.lambda.name = f.car;
            }

            try bindings.append(self.allocator, .{ .name = name, .value = lambda_ir, .index = index });

            binding_list = binding_cons.cdr;
        }

        // Compile body in new environment
        const body_ir = try self.compileBodyWithTail(body_exprs, &flet_env, in_tail);

        return try self.builder.letExpr(bindings.items, body_ir);
    }

    fn compileLabelsWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // (labels ((fname (args) body...) ...) body)
        // Like letrec - functions can see each other and themselves.
        //
        // Closures capture values, not cells. To preserve recursive LABELS
        // semantics we lower function slots as boxes:
        //   1) initialize each slot with (make-box nil)
        //   2) set each box to the compiled closure via box_set
        //   3) compile function references as box_ref of lexical fn slots
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // LABELS introduces lexical function bindings (function namespace).
        var labels_env = Env.initLet(self.allocator, env);
        defer labels_env.deinit();

        // First pass: pre-bind all function names so lambdas can see each other.
        var names = std.ArrayList([]const u8){};
        defer names.deinit(self.allocator);

        var lambda_args = std.ArrayList(Value){};
        defer lambda_args.deinit(self.allocator);

        var indices = std.ArrayList(u16){};
        defer indices.deinit(self.allocator);

        var sym_vals = std.ArrayList(Value){};
        defer sym_vals.deinit(self.allocator);

        var binding_list = bindings_expr;
        while (binding_list.isCons()) {
            const binding_cons = binding_list.toPtr(Cons);
            const fdef = binding_cons.car;

            // Each fdef is (fname (params) body...)
            if (!fdef.isCons()) return error.InvalidLet;
            const f = fdef.toPtr(Cons);
            if (!f.car.isSymbol()) return error.InvalidLet;
            const name_sym = f.car.toPtr(Symbol);
            if (!f.cdr.isCons()) return error.InvalidLet;

            const name = try self.allocator.dupe(u8, name_sym.getName());
            const idx = try labels_env.bindFunctionSym(f.car);

            try names.append(self.allocator, name);
            try lambda_args.append(self.allocator, f.cdr);
            try indices.append(self.allocator, idx);
            try sym_vals.append(self.allocator, f.car);

            binding_list = binding_cons.cdr;
        }

        const boxed_fn = try self.allocator.create(BoxingSet);
        boxed_fn.* = BoxingSet.init(self.allocator);
        defer {
            boxed_fn.deinit();
            self.allocator.destroy(boxed_fn);
        }
        for (sym_vals.items) |sym_val| {
            try boxed_fn.add(sym_val);
        }

        const saved_boxed_fn = self.boxed_fn_syms;
        self.boxed_fn_syms = boxed_fn;
        defer self.boxed_fn_syms = saved_boxed_fn;

        // First LET stage: initialize every fn slot with a box placeholder.
        var boxed_bindings = std.ArrayList(Ir.Binding){};
        defer boxed_bindings.deinit(self.allocator);
        for (names.items, indices.items) |name, idx| {
            const nil_ir = try self.builder.lit(Value.nil);
            const make_box = try self.allocator.create(Ir);
            make_box.* = .{ .make_box = .{ .operand = nil_ir } };
            try boxed_bindings.append(self.allocator, .{ .name = name, .value = make_box, .index = idx });
        }

        // Compile lambdas in labels_env for recursive visibility, then assign
        // each slot's box content.
        var init_forms = std.ArrayList(*const Ir){};
        defer init_forms.deinit(self.allocator);
        for (lambda_args.items, indices.items, sym_vals.items) |largs, idx, sym_val| {
            const lambda_ir = try self.compileLambda(largs, &labels_env);
            if (lambda_ir.* == .lambda) {
                lambda_ir.lambda.name = sym_val;
            }

            const slot_name = sym_val.toPtr(Symbol).getName();
            const slot_ref = try self.builder.variable(slot_name, 0, idx);
            const box_set = try self.allocator.create(Ir);
            box_set.* = .{ .box_set = .{ .left = slot_ref, .right = lambda_ir } };
            try init_forms.append(self.allocator, box_set);
        }

        const body_ir = try self.compileBodyWithTail(body_exprs, &labels_env, in_tail);
        try init_forms.append(self.allocator, body_ir);
        const seq_ir = try self.builder.progn(init_forms.items);

        return try self.builder.letExpr(boxed_bindings.items, seq_ir);
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

        // Desugar to explicit nested LET forms, then compile once.
        // This keeps lexical shadowing semantics correct for repeated names.
        const nested_let_form = try self.buildNestedLetFromLetStar(bindings_expr, body_exprs);
        return self.compileWithTail(nested_let_form, env, in_tail);
    }

    fn buildNestedLetFromLetStar(self: *Compiler, bindings_list: Value, body_exprs: Value) anyerror!Value {
        if (!bindings_list.isCons()) return error.InvalidLet;
        const heap = if (self.heap) |h| h else return error.InvalidLet;
        const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;

        const bind_cons = bindings_list.toPtr(Cons);
        const first_binding = bind_cons.car;
        const rest_bindings = bind_cons.cdr;

        // Validate binding syntax early so invalid let* forms still fail here.
        if (!first_binding.isSymbolLike() and !first_binding.isCons()) return error.InvalidLet;
        if (first_binding.isCons()) {
            const pair = first_binding.toPtr(Cons);
            if (!pair.car.isSymbolLike()) return error.InvalidLet;
        }

        const single_binding_list = try heap.allocCons(first_binding, Value.nil);

        const inner_body = if (rest_bindings.isNil()) blk: {
            break :blk body_exprs;
        } else blk: {
            const nested_inner = try self.buildNestedLetFromLetStar(rest_bindings, body_exprs);
            break :blk try heap.allocCons(nested_inner, Value.nil);
        };

        const let_args = try heap.allocCons(single_binding_list, inner_body);
        return try heap.allocCons(b.let, let_args);
    }

    fn compileLetStarBindings(self: *Compiler, bindings_list: Value, body_exprs: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        if (!bindings_list.isCons()) return error.InvalidLet;

        const binding_cons = bindings_list.toPtr(Cons);
        const binding = binding_cons.car;
        const rest = binding_cons.cdr;

        const bind_sym = if (binding.isSymbolLike()) blk: {
            break :blk binding;
        } else if (binding.isCons()) blk: {
            const b = binding.toPtr(Cons);
            if (!b.car.isSymbolLike()) return error.InvalidLet;
            break :blk b.car;
        } else {
            return error.InvalidLet;
        };
        const name_raw = bind_sym.getSymbolName();
        const name = try self.allocator.dupe(u8, name_raw);

        // Compile value before introducing the new binding.
        // This preserves CL let* shadowing semantics for repeated names:
        // (let* ((x 1) (x (+ x 1))) ...) must read the outer X in (+ x 1).
        const val_ir = blk: {
            if (binding.isSymbolLike()) break :blk try self.builder.lit(Value.nil);
            if (!binding.isCons()) return error.InvalidLet;
            const b = binding.toPtr(Cons);
            if (b.cdr.isNil()) break :blk try self.builder.lit(Value.nil);
            if (!b.cdr.isCons()) return error.InvalidLet;
            const val_cons = b.cdr.toPtr(Cons);
            break :blk try self.compile(val_cons.car, env);
        };

        // Create extended environment and allocate slot for this binding.
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();
        const index = if (bind_sym.typeKind() == .symbol)
            try let_env.bindSym(bind_sym)
        else
            try let_env.bindName(name);

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
            const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
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
            const first_ir = try self.compile(first, env);

            // Check for type predicate to enable occurrence typing in nested and
            const pred_info = extractPredicateInfo(first_ir);
            const nested_and = blk: {
                if (pred_info) |info| {
                    var then_occ = OccurrenceCtx.init(self.allocator);
                    defer then_occ.deinit();
                    try then_occ.narrowed.put(info.var_name, info.narrowed_type);

                    const saved_occ = self.occ;
                    self.occ = &then_occ;
                    defer self.occ = saved_occ;

                    break :blk try self.compileAnd(rest, env);
                } else {
                    break :blk try self.compileAnd(rest, env);
                }
            };

            const nil_ir = try self.builder.lit(Value.nil);
            return try self.builder.ifExpr(first_ir, nested_and, nil_ir);
        }

        const first_ir = try self.compile(first, env);

        // Check for type predicate to enable occurrence typing in second expr
        const pred_info = extractPredicateInfo(first_ir);
        const second_ir = blk: {
            if (pred_info) |info| {
                var then_occ = OccurrenceCtx.init(self.allocator);
                defer then_occ.deinit();
                try then_occ.narrowed.put(info.var_name, info.narrowed_type);

                const saved_occ = self.occ;
                self.occ = &then_occ;
                defer self.occ = saved_occ;

                break :blk try self.compile(second, env);
            } else {
                break :blk try self.compile(second, env);
            }
        };

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

        const first_ir = try self.compile(first, env);

        // Create let binding for tmp
        const tmp_name = "__or_tmp";
        var tmp_env = Env.initLet(self.allocator, env);
        defer tmp_env.deinit();
        const tmp_idx = try tmp_env.bindName(tmp_name);

        const bindings = try self.allocator.alloc(ir.Ir.Binding, 1);
        bindings[0] = .{ .name = tmp_name, .value = first_ir, .index = tmp_idx };

        const tmp_var1 = try self.builder.variable(tmp_name, 0, tmp_idx);
        const tmp_var2 = try self.builder.variable(tmp_name, 0, tmp_idx);

        // Check for type predicate to enable occurrence typing in else branch
        const pred_info = extractPredicateInfo(first_ir);
        const else_ir = blk: {
            if (pred_info) |info| {
                if (info.else_type) |else_ty| {
                    var else_occ = OccurrenceCtx.init(self.allocator);
                    defer else_occ.deinit();
                    try else_occ.narrowed.put(info.var_name, else_ty);

                    const saved_occ = self.occ;
                    self.occ = &else_occ;
                    defer self.occ = saved_occ;

                    break :blk try self.compileOr(rest, env);
                }
            }
            break :blk try self.compileOr(rest, env);
        };

        const body = try self.builder.ifExpr(tmp_var1, tmp_var2, else_ir);
        return try self.builder.letExpr(bindings, body);
    }

    fn compileFuncall(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (funcall fn arg1 arg2 ...) - fn is evaluated in value namespace
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const fn_ir = try self.compile(cons1.car, env);

        var compiled_args = std.ArrayList(*Ir){};
        defer compiled_args.deinit(self.allocator);

        var list = cons1.cdr;
        while (list.isCons()) {
            const c = list.toPtr(Cons);
            const arg_ir = try self.compile(c.car, env);
            try compiled_args.append(self.allocator, arg_ir);
            list = c.cdr;
        }

        const items = try self.allocator.dupe(*const Ir, compiled_args.items);
        return try self.builder.call(fn_ir, items);
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
        const var_val = cons1.car;
        const var_sym = var_val.toPtr(Symbol);
        const local_name = var_sym.getName();

        if (!cons1.cdr.isCons()) return error.InvalidSet;
        const cons2 = cons1.cdr.toPtr(Cons);
        const val_ir = try self.compile(cons2.car, env);

        // First check local environment
        if (env.lookupSym(var_val)) |binding| {
            // If this variable is boxed, use box-set! instead
            if (self.boxed_vars) |bv| {
                if (bv.contains(var_val)) {
                    // Compile (box-set! var val) instead of (set! var val)
                    const var_ir = try self.builder.variable(local_name, binding.depth, binding.index);
                    const box_set = try self.allocator.create(Ir);
                    box_set.* = .{ .box_set = .{ .left = var_ir, .right = val_ir } };
                    return box_set;
                }
            }
            return try self.builder.set(local_name, binding.depth, binding.index, val_ir);
        }

        // Check globals - use qualified name for package-aware lookup
        var qual_buf: [256]u8 = undefined;
        const q = try self.getQualifiedName(var_sym, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const global_name = q.name;

        var name = global_name;
        var idx_opt = self.globals.lookup(name);
        if (idx_opt == null) {
            if (self.globals.lookup(local_name)) |idx| {
                idx_opt = idx;
                name = local_name;
            } else {
                const prefixes = [_][]const u8{ "COMMON-LISP:", "CL:", "CL-USER:", "COMMON-LISP-USER:" };
                var full_buf: [640]u8 = undefined;
                for (prefixes) |prefix| {
                    if (prefix.len + local_name.len > full_buf.len) continue;
                    @memcpy(full_buf[0..prefix.len], prefix);
                    @memcpy(full_buf[prefix.len .. prefix.len + local_name.len], local_name);
                    const candidate = full_buf[0 .. prefix.len + local_name.len];
                    if (self.globals.lookup(candidate)) |idx| {
                        idx_opt = idx;
                        name = candidate;
                        break;
                    }
                }
            }
        }

        // CL semantics: top-level setq creates a global binding when absent.
        const idx = idx_opt orelse try self.globals.define(name);
        return try self.builder.define(name, idx, val_ir);
    }

    /// Compile setf special form: (setf place value)
    /// Handles symbol macros, compound places like (car x), (slot-value obj 'slot), etc.
    fn compileSetf(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const place = cons1.car;

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const value_expr = cons2.car;
        if (!cons2.cdr.isNil()) {
            // Multi-place setf: compile each pair in sequence and return last value.
            var items = std.ArrayList(*const Ir){};
            defer items.deinit(self.allocator);

            var p = args;
            const heap = if (self.heap) |val| val else return error.InvalidSyntax;
            while (p.isCons()) {
                const pair1 = p.toPtr(Cons);
                if (!pair1.cdr.isCons()) return error.InvalidSyntax;
                const pair2 = pair1.cdr.toPtr(Cons);

                const one = try heap.allocCons(pair1.car, try heap.allocCons(pair2.car, Value.nil));
                const node_ir = try self.compileSetf(one, env);
                try items.append(self.allocator, node_ir);

                p = pair2.cdr;
            }

            if (items.items.len == 0) return error.InvalidSyntax;
            if (items.items.len == 1) return @constCast(items.items[0]);
            return try self.builder.progn(items.items);
        }

        // If place is a symbol, check for symbol macro
        if (place.isSymbol()) {
            if (self.symbol_macros.get(place)) |expansion| {
                // Symbol macro expands to compound form - apply setf to expansion
                if (expansion.isCons()) {
                    // Rebuild (setf expanded-place value) and recompile
                    const heap = if (self.heap) |val| val else return error.InvalidSyntax;
                    const new_args = try heap.allocCons(expansion, cons1.cdr);
                    return self.compileSetf(new_args, env);
                }
                // Symbol macro expands to simple symbol - use setq on that symbol
                if (expansion.isSymbol()) {
                    const exp_val = expansion;
                    const exp_sym = exp_val.toPtr(Symbol);
                    const exp_name = exp_sym.getName();
                    const val_ir = try self.compile(value_expr, env);

                    // Check local environment
                    if (env.lookupSym(exp_val)) |binding| {
                        if (self.boxed_vars) |bv| {
                            if (bv.contains(exp_val)) {
                                const var_ir = try self.builder.variable(exp_name, binding.depth, binding.index);
                                const box_set = try self.allocator.create(Ir);
                                box_set.* = .{ .box_set = .{ .left = var_ir, .right = val_ir } };
                                return box_set;
                            }
                        }
                        return try self.builder.set(exp_name, binding.depth, binding.index, val_ir);
                    }

                    // Check globals
                    var qual_buf: [256]u8 = undefined;
                    const q = try self.getQualifiedName(exp_sym, &qual_buf);
                    defer if (q.owned) self.allocator.free(q.name);
                    const global_name = q.name;
                    var name = global_name;
                    var idx_opt = self.globals.lookup(name);
                    if (idx_opt == null) {
                        if (self.globals.lookup(exp_name)) |idx| {
                            idx_opt = idx;
                            name = exp_name;
                        } else {
                            const prefixes = [_][]const u8{ "COMMON-LISP:", "CL:", "CL-USER:", "COMMON-LISP-USER:" };
                            var full_buf: [640]u8 = undefined;
                            for (prefixes) |prefix| {
                                if (prefix.len + exp_name.len > full_buf.len) continue;
                                @memcpy(full_buf[0..prefix.len], prefix);
                                @memcpy(full_buf[prefix.len .. prefix.len + exp_name.len], exp_name);
                                const candidate = full_buf[0 .. prefix.len + exp_name.len];
                                if (self.globals.lookup(candidate)) |idx| {
                                    idx_opt = idx;
                                    name = candidate;
                                    break;
                                }
                            }
                        }
                    }
                    if (idx_opt) |idx| return try self.builder.define(name, idx, val_ir);
                    return error.UnboundVariable;
                }
                return error.InvalidSyntax;
            }
            // Simple variable - delegate to setq
            return self.compileSet(args, env);
        }

        // Place is a compound form - dispatch based on head
        if (place.isCons()) {
            const place_cons = place.toPtr(Cons);
            const head = place_cons.car;
            const place_args = place_cons.cdr;

            if (head.isSymbol()) {
                // If the place head is a macro, expand it and retry setf.
                // E.g., (setf (symbol-array x) v) where symbol-array is a macro
                //   expanding to (get x 'array) -> (setf (get x 'array) v)
                if (self.lookupMacroDef(head)) |macro_def| {
                    if (!macro_def.isNil()) {
                        if (self.vm) |vm| {
                            const heap = if (self.heap) |val| val else return error.InvalidSyntax;
                            // Expand the place macro: (symbol-array x) -> (get x 'array)
                            const expanded_place = self.expandMacro(macro_def, place_args, place, vm) catch place;
                            if (!expanded_place.eq(place)) {
                                // Rebuild (setf expanded-place value-expr) and retry
                                const new_args = try heap.allocCons(expanded_place, cons1.cdr);
                                return self.compileSetf(new_args, env);
                            }
                        }
                    }
                }

                const b = if (self.builtins) |val| val else return error.InvalidSyntax;
                const dispatch_head = self.canonicalBuiltinSymbol(head);
                const h = dispatch_head.raw;

                // (setf (car x) val) -> (rplaca x val)
                if (h == b.car.raw or h == b.first.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const obj_ir = try self.compile(place_args.toPtr(Cons).car, env);
                    const val_ir = try self.compile(value_expr, env);
                    return try self.builder.rplaca(obj_ir, val_ir);
                }

                // (setf (cdr x) val) -> (rplacd x val)
                if (h == b.cdr.raw or h == b.rest.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const obj_ir = try self.compile(place_args.toPtr(Cons).car, env);
                    const val_ir = try self.compile(value_expr, env);
                    return try self.builder.rplacd(obj_ir, val_ir);
                }

                // (setf (symbol-function 'sym) val) -> set_symbol_function
                // When the argument is a quoted symbol, we can resolve the global
                // at compile time (like defun). Otherwise emit the runtime opcode.
                if (h == b.@"symbol-function".raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const sym_arg = place_args.toPtr(Cons).car;
                    // Try to extract quoted symbol for compile-time global resolution
                    if (sym_arg.isCons()) {
                        const qa = sym_arg.toPtr(Cons);
                        if (qa.car.isSymbol() and qa.car.raw == b.quote.raw and qa.cdr.isCons()) {
                            const sym_val = qa.cdr.toPtr(Cons).car;
                            if (sym_val.isSymbol()) {
                                const sym = sym_val.toPtr(Symbol);
                                var qual_buf: [256]u8 = undefined;
                                const q = try self.getQualifiedName(sym, &qual_buf);
                                defer if (q.owned) self.allocator.free(q.name);
                                const idx = try self.globals.define(q.name);
                                const val_ir = try self.compile(value_expr, env);
                                return try self.builder.define(q.name, idx, val_ir);
                            }
                        }
                    }
                    // Dynamic case: emit runtime set_symbol_function opcode
                    const sym_ir = try self.compile(sym_arg, env);
                    const val_ir = try self.compile(value_expr, env);
                    return try self.builder.setSymbolFunction(sym_ir, val_ir);
                }

                // (setf (slot-value obj 'slot) val) -> set_slot_value
                if (h == b.@"slot-value".raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const pc1 = place_args.toPtr(Cons);
                    const obj_ir = try self.compile(pc1.car, env);

                    if (!pc1.cdr.isCons()) return error.InvalidSyntax;
                    const pc2 = pc1.cdr.toPtr(Cons);
                    var slot_name_expr = pc2.car;

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
                    const val_ir = try self.compile(value_expr, env);
                    return try self.builder.setSlotValue(obj_ir, slot_sym, val_ir);
                }

                // (setf (fdefinition 'name) val) -> define global function binding
                // (setf (fdefinition '(setf name)) val) -> define global binding for "(setf name)"
                // Note: we require a quoted function-name to avoid silently miscompiling dynamic names.
                if (h == b.fdefinition.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const ac1 = place_args.toPtr(Cons);
                    if (!ac1.cdr.isNil()) return error.InvalidSyntax;
                    const name_expr = ac1.car;

                    // Require (quote <function-name>)
                    if (!name_expr.isCons()) return error.InvalidSyntax;
                    const q0 = name_expr.toPtr(Cons);
                    if (!q0.car.isSymbol() or q0.car.raw != b.quote.raw) return error.InvalidSyntax;
                    if (!q0.cdr.isCons()) return error.InvalidSyntax;
                    const q1 = q0.cdr.toPtr(Cons);
                    if (!q1.cdr.isNil()) return error.InvalidSyntax;
                    const fn_name = q1.car;

                    const val_ir = try self.compile(value_expr, env);

                    switch (fn_name.typeKind()) {
                        .symbol => {
                            const sym = fn_name.toPtr(Symbol);
                            var qual_buf: [256]u8 = undefined;
                            const q = try self.getQualifiedName(sym, &qual_buf);
                            defer if (q.owned) self.allocator.free(q.name);
                            const name = q.name;
                            const idx = try self.globals.define(name);
                            return try self.builder.define(name, idx, val_ir);
                        },
                        .cons => {
                            const c0 = fn_name.toPtr(Cons);
                            if (!c0.car.isSymbol() or c0.car.raw != b.setf.raw) return error.InvalidSyntax;
                            if (!c0.cdr.isCons()) return error.InvalidSyntax;
                            const c1 = c0.cdr.toPtr(Cons);
                            if (!c1.car.isSymbol()) return error.InvalidSyntax;
                            if (!c1.cdr.isNil()) return error.InvalidSyntax;

                            const base_name = c1.car.toPtr(Symbol).getName();
                            const setf_name = try std.fmt.allocPrint(self.allocator, "(SETF {s})", .{base_name});
                            defer self.allocator.free(setf_name);

                            var qual_buf: [512]u8 = undefined;
                            const q = try self.qualifyName(setf_name, &qual_buf);
                            defer if (q.owned) self.allocator.free(q.name);
                            const name = q.name;
                            const idx = try self.globals.define(name);
                            return try self.builder.define(name, idx, val_ir);
                        },
                        else => return error.InvalidSyntax,
                    }
                }

                // (setf (gethash key table) val) -> hash_set IR
                if (h == b.gethash.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const pc1 = place_args.toPtr(Cons);
                    const key_ir = try self.compile(pc1.car, env);

                    if (!pc1.cdr.isCons()) return error.InvalidSyntax;
                    const pc2 = pc1.cdr.toPtr(Cons);
                    const table_ir = try self.compile(pc2.car, env);
                    const val_ir = try self.compile(value_expr, env);

                    const node = try self.allocator.create(Ir);
                    node.* = .{ .hash_set = .{ .table = table_ir, .key = key_ir, .value = val_ir } };
                    return node;
                }

                // (setf (aref array idx...) val), (setf (bit array idx) val),
                // (setf (sbit array idx) val) -- all lower to (%aset array idx... val).
                if (h == b.aref.raw or h == b.bit.raw or h == b.sbit.raw) {
                    // Build args for compileAset: (array sub1 sub2 ... val)
                    const heap = if (self.heap) |val| val else return error.InvalidSyntax;
                    // Append value_expr to place_args
                    var aset_args = place_args;
                    var last: ?*Cons = null;
                    var p = place_args;
                    while (p.isCons()) {
                        last = p.toPtr(Cons);
                        p = last.?.cdr;
                    }
                    if (last) |l| {
                        const new_cell = try heap.allocCons(value_expr, Value.nil);
                        l.cdr = new_cell;
                    } else {
                        aset_args = try heap.allocCons(value_expr, Value.nil);
                    }
                    return self.compileAset(aset_args, env);
                }

                // (setf (svref vec idx) val) -> vec_set
                if (h == b.svref.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const pc1 = place_args.toPtr(Cons);
                    const vec_ir = try self.compile(pc1.car, env);

                    if (!pc1.cdr.isCons()) return error.InvalidSyntax;
                    const pc2 = pc1.cdr.toPtr(Cons);
                    const idx_ir = try self.compile(pc2.car, env);
                    const val_ir = try self.compile(value_expr, env);
                    return try self.builder.vecSet(vec_ir, idx_ir, val_ir);
                }

                // (setf (nth n list) val) -> (rplaca (nthcdr n list) val)
                if (h == b.nth.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const pc1 = place_args.toPtr(Cons);
                    const n_ir = try self.compile(pc1.car, env);

                    if (!pc1.cdr.isCons()) return error.InvalidSyntax;
                    const pc2 = pc1.cdr.toPtr(Cons);
                    const list_ir = try self.compile(pc2.car, env);
                    const val_ir = try self.compile(value_expr, env);

                    // Build (nthcdr n list)
                    const nthcdr_node = try self.allocator.create(Ir);
                    nthcdr_node.* = .{ .nthcdr = .{ .left = n_ir, .right = list_ir } };
                    // Build (rplaca nthcdr-result val)
                    return try self.builder.rplaca(nthcdr_node, val_ir);
                }

                // (setf (elt seq idx) val) -> elt_set (polymorphic)
                if (h == b.elt.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const pc1 = place_args.toPtr(Cons);
                    const seq_ir = try self.compile(pc1.car, env);

                    if (!pc1.cdr.isCons()) return error.InvalidSyntax;
                    const pc2 = pc1.cdr.toPtr(Cons);
                    const idx_ir = try self.compile(pc2.car, env);
                    const val_ir = try self.compile(value_expr, env);

                    const node = try self.allocator.create(Ir);
                    node.* = .{ .elt_set = .{ .seq = seq_ir, .index = idx_ir, .value = val_ir } };
                    return node;
                }

                // (setf (fill-pointer vec) val) -> vec_set_fill_ptr
                if (h == b.@"%fill-pointer".raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const vec_ir = try self.compile(place_args.toPtr(Cons).car, env);
                    const val_ir = try self.compile(value_expr, env);
                    const node = try self.allocator.create(Ir);
                    node.* = .{ .vec_set_fill_ptr = .{ .left = vec_ir, .right = val_ir } };
                    return node;
                }

                // (setf (char str idx) val) -> str_set
                if (h == b.char.raw or h == b.schar.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const pc1 = place_args.toPtr(Cons);
                    const str_ir = try self.compile(pc1.car, env);

                    if (!pc1.cdr.isCons()) return error.InvalidSyntax;
                    const pc2 = pc1.cdr.toPtr(Cons);
                    const idx_ir = try self.compile(pc2.car, env);
                    const val_ir = try self.compile(value_expr, env);
                    return try self.builder.strSet(str_ir, idx_ir, val_ir);
                }

                // (setf (get sym key) val) -> put IR
                if (h == b.get.raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const pc1 = place_args.toPtr(Cons);
                    const sym_ir = try self.compile(pc1.car, env);

                    if (!pc1.cdr.isCons()) return error.InvalidSyntax;
                    const pc2 = pc1.cdr.toPtr(Cons);
                    const key_ir = try self.compile(pc2.car, env);
                    const val_ir = try self.compile(value_expr, env);

                    const node = try self.allocator.create(Ir);
                    node.* = .{ .put = .{ .first = sym_ir, .second = key_ir, .third = val_ir } };
                    return node;
                }

                // (setf (macro-function 'name) fn)
                // -> (setf (get name 'macro-function) fn) for property list storage
                if (head.isSymbol()) {
                    const head_name = head.toPtr(Symbol).getName();
                    if (std.mem.eql(u8, head_name, "MACRO-FUNCTION") or
                        std.mem.eql(u8, head_name, "macro-function"))
                    {
                        // (macro-function sym-expr [env]) -> rewrite to (get sym-expr 'macro-function)
                        if (!place_args.isCons()) return error.InvalidSyntax;
                        const sym_expr = place_args.toPtr(Cons).car;
                        const sym_ir = try self.compile(sym_expr, env);
                        const heap2 = if (self.heap) |val| val else return error.InvalidSyntax;
                        const mf_sym = try heap2.intern("MACRO-FUNCTION");
                        const key_ir = try self.builder.lit(mf_sym);
                        const val_ir = try self.compile(value_expr, env);
                        const node = try self.allocator.create(Ir);
                        node.* = .{ .put = .{ .first = sym_ir, .second = key_ir, .third = val_ir } };
                        return node;
                    }
                }

                // (setf (find-class name [errorp [environment]]) val)
                // Mutates runtime class registry directly.
                if (h == b.@"find-class".raw) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const pc1 = place_args.toPtr(Cons);
                    const name_ir = try self.compile(pc1.car, env);
                    var rest = pc1.cdr;
                    var optional_count: u8 = 0;
                    while (rest.isCons()) {
                        optional_count += 1;
                        if (optional_count > 2) return error.InvalidSyntax;
                        rest = rest.toPtr(Cons).cdr;
                    }
                    if (!rest.isNil()) return error.InvalidSyntax;

                    const val_ir = try self.compile(value_expr, env);
                    const node = try self.allocator.create(Ir);
                    node.* = .{ .set_find_class = .{ .left = name_ir, .right = val_ir } };
                    return node;
                }

                // (setf (logical-pathname-translations host) val)
                // Route directly to set-logical-pathname-translations(host, val).
                if (std.ascii.eqlIgnoreCase(head.toPtr(Symbol).getName(), "logical-pathname-translations")) {
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const host_cons = place_args.toPtr(Cons);
                    if (!host_cons.cdr.isNil()) return error.InvalidSyntax;

                    const host_ir = try self.compile(host_cons.car, env);
                    const val_ir = try self.compile(value_expr, env);

                    const setter_candidates = [_][]const u8{
                        "SET-LOGICAL-PATHNAME-TRANSLATIONS",
                        "set-logical-pathname-translations",
                        "COMMON-LISP:SET-LOGICAL-PATHNAME-TRANSLATIONS",
                        "COMMON-LISP:set-logical-pathname-translations",
                        "CL:SET-LOGICAL-PATHNAME-TRANSLATIONS",
                        "CL:set-logical-pathname-translations",
                        "CL-USER:SET-LOGICAL-PATHNAME-TRANSLATIONS",
                        "CL-USER:set-logical-pathname-translations",
                    };

                    for (setter_candidates) |name| {
                        if (self.globals.lookup(name)) |idx| {
                            const set_ref = try self.builder.globalRef(name, idx);
                            const args_ir = try self.allocator.alloc(*Ir, 2);
                            args_ir[0] = host_ir;
                            args_ir[1] = val_ir;
                            return try self.builder.call(set_ref, args_ir);
                        }
                    }
                }

                // (setf (values p1 p2 ... pn) form) ->
                // (multiple-value-bind (t1 t2 ... tn) form
                //   (setf p1 t1) (setf p2 t2) ... (setf pn tn)
                //   (values t1 t2 ... tn))
                if (h == b.values.raw) {
                    const heap = if (self.heap) |val| val else return error.InvalidSyntax;
                    // Collect places
                    var places = std.ArrayList(Value){};
                    defer places.deinit(self.allocator);
                    var p = place_args;
                    while (p.isCons()) {
                        const c = p.toPtr(Cons);
                        try places.append(self.allocator, c.car);
                        p = c.cdr;
                    }
                    if (places.items.len == 0) return error.InvalidSyntax;

                    // Generate temp vars and build mvb bindings list
                    var bindings = Value.nil;
                    var temps = std.ArrayList(Value){};
                    defer temps.deinit(self.allocator);
                    for (0..places.items.len) |_| {
                        const tmp = try prims.gensym(heap, null);
                        try temps.append(self.allocator, tmp);
                        bindings = try heap.allocCons(tmp, bindings);
                    }

                    // Build body: (setf p1 t1) ... (setf pn tn) (values t1 ... tn)
                    // Start from the end
                    var vals_list = Value.nil;
                    var i = temps.items.len;
                    while (i > 0) {
                        i -= 1;
                        vals_list = try heap.allocCons(temps.items[i], vals_list);
                    }
                    var body = try heap.allocCons(b.values, vals_list);
                    body = try heap.allocCons(body, Value.nil);

                    // Prepend setf forms in reverse
                    i = places.items.len;
                    while (i > 0) {
                        i -= 1;
                        const setf_pair = try heap.allocCons(places.items[i], try heap.allocCons(temps.items[i], Value.nil));
                        const setf_form = try heap.allocCons(b.setf, setf_pair);
                        body = try heap.allocCons(setf_form, body);
                    }

                    // Build (multiple-value-bind (temps...) value-expr body...)
                    const mvb_form = try heap.allocCons(
                        b.@"multiple-value-bind",
                        try heap.allocCons(
                            bindings,
                            try heap.allocCons(value_expr, body),
                        ),
                    );
                    return self.compile(mvb_form, env);
                }

                // (setf (the type place) val) -> (setf place (the type val))
                if (h == b.the.raw) {
                    const heap = if (self.heap) |val| val else return error.InvalidSyntax;
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const the_type_cons = place_args.toPtr(Cons);
                    if (!the_type_cons.cdr.isCons()) return error.InvalidSyntax;
                    const the_place_cons = the_type_cons.cdr.toPtr(Cons);
                    const inner_place = the_place_cons.car;
                    // Build (the type value_expr)
                    const the_val = try heap.allocCons(b.the, try heap.allocCons(the_type_cons.car, try heap.allocCons(value_expr, Value.nil)));
                    // Build (setf inner_place (the type val))
                    const new_args = try heap.allocCons(inner_place, try heap.allocCons(the_val, Value.nil));
                    return self.compileSetf(new_args, env);
                }

                // (setf (apply #'fn args...) val) -> (apply #'(setf fn) val args...)
                if (h == b.apply.raw) {
                    const heap = if (self.heap) |val| val else return error.InvalidSyntax;
                    if (!place_args.isCons()) return error.InvalidSyntax;
                    const fn_cons = place_args.toPtr(Cons);
                    const fn_expr = fn_cons.car;

                    // Extract function name from #'fn (function fn) form
                    if (fn_expr.isCons()) {
                        const fe = fn_expr.toPtr(Cons);
                        if (fe.car.isSymbol() and fe.car.raw == b.function.raw) {
                            if (fe.cdr.isCons()) {
                                const fn_name_val = fe.cdr.toPtr(Cons).car;
                                if (fn_name_val.isSymbol()) {
                                    // Build (setf fn-name) list
                                    const setf_fn_list = try heap.allocCons(b.setf, try heap.allocCons(fn_name_val, Value.nil));
                                    // Build #'(setf fn-name) -> (function (setf fn-name))
                                    const fn_ref = try heap.allocCons(b.function, try heap.allocCons(setf_fn_list, Value.nil));
                                    // Build (apply #'(setf fn-name) val args...)
                                    var apply_args = try heap.allocCons(fn_ref, try heap.allocCons(value_expr, fn_cons.cdr));
                                    const apply_form = try heap.allocCons(b.apply, apply_args);
                                    _ = &apply_args;
                                    return self.compile(apply_form, env);
                                }
                            }
                        }
                    }
                }

                // Fallback: look for (setf accessor-name) function
                // (setf (accessor-name args...) val) -> ((setf accessor-name) val args...)
                const func_sym = head.toPtr(Symbol);
                const func_name = func_sym.getName();

                if (try self.compileSetfCxr(func_name, place_args, value_expr, env)) |cxr_ir| {
                    return cxr_ir;
                }

                // (setf (struct-slot obj) val) for defstruct-generated accessors.
                if (try self.compileSetfStructAccessor(func_name, place_args, value_expr, env)) |set_ir| {
                    return set_ir;
                }

                // Accept both canonical "(SETF ...)" and legacy "(setf ...)" names.
                const setf_name_upper = try std.fmt.allocPrint(self.allocator, "(SETF {s})", .{func_name});
                defer self.allocator.free(setf_name_upper);
                if (self.globals.lookup(setf_name_upper)) |idx| {
                    return try self.compileSetfGlobalCall(env, value_expr, place_args, setf_name_upper, idx);
                }

                var qual_upper_buf: [512]u8 = undefined;
                const q_upper = try self.qualifyName(setf_name_upper, &qual_upper_buf);
                defer if (q_upper.owned) self.allocator.free(q_upper.name);
                if (self.globals.lookup(q_upper.name)) |idx| {
                    return try self.compileSetfGlobalCall(env, value_expr, place_args, q_upper.name, idx);
                }

                const prefixes = [_][]const u8{ "COMMON-LISP:", "CL:", "CL-USER:", "COMMON-LISP-USER:" };
                var prefixed_upper_buf: [768]u8 = undefined;
                for (prefixes) |prefix| {
                    if (prefix.len + setf_name_upper.len > prefixed_upper_buf.len) continue;
                    @memcpy(prefixed_upper_buf[0..prefix.len], prefix);
                    @memcpy(prefixed_upper_buf[prefix.len .. prefix.len + setf_name_upper.len], setf_name_upper);
                    const candidate = prefixed_upper_buf[0 .. prefix.len + setf_name_upper.len];
                    if (self.globals.lookup(candidate)) |idx| {
                        return try self.compileSetfGlobalCall(env, value_expr, place_args, candidate, idx);
                    }
                }

                const setf_name_lower = try std.fmt.allocPrint(self.allocator, "(setf {s})", .{func_name});
                defer self.allocator.free(setf_name_lower);
                if (self.globals.lookup(setf_name_lower)) |idx| {
                    return try self.compileSetfGlobalCall(env, value_expr, place_args, setf_name_lower, idx);
                }

                var qual_lower_buf: [512]u8 = undefined;
                const q_lower = try self.qualifyName(setf_name_lower, &qual_lower_buf);
                defer if (q_lower.owned) self.allocator.free(q_lower.name);
                if (self.globals.lookup(q_lower.name)) |idx| {
                    return try self.compileSetfGlobalCall(env, value_expr, place_args, q_lower.name, idx);
                }

                var prefixed_lower_buf: [768]u8 = undefined;
                for (prefixes) |prefix| {
                    if (prefix.len + setf_name_lower.len > prefixed_lower_buf.len) continue;
                    @memcpy(prefixed_lower_buf[0..prefix.len], prefix);
                    @memcpy(prefixed_lower_buf[prefix.len .. prefix.len + setf_name_lower.len], setf_name_lower);
                    const candidate = prefixed_lower_buf[0 .. prefix.len + setf_name_lower.len];
                    if (self.globals.lookup(candidate)) |idx| {
                        return try self.compileSetfGlobalCall(env, value_expr, place_args, candidate, idx);
                    }
                }
            }
        }

        if (try self.compileSetfViaMacro(args, env)) |expanded_ir| {
            return expanded_ir;
        }

        return error.InvalidSyntax;
    }

    fn compileSetfViaMacro(self: *Compiler, args: Value, env: *const Env) anyerror!?*Ir {
        const vm = self.vm orelse return null;
        const heap = self.heap orelse return null;

        try self.refreshBuiltins();
        const b = self.builtins orelse return null;
        const whole_form = try heap.allocCons(b.setf, args);

        if (self.lookupMacroDef(b.setf)) |macro_def| {
            const expanded = try self.expandMacro(macro_def, args, whole_form, vm);
            if (expanded.raw != whole_form.raw) {
                return try self.compileWithTail(expanded, env, false);
            }
        }

        if (vm.macroexpand_1_callback) |macroexpand1| {
            if (vm.macroexpand_1_context) |ctx| {
                const expanded = try macroexpand1(whole_form, ctx);
                if (expanded.raw != whole_form.raw) {
                    return try self.compileWithTail(expanded, env, false);
                }
            }
        }

        return null;
    }

    fn compileSetfStructAccessor(
        self: *Compiler,
        accessor_name: []const u8,
        place_args: Value,
        value_expr: Value,
        env: *const Env,
    ) anyerror!?*Ir {
        if (!place_args.isCons()) return null;
        const arg_cons = place_args.toPtr(Cons);
        if (!arg_cons.cdr.isNil()) return null;

        if (self.struct_accessors.get(accessor_name)) |slot_idx| {
            const obj_ir = try self.compile(arg_cons.car, env);
            const idx_ir = try self.builder.lit(Value.makeFixnum(@intCast(slot_idx + 1)));
            const val_ir = try self.compile(value_expr, env);
            return try self.builder.vecSet(obj_ir, idx_ir, val_ir);
        }

        var qual_buf: [512]u8 = undefined;
        const q = try self.qualifyName(accessor_name, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        if (self.struct_accessors.get(q.name)) |slot_idx| {
            const obj_ir = try self.compile(arg_cons.car, env);
            const idx_ir = try self.builder.lit(Value.makeFixnum(@intCast(slot_idx + 1)));
            const val_ir = try self.compile(value_expr, env);
            return try self.builder.vecSet(obj_ir, idx_ir, val_ir);
        }

        // Accessor format: STRUCT-FIELD where FIELD may contain dashes.
        const dash_idx = std.mem.indexOfScalar(u8, accessor_name, '-') orelse return null;
        if (dash_idx == 0 or dash_idx + 1 >= accessor_name.len) return null;
        const struct_name = accessor_name[0..dash_idx];
        const field_name = accessor_name[dash_idx + 1 ..];

        var slot_idx_opt: ?usize = null;
        var it = self.struct_types.iterator();
        while (it.next()) |entry| {
            const st_ty = entry.value_ptr.*.*;
            if (st_ty != .@"struct") continue;
            const st = st_ty.@"struct";
            if (!std.ascii.eqlIgnoreCase(st.name, struct_name)) continue;
            for (st.fields, 0..) |field, i| {
                if (std.ascii.eqlIgnoreCase(field.name, field_name)) {
                    slot_idx_opt = i;
                    break;
                }
            }
            if (slot_idx_opt != null) break;
        }
        if (slot_idx_opt == null) {
            var fallback_slot: ?usize = null;
            var fallback_matches: usize = 0;
            var it_fallback = self.struct_types.iterator();
            while (it_fallback.next()) |entry| {
                const st_ty = entry.value_ptr.*.*;
                if (st_ty != .@"struct") continue;
                const st = st_ty.@"struct";
                for (st.fields, 0..) |field, i| {
                    if (std.ascii.eqlIgnoreCase(field.name, field_name)) {
                        fallback_slot = i;
                        fallback_matches += 1;
                        break;
                    }
                }
                if (fallback_matches > 1) break;
            }
            if (fallback_matches == 1) {
                slot_idx_opt = fallback_slot;
            }
        }
        if (slot_idx_opt == null) return null;

        const obj_ir = try self.compile(arg_cons.car, env);
        const idx_ir = try self.builder.lit(Value.makeFixnum(@intCast(slot_idx_opt.? + 1)));
        const val_ir = try self.compile(value_expr, env);
        return try self.builder.vecSet(obj_ir, idx_ir, val_ir);
    }

    fn compileSetfCxr(
        self: *Compiler,
        accessor_name: []const u8,
        place_args: Value,
        value_expr: Value,
        env: *const Env,
    ) anyerror!?*Ir {
        if (accessor_name.len < 3) return null;
        if (!((accessor_name[0] == 'c' or accessor_name[0] == 'C') and (accessor_name[accessor_name.len - 1] == 'r' or accessor_name[accessor_name.len - 1] == 'R'))) return null;

        const ops = accessor_name[1 .. accessor_name.len - 1];
        if (ops.len == 0) return null;
        for (ops) |ch| {
            const c = std.ascii.toLower(ch);
            if (c != 'a' and c != 'd') return null;
        }

        if (!place_args.isCons()) return error.InvalidSyntax;
        const arg1 = place_args.toPtr(Cons);
        if (!arg1.cdr.isNil()) return error.InvalidSyntax;

        var target_ir = try self.compile(arg1.car, env);
        if (ops.len > 1) {
            var i: usize = ops.len - 1;
            while (i >= 1) : (i -= 1) {
                const c = std.ascii.toLower(ops[i]);
                target_ir = if (c == 'a')
                    try self.builder.car(target_ir)
                else
                    try self.builder.cdr(target_ir);
                if (i == 1) break;
            }
        }

        const val_ir = try self.compile(value_expr, env);
        return switch (std.ascii.toLower(ops[0])) {
            'a' => try self.builder.rplaca(target_ir, val_ir),
            'd' => try self.builder.rplacd(target_ir, val_ir),
            else => unreachable,
        };
    }

    fn compileSetfGlobalCall(
        self: *Compiler,
        env: *const Env,
        value_expr: Value,
        place_args: Value,
        setf_name: []const u8,
        idx: u16,
    ) anyerror!*Ir {
        const val_ir = try self.compile(value_expr, env);
        const setf_ref = try self.builder.globalRef(setf_name, idx);

        var arg_count: usize = 1;
        var p = place_args;
        while (p.isCons()) : (p = p.toPtr(Cons).cdr) arg_count += 1;

        const call_args = try self.allocator.alloc(*Ir, arg_count);
        call_args[0] = val_ir;

        var i: usize = 1;
        p = place_args;
        while (p.isCons()) {
            const arg_cons = p.toPtr(Cons);
            call_args[i] = try self.compile(arg_cons.car, env);
            i += 1;
            p = arg_cons.cdr;
        }

        return try self.builder.call(setf_ref, call_args);
    }

    fn maybeBoxRefFunction(self: *Compiler, sym: Value, func_ir: *Ir) anyerror!*Ir {
        if (self.boxed_fn_syms) |boxed_fn| {
            if (boxed_fn.contains(sym)) {
                const box_ref = try self.allocator.create(Ir);
                box_ref.* = .{ .box_ref = .{ .operand = func_ir } };
                return box_ref;
            }
        }
        return func_ir;
    }

    fn compileQuote(self: *Compiler, args: Value) anyerror!*Ir {
        // (quote expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const quoted = cons.car;

        // Preserve exact symbol identity/package in quoted forms.
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
            if (env.lookupFunctionSym(func_spec)) |binding| {
                const name = func_spec.toPtr(Symbol).getName();
                const fn_ref = try self.builder.variable(name, binding.depth, binding.index);
                return try self.maybeBoxRefFunction(func_spec, fn_ref);
            }
            // Check if it's a primitive that needs wrapping
            if (try self.compilePrimitiveFunctionRef(func_spec)) |wrapper| {
                return wrapper;
            }
            // Resolve global symbols through function namespace semantics.
            // This keeps #'SYMBOL aligned with SYMBOL-FUNCTION/FDEFINITION
            // and avoids value-cell NIL reads for lazily materialized builtins.
            const sym_ir = try self.builder.lit(func_spec);
            return try self.builder.symbolFunction(sym_ir);
        }

        // (function (lambda ...)) - compile the lambda
        if (func_spec.isCons()) {
            const inner = func_spec.toPtr(Cons);
            if (inner.car.isSymbol()) {
                if (self.builtins) |b| {
                    const canonical_head = self.canonicalBuiltinSymbol(inner.car);
                    if (canonical_head.raw == b.lambda.raw or canonical_head.raw == b.@"fn".raw) {
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
        const b = if (self.builtins) |val| val else return null;
        const dispatch_sym = self.canonicalBuiltinSymbol(sym);
        const s = dispatch_sym.raw;

        // Binary primitives: (lambda (a b) (prim a b))
        if (s == b.cons.raw) return try self.makeBinaryWrapper(&IrBuilder.cons);
        if (s == b.eq.raw) return try self.makeBinaryWrapper(&IrBuilder.eq);
        if (s == b.equal.raw) return try self.makeBinaryWrapper(&IrBuilder.equal);
        if (s == b.eql.raw) return try self.makeBinaryWrapper(&IrBuilder.eql);
        if (s == b.equalp.raw) return try self.makeBinaryWrapper(&IrBuilder.equalp);
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
        if (s == b.integerp.raw) return try self.makeUnaryWrapper(&IrBuilder.integerp);
        if (s == b.realp.raw) return try self.makeUnaryWrapper(&IrBuilder.realp);
        if (s == b.stringp.raw) return try self.makeUnaryWrapper(&IrBuilder.stringp);
        if (s == b.intern.raw) return try self.makeInternWrapper();
        if (s == b.atom.raw) return try self.makeUnaryWrapper(&IrBuilder.atomp);
        if (s == b.listp.raw) return try self.makeUnaryWrapper(&IrBuilder.listp);
        if (s == b.@"delete-file".raw) return try self.makeUnaryWrapper(&IrBuilder.deleteFile);
        if (s == b.@"probe-file".raw) return try self.makeUnaryWrapper(&IrBuilder.probeFile);
        if (s == b.@"file-write-date".raw) return try self.makeUnaryWrapper(&IrBuilder.fileWriteDate);
        if (s == b.@"file-author".raw) return try self.makeUnaryWrapper(&IrBuilder.fileAuthor);
        if (s == b.@"make-array".raw) return try self.makeMakeArrayWrapper();
        if (s == b.@"symbol-name".raw) return try self.makeUnaryPrimitiveWrapper(.sym_name);
        if (s == b.@"symbol-package".raw) return try self.makeUnaryWrapper(&IrBuilder.symbolPackage);
        if (s == b.boundp.raw) return try self.makeUnaryWrapper(&IrBuilder.boundp);
        if (s == b.fboundp.raw) return try self.makeUnaryWrapper(&IrBuilder.fboundp);
        if (s == b.@"symbol-value".raw) return try self.makeUnaryWrapper(&IrBuilder.symbolValue);
        if (s == b.@"symbol-function".raw) return try self.makeUnaryWrapper(&IrBuilder.symbolFunction);
        if (s == b.@"copy-structure".raw) return try self.makeUnaryWrapper(&IrBuilder.copyStructure);
        if (s == b.@"function-lambda-expression".raw) return try self.makeUnaryWrapper(&IrBuilder.functionLambdaExpression);
        if (s == b.@"char-code".raw) return try self.makeUnaryWrapper(&IrBuilder.charCode);
        if (s == b.@"code-char".raw) return try self.makeUnaryWrapper(&IrBuilder.codeChar);
        if (s == b.@"char-upcase".raw) return try self.makeUnaryWrapper(&IrBuilder.charUpcase);
        if (s == b.@"char-downcase".raw) return try self.makeUnaryWrapper(&IrBuilder.charDowncase);
        // digit-char-p removed — stdlib handles optional radix
        if (s == b.@"alpha-char-p".raw) return try self.makeUnaryWrapper(&IrBuilder.alphaCharP);
        if (s == b.values.raw) return try self.makeValuesWrapper();

        if (s == b.@"char=".raw) return try self.makeBinaryWrapper(&IrBuilder.charEq);
        if (s == b.@"char<".raw) return try self.makeBinaryWrapper(&IrBuilder.charLt);
        if (s == b.@"char>".raw) return try self.makeBinaryWrapper(&IrBuilder.charGt);

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
        return try self.builder.lambda(&params, &.{}, &.{}, false, null, &.{}, prim_call);
    }

    fn makeUnaryWrapper(self: *Compiler, buildFn: *const fn (IrBuilder, *const Ir) std.mem.Allocator.Error!*Ir) anyerror!*Ir {
        // Create: (lambda (a) (op a))
        const a_ref = try self.builder.variable("a", 0, 0);
        const prim_call = try buildFn(self.builder, a_ref);
        const params = [_][]const u8{"a"};
        return try self.builder.lambda(&params, &.{}, &.{}, false, null, &.{}, prim_call);
    }

    fn makeUnaryPrimitiveWrapper(self: *Compiler, comptime tag: std.meta.Tag(Ir)) anyerror!*Ir {
        const a_ref = try self.builder.variable("a", 0, 0);
        const prim_call = try self.allocator.create(Ir);
        prim_call.* = switch (tag) {
            .sym_name => .{ .sym_name = .{ .operand = a_ref } },
            .intern => .{ .intern = .{ .operand = a_ref } },
            else => unreachable,
        };
        const params = [_][]const u8{"a"};
        return try self.builder.lambda(&params, &.{}, &.{}, false, null, &.{}, prim_call);
    }

    fn makeMakeArrayWrapper(self: *Compiler) anyerror!*Ir {
        // (lambda (dimensions) (make-array dimensions))
        const dims_ref = try self.builder.variable("dimensions", 0, 0);
        const prim_call = try self.builder.arrNewDynamic(dims_ref, null);
        const params = [_][]const u8{"dimensions"};
        return try self.builder.lambda(&params, &.{}, &.{}, false, null, &.{}, prim_call);
    }

    fn makeValuesWrapper(self: *Compiler) anyerror!*Ir {
        // (lambda (&rest args) (values-list args))
        const args_ref = try self.builder.variable("args", 0, 0);
        const body = try self.builder.valuesList(args_ref);
        return try self.builder.lambda(&.{}, &.{}, &.{}, false, "args", &.{}, body);
    }

    fn makeInternWrapper(self: *Compiler) anyerror!*Ir {
        // (lambda (name &optional package) (declare (ignore package)) (intern name))
        // Current runtime INTERN primitive ignores package, but this preserves
        // function designator arity for callers that pass two arguments.
        const name_ref = try self.builder.variable("name", 0, 0);
        const nil_lit = try self.builder.lit(Value.nil);
        const optional = [_]Ir.OptionalParam{
            .{ .name = "package", .default = nil_lit },
        };
        const body = try self.allocator.create(Ir);
        body.* = .{ .intern = .{ .operand = name_ref } };
        return try self.builder.lambda(&.{ "name" }, &optional, &.{}, false, null, &.{}, body);
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

        return try self.builder.lambda(&.{}, &.{}, &.{}, false, "args", &.{}, outer_if);
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

        return try self.builder.lambda(&.{}, &.{}, &.{}, false, "args", &.{}, outer_if);
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

        return try self.builder.lambda(&.{}, &.{}, &.{}, false, "args", &.{}, let_node);
    }

    /// Compile quasiquote (backquote)
    /// Handles unquote (,) and unquote-splicing (,@)
    fn compileQuasiquote(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (quasiquote expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const expr = cons.car;

        return self.quasiquoteExpr(expr, env, 0);
    }

    /// Process an expression inside quasiquote at given nesting depth.
    /// At depth 0, unquotes are evaluated. At depth > 0, they're left as forms.
    fn quasiquoteExpr(self: *Compiler, expr: Value, env: *const Env, depth: u32) anyerror!*Ir {
        // Non-list: return as quoted literal
        if (!expr.isCons()) {
            return try self.builder.lit(expr);
        }

        const cons = expr.toPtr(Cons);
        const head = cons.car;

        // Check for special forms (unquote, unquote-splicing, quasiquote)
        if (head.isSymbol()) {
            const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
            const dispatch_head = self.canonicalBuiltinSymbol(head);
            if (dispatch_head.raw == b.unquote.raw) {
                if (!cons.cdr.isCons()) return error.InvalidSyntax;
                const unquoted = cons.cdr.toPtr(Cons).car;
                if (depth == 0) {
                    // At outermost level: evaluate the expression
                    return self.compile(unquoted, env);
                } else {
                    // Nested: build (unquote <processed>) form at runtime
                    // Process the argument at depth-1
                    const inner = try self.quasiquoteExpr(unquoted, env, depth - 1);
                    return try self.buildList2(try self.builder.lit(head), inner);
                }
            }
            if (dispatch_head.raw == b.@"unquote-splicing".raw) {
                if (depth == 0) {
                    // unquote-splicing outside of list context at depth 0 is an error
                    return error.InvalidSyntax;
                } else {
                    if (!cons.cdr.isCons()) return error.InvalidSyntax;
                    const unquoted = cons.cdr.toPtr(Cons).car;
                    const inner = try self.quasiquoteExpr(unquoted, env, depth - 1);
                    return try self.buildList2(try self.builder.lit(head), inner);
                }
            }
            if (dispatch_head.raw == b.quasiquote.raw) {
                // Nested quasiquote: increment depth
                if (!cons.cdr.isCons()) return error.InvalidSyntax;
                const nested_expr = cons.cdr.toPtr(Cons).car;
                const inner = try self.quasiquoteExpr(nested_expr, env, depth + 1);
                return try self.buildList2(try self.builder.lit(head), inner);
            }
        }

        // Regular list: build with cons at runtime
        return self.quasiquoteList(expr, env, depth);
    }

    /// Helper: build a 2-element list (a b) -> (cons a (cons b nil))
    fn buildList2(self: *Compiler, a: *Ir, b: *Ir) anyerror!*Ir {
        const nil_ir = try self.builder.lit(Value.nil);
        const inner_cons = try self.builder.cons(b, nil_ir);
        return try self.builder.cons(a, inner_cons);
    }

    /// Build a list from quasiquoted elements using cons/append
    fn quasiquoteList(self: *Compiler, list: Value, env: *const Env, depth: u32) anyerror!*Ir {
        if (list.isNil()) {
            return try self.builder.lit(Value.nil);
        }

        if (!list.isCons()) {
            // Improper list tail - just quote it
            return try self.builder.lit(list);
        }

        const cons = list.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Handle dotted pair unquote: (a b . ,x) where list = (UNQUOTE x)
        // The car is the UNQUOTE symbol and cdr is (x).
        if (head.isSymbol()) {
            const b_dot = if (self.builtins) |val| val else return error.UninitializedBuiltins;
            const dh_dot = self.canonicalBuiltinSymbol(head);
            if (dh_dot.raw == b_dot.unquote.raw) {
                if (!tail.isCons()) return error.InvalidSyntax;
                const unquoted = tail.toPtr(Cons).car;
                if (depth == 0) {
                    // Evaluate the unquoted expression (it becomes the tail)
                    return self.compile(unquoted, env);
                } else {
                    // Nested: build (unquote <processed>) form
                    const inner = try self.quasiquoteExpr(unquoted, env, depth - 1);
                    return try self.buildList2(try self.builder.lit(head), inner);
                }
            }
        }

        // Check for (unquote-splicing x) - splice x into result
        if (head.isCons()) {
            const head_cons = head.toPtr(Cons);
            if (head_cons.car.isSymbol()) {
                const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
                const dispatch_head = self.canonicalBuiltinSymbol(head_cons.car);
                if (dispatch_head.raw == b.@"unquote-splicing".raw) {
                    if (!head_cons.cdr.isCons()) return error.InvalidSyntax;
                    const spliced = head_cons.cdr.toPtr(Cons).car;
                    if (depth == 0) {
                        // At outermost: (,@x ...) -> (append x (quasiquote-list ...))
                        const spliced_ir = try self.compile(spliced, env);
                        const rest_ir = try self.quasiquoteList(tail, env, depth);
                        return try self.builder.append(spliced_ir, rest_ir);
                    } else {
                        // Nested: build (unquote-splicing <processed>) as a list element
                        const inner = try self.quasiquoteExpr(spliced, env, depth - 1);
                        const splice_form = try self.buildList2(try self.builder.lit(head_cons.car), inner);
                        const tail_ir = try self.quasiquoteList(tail, env, depth);
                        return try self.builder.cons(splice_form, tail_ir);
                    }
                }
            }
        }

        // Regular element: (cons (quasiquote head) (quasiquote-list tail))
        const head_ir = try self.quasiquoteExpr(head, env, depth);
        const tail_ir = try self.quasiquoteList(tail, env, depth);

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
        return try self.builder.block(Value.nil, loop_ir);
    }

    fn compileLoopSpecial(
        self: *Compiler,
        whole_form: Value,
        head: Value,
        args: Value,
        env: *const Env,
        in_tail: bool,
    ) anyerror!*Ir {
        // During bootstrap, LOOP is a built-in simple loop special form.
        // After stdlib defines LOOP as a macro, expanded LOOP clauses
        // (FOR/COLLECT/...) must macroexpand instead of taking special-form path.
        // Prefer compiler-local macro table first so file loads do not depend on
        // REPL macroexpand callbacks being wired.
        if (self.lookupMacroDef(head)) |macro_def| {
            if (self.vm) |vm| {
                const expanded = try self.expandMacro(macro_def, args, whole_form, vm);
                return self.compileWithTail(expanded, env, in_tail);
            }
        }

        // Fallback path for externally provided macroexpand hooks.
        if (self.vm) |vm| {
            if (vm.macroexpand_1_callback) |macroexpand1| {
                if (vm.macroexpand_1_context) |ctx| {
                    const expanded = try macroexpand1(whole_form, ctx);
                    if (expanded.raw != whole_form.raw) {
                        return self.compileWithTail(expanded, env, in_tail);
                    }
                }
            }
        }
        return self.compileLoop(args, env);
    }

    fn compileBlockWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) anyerror!*Ir {
        // (block name body...) - name can be symbol or nil
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const name = switch (cons.car.typeKind()) {
            .nil => Value.nil, // nil block name (used by dolist/dotimes)
            .symbol => cons.car,
            else => return error.InvalidSyntax,
        };

        // Compile body
        const body_ir = try self.compileBodyWithTail(cons.cdr, env, in_tail);

        return try self.builder.block(name, body_ir);
    }

    fn compileReturnFrom(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (return-from name value) - name can be symbol or nil
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const name = switch (cons.car.typeKind()) {
            .nil => Value.nil,
            .symbol => cons.car,
            else => return error.InvalidSyntax,
        };

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

    fn compileProgv(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (progv symbols values body...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const symbols = cons1.car;

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const values = cons2.car;

        const symbols_ir = try self.compile(symbols, env);
        const values_ir = try self.compile(values, env);
        const body_ir = try self.compileBody(cons2.cdr, env);

        return try self.builder.progv(symbols_ir, values_ir, body_ir);
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

    /// Compile handler-bind
    /// (handler-bind ((condition-type handler-fn) ...) body)
    fn compileHandlerBind(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const args_cons = args.toPtr(Cons);
        const handler_specs = args_cons.car;
        const body_forms = args_cons.cdr;

        // Collect handler bindings
        var handlers = std.ArrayList(ir.Handler){};
        defer handlers.deinit(self.allocator);

        var current = handler_specs;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const spec = cons.car;

            if (!spec.isCons()) return error.InvalidSyntax;
            const spec_cons = spec.toPtr(Cons);

            // First element: condition type designator (symbols are literal).
            const condition_type_expr = spec_cons.car;
            const condition_type_ir = if (condition_type_expr.isSymbol())
                try self.builder.lit(condition_type_expr)
            else
                try self.compile(condition_type_expr, env);

            // Second element: handler function (evaluate at runtime)
            const rest = spec_cons.cdr;
            if (!rest.isCons()) return error.InvalidSyntax;
            const handler_fn_expr = rest.toPtr(Cons).car;
            const handler_fn_ir = try self.compile(handler_fn_expr, env);

            try handlers.append(self.allocator, .{
                .condition_type = condition_type_ir,
                .handler_fn = handler_fn_ir,
            });

            current = cons.cdr;
        }

        // Compile body as zero-arg closure so handlers are active during execution.
        const heap = self.heap orelse return error.OutOfMemory;
        const lambda_args = try heap.allocCons(Value.nil, body_forms);
        const body_ir = try self.compileLambda(lambda_args, env);

        const handlers_slice = try self.allocator.dupe(ir.Handler, handlers.items);

        const node = try self.allocator.create(Ir);
        node.* = .{ .handler_bind = .{
            .body = body_ir,
            .handlers = handlers_slice,
        } };
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
        const cond_idx = try handler_env.bindName(cond_name);

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
        var var_sym: ?Value = null;
        var var_name: []const u8 = "_"; // default if no variable
        if (lambda_list.isCons()) {
            const ll_cons = lambda_list.toPtr(Cons);
            if (ll_cons.car.isSymbol()) {
                var_sym = ll_cons.car;
                var_name = try self.allocator.dupe(u8, ll_cons.car.toPtr(runtime.Symbol).getName());
            }
        }

        // Build handler body with variable binding.
        // CL handler-case binds the full condition object.
        const cond_var_ir2 = try self.builder.variable(cond_name, 0, cond_idx);

        // Create environment with binding
        var inner_env = Env.initLet(self.allocator, env);
        defer inner_env.deinit();
        const var_idx = if (var_sym) |s|
            try inner_env.bindSym(s)
        else
            try inner_env.bindName(var_name);

        // Compile body
        const body_ir = try self.compileBodyWithTail(body, &inner_env, in_tail);

        // Build let node
        const let_ir = try self.builder.let1(var_name, var_idx, cond_var_ir2, body_ir);

        // Check if condition_type is 't' (catch-all handler)
        if (condition_type.raw == Value.t.raw) {
            // t is catch-all, no test needed
            return let_ir;
        }

        // Build: (if (match-condition-type (car cond) 'type) (let ((var (cdr cond))) body...) else)

        // (car cond)
        const cond_var_ir = try self.builder.variable(cond_name, 0, cond_idx);
        const car_cond = try self.builder.car(cond_var_ir);

        const b = self.builtins.?;
        const canonical_type = self.canonicalBuiltinSymbol(condition_type);

        // CONDITION, ERROR, SERIOUS-CONDITION are catch-all supertypes: match any condition
        if (canonical_type.raw == b.@"error".raw or
            canonical_type.raw == b.condition.raw or
            canonical_type.raw == b.@"serious-condition".raw)
        {
            // Always match (non-nil car means a condition was thrown)
            const nil_ir = try self.builder.lit(Value.nil);
            const test_ir = try self.builder.not(try self.builder.eq(car_cond, nil_ir));
            return try self.builder.ifExpr(test_ir, let_ir, else_ir);
        }

        // Build type test including subtypes from the CL condition hierarchy.
        // Start with exact match, then add subtypes.
        var test_ir = try self.builder.eq(car_cond, try self.builder.lit(condition_type));

        // Add subtype checks based on CL hierarchy
        const subtypes = self.getConditionSubtypes(canonical_type);
        for (subtypes) |subtype_sym| {
            const sub_ir = try self.builder.eq(car_cond, try self.builder.lit(subtype_sym));
            // (or test sub) = (if test t sub)
            test_ir = try self.builder.ifExpr(test_ir, try self.builder.lit(Value.t), sub_ir);
        }

        // Build if node
        return try self.builder.ifExpr(test_ir, let_ir, else_ir);
    }

    fn compileTagbody(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (tagbody [tag | form]...)
        // Parse body into tags and segments
        var tags = std.ArrayList(Value){};
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

                try tags.append(self.allocator, elem);
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

        return try self.builder.go(cons.car);
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

        // Create environment with bindings for vars (body scope).
        // Expr is compiled in the outer env per CL semantics.
        var let_env = Env.initLet(self.allocator, env);
        defer let_env.deinit();

        // Parse variable list
        var vars = std.ArrayList([]const u8){};
        defer vars.deinit(self.allocator);

        var var_list = cons1.car;
        var start_index: u16 = 0;
        var first = true;
        while (var_list.isCons()) {
            const var_cons = var_list.toPtr(Cons);
            if (!var_cons.car.isSymbol()) return error.InvalidSyntax;
            const sym_val = var_cons.car;
            const sym = sym_val.toPtr(Symbol);
            const name_copy = try self.allocator.dupe(u8, sym.getName());
            try vars.append(self.allocator, name_copy);
            const idx = try let_env.bindSym(sym_val);
            if (first) {
                start_index = idx;
                first = false;
            }
            var_list = var_cons.cdr;
        }

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);

        // Compile the expression that produces multiple values
        const expr_ir = try self.compile(cons2.car, env);

        // Compile body forms
        const body_ir = try self.compileBody(cons2.cdr, &let_env);

        return try self.builder.mvBind(vars.items, start_index, expr_ir, body_ir);
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
        const q = try self.getQualifiedName(name_sym, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const local_name = name_sym.getName();
        var name = q.name;
        var idx_opt = self.globals.lookup(name);
        if (idx_opt == null) {
            if (self.globals.lookup(local_name)) |idx| {
                idx_opt = idx;
                name = local_name;
            } else {
                const prefixes = [_][]const u8{ "COMMON-LISP:", "CL:", "CL-USER:", "COMMON-LISP-USER:" };
                var full_buf: [640]u8 = undefined;
                for (prefixes) |prefix| {
                    if (prefix.len + local_name.len > full_buf.len) continue;
                    @memcpy(full_buf[0..prefix.len], prefix);
                    @memcpy(full_buf[prefix.len .. prefix.len + local_name.len], local_name);
                    const candidate = full_buf[0 .. prefix.len + local_name.len];
                    if (self.globals.lookup(candidate)) |idx| {
                        idx_opt = idx;
                        name = candidate;
                        break;
                    }
                }
            }
        }

        // Pre-register global for recursive definitions
        const idx = idx_opt orelse try self.globals.define(name);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        // std.debug.print("  Compiling value for define: {s}\n", .{name});
        const value_ir = try self.compile(cons2.car, env);
        if (value_ir.* == .lambda) {
            value_ir.lambda.name = cons1.car;
        }

        return try self.builder.define(name, idx, value_ir);
    }

    fn compileDefvar(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (defvar name [init [doc]])
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSyntax;

        const init_expr = if (cons1.cdr.isCons())
            cons1.cdr.toPtr(Cons).car
        else
            Value.nil;

        const heap = if (self.heap) |val| val else return error.InvalidSyntax;
        const def_tail = try heap.allocCons(cons1.car, try heap.allocCons(init_expr, Value.nil));
        return self.compileDefine(def_tail, env);
    }

    fn compileDefun(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (defun name (params...) body...) -> (define name (lambda (params...) body...))
        // (defun (name -> type) (params...) body...) -> with return type assertion
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const name_spec = cons1.car;

        var name_sym_saved: ?*const Symbol = null;
        var name_val: Value = undefined;
        var return_type: ?Value = null;
        var setf_name_owned: ?[]u8 = null;
        defer if (setf_name_owned) |setf_name| self.allocator.free(setf_name);

        switch (name_spec.typeKind()) {
            .symbol => {
                // Simple: (defun name ...)
                name_sym_saved = name_spec.toPtr(Symbol);
                name_val = name_spec;
            },
            .cons => {
                const spec_cons = name_spec.toPtr(Cons);
                if (!spec_cons.car.isSymbol()) return error.InvalidSyntax;
                const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;

                // SETF function name: (defun (setf foo) ...)
                if (self.canonicalBuiltinSymbol(spec_cons.car).raw == b.setf.raw) {
                    if (!spec_cons.cdr.isCons()) return error.InvalidSyntax;
                    const name_cons = spec_cons.cdr.toPtr(Cons);
                    if (!name_cons.car.isSymbol() or !name_cons.cdr.isNil()) return error.InvalidSyntax;
                    const base_name = name_cons.car.toPtr(Symbol).getName();
                    setf_name_owned = try std.fmt.allocPrint(self.allocator, "(SETF {s})", .{base_name});
                    name_val = name_spec;
                } else {
                    // Typed: (defun (name -> type) ...)
                    name_sym_saved = spec_cons.car.toPtr(Symbol);
                    name_val = spec_cons.car;

                    // Check for -> arrow (use symbol identity)
                    if (!spec_cons.cdr.isCons()) return error.InvalidSyntax;
                    const arrow_cons = spec_cons.cdr.toPtr(Cons);
                    if (arrow_cons.car.raw != b.@"->".raw) return error.InvalidSyntax;

                    // Get return type (symbol or complex type like (or T1 T2))
                    if (!arrow_cons.cdr.isCons()) return error.InvalidSyntax;
                    const type_cons = arrow_cons.cdr.toPtr(Cons);
                    // Accept symbol or cons (complex type expression)
                    if (!type_cons.car.isSymbol() and !type_cons.car.isCons()) return error.InvalidSyntax;
                    return_type = type_cons.car;
                }
            },
            else => return error.InvalidSyntax,
        }

        // Pre-register the global so recursive calls work
        var local_name: []const u8 = undefined;
        var name: []const u8 = undefined;
        var qual_sym_buf: [256]u8 = undefined;
        var qual_setf_buf: [512]u8 = undefined;

        if (setf_name_owned) |setf_name| {
            const q_setf = try self.qualifyName(setf_name, &qual_setf_buf);
            defer if (q_setf.owned) self.allocator.free(q_setf.name);
            local_name = setf_name;
            name = q_setf.name;
        } else {
            const sym = name_sym_saved orelse return error.InvalidSyntax;
            const q = try self.getQualifiedName(sym, &qual_sym_buf);
            defer if (q.owned) self.allocator.free(q.name);
            local_name = sym.getName();
            name = q.name;
        }

        var idx_opt = self.globals.lookup(name);
        if (idx_opt == null) {
            if (self.globals.lookup(local_name)) |idx| {
                idx_opt = idx;
                name = local_name;
            } else {
                const prefixes = [_][]const u8{ "COMMON-LISP:", "CL:", "CL-USER:", "COMMON-LISP-USER:" };
                var full_buf: [640]u8 = undefined;
                for (prefixes) |prefix| {
                    if (prefix.len + local_name.len > full_buf.len) continue;
                    @memcpy(full_buf[0..prefix.len], prefix);
                    @memcpy(full_buf[prefix.len .. prefix.len + local_name.len], local_name);
                    const candidate = full_buf[0 .. prefix.len + local_name.len];
                    if (self.globals.lookup(candidate)) |idx| {
                        idx_opt = idx;
                        name = candidate;
                        break;
                    }
                }
            }
        }
        const idx = idx_opt orelse try self.globals.define(name);

        // Rest is (params...) body...
        const lambda_args = cons1.cdr;
        const lambda_ir = try self.compileLambdaWithReturnType(lambda_args, env, return_type);
        if (lambda_ir.* == .lambda) {
            lambda_ir.lambda.name = name_val;
        }

        // A plain function definition overrides any previously tracked generic
        // metadata for this name so later DEFMETHOD can recreate a GF.
        self.removeGenericFunctionMeta(name);
        if (!std.mem.eql(u8, local_name, name)) {
            self.removeGenericFunctionMeta(local_name);
        }

        return try self.builder.define(name, idx, lambda_ir);
    }

    fn removeGenericFunctionMeta(self: *Compiler, name: []const u8) void {
        if (self.generic_functions.fetchRemove(name)) |removed| {
            self.globals.allocator.free(removed.key);
            for (removed.value.items) |method| {
                self.globals.allocator.free(method.specializers);
                self.globals.allocator.free(method.function_name);
            }
            var methods = removed.value;
            methods.deinit(self.globals.allocator);
        }
    }

    /// Build cons list from slice
    fn listFromSlice(self: *Compiler, items: []const Value) !Value {
        const heap = if (self.heap) |val| val else return error.InvalidSyntax;
        var result = Value.nil;
        var i = items.len;
        while (i > 0) {
            i -= 1;
            result = try heap.allocCons(items[i], result);
        }
        return result;
    }

    /// Transform destructured params: ((a b) &body c) -> (g123 &body c) + wrap body
    /// Returns ((new-params...) wrapped-body...)
    pub fn transformDestructuredParams(self: *Compiler, lambda_args: Value) !Value {
        const heap = if (self.heap) |val| val else return error.InvalidSyntax;
        const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;

        // lambda_args is ((params...) body...)
        const args_cons = lambda_args.toPtr(Cons);
        const params = args_cons.car;
        const body = args_cons.cdr;

        // Scan params for destructured (cons) params, &optional, &rest, &key
        var new_params = std.ArrayList(Value){};
        defer new_params.deinit(self.allocator);
        var bindings = std.ArrayList(Value){}; // (pattern gensym) pairs
        defer bindings.deinit(self.allocator);

        var p = params;
        var in_optional = false;
        var in_rest = false;
        var in_key = false;

        while (p.isCons()) {
            const p_cons = p.toPtr(Cons);
            const param = p_cons.car;

            if (param.eq(b.@"&optional")) {
                try new_params.append(self.allocator, param);
                in_optional = true;
                in_rest = false;
                in_key = false;
            } else if (param.eq(b.@"&rest") or param.eq(b.@"&body")) {
                try new_params.append(self.allocator, param);
                in_optional = false;
                in_rest = true;
                in_key = false;
            } else if (param.eq(b.@"&key")) {
                try new_params.append(self.allocator, param);
                in_optional = false;
                in_rest = false;
                in_key = true;
            } else if (param.eq(b.@"&aux")) {
                // &aux: collect remaining params as aux bindings, stop param scanning
                var aux_rest = p_cons.cdr;
                var aux_bindings = std.ArrayList(Value){};
                defer aux_bindings.deinit(self.allocator);
                while (aux_rest.isCons()) {
                    const aux_cons = aux_rest.toPtr(Cons);
                    try aux_bindings.append(self.allocator, aux_cons.car);
                    aux_rest = aux_cons.cdr;
                }
                // Wrap body in (let* ((var1 nil) (var2 nil) ...) body...)
                if (aux_bindings.items.len > 0) {
                    var let_bindings = std.ArrayList(Value){};
                    defer let_bindings.deinit(self.allocator);
                    for (aux_bindings.items) |ab| {
                        if (ab.isCons()) {
                            // (var init-form) - keep as-is
                            try let_bindings.append(self.allocator, ab);
                        } else {
                            // bare symbol - bind to nil
                            try let_bindings.append(self.allocator, try heap.allocCons(ab, try heap.allocCons(Value.nil, Value.nil)));
                        }
                    }
                    const bindings_list = try self.listFromSlice(let_bindings.items);
                    const let_star_sym = try heap.intern("let*");
                    // (let* bindings body...)
                    const new_body = try heap.allocCons(let_star_sym, try heap.allocCons(bindings_list, body));
                    // Rebuild with params up to &aux and new body
                    const new_params_list = try self.listFromSlice(new_params.items);

                    // If we have destructuring bindings, wrap further
                    if (bindings.items.len > 0) {
                        var wrapped = try heap.allocCons(new_body, Value.nil);
                        var bi = bindings.items.len;
                        while (bi >= 2) {
                            bi -= 2;
                            const pattern = bindings.items[bi];
                            const g = bindings.items[bi + 1];
                            const db_sym = try heap.intern("destructuring-bind");
                            const progn_sym = try heap.intern("progn");
                            const progn_body = try heap.allocCons(progn_sym, wrapped);
                            const progn_cell = try heap.allocCons(progn_body, Value.nil);
                            const g_cell = try heap.allocCons(g, progn_cell);
                            const pat_cell = try heap.allocCons(pattern, g_cell);
                            wrapped = try heap.allocCons(try heap.allocCons(db_sym, pat_cell), Value.nil);
                        }
                        return try heap.allocCons(new_params_list, wrapped);
                    }
                    return try heap.allocCons(new_params_list, try heap.allocCons(new_body, Value.nil));
                }
                break;
            } else if (param.eq(b.@"&whole") or param.eq(b.@"&environment")) {
                // &whole and &environment are handled in expandMacro - keep as-is
                try new_params.append(self.allocator, param);
            } else {
                switch (param.typeKind()) {
                    .cons => {
                        if (!in_optional and !in_rest and !in_key) {
                            // All cons params in macro lambda lists are destructuring.
                            // E.g., (a b) or (a . b) — bind parts of the argument.
                            const g = try prims.gensym(heap, null);
                            try new_params.append(self.allocator, g);
                            try bindings.append(self.allocator, param);
                            try bindings.append(self.allocator, g);
                        } else {
                            // Normal param or &optional/&key param with default: keep as-is
                            try new_params.append(self.allocator, param);
                        }
                    },
                    else => {
                        // Normal param or &optional/&key param with default: keep as-is
                        try new_params.append(self.allocator, param);
                    },
                }
            }

            p = p_cons.cdr;
        }

        // If no bindings, return original
        if (bindings.items.len == 0) {
            return lambda_args;
        }

        // Build new params list
        const new_params_list = try self.listFromSlice(new_params.items);

        // Wrap body with (destructuring-bind pattern gensym ...) for each binding
        var wrapped_body = body;
        var i = bindings.items.len;
        while (i >= 2) {
            i -= 2;
            const pattern = bindings.items[i];
            const g = bindings.items[i + 1];

            // (destructuring-bind pattern g (progn body...))
            const db_sym = try heap.intern("destructuring-bind");
            const progn_sym = try heap.intern("progn");

            // (progn body...)
            const progn_body = try heap.allocCons(progn_sym, wrapped_body);

            // (destructuring-bind pattern g progn-body)
            const progn_cell = try heap.allocCons(progn_body, Value.nil);
            const g_cell = try heap.allocCons(g, progn_cell);
            const pat_cell = try heap.allocCons(pattern, g_cell);
            const db_form = try heap.allocCons(db_sym, pat_cell);
            wrapped_body = try heap.allocCons(db_form, Value.nil);
        }

        // Return (new-params wrapped-body)
        return try heap.allocCons(new_params_list, wrapped_body);
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

        // Rest is ((params...) body...)
        const lambda_args = cons1.cdr;
        if (!lambda_args.isCons()) return error.InvalidSyntax;

        // Transform destructured params: ((a (b c)) body) -> ((a g123) (d-bind (b c) g123 body))
        const transformed = try self.transformDestructuredParams(lambda_args);

        // Store in macro_table: symbol -> transformed-lambda-args
        try self.macro_table.put(name_val, transformed);

        // defmacro has no runtime effect - return nil
        return try self.builder.lit(Value.nil);
    }

    /// Compile macrolet: (macrolet ((name (params) body)...) forms...)
    /// Establishes local macro definitions for the duration of body evaluation.
    fn compileMacrolet(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // Parse: (((name1 (params1) body1) (name2 (params2) body2)...) forms...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const macro_defs = cons1.car;
        const body = cons1.cdr;

        // Save old macro definitions for restoration
        const SavedMacro = struct { name: Value, def: ?Value };
        var saved_macros: std.ArrayList(SavedMacro) = .{};
        defer saved_macros.deinit(self.allocator);

        // Process each macro definition
        var defs = macro_defs;
        while (defs.isCons()) {
            const def_cons = defs.toPtr(Cons);
            const def = def_cons.car;
            defs = def_cons.cdr;

            if (!def.isCons()) return error.InvalidSyntax;

            const name_cons = def.toPtr(Cons);
            const name = name_cons.car;
            if (!name.isSymbol()) return error.InvalidSyntax;

            // Save old definition (if any)
            const old_def = self.macro_table.get(name);
            try saved_macros.append(self.allocator, .{ .name = name, .def = old_def });

            // Rest is ((params...) body...)
            const lambda_args = name_cons.cdr;
            if (!lambda_args.isCons()) return error.InvalidSyntax;

            // Transform destructured params
            const transformed = try self.transformDestructuredParams(lambda_args);

            // Add to macro table
            try self.macro_table.put(name, transformed);
        }

        // Compile body with local macros in effect
        const body_ir = try self.compileBody(body, env);

        // Restore old macro definitions
        for (saved_macros.items) |saved| {
            if (saved.def) |def| {
                try self.macro_table.put(saved.name, def);
            } else {
                _ = self.macro_table.remove(saved.name);
            }
        }

        return body_ir;
    }

    /// Compile symbol-macrolet: (symbol-macrolet ((sym expansion)...) forms...)
    /// Establishes local symbol macros for the duration of body evaluation.
    fn compileSymbolMacrolet(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // Parse: (((sym1 expansion1) (sym2 expansion2)...) forms...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const sym_defs = cons1.car;
        const body = cons1.cdr;

        // Save old symbol macro definitions for restoration
        const SavedSym = struct { name: Value, def: ?Value };
        var saved_syms: std.ArrayList(SavedSym) = .{};
        defer saved_syms.deinit(self.allocator);

        // Process each symbol macro definition
        var defs = sym_defs;
        while (defs.isCons()) {
            const def_cons = defs.toPtr(Cons);
            const def = def_cons.car;
            defs = def_cons.cdr;

            if (!def.isCons()) return error.InvalidSyntax;

            const bind_cons = def.toPtr(Cons);
            const name = bind_cons.car;
            if (!name.isSymbol()) return error.InvalidSyntax;

            // Get expansion form
            const rest = bind_cons.cdr;
            if (!rest.isCons()) return error.InvalidSyntax;
            const expansion = rest.toPtr(Cons).car;

            // Save old definition (if any)
            const old_def = self.symbol_macros.get(name);
            try saved_syms.append(self.allocator, .{ .name = name, .def = old_def });

            // Add to symbol macro table
            try self.symbol_macros.put(name, expansion);
        }

        // Compile body with local symbol macros in effect
        const body_ir = try self.compileBody(body, env);

        // Restore old symbol macro definitions
        for (saved_syms.items) |saved| {
            if (saved.def) |def| {
                try self.symbol_macros.put(saved.name, def);
            } else {
                _ = self.symbol_macros.remove(saved.name);
            }
        }

        return body_ir;
    }

    /// Compile destructuring-bind: (destructuring-bind pattern expr &rest body)
    /// Binds pattern to expr and evaluates body with those bindings.
    fn compileDestructuringBind(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // Parse: (pattern expr body...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const pattern = cons1.car;

        const rest1 = cons1.cdr;
        if (!rest1.isCons()) return error.InvalidSyntax;
        const cons2 = rest1.toPtr(Cons);
        const expr = cons2.car;
        const body = cons2.cdr;

        // Strategy: (destructuring-bind (a b) expr body...)
        // => (let ((#t expr)) (let ((a (car #t)) (b (cadr #t))) body...))

        // Compile expr in current env
        const expr_ir = try self.compile(expr, env);

        // Create temp var for expr result
        const temp_name = "#destruct-temp";
        var temp_env = Env.initLet(self.allocator, env);
        defer temp_env.deinit();
        const temp_idx = try temp_env.bindName(temp_name);

        // Generate bindings from pattern, using temp var
        const bindings = try self.genDestructBindings(pattern, temp_name, temp_idx);

        // Compile body with all bindings in scope
        var body_env = Env.initLet(self.allocator, &temp_env);
        defer body_env.deinit();
        var ir_bindings = try self.allocator.alloc(Ir.Binding, bindings.items.len);
        defer self.allocator.free(ir_bindings);
        for (bindings.items, 0..) |b, i| {
            const idx = try body_env.bindSym(b.sym);
            ir_bindings[i] = .{ .name = b.name, .index = idx, .value = b.init };
        }
        const body_ir = try self.compileProgn(body, &body_env);

        // Build nested let: (let ((temp expr)) (let ((a ...) (b ...)) body))
        const inner_let = try self.builder.letExpr(ir_bindings, body_ir);

        // Outer let binds temp
        const temp_binding = [_]Ir.Binding{.{ .name = temp_name, .index = temp_idx, .value = expr_ir }};
        return try self.builder.letExpr(&temp_binding, inner_let);
    }

    const PatBinding = struct {
        sym: Value,
        name: []const u8,
        init: *const Ir,
    };

    const Binding = struct {
        name: []const u8,
        init: *const Ir,
    };

    const DestructResult = struct {
        bindings: std.ArrayList(Binding),
        /// Intermediate IR nodes created during destructuring (cdr nodes)
        /// Caller must free these
        intermediates: std.ArrayList(*const Ir),

        fn deinit(self: *DestructResult, allocator: std.mem.Allocator) void {
            self.bindings.deinit(allocator);
            self.intermediates.deinit(allocator);
        }
    };

    /// Generate bindings from destructuring pattern
    /// temp_idx is the variable index of the temp var holding the expr result
    fn genDestructBindings(self: *Compiler, pattern: Value, temp_name: []const u8, temp_idx: u16) !std.ArrayList(PatBinding) {
        var bindings = std.ArrayList(PatBinding){};
        errdefer bindings.deinit(self.allocator);

        // Start with a var reference to the temp (depth=0 since it's in same scope)
        const temp_ir = try self.builder.variable(temp_name, 0, temp_idx);
        try self.genDestructBindingsRec(pattern, temp_ir, &bindings);
        return bindings;
    }

    fn genDestructBindingsRec(self: *Compiler, pattern: Value, expr_ir: *const Ir, bindings: *std.ArrayList(PatBinding)) !void {
        switch (pattern.typeKind()) {
            .symbol => {
                // Simple var binding
                const sym_val = pattern;
                const sym = sym_val.toPtr(Symbol);
                const name_copy = try self.allocator.dupe(u8, sym.getName());
                try bindings.append(self.allocator, .{
                    .sym = sym_val,
                    .name = name_copy,
                    .init = expr_ir,
                });
            },
            .cons => {
                // Recursive destructuring (car pattern) (cdr pattern)
                const p = pattern.toPtr(Cons);
                const car_pat = p.car;
                const cdr_pat = p.cdr;

                if (car_pat.typeKind() == .nil or car_pat.typeKind() == .t) {
                    return error.InvalidSyntax;
                }

                // car binding
                const car_ir = try self.builder.car(expr_ir);
                try self.genDestructBindingsRec(car_pat, car_ir, bindings);

                // cdr binding
                if (cdr_pat.isNil()) return;
                if (cdr_pat.typeKind() == .t) return error.InvalidSyntax;
                const cdr_ir = try self.builder.cdr(expr_ir);
                try self.genDestructBindingsRec(cdr_pat, cdr_ir, bindings);
            },
            else => {}, // nil or unsupported patterns: ignore
        }
    }

    /// Destructuring parameter tree node for defmacro
    pub const DestructParam = struct {
        pub const Kind = enum {
            simple, // Simple symbol parameter
            nested, // Nested list destructuring
            optional, // &optional parameter
            rest, // &rest parameter
            key, // &key parameter
        };
        kind: Kind,
        name: ?[]const u8, // For simple, optional, rest, key
        children: ?[]DestructParam, // For nested lists
        default_expr: ?Value, // For optional/key defaults
        keyword: ?[]const u8, // For key parameters

        fn deinit(self: *DestructParam, alloc: std.mem.Allocator) void {
            if (self.children) |ch| {
                for (ch) |*child| {
                    child.deinit(alloc);
                }
                alloc.free(ch);
            }
        }
    };

    /// Parse destructuring parameter list supporting nested lists and lambda-list keywords
    fn parseDestructParams(self: *Compiler, params: Value) ![]DestructParam {
        var result = std.ArrayList(DestructParam){};
        errdefer {
            for (result.items) |*item| {
                item.deinit(self.allocator);
            }
            result.deinit(self.allocator);
        }

        var in_optional = false;
        var in_key = false;
        var param_list = params;

        while (param_list.isCons()) {
            const cons = param_list.toPtr(Cons);
            const item = cons.car;

            switch (item.typeKind()) {
                .symbol => {
                    const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;

                    // Check for lambda-list keywords
                    if (item.raw == b.@"&optional".raw) {
                        in_optional = true;
                        in_key = false;
                        param_list = cons.cdr;
                        continue;
                    }

                    if (item.raw == b.@"&rest".raw or item.raw == b.@"&body".raw) {
                        if (!cons.cdr.isCons()) return error.InvalidSyntax;
                        const rest_cons = cons.cdr.toPtr(Cons);
                        if (!rest_cons.car.isSymbol()) return error.InvalidSyntax;
                        const rest_sym = rest_cons.car.toPtr(Symbol);
                        try result.append(self.allocator, .{
                            .kind = .rest,
                            .name = rest_sym.getName(),
                            .children = null,
                            .default_expr = null,
                            .keyword = null,
                        });
                        break; // &rest must be last
                    }

                    if (item.raw == b.@"&key".raw) {
                        in_key = true;
                        in_optional = false;
                        param_list = cons.cdr;
                        continue;
                    }

                    // Simple symbol parameter
                    const sym = item.toPtr(Symbol);
                    const name = sym.getName();

                    if (in_key) {
                        try result.append(self.allocator, .{
                            .kind = .key,
                            .name = name,
                            .children = null,
                            .default_expr = null,
                            .keyword = name,
                        });
                    } else if (in_optional) {
                        try result.append(self.allocator, .{
                            .kind = .optional,
                            .name = name,
                            .children = null,
                            .default_expr = null,
                            .keyword = null,
                        });
                    } else {
                        try result.append(self.allocator, .{
                            .kind = .simple,
                            .name = name,
                            .children = null,
                            .default_expr = null,
                            .keyword = null,
                        });
                    }
                },
                .cons => {
                    const nested = item.toPtr(Cons);

                    if (in_optional or in_key) {
                        if (in_key) {
                            // &key accepts (var default) and ((:keyword var) default)
                            const spec = parseKeyParamSpec(item) orelse return error.InvalidSyntax;
                            try result.append(self.allocator, .{
                                .kind = .key,
                                .name = spec.param_name,
                                .children = null,
                                .default_expr = spec.default_expr,
                                .keyword = spec.keyword_name,
                            });
                        } else {
                            // (name default) form
                            if (!nested.car.isSymbol()) return error.InvalidSyntax;
                            const name_sym = nested.car.toPtr(Symbol);
                            const name = name_sym.getName();
                            var default_expr: ?Value = null;
                            if (nested.cdr.isCons()) {
                                default_expr = nested.cdr.toPtr(Cons).car;
                            }
                            try result.append(self.allocator, .{
                                .kind = .optional,
                                .name = name,
                                .children = null,
                                .default_expr = default_expr,
                                .keyword = null,
                            });
                        }
                    } else {
                        // Nested destructuring list
                        const children = try self.parseDestructParams(item);
                        try result.append(self.allocator, .{
                            .kind = .nested,
                            .name = null,
                            .children = children,
                            .default_expr = null,
                            .keyword = null,
                        });
                    }
                },
                else => return error.InvalidSyntax,
            }

            param_list = cons.cdr;
        }

        // Handle dotted rest parameter: (a b . rest)
        if (!param_list.isNil() and param_list.isSymbol()) {
            const rest_sym = param_list.toPtr(Symbol);
            try result.append(self.allocator, .{
                .kind = .rest,
                .name = rest_sym.getName(),
                .children = null,
                .default_expr = null,
                .keyword = null,
            });
        }

        return result.toOwnedSlice(self.allocator);
    }
    /// Generate destructuring bindings from parsed parameter tree
    /// Returns list of bindings to extract from args_expr
    fn genDestructCode(self: *Compiler, params: []const DestructParam, args_expr: *const Ir, env: *const Env) !DestructResult {
        var bindings = std.ArrayList(Binding){};
        errdefer bindings.deinit(self.allocator);
        var intermediates = std.ArrayList(*const Ir){};
        errdefer intermediates.deinit(self.allocator);

        // Process each parameter, walking args with cdr
        var current_expr = args_expr;
        for (params, 0..) |param, i| {
            switch (param.kind) {
                .simple => {
                    // Simple: bind name to (car current)
                    const car_ir = try self.builder.car(current_expr);
                    try bindings.append(self.allocator, .{
                        .name = param.name.?,
                        .init = car_ir,
                    });
                    // Advance to cdr for next param
                    if (i + 1 < params.len) {
                        const cdr_ir = try self.builder.cdr(current_expr);
                        try intermediates.append(self.allocator, cdr_ir);
                        current_expr = cdr_ir;
                    }
                },
                .nested => {
                    // Nested: recursively destructure (car current)
                    const car_ir = try self.builder.car(current_expr);
                    try intermediates.append(self.allocator, car_ir);
                    var nested = try self.genDestructCode(param.children.?, car_ir, env);
                    defer nested.deinit(self.allocator);
                    try bindings.appendSlice(self.allocator, nested.bindings.items);
                    try intermediates.appendSlice(self.allocator, nested.intermediates.items);
                    // Advance to cdr
                    if (i + 1 < params.len) {
                        const cdr_ir = try self.builder.cdr(current_expr);
                        try intermediates.append(self.allocator, cdr_ir);
                        current_expr = cdr_ir;
                    }
                },
                .optional => {
                    // Optional: bind to (car current)
                    const car_ir = try self.builder.car(current_expr);
                    try bindings.append(self.allocator, .{
                        .name = param.name.?,
                        .init = car_ir,
                    });
                    if (i + 1 < params.len) {
                        const cdr_ir = try self.builder.cdr(current_expr);
                        try intermediates.append(self.allocator, cdr_ir);
                        current_expr = cdr_ir;
                    }
                },
                .rest => {
                    // Rest: bind to remaining list (no car/cdr)
                    try bindings.append(self.allocator, .{
                        .name = param.name.?,
                        .init = current_expr,
                    });
                    // Rest is always last, don't advance
                },
                .key => {
                    // Key: search for :keyword in remaining args
                    // Implementation TBD - for now just bind nil
                    const nil_ir = try self.builder.lit(Value.nil);
                    try bindings.append(self.allocator, .{
                        .name = param.name.?,
                        .init = nil_ir,
                    });
                },
            }
        }

        return .{
            .bindings = bindings,
            .intermediates = intermediates,
        };
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

    /// Compile load-time-value: (load-time-value form [read-only-p])
    /// Evaluates form at load time (compile time in our case) and returns the result.
    fn compileLoadTimeValue(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        _ = env;
        // Parse: (form [read-only-p])
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const form = cons1.car;
        // read-only-p is optional and ignored in our implementation

        // Need VM to evaluate at compile time
        const vm = if (self.vm) |val| val else return error.InvalidSyntax;
        const heap = if (self.heap) |val| val else return error.InvalidSyntax;

        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Compile the form
        var eval_compiler = try Compiler.initWithHeap(arena_alloc, vm);
        defer eval_compiler.deinit();

        // Copy macro table for consistency
        var iter = self.macro_table.iterator();
        while (iter.next()) |entry| {
            try eval_compiler.macro_table.put(entry.key_ptr.*, entry.value_ptr.*);
        }

        var empty_env = Env.init(arena_alloc, null);
        defer empty_env.deinit();
        const form_ir = try eval_compiler.compile(form, &empty_env);

        // Wrap in a thunk (lambda with no args) so it returns the value
        const thunk_ir = try eval_compiler.builder.lambda(
            &[_][]const u8{},
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            false,
            null,
            &[_]Ir.Capture{},
            form_ir,
        );

        // Emit to bytecode
        var emitter = Emitter.initWithHeap(arena_alloc, heap);
        emitter.speed = self.optimize_current.speed;
        emitter.safety = self.optimize_current.safety;
        defer emitter.deinit();
        try emitter.emit(thunk_ir);

        // Get child chunks and main chunk
        const child_chunks = try emitter.getChildChunks();

        const chunk_val = try emitter.finalize();

        const saved_state = vm_mod.State.save(vm);
        const saved_env = vm.global_env;
        const saved_ext = vm.ext_roots;
        const saved_pool = vm.chunk_pool;

        const pool_roots = try self.allocator.alloc(Value, saved_pool.len);
        defer self.allocator.free(pool_roots);
        const chunk_ptrs = try self.allocator.alloc(*Chunk, child_chunks.len);
        defer self.allocator.free(chunk_ptrs);
        for (saved_pool, 0..) |ptr, i| {
            pool_roots[i] = Value.makeChunk(ptr);
        }
        for (child_chunks, 0..) |cv, i| {
            chunk_ptrs[i] = cv.toPtr(Chunk);
        }

        vm.setExtRoots(pool_roots);
        vm.setChunkPool(chunk_ptrs);
        defer {
            for (saved_pool, 0..) |*slot, i| {
                const v = pool_roots[i];
                if (!v.isNil()) {
                    slot.* = v.toPtr(Chunk);
                }
            }

            vm.setExtRoots(saved_ext);
            vm.global_env = saved_env;
            saved_state.restore(vm);
        }

        const chunk_ptr = chunk_val.toPtr(Chunk);
        const thunk_factory = try heap.allocClosure(chunk_val, chunk_ptr.arity, &[_]Value{});
        const thunk_val = try vm.callFromStackAt(vm.sp, thunk_factory, &[_]Value{});
        if (!thunk_val.isClosure()) return error.InvalidSyntax;
        const result = try vm.callFromStackAt(vm.sp, thunk_val, &[_]Value{});

        // Return as literal
        return try self.builder.lit(result);
    }

    // ========================================================================
    // Package Support
    // ========================================================================

    /// Compile defpackage: (defpackage "name" (:use "other-pkg") (:export "sym1" "sym2"))
    /// Creates a new package with the given name
    fn compileDefpackage(self: *Compiler, args: Value) anyerror!*Ir {
        const heap = if (self.heap) |val| val else return error.InvalidSyntax;
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const pkg_name_val = cons1.car;

        // Get package name from string or symbol
        const pkg_name = switch (pkg_name_val.typeKind()) {
            .string => pkg_name_val.toPtr(runtime.String).bytes(),
            .symbol => pkg_name_val.toPtr(Symbol).getName(),
            .keyword => pkg_name_val.toPtr(runtime.Keyword).getName(),
            else => return error.InvalidSyntax,
        };

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
                            const use_pkg_name = if (self.getStringOrSymbolName(use_cons.car)) |val| val else return error.InvalidSyntax;
                            const use_pkg = try heap.findOrCreatePackage(use_pkg_name);
                            try pkg.usePackage(use_pkg);
                            use_list = use_cons.cdr;
                        }
                    } else if (kw.raw == b.kw_export.raw) {
                        // (:export "sym1" "sym2" ...)
                        var export_list = opt_list.cdr;
                        while (export_list.isCons()) {
                            const export_cons = export_list.toPtr(Cons);
                            const export_name = if (self.getStringOrSymbolName(export_cons.car)) |val| val else return error.InvalidSyntax;
                            try pkg.exportSymbol(export_name);
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
    /// Runtime semantics: (setf *package* (find-package <name>))
    fn compileInPackage(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const heap = if (self.heap) |val| val else return error.InvalidSyntax;
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const pkg_name = if (self.getStringOrSymbolName(cons1.car)) |val| val else return error.InvalidSyntax;
        const pkg_name_val = try heap.allocBaseString(pkg_name);

        const pkg_sym = (try heap.internInPackage("CL", "*PACKAGE*")) orelse return error.InvalidSyntax;
        const find_pkg_sym = (try heap.internInPackage("CL", "FIND-PACKAGE")) orelse return error.InvalidSyntax;
        const setf_sym = self.builtins.?.setf;

        // IN-PACKAGE package designator is read-time data, not a runtime variable.
        // Normalize to a string literal to avoid symbol-evaluation pitfalls.
        const find_pkg_args = try heap.allocCons(pkg_name_val, Value.nil);
        const find_pkg_call = try heap.allocCons(find_pkg_sym, find_pkg_args);
        const setf_rest = try heap.allocCons(find_pkg_call, Value.nil);
        const setf_args = try heap.allocCons(pkg_sym, setf_rest);
        const setf_expr = try heap.allocCons(setf_sym, setf_args);
        return try self.compile(setf_expr, env);
    }

    /// Compile export: (export symbols &optional package)
    /// Runtime semantics: evaluate arguments, then export symbols from package.
    fn compileExport(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return try self.builder.lit(Value.nil);

        const cons1 = args.toPtr(Cons);
        const symbols_ir = try self.compile(cons1.car, env);

        const pkg_ir = if (cons1.cdr.isCons()) blk: {
            const cons2 = cons1.cdr.toPtr(Cons);
            break :blk try self.compile(cons2.car, env);
        } else blk: {
            // Default package must be runtime *package*, not compile-time current package.
            if (env.lookupSymbolName("*PACKAGE*")) |binding| {
                break :blk try self.builder.variable("*PACKAGE*", binding.depth, binding.index);
            }

            const heap = if (self.heap) |val| val else return error.InvalidSyntax;
            const pkg_sym = if (heap.cl_package) |cl_pkg|
                cl_pkg.findAccessibleUpper("*PACKAGE*") orelse try heap.intern("*package*")
            else
                try heap.intern("*package*");
            break :blk try self.compile(pkg_sym, env);
        };

        const node = try self.allocator.create(Ir);
        node.* = .{ .pkg_export = .{ .left = symbols_ir, .right = pkg_ir } };
        return node;
    }

    /// Compile use-package: (use-package packages &optional package)
    /// Runtime semantics: evaluate arguments, then update target package use-list.
    fn compileUsePackage(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return try self.builder.lit(Value.nil);

        const cons1 = args.toPtr(Cons);
        const packages_ir = try self.compile(cons1.car, env);

        const pkg_ir = if (cons1.cdr.isCons()) blk: {
            const cons2 = cons1.cdr.toPtr(Cons);
            break :blk try self.compile(cons2.car, env);
        } else blk: {
            // Default package must be runtime *package*, not compile-time current package.
            if (env.lookupSymbolName("*PACKAGE*")) |binding| {
                break :blk try self.builder.variable("*PACKAGE*", binding.depth, binding.index);
            }

            const heap = if (self.heap) |val| val else return error.InvalidSyntax;
            const pkg_sym = if (heap.cl_package) |cl_pkg|
                cl_pkg.findAccessibleUpper("*PACKAGE*") orelse try heap.intern("*package*")
            else
                try heap.intern("*package*");
            break :blk try self.compile(pkg_sym, env);
        };

        const node = try self.allocator.create(Ir);
        node.* = .{ .pkg_use_package = .{ .left = packages_ir, .right = pkg_ir } };
        return node;
    }

    fn stripSingleQuote(self: *Compiler, val: Value) Value {
        if (self.builtins) |b| {
            if (val.isCons()) {
                const q0 = val.toPtr(Cons);
                if (q0.car.isSymbol() and q0.car.raw == b.quote.raw and q0.cdr.isCons()) {
                    const q1 = q0.cdr.toPtr(Cons);
                    if (q1.cdr.isNil()) return q1.car;
                }
            }
        }
        return val;
    }

    /// Helper to get string from a string or symbol value
    fn getStringOrSymbolName(self: *Compiler, val: Value) ?[]const u8 {
        _ = self;
        return switch (val.typeKind()) {
            .string => val.toPtr(runtime.String).bytes(),
            .symbol => val.toPtr(Symbol).getName(),
            .keyword => val.toPtr(runtime.Keyword).getName(),
            else => null,
        };
    }

    /// Get qualified name for a symbol (PKG:NAME or just NAME if no package)
    fn getQualifiedName(self: *Compiler, sym: *const Symbol, buf: []u8) !qual_name.QualName {
        return qual_name.qualSym(self.allocator, sym, buf);
    }

    /// Build qualified name from plain name using current package
    fn qualifyName(self: *Compiler, name: []const u8, buf: []u8) !qual_name.QualName {
        const heap = if (self.heap) |val| val else return .{ .name = name, .owned = false };
        const pkg = if (heap.current_package) |val| val else return .{ .name = name, .owned = false };
        return qual_name.qualName(self.allocator, pkg.name, name, buf);
    }

    /// Get qualified name for %next-method% (must match how symbol is interned)
    fn getNextMethodName(self: *Compiler) ![]const u8 {
        const nm_base = "%next-method%";
        var buf: [256]u8 = undefined;
        const qual = try self.qualifyName(nm_base, &buf);
        if (qual.owned) return qual.name;
        // Dupe to allocator since buf is stack-local
        return try self.allocator.dupe(u8, qual.name);
    }

    fn lookupGlobalIdxWithFallback(self: *const Compiler, name: []const u8) ?u16 {
        if (self.globals.lookup(name)) |idx| return idx;

        const prefixes = [_][]const u8{ "COMMON-LISP:", "CL:", "CL-USER:", "COMMON-LISP-USER:" };
        var full_buf: [640]u8 = undefined;
        for (prefixes) |prefix| {
            if (prefix.len + name.len > full_buf.len) continue;
            @memcpy(full_buf[0..prefix.len], prefix);
            @memcpy(full_buf[prefix.len .. prefix.len + name.len], name);
            const candidate = full_buf[0 .. prefix.len + name.len];
            if (self.globals.lookup(candidate)) |idx| return idx;
        }
        return null;
    }

    // ========================================================================
    // Structure Definition (defstruct)
    // ========================================================================

    /// Compile defstruct: (defstruct name slot1 slot2 ...)
    /// Generates: constructor (make-name), accessors (name-slot), predicate (name-p), copier (copy-name)
    /// Runtime representation: #(name slot1-val slot2-val ...)
    /// Registers struct type with type system for occurrence typing and type checking
    fn compileDefstruct(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const trace_defstruct = std.posix.getenv("HABU_TRACE_DEFSTRUCT") != null;
        const heap = if (self.heap) |val| val else {
            if (trace_defstruct) std.debug.print("TRACE defstruct invalid: missing heap\n", .{});
            return error.InvalidSyntax;
        };

        // Parse: (name slot1 slot2 ...) or ((name options...) slot1 slot2 ...)
        if (!args.isCons()) {
            if (trace_defstruct) std.debug.print("TRACE defstruct invalid: args-kind={s}\n", .{@tagName(args.typeKind())});
            return error.InvalidSyntax;
        }
        const cons1 = args.toPtr(Cons);
        const name_spec = cons1.car;

        var name_sym_val: Value = undefined;
        var struct_name_raw: []const u8 = undefined;
        var accessor_prefix: []const u8 = undefined;
        var accessor_prefix_owned = true;
        var emit_copier = true;
        var copier_name_override: ?[]const u8 = null;
        var emit_predicate = true;
        var predicate_name_override: ?[]const u8 = null;

        if (name_spec.isSymbol() or name_spec.isKeyword()) {
            name_sym_val = name_spec;
            struct_name_raw = self.getStringOrSymbolName(name_spec) orelse {
                if (trace_defstruct) {
                    std.debug.print(
                        "TRACE defstruct invalid: name literal kind={s}\n",
                        .{@tagName(name_spec.typeKind())},
                    );
                }
                return error.InvalidSyntax;
            };
            accessor_prefix = try self.concatStrings(struct_name_raw, "-");
        } else if (name_spec.isCons()) {
            const name_cons = name_spec.toPtr(Cons);
            if (!name_cons.car.isSymbol() and !name_cons.car.isKeyword()) {
                if (trace_defstruct) {
                    std.debug.print("TRACE defstruct invalid: name-head-kind={s}\n", .{@tagName(name_cons.car.typeKind())});
                }
                return error.InvalidSyntax;
            }
            name_sym_val = name_cons.car;
            struct_name_raw = self.getStringOrSymbolName(name_sym_val) orelse {
                if (trace_defstruct) {
                    std.debug.print(
                        "TRACE defstruct invalid: name-head no symbol/string kind={s}\n",
                        .{@tagName(name_sym_val.typeKind())},
                    );
                }
                return error.InvalidSyntax;
            };
            accessor_prefix = try self.concatStrings(struct_name_raw, "-");

            var options = name_cons.cdr;
            while (options.isCons()) {
                const opt_cell = options.toPtr(Cons);
                const opt = opt_cell.car;

                // Handle bare keyword options: :conc-name, :copier, :predicate, :constructor
                if (opt.isKeyword()) {
                    const key_name = opt.toPtr(runtime.Keyword).getName();
                    if (std.ascii.eqlIgnoreCase(key_name, "conc-name")) {
                        // Bare :conc-name = nil prefix (use slot name directly)
                        if (accessor_prefix_owned) self.allocator.free(accessor_prefix);
                        accessor_prefix = "";
                        accessor_prefix_owned = false;
                    }
                    // Bare :copier, :predicate, :constructor = use defaults (no-op)
                    options = opt_cell.cdr;
                    continue;
                }

                if (opt.isCons()) {
                    const opt_cons = opt.toPtr(Cons);
                    if (opt_cons.car.isKeyword()) {
                        const key_name = opt_cons.car.toPtr(runtime.Keyword).getName();

                        if (std.ascii.eqlIgnoreCase(key_name, "conc-name")) {
                            if (accessor_prefix_owned) self.allocator.free(accessor_prefix);

                            // (:conc-name) and (:conc-name nil) both mean no prefix.
                            if (opt_cons.cdr.isNil()) {
                                accessor_prefix = "";
                                accessor_prefix_owned = false;
                                options = opt_cell.cdr;
                                continue;
                            }
                            if (!opt_cons.cdr.isCons()) {
                                if (trace_defstruct) {
                                    std.debug.print(
                                        "TRACE defstruct invalid: conc-name tail-kind={s}\n",
                                        .{@tagName(opt_cons.cdr.typeKind())},
                                    );
                                }
                                return error.InvalidSyntax;
                            }
                            // (:conc-name) with no arg = nil prefix
                            if (!opt_cons.cdr.isCons()) {
                                accessor_prefix = "";
                                accessor_prefix_owned = false;
                            } else {
                                const conc_val = opt_cons.cdr.toPtr(Cons).car;
                                if (conc_val.isNil()) {
                                    accessor_prefix = "";
                                    accessor_prefix_owned = false;
                                } else if (conc_val.isCharacter()) {
                                    var utf8_buf: [4]u8 = undefined;
                                    const cp: u21 = conc_val.toCharacter();
                                    const n = try std.unicode.utf8Encode(@intCast(cp), &utf8_buf);
                                    accessor_prefix = try self.allocator.dupe(u8, utf8_buf[0..n]);
                                    accessor_prefix_owned = true;
                                } else if (self.getStringOrSymbolName(conc_val)) |prefix_name| {
                                    accessor_prefix = try self.allocator.dupe(u8, prefix_name);
                                    accessor_prefix_owned = true;
                                } else {
                                    return error.InvalidSyntax;
                                }
                            }
                        } else if (std.ascii.eqlIgnoreCase(key_name, "copier")) {
                            // (:copier) = default, (:copier nil) = suppress, (:copier name) = custom
                            if (!opt_cons.cdr.isCons()) {
                                emit_copier = true; // (:copier) = default
                            } else {
                                const value = opt_cons.cdr.toPtr(Cons).car;
                                if (value.isNil()) {
                                    emit_copier = false;
                                } else if (self.getStringOrSymbolName(value)) |name| {
                                    copier_name_override = try self.allocator.dupe(u8, name);
                                } else {
                                    return error.InvalidSyntax;
                                }
                            }
                        } else if (std.ascii.eqlIgnoreCase(key_name, "predicate")) {
                            // (:predicate) = default, (:predicate nil) = suppress, (:predicate name) = custom
                            if (!opt_cons.cdr.isCons()) {
                                emit_predicate = true; // (:predicate) = default
                            } else {
                                const value = opt_cons.cdr.toPtr(Cons).car;
                                if (value.isNil()) {
                                    emit_predicate = false;
                                } else if (self.getStringOrSymbolName(value)) |name| {
                                    predicate_name_override = try self.allocator.dupe(u8, name);
                                } else {
                                    return error.InvalidSyntax;
                                }
                            }
                        }
                        // Other options (:constructor, :include, :type, etc.) silently skipped for now
                    }
                }
                options = opt_cell.cdr;
            }
        } else {
            if (trace_defstruct) {
                std.debug.print("TRACE defstruct invalid: name-spec-kind={s}\n", .{@tagName(name_spec.typeKind())});
            }
            return error.InvalidSyntax;
        }
        defer if (accessor_prefix_owned) self.allocator.free(accessor_prefix);
        defer if (copier_name_override) |n| self.allocator.free(n);
        defer if (predicate_name_override) |n| self.allocator.free(n);

        // Dupe struct name to avoid dangling pointer if heap moves
        const struct_name = try self.allocator.dupe(u8, struct_name_raw);

        // Collect slot specs: symbol, keyword, or list (slot-name [init-form] [:type T] [:read-only RO])
        var slot_specs = std.ArrayList(SlotSpec){};
        defer slot_specs.deinit(self.allocator);
        var rest = cons1.cdr;
        while (rest.isCons()) {
            const c = rest.toPtr(Cons);
            const slot_spec = c.car;

            switch (slot_spec.typeKind()) {
                .symbol, .keyword => {
                    // Simple slot: `x` or `:x` -> type is any
                    const slot_name_raw = if (slot_spec.isKeyword())
                        slot_spec.toPtr(runtime.Keyword).getName()
                    else
                        slot_spec.toPtr(Symbol).getName();
                    const slot_name = try self.allocator.dupe(u8, slot_name_raw);
                    try slot_specs.append(self.allocator, .{
                        .name = slot_name,
                        .sym = slot_spec,
                        .field_type = &types.t_any,
                        .initargs = std.ArrayList(Value){},
                        .readers = std.ArrayList(Value){},
                        .writers = std.ArrayList(Value){},
                    });
                },
                .string => {
                    // CL allows trailing defstruct docstring; ignore it.
                },
                .cons => {
                    // CL slot spec: (slot-name [init-form] [:type T] [:read-only RO])
                    const spec_cons = slot_spec.toPtr(Cons);
                    const slot_name_val = spec_cons.car;
                    const slot_name_raw = if (slot_name_val.isKeyword())
                        slot_name_val.toPtr(runtime.Keyword).getName()
                    else if (slot_name_val.isSymbol())
                        slot_name_val.toPtr(Symbol).getName()
                    else
                        return error.InvalidSyntax;
                    const slot_name = try self.allocator.dupe(u8, slot_name_raw);

                    // Parse remaining: [init-form] followed by keyword options
                    var field_type: *const types.Type = &types.t_any;
                    var slot_rest = spec_cons.cdr;

                    // Check if second element is a keyword option or init-form
                    if (slot_rest.isCons()) {
                        const next = slot_rest.toPtr(Cons);
                        // If next element is a keyword like :type or :read-only, no init-form
                        if (!next.car.isKeyword()) {
                            // This is the init-form — skip it (init-forms not compiled yet)
                            slot_rest = next.cdr;
                        }
                    }

                    // Parse keyword options: :type T, :read-only RO
                    while (slot_rest.isCons()) {
                        const kw_cell = slot_rest.toPtr(Cons);
                        if (kw_cell.car.isKeyword()) {
                            const kw_name = kw_cell.car.toPtr(runtime.Keyword).getName();
                            if (std.ascii.eqlIgnoreCase(kw_name, "type")) {
                                if (kw_cell.cdr.isCons()) {
                                    const type_val = kw_cell.cdr.toPtr(Cons).car;
                                    if (try self.parseTypeExpr(type_val)) |ty| {
                                        field_type = ty;
                                    }
                                    slot_rest = kw_cell.cdr.toPtr(Cons).cdr;
                                    continue;
                                }
                            }
                            // Skip :read-only and other keyword options
                            if (kw_cell.cdr.isCons()) {
                                slot_rest = kw_cell.cdr.toPtr(Cons).cdr;
                                continue;
                            }
                        }
                        slot_rest = kw_cell.cdr;
                    }

                    try slot_specs.append(self.allocator, .{
                        .name = slot_name,
                        .sym = if (slot_name_val.isSymbol()) slot_name_val else Value.nil,
                        .field_type = field_type,
                        .initargs = std.ArrayList(Value){},
                        .readers = std.ArrayList(Value){},
                        .writers = std.ArrayList(Value){},
                    });
                },
                else => {
                    // Ignore unsupported defstruct metadata entries instead of aborting.
                },
            }
            rest = c.cdr;
        }

        // Create struct fields from specs
        const struct_fields = try self.allocator.alloc(types.StructField, slot_specs.items.len);
        defer {
            for (struct_fields) |field| self.allocator.free(field.name);
            self.allocator.free(struct_fields);
        }
        for (slot_specs.items, 0..) |spec, i| {
            struct_fields[i] = .{
                .name = try self.allocator.dupe(u8, spec.name),
                .type = spec.field_type,
            };
        }
        const struct_type = try self.type_checker.builder.makeStruct(struct_name, struct_fields);
        try self.registerStructType(struct_name, struct_type);

        // Extract slot names for constructor params
        var slot_names = try self.allocator.alloc([]const u8, slot_specs.items.len);
        for (slot_specs.items, 0..) |spec, i| {
            slot_names[i] = spec.name;
        }

        // Compute number of definitions: constructor + accessors + writers + name_lit
        // + optional predicate + optional copier
        const pred_count: usize = if (emit_predicate) 1 else 0;
        const copier_count: usize = if (emit_copier) 1 else 0;
        const num_defs = 2 + pred_count + copier_count + (slot_specs.items.len * 2);
        const defs = try self.allocator.alloc(*Ir, num_defs);
        var def_idx: usize = 0;

        // 1. Constructor: (defun make-name (slot1 slot2 ...) (vector 'name slot1 slot2 ...))
        const make_name = try self.concatStrings("MAKE-", struct_name);
        defs[def_idx] = try self.generateStructConstructor(heap, make_name, struct_name, slot_specs.items, env);
        def_idx += 1;

        // 2. Accessors: (defun name-slotN (obj) (if (name-p obj) (aref obj N+1) (error)))
        for (slot_specs.items, 0..) |spec, i| {
            const accessor_name = if (accessor_prefix.len == 0)
                try self.allocator.dupe(u8, spec.name)
            else
                try self.concatStrings(accessor_prefix, spec.name);
            defs[def_idx] = try self.generateStructAccessor(heap, accessor_name, struct_name, i);
            try self.registerStructAccessor(accessor_name, i);
            def_idx += 1;
        }

        // 3. Writers: (defun (setf name-slotN) (val obj) ...)
        for (slot_specs.items, 0..) |spec, i| {
            const accessor_name = if (accessor_prefix.len == 0)
                try self.allocator.dupe(u8, spec.name)
            else
                try self.concatStrings(accessor_prefix, spec.name);
            const setf_name = try self.concatStrings("(setf ", accessor_name);
            const setf_full = try self.concatStrings(setf_name, ")");
            defs[def_idx] = try self.generateStructWriter(heap, setf_full, struct_name, i);
            def_idx += 1;
        }

        // 4. Predicate (unless suppressed)
        if (emit_predicate) {
            const pred_name = if (predicate_name_override) |n|
                try self.allocator.dupe(u8, n)
            else
                try self.concatStrings(struct_name, "-P");
            defs[def_idx] = try self.generateStructPredicate(heap, pred_name, struct_name);
            def_idx += 1;

            // Register predicate for occurrence typing
            const persistent_pred_name = try self.globals.allocator.dupe(u8, pred_name);
            try self.struct_predicates.put(persistent_pred_name, struct_type);
        }

        // 5. Copier (unless suppressed)
        if (emit_copier) {
            const copy_name = if (copier_name_override) |n|
                try self.allocator.dupe(u8, n)
            else
                try self.concatStrings("COPY-", struct_name);
            defs[def_idx] = try self.generateStructCopier(copy_name);
            def_idx += 1;
        }

        // 6. Return struct name
        defs[def_idx] = try self.builder.lit(name_sym_val);

        return try self.builder.progn(defs);
    }

    /// Slot allocation type
    const Allocation = enum {
        instance, // :allocation :instance (default)
        class, // :allocation :class (shared across all instances)
    };

    /// Slot specification with name, type, and optional init form
    const SlotSpec = struct {
        name: []const u8,
        sym: Value = Value.nil,
        field_type: *const types.Type,
        type_sym: Value = Value.t, // Runtime type symbol (default T = any)
        initform: ?Value = null,
        allocation: Allocation = .instance,
        initargs: std.ArrayList(Value),
        readers: std.ArrayList(Value),
        writers: std.ArrayList(Value),
        is_direct: bool = false,
    };

    /// Allocate Class object and compute CPL
    fn allocateClass(self: *Compiler, heap: *Heap, name: Value, superclasses: Value, slot_specs: []const SlotSpec) !Value {
        const objects = @import("../runtime/objects.zig");

        // Convert superclasses list to array and resolve symbols to Class objects
        var direct_supers_classes = std.ArrayList(Value){};
        defer direct_supers_classes.deinit(self.allocator);

        var super_list = superclasses;
        while (super_list.isCons()) {
            const cons = super_list.toPtr(Cons);
            var super_entry = cons.car;
            // Accept quoted superclass names in addition to bare symbols.
            if (super_entry.isCons()) {
                const maybe_quote = super_entry.toPtr(Cons);
                if (maybe_quote.car.isSymbol() and maybe_quote.cdr.isCons()) {
                    const head_name = maybe_quote.car.toPtr(Symbol).getName();
                    if (std.mem.eql(u8, head_name, "QUOTE")) {
                        super_entry = maybe_quote.cdr.toPtr(Cons).car;
                    }
                }
            }

            if (super_entry.isClass()) {
                try direct_supers_classes.append(self.allocator, super_entry);
                super_list = cons.cdr;
                continue;
            }
            if (!super_entry.isSymbol()) return error.InvalidSyntax;

            // Resolve symbol to Class object for direct_supers
            if (heap.findLispClass(super_entry)) |class_val| {
                try direct_supers_classes.append(self.allocator, class_val);
            } else {
                // Some ANSI forms resolve superclasses via CL package symbols
                // even when the current package symbol is not pointer-identical.
                const super_name = super_entry.toPtr(Symbol).getName();
                if (try heap.internInPackage("CL", super_name)) |cl_super_sym| {
                    if (heap.findLispClass(cl_super_sym)) |class_val| {
                        try direct_supers_classes.append(self.allocator, class_val);
                        super_list = cons.cdr;
                        continue;
                    }
                }
                return error.UndefinedClass;
            }
            super_list = cons.cdr;
        }

        // Build direct_supers list from resolved Class objects
        var direct_supers_list = Value.nil;
        var ds_i = direct_supers_classes.items.len;
        while (ds_i > 0) {
            ds_i -= 1;
            direct_supers_list = try heap.allocCons(direct_supers_classes.items[ds_i], direct_supers_list);
        }

        // Allocate Class object early so CPL can refer to it
        const class_ptr = try heap.alloc(objects.Class);
        const class_val = Value.makeClass(class_ptr);
        class_ptr.* = .{
            .kind = .class,
            .name = name,
            .direct_supers = direct_supers_list,
            .cpl = Value.nil,
            .direct_slots = Value.nil,
            .slots = Value.nil,
            .metaclass = heap.standard_class,
            .num_shared = 0,
            .shared_slots = undefined,
        };

        // Compute CPL (using class objects)
        const cpl = try objects.computeCpl(
            heap.backing_allocator,
            class_val,
            direct_supers_classes.items,
            heap,
            getCpl,
        );
        defer heap.backing_allocator.free(cpl);

        // Convert CPL array to list of class objects
        var cpl_list = Value.nil;
        var i = cpl.len;
        while (i > 0) {
            i -= 1;
            cpl_list = try heap.allocCons(cpl[i], cpl_list);
        }

        // Create SlotDefinition objects
        var direct_slots = std.ArrayList(Value){};
        defer direct_slots.deinit(self.allocator);
        for (slot_specs) |spec| {
            if (!spec.is_direct) continue;
            const slot_def = try heap.alloc(objects.SlotDefinition);

            // Convert initargs ArrayList to list
            var initargs_list = Value.nil;
            var ia_i = spec.initargs.items.len;
            while (ia_i > 0) {
                ia_i -= 1;
                initargs_list = try heap.allocCons(spec.initargs.items[ia_i], initargs_list);
            }

            // Convert readers ArrayList to list
            var readers_list = Value.nil;
            var r_i = spec.readers.items.len;
            while (r_i > 0) {
                r_i -= 1;
                readers_list = try heap.allocCons(spec.readers.items[r_i], readers_list);
            }

            // Convert writers ArrayList to list
            var writers_list = Value.nil;
            var w_i = spec.writers.items.len;
            while (w_i > 0) {
                w_i -= 1;
                writers_list = try heap.allocCons(spec.writers.items[w_i], writers_list);
            }

            // Convert slot name to symbol
            const slot_name_sym = try heap.intern(spec.name);

            // Convert allocation to keyword
            const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
            const allocation_kw = if (spec.allocation == .class) b.kw_class else b.kw_instance;

            slot_def.* = .{
                .kind = .slotdef,
                .name = slot_name_sym,
                .initform = spec.initform orelse Value.nil,
                .initargs = initargs_list,
                .readers = readers_list,
                .writers = writers_list,
                .allocation = allocation_kw,
                .slot_type = spec.type_sym,
            };

            try direct_slots.append(self.allocator, Value.makeSlotDef(slot_def));
        }

        // Preserve slot order as declared
        var direct_slots_list = Value.nil;
        var ds_rev = direct_slots.items.len;
        while (ds_rev > 0) {
            ds_rev -= 1;
            direct_slots_list = try heap.allocCons(direct_slots.items[ds_rev], direct_slots_list);
        }

        // Merge inherited slots in CPL order (skip duplicates by name)
        var seen = std.AutoHashMapUnmanaged(Value, void){};
        defer seen.deinit(self.allocator);
        var all_slots = std.ArrayList(Value){};
        defer all_slots.deinit(self.allocator);

        for (direct_slots.items) |slot_val| {
            if (!slot_val.isSlotDefinition()) return error.InvalidSyntax;
            const name_sym = slot_val.toPtr(objects.SlotDefinition).name;
            try seen.put(self.allocator, name_sym, {});
            try all_slots.append(self.allocator, slot_val);
        }

        var cpl_tail = cpl_list;
        if (cpl_tail.isCons()) {
            cpl_tail = cpl_tail.toPtr(Cons).cdr;
        }
        while (cpl_tail.isCons()) {
            const cpl_cons = cpl_tail.toPtr(Cons);
            var super_val = cpl_cons.car;
            if (!super_val.isClass()) {
                if (super_val.isSymbol()) {
                    if (heap.findLispClass(super_val)) |found| {
                        super_val = found;
                    } else {
                        cpl_tail = cpl_cons.cdr;
                        continue;
                    }
                } else {
                    cpl_tail = cpl_cons.cdr;
                    continue;
                }
            }
            var super_slots = super_val.toPtr(runtime.Class).direct_slots;
            while (super_slots.isCons()) {
                const slot_cons = super_slots.toPtr(Cons);
                const slot_val = slot_cons.car;
                if (!slot_val.isSlotDefinition()) return error.InvalidSyntax;
                const name_sym = slot_val.toPtr(objects.SlotDefinition).name;
                if (!seen.contains(name_sym)) {
                    try seen.put(self.allocator, name_sym, {});
                    try all_slots.append(self.allocator, slot_val);
                }
                super_slots = slot_cons.cdr;
            }
            cpl_tail = cpl_cons.cdr;
        }

        var slots_list = Value.nil;
        var slots_rev = all_slots.items.len;
        while (slots_rev > 0) {
            slots_rev -= 1;
            slots_list = try heap.allocCons(all_slots.items[slots_rev], slots_list);
        }

        class_ptr.direct_supers = direct_supers_list;
        class_ptr.cpl = cpl_list;
        class_ptr.direct_slots = direct_slots_list;
        class_ptr.slots = slots_list;

        return class_val;
    }

    /// Get CPL for a class (used by computeCpl)
    fn getCpl(ctx: *anyopaque, class_val: Value) Value {
        const heap: *Heap = @ptrCast(@alignCast(ctx));
        var cls = class_val;
        if (!cls.isClass()) {
            if (cls.isSymbol()) {
                if (heap.findLispClass(cls)) |found| {
                    cls = found;
                } else {
                    return Value.nil;
                }
            } else {
                return Value.nil;
            }
        }
        return cls.toPtr(runtime.Class).cpl;
    }

    /// Generate constructor: creates a closure that takes args, checks types, returns vector
    fn generateStructConstructor(self: *Compiler, heap: *Heap, make_name: []const u8, struct_name: []const u8, slots: []const SlotSpec, env: *const Env) anyerror!*Ir {
        _ = env;
        // Qualify the constructor name with current package
        var qual_buf: [512]u8 = undefined;
        const q = try self.qualifyName(make_name, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const qualified_name = q.name;
        const global_idx = try self.globals.define(qualified_name);

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

        const lambda_ir = try self.builder.lambda(
            slot_names,
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            false,
            null,
            &[_]Ir.Capture{},
            body_ir,
        );

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
        const b = if (self.builtins) |val| val else return null;

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
    pub fn parseTypeExpr(self: *Compiler, type_expr: Value) anyerror!?*const types.Type {
        // Simple symbol case
        if (type_expr.isSymbol()) {
            return self.parseTypeSym(type_expr);
        }

        // Compound type form
        if (!type_expr.isCons()) return null;

        const cons = type_expr.toPtr(Cons);
        const head = cons.car;

        if (!head.isSymbol()) return null;

        const b = if (self.builtins) |val| val else return null;

        // (union T1 T2 ...) or (or T1 T2 ...) - union type
        if (head.raw == b.ty_union.raw or head.raw == b.ty_or.raw) {
            return try self.parseOrType(cons.cdr);
        }

        // (and T1 T2 ...) - intersection type
        if (head.raw == b.ty_and.raw) {
            return try self.parseAndType(cons.cdr);
        }

        // (not T) - negation type
        if (head.raw == b.ty_not.raw) {
            return try self.parseNotType(cons.cdr);
        }

        // (-> (A B) C) or (-> A B ... C) - function type
        if (head.raw == b.@"->".raw) {
            return try self.parseArrowType(cons.cdr);
        }

        // (list T) - list type
        if (head.raw == b.ty_list.raw) {
            return try self.parseListType(cons.cdr);
        }

        // (vec T) or (vec T N) - vector type
        if (head.raw == b.ty_vec.raw) {
            return try self.parseVecType(cons.cdr);
        }

        // (non-nil T) - non-nil type
        if (head.raw == b.@"ty_non-nil".raw) {
            return try self.parseNonNilType(cons.cdr);
        }

        // (pi (x : A) B) - dependent function type
        if (head.raw == b.ty_pi.raw) {
            return try self.parsePiType(cons.cdr);
        }

        // (sigma (x : A) B) - dependent pair type
        if (head.raw == b.ty_sigma.raw) {
            return try self.parseSigmaType(cons.cdr);
        }

        // (refine T x P) - refinement type
        if (head.raw == b.ty_refine.raw) {
            return try self.parseRefineType(cons.cdr);
        }

        // (member obj1 obj2 ...) - member type
        if (head.raw == b.ty_member.raw) {
            return try self.parseMemberType(cons.cdr);
        }

        // (eql obj) - eql type
        if (head.raw == b.ty_eql.raw) {
            return try self.parseEqlType(cons.cdr);
        }

        // (function (arg-types...) return-type) - CL-style function type
        if (head.raw == b.ty_function.raw) {
            return try self.parseFunctionType(cons.cdr);
        }

        return null;
    }

    /// Parse (or T1 T2 ...)
    fn parseOrType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        var type_list = std.ArrayList(*const types.Type){};
        defer type_list.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const c = current.toPtr(Cons);
            const t = (try self.parseTypeExpr(c.car)) orelse return null;
            try type_list.append(self.allocator, t);
            current = c.cdr;
        }

        if (type_list.items.len == 0) return null;
        if (type_list.items.len == 1) return type_list.items[0];

        return try self.type_checker.builder.makeOr(type_list.items);
    }

    /// Parse (and T1 T2 ...)
    fn parseAndType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        var type_list = std.ArrayList(*const types.Type){};
        defer type_list.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const c = current.toPtr(Cons);
            const t = (try self.parseTypeExpr(c.car)) orelse return null;
            try type_list.append(self.allocator, t);
            current = c.cdr;
        }

        if (type_list.items.len == 0) return null;
        if (type_list.items.len == 1) return type_list.items[0];

        return try self.type_checker.builder.makeAnd(type_list.items);
    }

    /// Parse (not T)
    fn parseNotType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        if (!args.isCons()) return null;
        const c = args.toPtr(Cons);
        const inner = (try self.parseTypeExpr(c.car)) orelse return null;
        return try self.type_checker.builder.makeNot(inner);
    }

    /// Parse (-> (A B) C) or (-> A B ... C) function type
    fn parseArrowType(self: *Compiler, args: Value) anyerror!?*const types.Type {
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
                    const t = (try self.parseTypeExpr(dc.car)) orelse return null;
                    try all_types.append(self.allocator, t);
                    domain = dc.cdr;
                }
            } else {
                // Single type
                const t = (try self.parseTypeExpr(c.car)) orelse return null;
                try all_types.append(self.allocator, t);
            }
            current = c.cdr;
        }

        if (all_types.items.len < 1) return null;

        // Last type is return type, rest are domain
        const return_type = all_types.items[all_types.items.len - 1];
        const domain = all_types.items[0 .. all_types.items.len - 1];

        return try self.type_checker.builder.makeArrow(domain, return_type);
    }

    /// Parse (list T)
    fn parseListType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        if (!args.isCons()) return &types.t_list_any;
        const c = args.toPtr(Cons);
        const elem = (try self.parseTypeExpr(c.car)) orelse return null;
        return try self.type_checker.builder.makeList(elem);
    }

    /// Parse (vec T) or (vec T N) - sized vectors use type_app
    fn parseVecType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        if (!args.isCons()) return &types.t_vector;
        const c = args.toPtr(Cons);
        const elem = (try self.parseTypeExpr(c.car)) orelse return null;

        // Check for (vec T N) - sized vector
        if (c.cdr.isCons()) {
            const rest = c.cdr.toPtr(Cons);
            const size_term: *const anyopaque = @ptrCast(&rest.car);
            const vec_t = try self.type_checker.builder.makeVec(elem, null);
            return try self.type_checker.builder.makeTypeApp(vec_t, size_term);
        }

        return try self.type_checker.builder.makeVec(elem, null);
    }

    /// Parse (non-nil T)
    fn parseNonNilType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        if (!args.isCons()) return null;
        const c = args.toPtr(Cons);
        const inner = (try self.parseTypeExpr(c.car)) orelse return null;
        return try self.type_checker.builder.makeNonNil(inner);
    }

    /// Parse (pi (x : A) B) dependent function type
    fn parsePiType(self: *Compiler, args: Value) anyerror!?*const types.Type {
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
            const b = if (self.builtins) |val| val else return null;
            if (rest1.car.eq(b.kw_colon)) {
                // Next is the actual type
                if (!rest1.cdr.isCons()) return null;
                type_expr = rest1.cdr.toPtr(Cons).car;
            }
        }

        const param_type = (try self.parseTypeExpr(type_expr)) orelse return null;

        // Second element is the return type B
        if (!c1.cdr.isCons()) return null;
        const c2 = c1.cdr.toPtr(Cons);
        const return_type = (try self.parseTypeExpr(c2.car)) orelse return null;

        return try self.type_checker.builder.makePi(param_name, param_type, return_type, .many);
    }

    /// Parse (sigma (x : A) B) dependent pair type
    fn parseSigmaType(self: *Compiler, args: Value) anyerror!?*const types.Type {
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
            const b = if (self.builtins) |val| val else return null;
            if (rest1.car.eq(b.kw_colon)) {
                if (!rest1.cdr.isCons()) return null;
                type_expr = rest1.cdr.toPtr(Cons).car;
            }
        }

        const first_type = (try self.parseTypeExpr(type_expr)) orelse return null;

        if (!c1.cdr.isCons()) return null;
        const c2 = c1.cdr.toPtr(Cons);
        const second_type = (try self.parseTypeExpr(c2.car)) orelse return null;

        return try self.type_checker.builder.makeSigma(first_name, first_type, second_type);
    }

    /// Parse (refine T x P) refinement type
    fn parseRefineType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        // (refine T x P) -> args = (T x P)
        if (!args.isCons()) return null;
        const c1 = args.toPtr(Cons);

        // Base type T
        const base_type = (try self.parseTypeExpr(c1.car)) orelse return null;

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
        return try self.type_checker.builder.makeRefinement(base_type, var_name, null);
    }

    fn parseMemberType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        var obj_list = std.ArrayList(*const anyopaque){};
        defer obj_list.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const c = current.toPtr(Cons);
            const val = @as(*const Value, @ptrCast(&c.car));
            try obj_list.append(self.allocator, @as(*const anyopaque, @ptrCast(val)));
            current = c.cdr;
        }

        if (obj_list.items.len == 0) return null;
        return try self.type_checker.builder.makeMember(obj_list.items);
    }

    fn parseEqlType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        if (!args.isCons()) return null;
        const c = args.toPtr(Cons);
        const val = @as(*const Value, @ptrCast(&c.car));
        return try self.type_checker.builder.makeEql(@as(*const anyopaque, @ptrCast(val)));
    }

    /// Parse (function (arg-types...) return-type) - CL-style function type
    /// Syntax: (function (arg1-type arg2-type...) return-type)
    /// Supports &optional, &rest, &key in arg list (simplified: types only)
    fn parseFunctionType(self: *Compiler, args: Value) anyerror!?*const types.Type {
        if (args.isNil()) return &types.t_closure;
        if (!args.isCons()) return null;

        const c = args.toPtr(Cons);
        var domain_types = std.ArrayList(*const types.Type){};
        defer domain_types.deinit(self.allocator);

        // First arg is the arg type list (possibly empty)
        switch (c.car.typeKind()) {
            .nil => {
                // No args: (function () return-type)
            },
            .cons => {
                // Parse arg types: (type1 type2 ...)
                var arg_list = c.car;
                const b = if (self.builtins) |val| val else return null;
                while (arg_list.isCons()) {
                    const ac = arg_list.toPtr(Cons);
                    // Skip lambda list markers
                    if (ac.car.isSymbol()) {
                        if (ac.car.raw == b.@"&optional".raw or
                            ac.car.raw == b.@"&rest".raw or
                            ac.car.raw == b.@"&key".raw)
                        {
                            arg_list = ac.cdr;
                            continue;
                        }
                    }
                    const arg_type = (try self.parseTypeExpr(ac.car)) orelse return null;
                    try domain_types.append(self.allocator, arg_type);
                    arg_list = ac.cdr;
                }
            },
            else => {
                // First arg is a single type (not a list)
                const arg_type = (try self.parseTypeExpr(c.car)) orelse return null;
                try domain_types.append(self.allocator, arg_type);
            },
        }

        // Second arg is return type (defaults to any if not specified)
        const return_type = if (c.cdr.isCons()) blk: {
            const rc = c.cdr.toPtr(Cons);
            break :blk (try self.parseTypeExpr(rc.car)) orelse &types.t_any;
        } else &types.t_any;

        return try self.type_checker.builder.makeArrow(domain_types.items, return_type);
    }

    /// Generate accessor with runtime type check:
    /// (lambda (obj)
    ///   (if (and (vectorp obj) (eq (vec-ref obj 0) 'struct-name))
    ///       (vec-ref obj slot_idx+1)
    ///       (error "type error")))
    fn generateStructAccessor(self: *Compiler, heap: *Heap, accessor_name: []const u8, struct_name: []const u8, slot_idx: usize) anyerror!*Ir {
        // Qualify the accessor name with current package
        var qual_buf: [512]u8 = undefined;
        const q = try self.qualifyName(accessor_name, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const qualified_name = q.name;
        const global_idx = try self.globals.define(qualified_name);

        // Accessor functions are plain closures; clear any stale generic metadata.
        self.removeGenericFunctionMeta(qualified_name);
        if (!std.mem.eql(u8, accessor_name, qualified_name)) {
            self.removeGenericFunctionMeta(accessor_name);
        }

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
        const error_str = try heap.allocBaseString(error_msg);
        const error_lit = try self.builder.lit(error_str);
        const error_call = try self.builder.errorUser(error_lit);

        // Full body: (if type-check (vec-ref obj idx) (error ...))
        const body_ir = try self.builder.ifExpr(type_check, vecref_ir, error_call);

        // Lambda with 1 param named "obj"
        const lambda_ir = try self.builder.lambda(
            &[_][]const u8{"obj"},
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            false,
            null,
            &[_]Ir.Capture{},
            body_ir,
        );

        return try self.builder.define(qualified_name, global_idx, lambda_ir);
    }

    /// Generate writer: (lambda (val obj) (if type-check (setf (vec-ref obj idx) val) (error)))
    fn generateStructWriter(self: *Compiler, heap: *Heap, writer_name: []const u8, struct_name: []const u8, slot_idx: usize) anyerror!*Ir {
        var qual_buf: [512]u8 = undefined;
        const q = try self.qualifyName(writer_name, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const qualified_name = q.name;
        const global_idx = try self.globals.define(qualified_name);

        // Writer functions are plain closures; clear any stale generic metadata.
        self.removeGenericFunctionMeta(qualified_name);
        if (!std.mem.eql(u8, writer_name, qualified_name)) {
            self.removeGenericFunctionMeta(writer_name);
        }

        const obj_ref = try self.builder.variable("obj", 0, 1);
        const vectorp_ir = try self.builder.vectorp(obj_ref);
        const idx0 = try self.builder.lit(Value.makeFixnum(0));
        const obj_ref2 = try self.builder.variable("obj", 0, 1);
        const vecref0 = try self.builder.vecRef(obj_ref2, idx0);
        const name_sym = try heap.intern(struct_name);
        const name_lit = try self.builder.lit(name_sym);
        const eq_ir = try self.builder.eq(vecref0, name_lit);
        const nil_ir = try self.builder.lit(Value.nil);
        const type_check = try self.builder.ifExpr(vectorp_ir, eq_ir, nil_ir);

        const obj_ref3 = try self.builder.variable("obj", 0, 1);
        const idx_lit = try self.builder.lit(Value.makeFixnum(@intCast(slot_idx + 1)));
        const val_ref = try self.builder.variable("val", 0, 0);
        const setf_ir = try self.builder.vecSet(obj_ref3, idx_lit, val_ref);

        const error_msg = try self.concatStrings3("type error: expected ", struct_name, "");
        const error_str = try heap.allocBaseString(error_msg);
        const error_lit = try self.builder.lit(error_str);
        const error_call = try self.builder.errorUser(error_lit);

        const body_ir = try self.builder.ifExpr(type_check, setf_ir, error_call);

        const lambda_ir = try self.builder.lambda(
            &[_][]const u8{ "val", "obj" },
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            false,
            null,
            &[_]Ir.Capture{},
            body_ir,
        );

        return try self.builder.define(qualified_name, global_idx, lambda_ir);
    }

    /// Generate predicate: checks if obj is a vector with correct type tag
    fn generateStructPredicate(self: *Compiler, heap: *Heap, pred_name: []const u8, struct_name: []const u8) anyerror!*Ir {
        // Qualify the predicate name with current package
        var qual_buf: [512]u8 = undefined;
        const q = try self.qualifyName(pred_name, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const qualified_name = q.name;
        const global_idx = try self.globals.define(qualified_name);

        // Body: (if (vectorp obj) (eq (vec-ref obj 0) 'name) nil)
        const obj_ref = try self.builder.variable("obj", 0, 0);
        const vectorp_ir = try self.builder.vectorp(obj_ref);
        const idx0 = try self.builder.lit(Value.makeFixnum(0));
        const obj_ref2 = try self.builder.variable("obj", 0, 0);
        const vecref0 = try self.builder.vecRef(obj_ref2, idx0);
        const name_sym = try heap.intern(struct_name);
        const name_lit = try self.builder.lit(name_sym);
        const eq_ir = try self.builder.eq(vecref0, name_lit);
        const nil_ir = try self.builder.lit(Value.nil);
        const body_ir = try self.builder.ifExpr(vectorp_ir, eq_ir, nil_ir);

        const lambda_ir = try self.builder.lambda(
            &[_][]const u8{"obj"},
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            false,
            null,
            &[_]Ir.Capture{},
            body_ir,
        );

        return try self.builder.define(qualified_name, global_idx, lambda_ir);
    }

    /// Generate copier: (lambda (obj) (copy-structure obj))
    fn generateStructCopier(self: *Compiler, copy_name: []const u8) anyerror!*Ir {
        // Qualify the copier name with current package
        var qual_buf: [512]u8 = undefined;
        const q = try self.qualifyName(copy_name, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const qualified_name = q.name;
        const global_idx = try self.globals.define(qualified_name);

        const obj_ref = try self.builder.variable("obj", 0, 0);
        const copy_ir = try self.builder.copyStructure(obj_ref);

        const lambda_ir = try self.builder.lambda(
            &[_][]const u8{"obj"},
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            false,
            null,
            &[_]Ir.Capture{},
            copy_ir,
        );

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

    const QualName = struct {
        slice: []const u8,
        owned: bool,
    };

    fn makeQualifiedName(self: *Compiler, buf: *[256]u8, prefix: []const u8, name: []const u8) error{ OutOfMemory, Overflow }!QualName {
        const total_len = try std.math.add(usize, prefix.len, name.len);
        if (total_len <= buf.len) {
            @memcpy(buf[0..prefix.len], prefix);
            @memcpy(buf[prefix.len..][0..name.len], name);
            return .{ .slice = buf[0..total_len], .owned = false };
        }
        const slice = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ prefix, name });
        return .{ .slice = slice, .owned = true };
    }

    fn makePkgQualifiedName(self: *Compiler, buf: *[256]u8, pkg: []const u8, name: []const u8) error{ OutOfMemory, Overflow }!QualName {
        const pkg_len = try std.math.add(usize, pkg.len, 1);
        const total_len = try std.math.add(usize, pkg_len, name.len);
        if (total_len <= buf.len) {
            @memcpy(buf[0..pkg.len], pkg);
            buf[pkg.len] = ':';
            @memcpy(buf[pkg_len..][0..name.len], name);
            return .{ .slice = buf[0..total_len], .owned = false };
        }
        const slice = try std.fmt.allocPrint(self.allocator, "{s}:{s}", .{ pkg, name });
        return .{ .slice = slice, .owned = true };
    }

    fn lookupClassMetadataByName(self: *Compiler, class_name: []const u8) error{ OutOfMemory, Overflow }!?[]const SlotSpec {
        if (self.class_metadata.get(class_name)) |specs| return specs;

        var qual_buf: [256]u8 = undefined;
        const prefixes = [_][]const u8{ "HABU:", "CL-USER:", "CL:", "" };
        for (prefixes) |prefix| {
            const qualified = try self.makeQualifiedName(&qual_buf, prefix, class_name);
            defer if (qualified.owned) self.allocator.free(qualified.slice);
            if (self.class_metadata.get(qualified.slice)) |specs| return specs;
        }

        return null;
    }

    fn lookupClassMetadataBySymbol(self: *Compiler, class_sym: *const Symbol) error{ OutOfMemory, Overflow }!?[]const SlotSpec {
        const class_name = class_sym.getName();
        const pkg_bits: u64 = class_sym.reserved;
        if (pkg_bits != 0 and (pkg_bits & 1) == 0) {
            const pkg: *const runtime.heap.Package = @ptrFromInt(pkg_bits);
            var qual_buf: [256]u8 = undefined;
            const qualified = try self.makePkgQualifiedName(&qual_buf, pkg.name, class_name);
            defer if (qualified.owned) self.allocator.free(qualified.slice);
            if (self.class_metadata.get(qualified.slice)) |specs| return specs;
        }

        return try self.lookupClassMetadataByName(class_name);
    }

    // ========================================================================
    // CLOS Support: defclass, make-instance, slot-value
    // ========================================================================

    /// Compile defclass: (defclass name (superclasses) (slot1 slot2 ...) ...)
    /// Simplified CLOS - for now, ignores superclasses and slot options
    /// Generates: class predicate, constructor (via make-instance), slot accessors
    /// Runtime representation: #('class-name slot1-val slot2-val ...)
    fn compileDefclass(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        const heap = if (self.heap) |val| val else return error.InvalidSyntax;
        const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
        const kw_default_initargs = try heap.internKeyword("default-initargs");
        const kw_metaclass = try heap.internKeyword("metaclass");
        const kw_documentation = try heap.internKeyword("documentation");

        // Parse: (name (superclasses...) (slot1 slot2 ...) ...)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const name_val = cons1.car;
        if (!name_val.isSymbol()) return error.InvalidSyntax;

        const class_name_raw = name_val.toPtr(Symbol).getName();
        const class_name = try self.allocator.dupe(u8, class_name_raw);

        // Parse superclasses (second arg) and inherit their slots
        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const superclasses = cons2.car;

        // Collect inherited slots from superclasses
        var slot_specs = std.ArrayList(SlotSpec){};
        defer {
            for (slot_specs.items) |*spec| {
                spec.initargs.deinit(self.allocator);
                spec.readers.deinit(self.allocator);
                spec.writers.deinit(self.allocator);
            }
            slot_specs.deinit(self.allocator);
        }
        const InitargDefault = struct {
            key: Value,
            value: Value,
        };
        var class_initarg_defaults = std.ArrayList(InitargDefault){};
        defer class_initarg_defaults.deinit(self.allocator);

        // Process superclass list
        if (superclasses.isCons()) {
            var super_list = superclasses;
            while (super_list.isCons()) {
                const super_cons = super_list.toPtr(Cons);
                const super_name_val = super_cons.car;

                if (super_name_val.isSymbol()) {
                    const super_name = super_name_val.toPtr(Symbol).getName();

                    const parent_specs = try self.lookupClassMetadataByName(super_name);

                    if (parent_specs) |specs| {
                        // Inherit slots from parent
                        for (specs) |parent_spec| {
                            const inherited_name = try self.allocator.dupe(u8, parent_spec.name);
                            var inherited_initargs = std.ArrayList(Value){};
                            for (parent_spec.initargs.items) |ia| try inherited_initargs.append(self.allocator, ia);
                            var inherited_readers = std.ArrayList(Value){};
                            for (parent_spec.readers.items) |r| try inherited_readers.append(self.allocator, r);
                            var inherited_writers = std.ArrayList(Value){};
                            for (parent_spec.writers.items) |w| try inherited_writers.append(self.allocator, w);
                            try slot_specs.append(self.allocator, .{
                                .name = inherited_name,
                                .sym = parent_spec.sym,
                                .field_type = parent_spec.field_type,
                                .initform = parent_spec.initform,
                                .initargs = inherited_initargs,
                                .readers = inherited_readers,
                                .writers = inherited_writers,
                                .is_direct = false,
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
        var class_options_rest = Value.nil;

        // Normalize defclass forms:
        // - CL standard: (defclass C () ((slot ...)) (:default-initargs ...))
        // - Habu style:  (defclass C () (slot ...) (slot ...))
        if (rest.isCons()) {
            const first = rest.toPtr(Cons);
            if (first.car.isNil()) {
                // Empty slot list in CL syntax
                rest = Value.nil;
                class_options_rest = first.cdr;
            } else if (first.car.isCons()) {
                const maybe_slot_list = first.car.toPtr(Cons);
                var looks_like_slot_list = false;
                if (maybe_slot_list.cdr.isNil()) {
                    // Single-element list: treat as slot list for CL compatibility.
                    looks_like_slot_list = true;
                } else if (maybe_slot_list.cdr.isCons()) {
                    const second = maybe_slot_list.cdr.toPtr(Cons).car;
                    // Habu style slot spec is typically (slot :keyword ...).
                    // CL style slot list is ((slot ...) (slot ...) ...), or (slot1 slot2 ...).
                    looks_like_slot_list = !second.isKeyword();
                }
                if (looks_like_slot_list) {
                    rest = first.car;
                    class_options_rest = first.cdr;
                }
            }
        }

        while (rest.isCons()) {
            const c = rest.toPtr(Cons);
            const slot_spec = c.car;

            if (slot_spec.isCons()) {
                const class_opt_cons = slot_spec.toPtr(Cons);
                const class_opt_key = class_opt_cons.car;
                if (class_opt_key.isKeyword()) {
                    // Recognize class options after the slot list.
                    if (class_opt_key.eq(kw_default_initargs)) {
                        var defaults = class_opt_cons.cdr;
                        while (defaults.isCons()) {
                            const default_cons = defaults.toPtr(Cons);
                            const initarg = default_cons.car;
                            switch (initarg.typeKind()) {
                                .keyword, .symbol, .nil, .t => {},
                                else => return error.InvalidSyntax,
                            }
                            if (!default_cons.cdr.isCons()) return error.InvalidSyntax;
                            const value_cons = default_cons.cdr.toPtr(Cons);
                            try class_initarg_defaults.append(self.allocator, .{
                                .key = initarg,
                                .value = value_cons.car,
                            });
                            defaults = value_cons.cdr;
                        }
                        if (!defaults.isNil()) return error.InvalidSyntax;
                        rest = c.cdr;
                        continue;
                    }
                    if (class_opt_key.eq(kw_metaclass) or class_opt_key.eq(kw_documentation)) {
                        // Parsed for syntax compatibility; currently ignored.
                        rest = c.cdr;
                        continue;
                    }
                }
            }

            switch (slot_spec.typeKind()) {
                .symbol => {
                    // Simple slot: `x`
                    const slot_name_raw = slot_spec.toPtr(Symbol).getName();
                    const slot_name = try self.allocator.dupe(u8, slot_name_raw);
                    var initargs = std.ArrayList(Value){};
                    const default_initarg = try heap.internKeyword(slot_name_raw);
                    try initargs.append(self.allocator, default_initarg);
                    try slot_specs.append(self.allocator, .{
                        .name = slot_name,
                        .sym = slot_spec,
                        .field_type = &types.t_any,
                        .initargs = initargs,
                        .readers = std.ArrayList(Value){},
                        .writers = std.ArrayList(Value){},
                        .is_direct = true,
                    });
                },
                .cons => {
                    // Slot with options: (name :initform expr :type type ...)
                    const spec_cons = slot_spec.toPtr(Cons);
                    if (!spec_cons.car.isSymbol()) return error.InvalidSyntax;
                    const slot_name_raw = spec_cons.car.toPtr(Symbol).getName();
                    const slot_name = try self.allocator.dupe(u8, slot_name_raw);

                    // Extract slot options
                    var field_type: *const types.Type = &types.t_any;
                    var type_sym: Value = Value.t;
                    var initform: ?Value = null;
                    var allocation: Allocation = .instance;
                    var initargs = std.ArrayList(Value){};
                    var readers = std.ArrayList(Value){};
                    var writers = std.ArrayList(Value){};
                    var opts = spec_cons.cdr;
                    while (opts.isCons()) {
                        const opt_cons = opts.toPtr(Cons);
                        const opt_key = opt_cons.car;

                        if (opt_key.isKeyword()) {
                            if (opt_key.eq(b.kw_type)) {
                                // type keyword - next element is the type
                                if (opt_cons.cdr.isCons()) {
                                    const type_cons = opt_cons.cdr.toPtr(Cons);
                                    type_sym = type_cons.car; // Store the type expression as runtime value
                                    if (try self.parseTypeExpr(type_cons.car)) |ty| {
                                        field_type = ty;
                                    }
                                    opts = type_cons.cdr;
                                    continue;
                                }
                            } else if (opt_key.eq(b.kw_initform)) {
                                // initform keyword - next element is the init expression
                                if (opt_cons.cdr.isCons()) {
                                    const init_cons = opt_cons.cdr.toPtr(Cons);
                                    initform = init_cons.car;
                                    opts = init_cons.cdr;
                                    continue;
                                }
                            } else if (opt_key.eq(b.kw_allocation)) {
                                // allocation keyword - next element is :instance or :class
                                if (opt_cons.cdr.isCons()) {
                                    const alloc_cons = opt_cons.cdr.toPtr(Cons);
                                    const alloc_val = alloc_cons.car;
                                    if (alloc_val.isKeyword()) {
                                        if (alloc_val.eq(b.kw_class)) {
                                            allocation = .class;
                                        } else if (alloc_val.eq(b.kw_instance)) {
                                            allocation = .instance;
                                        }
                                    }
                                    opts = alloc_cons.cdr;
                                    continue;
                                }
                            } else if (opt_key.eq(b.kw_initarg)) {
                                if (opt_cons.cdr.isCons()) {
                                    const initarg_cons = opt_cons.cdr.toPtr(Cons);
                                    switch (initarg_cons.car.typeKind()) {
                                        .keyword, .symbol, .nil, .t => {},
                                        else => return error.InvalidSyntax,
                                    }
                                    try initargs.append(self.allocator, initarg_cons.car);
                                    opts = initarg_cons.cdr;
                                    continue;
                                }
                            } else if (opt_key.eq(b.kw_reader)) {
                                if (opt_cons.cdr.isCons()) {
                                    const reader_cons = opt_cons.cdr.toPtr(Cons);
                                    try readers.append(self.allocator, reader_cons.car);
                                    opts = reader_cons.cdr;
                                    continue;
                                }
                            } else if (opt_key.eq(b.kw_writer)) {
                                if (opt_cons.cdr.isCons()) {
                                    const writer_cons = opt_cons.cdr.toPtr(Cons);
                                    try writers.append(self.allocator, writer_cons.car);
                                    opts = writer_cons.cdr;
                                    continue;
                                }
                            } else if (opt_key.eq(b.kw_accessor)) {
                                if (opt_cons.cdr.isCons()) {
                                    const accessor_cons = opt_cons.cdr.toPtr(Cons);
                                    const accessor_name = accessor_cons.car;
                                    try readers.append(self.allocator, accessor_name);
                                    try writers.append(self.allocator, accessor_name);
                                    opts = accessor_cons.cdr;
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

                    if (initargs.items.len == 0) {
                        const default_initarg = try heap.internKeyword(slot_name_raw);
                        try initargs.append(self.allocator, default_initarg);
                    }

                    try slot_specs.append(self.allocator, .{
                        .name = slot_name,
                        .sym = spec_cons.car,
                        .field_type = field_type,
                        .type_sym = type_sym,
                        .initform = initform,
                        .allocation = allocation,
                        .initargs = initargs,
                        .readers = readers,
                        .writers = writers,
                        .is_direct = true,
                    });
                },
                else => return error.InvalidSyntax,
            }
            rest = c.cdr;
        }

        while (class_options_rest.isCons()) {
            const opt_form_cons = class_options_rest.toPtr(Cons);
            const class_opt_form = opt_form_cons.car;
            if (!class_opt_form.isCons()) return error.InvalidSyntax;
            const class_opt = class_opt_form.toPtr(Cons);
            const class_opt_key = class_opt.car;
            if (!class_opt_key.isKeyword()) return error.InvalidSyntax;
            if (class_opt_key.eq(kw_default_initargs)) {
                var defaults = class_opt.cdr;
                while (defaults.isCons()) {
                    const default_cons = defaults.toPtr(Cons);
                    const initarg = default_cons.car;
                    switch (initarg.typeKind()) {
                        .keyword, .symbol, .nil, .t => {},
                        else => return error.InvalidSyntax,
                    }
                    if (!default_cons.cdr.isCons()) return error.InvalidSyntax;
                    const value_cons = default_cons.cdr.toPtr(Cons);
                    try class_initarg_defaults.append(self.allocator, .{
                        .key = initarg,
                        .value = value_cons.car,
                    });
                    defaults = value_cons.cdr;
                }
                if (!defaults.isNil()) return error.InvalidSyntax;
            } else if (!(class_opt_key.eq(kw_metaclass) or class_opt_key.eq(kw_documentation))) {
                // Unknown class option for now; parse and ignore.
            }
            class_options_rest = opt_form_cons.cdr;
        }
        if (!class_options_rest.isNil()) return error.InvalidSyntax;

        if (class_initarg_defaults.items.len > 0 and slot_specs.items.len > 0) {
            const slot_default_set = try self.allocator.alloc(bool, slot_specs.items.len);
            defer self.allocator.free(slot_default_set);
            @memset(slot_default_set, false);

            // Apply :default-initargs in declaration order; first matching initarg wins per slot.
            for (class_initarg_defaults.items) |default_initarg| {
                for (slot_specs.items, 0..) |*spec, idx| {
                    if (slot_default_set[idx]) continue;
                    for (spec.initargs.items) |slot_initarg| {
                        if (default_initarg.key.eq(slot_initarg)) {
                            spec.initform = default_initarg.value;
                            slot_default_set[idx] = true;
                            break;
                        }
                    }
                }
            }
        }

        // Create class type - similar to struct
        const class_fields = try self.allocator.alloc(types.StructField, slot_specs.items.len);
        defer {
            for (class_fields) |field| self.allocator.free(field.name);
            self.allocator.free(class_fields);
        }
        for (slot_specs.items, 0..) |spec, i| {
            class_fields[i] = .{
                .name = try self.allocator.dupe(u8, spec.name),
                .type = spec.field_type,
            };
        }
        const class_type = try self.type_checker.builder.makeStruct(class_name, class_fields);
        try self.registerStructType(class_name, class_type);

        // Store class metadata for compilation (compiler-side with initforms)
        const persistent_specs = try self.globals.allocator.alloc(SlotSpec, slot_specs.items.len);
        for (slot_specs.items, 0..) |spec, i| {
            const initargs_slice = try self.globals.allocator.dupe(Value, spec.initargs.items);
            const readers_slice = try self.globals.allocator.dupe(Value, spec.readers.items);
            const writers_slice = try self.globals.allocator.dupe(Value, spec.writers.items);
            persistent_specs[i] = .{
                .name = try self.globals.allocator.dupe(u8, spec.name),
                .sym = spec.sym,
                .field_type = spec.field_type,
                .initform = spec.initform,
                .initargs = std.ArrayList(Value).fromOwnedSlice(initargs_slice),
                .readers = std.ArrayList(Value).fromOwnedSlice(readers_slice),
                .writers = std.ArrayList(Value).fromOwnedSlice(writers_slice),
                .is_direct = spec.is_direct,
            };
        }
        var qual_buf: [256]u8 = undefined;
        const q = try self.qualifyName(class_name, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const persistent_class_name = try self.globals.allocator.dupe(u8, q.name);
        try self.class_metadata.put(persistent_class_name, persistent_specs);

        // Also store in heap for runtime slot-value lookup
        {
            const heap_slot_names = try heap.backing_allocator.alloc(Value, slot_specs.items.len);
            for (slot_specs.items, 0..) |spec, i| {
                if (!spec.sym.isSymbol()) return error.InvalidSyntax;
                heap_slot_names[i] = spec.sym;
            }
            try heap.class_metadata.put(heap.backing_allocator, name_val, heap_slot_names);
        }

        // Allocate Class object and compute CPL, then register in class registry
        const class_obj = try self.allocateClass(heap, name_val, superclasses, slot_specs.items);
        try heap.putLispClass(name_val, class_obj);

        // Build defclass expansion forms dynamically; reader/writer options may
        // include non-symbol entries that are skipped, so fixed-size pre-counting
        // can leave uninitialized tails.
        var defs = std.ArrayList(*Ir){};
        defer defs.deinit(self.allocator);

        // 1. Constructor: (defun make-class-name (slot1 slot2 ...) (vector 'class-name slot1 slot2 ...))
        const make_name = try self.concatStrings("make-", class_name);
        try defs.append(self.allocator, try self.generateStructConstructor(heap, make_name, class_name, slot_specs.items, env));

        // 2. Predicate: (defun class-name-p (obj) (and (vectorp obj) (eq (aref obj 0) 'class-name)))
        const pred_name = try self.concatStrings(class_name, "-p");
        try defs.append(self.allocator, try self.generateStructPredicate(heap, pred_name, class_name));

        // Register predicate for occurrence typing (use qualified name to match globals table)
        var pred_qual_buf: [512]u8 = undefined;
        const q_pred = try self.qualifyName(pred_name, &pred_qual_buf);
        defer if (q_pred.owned) self.allocator.free(q_pred.name);
        const persistent_pred_name = try self.globals.allocator.dupe(u8, q_pred.name);
        try self.struct_predicates.put(persistent_pred_name, class_type);

        // 3. Default accessors: (defun class-name-slot (obj) (if (class-name-p obj) (aref obj N+1) (error)))
        for (slot_specs.items, 0..) |spec, i| {
            const accessor_name = try self.concatStrings3(class_name, "-", spec.name);
            try defs.append(self.allocator, try self.generateStructAccessor(heap, accessor_name, class_name, i));
            try self.registerStructAccessor(accessor_name, i);
        }

        // 4. Default writers: (defun (setf class-name-slot) (val obj) ...)
        for (slot_specs.items, 0..) |spec, i| {
            const accessor_name = try self.concatStrings3(class_name, "-", spec.name);
            const setf_name = try self.concatStrings("(setf ", accessor_name);
            const setf_full = try self.concatStrings(setf_name, ")");
            try defs.append(self.allocator, try self.generateStructWriter(heap, setf_full, class_name, i));
        }

        // 5. Readers: (defun reader-name (obj) (class-name-slot obj))
        for (slot_specs.items, 0..) |spec, i| {
            for (spec.readers.items) |reader_val| {
                if (!reader_val.isSymbol()) continue;
                const reader_sym = reader_val.toPtr(Symbol);
                const reader_name = reader_sym.getName();
                try defs.append(self.allocator, try self.generateStructAccessor(heap, reader_name, class_name, i));
                var writable_reader = false;
                for (spec.writers.items) |writer_val| {
                    if (!writer_val.isSymbol()) continue;
                    if (std.mem.eql(u8, writer_val.toPtr(Symbol).getName(), reader_name)) {
                        writable_reader = true;
                        break;
                    }
                }
                if (writable_reader) {
                    try self.registerStructAccessor(reader_name, i);
                }
            }
        }

        // 6. Writers: (defun (setf writer-name) (val obj) (if (class-name-p obj) (setf (aref obj N+1) val) (error)))
        for (slot_specs.items, 0..) |spec, i| {
            for (spec.writers.items) |writer_val| {
                if (!writer_val.isSymbol()) continue;
                const writer_sym = writer_val.toPtr(Symbol);
                const writer_name = writer_sym.getName();
                const setf_name = try self.concatStrings("(setf ", writer_name);
                const setf_full = try self.concatStrings(setf_name, ")");
                try defs.append(self.allocator, try self.generateStructWriter(heap, setf_full, class_name, i));
            }
        }

        // 7. Return class name
        try defs.append(self.allocator, try self.builder.lit(name_val));

        return try self.builder.progn(defs.items);
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
        const class_sym = class_name_expr.toPtr(Symbol);
        const class_name = class_sym.getName();

        const slot_specs = if (try self.lookupClassMetadataBySymbol(class_sym)) |val| val else return error.InvalidSyntax;

        // Parse keyword arguments and build positional args array
        const slot_values = try self.allocator.alloc(?*Ir, slot_specs.len);
        for (slot_values) |*sv| sv.* = null;

        var rest = cons1.cdr;
        while (rest.isCons()) {
            const kw_cons = rest.toPtr(Cons);
            const kw = kw_cons.car;

            switch (kw.typeKind()) {
                .keyword, .symbol, .nil, .t => {},
                else => return error.InvalidSyntax,
            }

            // Get value (next element after keyword)
            if (!kw_cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = kw_cons.cdr.toPtr(Cons);
            const value_ir = try self.compile(val_cons.car, env);

            // Find matching slot by initarg keyword identity
            var matched = false;
            for (slot_specs, 0..) |spec, i| {
                for (spec.initargs.items) |initarg| {
                    if (kw.eq(initarg)) {
                        slot_values[i] = value_ir;
                        matched = true;
                        break;
                    }
                }
                if (matched) break;
            }

            // Move to next keyword-value pair
            rest = val_cons.cdr;
        }

        // Build call to make-class-name with positional args
        const ctor_name_plain = try self.concatStrings("make-", class_name);

        // Qualify the constructor name with current package
        var qual_buf: [512]u8 = undefined;
        const q = try self.qualifyName(ctor_name_plain, &qual_buf);
        defer if (q.owned) self.allocator.free(q.name);
        const ctor_name = q.name;

        const call_args = try self.allocator.alloc(*Ir, slot_specs.len);
        for (slot_values, 0..) |maybe_val, i| {
            if (maybe_val) |val| {
                call_args[i] = val;
            } else {
                // No value provided - use initform or unbound
                if (slot_specs[i].initform) |initform_expr| {
                    call_args[i] = try self.compile(initform_expr, env);
                } else {
                    call_args[i] = try self.builder.lit(Value.unbound);
                }
            }
        }

        const ctor_idx = if (self.globals.lookup(ctor_name)) |val| val else return error.InvalidSyntax;
        const ctor_ref = try self.builder.globalRef(ctor_name, ctor_idx);
        const ctor_call = try self.builder.call(ctor_ref, call_args);

        // Call initialize-instance on the new object (for :before/:after/:around methods)
        // Emit: (let ((#:obj (ctor ...))) (initialize-instance #:obj) #:obj)
        // Try multiple name forms since stdlib may use qualified names
        const init_idx_opt = self.globals.lookup("initialize-instance") orelse
            self.globals.lookup("COMMON-LISP:INITIALIZE-INSTANCE") orelse
            self.globals.lookup("CL:INITIALIZE-INSTANCE") orelse
            self.lookupGlobalIdxWithFallback("initialize-instance");
        if (init_idx_opt) |init_idx| {
            const init_ref = try self.builder.globalRef("initialize-instance", init_idx);
            const init_args = try self.allocator.alloc(*Ir, 1);
            const local_idx = env.localCount();
            const var_ref = try self.builder.variable("#:init-obj", 0, local_idx);
            init_args[0] = var_ref;
            const init_call = try self.builder.call(init_ref, init_args);
            const var_ref2 = try self.builder.variable("#:init-obj", 0, local_idx);
            const body = try self.builder.progn(&[_]*Ir{ init_call, var_ref2 });
            return try self.builder.let1("#:init-obj", local_idx, ctor_call, body);
        }

        return ctor_call;
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

    /// Compile slot-boundp: (slot-boundp obj 'slot-name)
    fn compileSlotBoundp(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
        return try self.builder.slotBoundp(obj_ir, slot_sym);
    }

    /// Compile slot-makunbound: (slot-makunbound obj 'slot-name)
    fn compileSlotMakunbound(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
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
        return try self.builder.slotMakunbound(obj_ir, slot_sym);
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

    /// Compile class-of: (class-of obj)
    /// Returns the class of an object
    fn compileClassOf(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const obj_expr = cons1.car;
        const obj_ir = try self.compile(obj_expr, env);

        return try self.builder.classOf(obj_ir);
    }

    /// Compile find-class with CL optional args:
    /// (find-class name &optional errorp environment)
    /// Runtime currently uses %find-class for lookup and ignores optional flags.
    /// We still compile optional args for side effects before the lookup.
    fn compileFindClass(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const first = args.toPtr(Cons);
        const class_name_ir = try self.compile(first.car, env);

        const lookup_ir = blk: {
            const node = try self.allocator.create(Ir);
            node.* = .{ .find_class = .{ .operand = class_name_ir } };
            break :blk node;
        };

        if (first.cdr.isNil()) return lookup_ir;

        var exprs = std.ArrayList(*const Ir){};
        defer exprs.deinit(self.allocator);

        var rest = first.cdr;
        while (rest.isCons()) : (rest = rest.toPtr(Cons).cdr) {
            const cell = rest.toPtr(Cons);
            try exprs.append(self.allocator, try self.compile(cell.car, env));
        }
        try exprs.append(self.allocator, lookup_ir);
        return try self.builder.progn(exprs.items);
    }

    /// Compile defgeneric: (defgeneric name (arg1 arg2 ...))
    /// Creates a generic function that dispatches on argument types
    fn compileDefgeneric(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        _ = env;
        const trace_defgeneric = std.posix.getenv("HABU_TRACE_DEFGENERIC") != null;

        if (!args.isCons()) {
            if (trace_defgeneric) std.debug.print("TRACE defgeneric invalid: args not cons kind={s}\n", .{@tagName(args.typeKind())});
            return error.InvalidSyntax;
        }
        const cons1 = args.toPtr(Cons);
        const name_spec = cons1.car;
        var name_val = name_spec;
        var qual_buf: [512]u8 = undefined;
        var q_name: []const u8 = "";
        var q_owned = false;
        defer if (q_owned) self.allocator.free(q_name);
        var setf_name: ?[]u8 = null;
        defer if (setf_name) |mem| self.allocator.free(mem);

        switch (name_spec.typeKind()) {
            .symbol => {
                const name_sym = name_spec.toPtr(Symbol);
                const q = try self.getQualifiedName(name_sym, &qual_buf);
                q_owned = q.owned;
                q_name = q.name;
            },
            .cons => {
                const outer = name_spec.toPtr(Cons);
                const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
                if (self.canonicalBuiltinSymbol(outer.car).raw != b.setf.raw) {
                    if (trace_defgeneric) {
                        std.debug.print("TRACE defgeneric invalid: cons name not setf head-kind={s}\n", .{@tagName(outer.car.typeKind())});
                    }
                    return error.InvalidSyntax;
                }
                if (!outer.cdr.isCons()) {
                    if (trace_defgeneric) std.debug.print("TRACE defgeneric invalid: setf name missing target\n", .{});
                    return error.InvalidSyntax;
                }
                const inner = outer.cdr.toPtr(Cons);
                if (!inner.car.isSymbol() or !inner.cdr.isNil()) {
                    if (trace_defgeneric) {
                        std.debug.print(
                            "TRACE defgeneric invalid: setf target malformed target-kind={s} tail-kind={s}\n",
                            .{ @tagName(inner.car.typeKind()), @tagName(inner.cdr.typeKind()) },
                        );
                    }
                    return error.InvalidSyntax;
                }
                const base_name = inner.car.toPtr(Symbol).getName();
                const setf_name_mem = try std.fmt.allocPrint(self.allocator, "(SETF {s})", .{base_name});
                setf_name = setf_name_mem;
                const q = try self.qualifyName(setf_name_mem, &qual_buf);
                q_owned = q.owned;
                q_name = q.name;
                name_val = name_spec;
            },
            else => {
                if (trace_defgeneric) {
                    std.debug.print("TRACE defgeneric invalid: unsupported name kind={s}\n", .{@tagName(name_spec.typeKind())});
                }
                return error.InvalidSyntax;
            },
        }

        const gen_name = try self.allocator.dupe(u8, q_name);

        // Parse lambda-list
        if (!cons1.cdr.isCons()) {
            if (trace_defgeneric) std.debug.print("TRACE defgeneric invalid: missing lambda-list\n", .{});
            return error.InvalidSyntax;
        }
        const cons2 = cons1.cdr.toPtr(Cons);
        const lambda_list = cons2.car;

        // Register generic function
        const persistent_name = try self.globals.allocator.dupe(u8, gen_name);
        try self.generic_functions.put(persistent_name, std.ArrayList(MethodDef){});

        // Register as global
        const idx = try self.globals.define(gen_name);

        // Generate: (define name (%make-generic-function name lambda-list))
        const name_ir = try self.builder.lit(name_val);
        const lambda_list_ir = try self.builder.lit(lambda_list);
        const gf_ir = try self.builder.makeGenericFunction(name_ir, lambda_list_ir);

        return try self.builder.define(gen_name, idx, gf_ir);
    }

    /// Compile defmethod: (defmethod name [qualifier] ((arg1 class1) ...) body...)
    /// qualifier is :before, :after, or :around (optional, default is primary)
    fn compileDefmethod(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const name_spec = cons1.car;
        var name_val = name_spec;
        var simple_name: []const u8 = undefined;
        var qual_buf: [512]u8 = undefined;
        var gen_name_tmp: []const u8 = "";
        var q_owned = false;
        defer if (q_owned) self.allocator.free(gen_name_tmp);
        var setf_name: ?[]u8 = null;
        defer if (setf_name) |mem| self.allocator.free(mem);

        switch (name_spec.typeKind()) {
            .symbol => {
                const name_sym = name_spec.toPtr(Symbol);
                simple_name = name_sym.getName();
                const q = try self.getQualifiedName(name_sym, &qual_buf);
                q_owned = q.owned;
                gen_name_tmp = q.name;
            },
            .cons => {
                const outer = name_spec.toPtr(Cons);
                const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
                if (self.canonicalBuiltinSymbol(outer.car).raw != b.setf.raw) return error.InvalidSyntax;
                if (!outer.cdr.isCons()) return error.InvalidSyntax;
                const inner = outer.cdr.toPtr(Cons);
                if (!inner.car.isSymbol() or !inner.cdr.isNil()) return error.InvalidSyntax;
                const base_name = inner.car.toPtr(Symbol).getName();
                const setf_name_mem = try std.fmt.allocPrint(self.allocator, "(SETF {s})", .{base_name});
                setf_name = setf_name_mem;
                simple_name = setf_name_mem;
                const q = try self.qualifyName(simple_name, &qual_buf);
                q_owned = q.owned;
                gen_name_tmp = q.name;
                name_val = name_spec;
            },
            else => return error.InvalidSyntax,
        }

        // Check for method qualifier (:before, :after, :around)
        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        var rest = cons1.cdr.toPtr(Cons);
        var qualifier: MethodQualifier = .primary;

        // Check if second element is a qualifier keyword
        if (rest.car.isKeyword()) {
            const builtins = if (self.builtins) |val| val else return error.CompilerNotInitialized;
            if (rest.car.eq(builtins.kw_before)) {
                qualifier = .before;
            } else if (rest.car.eq(builtins.kw_after)) {
                qualifier = .after;
            } else if (rest.car.eq(builtins.kw_around)) {
                qualifier = .around;
            } else {
                return error.InvalidMethodQualifier;
            }
            // Advance past qualifier
            if (!rest.cdr.isCons()) return error.InvalidSyntax;
            rest = rest.cdr.toPtr(Cons);
        }

        // Parse specialized lambda list
        const lambda_list = rest.car;
        const body_cons = rest;

        // Extract parameter names and specializers (interned symbols)
        var dispatch_params = std.ArrayList([]const u8){};
        defer dispatch_params.deinit(self.allocator);
        var specializers = std.ArrayList(Value){};
        defer specializers.deinit(self.allocator);

        const heap = if (self.heap) |val| val else return error.CompilerNotInitialized;

        // Lambda environment with method parameters (bind while parsing).
        var lambda_env = Env.init(self.allocator, env);
        defer lambda_env.deinit();

        var params = lambda_list;
        while (params.isCons()) {
            const param_cons = params.toPtr(Cons);
            const param = param_cons.car;

            switch (param.typeKind()) {
                .symbol => {
                    // Unspecialized parameter
                    const param_name = param.toPtr(Symbol).getName();
                    try dispatch_params.append(self.allocator, try self.allocator.dupe(u8, param_name));
                    _ = try lambda_env.bindSym(param);
                    try specializers.append(self.allocator, Value.t); // t = any type
                },
                .cons => {
                    // Specialized parameter: (param-name class-name)
                    const spec_cons = param.toPtr(Cons);
                    if (!spec_cons.car.isSymbol()) return error.InvalidSyntax;
                    const param_name = spec_cons.car.toPtr(Symbol).getName();
                    try dispatch_params.append(self.allocator, try self.allocator.dupe(u8, param_name));
                    _ = try lambda_env.bindSym(spec_cons.car);

                    if (spec_cons.cdr.isCons()) {
                        const class_cons = spec_cons.cdr.toPtr(Cons);
                        if (class_cons.car.isSymbol()) {
                            // Intern the class name symbol
                            const class_name = class_cons.car.toPtr(Symbol).getName();
                            try specializers.append(self.allocator, try heap.intern(class_name));
                        } else if (class_cons.car.isCons()) {
                            // (eql obj) specializer
                            if (self.eqlSpecializerObject(class_cons.car) != null) {
                                try specializers.append(self.allocator, class_cons.car);
                            } else {
                                try specializers.append(self.allocator, Value.t);
                            }
                        } else {
                            try specializers.append(self.allocator, Value.t);
                        }
                    } else {
                        try specializers.append(self.allocator, Value.t);
                    }
                },
                else => return error.InvalidSyntax,
            }

            params = param_cons.cdr;
        }

        // Save param names for call-next-method before compiling body
        const dispatch_params_for_cnm = try self.allocator.dupe([]const u8, dispatch_params.items);
        const saved_method_params = self.method_params;
        self.method_params = dispatch_params_for_cnm;
        defer self.method_params = saved_method_params;

        // Compile method body in lambda environment
        const body = body_cons.cdr;
        const body_ir = try self.compileBodyWithTail(body, &lambda_env, true);

        // Collect free variables for captures
        var capture_set = CaptureSet.init(self.allocator);
        defer capture_set.deinit();
        try self.collectFreeVars(body, &lambda_env, &capture_set);
        const captures = try self.allocator.dupe(Ir.Capture, capture_set.captures.items);

        // Save param names for dispatcher before toOwnedSlice consumes them
        const dispatch_params_copy = try self.allocator.dupe([]const u8, dispatch_params.items);

        // Create lambda
        const lambda_ir = try self.builder.lambda(
            try dispatch_params.toOwnedSlice(self.allocator),
            &[_]Ir.OptionalParam{},
            &[_]Ir.KeyParam{},
            false,
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

        // Generate unique method function name: generic-name$qualifier$specializer
        // Include qualifier to differentiate :before/:after/:around from primary
        // Use simple (non-qualified) name - VM looks up by local name
        const qual_str: []const u8 = switch (qualifier) {
            .primary => "p",
            .before => "b",
            .after => "a",
            .around => "r",
        };
        var spec_owned: ?[]u8 = null;
        defer if (spec_owned) |s| self.allocator.free(s);
        const spec_str = if (specializers.items.len > 0) blk: {
            const spec_val = specializers.items[0];
            switch (spec_val.typeKind()) {
                .t => break :blk "t",
                .symbol => break :blk spec_val.toPtr(Symbol).getName(),
                .cons => {
                    if (self.eqlSpecializerObject(spec_val)) |eql_obj| {
                        const txt = try std.fmt.allocPrint(self.allocator, "eql_{x}", .{eql_obj.raw});
                        spec_owned = txt;
                        break :blk txt;
                    }
                    break :blk "cons";
                },
                else => break :blk "obj",
            }
        } else "t";
        const method_name = try std.fmt.allocPrint(self.allocator, "{s}${s}${s}", .{ simple_name, qual_str, spec_str });

        // Define method as global function
        const method_global_idx = try self.globals.define(method_name);
        const method_define_ir = try self.builder.define(method_name, method_global_idx, lambda_ir);

        // Store method function name (persistent, needs globals.allocator)
        const persistent_method_name = try self.globals.allocator.dupe(u8, method_name);
        // Copy specializer Values (they're interned, so no string duplication needed)
        const persistent_specializers = try self.globals.allocator.dupe(Value, specializers.items);

        // Create method def
        const method_def = MethodDef{
            .specializers = persistent_specializers,
            .function_name = persistent_method_name,
            .qualifier = qualifier,
        };

        // Manually grow the methods list
        // Use globals.allocator for persistent storage across arena resets
        // Also track if this is a new GF (needs implicit creation)
        const needs_implicit_gf = !gop.found_existing;
        if (needs_implicit_gf) {
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
        const dispatcher = try self.generateMethodDispatcher(gen_name, gop.value_ptr.*, dispatch_params_copy);

        // Build qualifiers list for Method object (e.g., (:before) or nil for primary)
        const builtins = if (self.builtins) |val| val else return error.CompilerNotInitialized;
        const qualifiers_list: Value = switch (qualifier) {
            .primary => Value.nil,
            .before => try heap.allocCons(builtins.kw_before, Value.nil),
            .after => try heap.allocCons(builtins.kw_after, Value.nil),
            .around => try heap.allocCons(builtins.kw_around, Value.nil),
        };

        // Build specializers list for Method object
        var specializers_list = Value.nil;
        var spec_i = specializers.items.len;
        while (spec_i > 0) {
            spec_i -= 1;
            specializers_list = try heap.allocCons(specializers.items[spec_i], specializers_list);
        }

        // IR for qualifiers, specializers, lambda_list, and method function reference
        const qualifiers_ir = try self.builder.lit(qualifiers_list);
        const specializers_ir = try self.builder.lit(specializers_list);
        const lambda_list_ir = try self.builder.lit(lambda_list);
        const method_fn_ir = try self.builder.globalRef(method_name, method_global_idx);

        // Create Method object
        const make_method_ir = try self.builder.makeMethod(qualifiers_ir, specializers_ir, lambda_list_ir, method_fn_ir);

        // Ensure a global slot exists and promote non-GF values to a generic function
        // before adding methods. This handles names currently bound to plain closures.
        const global_idx = self.globals.lookup(gen_name) orelse try self.globals.define(gen_name);

        // Build lambda list from dispatch params for potential implicit GF creation.
        var gf_params = Value.nil;
        var i = dispatch_params_copy.len;
        while (i > 0) {
            i -= 1;
            const param_sym = try heap.intern(dispatch_params_copy[i]);
            gf_params = try heap.allocCons(param_sym, gf_params);
        }

        const name_ir = try self.builder.lit(name_val);
        const gf_params_ir = try self.builder.lit(gf_params);
        const gf_create_ir = try self.builder.makeGenericFunction(name_ir, gf_params_ir);
        const gf_def_ir = try self.builder.define(gen_name, global_idx, gf_create_ir);

        const gf_existing_for_check = try self.builder.globalRef(gen_name, global_idx);
        const gf_type_sym = (try heap.internInPackage("CL", "generic-function")) orelse try heap.intern("generic-function");
        const gf_type_ir = try self.builder.lit(gf_type_sym);
        const gf_is_generic_ir = try self.builder.typep(gf_existing_for_check, gf_type_ir);
        const gf_existing_for_add = try self.builder.globalRef(gen_name, global_idx);
        const gf_ready_ir = try self.builder.ifExpr(gf_is_generic_ir, gf_existing_for_add, gf_def_ir);

        const add_method_ir = try self.builder.addMethod(gf_ready_ir, make_method_ir);
        const gf_for_dispatch = try self.builder.globalRef(gen_name, global_idx);
        const set_dispatcher_ir = try self.builder.setGfDispatcher(gf_for_dispatch, dispatcher);

        const defs = try self.allocator.alloc(*Ir, 3);
        defs[0] = method_define_ir;
        defs[1] = add_method_ir;
        defs[2] = set_dispatcher_ir;
        return try self.builder.progn(defs);
    }

    /// Compile call-next-method: (call-next-method [args...])
    /// Calls the next applicable method in the dispatch chain
    fn compileCallNextMethod(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // call-next-method calls the %next-method% special variable
        // If no args provided, pass the original method arguments
        // If args provided, use those

        const nm_name = try self.getNextMethodName();

        const nm_global_idx = self.globals.lookup(nm_name) orelse blk: {
            break :blk try self.globals.define(nm_name);
        };

        const next_method_ir = try self.builder.globalRef(nm_name, nm_global_idx);

        var arg_irs = std.ArrayList(*Ir){};
        defer arg_irs.deinit(self.allocator);

        if (args.isNil()) {
            // No args provided - pass original method parameters
            if (self.method_params) |params| {
                for (params, 0..) |param_name, idx| {
                    // Look up param in environment to get correct depth/index
                    if (env.lookupName(param_name)) |binding| {
                        const var_ir = try self.builder.variable(param_name, binding.depth, binding.index);
                        try arg_irs.append(self.allocator, var_ir);
                    } else {
                        // Shouldn't happen - param should be in scope
                        const var_ir = try self.builder.variable(param_name, 0, @intCast(idx));
                        try arg_irs.append(self.allocator, var_ir);
                    }
                }
            }
        } else {
            // Explicit args provided
            var curr = args;
            while (curr.isCons()) {
                const cons = curr.toPtr(Cons);
                try arg_irs.append(self.allocator, try self.compile(cons.car, env));
                curr = cons.cdr;
            }
        }

        const call_ir = try self.builder.call(next_method_ir, try arg_irs.toOwnedSlice(self.allocator));

        // Check if %next-method% is nil before calling
        // (if %next-method% (call %next-method% args...) (no-next-method gf method args...))
        const next_method_check_ir = try self.builder.globalRef(nm_name, nm_global_idx);

        // Call (no-next-method gf method args...)
        const no_next_fn_idx = self.globals.lookup("COMMON-LISP:NO-NEXT-METHOD") orelse
            self.globals.lookup("CL:NO-NEXT-METHOD") orelse
            self.lookupGlobalIdxWithFallback("no-next-method") orelse
            try self.globals.define("COMMON-LISP:NO-NEXT-METHOD");
        const no_next_fn = try self.builder.globalRef("COMMON-LISP:NO-NEXT-METHOD", no_next_fn_idx);

        // Build args: gf=nil, method=nil, original args
        var no_next_args = std.ArrayList(*Ir){};
        defer no_next_args.deinit(self.allocator);
        try no_next_args.append(self.allocator, try self.builder.lit(Value.nil));
        try no_next_args.append(self.allocator, try self.builder.lit(Value.nil));

        // Add original method args
        if (self.method_params) |params| {
            for (params, 0..) |param_name, idx| {
                if (env.lookupName(param_name)) |binding| {
                    const var_ir = try self.builder.variable(param_name, binding.depth, binding.index);
                    try no_next_args.append(self.allocator, var_ir);
                } else {
                    const var_ir = try self.builder.variable(param_name, 0, @intCast(idx));
                    try no_next_args.append(self.allocator, var_ir);
                }
            }
        }

        const no_next_call = try self.builder.call(no_next_fn, try no_next_args.toOwnedSlice(self.allocator));

        return try self.builder.ifExpr(next_method_check_ir, call_ir, no_next_call);
    }

    /// Compile next-method-p: returns t if call-next-method would succeed, nil otherwise
    fn compileNextMethodP(self: *Compiler, env: *const Env) anyerror!*Ir {
        _ = env;
        // Check if %next-method% is bound and non-nil
        const nm_name = try self.getNextMethodName();

        const nm_global_idx = self.globals.lookup(nm_name) orelse blk: {
            break :blk try self.globals.define(nm_name);
        };

        // Get %next-method% value
        const next_method_ir = try self.builder.globalRef(nm_name, nm_global_idx);

        // Check if it's non-nil: (if %next-method% t nil)
        const t_ir = try self.builder.lit(Value.t);
        const nil_ir = try self.builder.lit(Value.nil);
        return try self.builder.ifExpr(next_method_ir, t_ir, nil_ir);
    }

    /// Generate a dispatcher lambda implementing standard method combination
    /// Order: :around (call-next-method) -> :before -> primary -> :after
    fn generateMethodDispatcher(
        self: *Compiler,
        gf_name: []const u8,
        methods: std.ArrayList(MethodDef),
        _: []const []const u8,
    ) anyerror!*Ir {
        // Compute max arity across all methods - dispatcher needs to accept all params
        var max_arity: usize = 0;
        for (methods.items) |method| {
            if (method.specializers.len > max_arity) {
                max_arity = method.specializers.len;
            }
        }

        // Generate parameter names for dispatcher: arg0, arg1, ...
        var dispatcher_params = std.ArrayList([]const u8){};
        defer dispatcher_params.deinit(self.allocator);
        for (0..max_arity) |i| {
            const pname = try std.fmt.allocPrint(self.allocator, "arg{d}", .{i});
            try dispatcher_params.append(self.allocator, pname);
        }
        const dispatch_params = try dispatcher_params.toOwnedSlice(self.allocator);

        // Separate methods by qualifier
        var primary_methods = std.ArrayList(MethodDef){};
        defer primary_methods.deinit(self.allocator);
        var before_methods = std.ArrayList(MethodDef){};
        defer before_methods.deinit(self.allocator);
        var after_methods = std.ArrayList(MethodDef){};
        defer after_methods.deinit(self.allocator);
        var around_methods = std.ArrayList(MethodDef){};
        defer around_methods.deinit(self.allocator);

        for (methods.items) |method| {
            switch (method.qualifier) {
                .primary => try primary_methods.append(self.allocator, method),
                .before => try before_methods.append(self.allocator, method),
                .after => try after_methods.append(self.allocator, method),
                .around => try around_methods.append(self.allocator, method),
            }
        }

        // Sort each list by specificity (most specific first)
        try self.sortMethodsBySpecificity(primary_methods.items);
        try self.sortMethodsBySpecificity(before_methods.items);
        try self.sortMethodsBySpecificity(after_methods.items);
        try self.sortMethodsBySpecificity(around_methods.items);

        // Build dispatcher body: nested if-then-else checking types
        var dispatch_body: *Ir = undefined;

        // Start from the end: call no-applicable-method
        // (no-applicable-method gf-sym arg1 arg2 ...)
        const gf_sym = try self.heap.?.intern(gf_name);
        const gf_lit = try self.builder.lit(gf_sym);
        var no_app_args = std.ArrayList(*Ir){};
        try no_app_args.append(self.allocator, gf_lit);
        for (dispatch_params, 0..) |pname, idx| {
            const arg_ir = try self.builder.variable(pname, 0, @intCast(idx));
            try no_app_args.append(self.allocator, arg_ir);
        }
        // Look up no-applicable-method as a global function
        const no_app_idx = self.globals.lookup("COMMON-LISP:NO-APPLICABLE-METHOD") orelse
            self.globals.lookup("CL:NO-APPLICABLE-METHOD") orelse
            self.lookupGlobalIdxWithFallback("no-applicable-method") orelse
            try self.globals.define("COMMON-LISP:NO-APPLICABLE-METHOD");
        const no_app_fn = try self.builder.globalRef("COMMON-LISP:NO-APPLICABLE-METHOD", no_app_idx);
        dispatch_body = try self.builder.call(no_app_fn, no_app_args.items);

        // Build dispatch chain for primary methods (type checking)
        // For each primary method, also run applicable :before and :after
        // Track effective methods for call-next-method chaining
        var prev_effective: ?*Ir = null;
        var i = primary_methods.items.len;
        while (i > 0) {
            i -= 1;
            const primary = primary_methods.items[i];

            // Build condition by AND-ing all parameter type checks
            var cond_ir: ?*Ir = null;

            for (primary.specializers, 0..) |spec_val, param_idx| {
                if (param_idx >= dispatch_params.len) return error.InvalidSyntax;

                // Reference to parameter
                const arg_ir = try self.builder.variable(dispatch_params[param_idx], 0, @intCast(param_idx));
                const check_ir = if (try self.buildMethodSpecializerCheck(spec_val, arg_ir)) |check|
                    check
                else
                    continue;

                // AND with previous checks: (if prev check nil)
                cond_ir = if (cond_ir) |prev| blk: {
                    const nil_ir = try self.builder.lit(Value.nil);
                    break :blk try self.builder.ifExpr(prev, check_ir, nil_ir);
                } else check_ir;
            }

            // Build effective method: before* -> primary -> after*
            // For call-next-method: use prev_effective (the less-specific method)
            // For the least specific method, prev_effective is null -> %next-method% = nil
            const effective_method = try self.buildEffectiveMethod(
                primary,
                before_methods.items,
                after_methods.items,
                dispatch_params,
                prev_effective,
            );
            prev_effective = effective_method;

            // If all parameters are unspecialized, this method always matches
            if (cond_ir == null) {
                dispatch_body = effective_method;
                continue;
            }

            // Wrap in if
            dispatch_body = try self.builder.ifExpr(cond_ir.?, effective_method, dispatch_body);
        }

        // Handle :around methods with call-next-method support
        // Build a chain: around1 -> around2 -> ... -> effective_method
        if (around_methods.items.len > 0) {
            var next_method = dispatch_body;

            // Build chain from innermost (last around) to outermost (first around)
                var a = around_methods.items.len;
                while (a > 0) {
                    a -= 1;
                    const around = around_methods.items[a];

                    // Wrap next_method in a lambda so it can be called via %next-method%
                    const lambda_params = try self.allocator.dupe([]const u8, dispatch_params);
                    const lambda_captures = try self.allocator.alloc(Ir.Capture, dispatch_params.len);
                    for (dispatch_params, 0..) |pname, idx| {
                        lambda_captures[idx] = .{
                            .name = pname,
                            .depth = 0,
                            .index = @intCast(idx),
                        };
                    }
                    const next_method_lambda = try self.builder.lambda(
                        lambda_params,
                        &[_]Ir.OptionalParam{},
                        &[_]Ir.KeyParam{},
                        false,
                        null,
                        lambda_captures,
                        next_method,
                    );

                // Define %next-method% global (use qualified name to match symbol interning)
                const nm_name = try self.getNextMethodName();
                const nm_idx = self.globals.lookup(nm_name) orelse try self.globals.define(nm_name);
                const set_next = try self.builder.define(nm_name, nm_idx, next_method_lambda);

                // Call the around method
                const around_call = try self.generateMethodCall(around, dispatch_params);

                // Sequence: set %next-method%, then call around method
                const seq = try self.allocator.alloc(*Ir, 2);
                seq[0] = set_next;
                seq[1] = around_call;
                next_method = try self.builder.progn(seq);
            }

            dispatch_body = next_method;
        }

        // Wrap dispatch body in lambda with all params optional (default nil)
        // This allows calling with fewer args than max arity
        var opt_params = std.ArrayList(Ir.OptionalParam){};
        defer opt_params.deinit(self.allocator);
        const nil_ir = try self.builder.lit(Value.nil);
        for (dispatch_params) |pname| {
            try opt_params.append(self.allocator, .{ .name = pname, .default = nil_ir });
        }

        const dispatcher = try self.builder.lambda(
            &[_][]const u8{}, // No required params
            try opt_params.toOwnedSlice(self.allocator),
            &[_]Ir.KeyParam{},
            false,
            null,
            &[_]Ir.Capture{}, // No captures - methods are stored as IR
            dispatch_body,
        );

        return dispatcher;
    }

    /// Build effective method: before* -> primary -> after*
    /// Returns the primary method's value (after methods are for side effects only)
    /// next_method_body: IR representing the next less-specific method (or nil)
    fn buildEffectiveMethod(
        self: *Compiler,
        primary: MethodDef,
        before_methods: []const MethodDef,
        after_methods: []const MethodDef,
        dispatch_params: []const []const u8,
        next_method_body: ?*Ir,
    ) anyerror!*Ir {
        // Count :after methods - all will be conditionally run at runtime
        const after_count: usize = after_methods.len;

        // Check if we have any :before methods - all will be conditionally run at runtime
        const has_before = before_methods.len > 0;

        // Bind %next-method% to closure or nil
        const nm_name = try self.getNextMethodName();
        const nm_idx = self.globals.lookup(nm_name) orelse try self.globals.define(nm_name);

        const next_method_value = if (next_method_body) |body| blk: {
            const lambda_params = try self.allocator.dupe([]const u8, dispatch_params);
            const lambda_captures = try self.allocator.alloc(Ir.Capture, dispatch_params.len);
            for (dispatch_params, 0..) |pname, idx| {
                lambda_captures[idx] = .{
                    .name = pname,
                    .depth = 0,
                    .index = @intCast(idx),
                };
            }
            break :blk try self.builder.lambda(
                lambda_params,
                &[_]Ir.OptionalParam{},
                &[_]Ir.KeyParam{},
                false,
                null,
                lambda_captures,
                body,
            );
        } else try self.builder.lit(Value.nil);

        const set_next = try self.builder.define(nm_name, nm_idx, next_method_value);

        if (!has_before and after_count == 0) {
            // Just primary, wrapped with %next-method% binding
            const primary_call = try self.generateMethodCall(primary, dispatch_params);
            const seq = try self.allocator.alloc(*Ir, 2);
            seq[0] = set_next;
            seq[1] = primary_call;
            return try self.builder.progn(seq);
        }

        var stmts = std.ArrayList(*Ir){};
        defer stmts.deinit(self.allocator);

        // Bind %next-method% first
        try stmts.append(self.allocator, set_next);

        // Call applicable :before methods (most specific first)
        // Generate runtime typep checks for each :before method
        for (before_methods) |before| {
            // Build condition: (typep arg class) for each specialized parameter
            var cond: ?*Ir = null;
            for (before.specializers, 0..) |spec, param_idx| {
                if (param_idx >= dispatch_params.len) continue;

                const arg_ir = try self.builder.variable(dispatch_params[param_idx], 0, @intCast(param_idx));
                const check = if (try self.buildMethodSpecializerCheck(spec, arg_ir)) |val|
                    val
                else
                    continue;

                cond = if (cond) |prev| blk: {
                    const nil_ir = try self.builder.lit(Value.nil);
                    break :blk try self.builder.ifExpr(prev, check, nil_ir);
                } else check;
            }

            // Wrap call in conditional if there are specializers
            const before_call = try self.generateMethodCall(before, dispatch_params);
            if (cond) |c| {
                const nil_ir = try self.builder.lit(Value.nil);
                try stmts.append(self.allocator, try self.builder.ifExpr(c, before_call, nil_ir));
            } else {
                try stmts.append(self.allocator, before_call);
            }
        }

        // If we have :after methods, save primary result and return it at the end
        if (after_count > 0) {
            // Generate: (let ((%result% (primary args))) (after1) ... (afterN) %result%)
            const result_name = "%method-result%";
            const primary_call = try self.generateMethodCall(primary, dispatch_params);

            // Build let body: after calls + result reference
            var let_body = std.ArrayList(*Ir){};
            defer let_body.deinit(self.allocator);

            // Generate after calls at depth 0 - let doesn't create a new lambda scope
            // The params are still at depth=0, indices 0..n-1
            // Runtime typep checks for :after methods (least specific first)
            var k = after_methods.len;
            while (k > 0) {
                k -= 1;
                const after = after_methods[k];

                // Build runtime type check condition
                var cond: ?*Ir = null;
                for (after.specializers, 0..) |spec, param_idx| {
                    if (param_idx >= dispatch_params.len) continue;

                    const arg_ir = try self.builder.variable(dispatch_params[param_idx], 0, @intCast(param_idx));
                    const check = if (try self.buildMethodSpecializerCheck(spec, arg_ir)) |val|
                        val
                    else
                        continue;

                    cond = if (cond) |prev| blk: {
                        const nil_ir = try self.builder.lit(Value.nil);
                        break :blk try self.builder.ifExpr(prev, check, nil_ir);
                    } else check;
                }

                const after_call = try self.generateMethodCall(after, dispatch_params);
                if (cond) |c| {
                    const nil_ir = try self.builder.lit(Value.nil);
                    try let_body.append(self.allocator, try self.builder.ifExpr(c, after_call, nil_ir));
                } else {
                    try let_body.append(self.allocator, after_call);
                }
            }

            // Return the result variable
            // Let binding index is after all params: dispatch_params.len
            const result_index: u16 = @intCast(dispatch_params.len);
            const result_ref = try self.builder.variable(result_name, 0, result_index);
            try let_body.append(self.allocator, result_ref);

            const body_progn = try self.builder.progn(try let_body.toOwnedSlice(self.allocator));

            // Create let binding for result at index after params
            const let_ir = try self.builder.let1(result_name, result_index, primary_call, body_progn);

            try stmts.append(self.allocator, let_ir);
        } else {
            // No :after methods, just call primary
            try stmts.append(self.allocator, try self.generateMethodCall(primary, dispatch_params));
        }

        return try self.builder.progn(try stmts.toOwnedSlice(self.allocator));
    }

    /// Sort methods by specificity (most specific first)
    /// Uses CPL to determine which class is more specific
    fn sortMethodsBySpecificity(self: *Compiler, methods: []MethodDef) !void {
        if (methods.len <= 1) return;

        // Insertion sort - stable and works well for small arrays
        // We need manual sort because std.mem.sort doesn't support fallible comparators
        var i: usize = 1;
        while (i < methods.len) : (i += 1) {
            const key = methods[i];
            var j: usize = i;
            while (j > 0) {
                const cmp = try self.compareMethodSpecificity(key, methods[j - 1]);
                if (cmp != .more_specific) break;
                methods[j] = methods[j - 1];
                j -= 1;
            }
            methods[j] = key;
        }
    }

    const SpecificityOrder = enum { more_specific, less_specific, equal };

    /// Compare two methods for specificity
    /// Returns .more_specific if a is more specific than b
    fn compareMethodSpecificity(self: *Compiler, a: MethodDef, b: MethodDef) !SpecificityOrder {
        const min_len = @min(a.specializers.len, b.specializers.len);

        for (0..min_len) |i| {
            const a_spec = a.specializers[i];
            const b_spec = b.specializers[i];

            const a_is_t = a_spec.eq(Value.t);
            const b_is_t = b_spec.eq(Value.t);

            // Non-t is more specific than t
            if (!a_is_t and b_is_t) return .more_specific;
            if (a_is_t and !b_is_t) return .less_specific;

            // Both t - equal at this position
            if (a_is_t and b_is_t) continue;

            const a_is_eql = self.eqlSpecializerObject(a_spec) != null;
            const b_is_eql = self.eqlSpecializerObject(b_spec) != null;

            if (a_is_eql and b_is_eql) {
                if (a_spec.eq(b_spec)) continue;
                return .equal;
            }

            if (a_is_eql and !b_is_eql) return .more_specific;
            if (!a_is_eql and b_is_eql) return .less_specific;

            // Both non-t - check class hierarchy
            if (a_spec.eq(b_spec)) continue; // Same class

            // Check if a_spec is more specific (subclass of b_spec)
            const cmp = try self.compareClassSpecificity(a_spec, b_spec);
            if (cmp != .equal) return cmp;
        }

        // All compared positions equal - longer specializer list is more specific
        if (a.specializers.len > b.specializers.len) return .more_specific;
        if (a.specializers.len < b.specializers.len) return .less_specific;
        return .equal;
    }

    fn eqlSpecializerObject(self: *Compiler, spec: Value) ?Value {
        const builtins = self.builtins orelse return null;
        if (!spec.isCons()) return null;
        const cons = spec.toPtr(Cons);
        if (!cons.car.eq(builtins.ty_eql)) return null;
        if (!cons.cdr.isCons()) return null;
        const arg_cons = cons.cdr.toPtr(Cons);
        if (!arg_cons.cdr.isNil()) return null;
        return arg_cons.car;
    }

    fn buildMethodSpecializerCheck(self: *Compiler, spec: Value, arg_ir: *const Ir) anyerror!?*Ir {
        if (spec.eq(Value.t)) return null;
        if (self.eqlSpecializerObject(spec)) |eql_obj| {
            const eql_obj_ir = try self.builder.lit(eql_obj);
            return try self.builder.eql(arg_ir, eql_obj_ir);
        }
        const class_ir = try self.builder.lit(spec);
        return try self.builder.typep(arg_ir, class_ir);
    }

    /// Compare two class specializers using CPL
    /// Returns .more_specific if class_a is a subclass of class_b
    fn compareClassSpecificity(self: *Compiler, class_a: Value, class_b: Value) !SpecificityOrder {
        const heap = if (self.heap) |val| val else return .equal;

        // Look up both classes
        const a_class = if (self.lookupClass(heap, class_a)) |val| val else return .equal;
        const b_class = if (self.lookupClass(heap, class_b)) |val| val else return .equal;

        // Check if class_b appears in class_a's CPL (meaning a is a subclass of b)
        if (self.classInCpl(a_class, class_b)) return .more_specific;
        // Check if class_a appears in class_b's CPL (meaning b is a subclass of a)
        if (self.classInCpl(b_class, class_a)) return .less_specific;

        return .equal;
    }

    /// Look up a class by name in the class registry
    fn lookupClass(self: *Compiler, heap: *Heap, class_name: Value) ?*runtime.Class {
        _ = self;
        const val = heap.findLispClass(class_name) orelse return null;
        return if (val.isClass()) val.toPtr(runtime.Class) else null;
    }

    /// Check if target_class (a class name symbol) appears in class's CPL
    fn classInCpl(self: *Compiler, class: *runtime.Class, target_class: Value) bool {
        _ = self;
        var cpl = class.cpl;
        while (cpl.isCons()) {
            const cons = cpl.toPtr(Cons);
            const car = cons.car;
            // CPL contains Class objects, compare their names with target_class
            if (car.isClass()) {
                const c = car.toPtr(runtime.Class);
                if (c.name.eq(target_class)) return true;
            }
            cpl = cons.cdr;
        }
        return false;
    }

    /// Check if method specializers are compatible (aux method applies to primary)
    /// For now: aux method applies if its specializers are same or more general
    fn specializerMatches(self: *Compiler, aux_specs: []const Value, primary_specs: []const Value) !bool {
        _ = self;
        if (aux_specs.len != primary_specs.len) return false;
        // Symbol identity comparison via Value.eq()
        for (aux_specs, primary_specs) |aux, prim| {
            // t (any type) matches everything
            if (aux.eq(Value.t)) continue;
            // Otherwise must be same specializer
            if (!aux.eq(prim)) return false;
        }
        return true;
    }

    /// Generate a call to a method by function name with given parameters
    fn generateMethodCallByName(
        self: *Compiler,
        function_name: []const u8,
        dispatch_params: []const []const u8,
    ) anyerror!*Ir {
        return try self.generateMethodCallByNameAtDepth(function_name, dispatch_params, 0);
    }

    fn generateMethodCall(
        self: *Compiler,
        method: MethodDef,
        dispatch_params: []const []const u8,
    ) anyerror!*Ir {
        const arity = method.specializers.len;
        const params = dispatch_params[0..arity];
        return try self.generateMethodCallByName(method.function_name, params);
    }

    fn generateMethodCallByNameAtDepth(
        self: *Compiler,
        function_name: []const u8,
        dispatch_params: []const []const u8,
        depth: u8,
    ) anyerror!*Ir {
        // Build argument list: pass all parameters
        var args = std.ArrayList(*const Ir){};
        defer args.deinit(self.allocator);

        for (dispatch_params, 0..) |param, idx| {
            const arg_ir = try self.builder.variable(param, depth, @intCast(idx));
            try args.append(self.allocator, arg_ir);
        }

        // Look up the method function by name (direct global lookup, not symbol interning)
        // Method function names are synthetic (e.g., "foo$p$fixnum") and don't use package qualification
        const func_idx = if (self.globals.lookup(function_name)) |val| val else return error.UnboundMethodFunction;
        const func_val_ir = try self.builder.globalRef(function_name, func_idx);

        // Call the method function
        return try self.builder.call(func_val_ir, try args.toOwnedSlice(self.allocator));
    }

    /// Compile define-method-combination: stub for custom method combinations
    /// (define-method-combination name &optional operator identity)
    /// Short form: defines simple list-combining method combination
    fn compileDefineMethodCombination(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        _ = env;

        // Parse: (name &optional operator identity)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const name_val = cons1.car;

        if (!name_val.isSymbol()) return error.InvalidSyntax;

        // For now, just return the name (stub implementation)
        // Full implementation would:
        // 1. Parse operator and identity-with-one-arg
        // 2. Store combination type definition
        // 3. Apply to generic functions via :method-combination

        return try self.builder.lit(name_val);
    }

    /// Compile method-qualifiers: (method-qualifiers method)
    fn compileMethodQualifiers(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileUnaryPrim(args, env, .method_qualifiers);
    }

    /// Compile method-specializers: (method-specializers method)
    fn compileMethodSpecializers(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileUnaryPrim(args, env, .method_specializers);
    }

    /// Compile method-function: (method-function method)
    fn compileMethodFunction(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileUnaryPrim(args, env, .method_function);
    }

    /// Compile generic-function-methods: (generic-function-methods gf)
    fn compileGenericFunctionMethods(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileUnaryPrim(args, env, .generic_function_methods);
    }

    /// Compile generic-function-lambda-list: (generic-function-lambda-list gf)
    fn compileGenericFunctionLambdaList(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileUnaryPrim(args, env, .generic_function_lambda_list);
    }

    /// Compile generic-function-name: (generic-function-name gf)
    fn compileGenericFunctionName(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return self.compileUnaryPrim(args, env, .generic_function_name);
    }

    // ========================================================================
    // ADT Support: deftype and match
    // ========================================================================

    /// Compile deftype: CL-style type abbreviation
    /// (deftype name lambda-list body...) - defines type specifier expansion
    /// Stores (lambda-list . body) in type_aliases registry
    fn compileDeftype(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        _ = env;
        // Parse: (name lambda-list body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const type_name_val = cons1.car;
        if (!type_name_val.isSymbol()) return error.InvalidSyntax;
        const type_name_raw = type_name_val.toPtr(Symbol).getName();
        const type_name = try self.globals.allocator.dupe(u8, type_name_raw);

        // Rest is (lambda-list body...) - store for later expansion
        const lambda_args = cons1.cdr;
        if (!lambda_args.isCons()) return error.InvalidSyntax;

        // Store in type_aliases: name -> (lambda-list body...)
        try self.type_aliases.put(type_name, lambda_args);

        // deftype has no runtime effect - return nil
        return try self.builder.lit(Value.nil);
    }

    /// Get type alias expansion function for a type name
    /// Returns (lambda-list body...) or null if no alias defined
    pub fn getTypeAlias(self: *const Compiler, type_name: []const u8) ?Value {
        return self.type_aliases.get(type_name);
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
            .sym = cons1.car,
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
            .allow_other_keys = false,
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
            .allow_other_keys = false,
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
            .allow_other_keys = false,
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
            .allow_other_keys = false,
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
        _ = try self.checkMatchExhaustiveness(clauses);

        return self.compileMatchClauses(scrutinee, clauses, env);
    }

    /// Check if match covers all variants of the ADT (warning only, doesn't fail)
    fn checkMatchExhaustiveness(self: *Compiler, clauses: Value) !?usize {
        var has_wildcard = false;
        var covered = std.AutoHashMap(Value, void).init(self.allocator);
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
                const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
                if (pattern.raw == b._.raw) {
                    has_wildcard = true;
                    break;
                }
            }

            // Extract variant name from pattern (variant-name field1 field2 ...)
            if (pattern.isCons()) {
                const variant_cons = pattern.toPtr(Cons);
                if (variant_cons.car.isSymbol()) {
                    try covered.put(variant_cons.car, {});
                }
            }

            current = clause_cons.cdr;
        }

        if (has_wildcard) return null; // Wildcard covers everything

        // Find the ADT type from the first variant
        var type_variants: ?[]const Variant = null;
        var iter = covered.keyIterator();
        while (iter.next()) |variant_sym| {
            // Search all defined types for this variant
            var type_iter = self.defined_types.iterator();
            while (type_iter.next()) |entry| {
                for (entry.value_ptr.*) |v| {
                    if (v.sym.eq(variant_sym.*)) {
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
                if (!covered.contains(v.sym)) {
                    std.log.warn("match: missing case for variant '{s}'", .{v.name});
                }
            }
            return variants.len;
        }
        return null;
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
            const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
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

        const Field = struct {
            sym: Value,
            name: []const u8,
        };
        // Collect field bindings
        var fields = std.ArrayList(Field){};
        defer fields.deinit(self.allocator);
        var field_current = variant_cons.cdr;
        while (field_current.isCons()) {
            const fc = field_current.toPtr(Cons);
            if (!fc.car.isSymbol()) return error.InvalidSyntax;
            const field_sym = fc.car;
            const field_name = try self.allocator.dupe(u8, field_sym.toPtr(Symbol).getName());
            try fields.append(self.allocator, .{ .sym = field_sym, .name = field_name });
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
        var bindings = try self.allocator.alloc(Ir.Binding, fields.items.len);
        for (fields.items, 0..) |field, i| {
            const idx_lit = try self.builder.lit(Value.makeFixnum(@intCast(i + 1)));
            const field_aref = try self.allocator.create(Ir);
            field_aref.* = .{ .vec_ref = .{ .left = scrutinee, .right = idx_lit } };

            const binding_idx = try let_env.bindSym(field.sym);
            bindings[i] = .{
                .name = field.name,
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
        // Always generate assert_* nodes for specialization, even at safety=0.
        // The emitter will skip runtime check_* bytecodes when safety=0.

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

    fn compileDeclare(self: *Compiler, args: Value) !*Ir {
        // (declare (spec var...) (spec2 var2...))
        // Process each declaration spec
        var list = args;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const decl_spec = cons.car;

            // Each spec should be a list like (type fixnum x y)
            if (decl_spec.isCons()) {
                try self.processDeclSpec(decl_spec, null);
            }

            list = cons.cdr;
        }

        // Declarations are compile-time only, return nil
        return self.builder.lit(Value.nil);
    }

    fn processDeclSpec(self: *Compiler, spec: Value, env: ?*Env) !void {
        const spec_cons = spec.toPtr(Cons);
        const spec_name = spec_cons.car;
        const spec_args = spec_cons.cdr;

        if (!spec_name.isSymbol()) return error.InvalidSyntax;

        const heap = if (self.heap) |val| val else return error.InvalidSyntax;

        // Use global_decls instead of per-scope decl_env
        // Match declaration spec by symbol identity
        const type_sym = try heap.intern("type");
        const ftype_sym = try heap.intern("ftype");
        const inline_sym = try heap.intern("inline");
        const notinline_sym = try heap.intern("notinline");
        const ignore_sym = try heap.intern("ignore");
        const ignorable_sym = try heap.intern("ignorable");
        const special_sym = try heap.intern("special");
        const dynamic_extent_sym = try heap.intern("dynamic-extent");
        const declaration_sym = try heap.intern("declaration");
        const optimize_sym = try heap.intern("optimize");

        if (spec_name.eq(type_sym)) {
            // (type type-spec var1 var2 ...)
            if (!spec_args.isCons()) return error.InvalidSyntax;
            const type_cons = spec_args.toPtr(Cons);
            const type_expr = type_cons.car;
            var var_list = type_cons.cdr;

            while (var_list.isCons()) {
                const var_cons = var_list.toPtr(Cons);
                const var_name_val = var_cons.car;
                if (!var_name_val.isSymbol()) return error.InvalidSyntax;

                const var_sym = var_name_val.toPtr(Symbol);
                const var_name = var_sym.getName();

                try self.global_decls.addDecl(var_name, .{
                    .spec = .type_decl,
                    .type_expr = type_expr,
                });

                var_list = var_cons.cdr;
            }
        } else if (spec_name.eq(ftype_sym)) {
            // (ftype function-type fname1 fname2 ...)
            if (!spec_args.isCons()) return error.InvalidSyntax;
            const ftype_cons = spec_args.toPtr(Cons);
            const ftype_expr = ftype_cons.car;
            var fn_list = ftype_cons.cdr;

            while (fn_list.isCons()) {
                const fn_cons = fn_list.toPtr(Cons);
                const fn_name_val = fn_cons.car;
                if (!fn_name_val.isSymbol()) return error.InvalidSyntax;

                const fn_sym = fn_name_val.toPtr(Symbol);
                const fn_name = fn_sym.getName();

                try self.global_decls.addDecl(fn_name, .{
                    .spec = .ftype,
                    .type_expr = ftype_expr,
                });

                fn_list = fn_cons.cdr;
            }
        } else if (spec_name.eq(inline_sym)) {
            // (inline fname1 fname2 ...)
            try self.addSimpleDecls(spec_args, .inline_decl);
        } else if (spec_name.eq(notinline_sym)) {
            // (notinline fname1 fname2 ...)
            try self.addSimpleDecls(spec_args, .notinline);
        } else if (spec_name.eq(ignore_sym)) {
            // (ignore var1 var2 ...)
            try self.addSimpleDecls(spec_args, .ignore);
        } else if (spec_name.eq(ignorable_sym)) {
            // (ignorable var1 var2 ...)
            try self.addSimpleDecls(spec_args, .ignorable);
        } else if (spec_name.eq(special_sym)) {
            // (special var1 var2 ...)
            try self.addSimpleDecls(spec_args, .special);
        } else if (spec_name.eq(dynamic_extent_sym)) {
            // (dynamic-extent var1 var2 ...)
            try self.addSimpleDecls(spec_args, .dynamic_extent);
        } else if (spec_name.eq(declaration_sym)) {
            // (declaration name1 name2 ...)
            try self.addSimpleDecls(spec_args, .declaration);
        } else if (spec_name.eq(optimize_sym)) {
            // (optimize (quality value)...) or (optimize quality)
            var updated = self.optimize_current;
            try self.parseOptimizeQualities(spec_args, &updated);
            self.optimize_current = updated;
            if (env) |scope_env| scope_env.optimize = updated;
        }
        // Ignore unknown declaration specs
    }

    fn addSimpleDecls(self: *Compiler, vars: Value, spec: DeclSpec) !void {
        var list = vars;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const var_val = cons.car;
            if (!var_val.isSymbol()) return error.InvalidSyntax;

            const var_sym = var_val.toPtr(Symbol);
            const var_name = var_sym.getName();

            try self.global_decls.addDecl(var_name, .{ .spec = spec });

            list = cons.cdr;
        }
    }

    fn compileDeclaim(self: *Compiler, args: Value) !*Ir {
        // (declaim (spec var...) (spec2 var2...))
        // Process each declaration spec into global declarations
        var list = args;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const decl_spec = cons.car;

            // Each spec should be a list like (inline func-name)
            if (decl_spec.isCons()) {
                try self.processGlobalDeclSpec(decl_spec);
            }

            list = cons.cdr;
        }

        // Declarations are compile-time only, return nil
        return self.builder.lit(Value.nil);
    }

    fn processGlobalDeclSpec(self: *Compiler, spec: Value) !void {
        const spec_cons = spec.toPtr(Cons);
        const spec_name = spec_cons.car;
        const spec_args = spec_cons.cdr;

        if (!spec_name.isSymbol()) return error.InvalidSyntax;

        const heap = if (self.heap) |val| val else return error.InvalidSyntax;

        // Match declaration spec by symbol identity
        const type_sym = try heap.intern("type");
        const ftype_sym = try heap.intern("ftype");
        const inline_sym = try heap.intern("inline");
        const notinline_sym = try heap.intern("notinline");
        const special_sym = try heap.intern("special");
        const declaration_sym = try heap.intern("declaration");
        const optimize_sym = try heap.intern("optimize");

        if (spec_name.eq(type_sym)) {
            // (type type-spec var1 var2 ...)
            if (!spec_args.isCons()) return error.InvalidSyntax;
            const type_cons = spec_args.toPtr(Cons);
            const type_expr = type_cons.car;
            var var_list = type_cons.cdr;

            while (var_list.isCons()) {
                const var_cons = var_list.toPtr(Cons);
                const var_name_val = var_cons.car;
                if (!var_name_val.isSymbol()) return error.InvalidSyntax;

                const var_sym = var_name_val.toPtr(Symbol);
                const var_name = var_sym.getName();

                try self.global_decls.addDecl(var_name, .{
                    .spec = .type_decl,
                    .type_expr = type_expr,
                });

                var_list = var_cons.cdr;
            }
        } else if (spec_name.eq(ftype_sym)) {
            // (ftype function-type fname1 fname2 ...)
            if (!spec_args.isCons()) return error.InvalidSyntax;
            const ftype_cons = spec_args.toPtr(Cons);
            const ftype_expr = ftype_cons.car;
            var fn_list = ftype_cons.cdr;

            while (fn_list.isCons()) {
                const fn_cons = fn_list.toPtr(Cons);
                const fn_name_val = fn_cons.car;
                const parsed_name = try self.parseDeclName(fn_name_val, true);
                defer if (parsed_name.owned) |mem| self.allocator.free(mem);

                try self.global_decls.addDecl(parsed_name.name, .{
                    .spec = .ftype,
                    .type_expr = ftype_expr,
                });

                fn_list = fn_cons.cdr;
            }
        } else if (spec_name.eq(inline_sym)) {
            // (inline fname1 fname2 ...)
            try self.addGlobalSimpleDecls(spec_args, .inline_decl);
        } else if (spec_name.eq(notinline_sym)) {
            // (notinline fname1 fname2 ...)
            try self.addGlobalSimpleDecls(spec_args, .notinline);
        } else if (spec_name.eq(special_sym)) {
            // (special var1 var2 ...)
            try self.addGlobalSimpleDecls(spec_args, .special);
        } else if (spec_name.eq(declaration_sym)) {
            // (declaration name1 name2 ...)
            try self.addGlobalSimpleDecls(spec_args, .declaration);
        } else if (spec_name.eq(optimize_sym)) {
            // (optimize (quality value)...) or (optimize quality)
            var updated = self.optimize_global;
            try self.parseOptimizeQualities(spec_args, &updated);
            self.optimize_global = updated;
            self.optimize_current = updated;
        }
        // Ignore unknown declaration specs
    }

    fn addGlobalSimpleDecls(self: *Compiler, vars: Value, spec: DeclSpec) !void {
        var list = vars;
        const allow_setf_name = spec == .inline_decl or spec == .notinline;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const var_val = cons.car;
            const parsed_name = try self.parseDeclName(var_val, allow_setf_name);
            defer if (parsed_name.owned) |mem| self.allocator.free(mem);

            try self.global_decls.addDecl(parsed_name.name, .{ .spec = spec });

            list = cons.cdr;
        }
    }

    fn parseDeclName(self: *Compiler, form: Value, allow_setf_name: bool) !struct { name: []const u8, owned: ?[]u8 } {
        if (form.isSymbol()) {
            return .{ .name = form.toPtr(Symbol).getName(), .owned = null };
        }

        if (allow_setf_name and form.isCons()) {
            const outer = form.toPtr(Cons);
            const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
            if (self.canonicalBuiltinSymbol(outer.car).raw == b.setf.raw) {
                if (!outer.cdr.isCons()) return error.InvalidSyntax;
                const inner = outer.cdr.toPtr(Cons);
                if (!inner.car.isSymbol() or !inner.cdr.isNil()) return error.InvalidSyntax;
                const base_name = inner.car.toPtr(Symbol).getName();
                const setf_name = try std.fmt.allocPrint(self.allocator, "(SETF {s})", .{base_name});
                return .{ .name = setf_name, .owned = setf_name };
            }
        }

        return error.InvalidSyntax;
    }

    fn parseOptimizeLevel(level_val: Value) !u8 {
        if (!level_val.isFixnum()) return error.InvalidSyntax;
        const raw = level_val.toFixnum();
        if (raw <= 0) return 0;
        if (raw >= 3) return 3;
        return @intCast(raw);
    }

    fn parseOptimizeQualities(self: *Compiler, specs: Value, target: *OptimizeSettings) !void {
        const heap = if (self.heap) |val| val else return error.InvalidSyntax;
        const speed_sym = try heap.intern("speed");
        const safety_sym = try heap.intern("safety");
        const debug_sym = try heap.intern("debug");
        const space_sym = try heap.intern("space");
        const comp_speed_sym = try heap.intern("compilation-speed");

        var list = specs;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const spec = cons.car;

            var quality = Value.nil;
            var level: u8 = 3; // Bare quality implies max preference.

            if (spec.isSymbol()) {
                quality = spec;
            } else if (spec.isCons()) {
                const qcons = spec.toPtr(Cons);
                quality = qcons.car;
                if (!quality.isSymbol()) return error.InvalidSyntax;
                if (!qcons.cdr.isCons()) return error.InvalidSyntax;
                level = try parseOptimizeLevel(qcons.cdr.toPtr(Cons).car);
            } else {
                return error.InvalidSyntax;
            }

            if (quality.eq(speed_sym)) {
                target.speed = level;
            } else if (quality.eq(safety_sym)) {
                target.safety = level;
            } else if (quality.eq(debug_sym)) {
                target.debug = level;
            } else if (quality.eq(space_sym)) {
                target.space = level;
            } else if (quality.eq(comp_speed_sym)) {
                target.compilation_speed = level;
            }

            list = cons.cdr;
        }
    }

    fn typeChecksEnabled(self: *const Compiler) bool {
        return self.optimize_current.safety > 0;
    }

    fn isDefaultOptimize(opt: OptimizeSettings) bool {
        return opt.speed == 1 and opt.safety == 1 and opt.debug == 1 and opt.space == 1 and opt.compilation_speed == 1;
    }

    fn effectiveOptimizeForEnv(self: *const Compiler, env: *const Env) OptimizeSettings {
        if (env.parent == null and isDefaultOptimize(env.optimize)) {
            return self.optimize_global;
        }
        return env.optimize;
    }

    fn compileProclaim(self: *Compiler, args: Value, _: *const Env) !*Ir {
        // (proclaim '(spec var...))
        // Runtime global declaration - compile as call to proclaim primitive
        // For now, just process at compile time
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const quoted = cons.car;

        // Unwrap quote if present
        const decl_spec = if (quoted.isCons()) blk: {
            const q = quoted.toPtr(Cons);
            const heap = if (self.heap) |val| val else return error.InvalidSyntax;
            const quote_sym = try heap.intern("quote");
            if (q.car.eq(quote_sym) and q.cdr.isCons()) {
                break :blk q.cdr.toPtr(Cons).car;
            }
            break :blk quoted;
        } else quoted;

        if (decl_spec.isCons()) {
            try self.processGlobalDeclSpec(decl_spec);
        }

        // Return nil for now (in future, could emit runtime call)
        return self.builder.lit(Value.nil);
    }

    /// Compile a simple type check for a single type symbol (uses symbol identity)
    fn compileSimpleTypeCheckSym(self: *Compiler, type_sym: Value, expr_ir: *const Ir) anyerror!*Ir {
        const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
        // Dispatch by symbol identity (no string comparison)
        if (type_sym.raw == b.ty_fixnum.raw or type_sym.raw == b.ty_integer.raw) return self.builder.assertFixnum(expr_ir);
        if (type_sym.raw == b.cons.raw) return self.builder.assertCons(expr_ir);
        if (type_sym.raw == b.ty_symbol.raw) return self.builder.assertSymbol(expr_ir);
        if (type_sym.raw == b.string.raw) return self.builder.assertString(expr_ir);
        if (type_sym.raw == b.ty_vector.raw) return self.builder.assertVector(expr_ir);
        if (type_sym.raw == b.ty_closure.raw or type_sym.raw == b.ty_function.raw or type_sym.raw == b.function.raw) return self.builder.assertClosure(expr_ir);
        if (type_sym.raw == b.@"ty_non-nil".raw) return self.builder.assertNonNil(expr_ir);
        if (type_sym.raw == b.ty_list.raw or type_sym.raw == b.list.raw) return self.builder.assertList(expr_ir);
        if (type_sym.raw == b.ty_nil.raw or type_sym.raw == b.null.raw) {
            const syms = try self.allocator.alloc(Value, 1);
            syms[0] = b.ty_nil;
            return self.builder.assertOr(expr_ir, syms);
        }
        if (type_sym.raw == b.ty_any.raw or type_sym.raw == b.ty_t.raw) return @constCast(expr_ir); // no check
        // Defer unsupported type symbols to runtime semantics instead of failing compile-time.
        return @constCast(expr_ir);
    }

    /// Compile a compound type check: (or type1 type2 ...), (refine T x P), etc.
    fn compileCompoundTypeCheck(self: *Compiler, type_spec: Value, expr_ir: *const Ir) anyerror!*Ir {
        const cons = type_spec.toPtr(Cons);
        if (!cons.car.isSymbol()) return @constCast(expr_ir);

        const b = if (self.builtins) |val| val else return error.UninitializedBuiltins;
        const head = self.canonicalBuiltinSymbol(cons.car);

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
        if (head.raw == b.ty_fixnum.raw or head.raw == b.ty_integer.raw or head.raw == b.ty_symbol.raw or head.raw == b.string.raw or head.raw == b.ty_vector.raw or head.raw == b.ty_closure.raw or head.raw == b.ty_function.raw or head.raw == b.function.raw or head.raw == b.@"ty_non-nil".raw or head.raw == b.ty_list.raw or head.raw == b.list.raw or head.raw == b.ty_nil.raw or head.raw == b.null.raw or head.raw == b.ty_any.raw or head.raw == b.ty_t.raw or head.raw == b.ty_float.raw or head.raw == b.ty_char.raw or head.raw == b.ty_character.raw or head.raw == b.ty_keyword.raw) {
            // For shapes like (integer lo hi), (float low high), etc., enforce only the head check.
            return self.compileSimpleTypeCheckSym(head, expr_ir);
        }

        // Unknown compound type: keep runtime behavior unchanged and do not fail compilation.
        return @constCast(expr_ir);
    }

    /// Compile (or type1 type2 ...) check
    /// Expands to: check_list if (or cons nil), else check each type
    fn compileOrTypeCheck(self: *Compiler, type_list: Value, expr_ir: *const Ir) anyerror!*Ir {
        const b = if (self.builtins) |val| val else return error.InvalidSyntax;

        // Collect type symbols (symbol Values or nil for "nil" type).
        // Nested type specs in OR are reduced to their head symbol when possible.
        var type_syms = std.ArrayList(Value){};
        defer type_syms.deinit(self.allocator);

        var list = type_list;
        while (list.isCons()) {
            const c = list.toPtr(Cons);
            switch (c.car.typeKind()) {
                .symbol => try type_syms.append(self.allocator, c.car),
                .nil => {
                    // nil value in type position means the nil type symbol
                    try type_syms.append(self.allocator, b.ty_nil);
                },
                .cons => {
                    const nested = c.car.toPtr(Cons);
                    if (!nested.car.isSymbol()) return @constCast(expr_ir);
                    const nested_head = self.canonicalBuiltinSymbol(nested.car);
                    if (nested_head.raw == b.ty_fixnum.raw or nested_head.raw == b.ty_integer.raw or nested_head.raw == b.ty_symbol.raw or nested_head.raw == b.string.raw or nested_head.raw == b.ty_vector.raw or nested_head.raw == b.ty_closure.raw or nested_head.raw == b.ty_function.raw or nested_head.raw == b.function.raw or nested_head.raw == b.@"ty_non-nil".raw or nested_head.raw == b.ty_list.raw or nested_head.raw == b.list.raw or nested_head.raw == b.ty_nil.raw or nested_head.raw == b.null.raw or nested_head.raw == b.ty_any.raw or nested_head.raw == b.ty_t.raw or nested_head.raw == b.ty_float.raw or nested_head.raw == b.ty_char.raw or nested_head.raw == b.ty_character.raw or nested_head.raw == b.ty_keyword.raw) {
                        try type_syms.append(self.allocator, nested_head);
                    } else {
                        return @constCast(expr_ir);
                    }
                },
                else => return @constCast(expr_ir),
            }
            list = c.cdr;
        }

        if (type_syms.items.len == 0) return @constCast(expr_ir);

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
        _ = try pred_env.bindSym(c2.car);

        const predicate_body = try self.compile(predicate_expr, &pred_env);
        const dispatch_params = try self.allocator.alloc([]const u8, 1);
        dispatch_params[0] = try self.allocator.dupe(u8, var_name);
        const empty_opt = try self.allocator.alloc(Ir.OptionalParam, 0);
        const empty_key = try self.allocator.alloc(Ir.KeyParam, 0);
        const empty_cap = try self.allocator.alloc(Ir.Capture, 0);

        const predicate_lambda = try self.builder.lambda(
            dispatch_params,
            empty_opt,
            empty_key,
            false,
            null,
            empty_cap,
            predicate_body,
        );

        // Parse base type for type info (optional)
        const base_type = try self.parseTypeExpr(base_type_spec);

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
        const b = if (self.builtins) |val| val else return false;

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
        const items = try self.allocator.dupe(*const Ir, expr_list.items);
        const result = try self.builder.progn(items);
        // DEBUG: verify progn was created with all items
        std.debug.assert(std.meta.activeTag(result.*) == .progn);
        std.debug.assert(result.progn.len == count);
        return result;
    }

    fn filterDeclares(self: *Compiler, exprs: Value, env: *Env) !Value {
        const heap = if (self.heap) |val| val else return exprs;
        const declare_sym = try heap.intern("declare");

        var filtered = std.ArrayList(Value){};
        defer filtered.deinit(self.allocator);

        var list = exprs;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const expr = cons.car;

            var is_declare = false;
            if (expr.isCons()) {
                const expr_cons = expr.toPtr(Cons);
                if (expr_cons.car.eq(declare_sym)) {
                    is_declare = true;
                    try self.processDeclareList(expr_cons.cdr, env);
                }
            }

            if (!is_declare) {
                try filtered.append(self.allocator, expr);
            }

            list = cons.cdr;
        }

        if (filtered.items.len == 0) {
            return Value.nil;
        }

        var result = Value.nil;
        var i = filtered.items.len;
        while (i > 0) {
            i -= 1;
            const pair = try heap.alloc(Cons);
            pair.car = filtered.items[i];
            pair.cdr = result;
            result = Value.makeCons(pair);
        }
        return result;
    }

    fn processDeclareList(self: *Compiler, decl_list: Value, env: *Env) !void {
        var list = decl_list;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const decl_spec = cons.car;
            if (decl_spec.isCons()) {
                try self.processDeclSpec(decl_spec, env);
            }
            list = cons.cdr;
        }
    }

    /// Primitive operation tags for dispatch
    const PrimTag = enum {
        // Arithmetic
        add,
        sub,
        mul,
        div,
        mod,
        quot,
        rem,
        // Comparison
        eq,
        equal,
        eql,
        equalp,
        lt,
        gt,
        le,
        ge,
        num_eq,
        // List
        cons,
        car,
        cdr,
        append,
        length,
        reverse,
        nth,
        nthcdr,
        last,
        member,
        assoc,
        rplaca,
        rplacd,
        // Type predicates
        consp,
        symbolp,
        numberp,
        integerp,
        realp,
        stringp,
        vectorp,
        closurep,
        keywordp,
        nilp,
        not,
        characterp,
        floatp,
        listp,
        atom,
        // CLOS introspection
        method_qualifiers,
        method_specializers,
        method_function,
        generic_function_methods,
        generic_function_lambda_list,
        generic_function_name,
        // Vector
        vec_ref,
        vec_len,
        vec_fill_ptr,
        vec_set_fill_ptr,
        vec_set_adjustable,
        vec_push,
        vec_push_ext,
        vec_pop,
        vec_adjust,
        copy_structure,
        // Box
        make_box,
        box_ref,
        box_set,
        // String
        str_ref,
        str_len,
        str_eq,
        str_lt,
        str_gt,
        str_le,
        str_ge,
        str_concat,
        // I/O
        write,
        print,
        princ,
        terpri,
        write_char,
        random,
        random_seed,
        // Symbol
        intern,
        make_symbol,
        unintern,
        sym_name,
        copy_symbol,
        makunbound,
        set_sym_val,
        type_of,
        error_user,
        boundp,
        fboundp,
        symbol_value,
        symbol_function,
        set_symbol_function,
        symbol_plist,
        set_symbol_plist,
        function_lambda_expression,
        typep,
        subtypep,
        // Character
        char_code,
        code_char,
        char_eq,
        char_lt,
        char_gt,
        char_upcase,
        char_downcase,
        digit_char_p,
        alpha_char_p,
        read_char,
        peek_char,
        unread_char,
        listen,
        upgraded_complex_part_type,
        // Read/eval
        read,
        read_from_string,
        load,
        eval,
        gensym,
        macroexpand,
        macroexpand_1,
        // String/number conversion
        parse_integer,
        write_to_string,
        // Bitwise
        logand,
        logior,
        logxor,
        lognot,
        ash,
        lognand,
        lognor,
        logandc1,
        logandc2,
        logorc1,
        logorc2,
        logeqv,
        logtest,
        logbitp,
        logcount,
        integer_length,
        // File I/O
        read_file,
        write_file,
        delete_file,
        rename_file,
        probe_file,
        file_write_date,
        file_author,
        file_string_length,
        // Time
        get_universal_time,
        get_internal_real_time,
        get_internal_run_time,
        get_decoded_time,
        decode_universal_time,
        encode_universal_time,
        // Environment
        room,
        lisp_implementation_type,
        lisp_implementation_version,
        software_type,
        machine_type,
        machine_instance,
        machine_version,
        software_version,
        short_site_name,
        long_site_name,
        user_homedir_pathname,
        make_pathname,
        // String construction
        make_string,
        list_to_string,
        string_upcase,
        string_downcase,
        // Numeric
        abs,
        zerop,
        plusp,
        minusp,
        evenp,
        oddp,
        // Math
        sqrt,
        sin,
        cos,
        tan,
        asin,
        acos,
        atan,
        atan2,
        sinh,
        cosh,
        tanh,
        asinh,
        acosh,
        atanh,
        exp,
        log,
        floor,
        ceiling,
        round,
        decode_float,
        integer_decode_float,
        float_radix,
        float_digits,
        // Numeric types
        rationalp,
        complexp,
        make_complex,
        real_part,
        imag_part,
        numerator,
        denominator,
        rational,
        rationalize,
        // Properties
        get,
        put,
        remprop,
        get_macro_character,
        set_dispatch_macro_character,
        get_dispatch_macro_character,
        // Hash tables
        hashtablep,
        hash_clear,
        hash_test,
        hash_keys,
        hash_alist,
        sxhash,
        // Streams
        streamp,
        input_stream_p,
        output_stream_p,
        open_stream_p,
        interactive_stream_p,
        stream_element_type,
        stream_external_format,
        make_string_input_stream,
        make_string_output_stream,
        get_output_stream_string,
        write_to_stream,
        // Compound streams
        broadcast_stream_streams,
        concatenated_stream_streams,
        echo_stream_input_stream,
        echo_stream_output_stream,
        synonym_stream_symbol,
        two_way_stream_input_stream,
        two_way_stream_output_stream,
        make_synonym_stream,
        make_echo_stream,
        make_two_way_stream,
        make_broadcast_stream_list,
        make_concatenated_stream_list,
        disassemble,
        read_char_stream,
        peek_char_stream,
        open_file,
        close_stream,
        // Pathnames
        pathname_host,
        pathname_device,
        pathname_directory,
        pathname_name,
        pathname_type,
        pathname_version,
        truename,
        ensure_directories_exist,
        pathname,
        parse_namestring,
        namestring,
        merge_pathnames,
        directory_namestring,
        file_namestring,
        host_namestring,
        wild_pathname_p,
        // Packages
        package_symbols_table,
        package_exports_table,
        package_symbols_list,
        package_exports_list,
        package_name,
        package_nicknames,
        package_use_list,
        package_used_by_list,
        package_shadowing_symbols,
        packagep,
        symbol_package,
        list_all_packages,
        find_package,
        delete_package,
        pkg_import,
        pkg_use_package,
        pkg_unexport,
        pkg_shadow,
        pkg_shadowing_import,
        pkg_unuse_package,
        pkg_unintern,
        pkg_find_symbol,
        pkg_find_all_symbols,
        pkg_make_package,
        pkg_rename_package,
        apropos_list,
        read_char_no_hang,
        compute_restarts,
        restart_name,
        directory,
        pathname_match_p,
        enough_namestring,
        // Class/slot introspection
        find_symbol,
        find_class,
        class_name,
        class_direct_superclasses,
        class_precedence_list,
        class_direct_slots,
        class_slots,
        slot_definition_name,
        slot_definition_initform,
        slot_definition_initargs,
        slot_definition_readers,
        slot_definition_writers,
        slot_definition_allocation,
        slot_definition_type,
        // Misc
        sleep,
    };

    pub const PrimitiveRefArity = enum {
        nullary,
        unary,
        binary,
        ternary,
    };

    /// Dispatch entry for simple unary primitives
    const UnaryEntry = struct { field: []const u8, tag: PrimTag };
    const unary_dispatch = [_]UnaryEntry{
        .{ .field = "car", .tag = .car },
        .{ .field = "first", .tag = .car },
        .{ .field = "cdr", .tag = .cdr },
        .{ .field = "rest", .tag = .cdr },
        .{ .field = "length", .tag = .length },
        .{ .field = "reverse", .tag = .reverse },
        .{ .field = "last", .tag = .last },
        .{ .field = "consp", .tag = .consp },
        .{ .field = "symbolp", .tag = .symbolp },
        .{ .field = "numberp", .tag = .numberp },
        .{ .field = "integerp", .tag = .integerp },
        .{ .field = "realp", .tag = .realp },
        .{ .field = "stringp", .tag = .stringp },
        .{ .field = "vectorp", .tag = .vectorp },
        .{ .field = "closurep", .tag = .closurep },
        .{ .field = "keywordp", .tag = .keywordp },
        .{ .field = "null", .tag = .nilp },
        .{ .field = "not", .tag = .not },
        .{ .field = "characterp", .tag = .characterp },
        .{ .field = "floatp", .tag = .floatp },
        .{ .field = "listp", .tag = .listp },
        .{ .field = "atom", .tag = .atom },
        .{ .field = "char-code", .tag = .char_code },
        .{ .field = "code-char", .tag = .code_char },
        .{ .field = "unread-char", .tag = .unread_char },
        .{ .field = "listen", .tag = .listen },
        .{ .field = "upgraded-complex-part-type", .tag = .upgraded_complex_part_type },
        .{ .field = "read-from-string", .tag = .read_from_string },
        .{ .field = "load", .tag = .load },
        .{ .field = "eval", .tag = .eval },
        .{ .field = "macroexpand", .tag = .macroexpand },
        .{ .field = "macroexpand-1", .tag = .macroexpand_1 },
        .{ .field = "boundp", .tag = .boundp },
        .{ .field = "fboundp", .tag = .fboundp },
        .{ .field = "symbol-value", .tag = .symbol_value },
        .{ .field = "symbol-function", .tag = .symbol_function },
        .{ .field = "symbol-plist", .tag = .symbol_plist },
        .{ .field = "function-lambda-expression", .tag = .function_lambda_expression },
        .{ .field = "type-of", .tag = .type_of },
        .{ .field = "intern", .tag = .intern },
        .{ .field = "%make-symbol", .tag = .make_symbol },
        .{ .field = "symbol-name", .tag = .sym_name },
        .{ .field = "makunbound", .tag = .makunbound },
        .{ .field = "abs", .tag = .abs },
        .{ .field = "zerop", .tag = .zerop },
        .{ .field = "plusp", .tag = .plusp },
        .{ .field = "minusp", .tag = .minusp },
        .{ .field = "evenp", .tag = .evenp },
        .{ .field = "oddp", .tag = .oddp },
        .{ .field = "sqrt", .tag = .sqrt },
        .{ .field = "sin", .tag = .sin },
        .{ .field = "cos", .tag = .cos },
        .{ .field = "tan", .tag = .tan },
        .{ .field = "asin", .tag = .asin },
        .{ .field = "acos", .tag = .acos },
        .{ .field = "sinh", .tag = .sinh },
        .{ .field = "cosh", .tag = .cosh },
        .{ .field = "tanh", .tag = .tanh },
        .{ .field = "asinh", .tag = .asinh },
        .{ .field = "acosh", .tag = .acosh },
        .{ .field = "atanh", .tag = .atanh },
        .{ .field = "exp", .tag = .exp },
        // log is handled separately in compilePrimitive for 1/2-arg dispatch
        .{ .field = "decode-float", .tag = .decode_float },
        .{ .field = "integer-decode-float", .tag = .integer_decode_float },
        .{ .field = "float-radix", .tag = .float_radix },
        .{ .field = "float-digits", .tag = .float_digits },
        .{ .field = "vector-length", .tag = .vec_len },
        .{ .field = "copy-structure", .tag = .copy_structure },
        .{ .field = "%error", .tag = .error_user },
        .{ .field = "%floor", .tag = .floor },
        .{ .field = "%ceiling", .tag = .ceiling },
        .{ .field = "%round", .tag = .round },
        .{ .field = "%truncate", .tag = .quot },
        .{ .field = "%fill-pointer", .tag = .vec_fill_ptr },
        .{ .field = "%vector-pop", .tag = .vec_pop },
        .{ .field = "%find-class", .tag = .find_class },
        .{ .field = "%class-name", .tag = .class_name },
        .{ .field = "string-length", .tag = .str_len },
        .{ .field = "write", .tag = .write },
        .{ .field = "write-char", .tag = .write_char },
        .{ .field = "char-upcase", .tag = .char_upcase },
        .{ .field = "char-downcase", .tag = .char_downcase },
        // digit-char-p removed from unary dispatch — stdlib handles optional radix
        .{ .field = "alpha-char-p", .tag = .alpha_char_p },
        // parse-integer removed from unary dispatch — stdlib handles kwargs
        .{ .field = "write-to-string", .tag = .write_to_string },
        .{ .field = "lognot", .tag = .lognot },
        .{ .field = "logcount", .tag = .logcount },
        .{ .field = "integer-length", .tag = .integer_length },
        .{ .field = "read-file", .tag = .read_file },
        .{ .field = "delete-file", .tag = .delete_file },
        .{ .field = "probe-file", .tag = .probe_file },
        .{ .field = "file-write-date", .tag = .file_write_date },
        .{ .field = "file-author", .tag = .file_author },
        .{ .field = "list-to-string", .tag = .list_to_string },
        // string-upcase/string-downcase: stdlib wrappers handle :start/:end
        .{ .field = "%string-upcase", .tag = .string_upcase },
        .{ .field = "%string-downcase", .tag = .string_downcase },
        .{ .field = "rationalp", .tag = .rationalp },
        .{ .field = "complexp", .tag = .complexp },
        .{ .field = "real-part", .tag = .real_part },
        .{ .field = "imag-part", .tag = .imag_part },
        .{ .field = "numerator", .tag = .numerator },
        .{ .field = "denominator", .tag = .denominator },
        .{ .field = "rational", .tag = .rational },
        .{ .field = "rationalize", .tag = .rationalize },
        .{ .field = "get-macro-character", .tag = .get_macro_character },
        .{ .field = "hash-table-p", .tag = .hashtablep },
        .{ .field = "clrhash", .tag = .hash_clear },
        .{ .field = "hash-table-test", .tag = .hash_test },
        .{ .field = "hash-table-keys", .tag = .hash_keys },
        .{ .field = "hash-table-alist", .tag = .hash_alist },
        .{ .field = "sxhash", .tag = .sxhash },
        .{ .field = "streamp", .tag = .streamp },
        .{ .field = "input-stream-p", .tag = .input_stream_p },
        .{ .field = "output-stream-p", .tag = .output_stream_p },
        .{ .field = "open-stream-p", .tag = .open_stream_p },
        .{ .field = "interactive-stream-p", .tag = .interactive_stream_p },
        .{ .field = "stream-element-type", .tag = .stream_element_type },
        .{ .field = "stream-external-format", .tag = .stream_external_format },
        .{ .field = "make-string-input-stream", .tag = .make_string_input_stream },
        // Compound stream accessors
        .{ .field = "broadcast-stream-streams", .tag = .broadcast_stream_streams },
        .{ .field = "concatenated-stream-streams", .tag = .concatenated_stream_streams },
        .{ .field = "echo-stream-input-stream", .tag = .echo_stream_input_stream },
        .{ .field = "echo-stream-output-stream", .tag = .echo_stream_output_stream },
        .{ .field = "synonym-stream-symbol", .tag = .synonym_stream_symbol },
        .{ .field = "two-way-stream-input-stream", .tag = .two_way_stream_input_stream },
        .{ .field = "two-way-stream-output-stream", .tag = .two_way_stream_output_stream },
        // Compound stream constructors (simple cases)
        .{ .field = "%make-synonym-stream", .tag = .make_synonym_stream },
        .{ .field = "%make-broadcast-stream-list", .tag = .make_broadcast_stream_list },
        .{ .field = "%make-concatenated-stream-list", .tag = .make_concatenated_stream_list },
        .{ .field = "%disassemble", .tag = .disassemble },
        .{ .field = "%read-char-from-stream", .tag = .read_char_stream },
        .{ .field = "%peek-char-from-stream", .tag = .peek_char_stream },
        .{ .field = "%close-stream", .tag = .close_stream },
        .{ .field = "get-output-stream-string", .tag = .get_output_stream_string },
        .{ .field = "%pathname-host", .tag = .pathname_host },
        .{ .field = "%pathname-device", .tag = .pathname_device },
        .{ .field = "%pathname-directory", .tag = .pathname_directory },
        .{ .field = "%pathname-name", .tag = .pathname_name },
        .{ .field = "%pathname-type", .tag = .pathname_type },
        .{ .field = "%pathname-version", .tag = .pathname_version },
        .{ .field = "pathname-host", .tag = .pathname_host },
        .{ .field = "pathname-device", .tag = .pathname_device },
        .{ .field = "pathname-directory", .tag = .pathname_directory },
        .{ .field = "pathname-name", .tag = .pathname_name },
        .{ .field = "pathname-type", .tag = .pathname_type },
        .{ .field = "pathname-version", .tag = .pathname_version },
        .{ .field = "truename", .tag = .truename },
        .{ .field = "ensure-directories-exist", .tag = .ensure_directories_exist },
        .{ .field = "pathname", .tag = .pathname },
        .{ .field = "parse-namestring", .tag = .parse_namestring },
        .{ .field = "namestring", .tag = .namestring },
        .{ .field = "directory-namestring", .tag = .directory_namestring },
        .{ .field = "file-namestring", .tag = .file_namestring },
        .{ .field = "host-namestring", .tag = .host_namestring },
        .{ .field = "wild-pathname-p", .tag = .wild_pathname_p },
        .{ .field = "decode-universal-time", .tag = .decode_universal_time },
        .{ .field = "package-symbols-table", .tag = .package_symbols_table },
        .{ .field = "package-exports-table", .tag = .package_exports_table },
        .{ .field = "%package-symbols-list", .tag = .package_symbols_list },
        .{ .field = "%package-exports-list", .tag = .package_exports_list },
        .{ .field = "package-name", .tag = .package_name },
        .{ .field = "package-nicknames", .tag = .package_nicknames },
        .{ .field = "package-use-list", .tag = .package_use_list },
        .{ .field = "package-used-by-list", .tag = .package_used_by_list },
        .{ .field = "package-shadowing-symbols", .tag = .package_shadowing_symbols },
        .{ .field = "packagep", .tag = .packagep },
        .{ .field = "symbol-package", .tag = .symbol_package },
        .{ .field = "find-package", .tag = .find_package },
        .{ .field = "delete-package", .tag = .delete_package },
        .{ .field = "find-all-symbols", .tag = .pkg_find_all_symbols },
        .{ .field = "apropos-list", .tag = .apropos_list },
        .{ .field = "read-char-no-hang", .tag = .read_char_no_hang },
        .{ .field = "restart-name", .tag = .restart_name },
        .{ .field = "directory", .tag = .directory },
        .{ .field = "enough-namestring", .tag = .enough_namestring },
        .{ .field = "random", .tag = .random },
        .{ .field = "random-seed", .tag = .random_seed },
        .{ .field = "method-qualifiers", .tag = .method_qualifiers },
        .{ .field = "method-specializers", .tag = .method_specializers },
        .{ .field = "method-function", .tag = .method_function },
        .{ .field = "generic-function-methods", .tag = .generic_function_methods },
        .{ .field = "generic-function-lambda-list", .tag = .generic_function_lambda_list },
        .{ .field = "generic-function-name", .tag = .generic_function_name },
        .{ .field = "%sleep", .tag = .sleep },
    };

    /// Dispatch entry for simple binary primitives
    const BinaryEntry = struct { field: []const u8, tag: PrimTag };
    const binary_dispatch = [_]BinaryEntry{
        .{ .field = "mod", .tag = .mod },
        .{ .field = "%", .tag = .mod },
        .{ .field = "quot", .tag = .quot },
        .{ .field = "rem", .tag = .rem },
        .{ .field = "eq", .tag = .eq },
        .{ .field = "equal", .tag = .equal },
        .{ .field = "eql", .tag = .eql },
        .{ .field = "equalp", .tag = .equalp },
        .{ .field = "<", .tag = .lt },
        .{ .field = ">", .tag = .gt },
        .{ .field = "<=", .tag = .le },
        .{ .field = ">=", .tag = .ge },
        .{ .field = "=", .tag = .num_eq },
        .{ .field = "cons", .tag = .cons },
        .{ .field = "append", .tag = .append },
        .{ .field = "nth", .tag = .nth },
        .{ .field = "nthcdr", .tag = .nthcdr },
        .{ .field = "rplaca", .tag = .rplaca },
        .{ .field = "rplacd", .tag = .rplacd },
        .{ .field = "char=", .tag = .char_eq },
        .{ .field = "char<", .tag = .char_lt },
        .{ .field = "char>", .tag = .char_gt },
        .{ .field = "typep", .tag = .typep },
        .{ .field = "subtypep", .tag = .subtypep },
        .{ .field = "unintern", .tag = .unintern },
        // find-symbol: handled by stdlib wrapper (optional package arg)
        .{ .field = "copy-symbol", .tag = .copy_symbol },
        .{ .field = "file-string-length", .tag = .file_string_length },
        .{ .field = "set", .tag = .set_sym_val },
        .{ .field = "%set-symbol-value", .tag = .set_sym_val },
        .{ .field = "%set-symbol-plist", .tag = .set_symbol_plist },
        .{ .field = "get", .tag = .get },
        .{ .field = "remprop", .tag = .remprop },
        .{ .field = "svref", .tag = .vec_ref },
        .{ .field = "%vector-push", .tag = .vec_push },
        .{ .field = "%set-fill-pointer", .tag = .vec_set_fill_ptr },
        .{ .field = "%set-adjustable", .tag = .vec_set_adjustable },
        .{ .field = "string-concat", .tag = .str_concat },
        .{ .field = "string=", .tag = .str_eq },
        .{ .field = "string<", .tag = .str_lt },
        .{ .field = "string>", .tag = .str_gt },
        .{ .field = "string<=", .tag = .str_le },
        .{ .field = "string>=", .tag = .str_ge },
        .{ .field = "logand", .tag = .logand },
        .{ .field = "logior", .tag = .logior },
        .{ .field = "logxor", .tag = .logxor },
        .{ .field = "ash", .tag = .ash },
        .{ .field = "lognand", .tag = .lognand },
        .{ .field = "lognor", .tag = .lognor },
        .{ .field = "logandc1", .tag = .logandc1 },
        .{ .field = "logandc2", .tag = .logandc2 },
        .{ .field = "logorc1", .tag = .logorc1 },
        .{ .field = "logorc2", .tag = .logorc2 },
        .{ .field = "logeqv", .tag = .logeqv },
        .{ .field = "logbitp", .tag = .logbitp },
        .{ .field = "write-file", .tag = .write_file },
        .{ .field = "rename-file", .tag = .rename_file },
        .{ .field = "make-complex", .tag = .make_complex },
        .{ .field = "write-to-stream", .tag = .write_to_stream },
        .{ .field = "merge-pathnames", .tag = .merge_pathnames },
        .{ .field = "%import", .tag = .pkg_import },
        .{ .field = "%use-package", .tag = .pkg_use_package },
        .{ .field = "%unexport", .tag = .pkg_unexport },
        .{ .field = "%shadow", .tag = .pkg_shadow },
        .{ .field = "%shadowing-import", .tag = .pkg_shadowing_import },
        .{ .field = "%unuse-package", .tag = .pkg_unuse_package },
        .{ .field = "%unintern", .tag = .pkg_unintern },
        .{ .field = "%find-symbol", .tag = .pkg_find_symbol },
        .{ .field = "pathname-match-p", .tag = .pathname_match_p },
        .{ .field = "%make-echo-stream", .tag = .make_echo_stream },
        .{ .field = "%make-two-way-stream", .tag = .make_two_way_stream },
        .{ .field = "%open-file", .tag = .open_file },
    };

    /// Dispatch entry for ternary primitives
    const TernaryEntry = struct { field: []const u8, tag: PrimTag };
    const ternary_dispatch = [_]TernaryEntry{
        .{ .field = "put", .tag = .put },
        .{ .field = "%vector-push-extend", .tag = .vec_push_ext },
        .{ .field = "%adjust-array", .tag = .vec_adjust },
        .{ .field = "set-dispatch-macro-character", .tag = .set_dispatch_macro_character },
        .{ .field = "%make-package", .tag = .pkg_make_package },
        .{ .field = "%rename-package", .tag = .pkg_rename_package },
    };

    /// Dispatch entry for nullary primitives
    const NullaryEntry = struct { field: []const u8, tag: PrimTag };
    const nullary_dispatch = [_]NullaryEntry{
        .{ .field = "%read-char", .tag = .read_char },
        .{ .field = "%peek-char", .tag = .peek_char },
        // read: handled by stdlib wrapper (optional stream arg)
        .{ .field = "%read", .tag = .read },
        .{ .field = "terpri", .tag = .terpri },
        .{ .field = "make-string-output-stream", .tag = .make_string_output_stream },
        .{ .field = "get-universal-time", .tag = .get_universal_time },
        .{ .field = "get-internal-real-time", .tag = .get_internal_real_time },
        .{ .field = "get-internal-run-time", .tag = .get_internal_run_time },
        .{ .field = "get-decoded-time", .tag = .get_decoded_time },
        .{ .field = "room", .tag = .room },
        .{ .field = "lisp-implementation-type", .tag = .lisp_implementation_type },
        .{ .field = "lisp-implementation-version", .tag = .lisp_implementation_version },
        .{ .field = "software-type", .tag = .software_type },
        .{ .field = "machine-type", .tag = .machine_type },
        .{ .field = "machine-instance", .tag = .machine_instance },
        .{ .field = "machine-version", .tag = .machine_version },
        .{ .field = "software-version", .tag = .software_version },
        .{ .field = "short-site-name", .tag = .short_site_name },
        .{ .field = "long-site-name", .tag = .long_site_name },
        .{ .field = "user-homedir-pathname", .tag = .user_homedir_pathname },
        .{ .field = "list-all-packages", .tag = .list_all_packages },
        .{ .field = "compute-restarts", .tag = .compute_restarts },
    };

    /// Composed accessor patterns (c[ad]+r)
    const ComposedEntry = struct { field: []const u8, pattern: []const u8 };
    const composed_dispatch = [_]ComposedEntry{
        .{ .field = "caar", .pattern = "aa" },
        .{ .field = "cadr", .pattern = "ad" },
        .{ .field = "second", .pattern = "ad" },
        .{ .field = "cdar", .pattern = "da" },
        .{ .field = "cddr", .pattern = "dd" },
        .{ .field = "caaar", .pattern = "aaa" },
        .{ .field = "caadr", .pattern = "aad" },
        .{ .field = "cadar", .pattern = "ada" },
        .{ .field = "caddr", .pattern = "add" },
        .{ .field = "third", .pattern = "add" },
        .{ .field = "cdaar", .pattern = "daa" },
        .{ .field = "cdadr", .pattern = "dad" },
        .{ .field = "cddar", .pattern = "dda" },
        .{ .field = "cdddr", .pattern = "ddd" },
        .{ .field = "fourth", .pattern = "addd" },
    };

    /// Return fixed arity for primitive function references that can be wrapped
    /// as (lambda (...) (prim ...)). Variadic primitives return null.
    pub fn primitiveRefArity(self: *Compiler, sym: Value) ?PrimitiveRefArity {
        const b = if (self.builtins) |val| val else return null;
        const dispatch_sym = self.canonicalBuiltinSymbol(sym);
        const s = dispatch_sym.raw;

        // INTERN has optional package argument; do not force fixed-arity wrappers.
        if (s == b.intern.raw) return null;

        inline for (nullary_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) return .nullary;
        }
        inline for (unary_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) return .unary;
        }
        inline for (binary_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) return .binary;
        }
        inline for (ternary_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) return .ternary;
        }

        // Composed accessors in this table are all unary.
        inline for (composed_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) return .unary;
        }

        // Sequence operators compiled via custom handlers still have
        // a stable 2-arg core shape for (function <sym>) wrappers.
        if (s == b.member.raw) return .binary;
        if (s == b.assoc.raw) return .binary;
        if (s == b.find.raw) return .binary;
        if (s == b.position.raw) return .binary;
        if (s == b.count.raw) return .binary;
        if (s == b.remove.raw) return .binary;

        return null;
    }

    /// Return the subtypes of a condition type for handler-case dispatch.
    /// Used to compile (handler-case ... (arithmetic-error (c) ...)) so it
    /// also catches division-by-zero, floating-point-overflow, etc.
    fn getConditionSubtypes(self: *Compiler, condition_type: Value) []const Value {
        const b = self.builtins orelse return &.{};
        // Table-driven: each entry is (supertype, list of subtypes)
        const Entry = struct { super: Value, subs: []const Value };
        const table = [_]Entry{
            .{ .super = b.@"arithmetic-error", .subs = &.{b.@"division-by-zero"} },
            .{ .super = b.@"cell-error", .subs = &.{ b.@"unbound-variable", b.@"undefined-function" } },
            .{ .super = b.@"stream-error", .subs = &.{b.@"end-of-file"} },
            .{ .super = b.warning, .subs = &.{b.@"simple-warning"} },
        };
        for (&table) |entry| {
            if (condition_type.raw == entry.super.raw) return entry.subs;
        }
        return &.{};
    }

    fn canonicalBuiltinSymbol(self: *Compiler, sym: Value) Value {
        if (!sym.isSymbol()) return sym;
        const heap = if (self.heap) |val| val else return sym;
        const cl_pkg = if (heap.cl_package) |val| val else return sym;
        const name = sym.toPtr(Symbol).getName();
        return cl_pkg.findAccessibleUpper(name) orelse sym;
    }

    fn lookupMacroDef(self: *Compiler, sym: Value) ?Value {
        if (!sym.isSymbol()) return null;
        if (self.macro_table.get(sym)) |def| return def;
        const canonical = self.canonicalBuiltinSymbol(sym);
        if (canonical.raw != sym.raw) {
            if (self.macro_table.get(canonical)) |def| return def;
        }
        // Also check symbol property list for macro-function (set via setf)
        if (sym.isSymbol()) {
            const sym_ptr = sym.toPtr(Symbol);
            if (sym_ptr.plist.isCons()) {
                // Search plist for 'macro-function key
                // Plist stored as alist: ((key . val) (key . val) ...)
                var plist = sym_ptr.plist;
                while (plist.isCons()) {
                    const pc = plist.toPtr(Cons);
                    const entry = pc.car;
                    if (entry.isCons()) {
                        const entry_cons = entry.toPtr(Cons);
                        if (entry_cons.car.isSymbol()) {
                            const key_name = entry_cons.car.toPtr(Symbol).getName();
                            if (std.mem.eql(u8, key_name, "MACRO-FUNCTION") or
                                std.mem.eql(u8, key_name, "macro-function"))
                            {
                                const val = entry_cons.cdr;
                                if (val.isClosure()) return val;
                            }
                        }
                    }
                    plist = pc.cdr;
                }
            }
        }
        return null;
    }

    fn lookupSymbolMacro(self: *Compiler, sym: Value) ?Value {
        if (!sym.isSymbol()) return null;
        if (self.symbol_macros.get(sym)) |expansion| return expansion;
        const canonical = self.canonicalBuiltinSymbol(sym);
        if (canonical.raw != sym.raw) {
            if (self.symbol_macros.get(canonical)) |expansion| return expansion;
        }
        return null;
    }

    fn compilePrimitive(self: *Compiler, sym: Value, args: Value, env: *const Env) anyerror!*Ir {
        // Heap GC can move interned symbols while compiling; refresh identity-based
        // builtin handles before symbol.raw dispatch.
        try self.refreshBuiltins();
        const dispatch_sym = self.canonicalBuiltinSymbol(sym);
        const s = dispatch_sym.raw;
        const b = if (self.builtins) |val| val else return error.InvalidSyntax;

        // Variadic arithmetic (+, -, *, /)
        if (s == b.@"+".raw) return self.compileVariadicArith(args, env, .add, 0);
        if (s == b.@"-".raw) return self.compileVariadicArith(args, env, .sub, null);
        if (s == b.@"*".raw) return self.compileVariadicArith(args, env, .mul, 1);
        if (s == b.@"/".raw) return self.compileVariadicArith(args, env, .div, null);
        if (s == b.append.raw) {
            if (args.isNil()) return try self.builder.lit(Value.nil);

            var parts = std.ArrayList(*Ir){};
            defer parts.deinit(self.allocator);

            var rest = args;
            while (rest.isCons()) : (rest = rest.toPtr(Cons).cdr) {
                const cell = rest.toPtr(Cons);
                try parts.append(self.allocator, try self.compile(cell.car, env));
            }

            if (parts.items.len == 0) return try self.builder.lit(Value.nil);
            if (parts.items.len == 1) return parts.items[0];

            var acc = parts.items[0];
            var i: usize = 1;
            while (i < parts.items.len) : (i += 1) {
                acc = try self.builder.append(acc, parts.items[i]);
            }
            return acc;
        }
        if (s == b.log.raw) {
            // LOG supports (log x) and (log x base); lower base form to (/ (log x) (log base)).
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const x_ir = try self.compile(cons1.car, env);
            if (cons1.cdr.isNil()) {
                return try self.builder.log_fn(x_ir);
            }
            if (!cons1.cdr.isCons()) return error.InvalidSyntax;
            const cons2 = cons1.cdr.toPtr(Cons);
            if (!cons2.cdr.isNil()) return error.InvalidSyntax;
            const base_ir = try self.compile(cons2.car, env);
            const x_log = try self.builder.log_fn(x_ir);
            const base_log = try self.builder.log_fn(base_ir);
            return try self.builder.div(x_log, base_log);
        }

        // Table-driven dispatch for unary primitives
        inline for (unary_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) {
                return self.compileUnaryPrim(args, env, entry.tag);
            }
        }

        // Table-driven dispatch for binary primitives
        inline for (binary_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) {
                return self.compileBinaryPrim(args, env, entry.tag);
            }
        }

        // Table-driven dispatch for ternary primitives
        inline for (ternary_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) {
                return self.compileTernaryPrim(args, env, entry.tag);
            }
        }

        // Table-driven dispatch for nullary primitives
        inline for (nullary_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) {
                return self.compileNullaryPrim(entry.tag);
            }
        }

        // Table-driven dispatch for composed accessors
        inline for (composed_dispatch) |entry| {
            if (s == @field(b, entry.field).raw) {
                return self.compileComposedAccessor(args, env, entry.pattern);
            }
        }

        // Special cases with custom handling
        if (s == b.gensym.raw) {
            if (args.isNil()) {
                return self.compileNullaryPrim(.gensym);
            } else {
                return self.compileUnaryPrim(args, env, .gensym);
            }
        }
        if (s == b.atan.raw) {
            // atan can be 1 or 2 arg
            if (args.isNil()) return error.InvalidSyntax;
            const rest = args.toPtr(Cons).cdr;
            if (rest.isNil()) {
                return self.compileUnaryPrim(args, env, .atan);
            } else {
                return self.compileBinaryPrim(args, env, .atan2);
            }
        }
        if (s == b.member.raw) return self.compileMemberWithTest(args, env);
        if (s == b.assoc.raw) return self.compileAssocWithTest(args, env);
        if (s == b.find.raw and !self.hasKeywordArg(args, b.kw_key)) return self.compileFindWithTest(args, env);
        if (s == b.position.raw and !self.hasKeywordArg(args, b.kw_key)) return self.compilePositionWithTest(args, env);
        if (s == b.count.raw and !self.hasKeywordArg(args, b.kw_key)) return self.compileCountWithTest(args, env);
        if (s == b.remove.raw and !self.hasKeywordArg(args, b.kw_key) and !self.hasKeywordArg(args, b.kw_count)) return self.compileRemoveWithTest(args, env);
        if (s == b.list.raw) return self.compileListPrim(args, env);
        if (s == b.@"%make-broadcast-stream".raw) return self.compileBroadcastStream(args, env);
        if (s == b.@"%make-concatenated-stream".raw) return self.compileConcatenatedStream(args, env);
        if (s == b.@"class-of".raw) return self.compileClassOf(args, env);
        // floor/ceiling/round: 1-arg uses opcode (sets secondary values), 2-arg uses stdlib defun
        if (s == b.floor.raw) { if (try self.compileFloorCeilRound(args, env, .floor)) |r| return r; }
        if (s == b.ceiling.raw) { if (try self.compileFloorCeilRound(args, env, .ceiling)) |r| return r; }
        if (s == b.round.raw) { if (try self.compileFloorCeilRound(args, env, .round)) |r| return r; }
        // truncate: 1-arg only (2-arg handled by stdlib)
        if (s == b.truncate.raw and args.isCons() and !args.toPtr(Cons).cdr.isCons())
            return self.compileBinaryPrim(args, env, .quot);
        if (s == b.aref.raw) return self.compileAref(args, env);
        if (s == b.@"make-string".raw) return self.compileMakeString(args, env);
        if (s == b.@"make-vector".raw) return self.compileMakeVector(args, env);
        if (s == b.@"%svset".raw) return self.compileSvset(args, env);
        if (s == b.@"%aset".raw) return self.compileAset(args, env);
        if (s == b.@"%set-slot-value".raw) return self.compileSetSlotValue(args, env);
        if (s == b.@"%sset".raw) return self.compileSset(args, env);
        if (s == b.@"%make-unbound".raw) return self.builder.makeUnbound();
        if (s == b.@"%class-of".raw) return self.compileClassOf(args, env);
        if (s == b.vector.raw) return self.compileVectorPrim(args, env);
        if (s == b.@"make-array".raw) return self.compileMakeArray(args, env);
        if (s == b.char.raw or s == b.schar.raw) return self.compileBinaryPrim(args, env, .str_ref);
        if (s == b.substring.raw) return self.compileSubstring(args, env);
        // subseq: handled by stdlib (supports strings, vectors, and lists)
        if (s == b.concatenate.raw) return self.compileConcatenate(args, env);
        if (s == b.format.raw) return self.compileFormat(args, env);
        if (s == b.print.raw) return self.compilePrint(args, env);
        if (s == b.princ.raw) return self.compilePrinc(args, env);
        if (s == b.@"encode-universal-time".raw) return self.compileEncodeUniversalTime(args, env);
        if (s == b.@"%make-pathname".raw) return self.compileMakePathname(args, env);
        if (s == b.@"set-macro-character".raw) return self.compileSetMacroCharacter(args, env);
        if (s == b.@"make-hash-table".raw) return self.compileMakeHash(args);
        if (s == b.gethash.raw) return self.compileGethash(args, env);
        if (s == b.puthash.raw) return self.compileSethash(args, env);
        if (s == b.remhash.raw) return self.compileRemhash(args, env);
        if (s == b.@"hash-table-count".raw) return self.compileHashTableCount(args, env);
        if (s == b.@"hash-table-capacity".raw) return self.compileHashTableCapacity(args, env);

        // Stream I/O operations - inline handling
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
        if (s == b.@"%close".raw or s == b.close.raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            if (cons1.cdr.isCons()) {
                const cons2 = cons1.cdr.toPtr(Cons);
                _ = try self.compile(cons2.car, env);
            }
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
        if (s == b.@"%write-string".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            if (!cons1.cdr.isCons()) return error.InvalidSyntax;
            const cons2 = cons1.cdr.toPtr(Cons);
            const text_ir = try self.compile(cons2.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .write_string = .{ .left = stream_ir, .right = text_ir } };
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
        if (s == b.@"%clear-input".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .clear_input = .{ .operand = stream_ir } };
            return node;
        }
        if (s == b.@"%clear-output".raw) {
            if (!args.isCons()) return error.InvalidSyntax;
            const cons1 = args.toPtr(Cons);
            const stream_ir = try self.compile(cons1.car, env);
            const node = try self.allocator.create(Ir);
            node.* = .{ .clear_output = .{ .operand = stream_ir } };
            return node;
        }

        // Class/slot introspection (CLOS)
        if (s == b.@"class-direct-superclasses".raw) return self.compileUnaryPrim(args, env, .class_direct_superclasses);
        if (s == b.@"class-precedence-list".raw) return self.compileUnaryPrim(args, env, .class_precedence_list);
        if (s == b.@"class-direct-slots".raw) return self.compileUnaryPrim(args, env, .class_direct_slots);
        if (s == b.@"class-slots".raw) return self.compileUnaryPrim(args, env, .class_slots);
        if (s == b.@"slot-definition-name".raw) return self.compileUnaryPrim(args, env, .slot_definition_name);
        if (s == b.@"slot-definition-initform".raw) return self.compileUnaryPrim(args, env, .slot_definition_initform);
        if (s == b.@"slot-definition-initargs".raw) return self.compileUnaryPrim(args, env, .slot_definition_initargs);
        if (s == b.@"slot-definition-readers".raw) return self.compileUnaryPrim(args, env, .slot_definition_readers);
        if (s == b.@"slot-definition-writers".raw) return self.compileUnaryPrim(args, env, .slot_definition_writers);
        if (s == b.@"slot-definition-allocation".raw) return self.compileUnaryPrim(args, env, .slot_definition_allocation);
        if (s == b.@"slot-definition-type".raw) return self.compileUnaryPrim(args, env, .slot_definition_type);

        return error.InvalidSyntax; // Not a known primitive - let the special form handler try it
    }

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
            .equalp => try self.builder.equalp(left, right),
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
            .str_lt => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .str_lt = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_gt => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .str_gt = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_le => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .str_le = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_ge => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .str_ge = .{ .left = left, .right = right } };
                break :blk node;
            },
            .str_concat => try self.builder.strConcat(left, right),
            .char_eq => try self.builder.charEq(left, right),
            .char_lt => try self.builder.charLt(left, right),
            .char_gt => try self.builder.charGt(left, right),
            .typep => try self.builder.typep(left, right),
            .subtypep => try self.builder.subtypep(left, right),
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
            .lognand => try self.builder.lognand(left, right),
            .lognor => try self.builder.lognor(left, right),
            .logandc1 => try self.builder.logandc1(left, right),
            .logandc2 => try self.builder.logandc2(left, right),
            .logeqv => try self.builder.logeqv(left, right),
            .logbitp => try self.builder.logbitp(left, right),
            .logior => try self.builder.logior(left, right),
            .logxor => try self.builder.logxor(left, right),
            .ash => try self.builder.ash(left, right),
            .atan2 => try self.builder.atan2(left, right),
            .write_file => try self.builder.writeFile(left, right),
            .rename_file => try self.builder.renameFile(left, right),
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
            .find_symbol => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .find_symbol = .{ .left = left, .right = right } };
                break :blk node;
            },
            .copy_symbol => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .copy_symbol = .{ .left = left, .right = right } };
                break :blk node;
            },
            .file_string_length => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .file_string_length = .{ .left = left, .right = right } };
                break :blk node;
            },
            .set_sym_val => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .set_sym_val = .{ .left = left, .right = right } };
                break :blk node;
            },
            .set_symbol_function => try self.builder.setSymbolFunction(left, right),
            .set_symbol_plist => try self.builder.setSymbolPlist(left, right),
            .write_to_stream => try self.builder.writeToStream(left, right),
            .merge_pathnames => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .merge_pathnames = .{ .left = left, .right = right } };
                break :blk node;
            },
            .pkg_import => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_import = .{ .left = left, .right = right } };
                break :blk node;
            },
            .pkg_use_package => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_use_package = .{ .left = left, .right = right } };
                break :blk node;
            },
            .pkg_unexport => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_unexport = .{ .left = left, .right = right } };
                break :blk node;
            },
            .pkg_shadow => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_shadow = .{ .left = left, .right = right } };
                break :blk node;
            },
            .pkg_shadowing_import => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_shadowing_import = .{ .left = left, .right = right } };
                break :blk node;
            },
            .pkg_unuse_package => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_unuse_package = .{ .left = left, .right = right } };
                break :blk node;
            },
            .pkg_unintern => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_unintern = .{ .left = left, .right = right } };
                break :blk node;
            },
            .pkg_find_symbol => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_find_symbol = .{ .left = left, .right = right } };
                break :blk node;
            },
            .vec_set_fill_ptr => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .vec_set_fill_ptr = .{ .left = left, .right = right } };
                break :blk node;
            },
            .vec_set_adjustable => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .vec_set_adjustable = .{ .left = left, .right = right } };
                break :blk node;
            },
            .vec_push => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .vec_push = .{ .left = left, .right = right } };
                break :blk node;
            },
            .make_echo_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .make_echo_stream = .{ .left = left, .right = right } };
                break :blk node;
            },
            .make_two_way_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .make_two_way_stream = .{ .left = left, .right = right } };
                break :blk node;
            },
            .open_file => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .open_file = .{ .left = left, .right = right } };
                break :blk node;
            },
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
            .vec_push_ext => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .vec_push_ext = .{ .first = first, .second = second, .third = third } };
                break :blk node;
            },
            .vec_adjust => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .vec_adjust = .{ .first = first, .second = second, .third = third } };
                break :blk node;
            },
            .set_dispatch_macro_character => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .set_dispatch_macro_character = .{ .first = first, .second = second, .third = third } };
                break :blk node;
            },
            .pkg_make_package => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_make_package = .{ .first = first, .second = second, .third = third } };
                break :blk node;
            },
            .pkg_rename_package => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pkg_rename_package = .{ .first = first, .second = second, .third = third } };
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
    fn compileWrite(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const obj_ir = try self.compile(cons.car, env);
        return try self.builder.write(obj_ir);
    }

    fn compilePrint(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return try self.compilePrintOrPrinc(args, env, true);
    }

    fn compilePrinc(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        return try self.compilePrintOrPrinc(args, env, false);
    }

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
                const heap = if (self.heap) |val| val else return error.UninitializedBuiltins;
                const newline_ir = try self.builder.lit(try heap.allocBaseString("\n"));
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
        if (!cons.cdr.isNil()) return error.InvalidSyntax;
        const operand = try self.compile(cons.car, env);

        return switch (prim) {
            .car => try self.builder.car(operand),
            .cdr => try self.builder.cdr(operand),
            .consp => try self.builder.consp(operand),
            .symbolp => try self.builder.symbolp(operand),
            .numberp => try self.builder.numberp(operand),
            .integerp => try self.builder.integerp(operand),
            .realp => try self.builder.realp(operand),
            .nilp => try self.builder.nilp(operand),
            .not => try self.builder.not(operand),
            .vec_len => try self.builder.vecLen(operand),
            .copy_structure => try self.builder.copyStructure(operand),
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
            .write => try self.builder.write(operand),
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
            .logcount => try self.builder.logcount(operand),
            .integer_length => try self.builder.integerLength(operand),
            .read_file => try self.builder.readFile(operand),
            .delete_file => try self.builder.deleteFile(operand),
            .probe_file => try self.builder.probeFile(operand),
            .file_write_date => try self.builder.fileWriteDate(operand),
            .file_author => try self.builder.fileAuthor(operand),
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
            .method_qualifiers => try self.builder.methodQualifiers(operand),
            .method_specializers => try self.builder.methodSpecializers(operand),
            .method_function => try self.builder.methodFunction(operand),
            .generic_function_methods => try self.builder.genericFunctionMethods(operand),
            .generic_function_lambda_list => try self.builder.genericFunctionLambdaList(operand),
            .generic_function_name => try self.builder.genericFunctionName(operand),
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
            .make_symbol => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .make_symbol = .{ .operand = operand } };
                break :blk node;
            },
            .sym_name => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .sym_name = .{ .operand = operand } };
                break :blk node;
            },
            .makunbound => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .makunbound = .{ .operand = operand } };
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
            .listen => try self.builder.listen(operand),
            .upgraded_complex_part_type => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .upgraded_complex_part_type = .{ .operand = operand } };
                break :blk node;
            },
            .load => try self.builder.load(operand),
            .read_from_string => try self.builder.readFromString(operand),
            .eval => try self.builder.eval(operand),
            .gensym => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .gensym = .{ .operand = operand } };
                break :blk node;
            },
            .macroexpand => try self.builder.macroexpand(operand),
            .macroexpand_1 => try self.builder.macroexpand1(operand),
            .boundp => try self.builder.boundp(operand),
            .fboundp => try self.builder.fboundp(operand),
            .symbol_value => try self.builder.symbolValue(operand),
            .symbol_function => try self.builder.symbolFunction(operand),
            .symbol_plist => try self.builder.symbolPlist(operand),
            .function_lambda_expression => try self.builder.functionLambdaExpression(operand),
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
            .asin => try self.builder.asin(operand),
            .acos => try self.builder.acos(operand),
            .atan => try self.builder.atan(operand),
            .sinh => try self.builder.sinh(operand),
            .cosh => try self.builder.cosh(operand),
            .tanh => try self.builder.tanh(operand),
            .asinh => try self.builder.asinh(operand),
            .acosh => try self.builder.acosh(operand),
            .atanh => try self.builder.atanh(operand),
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
            .rational => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .rational = .{ .operand = operand } };
                break :blk node;
            },
            .rationalize => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .rationalize = .{ .operand = operand } };
                break :blk node;
            },
            .hashtablep => try self.builder.hashtablep(operand),
            .packagep => try self.builder.packagep(operand),
            .symbol_package => try self.builder.symbolPackage(operand),
            .package_name => try self.builder.packageName(operand),
            .package_nicknames => try self.builder.packageNicknames(operand),
            .package_use_list => try self.builder.packageUseList(operand),
            .package_used_by_list => try self.builder.packageUsedByList(operand),
            .package_shadowing_symbols => try self.builder.packageShadowingSymbols(operand),
            .find_package => try self.builder.findPackage(operand),
            .delete_package => try self.builder.deletePackage(operand),
            .pkg_find_all_symbols => try self.builder.findAllSymbols(operand),
            .apropos_list => try self.builder.aproposList(operand),
            .read_char_no_hang => try self.builder.readCharNoHang(operand),
            .restart_name => try self.builder.restartName(operand),
            .directory => try self.builder.directory(operand),
            .enough_namestring => try self.builder.enoughNamestring(operand),
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
            .sxhash => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .sxhash = .{ .operand = operand } };
                break :blk node;
            },
            .streamp => try self.builder.streamp(operand),
            .input_stream_p => try self.builder.inputStreamP(operand),
            .output_stream_p => try self.builder.outputStreamP(operand),
            .open_stream_p => try self.builder.openStreamP(operand),
            .interactive_stream_p => try self.builder.interactiveStreamP(operand),
            .stream_element_type => try self.builder.streamElementType(operand),
            .stream_external_format => try self.builder.streamExternalFormat(operand),
            .make_string_input_stream => try self.builder.makeStringInputStream(operand),
            .get_output_stream_string => try self.builder.getOutputStreamString(operand),
            .pathname_host => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pathname_host = .{ .operand = operand } };
                break :blk node;
            },
            .pathname_device => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pathname_device = .{ .operand = operand } };
                break :blk node;
            },
            .pathname_directory => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pathname_directory = .{ .operand = operand } };
                break :blk node;
            },
            .pathname_name => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pathname_name = .{ .operand = operand } };
                break :blk node;
            },
            .pathname_type => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pathname_type = .{ .operand = operand } };
                break :blk node;
            },
            .pathname_version => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pathname_version = .{ .operand = operand } };
                break :blk node;
            },
            .truename => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .truename = .{ .operand = operand } };
                break :blk node;
            },
            .ensure_directories_exist => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .ensure_directories_exist = .{ .operand = operand } };
                break :blk node;
            },
            .pathname => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .pathname = .{ .operand = operand } };
                break :blk node;
            },
            .parse_namestring => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .parse_namestring = .{ .operand = operand } };
                break :blk node;
            },
            .namestring => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .namestring = .{ .operand = operand } };
                break :blk node;
            },
            .directory_namestring => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .directory_namestring = .{ .operand = operand } };
                break :blk node;
            },
            .file_namestring => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .file_namestring = .{ .operand = operand } };
                break :blk node;
            },
            .host_namestring => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .host_namestring = .{ .operand = operand } };
                break :blk node;
            },
            .wild_pathname_p => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .wild_pathname_p = .{ .operand = operand } };
                break :blk node;
            },
            .decode_universal_time => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .decode_universal_time = .{ .operand = operand } };
                break :blk node;
            },
            .find_class => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .find_class = .{ .operand = operand } };
                break :blk node;
            },
            .class_name => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .class_name = .{ .operand = operand } };
                break :blk node;
            },
            .class_direct_superclasses => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .class_direct_superclasses = .{ .operand = operand } };
                break :blk node;
            },
            .class_precedence_list => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .class_precedence_list = .{ .operand = operand } };
                break :blk node;
            },
            .class_direct_slots => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .class_direct_slots = .{ .operand = operand } };
                break :blk node;
            },
            .class_slots => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .class_slots = .{ .operand = operand } };
                break :blk node;
            },
            .slot_definition_name => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .slot_definition_name = .{ .operand = operand } };
                break :blk node;
            },
            .slot_definition_initform => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .slot_definition_initform = .{ .operand = operand } };
                break :blk node;
            },
            .slot_definition_initargs => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .slot_definition_initargs = .{ .operand = operand } };
                break :blk node;
            },
            .slot_definition_readers => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .slot_definition_readers = .{ .operand = operand } };
                break :blk node;
            },
            .slot_definition_writers => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .slot_definition_writers = .{ .operand = operand } };
                break :blk node;
            },
            .slot_definition_allocation => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .slot_definition_allocation = .{ .operand = operand } };
                break :blk node;
            },
            .slot_definition_type => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .slot_definition_type = .{ .operand = operand } };
                break :blk node;
            },
            .vec_fill_ptr => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .vec_fill_ptr = .{ .operand = operand } };
                break :blk node;
            },
            .vec_pop => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .vec_pop = .{ .operand = operand } };
                break :blk node;
            },
            .decode_float => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .decode_float = .{ .operand = operand } };
                break :blk node;
            },
            .integer_decode_float => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .integer_decode_float = .{ .operand = operand } };
                break :blk node;
            },
            .float_radix => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .float_radix = .{ .operand = operand } };
                break :blk node;
            },
            .float_digits => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .float_digits = .{ .operand = operand } };
                break :blk node;
            },
            // Compound stream accessors
            .broadcast_stream_streams => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .broadcast_stream_streams = .{ .operand = operand } };
                break :blk node;
            },
            .concatenated_stream_streams => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .concatenated_stream_streams = .{ .operand = operand } };
                break :blk node;
            },
            .echo_stream_input_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .echo_stream_input_stream = .{ .operand = operand } };
                break :blk node;
            },
            .echo_stream_output_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .echo_stream_output_stream = .{ .operand = operand } };
                break :blk node;
            },
            .synonym_stream_symbol => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .synonym_stream_symbol = .{ .operand = operand } };
                break :blk node;
            },
            .two_way_stream_input_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .two_way_stream_input_stream = .{ .operand = operand } };
                break :blk node;
            },
            .two_way_stream_output_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .two_way_stream_output_stream = .{ .operand = operand } };
                break :blk node;
            },
            .make_synonym_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .make_synonym_stream = .{ .operand = operand } };
                break :blk node;
            },
            .make_broadcast_stream_list => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .make_broadcast_stream_list = .{ .operand = operand } };
                break :blk node;
            },
            .make_concatenated_stream_list => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .make_concatenated_stream_list = .{ .operand = operand } };
                break :blk node;
            },
            .disassemble => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .disassemble = .{ .operand = operand } };
                break :blk node;
            },
            .read_char_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .read_char_stream = .{ .operand = operand } };
                break :blk node;
            },
            .peek_char_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .peek_char_stream = .{ .operand = operand } };
                break :blk node;
            },
            .close_stream => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .close_stream = .{ .operand = operand } };
                break :blk node;
            },
            .package_symbols_table => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .package_symbols_table = .{ .operand = operand } };
                break :blk node;
            },
            .package_exports_table => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .package_exports_table = .{ .operand = operand } };
                break :blk node;
            },
            .package_symbols_list => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .package_symbols_list = .{ .operand = operand } };
                break :blk node;
            },
            .package_exports_list => blk: {
                const node = try self.allocator.create(Ir);
                node.* = .{ .package_exports_list = .{ .operand = operand } };
                break :blk node;
            },
            else => return error.InvalidSyntax,
        };
    }

    /// Compile floor/ceiling/round with optional divisor: (floor x) or (floor x y)
    fn compileFloorCeilRound(self: *Compiler, args: Value, env: *const Env, op: PrimTag) anyerror!?*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);

        // 2-arg case: let stdlib handle it (returns multiple values correctly)
        if (cons.cdr.isCons()) return null;

        // Single argument case: use opcode (sets secondary values in VM)
        const dividend = try self.compile(cons.car, env);
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
            .get_universal_time => try self.builder.getUniversalTime(),
            .get_internal_real_time => try self.builder.getInternalRealTime(),
            .get_internal_run_time => try self.builder.getInternalRunTime(),
            .get_decoded_time => try self.builder.getDecodedTime(),
            .room => try self.builder.room(),
            .lisp_implementation_type => try self.builder.lispImplementationType(),
            .lisp_implementation_version => try self.builder.lispImplementationVersion(),
            .software_type => try self.builder.softwareType(),
            .machine_type => try self.builder.machineType(),
            .machine_instance => try self.builder.machineInstance(),
            .machine_version => try self.builder.machineVersion(),
            .software_version => try self.builder.softwareVersion(),
            .short_site_name => try self.builder.shortSiteName(),
            .long_site_name => try self.builder.longSiteName(),
            .user_homedir_pathname => try self.builder.userHomedirPathname(),
            .list_all_packages => try self.builder.listAllPackages(),
            .compute_restarts => try self.builder.computeRestarts(),
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

    fn compileBroadcastStream(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        var streams = std.ArrayList(*const Ir){};
        defer streams.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const stream_ir = try self.compile(cons.car, env);
            try streams.append(self.allocator, stream_ir);
            current = cons.cdr;
        }

        const node = try self.allocator.create(Ir);
        node.* = .{ .make_broadcast_stream = try self.allocator.dupe(*const Ir, streams.items) };
        return node;
    }

    fn compileConcatenatedStream(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        var streams = std.ArrayList(*const Ir){};
        defer streams.deinit(self.allocator);

        var current = args;
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            const stream_ir = try self.compile(cons.car, env);
            try streams.append(self.allocator, stream_ir);
            current = cons.cdr;
        }

        const node = try self.allocator.create(Ir);
        node.* = .{ .make_concatenated_stream = try self.allocator.dupe(*const Ir, streams.items) };
        return node;
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
        const b = if (self.builtins) |val| val else return error.InvalidSyntax;
        // Delegate to the stdlib implementation so full CL sequence coercion rules
        // apply for both list and string result types.
        return self.compileCall(b.concatenate, args, env);
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

    fn compileEncodeUniversalTime(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (encode-universal-time second minute hour date month year [zone])
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const second_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const minute_ir = try self.compile(cons2.car, env);

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const hour_ir = try self.compile(cons3.car, env);

        if (!cons3.cdr.isCons()) return error.InvalidSyntax;
        const cons4 = cons3.cdr.toPtr(Cons);
        const date_ir = try self.compile(cons4.car, env);

        if (!cons4.cdr.isCons()) return error.InvalidSyntax;
        const cons5 = cons4.cdr.toPtr(Cons);
        const month_ir = try self.compile(cons5.car, env);

        if (!cons5.cdr.isCons()) return error.InvalidSyntax;
        const cons6 = cons5.cdr.toPtr(Cons);
        const year_ir = try self.compile(cons6.car, env);

        // Optional zone argument
        var zone_ir: ?*const Ir = null;
        if (cons6.cdr.isCons()) {
            const cons7 = cons6.cdr.toPtr(Cons);
            zone_ir = try self.compile(cons7.car, env);
        }

        const node = try self.allocator.create(Ir);
        node.* = .{ .encode_universal_time = .{
            .second = second_ir,
            .minute = minute_ir,
            .hour = hour_ir,
            .date = date_ir,
            .month = month_ir,
            .year = year_ir,
            .zone = zone_ir,
        } };
        return node;
    }

    fn compileMakePathname(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (%make-pathname host device directory name type version)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const host_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const device_ir = try self.compile(cons2.car, env);

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const directory_ir = try self.compile(cons3.car, env);

        if (!cons3.cdr.isCons()) return error.InvalidSyntax;
        const cons4 = cons3.cdr.toPtr(Cons);
        const name_ir = try self.compile(cons4.car, env);

        if (!cons4.cdr.isCons()) return error.InvalidSyntax;
        const cons5 = cons4.cdr.toPtr(Cons);
        const type_ir = try self.compile(cons5.car, env);

        if (!cons5.cdr.isCons()) return error.InvalidSyntax;
        const cons6 = cons5.cdr.toPtr(Cons);
        const version_ir = try self.compile(cons6.car, env);

        return try self.builder.makePathname(host_ir, device_ir, directory_ir, name_ir, type_ir, version_ir);
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
                    const canon = self.canonicalBuiltinSymbol(test_sym);
                    // Compare by identity with pre-interned symbols
                    if (canon.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (canon.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (canon.raw == b.equal.raw) {
                        test_type = .equal;
                    } else if (canon.raw == b.equalp.raw) {
                        test_type = .equalp;
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
                    const canon = self.canonicalBuiltinSymbol(test_sym);
                    if (canon.raw == b.eq.raw) {
                        test_type = .eq;
                    } else if (canon.raw == b.eql.raw) {
                        test_type = .eql;
                    } else if (canon.raw == b.equal.raw) {
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
    /// Check if a keyword argument is present in a function call's argument list.
    /// Scans the args list (past positional args) for a keyword matching `kw`.
    fn hasKeywordArg(self: *const Compiler, args: Value, kw: Value) bool {
        _ = self;
        // Skip first two positional args (item, sequence)
        var current = args;
        var skip: u8 = 2;
        while (current.isCons() and skip > 0) {
            current = current.toPtr(Cons).cdr;
            skip -= 1;
        }
        // Scan for keyword
        while (current.isCons()) {
            const cons = current.toPtr(Cons);
            if (cons.car.isKeyword() and cons.car.raw == kw.raw) return true;
            // Skip key-value pair
            if (cons.cdr.isCons()) {
                current = cons.cdr.toPtr(Cons).cdr;
            } else {
                break;
            }
        }
        return false;
    }

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

    fn compileMakeString(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (make-string size &key initial-element element-type allow-other-keys)
        if (!args.isCons()) return error.InvalidSyntax;
        const b = if (self.builtins) |val| val else return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const len_ir = try self.compile(cons1.car, env);

        var fill_ir = try self.builder.lit(Value.nil);
        var allow_other_keys = false;
        var rest = cons1.cdr;

        // Backward-compatible positional fill character.
        if (rest.isCons()) {
            const maybe_pos = rest.toPtr(Cons);
            if (!maybe_pos.car.isKeyword()) {
                fill_ir = try self.compile(maybe_pos.car, env);
                rest = maybe_pos.cdr;
            }
        }

        // Parse keyword arguments.
        while (rest.isCons()) {
            const key_cons = rest.toPtr(Cons);
            const key = key_cons.car;
            if (!key.isKeyword()) return error.InvalidSyntax;
            if (!key_cons.cdr.isCons()) return error.InvalidSyntax;
            const val_cons = key_cons.cdr.toPtr(Cons);

            if (key.raw == b.@"kw_initial-element".raw) {
                fill_ir = try self.compile(val_cons.car, env);
            } else if (key.raw == b.@"kw_allow-other-keys".raw) {
                if (val_cons.car.isNil()) {
                    allow_other_keys = false;
                } else if (val_cons.car.isKeyword()) {
                    allow_other_keys = true;
                } else if (val_cons.car.isFixnum()) {
                    allow_other_keys = val_cons.car.toFixnum() != 0;
                } else {
                    allow_other_keys = true;
                }
            } else if (key.raw == b.@"kw_element-type".raw) {
                // Accepted but ignored for now.
            } else if (!allow_other_keys) {
                return error.InvalidSyntax;
            }

            rest = val_cons.cdr;
        }

        return try self.builder.makeString(len_ir, fill_ir);
    }

    fn compileMakeArray(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (make-array dimensions &optional initial-element)
        // dimensions can be a single fixnum or a quoted list of fixnums
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);

        var dimensions = std.ArrayList(*const Ir){};
        defer dimensions.deinit(self.allocator);
        var dynamic_dimensions: ?*const Ir = null;

        var dims_val = cons1.car;

        // If dims_val is (quote (2 3)), unwrap it
        const b = if (self.builtins) |val| val else return error.InvalidSyntax;
        if (dims_val.isCons()) {
            const quote_cons = dims_val.toPtr(Cons);
            if (quote_cons.car.raw == b.quote.raw and quote_cons.cdr.isCons()) {
                const inner_cons = quote_cons.cdr.toPtr(Cons);
                dims_val = inner_cons.car;
            }
        }

        // dims can be a fixnum, literal list of fixnums, or a dynamic expression.
        if (dims_val.isFixnum()) {
            const dim_ir = try self.compile(dims_val, env);
            try dimensions.append(self.allocator, dim_ir);
        } else if (dims_val.isCons()) {
            var current = dims_val;
            var all_fixnums = true;
            while (current.isCons()) {
                const dim_cons = current.toPtr(Cons);
                if (!dim_cons.car.isFixnum()) {
                    all_fixnums = false;
                    break;
                }
                current = dim_cons.cdr;
            }
            if (all_fixnums and current.isNil()) {
                current = dims_val;
                while (current.isCons()) {
                    const dim_cons = current.toPtr(Cons);
                    const dim_ir = try self.compile(dim_cons.car, env);
                    try dimensions.append(self.allocator, dim_ir);
                    current = dim_cons.cdr;
                }
            } else {
                dynamic_dimensions = try self.compile(dims_val, env);
            }
        } else {
            dynamic_dimensions = try self.compile(dims_val, env);
        }

        // Optional initial element - handle keyword args (:initial-element/:initial-contents)
        var init_ir: ?*const Ir = null;
        var initial_contents: ?Value = null;
        var rest = cons1.cdr;
        while (rest.isCons()) {
            const kv_cons = rest.toPtr(Cons);
            // Check if it's a keyword
            if (kv_cons.car.isKeyword()) {
                // Skip the keyword, get the value
                if (!kv_cons.cdr.isCons()) break;
                const val_cons = kv_cons.cdr.toPtr(Cons);
                // Check which keyword
                const kw_sym = kv_cons.car.toPtr(runtime.Keyword);
                const kw_name = kw_sym.getName();
                if (std.mem.eql(u8, kw_name, "initial-contents") or
                    std.mem.eql(u8, kw_name, "INITIAL-CONTENTS"))
                {
                    // Store raw value for post-construction fill
                    initial_contents = val_cons.car;
                } else if (std.mem.eql(u8, kw_name, "element-type") or
                    std.mem.eql(u8, kw_name, "ELEMENT-TYPE") or
                    std.mem.eql(u8, kw_name, "adjustable") or
                    std.mem.eql(u8, kw_name, "ADJUSTABLE") or
                    std.mem.eql(u8, kw_name, "fill-pointer") or
                    std.mem.eql(u8, kw_name, "FILL-POINTER"))
                {
                    // ignore for now
                } else {
                    init_ir = try self.compile(val_cons.car, env);
                }
                rest = val_cons.cdr;
            } else {
                // Non-keyword arg is the initial element (legacy support)
                init_ir = try self.compile(kv_cons.car, env);
                rest = kv_cons.cdr;
            }
        }

        // Handle :initial-contents: build (let ((a (make-array dim)) ...)  fill from contents)
        if (initial_contents) |contents_val| {
            // Build Lisp form: (%make-array-contents dim contents-expr)
            // and compile it as a regular call
            const h = self.heap orelse return error.InvalidSyntax;
            const helper_sym = try h.intern("%make-array-contents");
            const dim_val = cons1.car;
            const form = try h.allocCons(helper_sym, try h.allocCons(dim_val, try h.allocCons(contents_val, Value.nil)));
            return self.compile(form, env);
        }

        if (dynamic_dimensions) |dyn| {
            return try self.builder.arrNewDynamic(dyn, init_ir);
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

    fn compileSset(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (%sset string index char) - internal setter for (setf (char ...))
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const str_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const idx_ir = try self.compile(cons2.car, env);

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const char_ir = try self.compile(cons3.car, env);

        return try self.builder.strSet(str_ir, idx_ir, char_ir);
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

        if (value_ir == null) return error.InvalidSyntax;

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

    fn compileHashTableCapacity(self: *Compiler, args: Value, env: *const Env) anyerror!*Ir {
        // (hash-table-capacity hashtable)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const table_ir = try self.compile(cons.car, env);

        const node = try self.allocator.create(Ir);
        node.* = .{ .hash_capacity = .{ .operand = table_ir } };
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
        if (func_expr.isSymbol() and self.struct_predicates.count() > 0 and env.lookupFunctionSym(func_expr) == null) {
            // Copy name to avoid dangling pointer if heap moves
            const sym_name_raw = func_expr.toPtr(Symbol).getName();
            const sym_name = try self.allocator.dupe(u8, sym_name_raw);
            defer self.allocator.free(sym_name);
            if (self.struct_predicates.get(sym_name)) |struct_type| {
                // This is a struct predicate call - generate struct_p IR
                // Extract struct name from predicate (remove "-p" suffix)
                const struct_name = if (sym_name.len > 2 and
                    sym_name[sym_name.len - 2] == '-' and
                    (sym_name[sym_name.len - 1] == 'p' or sym_name[sym_name.len - 1] == 'P'))
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

        const func_ir = if (func_expr.isSymbol()) blk: {
            if (env.lookupFunctionSym(func_expr)) |binding| {
                const name = func_expr.toPtr(Symbol).getName();
                const fn_ref = try self.builder.variable(name, binding.depth, binding.index);
                break :blk try self.maybeBoxRefFunction(func_expr, fn_ref);
            }

            // Function position uses function namespace, not value namespace.
            // Emit symbol designator so VM resolves fdefinition/builtin wrappers.
            if (try self.compilePrimitiveFunctionRef(func_expr)) |wrapper| {
                break :blk wrapper;
            }
            break :blk try self.builder.lit(func_expr);
        } else try self.compile(func_expr, env);

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
        const items = try self.allocator.dupe(*const Ir, args.items);

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
        const inferred = try ctx.infer(ir_node, &type_env);

        // Solve constraints via unification
        try ctx.solve();

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

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
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

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    const result = try compiler.compile(Value.nil, &env);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(result.*));
    try testing.expect(result.lit.isNil());

    allocator.destroy(result);
}

test "compile lambda params" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    // (lambda (a (b fixnum) &rest r) a)
    const lambda_sym = try heap.intern("lambda");
    const a_sym = try heap.intern("a");
    const b_sym = try heap.intern("b");
    const fixnum_sym = try heap.intern("fixnum");
    const rest_kw = try heap.intern("&rest");
    const r_sym = try heap.intern("r");

    const b_typed = try heap.allocCons(b_sym, try heap.allocCons(fixnum_sym, Value.nil));
    const params = try heap.allocCons(a_sym, try heap.allocCons(b_typed, try heap.allocCons(rest_kw, try heap.allocCons(r_sym, Value.nil))));
    const body = try heap.allocCons(a_sym, Value.nil);
    const lambda_args = try heap.allocCons(params, body);
    const expr = try heap.allocCons(lambda_sym, lambda_args);

    const result = try compiler.compile(expr, &env);
    defer arena_alloc.destroy(result);

    try testing.expectEqual(Ir.lambda, std.meta.activeTag(result.*));
    try testing.expectEqual(@as(usize, 2), result.lambda.params.len);
    try testing.expectEqualStrings("A", result.lambda.params[0]);
    try testing.expectEqualStrings("B", result.lambda.params[1]);
    try testing.expect(result.lambda.optional_params.len == 0);
    try testing.expect(result.lambda.key_params.len == 0);
    try testing.expect(result.lambda.rest_param != null);
    try testing.expectEqualStrings("R", result.lambda.rest_param.?);
}

test "compile lambda invalid param" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const lambda_sym = try heap.intern("lambda");
    const params = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const body = try heap.allocCons(Value.makeFixnum(2), Value.nil);
    const lambda_args = try heap.allocCons(params, body);
    const expr = try heap.allocCons(lambda_sym, lambda_args);

    try testing.expectError(error.InvalidLambda, compiler.compile(expr, &env));
}

test "compile invalid if" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const if_sym = try heap.intern("if");
    const expr = try heap.allocCons(if_sym, Value.nil);

    try testing.expectError(error.InvalidIf, compiler.compile(expr, &env));
}

test "typeInfer type mismatch" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var builder = IrBuilder.init(arena_alloc);
    const str_val = try heap.allocBaseString("hi");
    const lit_str = try builder.lit(str_val);
    const lit_num = try builder.lit(Value.makeFixnum(1));
    const add_ir = try builder.add(lit_str, lit_num);

    try testing.expectError(error.TypeMismatch, compiler.typeInfer(add_ir));
}

test "compile setf long name" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    var name_buf: [600]u8 = undefined;
    @memset(&name_buf, 'a');
    const long_sym = try heap.intern(name_buf[0..]);

    const setf_name = try std.fmt.allocPrint(arena_alloc, "(setf {s})", .{long_sym.toPtr(Symbol).getName()});
    _ = try compiler.globals.define(setf_name);

    const setf_sym = try heap.intern("setf");
    const x_sym = try heap.intern("x");
    const place = try heap.allocCons(long_sym, try heap.allocCons(x_sym, Value.nil));
    const args = try heap.allocCons(place, try heap.allocCons(Value.makeFixnum(1), Value.nil));
    const expr = try heap.allocCons(setf_sym, args);

    const result = try compiler.compile(expr, &env);
    defer arena_alloc.destroy(result);
    try testing.expectEqual(Ir.call, std.meta.activeTag(result.*));
}

test "compile block return-from names" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const block_sym = try heap.intern("block");
    const return_from_sym = try heap.intern("return-from");

    // (block nil (return-from nil 1))
    const ret_args_nil = try heap.allocCons(Value.nil, try heap.allocCons(Value.makeFixnum(1), Value.nil));
    const ret_form_nil = try heap.allocCons(return_from_sym, ret_args_nil);
    const body_nil = try heap.allocCons(ret_form_nil, Value.nil);
    const block_args_nil = try heap.allocCons(Value.nil, body_nil);
    const expr_nil = try heap.allocCons(block_sym, block_args_nil);

    const ir_nil = try compiler.compile(expr_nil, &env);
    defer arena_alloc.destroy(ir_nil);
    try testing.expectEqual(Ir.block, std.meta.activeTag(ir_nil.*));
    try testing.expectEqual(Value.nil.raw, ir_nil.block.name.raw);
    try testing.expect(ir_nil.block.body.* == .return_from);
    try testing.expectEqual(Value.nil.raw, ir_nil.block.body.return_from.name.raw);

    // (block foo (return-from foo 2))
    const foo_sym = try heap.intern("foo");
    const ret_args_sym = try heap.allocCons(foo_sym, try heap.allocCons(Value.makeFixnum(2), Value.nil));
    const ret_form_sym = try heap.allocCons(return_from_sym, ret_args_sym);
    const body_sym = try heap.allocCons(ret_form_sym, Value.nil);
    const block_args_sym = try heap.allocCons(foo_sym, body_sym);
    const expr_sym = try heap.allocCons(block_sym, block_args_sym);

    const ir_sym = try compiler.compile(expr_sym, &env);
    defer arena_alloc.destroy(ir_sym);
    try testing.expectEqual(Ir.block, std.meta.activeTag(ir_sym.*));
    try testing.expectEqual(foo_sym.raw, ir_sym.block.name.raw);
    try testing.expect(ir_sym.block.body.* == .return_from);
    try testing.expectEqual(foo_sym.raw, ir_sym.block.body.return_from.name.raw);
}

test "compile or type assertions" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    const lambda_sym = try heap.intern("lambda");
    const the_sym = try heap.intern("the");
    const or_sym = try heap.intern("or");
    const fixnum_sym = try heap.intern("fixnum");
    const x_sym = try heap.intern("x");

    // (lambda ((x (or fixnum nil))) x)
    const or_list_param = try heap.allocCons(or_sym, try heap.allocCons(fixnum_sym, try heap.allocCons(Value.nil, Value.nil)));
    const typed_param = try heap.allocCons(x_sym, try heap.allocCons(or_list_param, Value.nil));
    const params = try heap.allocCons(typed_param, Value.nil);
    const body = try heap.allocCons(x_sym, Value.nil);
    const lambda_args = try heap.allocCons(params, body);
    const lambda_expr = try heap.allocCons(lambda_sym, lambda_args);

    var env_lambda = Env.init(arena_alloc, null);
    defer env_lambda.deinit();

    const ir_lambda = try compiler.compile(lambda_expr, &env_lambda);
    defer arena_alloc.destroy(ir_lambda);
    try testing.expectEqual(Ir.lambda, std.meta.activeTag(ir_lambda.*));
    try testing.expect(ir_lambda.lambda.body.* == .progn);
    try testing.expectEqual(@as(usize, 2), ir_lambda.lambda.body.progn.len);
    try testing.expect(ir_lambda.lambda.body.progn[0].* == .assert_or);

    // (the (or fixnum nil) x)
    const or_list_the = try heap.allocCons(or_sym, try heap.allocCons(fixnum_sym, try heap.allocCons(Value.nil, Value.nil)));
    const the_args = try heap.allocCons(or_list_the, try heap.allocCons(x_sym, Value.nil));
    const the_expr = try heap.allocCons(the_sym, the_args);

    var env_the = Env.init(arena_alloc, null);
    defer env_the.deinit();
    _ = try env_the.bindSym(x_sym);

    const ir_the = try compiler.compile(the_expr, &env_the);
    defer arena_alloc.destroy(ir_the);
    try testing.expect(ir_the.* == .assert_or);
}

test "env lookup" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var outer = Env.init(allocator, null);
    defer outer.deinit();
    _ = try outer.bindName("x");
    _ = try outer.bindName("y");

    var inner = Env.init(allocator, &outer);
    defer inner.deinit();
    _ = try inner.bindName("z");

    // z is at depth 0, index 0
    const z_lookup = inner.lookupName("z");
    try testing.expect(z_lookup != null);
    try testing.expectEqual(@as(u16, 0), z_lookup.?.depth);
    try testing.expectEqual(@as(u16, 0), z_lookup.?.index);

    // x is at depth 1, index 0
    const x_lookup = inner.lookupName("x");
    try testing.expect(x_lookup != null);
    try testing.expectEqual(@as(u16, 1), x_lookup.?.depth);
    try testing.expectEqual(@as(u16, 0), x_lookup.?.index);

    // w doesn't exist
    try testing.expect(inner.lookupName("w") == null);
}

test "env lookup distinguishes package symbols" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    const pkg1 = try heap.findOrCreatePackage("PKG1");
    const pkg2 = try heap.findOrCreatePackage("PKG2");
    const x1 = try pkg1.intern(&heap, "x");
    const x2 = try pkg2.intern(&heap, "x");
    try testing.expect(x1.raw != x2.raw);

    var env = Env.init(allocator, null);
    defer env.deinit();

    const idx1 = try env.bindSym(x1);
    const idx2 = try env.bindSym(x2);
    try testing.expect(idx1 != idx2);

    const b1_opt = env.lookupSym(x1);
    const b2_opt = env.lookupSym(x2);
    try testing.expect(b1_opt != null);
    try testing.expect(b2_opt != null);
    const b1 = b1_opt.?;
    const b2 = b2_opt.?;
    try testing.expectEqual(@as(u16, 0), b1.depth);
    try testing.expectEqual(idx1, b1.index);
    try testing.expectEqual(@as(u16, 0), b2.depth);
    try testing.expectEqual(idx2, b2.index);
}

test "env shadowing uses symbol identity" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    const pkg = try heap.findOrCreatePackage("PKG");
    const x = try pkg.intern(&heap, "x");

    var outer = Env.init(allocator, null);
    defer outer.deinit();
    _ = try outer.bindSym(x);

    var inner = Env.init(allocator, &outer);
    defer inner.deinit();
    const inner_idx = try inner.bindSym(x);

    const b_opt = inner.lookupSym(x);
    try testing.expect(b_opt != null);
    const b = b_opt.?;
    try testing.expectEqual(@as(u16, 0), b.depth);
    try testing.expectEqual(inner_idx, b.index);
}

test "type inference for literals" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
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

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
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

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // Create a simple body IR (literal fixnum)
    const body = try compiler.builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(body);

    // Empty typed params (untyped function)
    const typed_params = [_]Compiler.TypedParam{};

    // Check with no return type - should succeed (just infers)
    try compiler.checkLambdaTypes(&typed_params, null, body);

    // No errors expected
    try testing.expect(!compiler.hasBiCheckErrors());
}

test "BiChecker integration - checkLambdaTypes with type mismatch" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // Create body that returns a fixnum
    const body = try compiler.builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(body);

    // Empty typed params
    const typed_params = [_]Compiler.TypedParam{};

    // Expect string return type (but body returns fixnum)
    const string_sym = try heap.intern("string");
    try testing.expectError(error.TypeError, compiler.checkLambdaTypes(&typed_params, string_sym, body));

    // BiChecker should have recorded an error
    try testing.expect(compiler.hasBiCheckErrors());
}

test "match exhaustiveness uses variant symbol identity" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    _ = try heap.findOrCreatePackage("PKG1");
    _ = try heap.findOrCreatePackage("PKG2");
    const sym1 = (try heap.internInPackage("PKG1", "V")).?;
    const sym2 = (try heap.internInPackage("PKG2", "V")).?;

    const variants = try compiler.allocator.alloc(Compiler.Variant, 1);
    variants[0] = .{ .name = "V", .sym = sym1, .fields = &.{} };
    try compiler.defined_types.put("T", variants);

    const pattern_other = try heap.allocCons(sym2, Value.nil);
    const body_other = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const clause_other = try heap.allocCons(pattern_other, body_other);
    const clauses_other = try heap.allocCons(clause_other, Value.nil);
    const found_other = try compiler.checkMatchExhaustiveness(clauses_other);
    try testing.expect(found_other == null);

    const pattern_ok = try heap.allocCons(sym1, Value.nil);
    const body_ok = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const clause_ok = try heap.allocCons(pattern_ok, body_ok);
    const clauses_ok = try heap.allocCons(clause_ok, Value.nil);
    const found_ok = try compiler.checkMatchExhaustiveness(clauses_ok);
    try testing.expectEqual(@as(?usize, 1), found_ok);

    _ = compiler.defined_types.remove("T");
    compiler.allocator.free(variants);
}

test "declare - type declaration" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // Build (declare (type fixnum x y))
    const type_sym = try heap.intern("type");
    const fixnum_sym = try heap.intern("fixnum");
    const x_sym = try heap.intern("x");
    const y_sym = try heap.intern("y");

    // (fixnum x y)
    const vars = try heap.allocCons(y_sym, Value.nil);
    const type_args = try heap.allocCons(x_sym, vars);
    const type_spec = try heap.allocCons(fixnum_sym, type_args);
    const decl_spec = try heap.allocCons(type_sym, type_spec);
    const args = try heap.allocCons(decl_spec, Value.nil);

    const result = try compiler.compileDeclare(args);
    defer allocator.destroy(result);

    // Should return nil
    try testing.expect(result.* == .lit);
    try testing.expect(result.lit.isNil());

    // Check that declarations were recorded
    try testing.expect(compiler.global_decls.hasDecl("X", .type_decl));
    try testing.expect(compiler.global_decls.hasDecl("Y", .type_decl));
    // TEMP: getTypeDecl disabled due to HashMap corruption bug
    // const x_type = compiler.global_decls.getTypeDecl("x");
    // try testing.expect(x_type != null);
    // try testing.expect(x_type.?.eq(fixnum_sym));
}

test "declare - ignore declaration" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // Build (declare (ignore x y))
    const ignore_sym = try heap.intern("ignore");
    const x_sym = try heap.intern("x");
    const y_sym = try heap.intern("y");

    const vars = try heap.allocCons(y_sym, Value.nil);
    const ignore_args = try heap.allocCons(x_sym, vars);
    const decl_spec = try heap.allocCons(ignore_sym, ignore_args);
    const args = try heap.allocCons(decl_spec, Value.nil);

    const result = try compiler.compileDeclare(args);
    defer allocator.destroy(result);

    // Should return nil
    try testing.expect(result.* == .lit);
    try testing.expect(result.lit.isNil());

    // Check that declarations were recorded
    try testing.expect(compiler.global_decls.hasDecl("X", .ignore));
    try testing.expect(compiler.global_decls.hasDecl("Y", .ignore));
}

test "declare - multiple declaration specs" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // Build (declare (type fixnum x) (ignore y))
    const type_sym = try heap.intern("type");
    const ignore_sym = try heap.intern("ignore");
    const fixnum_sym = try heap.intern("fixnum");
    const x_sym = try heap.intern("x");
    const y_sym = try heap.intern("y");

    // (type fixnum x)
    const x_list = try heap.allocCons(x_sym, Value.nil);
    const type_spec = try heap.allocCons(fixnum_sym, x_list);
    const type_decl = try heap.allocCons(type_sym, type_spec);

    // (ignore y)
    const y_list = try heap.allocCons(y_sym, Value.nil);
    const ignore_decl = try heap.allocCons(ignore_sym, y_list);

    // (decl1 decl2)
    const specs = try heap.allocCons(ignore_decl, Value.nil);
    const args = try heap.allocCons(type_decl, specs);

    const result = try compiler.compileDeclare(args);
    defer allocator.destroy(result);

    // Should return nil
    try testing.expect(result.* == .lit);
    try testing.expect(result.lit.isNil());

    // Check both declarations
    try testing.expect(compiler.global_decls.hasDecl("X", .type_decl));
    try testing.expect(compiler.global_decls.hasDecl("Y", .ignore));
}

test "declaim - declaration spec is recorded" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    const declaration_sym = try heap.intern("declaration");
    const foo_sym = try heap.intern("foo");

    const names = try heap.allocCons(foo_sym, Value.nil);
    const spec = try heap.allocCons(declaration_sym, names);
    const args = try heap.allocCons(spec, Value.nil);

    const result = try compiler.compileDeclaim(args);
    defer allocator.destroy(result);

    try testing.expect(result.* == .lit);
    try testing.expect(result.lit.isNil());
    try testing.expect(compiler.global_decls.hasDecl("FOO", .declaration));
}

test "declare - optimize updates current settings" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    const optimize_sym = try heap.intern("optimize");
    const safety_sym = try heap.intern("safety");
    const speed_sym = try heap.intern("speed");
    const debug_sym = try heap.intern("debug");
    const space_sym = try heap.intern("space");
    const comp_speed_sym = try heap.intern("compilation-speed");

    const safety_spec = try heap.allocCons(safety_sym, try heap.allocCons(Value.makeFixnum(0), Value.nil));
    const debug_spec = try heap.allocCons(debug_sym, try heap.allocCons(Value.makeFixnum(2), Value.nil));
    const space_spec = try heap.allocCons(space_sym, try heap.allocCons(Value.makeFixnum(1), Value.nil));
    const comp_speed_spec = try heap.allocCons(comp_speed_sym, try heap.allocCons(Value.makeFixnum(3), Value.nil));
    const optimize_specs = try heap.allocCons(safety_spec, try heap.allocCons(speed_sym, try heap.allocCons(debug_spec, try heap.allocCons(space_spec, try heap.allocCons(comp_speed_spec, Value.nil)))));
    const optimize_decl = try heap.allocCons(optimize_sym, optimize_specs);
    const args = try heap.allocCons(optimize_decl, Value.nil);

    const result = try compiler.compileDeclare(args);
    defer allocator.destroy(result);
    try testing.expect(result.* == .lit);
    try testing.expect(result.lit.isNil());
    try testing.expectEqual(@as(u8, 0), compiler.optimize_current.safety);
    try testing.expectEqual(@as(u8, 3), compiler.optimize_current.speed);
    try testing.expectEqual(@as(u8, 2), compiler.optimize_current.debug);
    try testing.expectEqual(@as(u8, 1), compiler.optimize_current.space);
    try testing.expectEqual(@as(u8, 3), compiler.optimize_current.compilation_speed);
}

test "declaim/proclaim optimize update global settings" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    const optimize_sym = try heap.intern("optimize");
    const safety_sym = try heap.intern("safety");
    const speed_sym = try heap.intern("speed");
    const debug_sym = try heap.intern("debug");
    const quote_sym = try heap.intern("quote");

    const declaim_opt = try heap.allocCons(optimize_sym, try heap.allocCons(try heap.allocCons(safety_sym, try heap.allocCons(Value.makeFixnum(2), Value.nil)), try heap.allocCons(try heap.allocCons(speed_sym, try heap.allocCons(Value.makeFixnum(1), Value.nil)), Value.nil)));
    const declaim_args = try heap.allocCons(declaim_opt, Value.nil);
    const declaim_result = try compiler.compileDeclaim(declaim_args);
    defer allocator.destroy(declaim_result);
    try testing.expectEqual(@as(u8, 2), compiler.optimize_global.safety);
    try testing.expectEqual(@as(u8, 1), compiler.optimize_global.speed);

    const proclaim_opt = try heap.allocCons(optimize_sym, try heap.allocCons(try heap.allocCons(safety_sym, try heap.allocCons(Value.makeFixnum(0), Value.nil)), try heap.allocCons(try heap.allocCons(debug_sym, try heap.allocCons(Value.makeFixnum(3), Value.nil)), Value.nil)));
    const quoted = try heap.allocCons(quote_sym, try heap.allocCons(proclaim_opt, Value.nil));
    const proclaim_args = try heap.allocCons(quoted, Value.nil);
    var env = Env.init(allocator, null);
    defer env.deinit();
    const proclaim_result = try compiler.compileProclaim(proclaim_args, &env);
    defer allocator.destroy(proclaim_result);
    try testing.expectEqual(@as(u8, 0), compiler.optimize_global.safety);
    try testing.expectEqual(@as(u8, 3), compiler.optimize_global.debug);
}

test "parseDestructParams - simple parameters" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // (a b c)
    const a = try heap.intern("a");
    const b = try heap.intern("b");
    const c = try heap.intern("c");
    const params = try heap.allocCons(a, try heap.allocCons(b, try heap.allocCons(c, Value.nil)));

    const result = try compiler.parseDestructParams(params);
    defer {
        for (result) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(result);
    }

    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqual(Compiler.DestructParam.Kind.simple, result[0].kind);
    try testing.expectEqualStrings("A", result[0].name.?);
    try testing.expectEqual(Compiler.DestructParam.Kind.simple, result[1].kind);
    try testing.expectEqualStrings("B", result[1].name.?);
    try testing.expectEqual(Compiler.DestructParam.Kind.simple, result[2].kind);
    try testing.expectEqualStrings("C", result[2].name.?);
}

test "parseDestructParams - nested lists" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // ((a b) c)
    const a = try heap.intern("a");
    const b = try heap.intern("b");
    const c = try heap.intern("c");
    const nested = try heap.allocCons(a, try heap.allocCons(b, Value.nil));
    const params = try heap.allocCons(nested, try heap.allocCons(c, Value.nil));

    const result = try compiler.parseDestructParams(params);
    defer {
        for (result) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(result);
    }

    try testing.expectEqual(@as(usize, 2), result.len);
    try testing.expectEqual(Compiler.DestructParam.Kind.nested, result[0].kind);
    try testing.expect(result[0].children != null);
    try testing.expectEqual(@as(usize, 2), result[0].children.?.len);
    try testing.expectEqualStrings("A", result[0].children.?[0].name.?);
    try testing.expectEqualStrings("B", result[0].children.?[1].name.?);
    try testing.expectEqual(Compiler.DestructParam.Kind.simple, result[1].kind);
    try testing.expectEqualStrings("C", result[1].name.?);
}

test "parseDestructParams - optional parameters" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // (a &optional b (c 10))
    const a = try heap.intern("a");
    const opt_kw = try heap.intern("&optional");
    const b = try heap.intern("b");
    const c = try heap.intern("c");
    const ten = Value.makeFixnum(10);
    const c_with_default = try heap.allocCons(c, try heap.allocCons(ten, Value.nil));
    const params = try heap.allocCons(a, try heap.allocCons(opt_kw, try heap.allocCons(b, try heap.allocCons(c_with_default, Value.nil))));

    const result = try compiler.parseDestructParams(params);
    defer {
        for (result) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(result);
    }

    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqual(Compiler.DestructParam.Kind.simple, result[0].kind);
    try testing.expectEqualStrings("A", result[0].name.?);
    try testing.expectEqual(Compiler.DestructParam.Kind.optional, result[1].kind);
    try testing.expectEqualStrings("B", result[1].name.?);
    try testing.expect(result[1].default_expr == null);
    try testing.expectEqual(Compiler.DestructParam.Kind.optional, result[2].kind);
    try testing.expectEqualStrings("C", result[2].name.?);
    try testing.expect(result[2].default_expr != null);
    try testing.expect(result[2].default_expr.?.isFixnum());
}

test "parseDestructParams - rest parameter" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // (a b &rest c)
    const a = try heap.intern("a");
    const b = try heap.intern("b");
    const rest_kw = try heap.intern("&rest");
    const c = try heap.intern("c");
    const params = try heap.allocCons(a, try heap.allocCons(b, try heap.allocCons(rest_kw, try heap.allocCons(c, Value.nil))));

    const result = try compiler.parseDestructParams(params);
    defer {
        for (result) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(result);
    }

    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqual(Compiler.DestructParam.Kind.simple, result[0].kind);
    try testing.expectEqual(Compiler.DestructParam.Kind.simple, result[1].kind);
    try testing.expectEqual(Compiler.DestructParam.Kind.rest, result[2].kind);
    try testing.expectEqualStrings("C", result[2].name.?);
}

test "parseDestructParams - key parameters" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // (a &key b (c 20))
    const a = try heap.intern("a");
    const key_kw = try heap.intern("&key");
    const b = try heap.intern("b");
    const c = try heap.intern("c");
    const twenty = Value.makeFixnum(20);
    const c_with_default = try heap.allocCons(c, try heap.allocCons(twenty, Value.nil));
    const params = try heap.allocCons(a, try heap.allocCons(key_kw, try heap.allocCons(b, try heap.allocCons(c_with_default, Value.nil))));

    const result = try compiler.parseDestructParams(params);
    defer {
        for (result) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(result);
    }

    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqual(Compiler.DestructParam.Kind.simple, result[0].kind);
    try testing.expectEqual(Compiler.DestructParam.Kind.key, result[1].kind);
    try testing.expectEqualStrings("B", result[1].name.?);
    try testing.expectEqual(Compiler.DestructParam.Kind.key, result[2].kind);
    try testing.expectEqualStrings("C", result[2].name.?);
    try testing.expect(result[2].default_expr != null);
}

test "parseDestructParams - complex nested with keywords" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // ((a b) &optional c &rest d)
    const a = try heap.intern("a");
    const b = try heap.intern("b");
    const c = try heap.intern("c");
    const d = try heap.intern("d");
    const opt_kw = try heap.intern("&optional");
    const rest_kw = try heap.intern("&rest");
    const nested = try heap.allocCons(a, try heap.allocCons(b, Value.nil));
    const params = try heap.allocCons(nested, try heap.allocCons(opt_kw, try heap.allocCons(c, try heap.allocCons(rest_kw, try heap.allocCons(d, Value.nil)))));

    const result = try compiler.parseDestructParams(params);
    defer {
        for (result) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(result);
    }

    try testing.expectEqual(@as(usize, 3), result.len);
    try testing.expectEqual(Compiler.DestructParam.Kind.nested, result[0].kind);
    try testing.expect(result[0].children != null);
    try testing.expectEqual(@as(usize, 2), result[0].children.?.len);
    try testing.expectEqual(Compiler.DestructParam.Kind.optional, result[1].kind);
    try testing.expectEqualStrings("C", result[1].name.?);
    try testing.expectEqual(Compiler.DestructParam.Kind.rest, result[2].kind);
    try testing.expectEqualStrings("D", result[2].name.?);
}

test "genDestructCode - simple parameters" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // Parse (a b) and generate code to extract from args
    const a = try heap.intern("a");
    const b = try heap.intern("b");
    const params_val = try heap.allocCons(a, try heap.allocCons(b, Value.nil));
    const params = try compiler.parseDestructParams(params_val);
    defer {
        for (params) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(params);
    }

    // Create args_ir to destructure from
    var env = Env.init(allocator, null);
    defer env.deinit();

    const args_ir = try compiler.builder.lit(Value.nil);
    defer allocator.destroy(args_ir);

    var result = try compiler.genDestructCode(params, args_ir, &env);
    defer result.deinit(allocator);
    defer {
        for (result.bindings.items) |binding| {
            // Don't free if it's the input args_ir or already in intermediates
            var is_ref = (binding.init == args_ir);
            for (result.intermediates.items) |node| {
                if (binding.init == node) is_ref = true;
            }
            if (!is_ref) allocator.destroy(binding.init);
        }
        for (result.intermediates.items) |node| {
            allocator.destroy(node);
        }
    }

    try testing.expectEqual(@as(usize, 2), result.bindings.items.len);
    try testing.expectEqualStrings("A", result.bindings.items[0].name);
    try testing.expectEqualStrings("B", result.bindings.items[1].name);
    // Check IR nodes are car/cdr operations
    try testing.expect(result.bindings.items[0].init.* == .car);
    try testing.expect(result.bindings.items[1].init.* == .car);
}

test "genDestructCode - nested parameters" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // Parse ((a b) c) - nested destructuring
    const a = try heap.intern("a");
    const b = try heap.intern("b");
    const c = try heap.intern("c");
    const nested = try heap.allocCons(a, try heap.allocCons(b, Value.nil));
    const params_val = try heap.allocCons(nested, try heap.allocCons(c, Value.nil));
    const params = try compiler.parseDestructParams(params_val);
    defer {
        for (params) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(params);
    }

    var env = Env.init(allocator, null);
    defer env.deinit();

    const args_ir = try compiler.builder.lit(Value.nil);
    defer allocator.destroy(args_ir);
    var result = try compiler.genDestructCode(params, args_ir, &env);
    defer result.deinit(allocator);
    defer {
        for (result.bindings.items) |binding| {
            var is_ref = (binding.init == args_ir);
            for (result.intermediates.items) |node| {
                if (binding.init == node) is_ref = true;
            }
            if (!is_ref) allocator.destroy(binding.init);
        }
        for (result.intermediates.items) |node| {
            allocator.destroy(node);
        }
    }

    try testing.expectEqual(@as(usize, 3), result.bindings.items.len);
    try testing.expectEqualStrings("A", result.bindings.items[0].name);
    try testing.expectEqualStrings("B", result.bindings.items[1].name);
    try testing.expectEqualStrings("C", result.bindings.items[2].name);
}

test "genDestructCode - rest parameter" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // Parse (a &rest b)
    const a = try heap.intern("a");
    const rest_kw = try heap.intern("&rest");
    const b = try heap.intern("b");
    const params_val = try heap.allocCons(a, try heap.allocCons(rest_kw, try heap.allocCons(b, Value.nil)));
    const params = try compiler.parseDestructParams(params_val);
    defer {
        for (params) |*p| {
            var mut_p = p.*;
            mut_p.deinit(allocator);
        }
        allocator.free(params);
    }

    var env = Env.init(allocator, null);
    defer env.deinit();

    const args_ir = try compiler.builder.lit(Value.nil);
    defer allocator.destroy(args_ir);
    var result = try compiler.genDestructCode(params, args_ir, &env);
    defer result.deinit(allocator);
    defer {
        for (result.bindings.items) |binding| {
            // Don't free if it's the input args_ir or already in intermediates
            var is_ref = (binding.init == args_ir);
            for (result.intermediates.items) |node| {
                if (binding.init == node) is_ref = true;
            }
            if (!is_ref) allocator.destroy(binding.init);
        }
        for (result.intermediates.items) |node| {
            allocator.destroy(node);
        }
    }

    try testing.expectEqual(@as(usize, 2), result.bindings.items.len);
    try testing.expectEqualStrings("A", result.bindings.items[0].name);
    try testing.expectEqualStrings("B", result.bindings.items[1].name);
    // First is car, second is rest (no car)
    try testing.expect(result.bindings.items[0].init.* == .car);
}

test "defmacro with destructured params" {
    const testing = std.testing;
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    // (defmacro test-m ((a b)) (list a b))
    const a_sym = try heap.intern("a");
    const b_sym = try heap.intern("b");
    const param_list = try heap.allocCons(a_sym, try heap.allocCons(b_sym, Value.nil));
    const params = try heap.allocCons(param_list, Value.nil);

    const list_sym = try heap.intern("list");
    const body = try heap.allocCons(list_sym, try heap.allocCons(a_sym, try heap.allocCons(b_sym, Value.nil)));

    const name_sym = try heap.intern("test-m");
    const lambda_args = try heap.allocCons(params, try heap.allocCons(body, Value.nil));
    const args = try heap.allocCons(name_sym, lambda_args);

    const defmacro_ir = try compiler.compileDefmacro(args, &env);
    defer allocator.destroy(defmacro_ir);
    try testing.expect(defmacro_ir.* == .lit);

    // Macro should be in table with transformed params
    const stored = compiler.macro_table.get(name_sym).?;
    try testing.expect(stored.isCons());

    // First element should be params (now transformed with gensym)
    const stored_cons = stored.toPtr(Cons);
    const stored_params = stored_cons.car;
    try testing.expect(stored_params.isCons());
}

test "macro expansion restores VM chunk_pool" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    const start_pool_len = vm.chunk_pool.len;

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    errdefer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    const m_sym = try heap.intern("m");
    const body = try heap.allocCons(Value.makeFixnum(42), Value.nil);
    const macro_def = try heap.allocCons(Value.nil, body);
    try compiler.macro_table.put(m_sym, macro_def);

    const call_expr = try heap.allocCons(m_sym, Value.nil);
    const ir_node = try compiler.compile(call_expr, &env);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(ir_node.*));
    try testing.expectEqual(@as(i64, 42), ir_node.lit.toFixnum());
    allocator.destroy(ir_node);

    compiler.deinit();
    try testing.expectEqual(start_pool_len, vm.chunk_pool.len);
    _ = try vm.collectGarbage();
}

test "macro expansion restores VM global_env" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var other_env = GlobalEnv.init(allocator);
    defer other_env.deinit();
    vm.setGlobalEnv(&other_env);
    const saved_env = vm.global_env;

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    errdefer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    const m_sym = try heap.intern("m");
    const body = try heap.allocCons(Value.makeFixnum(42), Value.nil);
    const macro_def = try heap.allocCons(Value.nil, body);
    try compiler.macro_table.put(m_sym, macro_def);

    const call_expr = try heap.allocCons(m_sym, Value.nil);
    const ir_node = try compiler.compile(call_expr, &env);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(ir_node.*));
    try testing.expectEqual(@as(i64, 42), ir_node.lit.toFixnum());
    allocator.destroy(ir_node);

    compiler.deinit();
    try testing.expect(vm.global_env == saved_env);
    _ = try vm.collectGarbage();
}

test "load-time-value restores VM chunk_pool" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    const start_pool_len = vm.chunk_pool.len;

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    errdefer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    const ltv_sym = try heap.intern("load-time-value");
    const args = try heap.allocCons(Value.makeFixnum(42), Value.nil);
    const expr = try heap.allocCons(ltv_sym, args);
    const ir_node = try compiler.compile(expr, &env);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(ir_node.*));
    try testing.expectEqual(@as(i64, 42), ir_node.lit.toFixnum());
    allocator.destroy(ir_node);

    compiler.deinit();
    try testing.expectEqual(start_pool_len, vm.chunk_pool.len);
    _ = try vm.collectGarbage();
}

test "transformDestructuredParams nested" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();

    // (((a b) c) c)
    const a_sym = try heap.intern("a");
    const b_sym = try heap.intern("b");
    const c_sym = try heap.intern("c");
    const nested = try heap.allocCons(a_sym, try heap.allocCons(b_sym, Value.nil));
    const destruct_param = try heap.allocCons(nested, Value.nil);
    const params = try heap.allocCons(destruct_param, try heap.allocCons(c_sym, Value.nil));
    const body = try heap.allocCons(c_sym, Value.nil);
    const lambda_args = try heap.allocCons(params, body);

    const transformed = try compiler.transformDestructuredParams(lambda_args);
    try testing.expect(transformed.isCons());

    const trans_cons = transformed.toPtr(Cons);
    const new_params = trans_cons.car;
    const wrapped_body = trans_cons.cdr;

    // New params: (g c)
    try testing.expect(new_params.isCons());
    const new_params_cons = new_params.toPtr(Cons);
    const g_sym = new_params_cons.car;
    try testing.expect(g_sym.isSymbol());
    const g_name = g_sym.toPtr(Symbol).getName();
    try testing.expect(g_name.len > 0);
    try testing.expectEqual(@as(u8, 'G'), g_name[0]);
    try testing.expect(new_params_cons.cdr.isCons());
    try testing.expect(new_params_cons.cdr.toPtr(Cons).car.eq(c_sym));

    // Wrapped body starts with (destructuring-bind pattern g ...)
    try testing.expect(wrapped_body.isCons());
    const wrapped_cons = wrapped_body.toPtr(Cons);
    const db_form = wrapped_cons.car;
    try testing.expect(db_form.isCons());
    const db_cons = db_form.toPtr(Cons);
    try testing.expect(db_cons.car.isSymbol());
    try testing.expectEqualStrings("DESTRUCTURING-BIND", db_cons.car.toPtr(Symbol).getName());

    const db_args = db_cons.cdr.toPtr(Cons);
    const pattern = db_args.car;
    try testing.expect(pattern.isCons());
    const pattern_cons = pattern.toPtr(Cons);
    try testing.expect(pattern_cons.car.isCons());
    const inner = pattern_cons.car.toPtr(Cons);
    try testing.expect(inner.car.eq(a_sym));
    try testing.expect(inner.cdr.isCons());
    try testing.expect(inner.cdr.toPtr(Cons).car.eq(b_sym));

    const g_arg = db_args.cdr.toPtr(Cons).car;
    try testing.expect(g_arg.eq(g_sym));
}

test "genDestructBindings nested" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    const a_sym = try heap.intern("a");
    const b_sym = try heap.intern("b");
    const pattern = try heap.allocCons(a_sym, try heap.allocCons(b_sym, Value.nil));

    var bindings = try compiler.genDestructBindings(pattern, "#destruct-temp", 0);
    defer bindings.deinit(arena_alloc);

    try testing.expectEqual(@as(usize, 2), bindings.items.len);
    try testing.expectEqualStrings("A", bindings.items[0].name);
    try testing.expectEqualStrings("B", bindings.items[1].name);
    try testing.expect(bindings.items[0].init.* == .car);
    try testing.expect(bindings.items[1].init.* == .car);
}

test "compile defun typed name" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const defun_sym = try heap.intern("defun");
    const foo_sym = try heap.intern("foo");
    const arrow_sym = try heap.intern("->");
    const fixnum_sym = try heap.intern("fixnum");
    const x_sym = try heap.intern("x");

    const name_spec = try heap.allocCons(foo_sym, try heap.allocCons(arrow_sym, try heap.allocCons(fixnum_sym, Value.nil)));
    const params = try heap.allocCons(x_sym, Value.nil);
    const body = try heap.allocCons(x_sym, Value.nil);
    const defun_args = try heap.allocCons(name_spec, try heap.allocCons(params, body));
    const expr = try heap.allocCons(defun_sym, defun_args);

    const ir_def = try compiler.compile(expr, &env);
    defer arena_alloc.destroy(ir_def);

    try testing.expectEqual(Ir.define, std.meta.activeTag(ir_def.*));
    try testing.expectEqualStrings("CL-USER:FOO", ir_def.define.name);
    try testing.expect(ir_def.define.value.* == .lambda);
    try testing.expect(ir_def.define.value.lambda.body.* == .assert_fixnum);
}

test "compile defun sets lambda name" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const defun_sym = try heap.intern("defun");
    const foo_sym = try heap.intern("foo");
    const x_sym = try heap.intern("x");

    const params = try heap.allocCons(x_sym, Value.nil);
    const body = try heap.allocCons(x_sym, Value.nil);
    const defun_args = try heap.allocCons(foo_sym, try heap.allocCons(params, body));
    const expr = try heap.allocCons(defun_sym, defun_args);

    const ir_def = try compiler.compile(expr, &env);
    defer arena_alloc.destroy(ir_def);

    try testing.expect(ir_def.define.value.* == .lambda);
    try testing.expectEqual(foo_sym.raw, ir_def.define.value.lambda.name.raw);
}

test "compile defpackage names" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    const pkg_str = try heap.allocBaseString("foo");
    const args_str = try heap.allocCons(pkg_str, Value.nil);
    const ir_str = try compiler.compileDefpackage(args_str);
    defer arena_alloc.destroy(ir_str);

    try testing.expect(ir_str.* == .lit);
    try testing.expect(ir_str.lit.isSymbol());
    try testing.expectEqualStrings("FOO", ir_str.lit.toPtr(Symbol).getName());

    const bar_sym = try heap.intern("bar");
    const args_sym = try heap.allocCons(bar_sym, Value.nil);
    const ir_sym = try compiler.compileDefpackage(args_sym);
    defer arena_alloc.destroy(ir_sym);

    try testing.expect(ir_sym.* == .lit);
    try testing.expect(ir_sym.lit.isSymbol());
    try testing.expectEqualStrings("BAR", ir_sym.lit.toPtr(Symbol).getName());
}

test "parseTypeExpr function type" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    const function_sym = try heap.intern("function");
    const fixnum_sym = try heap.intern("fixnum");

    // (function (fixnum) fixnum)
    const arg_list = try heap.allocCons(fixnum_sym, Value.nil);
    const args = try heap.allocCons(arg_list, try heap.allocCons(fixnum_sym, Value.nil));
    const type_expr = try heap.allocCons(function_sym, args);

    const ty1 = try compiler.parseTypeExpr(type_expr);
    try testing.expect(ty1 != null);
    try testing.expect(std.meta.activeTag(ty1.?.*) == .arrow);

    // (function () fixnum)
    const args_nil = try heap.allocCons(Value.nil, try heap.allocCons(fixnum_sym, Value.nil));
    const type_expr2 = try heap.allocCons(function_sym, args_nil);

    const ty2 = try compiler.parseTypeExpr(type_expr2);
    try testing.expect(ty2 != null);
    try testing.expect(std.meta.activeTag(ty2.?.*) == .arrow);
}

test "compile defstruct typed slot" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const foo_sym = try heap.intern("foo");
    const bar_sym = try heap.intern("bar");
    const fixnum_sym = try heap.intern("fixnum");

    const slot_spec = try heap.allocCons(bar_sym, try heap.allocCons(fixnum_sym, Value.nil));
    const args = try heap.allocCons(foo_sym, try heap.allocCons(slot_spec, Value.nil));

    const ir_def = try compiler.compileDefstruct(args, &env);
    defer arena_alloc.destroy(ir_def);

    try testing.expectEqual(Ir.progn, std.meta.activeTag(ir_def.*));
    try testing.expect(ir_def.progn.len > 0);
    const last = ir_def.progn[ir_def.progn.len - 1];
    try testing.expect(last.* == .lit);
    try testing.expect(last.lit.eq(foo_sym));
}

test "compile defclass slot list" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const class_sym = try heap.intern("foo");
    const bar_sym = try heap.intern("bar");

    const slot_spec = try heap.allocCons(bar_sym, Value.nil);
    const slot_list = try heap.allocCons(slot_spec, Value.nil);
    const args = try heap.allocCons(class_sym, try heap.allocCons(Value.nil, try heap.allocCons(slot_list, Value.nil)));

    const ir_def = try compiler.compileDefclass(args, &env);
    defer arena_alloc.destroy(ir_def);

    try testing.expectEqual(Ir.progn, std.meta.activeTag(ir_def.*));
    try testing.expect(ir_def.progn.len > 0);
    const last = ir_def.progn[ir_def.progn.len - 1];
    try testing.expect(last.* == .lit);
    try testing.expect(last.lit.eq(class_sym));
}

test "compile defmethod specialized param" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const defmethod_sym = try heap.intern("defmethod");
    const foo_sym = try heap.intern("foo");
    const x_sym = try heap.intern("x");
    const bar_sym = try heap.intern("bar");

    const spec_param = try heap.allocCons(x_sym, try heap.allocCons(bar_sym, Value.nil));
    const lambda_list = try heap.allocCons(spec_param, Value.nil);
    const body = try heap.allocCons(x_sym, Value.nil);
    const args = try heap.allocCons(foo_sym, try heap.allocCons(lambda_list, body));
    const expr = try heap.allocCons(defmethod_sym, args);

    const ir_def = try compiler.compile(expr, &env);
    defer arena_alloc.destroy(ir_def);

    try testing.expect(ir_def.* == .progn);
    try testing.expect(ir_def.progn.len >= 1);

    var found = false;
    for (ir_def.progn) |node| {
        if (node.* != .define) continue;
        if (std.mem.eql(u8, node.define.name, "FOO$p$BAR")) {
            found = true;
            break;
        }
    }
    try testing.expect(found);
}

test "compiler qualifyName allocates for long names" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    const long_pkg = "THIS-IS-A-VERY-LONG-PACKAGE-NAME";
    const pkg = try heap.findOrCreatePackage(long_pkg);
    heap.setCurrentPackage(pkg);

    var buf: [4]u8 = undefined;
    const q = try compiler.qualifyName("FOO", &buf);
    defer if (q.owned) compiler.allocator.free(q.name);

    const expected = try std.fmt.allocPrint(allocator, "{s}:FOO", .{pkg.name});
    defer allocator.free(expected);

    try testing.expect(q.owned);
    try testing.expectEqualStrings(expected, q.name);
}

test "parser resolves list symbol to builtin" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var parser = try Parser.init(allocator, &heap, "(list 1 2)", &vm.builtins);
    defer parser.deinit();

    const expr = try parser.parse();
    try testing.expect(expr.isCons());
    const head = expr.toPtr(Cons).car;
    try testing.expect(head.isSymbol());

    var compiler = try Compiler.initWithHeap(allocator, &vm);
    defer compiler.deinit();
    const b = compiler.builtins.?;
    try testing.expectEqual(b.list.raw, head.raw);
}

test "compile list and listen use intrinsic IR" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var compiler = try Compiler.initWithHeap(arena_alloc, &vm);
    defer compiler.deinit();

    {
        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        var parser = try Parser.init(arena_alloc, &heap, "(list 1 2)", &vm.builtins);
        defer parser.deinit();
        const expr = try parser.parse();
        const ir_node = try compiler.compile(expr, &env);
        const is_list = switch (ir_node.*) {
            .list => true,
            else => false,
        };
        try testing.expect(is_list);
    }

    {
        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        var parser = try Parser.init(arena_alloc, &heap, "(listen (make-string-input-stream \"a\"))", &vm.builtins);
        defer parser.deinit();
        const expr = try parser.parse();
        const ir_node = try compiler.compile(expr, &env);
        const is_listen = switch (ir_node.*) {
            .listen => true,
            else => false,
        };
        try testing.expect(is_listen);
    }
}
