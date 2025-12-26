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
const types = @import("../types/types.zig");
const Type = types.Type;
const TypeEnv = types.TypeEnv;
const OccurrenceCtx = types.OccurrenceCtx;
const TypeChecker = types.TypeChecker;

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
    define: Value,
    defun: Value,
    @"set!": Value,

    // Sequencing
    progn: Value,
    begin: Value,
    @"while": Value,

    // Quoting
    quote: Value,
    quasiquote: Value,
    unquote: Value,
    @"unquote-splicing": Value,

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
            .define = heap.intern("define") orelse return null,
            .defun = heap.intern("defun") orelse return null,
            .@"set!" = heap.intern("set!") orelse return null,
            .progn = heap.intern("progn") orelse return null,
            .begin = heap.intern("begin") orelse return null,
            .@"while" = heap.intern("while") orelse return null,
            .quote = heap.intern("quote") orelse return null,
            .quasiquote = heap.intern("quasiquote") orelse return null,
            .unquote = heap.intern("unquote") orelse return null,
            .@"unquote-splicing" = heap.intern("unquote-splicing") orelse return null,
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
        self.bindings.deinit();
    }

    /// Define a global, returns its index
    pub fn define(self: *GlobalEnv, name: []const u8) !u16 {
        if (self.bindings.get(name)) |idx| {
            return idx; // Already defined, return existing index
        }
        const idx = self.next_index;
        try self.bindings.put(name, idx);
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

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .type_checker = TypeChecker.init(allocator),
            .type_checking_enabled = false, // Off by default for gradual typing
            .globals = GlobalEnv.init(allocator),
            .builtins = null, // Lazily initialized when heap is available
            .occ = null,
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
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.type_checker.deinit();
        self.globals.deinit();
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
            .lit => |val| {
                if (val.isNil()) return &types.t_nil;
                if (val.isFixnum()) return &types.t_fixnum;
                if (val.isString()) return &types.t_string;
                if (val.isSymbol()) return &types.t_symbol;
                if (val.isCons()) return &types.t_cons;
                return &types.t_any;
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
        // nilp: if nil then x is nil, else x is non-nil
        .{ .tag = .nilp, .then_ty = &types.t_nil, .else_ty = &types.t_non_nil },
    };

    /// Get operand from a unary predicate IR node
    fn getPredicateOperand(node: *const Ir) ?*const Ir {
        return switch (node.*) {
            .consp => |p| p.operand,
            .symbolp => |p| p.operand,
            .numberp => |p| p.operand,
            .stringp => |p| p.operand,
            .vectorp => |p| p.operand,
            .nilp => |p| p.operand,
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
                return self.builder.variable(name, binding.depth, binding.index) catch
                    return error.OutOfMemory;
            }
            // Check globals
            if (self.globals.lookup(name)) |idx| {
                return self.builder.globalRef(name, idx) catch
                    return error.OutOfMemory;
            }
            // Unbound variable
            return error.UnboundVariable;
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
        quote,
        quasiquote,
        @"while",
        define,
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
        .{ "quote", .quote },
        .{ "quasiquote", .quasiquote },
        .{ "while", .@"while" },
        .{ "define", .define },
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
    });

    fn compileListWithTail(self: *Compiler, expr: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check for special forms using symbol identity comparison
        if (head.isSymbol()) {
            if (self.builtins) |b| {
                // Fast path: identity comparison (single u64 compare)
                if (head.raw == b.@"if".raw) return self.compileIfWithTail(tail, env, in_tail);
                if (head.raw == b.let.raw) return self.compileLetWithTail(tail, env, in_tail);
                if (head.raw == b.letrec.raw) return self.compileLetrecWithTail(tail, env, in_tail);
                if (head.raw == b.@"let*".raw) return self.compileLetStarWithTail(tail, env, in_tail);
                if (head.raw == b.cond.raw) return self.compileCondWithTail(tail, env, in_tail);
                if (head.raw == b.progn.raw or head.raw == b.begin.raw) return self.compilePrognWithTail(tail, env, in_tail);
                if (head.raw == b.flet.raw) return self.compileFletWithTail(tail, env, in_tail);
                if (head.raw == b.labels.raw) return self.compileLabelsWithTail(tail, env, in_tail);
                if (head.raw == b.block.raw) return self.compileBlockWithTail(tail, env, in_tail);
                if (head.raw == b.@"return-from".raw) return self.compileReturnFrom(tail, env);
                if (head.raw == b.@"unwind-protect".raw) return self.compileUnwindProtectWithTail(tail, env, in_tail);
                if (head.raw == b.@"catch".raw) return self.compileCatchWithTail(tail, env, in_tail);
                if (head.raw == b.throw.raw) return self.compileThrow(tail, env);
                if (head.raw == b.tagbody.raw) return self.compileTagbody(tail, env);
                if (head.raw == b.go.raw) return self.compileGo(tail);
                if (head.raw == b.values.raw) return self.compileValues(tail, env);
                if (head.raw == b.@"multiple-value-bind".raw) return self.compileMvBind(tail, env);
                if (head.raw == b.lambda.raw) return self.compileLambda(tail, env);
                if (head.raw == b.@"and".raw) return self.compileAnd(tail, env);
                if (head.raw == b.@"or".raw) return self.compileOr(tail, env);
                if (head.raw == b.funcall.raw) return self.compileFuncall(tail, env);
                if (head.raw == b.apply.raw) return self.compileApply(tail, env);
                if (head.raw == b.@"set!".raw) return self.compileSet(tail, env);
                if (head.raw == b.quote.raw) return self.compileQuote(tail);
                if (head.raw == b.quasiquote.raw) return self.compileQuasiquote(tail, env);
                if (head.raw == b.@"while".raw) return self.compileWhile(tail, env);
                if (head.raw == b.define.raw) return self.compileDefine(tail, env);
                if (head.raw == b.defun.raw) return self.compileDefun(tail, env);
                if (head.raw == b.the.raw) return self.compileThe(tail, env);
            } else {
                // Fallback: string comparison via StaticStringMap
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
                        // Non-tail forms
                        .lambda => self.compileLambda(tail, env),
                        .@"and" => self.compileAnd(tail, env),
                        .@"or" => self.compileOr(tail, env),
                        .funcall => self.compileFuncall(tail, env),
                        .apply => self.compileApply(tail, env),
                        .@"set!" => self.compileSet(tail, env),
                        .quote => self.compileQuote(tail),
                        .quasiquote => self.compileQuasiquote(tail, env),
                        .@"while" => self.compileWhile(tail, env),
                        .define => self.compileDefine(tail, env),
                        .defun => self.compileDefun(tail, env),
                        .the => self.compileThe(tail, env),
                        .@"return-from" => self.compileReturnFrom(tail, env),
                        .@"unwind-protect" => self.compileUnwindProtectWithTail(tail, env, in_tail),
                        .@"catch" => self.compileCatchWithTail(tail, env, in_tail),
                        .throw => self.compileThrow(tail, env),
                        .tagbody => self.compileTagbody(tail, env),
                        .go => self.compileGo(tail),
                        .values => self.compileValues(tail, env),
                        .@"multiple-value-bind" => self.compileMvBind(tail, env),
                    };
                }
            }

            // Check for primitives (both paths need this)
            const sym = head.toPtr(Symbol);
            const name = sym.getName();
            if (self.compilePrimitive(name, tail, env)) |prim| {
                return prim;
            } else |_| {
                // Fall through to function call
            }
        }

        // Function call - pass tail position
        return self.compileCallWithTail(head, tail, env, in_tail);
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

        // Create new environment with parameters
        var lambda_env = Env.init(self.allocator, env);
        defer lambda_env.deinit();

        for (params.items) |param| {
            _ = lambda_env.bind(param) catch return error.OutOfMemory;
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

        return self.builder.lambda(params.items, captures, body_ir) catch
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
                const sym = head.toPtr(Symbol);
                const name = sym.getName();

                if (std.mem.eql(u8, name, "lambda")) {
                    // Lambda creates new scope - handled recursively by compileLambda
                    return;
                }

                if (std.mem.eql(u8, name, "let")) {
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
                                const b = binding.toPtr(Cons);
                                if (b.cdr.isCons()) {
                                    const val_cons = b.cdr.toPtr(Cons);
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
                                const b = binding.toPtr(Cons);
                                if (b.car.isSymbol()) {
                                    const bname_sym = b.car.toPtr(Symbol);
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

                if (std.mem.eql(u8, name, "quote")) {
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

    fn compileLet(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        return self.compileLetWithTail(args, env, false);
    }

    fn compileLetWithTail(self: *Compiler, args: Value, env: *const Env, in_tail: bool) CompileError!*Ir {
        // (let ((x 1) (y 2)) body)
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // Parse bindings
        var bindings = std.ArrayList(Ir.Binding){};
        defer bindings.deinit(self.allocator);

        var binding_list = bindings_expr;
        while (binding_list.isCons()) {
            const binding_cons = binding_list.toPtr(Cons);
            const binding = binding_cons.car;

            if (!binding.isCons()) return error.InvalidLet;
            const b = binding.toPtr(Cons);

            if (!b.car.isSymbol()) return error.InvalidLet;
            const name_sym = b.car.toPtr(Symbol);
            const name = name_sym.getName();

            // Get value expression
            if (!b.cdr.isCons()) return error.InvalidLet;
            const val_cons = b.cdr.toPtr(Cons);
            const val_ir = try self.compile(val_cons.car, env);

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

        // Compile body - body is in tail position if let is
        const body_ir = try self.compileBodyWithTail(body_exprs, &let_env, in_tail);

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
        // (set! var value)
        if (!args.isCons()) return error.InvalidSet;

        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSet;
        const var_sym = cons1.car.toPtr(Symbol);
        const name = var_sym.getName();

        if (!cons1.cdr.isCons()) return error.InvalidSet;
        const cons2 = cons1.cdr.toPtr(Cons);
        const val_ir = try self.compile(cons2.car, env);

        if (env.lookup(name)) |binding| {
            return self.builder.set(name, binding.depth, binding.index, val_ir) catch
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

    fn compilePrimitive(self: *Compiler, name: []const u8, args: Value, env: *const Env) CompileError!*Ir {
        // Binary arithmetic
        if (std.mem.eql(u8, name, "+")) {
            return self.compileBinaryPrim(args, env, .add);
        }
        if (std.mem.eql(u8, name, "-")) {
            return self.compileBinaryPrim(args, env, .sub);
        }
        if (std.mem.eql(u8, name, "*")) {
            return self.compileBinaryPrim(args, env, .mul);
        }
        if (std.mem.eql(u8, name, "/")) {
            return self.compileBinaryPrim(args, env, .div);
        }
        if (std.mem.eql(u8, name, "mod") or std.mem.eql(u8, name, "%")) {
            return self.compileBinaryPrim(args, env, .mod);
        }

        // Comparison
        if (std.mem.eql(u8, name, "eq")) {
            return self.compileBinaryPrim(args, env, .eq);
        }
        if (std.mem.eql(u8, name, "<")) {
            return self.compileBinaryPrim(args, env, .lt);
        }
        if (std.mem.eql(u8, name, ">")) {
            return self.compileBinaryPrim(args, env, .gt);
        }
        if (std.mem.eql(u8, name, "<=")) {
            return self.compileBinaryPrim(args, env, .le);
        }
        if (std.mem.eql(u8, name, ">=")) {
            return self.compileBinaryPrim(args, env, .ge);
        }
        if (std.mem.eql(u8, name, "=")) {
            return self.compileBinaryPrim(args, env, .num_eq);
        }

        // List operations
        if (std.mem.eql(u8, name, "cons")) {
            return self.compileBinaryPrim(args, env, .cons);
        }
        if (std.mem.eql(u8, name, "car")) {
            return self.compileUnaryPrim(args, env, .car);
        }
        if (std.mem.eql(u8, name, "cdr")) {
            return self.compileUnaryPrim(args, env, .cdr);
        }

        // Type predicates
        if (std.mem.eql(u8, name, "consp") or std.mem.eql(u8, name, "cons?")) {
            return self.compileUnaryPrim(args, env, .consp);
        }
        if (std.mem.eql(u8, name, "symbolp") or std.mem.eql(u8, name, "symbol?")) {
            return self.compileUnaryPrim(args, env, .symbolp);
        }
        if (std.mem.eql(u8, name, "numberp") or std.mem.eql(u8, name, "number?")) {
            return self.compileUnaryPrim(args, env, .numberp);
        }
        if (std.mem.eql(u8, name, "stringp") or std.mem.eql(u8, name, "string?")) {
            return self.compileUnaryPrim(args, env, .stringp);
        }
        if (std.mem.eql(u8, name, "vectorp") or std.mem.eql(u8, name, "vector?")) {
            return self.compileUnaryPrim(args, env, .vectorp);
        }
        if (std.mem.eql(u8, name, "null") or std.mem.eql(u8, name, "null?")) {
            return self.compileUnaryPrim(args, env, .nilp);
        }
        if (std.mem.eql(u8, name, "not")) {
            return self.compileUnaryPrim(args, env, .not);
        }
        if (std.mem.eql(u8, name, "characterp") or std.mem.eql(u8, name, "character?")) {
            return self.compileUnaryPrim(args, env, .characterp);
        }

        // Character operations
        if (std.mem.eql(u8, name, "char-code")) {
            return self.compileUnaryPrim(args, env, .char_code);
        }
        if (std.mem.eql(u8, name, "code-char")) {
            return self.compileUnaryPrim(args, env, .code_char);
        }
        if (std.mem.eql(u8, name, "char=")) {
            return self.compileBinaryPrim(args, env, .char_eq);
        }
        if (std.mem.eql(u8, name, "char<")) {
            return self.compileBinaryPrim(args, env, .char_lt);
        }
        if (std.mem.eql(u8, name, "char>")) {
            return self.compileBinaryPrim(args, env, .char_gt);
        }
        if (std.mem.eql(u8, name, "read-char")) {
            return self.compileNullaryPrim(.read_char);
        }
        if (std.mem.eql(u8, name, "peek-char")) {
            return self.compileNullaryPrim(.peek_char);
        }
        if (std.mem.eql(u8, name, "unread-char")) {
            return self.compileUnaryPrim(args, env, .unread_char);
        }
        if (std.mem.eql(u8, name, "boundp")) {
            return self.compileUnaryPrim(args, env, .boundp);
        }
        if (std.mem.eql(u8, name, "fboundp")) {
            return self.compileUnaryPrim(args, env, .fboundp);
        }
        if (std.mem.eql(u8, name, "symbol-value")) {
            return self.compileUnaryPrim(args, env, .symbol_value);
        }
        if (std.mem.eql(u8, name, "symbol-function")) {
            return self.compileUnaryPrim(args, env, .symbol_function);
        }
        if (std.mem.eql(u8, name, "typep")) {
            return self.compileBinaryPrim(args, env, .typep);
        }

        // Numeric predicates
        if (std.mem.eql(u8, name, "abs")) {
            return self.compileUnaryPrim(args, env, .abs);
        }
        if (std.mem.eql(u8, name, "zerop")) {
            return self.compileUnaryPrim(args, env, .zerop);
        }
        if (std.mem.eql(u8, name, "plusp")) {
            return self.compileUnaryPrim(args, env, .plusp);
        }
        if (std.mem.eql(u8, name, "minusp")) {
            return self.compileUnaryPrim(args, env, .minusp);
        }
        if (std.mem.eql(u8, name, "evenp")) {
            return self.compileUnaryPrim(args, env, .evenp);
        }
        if (std.mem.eql(u8, name, "oddp")) {
            return self.compileUnaryPrim(args, env, .oddp);
        }

        // Vector operations
        if (std.mem.eql(u8, name, "vector-ref")) {
            return self.compileBinaryPrim(args, env, .vec_ref);
        }
        if (std.mem.eql(u8, name, "vector-length")) {
            return self.compileUnaryPrim(args, env, .vec_len);
        }

        // String operations
        if (std.mem.eql(u8, name, "string-ref")) {
            return self.compileBinaryPrim(args, env, .str_ref);
        }
        if (std.mem.eql(u8, name, "string-length")) {
            return self.compileUnaryPrim(args, env, .str_len);
        }
        if (std.mem.eql(u8, name, "string=")) {
            return self.compileBinaryPrim(args, env, .str_eq);
        }
        if (std.mem.eql(u8, name, "substring")) {
            return self.compileSubstring(args, env);
        }
        if (std.mem.eql(u8, name, "subseq")) {
            return self.compileSubseq(args, env);
        }
        if (std.mem.eql(u8, name, "concatenate")) {
            return self.compileConcatenate(args, env);
        }

        // I/O
        if (std.mem.eql(u8, name, "print")) {
            return self.compileUnaryPrim(args, env, .print);
        }
        if (std.mem.eql(u8, name, "format")) {
            return self.compileFormat(args, env);
        }

        // Hash tables
        if (std.mem.eql(u8, name, "make-hash-table")) {
            return self.compileMakeHash(args);
        }
        if (std.mem.eql(u8, name, "gethash")) {
            return self.compileGethash(args, env);
        }
        if (std.mem.eql(u8, name, "sethash")) {
            return self.compileSethash(args, env);
        }
        if (std.mem.eql(u8, name, "remhash")) {
            return self.compileRemhash(args, env);
        }
        if (std.mem.eql(u8, name, "hash-table-count")) {
            return self.compileHashTableCount(args, env);
        }
        if (std.mem.eql(u8, name, "hash-table-p")) {
            return self.compileHashTableP(args, env);
        }

        // Random
        if (std.mem.eql(u8, name, "random")) {
            return self.compileUnaryPrim(args, env, .random);
        }

        // Symbol creation
        if (std.mem.eql(u8, name, "intern")) {
            return self.compileUnaryPrim(args, env, .intern);
        }
        if (std.mem.eql(u8, name, "symbol-name")) {
            return self.compileUnaryPrim(args, env, .sym_name);
        }
        if (std.mem.eql(u8, name, "type-of")) {
            return self.compileUnaryPrim(args, env, .type_of);
        }

        return error.InvalidSyntax; // Not a known primitive
    }

    const PrimTag = enum { add, sub, mul, div, mod, eq, lt, gt, le, ge, num_eq, cons, car, cdr, consp, symbolp, numberp, stringp, vectorp, nilp, not, vec_ref, vec_len, str_ref, str_len, str_eq, print, random, intern, sym_name, type_of, characterp, char_code, code_char, char_eq, char_lt, char_gt, read_char, peek_char, unread_char, boundp, fboundp, symbol_value, symbol_function, typep, abs, zerop, plusp, minusp, evenp, oddp };

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
            .char_eq => self.builder.charEq(left, right),
            .char_lt => self.builder.charLt(left, right),
            .char_gt => self.builder.charGt(left, right),
            .typep => self.builder.typep(left, right),
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
            .str_len => self.builder.strLen(operand),
            .print => self.builder.print(operand),
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
            .characterp => self.builder.characterp(operand),
            .char_code => self.builder.charCode(operand),
            .code_char => self.builder.codeChar(operand),
            .unread_char => self.builder.unreadChar(operand),
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
            else => error.InvalidSyntax,
        } catch return error.OutOfMemory;
    }

    fn compileNullaryPrim(self: *Compiler, prim: PrimTag) CompileError!*Ir {
        return switch (prim) {
            .read_char => self.builder.readChar(),
            .peek_char => self.builder.peekChar(),
            else => error.InvalidSyntax,
        } catch return error.OutOfMemory;
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
        // (concatenate 'string s1 s2 ...)
        // For now, only supports 'string type
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        // Skip the type argument (assume 'string)
        _ = cons1.car;

        // Get the sequences
        var rest = cons1.cdr;
        if (!rest.isCons()) {
            // No sequences, return empty string
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        // Compile first sequence
        const first_cons = rest.toPtr(Cons);
        var result_ir = try self.compile(first_cons.car, env);
        rest = first_cons.cdr;

        // Concatenate remaining sequences
        while (rest.isCons()) {
            const cons = rest.toPtr(Cons);
            const next_ir = try self.compile(cons.car, env);
            result_ir = self.builder.strConcat(result_ir, next_ir) catch return error.OutOfMemory;
            rest = cons.cdr;
        }

        return result_ir;
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
        // (sethash key hashtable value)
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const key_ir = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const table_ir = try self.compile(cons2.car, env);

        if (!cons2.cdr.isCons()) return error.InvalidSyntax;
        const cons3 = cons2.cdr.toPtr(Cons);
        const value_ir = try self.compile(cons3.car, env);

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
