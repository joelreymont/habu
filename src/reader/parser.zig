//! S-expression parser
//!
//! Parses tokens into Habu Values (cons trees)

const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("lexer.zig").Token;
const TokenKind = @import("lexer.zig").TokenKind;
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const objects = @import("../runtime/objects.zig");
const primitives = @import("../runtime/primitives/primitives.zig");
const builtins_mod = @import("../runtime/builtins.zig");

pub const Error = error{
    UnexpectedToken,
    UnterminatedList,
    SkipForm,
    InvalidNumber,
    InvalidCharacter,
    Utf8InvalidStartByte,
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8EncodesSurrogateHalf,
    Utf8CodepointTooLarge,
    VectorTooLarge,
    InvalidStruct,
    InvalidArray,
    TooManySlots,
    OutOfMemory,
    TypeMismatch,
    InvalidPackage,
    SymbolNotExternal,
    Overflow,
};

pub const Parser = struct {
    pub const ReadEvalFn = *const fn (ctx: *anyopaque, expr: Value) anyerror!Value;
    pub const DispatchMacroFn = *const fn (
        ctx: *anyopaque,
        function: Value,
        disp_char: u8,
        sub_char: u8,
        arg: ?u32,
        stream: Value,
    ) anyerror!Value;

    lexer: Lexer,
    heap: *Heap,
    current: Token,
    /// List of active feature keywords (e.g., :habu)
    features: std.ArrayList(Value),
    /// Reader labels (#n= and #n#) for shared references.
    labels: std.AutoHashMap(u32, Value),
    alloc: std.mem.Allocator,
    builtins: *const builtins_mod.BuiltinSymbols,
    read_eval_ctx: ?*anyopaque,
    read_eval_fn: ?ReadEvalFn,
    dispatch_ctx: ?*anyopaque,
    dispatch_fn: ?DispatchMacroFn,
    hook_error: ?anyerror,

    pub fn init(alloc: std.mem.Allocator, heap: *Heap, source: []const u8, builtins: *const builtins_mod.BuiltinSymbols) Error!Parser {
        var lexer = Lexer.init(source);
        const first_token = lexer.next();

        var feats = std.ArrayList(Value){};
        // Add :habu by default
        const habu_kw = try heap.internKeyword("habu");
        try feats.append(alloc, habu_kw);
        var labels = std.AutoHashMap(u32, Value).init(alloc);
        errdefer labels.deinit();

        return .{
            .lexer = lexer,
            .heap = heap,
            .current = first_token,
            .features = feats,
            .labels = labels,
            .alloc = alloc,
            .builtins = builtins,
            .read_eval_ctx = null,
            .read_eval_fn = null,
            .dispatch_ctx = null,
            .dispatch_fn = null,
            .hook_error = null,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.features.deinit(self.alloc);
        self.labels.deinit();
    }

    pub fn setReadEvalHook(self: *Parser, ctx: *anyopaque, hook: ReadEvalFn) void {
        self.read_eval_ctx = ctx;
        self.read_eval_fn = hook;
    }

    pub fn setDispatchMacroHook(self: *Parser, ctx: *anyopaque, hook: DispatchMacroFn) void {
        self.dispatch_ctx = ctx;
        self.dispatch_fn = hook;
    }

    pub fn takeHookError(self: *Parser) ?anyerror {
        const err = self.hook_error;
        self.hook_error = null;
        return err;
    }

    /// Get the current token's location for error reporting
    pub fn getErrorLocation(self: *Parser) struct { line: u32, column: u32, text: []const u8 } {
        return .{
            .line = self.current.line,
            .column = self.current.column,
            .text = self.current.text,
        };
    }

    /// Parse one S-expression
    pub fn parse(self: *Parser) Error!Value {
        self.hook_error = null;
        while (true) {
            const expr = self.parseExpr() catch |err| switch (err) {
                error.SkipForm => continue,
                else => return err,
            };
            return expr;
        }
    }

    /// Parse all expressions until EOF
    pub fn parseAll(self: *Parser, allocator: std.mem.Allocator, results: *std.ArrayList(Value)) Error!void {
        while (self.current.kind != .eof) {
            self.hook_error = null;
            const expr = self.parseExpr() catch |err| switch (err) {
                error.SkipForm => continue,
                else => return err,
            };
            try results.append(allocator, expr);
        }
    }

    fn parseExpr(self: *Parser) Error!Value {
        switch (self.current.kind) {
            .lparen => return self.parseList(),
            .vector_open => return self.parseVector(),
            .complex_open => return self.parseComplex(),
            .struct_open => return self.parseStruct(),
            .array_open => return self.parseArray(),
            .pathname => return self.parsePathname(),
            .bitvec => return self.parseBitVector(),
            .quote => return self.parseQuote("quote"),
            .backquote => return self.parseQuote("quasiquote"),
            .comma => return self.parseQuote("unquote"),
            .comma_at => return self.parseQuote("unquote-splicing"),
            .function_quote => return self.parseQuote("function"),
            .read_eval => return self.parseReadEval(),
            .feature_present => return self.parseFeatureConditional(true),
            .feature_absent => return self.parseFeatureConditional(false),
            .number => return self.parseNumber(),
            .bignum => return self.parseBignum(),
            .float => return self.parseFloat(),
            .rational => return self.parseRational(),
            .string => return self.parseString(),
            .symbol => return self.parseSymbol(),
            .keyword => return self.parseKeyword(),
            .uninterned_symbol => return self.parseUninternedSymbol(),
            .character => return self.parseCharacter(),
            .label_def => return self.parseLabelDef(),
            .label_ref => return self.parseLabelRef(),
            .dispatch => return self.parseDispatch(),
            .eof => return Value.nil,
            .rparen, .dot => return error.UnexpectedToken,
            .err => return error.UnexpectedToken,
        }
    }

    fn parseReadEval(self: *Parser) Error!Value {
        self.advance(); // consume #.
        const expr = try self.parseExpr();
        if (self.read_eval_fn) |hook| {
            const ctx = self.read_eval_ctx orelse return error.UnexpectedToken;
            return hook(ctx, expr) catch |hook_err| {
                self.hook_error = hook_err;
                return error.UnexpectedToken;
            };
        }
        // Fallback when no read-eval hook is configured.
        return expr;
    }

    fn parseDispatchHeader(text: []const u8) Error!struct { disp_char: u8, sub_char: u8, arg: ?u32 } {
        if (text.len < 2 or text[0] != '#') return error.UnexpectedToken;
        const sub_char = text[text.len - 1];
        const arg_text = text[1 .. text.len - 1];
        if (arg_text.len == 0) {
            return .{
                .disp_char = '#',
                .sub_char = sub_char,
                .arg = null,
            };
        }
        for (arg_text) |c| {
            if (!std.ascii.isDigit(c)) return error.UnexpectedToken;
        }
        return .{
            .disp_char = '#',
            .sub_char = sub_char,
            .arg = std.fmt.parseUnsigned(u32, arg_text, 10) catch return error.InvalidNumber,
        };
    }

    fn parseDispatch(self: *Parser) Error!Value {
        const hook = self.dispatch_fn orelse return error.UnexpectedToken;
        const ctx = self.dispatch_ctx orelse return error.UnexpectedToken;
        const header = try parseDispatchHeader(self.current.text);

        const sub_table = self.heap.dispatch_readtable.get(header.disp_char) orelse return error.UnexpectedToken;
        const function = sub_table.get(header.sub_char) orelse return error.UnexpectedToken;

        const tail = self.lexer.source[self.lexer.pos..];
        const tail_str = try self.heap.allocBaseString(tail);
        const stream = try self.heap.allocStringInputStream(tail_str);
        const result = hook(ctx, function, header.disp_char, header.sub_char, header.arg, stream) catch |hook_err| {
            self.hook_error = hook_err;
            return error.UnexpectedToken;
        };

        const consumed_u64 = stream.toPtr(runtime.Stream).position;
        const consumed = std.math.cast(usize, consumed_u64) orelse return error.UnexpectedToken;
        if (consumed == 0 or consumed > tail.len) return error.UnexpectedToken;
        self.advanceSource(consumed);
        self.current = self.lexer.next();
        return result;
    }

    fn parseLabelId(text: []const u8) Error!u32 {
        if (text.len < 3 or text[0] != '#') return error.UnexpectedToken;
        const suffix = text[text.len - 1];
        if (suffix != '=' and suffix != '#') return error.UnexpectedToken;
        const digits = text[1 .. text.len - 1];
        if (digits.len == 0) return error.UnexpectedToken;
        return std.fmt.parseUnsigned(u32, digits, 10) catch error.InvalidNumber;
    }

    fn parseLabelDef(self: *Parser) Error!Value {
        const label_id = try parseLabelId(self.current.text);
        self.advance(); // consume #n=
        const expr = try self.parseExpr();
        try self.labels.put(label_id, expr);
        return expr;
    }

    fn parseLabelRef(self: *Parser) Error!Value {
        const label_id = try parseLabelId(self.current.text);
        self.advance(); // consume #n#
        return self.labels.get(label_id) orelse error.UnexpectedToken;
    }

    fn parseList(self: *Parser) Error!Value {
        self.advance(); // consume '('

        if (self.current.kind == .rparen) {
            self.advance();
            return Value.nil;
        }

        // Parse first element
        const first = self.parseExpr() catch |err| switch (err) {
            error.SkipForm => return self.parseListTail(),
            else => return err,
        };

        // Check for dotted pair
        if (self.current.kind == .dot) {
            self.advance(); // consume '.'
            const second = while (true) {
                break self.parseExpr() catch |err| switch (err) {
                    error.SkipForm => continue,
                    else => return err,
                };
            };

            if (self.current.kind != .rparen) {
                return error.UnexpectedToken;
            }
            self.advance();

            return try self.heap.allocCons(first, second);
        }

        // Parse rest as proper list
        const rest = try self.parseListTail();
        return try self.heap.allocCons(first, rest);
    }

    fn parseListTail(self: *Parser) Error!Value {
        if (self.current.kind == .rparen) {
            self.advance();
            return Value.nil;
        }

        if (self.current.kind == .eof) {
            return error.UnterminatedList;
        }

        if (self.current.kind == .dot) {
            self.advance();
            const cdr = while (true) {
                break self.parseExpr() catch |err| switch (err) {
                    error.SkipForm => continue,
                    else => return err,
                };
            };
            if (self.current.kind != .rparen) {
                return error.UnexpectedToken;
            }
            self.advance();
            return cdr;
        }

        const car = self.parseExpr() catch |err| switch (err) {
            error.SkipForm => return self.parseListTail(),
            else => return err,
        };
        const cdr = try self.parseListTail();
        return try self.heap.allocCons(car, cdr);
    }

    fn parseVector(self: *Parser) Error!Value {
        self.advance(); // consume '#('

        // Collect elements into temporary stack buffer
        var elements: [256]Value = undefined;
        var count: usize = 0;

        while (self.current.kind != .rparen) {
            if (self.current.kind == .eof) {
                return error.UnterminatedList;
            }
            if (count >= elements.len) {
                return error.VectorTooLarge;
            }
            const next = self.parseExpr() catch |err| switch (err) {
                error.SkipForm => continue,
                else => return err,
            };
            elements[count] = next;
            count += 1;
        }
        self.advance(); // consume ')'

        // Allocate vector and copy elements
        const vec_val = try self.heap.allocVector(count, count);
        const vec = vec_val.toPtr(runtime.Vector);
        for (0..count) |i| {
            vec.data[i] = elements[i];
        }

        return vec_val;
    }

    fn parseComplex(self: *Parser) Error!Value {
        self.advance(); // consume '#C('

        // Parse real part
        if (self.current.kind == .rparen or self.current.kind == .eof) {
            return error.InvalidNumber;
        }
        const real_val = try self.parseExpr();

        // Parse imaginary part
        if (self.current.kind == .rparen or self.current.kind == .eof) {
            return error.InvalidNumber;
        }
        const imag_val = try self.parseExpr();

        // Expect closing paren
        if (self.current.kind != .rparen) {
            return error.UnexpectedToken;
        }
        self.advance(); // consume ')'

        // Convert to floats - validate both parts are real numbers
        const real = try toReal(real_val);
        const imag = try toReal(imag_val);

        // CL reader canonicalizes #C(x 0) to x.
        if (imag == 0.0) {
            return real_val;
        }
        return primitives.complex.makeComplex(self.heap, real, imag);
    }

    fn toReal(val: Value) Error!f64 {
        return switch (val.typeKind()) {
            .fixnum => @floatFromInt(val.toFixnum()),
            .float => val.toFloat(),
            .rational => blk: {
                const rat = val.toPtr(objects.Rational);
                break :blk @as(f64, @floatFromInt(rat.numerator)) / @as(f64, @floatFromInt(rat.denominator));
            },
            else => error.TypeMismatch,
        };
    }

    fn parseStruct(self: *Parser) Error!Value {
        self.advance(); // consume '#S('

        // Parse struct name (must be a symbol)
        if (self.current.kind != .symbol) {
            return error.InvalidStruct;
        }
        const struct_name = try self.parseSymbol();

        // Parse keyword-value pairs
        var args_stack: [128]Value = undefined;
        var arg_count: usize = 0;

        while (self.current.kind != .rparen) {
            if (self.current.kind == .eof) {
                return error.UnterminatedList;
            }
            if (arg_count >= args_stack.len) {
                return error.TooManySlots;
            }
            args_stack[arg_count] = try self.parseExpr();
            arg_count += 1;
        }
        self.advance(); // consume ')'

        // Build (make-struct 'struct-name :slot1 val1 :slot2 val2 ...)
        const make_struct_sym = try self.internSymbol("make-struct");
        const quoted_name = try self.buildQuote(struct_name);

        // Build arg list
        var args = Value.nil;
        var i = arg_count;
        while (i > 0) {
            i -= 1;
            args = try self.heap.allocCons(args_stack[i], args);
        }

        // Build (make-struct 'name ...)
        const rest = try self.heap.allocCons(quoted_name, args);
        return try self.heap.allocCons(make_struct_sym, rest);
    }

    fn buildQuote(self: *Parser, expr: Value) Error!Value {
        const quote_sym = try self.internSymbol("quote");
        const inner = try self.heap.allocCons(expr, Value.nil);
        return try self.heap.allocCons(quote_sym, inner);
    }

    fn parseArray(self: *Parser) Error!Value {
        const token_text = self.current.text;

        // Extract rank from token text before consuming
        // Accept both "#2A(" and "2A(" token shapes.
        var rank: ?i64 = null;
        const start_idx: usize = if (token_text.len > 0 and token_text[0] == '#') 1 else 0;
        if (token_text.len > start_idx + 1 and token_text[start_idx] >= '0' and token_text[start_idx] <= '9') {
            const digits_end = blk: {
                for (token_text[start_idx..], start_idx..) |ch, idx| {
                    if (ch == 'A' or ch == 'a') break :blk idx;
                }
                break :blk token_text.len;
            };
            rank = try std.fmt.parseInt(i64, token_text[start_idx..digits_end], 10);
        }

        self.advance(); // consume array_open token

        // Rank-0 arrays are scalar literals: #0AT, #0A(T), or #0A()
        if (rank != null and rank.? == 0) {
            const make_array_sym = try self.internSymbol("make-array");
            const dims = Value.nil;
            const has_paren_contents = token_text.len > 0 and token_text[token_text.len - 1] == '(';

            if (has_paren_contents and self.current.kind == .rparen) {
                self.advance();
                const args = try self.heap.allocCons(dims, Value.nil);
                return try self.heap.allocCons(make_array_sym, args);
            }

            const scalar = try self.parseExpr();
            if (has_paren_contents) {
                if (self.current.kind != .rparen) return error.UnexpectedToken;
                self.advance();
            }
            const scalar_lit = try self.quoteIfNeeded(scalar);

            const initial_element_kw = try self.internKeyword("initial-element");
            const kw_pair = try self.heap.allocCons(initial_element_kw, try self.heap.allocCons(scalar_lit, Value.nil));
            const args = try self.heap.allocCons(dims, kw_pair);
            return try self.heap.allocCons(make_array_sym, args);
        }

        // Parse contents in literal reader mode.
        var contents = Value.nil;
        if (self.current.kind == .rparen) {
            self.advance();
        } else {
            const first = self.parseExpr() catch |err| switch (err) {
                error.SkipForm => return self.parseListTail(),
                else => return err,
            };
            const rest = try self.parseListTail();
            contents = try self.heap.allocCons(first, rest);
        }

        // Build dims argument based on rank
        const rank_val: ?usize = if (rank) |r| blk: {
            const ur_opt = std.math.cast(usize, r);
            if (ur_opt == null) return error.InvalidArray;
            const ur = ur_opt.?;
            if (ur == 0) return error.InvalidArray;
            break :blk ur;
        } else null;
        const dims_arg = try self.inferArrayDims(contents, rank_val);

        var dims_buf: [8]u64 = [_]u64{0} ** 8;
        const dims_len = try dimsToBuffer(dims_arg, &dims_buf);
        const arr_val = try self.heap.allocArray(dims_buf[0..dims_len]);
        const arr = arr_val.toPtr(objects.Array);

        var write_idx: usize = 0;
        try self.fillArrayLiteral(arr, contents, 0, &write_idx);
        if (write_idx != @as(usize, @intCast(arr.total_size))) return error.InvalidArray;

        return arr_val;
    }

    fn quoteIfNeeded(self: *Parser, expr: Value) Error!Value {
        if (expr.isCons()) return self.buildQuote(expr);
        if (expr.isSymbol() and !expr.isMagicSymbol()) return self.buildQuote(expr);
        return expr;
    }

    fn dimsToBuffer(dims: Value, out: *[8]u64) Error!usize {
        var count: usize = 0;
        var current = dims;
        while (current.isCons()) {
            if (count >= out.len) return error.InvalidArray;
            const cons = current.toPtr(objects.Cons);
            if (!cons.car.isFixnum()) return error.InvalidArray;
            const dim = cons.car.toFixnum();
            if (dim < 0) return error.InvalidArray;
            out[count] = @intCast(dim);
            count += 1;
            current = cons.cdr;
        }
        if (!current.isNil() or count == 0) return error.InvalidArray;
        return count;
    }

    fn fillArrayLiteral(self: *Parser, arr: *objects.Array, contents: Value, depth: usize, write_idx: *usize) Error!void {
        const rank: usize = @intCast(arr.rank);
        if (depth >= rank) return error.InvalidArray;
        const terminal = depth + 1 == rank;
        var count: usize = 0;
        var current = contents;
        const data: [*]Value = @ptrFromInt(arr.data_ptr);

        while (current.isCons()) {
            const cons = current.toPtr(objects.Cons);
            if (terminal) {
                if (write_idx.* >= @as(usize, @intCast(arr.total_size))) return error.InvalidArray;
                data[write_idx.*] = cons.car;
                write_idx.* += 1;
            } else {
                try self.fillArrayLiteral(arr, cons.car, depth + 1, write_idx);
            }
            count += 1;
            current = cons.cdr;
        }
        if (!current.isNil()) return error.InvalidArray;
        if (count != @as(usize, @intCast(arr.dimensions[depth]))) return error.InvalidArray;
    }

    fn inferArrayDims(self: *Parser, contents: Value, rank: ?usize) Error!Value {
        const dims = try self.inferDims(contents, rank);
        if (rank) |r| {
            const len = try dimsLen(dims);
            if (len != r) return error.InvalidArray;
        }
        return dims;
    }

    fn inferDims(self: *Parser, contents: Value, rank: ?usize) Error!Value {
        if (contents.isNil()) {
            if (rank) |r| {
                return try self.zeroDims(r);
            }
            return try self.heap.allocCons(Value.makeFixnum(0), Value.nil);
        }
        if (!contents.isCons()) return error.InvalidArray;

        var len: i64 = 0;
        var current = contents;
        while (current.isCons()) {
            const cons = current.toPtr(objects.Cons);
            len += 1;
            current = cons.cdr;
        }
        if (!current.isNil()) return error.InvalidArray;

        var nested = false;
        if (rank) |r| {
            if (r == 0) return error.InvalidArray;
            nested = r > 1;
            var cur = contents;
            while (cur.isCons()) {
                const cons = cur.toPtr(objects.Cons);
                const elem = cons.car;
                if (nested and !elem.isCons() and !elem.isNil()) return error.InvalidArray;
                cur = cons.cdr;
            }
        } else {
            var cur = contents;
            while (cur.isCons()) {
                const cons = cur.toPtr(objects.Cons);
                if (cons.car.isCons()) {
                    nested = true;
                    break;
                }
                cur = cons.cdr;
            }
        }

        if (!nested) {
            return try self.heap.allocCons(Value.makeFixnum(len), Value.nil);
        }

        const next_rank = if (rank) |r| r - 1 else null;
        const first = contents.toPtr(objects.Cons).car;
        const sub_dims = try self.inferDims(first, next_rank);
        var rest = contents.toPtr(objects.Cons).cdr;
        while (rest.isCons()) {
            const cons = rest.toPtr(objects.Cons);
            const elem_dims = try self.inferDims(cons.car, next_rank);
            if (!dimsEq(sub_dims, elem_dims)) return error.InvalidArray;
            rest = cons.cdr;
        }

        return try self.heap.allocCons(Value.makeFixnum(len), sub_dims);
    }

    fn dimsLen(dims: Value) Error!usize {
        var len: usize = 0;
        var current = dims;
        while (current.isCons()) {
            len += 1;
            current = current.toPtr(objects.Cons).cdr;
        }
        if (!current.isNil()) return error.InvalidArray;
        return len;
    }

    fn dimsEq(a: Value, b: Value) bool {
        var cur_a = a;
        var cur_b = b;
        while (cur_a.isCons() and cur_b.isCons()) {
            const a_cons = cur_a.toPtr(objects.Cons);
            const b_cons = cur_b.toPtr(objects.Cons);
            if (!a_cons.car.isFixnum() or !b_cons.car.isFixnum()) return false;
            if (a_cons.car.toFixnum() != b_cons.car.toFixnum()) return false;
            cur_a = a_cons.cdr;
            cur_b = b_cons.cdr;
        }
        return cur_a.isNil() and cur_b.isNil();
    }

    fn zeroDims(self: *Parser, rank: usize) Error!Value {
        var dims = Value.nil;
        var i: usize = 0;
        while (i < rank) : (i += 1) {
            dims = try self.heap.allocCons(Value.makeFixnum(0), dims);
        }
        return dims;
    }

    fn parseBitVector(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance(); // consume bitvec token

        // Strip '#*' prefix
        const bits = text[2..];

        // Create vector of fixnums (0 or 1)
        const vec_val = try self.heap.allocVector(bits.len, bits.len);
        const vec = vec_val.toPtr(runtime.Vector);

        for (bits, 0..) |c, i| {
            vec.data[i] = Value.makeFixnum(if (c == '1') 1 else 0);
        }

        return vec_val;
    }

    fn parsePathname(self: *Parser) Error!Value {
        const path_str = try self.parseString();

        // Build (parse-namestring "path")
        const parse_namestring_sym = try self.internSymbol("parse-namestring");
        const args = try self.heap.allocCons(path_str, Value.nil);
        return try self.heap.allocCons(parse_namestring_sym, args);
    }

    fn parseQuote(self: *Parser, quote_name: []const u8) Error!Value {
        self.advance(); // consume quote token
        const quoted = try self.parseExpr();

        // Build (quote <expr>) or (quasiquote <expr>) etc.
        const quote_sym = try self.internSymbol(quote_name);
        const inner = try self.heap.allocCons(quoted, Value.nil);
        return try self.heap.allocCons(quote_sym, inner);
    }

    fn parseSignedRadixValue(self: *Parser, text: []const u8, radix: u8) Error!Value {
        if (text.len == 0 or radix < 2 or radix > 36) return error.InvalidNumber;

        var negative = false;
        var digits = text;
        switch (digits[0]) {
            '+' => digits = digits[1..],
            '-' => {
                negative = true;
                digits = digits[1..];
            },
            else => {},
        }
        if (digits.len == 0) return error.InvalidNumber;

        var limbs = std.ArrayList(u64){};
        defer limbs.deinit(self.alloc);
        try limbs.append(self.alloc, 0);

        const radix_u128: u128 = @as(u128, radix);
        for (digits) |ch| {
            const digit = std.fmt.charToDigit(ch, radix) catch return error.InvalidNumber;
            var carry: u128 = @as(u128, digit);
            var i: usize = 0;
            while (i < limbs.items.len) : (i += 1) {
                const acc: u128 = (@as(u128, limbs.items[i]) * radix_u128) + carry;
                limbs.items[i] = @truncate(acc);
                carry = acc >> 64;
            }
            while (carry != 0) {
                try limbs.append(self.alloc, @truncate(carry));
                carry >>= 64;
            }
        }

        const max_pos: u64 = @as(u64, @intCast(std.math.maxInt(i64)));
        const max_neg_mag: u64 = max_pos + 1;
        if (limbs.items.len == 1) {
            const magnitude = limbs.items[0];
            if (negative) {
                if (magnitude == 0) return Value.makeFixnum(0);
                if (magnitude <= max_neg_mag) {
                    if (magnitude == max_neg_mag) return Value.makeFixnum(std.math.minInt(i64));
                    return Value.makeFixnum(-@as(i64, @intCast(magnitude)));
                }
            } else if (magnitude <= max_pos) {
                return Value.makeFixnum(@as(i64, @intCast(magnitude)));
            }
        }

        return self.heap.allocBignumFromLimbs(limbs.items, negative);
    }

    fn parseNumber(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        // Check for radix prefixes: #x (hex), #b (binary), #o (octal), #<n>r (base n)
        if (text.len >= 2 and text[0] == '#') {
            const radix_char = text[1];
            if (radix_char == 'x' or radix_char == 'X' or radix_char == 'b' or radix_char == 'B' or radix_char == 'o' or radix_char == 'O') {
                const digits = text[2..];
                if (digits.len == 0) return error.InvalidNumber;

                const radix: u8 = switch (radix_char) {
                    'x', 'X' => 16,
                    'b', 'B' => 2,
                    'o', 'O' => 8,
                    else => unreachable,
                };
                return self.parseSignedRadixValue(digits, radix);
            }

            var idx: usize = 1;
            while (idx < text.len and text[idx] >= '0' and text[idx] <= '9') : (idx += 1) {}
            if (idx > 1 and idx < text.len and (text[idx] == 'r' or text[idx] == 'R')) {
                const radix = try std.fmt.parseInt(u8, text[1..idx], 10);
                if (radix < 2 or radix > 36) return error.InvalidNumber;
                const digits = text[idx + 1 ..];
                if (digits.len == 0) return error.InvalidNumber;
                return self.parseSignedRadixValue(digits, radix);
            }
        }

        // CL spec: trailing dot on integer (e.g., "55.") means integer 55
        var num_text = text;
        if (num_text.len > 0 and num_text[num_text.len - 1] == '.') {
            num_text = num_text[0 .. num_text.len - 1];
        }
        return self.parseSignedRadixValue(num_text, 10);
    }

    fn parseBignum(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();
        return self.parseSignedRadixValue(text, 10);
    }

    fn parseFloat(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        var float_text = text;
        var normalized: ?[]u8 = null;
        defer if (normalized) |buf| self.alloc.free(buf);

        for (text, 0..) |ch, i| {
            if (ch == 's' or ch == 'S' or ch == 'f' or ch == 'F' or ch == 'd' or ch == 'D' or ch == 'l' or ch == 'L') {
                const buf = try self.alloc.alloc(u8, text.len);
                std.mem.copyForwards(u8, buf, text);
                buf[i] = 'e';
                float_text = buf;
                normalized = buf;
                break;
            }
        }

        const f = try std.fmt.parseFloat(f64, float_text);
        return Value.makeFloat(f);
    }

    fn parseRational(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        // Split on '/'
        const slash_pos = if (std.mem.indexOf(u8, text, "/")) |val| val else return error.InvalidNumber;
        const num_str = text[0..slash_pos];
        const den_str = text[slash_pos + 1 ..];

        const num_opt = std.fmt.parseInt(i64, num_str, 10) catch |err| switch (err) {
            error.Overflow => null,
            else => return error.InvalidNumber,
        };
        const den_opt = std.fmt.parseInt(i64, den_str, 10) catch |err| switch (err) {
            error.Overflow => null,
            else => return error.InvalidNumber,
        };

        if (num_opt) |num| {
            if (den_opt) |den| {
                return primitives.rational.makeRational(self.heap, num, den);
            }
        }

        // Fallback for very large literals: approximate via float and convert back
        // to the runtime rational representation.
        const num_f = std.fmt.parseFloat(f64, num_str) catch return error.InvalidNumber;
        const den_f = std.fmt.parseFloat(f64, den_str) catch return error.InvalidNumber;
        if (den_f == 0.0) return error.InvalidNumber;
        const ratio = num_f / den_f;
        if (std.math.isNan(ratio) or std.math.isInf(ratio)) return error.Overflow;
        return primitives.rational.floatToRational(self.heap, ratio);
    }

    fn parseString(self: *Parser) Error!Value {
        var text = self.current.text;
        self.advance();

        // Remove quotes
        if (text.len >= 2) {
            text = text[1 .. text.len - 1];
        }

        // Check if we need to decode escapes or if string has non-ASCII
        const has_escape = std.mem.indexOf(u8, text, "\\") != null;
        var has_non_ascii = false;
        for (text) |byte| {
            if (byte >= 128) {
                has_non_ascii = true;
                break;
            }
        }

        if (has_escape) {
            // Has escapes - decode them into String32
            return try self.decodeStringEscapes(text);
        }

        if (has_non_ascii) {
            // UTF-8 content without escapes - convert to String32
            return try self.heap.allocString32FromUtf8(text);
        }

        // Pure ASCII without escapes - use base-string
        return try self.heap.allocBaseString(text);
    }

    /// Decode escape sequences in a string, allocating result in heap's string space
    /// Returns String32 to support Unicode escapes
    fn decodeStringEscapes(self: *Parser, text: []const u8) Error!Value {
        // First pass: count output codepoints and check for Unicode escapes
        var out_len: usize = 0;
        var has_unicode = false;
        var i: usize = 0;
        while (i < text.len) {
            if (text[i] == '\\' and i + 1 < text.len) {
                const next = text[i + 1];
                if (next == 'u' or next == 'U') {
                    has_unicode = true;
                    // \uXXXX or \UXXXXXXXX
                    const hex_digits = if (next == 'u') @as(usize, 4) else @as(usize, 8);
                    if (i + 2 + hex_digits > text.len) return error.UnexpectedToken;
                    i += 2 + hex_digits;
                } else {
                    i += 2;
                }
                out_len += 1;
            } else {
                // Decode UTF-8 to count codepoints
                const cp_len = try std.unicode.utf8ByteSequenceLength(text[i]);
                if (i + cp_len > text.len) return error.UnexpectedToken;
                i += cp_len;
                out_len += 1;
            }
        }

        // Allocate UTF-32 string with uninitialized content
        const str_val = try self.heap.allocString32Uninitialized(out_len);
        const str = str_val.toPtr(runtime.String32);
        const buffer = str.mutableCodepoints();

        // Second pass: decode into buffer
        var out_idx: usize = 0;
        i = 0;
        while (i < text.len) {
            if (text[i] == '\\' and i + 1 < text.len) {
                const next = text[i + 1];
                buffer[out_idx] = switch (next) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '"' => '"',
                    '0' => 0,
                    'u' => blk: {
                        // \uXXXX - 4 hex digits
                        if (i + 6 > text.len) return error.UnexpectedToken;
                        const hex = text[i + 2 .. i + 6];
                        const cp = try std.fmt.parseInt(u32, hex, 16);
                        i += 4; // Extra advance (base +2 below)
                        break :blk cp;
                    },
                    'U' => blk: {
                        // \UXXXXXXXX - 8 hex digits
                        if (i + 10 > text.len) return error.UnexpectedToken;
                        const hex = text[i + 2 .. i + 10];
                        const cp = try std.fmt.parseInt(u32, hex, 16);
                        i += 8; // Extra advance
                        break :blk cp;
                    },
                    else => @as(u32, next),
                };
                out_idx += 1;
                i += 2;
            } else {
                // Decode UTF-8 codepoint
                const cp_len = try std.unicode.utf8ByteSequenceLength(text[i]);
                if (i + cp_len > text.len) return error.UnexpectedToken;
                const cp = try std.unicode.utf8Decode(text[i .. i + cp_len]);
                buffer[out_idx] = cp;
                out_idx += 1;
                i += cp_len;
            }
        }

        // If all codepoints are ASCII, downgrade to base String for compatibility
        if (!has_unicode) {
            var all_ascii = true;
            for (buffer[0..out_idx]) |cp| {
                if (cp >= 128) {
                    all_ascii = false;
                    break;
                }
            }
            if (all_ascii) {
                // Convert to base string (u8 bytes)
                var ascii_buf: [1024]u8 = undefined;
                if (out_idx <= ascii_buf.len) {
                    for (buffer[0..out_idx], 0..) |cp, j| {
                        ascii_buf[j] = @intCast(cp);
                    }
                    return try self.heap.allocBaseString(ascii_buf[0..out_idx]);
                }
                // Fall through to String32 for very long strings
            }
        }

        return str_val;
    }

    fn parseSymbol(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        // Check for package-qualified symbol (pkg:sym or pkg::sym)
        if (findUnescapedColon(text)) |colon_pos| {
            if (colon_pos > 0) {
                const pkg_name_raw = text[0..colon_pos];
                // Skip one or two colons
                var sym_start = colon_pos + 1;
                const external_only = !(sym_start < text.len and text[sym_start] == ':');
                if (!external_only) {
                    sym_start += 1;
                }
                if (sym_start >= text.len) {
                    // Just "pkg:" or "pkg::" with no symbol name
                    return error.UnexpectedToken;
                }
                const sym_name_raw = text[sym_start..];
                const pkg_name = try self.decodeSymbolToken(pkg_name_raw);
                defer if (pkg_name.owned) |buf| self.alloc.free(buf);
                const sym_name = try self.decodeSymbolToken(sym_name_raw);
                defer if (sym_name.owned) |buf| self.alloc.free(buf);
                if (std.posix.getenv("HABU_TRACE_SYMBOL_COLON") != null) {
                    std.debug.print(
                        "TRACE symbol package split: token={s} pkg={s} sym={s}\n",
                        .{ text, pkg_name.slice, sym_name.slice },
                    );
                }
                return self.internSymbolInPackage(pkg_name.slice, sym_name.slice, external_only);
            }
        }

        const name = try self.decodeSymbolToken(text);
        defer if (name.owned) |buf| self.alloc.free(buf);
        return self.internSymbol(name.slice);
    }

    fn findUnescapedColon(text: []const u8) ?usize {
        var i: usize = 0;
        var in_bar = false;
        while (i < text.len) {
            const c = text[i];
            if (c == '\\') {
                if (i + 1 < text.len) i += 2 else i += 1;
                continue;
            }
            if (c == '|') {
                in_bar = !in_bar;
                i += 1;
                continue;
            }
            if (!in_bar and c == ':') return i;
            i += 1;
        }
        return null;
    }

    fn parseKeyword(self: *Parser) Error!Value {
        var text = self.current.text;
        self.advance();

        // Remove leading colon for storage
        if (text.len > 0 and text[0] == ':') {
            text = text[1..];
        }

        const name = try self.decodeSymbolToken(text);
        defer if (name.owned) |buf| self.alloc.free(buf);
        return self.internKeyword(name.slice);
    }

    /// Intern a symbol (same name = same Value)
    fn internSymbol(self: *Parser, name: []const u8) Error!Value {
        return try self.heap.intern(name);
    }

    /// Create an uninterned symbol (fresh Value each read)
    fn parseUninternedSymbol(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        if (text.len < 3) return error.UnexpectedToken;
        const name_raw = text[2..];
        if (name_raw.len == 0) return error.UnexpectedToken;
        const name = try self.decodeSymbolToken(name_raw);
        defer if (name.owned) |buf| self.alloc.free(buf);
        if (name.slice.len == 0) return error.UnexpectedToken;

        var upper_buf: [256]u8 = undefined;
        const upper = try runtime.upperNameAlloc(self.alloc, name.slice, upper_buf[0..]);
        defer runtime.freeUpperName(self.alloc, upper);

        return try self.heap.allocSymbol(upper.slice);
    }

    const DecodedTokenName = struct {
        slice: []const u8,
        owned: ?[]u8,
    };

    fn decodeSymbolToken(self: *Parser, raw: []const u8) Error!DecodedTokenName {
        if (std.mem.indexOfAny(u8, raw, "\\|") == null) {
            var has_lower = false;
            for (raw) |ch| {
                if (ch >= 'a' and ch <= 'z') {
                    has_lower = true;
                    break;
                }
            }
            if (!has_lower) {
                return .{ .slice = raw, .owned = null };
            }
            const buf = try self.alloc.alloc(u8, raw.len);
            errdefer self.alloc.free(buf);
            for (raw, 0..) |ch, i| {
                buf[i] = std.ascii.toUpper(ch);
            }
            return .{ .slice = buf, .owned = buf };
        }
        const buf = try self.alloc.alloc(u8, raw.len);
        errdefer self.alloc.free(buf);
        var out_i: usize = 0;
        var i: usize = 0;
        var in_bar = false;
        while (i < raw.len) {
            const c = raw[i];
            if (in_bar) {
                if (c == '\\') {
                    if (i + 1 >= raw.len) return error.UnexpectedToken;
                    buf[out_i] = raw[i + 1];
                    out_i += 1;
                    i += 2;
                    continue;
                }
                if (c == '|') {
                    in_bar = false;
                    i += 1;
                    continue;
                }
                buf[out_i] = c;
                out_i += 1;
                i += 1;
                continue;
            }
            if (c == '\\') {
                if (i + 1 >= raw.len) return error.UnexpectedToken;
                buf[out_i] = raw[i + 1];
                out_i += 1;
                i += 2;
                continue;
            }
            if (c == '|') {
                in_bar = true;
                i += 1;
                continue;
            }
            buf[out_i] = std.ascii.toUpper(c);
            out_i += 1;
            i += 1;
        }
        if (in_bar) return error.UnexpectedToken;
        return .{ .slice = buf[0..out_i], .owned = buf };
    }

    /// Intern a symbol in a specific package
    fn packageNativeName(pkg_name: Value) Error![]const u8 {
        return switch (pkg_name.typeKind()) {
            .symbol => pkg_name.toPtr(runtime.Symbol).getName(),
            .string => pkg_name.toPtr(runtime.String).bytes(),
            .keyword => pkg_name.toPtr(runtime.Keyword).getName(),
            else => error.TypeMismatch,
        };
    }

    fn internSymbolInPackage(
        self: *Parser,
        pkg_name: []const u8,
        sym_name: []const u8,
        external_only: bool,
    ) Error!Value {
        const pkg_val = try self.heap.findLispPackageBytes(pkg_name) orelse return error.InvalidPackage;
        if (!pkg_val.isPackage()) return error.TypeMismatch;

        const native_name = try packageNativeName(pkg_val.toPtr(objects.Package).name);
        const native_pkg = self.heap.findPackage(native_name) orelse return error.InvalidPackage;

        if (!external_only) {
            return try native_pkg.intern(self.heap, sym_name);
        }

        if (native_pkg.auto_export) {
            return native_pkg.symbols.get(sym_name) orelse error.SymbolNotExternal;
        }
        if (native_pkg.exports.get(sym_name) != null) {
            return native_pkg.symbols.get(sym_name) orelse error.SymbolNotExternal;
        }
        return error.SymbolNotExternal;
    }

    /// Intern a keyword (same name = same Value)
    fn internKeyword(self: *Parser, name: []const u8) Error!Value {
        return try self.heap.internKeyword(name);
    }

    fn parseCharacter(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        // Text is "#\..." - extract the character part after "#\"
        if (text.len < 3) return error.UnexpectedToken;
        const char_part = text[2..];

        // Single character
        if (char_part.len == 1) {
            return Value.makeCharacter(char_part[0]);
        }

        // Named characters (case-insensitive, per CL spec)
        if (std.ascii.eqlIgnoreCase(char_part, "space")) return Value.makeCharacter(' ');
        if (std.ascii.eqlIgnoreCase(char_part, "newline")) return Value.makeCharacter('\n');
        if (std.ascii.eqlIgnoreCase(char_part, "tab")) return Value.makeCharacter('\t');
        if (std.ascii.eqlIgnoreCase(char_part, "return")) return Value.makeCharacter('\r');
        if (std.ascii.eqlIgnoreCase(char_part, "backspace")) return Value.makeCharacter(0x08);
        if (std.ascii.eqlIgnoreCase(char_part, "linefeed")) return Value.makeCharacter('\n');
        if (std.ascii.eqlIgnoreCase(char_part, "page")) return Value.makeCharacter(0x0C);
        if (std.ascii.eqlIgnoreCase(char_part, "vt")) return Value.makeCharacter(0x0B);
        if (std.ascii.eqlIgnoreCase(char_part, "vertical-tab")) return Value.makeCharacter(0x0B);
        if (std.ascii.eqlIgnoreCase(char_part, "code11")) return Value.makeCharacter(0x0B);
        if (std.ascii.eqlIgnoreCase(char_part, "rubout")) return Value.makeCharacter(0x7F);
        if (std.ascii.eqlIgnoreCase(char_part, "nul") or std.ascii.eqlIgnoreCase(char_part, "null")) return Value.makeCharacter(0);

        // Unicode escape: #\uXXXX or #\U+XXXX
        if (char_part.len >= 2 and (char_part[0] == 'u' or char_part[0] == 'U')) {
            var hex_part = char_part[1..];
            if (hex_part.len > 0 and hex_part[0] == '+') hex_part = hex_part[1..];
            const codepoint = try std.fmt.parseInt(u21, hex_part, 16);
            return Value.makeCharacter(codepoint);
        }

        return error.UnexpectedToken;
    }

    fn parseFeatureConditional(self: *Parser, present: bool) Error!Value {
        self.advance(); // consume #+ or #-
        const feat = try self.parseExpr(); // feature expression

        // Evaluate feature expression
        const feat_present = try self.evalFeature(feat);

        // If feature matches conditional, parse and return form
        if (feat_present == present) {
            return try self.parseExpr();
        }

        // Otherwise consume one full form without constructing objects.
        try self.skipExpr();
        return error.SkipForm;
    }

    fn skipExpr(self: *Parser) Error!void {
        switch (self.current.kind) {
            .lparen, .vector_open, .complex_open, .struct_open, .array_open => {
                self.advance(); // consume opening token
                while (true) {
                    switch (self.current.kind) {
                        .rparen => {
                            self.advance();
                            return;
                        },
                        .eof => return error.UnterminatedList,
                        .dot => {
                            self.advance();
                            try self.skipExpr();
                            if (self.current.kind != .rparen) return error.UnexpectedToken;
                            self.advance();
                            return;
                        },
                        else => try self.skipExpr(),
                    }
                }
            },
            .quote, .backquote, .comma, .comma_at, .function_quote, .read_eval => {
                self.advance();
                try self.skipExpr();
            },
            .feature_present, .feature_absent => {
                self.advance(); // consume #+ or #-
                try self.skipExpr(); // feature expression
                try self.skipExpr(); // conditional form
            },
            .label_def => {
                self.advance(); // consume #n=
                try self.skipExpr();
            },
            .label_ref => {
                self.advance(); // consume #n#
            },
            .dispatch => {
                _ = try self.parseDispatch();
            },
            .number, .bignum, .float, .rational, .string, .symbol, .keyword, .uninterned_symbol, .character, .pathname, .bitvec => {
                self.advance();
            },
            .eof => return error.UnexpectedToken,
            .rparen, .dot, .err => return error.UnexpectedToken,
        }
    }

    fn evalFeature(self: *Parser, expr: Value) Error!bool {
        if (expr.isSymbol()) {
            // Convert symbol to keyword for comparison
            const sym = expr.toPtr(objects.Symbol);
            const name = sym.getName();
            const kw = try self.heap.internKeyword(name);
            for (self.features.items) |feat| {
                if (feat.eq(kw)) return true;
            }
            return false;
        }

        if (expr.isKeyword()) {
            for (self.features.items) |feat| {
                if (feat.eq(expr)) return true;
            }
            return false;
        }

        if (expr.isCons()) {
            const cons = expr.toPtr(objects.Cons);
            const head = cons.car;
            if (!head.isSymbol()) return false;

            if (head.eq(self.builtins.sym_and)) {
                var rest = cons.cdr;
                while (rest.isCons()) {
                    const arg_cons = rest.toPtr(objects.Cons);
                    if (!try self.evalFeature(arg_cons.car)) return false;
                    rest = arg_cons.cdr;
                }
                return true;
            }

            if (head.eq(self.builtins.sym_or)) {
                var rest = cons.cdr;
                while (rest.isCons()) {
                    const arg_cons = rest.toPtr(objects.Cons);
                    if (try self.evalFeature(arg_cons.car)) return true;
                    rest = arg_cons.cdr;
                }
                return false;
            }

            if (head.eq(self.builtins.sym_not)) {
                const arg_cons = cons.cdr.toPtr(objects.Cons);
                return !try self.evalFeature(arg_cons.car);
            }

            return false;
        }

        return false;
    }

    fn advance(self: *Parser) void {
        self.current = self.lexer.next();
    }

    fn advanceSource(self: *Parser, count: usize) void {
        var i: usize = 0;
        while (i < count and self.lexer.pos < self.lexer.source.len) : (i += 1) {
            const c = self.lexer.source[self.lexer.pos];
            self.lexer.pos += 1;
            if (c == '\n') {
                self.lexer.line += 1;
                self.lexer.column = 1;
            } else {
                self.lexer.column += 1;
            }
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

const Vm = @import("../interp/vm.zig").Vm;

const DispatchTestCtx = struct {
    expected_fn: Value,
    expected_sub_char: u8,
    expected_arg: ?u32,
};

fn parserDispatchCount(
    ctx: *anyopaque,
    function: Value,
    disp_char: u8,
    sub_char: u8,
    arg: ?u32,
    stream: Value,
) Error!Value {
    const hook: *DispatchTestCtx = @ptrCast(@alignCast(ctx));
    if (disp_char != '#') return error.UnexpectedToken;
    if (function.raw != hook.expected_fn.raw) return error.UnexpectedToken;
    if (sub_char != hook.expected_sub_char) return error.UnexpectedToken;
    if (arg != hook.expected_arg) return error.UnexpectedToken;
    if (!stream.isStream()) return error.UnexpectedToken;
    const s = stream.toPtr(runtime.Stream);
    if (s.stream_type != .string) return error.UnexpectedToken;
    const data: [*]const u8 = @ptrFromInt(s.data_ptr);
    while (s.position < s.length and data[s.position] != '$') : (s.position += 1) {}
    if (s.position < s.length and data[s.position] == '$') s.position += 1;
    return Value.makeFixnum(@intCast(s.position));
}

test "parse number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "42", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isFixnum());
    try testing.expectEqual(@as(i64, 42), val.toFixnum());
}

test "parse negative number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "-123", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isFixnum());
    try testing.expectEqual(@as(i64, -123), val.toFixnum());
}

test "parse large positive bignum" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(
        testing.allocator,
        &heap,
        "1234567890123456789012345678901234567890",
        &vm.builtins,
    );
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isBignum());
    const bn = val.toPtr(objects.Bignum);
    try testing.expect(bn.size > 0);
    try testing.expect(@as(u64, @intCast(bn.size)) >= 3);
}

test "parse large negative bignum" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(
        testing.allocator,
        &heap,
        "-12345678901234567890123456789012345678901234567890",
        &vm.builtins,
    );
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isBignum());
    const bn = val.toPtr(objects.Bignum);
    try testing.expect(bn.size < 0);
    try testing.expect(@as(u64, @intCast(-bn.size)) >= 3);
}

test "parse very large decimal bignum beyond 8 limbs" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var digits: [240]u8 = undefined;
    @memset(&digits, '9');

    var parser = try Parser.init(testing.allocator, &heap, digits[0..], &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isBignum());
    const bn = val.toPtr(objects.Bignum);
    try testing.expect(bn.size > 0);
    // Runtime bignum storage currently caps at 8 limbs.
    try testing.expectEqual(@as(u64, 8), @as(u64, @intCast(bn.size)));
}

test "parse nil" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "nil", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isNil());
}

test "parse quoted backslash-escaped symbol" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "'\\+", &vm.builtins);
    defer parser.deinit();

    const expr = try parser.parse();
    try testing.expect(expr.isCons());
    const quote_cell = expr.toPtr(runtime.Cons);
    try testing.expect(quote_cell.car.isSymbol());
    try testing.expectEqualStrings("QUOTE", quote_cell.car.toPtr(runtime.Symbol).getName());
    try testing.expect(quote_cell.cdr.isCons());
    const arg_cell = quote_cell.cdr.toPtr(runtime.Cons);
    try testing.expect(arg_cell.car.isSymbol());
    try testing.expectEqualStrings("+", arg_cell.car.toPtr(runtime.Symbol).getName());
}

test "feature conditional skips absent branch and keeps next form" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(
        testing.allocator,
        &heap,
        "#+ecl (si::package-lock nil nil)\n42",
        &vm.builtins,
    );
    defer parser.deinit();

    var results = std.ArrayList(Value){};
    defer results.deinit(testing.allocator);
    try parser.parseAll(testing.allocator, &results);

    try testing.expectEqual(@as(usize, 1), results.items.len);
    try testing.expect(results.items[0].isFixnum());
    try testing.expectEqual(@as(i64, 42), results.items[0].toFixnum());
}

test "feature conditionals inside list skip only guarded forms" {
    const testing = std.testing;
    const list = @import("../runtime/primitives/list.zig");

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(
        testing.allocator,
        &heap,
        "(let ((*default-pathname-defaults* *root-path*)) #+allegro (foo 1) #+clasp (bar 2) 42)",
        &vm.builtins,
    );
    defer parser.deinit();

    const expr = try parser.parse();
    try testing.expect(expr.isCons());
    try testing.expect(list.car(expr).isSymbol());

    const args = list.cdr(expr);
    try testing.expect(args.isCons());
    const body = list.cdr(args);
    try testing.expect(body.isCons());
    try testing.expect(list.car(body).isFixnum());
    try testing.expectEqual(@as(i64, 42), list.car(body).toFixnum());
    try testing.expect(list.cdr(body).isNil());
}

test "parse all skips feature-conditional forms with no matches" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "#+allegro 1 #-habu 2", &vm.builtins);
    defer parser.deinit();

    var results = std.ArrayList(Value){};
    defer results.deinit(testing.allocator);
    try parser.parseAll(testing.allocator, &results);

    try testing.expectEqual(@as(usize, 0), results.items.len);
}

test "feature conditional skip handles pathname forms" {
    const testing = std.testing;
    const list = @import("../runtime/primitives/list.zig");

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(
        testing.allocator,
        &heap,
        \\(let ((*default-pathname-defaults* *root-path*))
        \\  #+allegro
        \\  (rt:load-expected-failures #P"expected-failures/acl.sexp" :if-does-not-exist nil)
        \\
        \\  #+clasp
        \\  (rt:load-expected-failures #P"expected-failures/clasp.sexp" :if-does-not-exist nil)
        \\  42)
    ,
        &vm.builtins,
    );
    defer parser.deinit();

    const expr = try parser.parse();
    try testing.expect(expr.isCons());

    const args = list.cdr(expr);
    try testing.expect(args.isCons());
    const body = list.cdr(args);
    try testing.expect(body.isCons());
    try testing.expect(list.car(body).isFixnum());
    try testing.expectEqual(@as(i64, 42), list.car(body).toFixnum());
    try testing.expect(list.cdr(body).isNil());
}

test "feature conditional with vertical-tab character parses" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(
        testing.allocator,
        &heap,
        "(member #\\space '(#\\linefeed #\\return #\\space #\\tab #\\page #-(or clisp gcl openmcl abcl) #\\vt #+clisp #\\code11) :test #'char=)",
        &vm.builtins,
    );
    defer parser.deinit();

    const expr = try parser.parse();
    try testing.expect(expr.isCons());
}

test "parse uninterned symbols are fresh values" {
    const testing = std.testing;
    const list = @import("../runtime/primitives/list.zig");

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "(#:foo #:foo)", &vm.builtins);
    defer parser.deinit();

    const expr = try parser.parse();
    try testing.expect(expr.isCons());

    const first = list.car(expr);
    const second = list.car(list.cdr(expr));
    try testing.expect(first.isSymbol());
    try testing.expect(second.isSymbol());
    try testing.expect(!first.eq(second));
}

test "parse shared labels reuse symbol identity" {
    const testing = std.testing;
    const list = @import("../runtime/primitives/list.zig");

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "(#1=#:foo #1#)", &vm.builtins);
    defer parser.deinit();

    const expr = try parser.parse();
    try testing.expect(expr.isCons());

    const first = list.car(expr);
    const rest = list.cdr(expr);
    try testing.expect(rest.isCons());
    const second = list.car(rest);
    try testing.expect(first.isSymbol());
    try testing.expect(second.isSymbol());
    try testing.expect(first.eq(second));
}

test "parse all expressions" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "42 nil (1 2)", &vm.builtins);
    defer parser.deinit();

    var results = std.ArrayList(Value){};
    defer results.deinit(testing.allocator);

    try parser.parseAll(testing.allocator, &results);

    try testing.expectEqual(@as(usize, 3), results.items.len);
    try testing.expect(results.items[0].isFixnum());
    try testing.expectEqual(@as(i64, 42), results.items[0].toFixnum());
    try testing.expect(results.items[1].isNil());
    try testing.expect(results.items[2].isCons());
}

test "parse empty list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "()", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isNil());
}

test "parse simple list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "(1 2 3)", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());

    const list = @import("../runtime/primitives/list.zig");
    try testing.expectEqual(@as(i64, 1), list.car(val).toFixnum());
    try testing.expectEqual(@as(i64, 2), list.car(list.cdr(val)).toFixnum());
    try testing.expectEqual(@as(i64, 3), list.car(list.cdr(list.cdr(val))).toFixnum());
    try testing.expect(list.cdr(list.cdr(list.cdr(val))).isNil());
}

test "parse dotted pair" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "(1 . 2)", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());

    const list = @import("../runtime/primitives/list.zig");
    try testing.expectEqual(@as(i64, 1), list.car(val).toFixnum());
    try testing.expectEqual(@as(i64, 2), list.cdr(val).toFixnum());
}

test "parse nested list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "((1 2) (3 4))", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());

    const list = @import("../runtime/primitives/list.zig");
    const first = list.car(val);
    try testing.expect(first.isCons());
    try testing.expectEqual(@as(i64, 1), list.car(first).toFixnum());
}

test "parse string" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "\"hello\"", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isString());

    const string = @import("../runtime/primitives/string.zig");
    try testing.expectEqualStrings("hello", string.stringBytes(val).?);
}

test "parse symbol" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "foo", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isSymbol());

    const string = @import("../runtime/primitives/string.zig");
    try testing.expectEqualStrings("FOO", string.symbolNameBytes(val).?);
}

test "parse keyword" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, ":test", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isKeyword());

    const string = @import("../runtime/primitives/string.zig");
    try testing.expectEqualStrings("TEST", string.keywordNameBytes(val).?);
}

test "parse quote" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "'foo", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());

    const list = @import("../runtime/primitives/list.zig");
    const quote_sym = list.car(val);
    try testing.expect(quote_sym.isSymbol());

    const string = @import("../runtime/primitives/string.zig");
    try testing.expectEqualStrings("QUOTE", string.symbolNameBytes(quote_sym).?);
}

test "parse sharp dot fallback expression" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "#.(+ 1 2)", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());

    const list = @import("../runtime/primitives/list.zig");
    const head = list.car(val);
    try testing.expect(head.isSymbol());

    const string = @import("../runtime/primitives/string.zig");
    try testing.expectEqualStrings("+", string.symbolNameBytes(head).?);
}

test "parse dispatch macro hook consumes stream" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const marker_fn = vm.builtins.sym_quote;
    const gop = try heap.dispatch_readtable.getOrPut(testing.allocator, '#');
    if (!gop.found_existing) {
        gop.value_ptr.* = .{};
    }
    try gop.value_ptr.put(testing.allocator, '$', marker_fn);

    var parser = try Parser.init(testing.allocator, &heap, "#12$abc$ 7", &vm.builtins);
    defer parser.deinit();
    var dispatch_ctx = DispatchTestCtx{
        .expected_fn = marker_fn,
        .expected_sub_char = '$',
        .expected_arg = 12,
    };
    parser.setDispatchMacroHook(@ptrCast(&dispatch_ctx), parserDispatchCount);

    const first = try parser.parse();
    try testing.expect(first.isFixnum());
    // "abc$" consumed from the dispatch stream.
    try testing.expectEqual(@as(i64, 4), first.toFixnum());

    const second = try parser.parse();
    try testing.expect(second.isFixnum());
    try testing.expectEqual(@as(i64, 7), second.toFixnum());
}

test "parse comma dot as unquote-splicing" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "`((%derivative) ,expr ,.(nreverse old-wrt))", &vm.builtins);
    defer parser.deinit();

    const expr = try parser.parse();
    try testing.expect(expr.isCons());
}

test "symbol interning" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "foo foo", &vm.builtins);
    defer parser.deinit();

    const sym1 = try parser.parse();
    const sym2 = try parser.parse();

    // Same symbol should have same address (interned)
    try testing.expectEqual(sym1.raw, sym2.raw);
}

test "parse expression" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "(+ 1 2)", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());

    const list = @import("../runtime/primitives/list.zig");
    const string = @import("../runtime/primitives/string.zig");

    const op = list.car(val);
    try testing.expect(op.isSymbol());
    try testing.expectEqualStrings("+", string.symbolNameBytes(op).?);

    try testing.expectEqual(@as(i64, 1), list.car(list.cdr(val)).toFixnum());
    try testing.expectEqual(@as(i64, 2), list.car(list.cdr(list.cdr(val))).toFixnum());
}

test "parse hex number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #x20 = 32
    var parser1 = try Parser.init(testing.allocator, &heap, "#x20", &vm.builtins);
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.isFixnum());
    try testing.expectEqual(@as(i64, 32), val1.toFixnum());

    // #xFF = 255
    var parser2 = try Parser.init(testing.allocator, &heap, "#xFF", &vm.builtins);
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expectEqual(@as(i64, 255), val2.toFixnum());

    // #xABCD = 43981
    var parser3 = try Parser.init(testing.allocator, &heap, "#xABCD", &vm.builtins);
    defer parser3.deinit();
    const val3 = try parser3.parse();
    try testing.expectEqual(@as(i64, 43981), val3.toFixnum());

    // Case insensitive: #X1a2B = 6699
    var parser4 = try Parser.init(testing.allocator, &heap, "#X1a2B", &vm.builtins);
    defer parser4.deinit();
    const val4 = try parser4.parse();
    try testing.expectEqual(@as(i64, 6699), val4.toFixnum());

    // Overflowing radix literals should produce bignum, not parse errors.
    var parser5 = try Parser.init(
        testing.allocator,
        &heap,
        "#xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
        &vm.builtins,
    );
    defer parser5.deinit();
    const val5 = try parser5.parse();
    try testing.expect(val5.isBignum());
}

test "parse binary number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #b101 = 5
    var parser1 = try Parser.init(testing.allocator, &heap, "#b101", &vm.builtins);
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.isFixnum());
    try testing.expectEqual(@as(i64, 5), val1.toFixnum());

    // #B11111111 = 255
    var parser2 = try Parser.init(testing.allocator, &heap, "#B11111111", &vm.builtins);
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expectEqual(@as(i64, 255), val2.toFixnum());

    // #b0 = 0
    var parser3 = try Parser.init(testing.allocator, &heap, "#b0", &vm.builtins);
    defer parser3.deinit();
    const val3 = try parser3.parse();
    try testing.expectEqual(@as(i64, 0), val3.toFixnum());
}

test "parse 21 parameter lambda" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var parser = try Parser.init(testing.allocator, &heap, "(lambda (a b c d e f g h i j k l m n o p q r s t u) u)", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());

    const cons1 = val.toPtr(Cons);
    const params = cons1.cdr.toPtr(Cons).car;

    var param_list = params;
    var count: u32 = 0;
    while (param_list.isCons()) {
        const param_cons = param_list.toPtr(Cons);
        const param_item = param_cons.car;
        count += 1;
        if (!param_item.isSymbolLike()) {
            return error.TestExpectedSymbol;
        }
        param_list = param_cons.cdr;
    }

    try testing.expectEqual(@as(u32, 21), count);
}

test "parse #S struct" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #S(point :x 10 :y 20) -> (make-struct 'point :x 10 :y 20)
    var parser = try Parser.init(testing.allocator, &heap, "#S(point :x 10 :y 20)", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isCons());

    const cons = result.toPtr(objects.Cons);
    try testing.expect(cons.car.isSymbol());

    const sym = cons.car.toPtr(objects.Symbol);
    try testing.expectEqualStrings("MAKE-STRUCT", sym.getName());

    // Check second element is quoted name
    const rest1 = cons.cdr;
    try testing.expect(rest1.isCons());
    const rest1_cons = rest1.toPtr(objects.Cons);
    try testing.expect(rest1_cons.car.isCons());

    // (quote point)
    const quote_form = rest1_cons.car.toPtr(objects.Cons);
    try testing.expect(quote_form.car.isSymbol());
    const quote_sym = quote_form.car.toPtr(objects.Symbol);
    try testing.expectEqualStrings("QUOTE", quote_sym.getName());
}

test "parse #S empty struct" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var parser = try Parser.init(testing.allocator, &heap, "#S(empty)", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isCons());

    const cons = result.toPtr(objects.Cons);
    const sym = cons.car.toPtr(objects.Symbol);
    try testing.expectEqualStrings("MAKE-STRUCT", sym.getName());
}

test "parse #A array" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #A((1 2 3)) -> rank-2 array literal with dimensions (1 3)
    var parser = try Parser.init(testing.allocator, &heap, "#A((1 2 3))", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isArray());

    const arr = result.toPtr(objects.Array);
    try testing.expectEqual(@as(u8, 2), arr.rank);
    try testing.expectEqual(@as(u64, 1), arr.dimensions[0]);
    try testing.expectEqual(@as(u64, 3), arr.dimensions[1]);
    const data: [*]Value = @ptrFromInt(arr.data_ptr);
    try testing.expectEqual(@as(i64, 1), data[0].toFixnum());
    try testing.expectEqual(@as(i64, 2), data[1].toFixnum());
    try testing.expectEqual(@as(i64, 3), data[2].toFixnum());
}

test "parse #2A array" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #2A((1 2) (3 4)) -> rank-2 array literal with dimensions (2 2)
    var parser = try Parser.init(testing.allocator, &heap, "#2A((1 2) (3 4))", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isArray());

    const arr = result.toPtr(objects.Array);
    try testing.expectEqual(@as(u8, 2), arr.rank);
    try testing.expectEqual(@as(u64, 2), arr.dimensions[0]);
    try testing.expectEqual(@as(u64, 2), arr.dimensions[1]);
    const data: [*]Value = @ptrFromInt(arr.data_ptr);
    try testing.expectEqual(@as(i64, 1), data[0].toFixnum());
    try testing.expectEqual(@as(i64, 2), data[1].toFixnum());
    try testing.expectEqual(@as(i64, 3), data[2].toFixnum());
    try testing.expectEqual(@as(i64, 4), data[3].toFixnum());
}

test "parse #2A array with cons elements" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // Terminal rank elements may be arbitrary objects, including conses.
    var parser = try Parser.init(testing.allocator, &heap, "#2A((4 (17)) (9 (a)) ((b) 0))", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isArray());

    const arr = result.toPtr(objects.Array);
    try testing.expectEqual(@as(u8, 2), arr.rank);
    try testing.expectEqual(@as(u64, 3), arr.dimensions[0]);
    try testing.expectEqual(@as(u64, 2), arr.dimensions[1]);
    const data: [*]Value = @ptrFromInt(arr.data_ptr);
    try testing.expect(data[1].isCons());
    const row1_col2 = data[1].toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 17), row1_col2.car.toFixnum());
    try testing.expect(row1_col2.cdr.isNil());
    try testing.expect(data[3].isCons());
    const row2_col2 = data[3].toPtr(objects.Cons);
    try testing.expect(row2_col2.car.isSymbol());
    try testing.expectEqualStrings("A", row2_col2.car.toPtr(objects.Symbol).getName());
}

test "parse #0A array" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var parser = try Parser.init(testing.allocator, &heap, "#0A()", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isCons());

    const cons = result.toPtr(objects.Cons);
    const sym = cons.car.toPtr(objects.Symbol);
    try testing.expectEqualStrings("MAKE-ARRAY", sym.getName());

    const args = cons.cdr;
    try testing.expect(args.isCons());
    const args_cons = args.toPtr(objects.Cons);
    const dims = args_cons.car;
    try testing.expect(dims.isNil());
    try testing.expect(args_cons.cdr.isNil());
}

test "parse #0A scalar array" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var parser = try Parser.init(testing.allocator, &heap, "#0AT", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isCons());

    const cons = result.toPtr(objects.Cons);
    const sym = cons.car.toPtr(objects.Symbol);
    try testing.expectEqualStrings("MAKE-ARRAY", sym.getName());

    const args = cons.cdr;
    try testing.expect(args.isCons());
    const args_cons = args.toPtr(objects.Cons);
    try testing.expect(args_cons.car.isNil());
    try testing.expect(args_cons.cdr.isCons());

    const kw_pair = args_cons.cdr.toPtr(objects.Cons);
    try testing.expect(kw_pair.car.isKeyword());
    try testing.expectEqualStrings("INITIAL-ELEMENT", kw_pair.car.toPtr(objects.Symbol).getName());
}

test "parse #2A ragged errors" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var parser = try Parser.init(testing.allocator, &heap, "#2A((1 2) (3))", &vm.builtins);
    defer parser.deinit();

    try testing.expectError(error.InvalidArray, parser.parse());
}

test "parse #P pathname" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #P"/path/to/file" -> (parse-namestring "/path/to/file")
    var parser = try Parser.init(testing.allocator, &heap, "#P\"/path/to/file\"", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isCons());

    const cons = result.toPtr(objects.Cons);
    const sym = cons.car.toPtr(objects.Symbol);
    try testing.expectEqualStrings("PARSE-NAMESTRING", sym.getName());
}

test "parse #C complex number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #C(3 4) = 3+4i
    var parser1 = try Parser.init(testing.allocator, &heap, "#C(3 4)", &vm.builtins);
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.typeKind() == .complex);
    const c1 = val1.toPtr(objects.Complex);
    try testing.expectApproxEqAbs(@as(f64, 3.0), c1.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 4.0), c1.imag, 0.0001);

    // #C(1.5 2.5) = 1.5+2.5i
    var parser2 = try Parser.init(testing.allocator, &heap, "#C(1.5 2.5)", &vm.builtins);
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expect(val2.typeKind() == .complex);
    const c2 = val2.toPtr(objects.Complex);
    try testing.expectApproxEqAbs(@as(f64, 1.5), c2.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 2.5), c2.imag, 0.0001);

    // #C(0 -1) = -i
    var parser3 = try Parser.init(testing.allocator, &heap, "#C(0 -1)", &vm.builtins);
    defer parser3.deinit();
    const val3 = try parser3.parse();
    try testing.expect(val3.typeKind() == .complex);
    const c3 = val3.toPtr(objects.Complex);
    try testing.expectApproxEqAbs(@as(f64, 0.0), c3.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, -1.0), c3.imag, 0.0001);

    // #C(1/2 1/3) = 0.5+0.333...i (rational parts)
    var parser4 = try Parser.init(testing.allocator, &heap, "#C(1/2 1/3)", &vm.builtins);
    defer parser4.deinit();
    const val4 = try parser4.parse();
    try testing.expect(val4.typeKind() == .complex);
    const c4 = val4.toPtr(objects.Complex);
    try testing.expectApproxEqAbs(@as(f64, 0.5), c4.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 0.3333), c4.imag, 0.0001);

    // #C(1 0) canonicalizes to the real part.
    var parser5 = try Parser.init(testing.allocator, &heap, "#C(1 0)", &vm.builtins);
    defer parser5.deinit();
    const val5 = try parser5.parse();
    try testing.expect(val5.isFixnum());
    try testing.expectEqual(@as(i64, 1), val5.toFixnum());

    // #c may include whitespace before the complex list.
    var parser6 = try Parser.init(testing.allocator, &heap, "#c (1 1)", &vm.builtins);
    defer parser6.deinit();
    const val6 = try parser6.parse();
    try testing.expect(val6.typeKind() == .complex);
    const c6 = val6.toPtr(objects.Complex);
    try testing.expectApproxEqAbs(@as(f64, 1.0), c6.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 1.0), c6.imag, 0.0001);

    // #C("x" 1) is invalid
    var parser_bad = try Parser.init(testing.allocator, &heap, "#C(\"x\" 1)", &vm.builtins);
    defer parser_bad.deinit();
    try testing.expectError(error.TypeMismatch, parser_bad.parse());
}

test "parse #* bit vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #*101010 -> vector of 6 bits
    var parser1 = try Parser.init(testing.allocator, &heap, "#*101010", &vm.builtins);
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.typeKind() == .vector);
    const vec1 = val1.toPtr(objects.Vector);
    try testing.expectEqual(@as(usize, 6), vec1.length);
    try testing.expectEqual(@as(i64, 1), vec1.data[0].toFixnum());
    try testing.expectEqual(@as(i64, 0), vec1.data[1].toFixnum());
    try testing.expectEqual(@as(i64, 1), vec1.data[2].toFixnum());
    try testing.expectEqual(@as(i64, 0), vec1.data[3].toFixnum());
    try testing.expectEqual(@as(i64, 1), vec1.data[4].toFixnum());
    try testing.expectEqual(@as(i64, 0), vec1.data[5].toFixnum());

    // #*0 -> single bit
    var parser2 = try Parser.init(testing.allocator, &heap, "#*0", &vm.builtins);
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expect(val2.typeKind() == .vector);
    const vec2 = val2.toPtr(objects.Vector);
    try testing.expectEqual(@as(usize, 1), vec2.length);
    try testing.expectEqual(@as(i64, 0), vec2.data[0].toFixnum());

    // #* -> empty bit vector
    var parser3 = try Parser.init(testing.allocator, &heap, "#*", &vm.builtins);
    defer parser3.deinit();
    const val3 = try parser3.parse();
    try testing.expect(val3.typeKind() == .vector);
    const vec3 = val3.toPtr(objects.Vector);
    try testing.expectEqual(@as(usize, 0), vec3.length);
}

test "parse octal number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #o77 = 63
    var parser1 = try Parser.init(testing.allocator, &heap, "#o77", &vm.builtins);
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.isFixnum());
    try testing.expectEqual(@as(i64, 63), val1.toFixnum());

    // #O755 = 493
    var parser2 = try Parser.init(testing.allocator, &heap, "#O755", &vm.builtins);
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expectEqual(@as(i64, 493), val2.toFixnum());

    // #o0 = 0
    var parser3 = try Parser.init(testing.allocator, &heap, "#o0", &vm.builtins);
    defer parser3.deinit();
    const val3 = try parser3.parse();
    try testing.expectEqual(@as(i64, 0), val3.toFixnum());

    // #o-777 = -511
    var parser4 = try Parser.init(testing.allocator, &heap, "#o-777", &vm.builtins);
    defer parser4.deinit();
    const val4 = try parser4.parse();
    try testing.expectEqual(@as(i64, -511), val4.toFixnum());
}

test "parse arbitrary radix number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var parser1 = try Parser.init(testing.allocator, &heap, "#3r2120012102", &vm.builtins);
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.isFixnum());
    try testing.expectEqual(@as(i64, 50447), val1.toFixnum());

    var parser2 = try Parser.init(testing.allocator, &heap, "#16r-ff", &vm.builtins);
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expect(val2.isFixnum());
    try testing.expectEqual(@as(i64, -255), val2.toFixnum());
}

test "parse def-format-test form with #3r literal" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const src =
        "(def-format-test format.r.13 \"~3@:r\" (#3r2120012102) \"+2,120,012,102\")";
    var parser = try Parser.init(testing.allocator, &heap, src, &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());
    const head = val.toPtr(objects.Cons).car;
    try testing.expect(head.isSymbol());
    try testing.expectEqualStrings("DEF-FORMAT-TEST", head.toPtr(runtime.Symbol).getName());
}

test "parse symbol with escaped colon does not trigger package lookup" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var parser = try Parser.init(testing.allocator, &heap, "format.\\:_.1", &vm.builtins);
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isSymbol());
    try testing.expectEqualStrings("FORMAT.:_.1", val.toPtr(runtime.Symbol).getName());
}

test "parse quoted symbol with escaped colon" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var parser = try Parser.init(testing.allocator, &heap, "(quote format.\\:_.1)", &vm.builtins);
    defer parser.deinit();

    const form = try parser.parse();
    try testing.expect(form.isCons());
    const quote_sym = form.toPtr(objects.Cons).car;
    try testing.expect(quote_sym.isSymbol());
    try testing.expectEqualStrings("QUOTE", quote_sym.toPtr(runtime.Symbol).getName());

    const rest = form.toPtr(objects.Cons).cdr;
    try testing.expect(rest.isCons());
    const escaped_sym = rest.toPtr(objects.Cons).car;
    try testing.expect(escaped_sym.isSymbol());
    try testing.expectEqualStrings("FORMAT.:_.1", escaped_sym.toPtr(runtime.Symbol).getName());
}

test "parse package-qualified symbol requires package to exist" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var parser = try Parser.init(testing.allocator, &heap, "missing::foo", &vm.builtins);
    defer parser.deinit();

    try testing.expectError(error.InvalidPackage, parser.parse());
    try testing.expect(heap.findPackage("MISSING") == null);
}

test "parse package-qualified symbol with single colon requires export" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const pkg = try primitives.package.makePackage(&heap, try heap.allocBaseString("TEST-PKG"), null, null);
    _ = try primitives.package.internSymbol(&heap, try heap.allocBaseString("FOO"), pkg);

    var parser = try Parser.init(testing.allocator, &heap, "test-pkg:foo", &vm.builtins);
    defer parser.deinit();
    try testing.expectError(error.SymbolNotExternal, parser.parse());

    const result = try primitives.package.internSymbol(&heap, try heap.allocBaseString("BAR"), pkg);
    try primitives.package.exportSymbols(&heap, result.toPtr(objects.Cons).car, pkg);

    var parser2 = try Parser.init(testing.allocator, &heap, "test-pkg:bar", &vm.builtins);
    defer parser2.deinit();
    const val = try parser2.parse();
    try testing.expect(val.isSymbol());
    try testing.expectEqualStrings("BAR", val.toPtr(runtime.Symbol).getName());
}
