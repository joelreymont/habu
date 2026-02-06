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
    Overflow,
};

pub const Parser = struct {
    pub const ReadEvalFn = *const fn (ctx: *anyopaque, expr: Value) Error!Value;

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
            return try hook(ctx, expr);
        }
        // Fallback when no read-eval hook is configured.
        return expr;
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
            const second = self.parseExpr() catch |err| switch (err) {
                error.SkipForm => return error.UnexpectedToken,
                else => return err,
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
            const cdr = self.parseExpr() catch |err| switch (err) {
                error.SkipForm => return error.UnexpectedToken,
                else => return err,
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
        // #2A( -> "2A(" text includes opening paren
        var rank: ?i64 = null;
        if (token_text.len > 2 and token_text[1] >= '0' and token_text[1] <= '9') {
            const digits_end = blk: {
                for (token_text[1..], 1..) |ch, idx| {
                    if (ch == 'A' or ch == 'a') break :blk idx;
                }
                break :blk token_text.len;
            };
            rank = try std.fmt.parseInt(i64, token_text[1..digits_end], 10);
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

            const initial_element_kw = try self.internKeyword("initial-element");
            const kw_pair = try self.heap.allocCons(initial_element_kw, try self.heap.allocCons(scalar, Value.nil));
            const args = try self.heap.allocCons(dims, kw_pair);
            return try self.heap.allocCons(make_array_sym, args);
        }

        // Parse all contents until closing paren (similar to parseList)
        if (self.current.kind == .rparen) {
            self.advance();
            // Empty array
            const make_array_sym = try self.internSymbol("make-array");
            const dims = Value.makeFixnum(0);
            const args = try self.heap.allocCons(dims, Value.nil);
            return try self.heap.allocCons(make_array_sym, args);
        }

        const first = self.parseExpr() catch |err| switch (err) {
            error.SkipForm => return self.parseListTail(),
            else => return err,
        };
        const rest = try self.parseListTail();
        const contents = try self.heap.allocCons(first, rest);

        // Build (make-array dims :initial-contents contents)
        const make_array_sym = try self.internSymbol("make-array");
        const initial_contents_kw = try self.internKeyword("initial-contents");

        // Build dims argument based on rank
        const rank_val: ?usize = if (rank) |r| blk: {
            const ur_opt = std.math.cast(usize, r);
            if (ur_opt == null) return error.InvalidArray;
            const ur = ur_opt.?;
            if (ur == 0) return error.InvalidArray;
            break :blk ur;
        } else null;
        const dims_arg = try self.inferArrayDims(contents, rank_val);

        // Build argument list: (dims :initial-contents contents)
        const kw_pair = try self.heap.allocCons(initial_contents_kw, try self.heap.allocCons(contents, Value.nil));
        const args = try self.heap.allocCons(dims_arg, kw_pair);

        return try self.heap.allocCons(make_array_sym, args);
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
                if (nested) {
                    if (!elem.isCons() and !elem.isNil()) return error.InvalidArray;
                } else {
                    if (elem.isCons()) return error.InvalidArray;
                }
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

    fn parseNumber(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        // Check for radix prefixes: #x (hex), #b (binary), #o (octal)
        if (text.len >= 2 and text[0] == '#') {
            const radix_char = text[1];
            const digits = text[2..];
            if (digits.len == 0) return error.InvalidNumber;

            const radix: u8 = switch (radix_char) {
                'x', 'X' => 16,
                'b', 'B' => 2,
                'o', 'O' => 8,
                else => return error.InvalidNumber,
            };
            const n = try std.fmt.parseInt(i64, digits, radix);
            return Value.makeFixnum(n);
        }

        const n = try std.fmt.parseInt(i64, text, 10);
        return Value.makeFixnum(n);
    }

    fn parseBignum(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        if (text.len == 0) return error.InvalidNumber;

        const is_negative = text[0] == '-';
        const digits = if (text[0] == '-' or text[0] == '+') text[1..] else text;
        if (digits.len == 0) return error.InvalidNumber;

        var limbs: [8]u64 = [_]u64{0} ** 8;

        // Parse arbitrary-length decimal by repeated multiply-by-10 and add-digit.
        for (digits) |ch| {
            if (ch < '0' or ch > '9') return error.InvalidNumber;
            var carry: u128 = @as(u128, ch - '0');
            var i: usize = 0;
            while (i < limbs.len) : (i += 1) {
                const acc: u128 = (@as(u128, limbs[i]) * 10) + carry;
                limbs[i] = @truncate(acc);
                carry = acc >> 64;
            }
            // Current Bignum stores up to 8 limbs; reject values that overflow this capacity.
            if (carry != 0) return error.InvalidNumber;
        }

        return self.heap.allocBignumFromLimbs(&limbs, is_negative);
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

        return str_val;
    }

    fn parseSymbol(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        // Check for package-qualified symbol (pkg:sym or pkg::sym)
        if (std.mem.indexOf(u8, text, ":")) |colon_pos| {
            if (colon_pos > 0) {
                const pkg_name = text[0..colon_pos];
                // Skip one or two colons
                var sym_start = colon_pos + 1;
                if (sym_start < text.len and text[sym_start] == ':') {
                    sym_start += 1;
                }
                if (sym_start >= text.len) {
                    // Just "pkg:" or "pkg::" with no symbol name
                    return error.UnexpectedToken;
                }
                const sym_name = text[sym_start..];
                return self.internSymbolInPackage(pkg_name, sym_name);
            }
        }

        return self.internSymbol(text);
    }

    fn parseKeyword(self: *Parser) Error!Value {
        var text = self.current.text;
        self.advance();

        // Remove leading colon for storage
        if (text.len > 0 and text[0] == ':') {
            text = text[1..];
        }

        return self.internKeyword(text);
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
        const name = text[2..];
        if (name.len == 0) return error.UnexpectedToken;

        var upper_buf: [256]u8 = undefined;
        const upper = try runtime.upperNameAlloc(self.alloc, name, upper_buf[0..]);
        defer runtime.freeUpperName(self.alloc, upper);

        return try self.heap.allocSymbol(upper.slice);
    }

    /// Intern a symbol in a specific package
    fn internSymbolInPackage(self: *Parser, pkg_name: []const u8, sym_name: []const u8) Error!Value {
        // Uppercase package name for CL-spec case-insensitivity
        var upper_buf: [128]u8 = undefined;
        const upper = try runtime.upperNameAlloc(self.alloc, pkg_name, upper_buf[0..]);
        defer runtime.freeUpperName(self.alloc, upper);
        const pkg_name_val = try self.heap.allocBaseString(upper.slice);
        const pkg_val = (self.heap.findLispPackage(pkg_name_val) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.TypeError => return error.UnexpectedToken,
        });
        const native_pkg = if (pkg_val) |val| blk: {
            if (!val.isPackage()) return error.UnexpectedToken;
            const pkg_obj = val.toPtr(objects.Package);
            const native_name = switch (pkg_obj.name.typeKind()) {
                .symbol => pkg_obj.name.toPtr(runtime.Symbol).getName(),
                .string => pkg_obj.name.toPtr(runtime.String).bytes(),
                .keyword => pkg_obj.name.toPtr(runtime.Keyword).getName(),
                else => return error.UnexpectedToken,
            };
            break :blk (self.heap.findPackage(native_name) orelse return error.UnexpectedToken);
        } else blk: {
            // Keep reader/package behavior consistent for native-only packages
            // created via compiler special forms (e.g. DEFPACKAGE in test files).
            break :blk (self.heap.findPackage(upper.slice) orelse return error.UnexpectedToken);
        };
        return try native_pkg.intern(self.heap, sym_name);
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
};

// ============================================================================
// Tests
// ============================================================================

const Vm = @import("../interp/vm.zig").Vm;

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

    // #A((1 2 3)) -> (make-array (1 3) :initial-contents (1 2 3))
    var parser = try Parser.init(testing.allocator, &heap, "#A((1 2 3))", &vm.builtins);
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
    try testing.expect(dims.isCons());
    const d1 = dims.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), d1.car.toFixnum());
    try testing.expect(d1.cdr.isCons());
    const d2 = d1.cdr.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 3), d2.car.toFixnum());
    try testing.expect(d2.cdr.isNil());
}

test "parse #2A array" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    // #2A((1 2) (3 4)) -> (make-array (2 2) :initial-contents ((1 2) (3 4)))
    var parser = try Parser.init(testing.allocator, &heap, "#2A((1 2) (3 4))", &vm.builtins);
    defer parser.deinit();

    const result = try parser.parse();
    try testing.expect(result.isCons());

    const cons = result.toPtr(objects.Cons);
    const sym = cons.car.toPtr(objects.Symbol);
    try testing.expectEqualStrings("MAKE-ARRAY", sym.getName());

    // Check dims argument is (2 2)
    const args = cons.cdr;
    try testing.expect(args.isCons());
    const args_cons = args.toPtr(objects.Cons);
    const dims = args_cons.car;
    try testing.expect(dims.isCons());
    const d1 = dims.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 2), d1.car.toFixnum());
    try testing.expect(d1.cdr.isCons());
    const d2 = d1.cdr.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 2), d2.car.toFixnum());
    try testing.expect(d2.cdr.isNil());
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
}
