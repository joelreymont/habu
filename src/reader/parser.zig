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
const primitives = @import("../runtime/primitives/primitives.zig");

pub const Error = error{
    UnexpectedToken,
    UnterminatedList,
    InvalidNumber,
    VectorTooLarge,
    OutOfMemory,
    TypeMismatch,
};

pub const Parser = struct {
    lexer: Lexer,
    heap: *Heap,
    current: Token,

    pub fn init(_: std.mem.Allocator, heap: *Heap, source: []const u8) Parser {
        var lexer = Lexer.init(source);
        const first_token = lexer.next();

        return .{
            .lexer = lexer,
            .heap = heap,
            .current = first_token,
        };
    }

    pub fn deinit(_: *Parser) void {
        // Symbol table is now owned by Heap - nothing to clean up here
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
        return self.parseExpr();
    }

    /// Parse all expressions until EOF
    pub fn parseAll(self: *Parser, allocator: std.mem.Allocator, results: *std.ArrayList(Value)) Error!void {
        while (self.current.kind != .eof) {
            const expr = try self.parseExpr();
            results.append(allocator, expr) catch return error.OutOfMemory;
        }
    }

    fn parseExpr(self: *Parser) Error!Value {
        switch (self.current.kind) {
            .lparen => return self.parseList(),
            .vector_open => return self.parseVector(),
            .complex_open => return self.parseComplex(),
            .quote => return self.parseQuote("quote"),
            .backquote => return self.parseQuote("quasiquote"),
            .comma => return self.parseQuote("unquote"),
            .comma_at => return self.parseQuote("unquote-splicing"),
            .function_quote => return self.parseQuote("function"),
            .number => return self.parseNumber(),
            .float => return self.parseFloat(),
            .rational => return self.parseRational(),
            .string => return self.parseString(),
            .symbol => return self.parseSymbol(),
            .keyword => return self.parseKeyword(),
            .character => return self.parseCharacter(),
            .eof => return Value.nil,
            .rparen, .dot => return error.UnexpectedToken,
            .err => return error.UnexpectedToken,
        }
    }

    fn parseList(self: *Parser) Error!Value {
        self.advance(); // consume '('

        if (self.current.kind == .rparen) {
            self.advance();
            return Value.nil;
        }

        // Parse first element
        const first = try self.parseExpr();

        // Check for dotted pair
        if (self.current.kind == .dot) {
            self.advance(); // consume '.'
            const second = try self.parseExpr();

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
            const cdr = try self.parseExpr();
            if (self.current.kind != .rparen) {
                return error.UnexpectedToken;
            }
            self.advance();
            return cdr;
        }

        const car = try self.parseExpr();
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
            elements[count] = try self.parseExpr();
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

        // Convert to floats
        var real: f64 = 0.0;
        var imag: f64 = 0.0;

        if (real_val.isFixnum()) {
            real = @floatFromInt(real_val.toFixnum());
        } else if (real_val.isFloat()) {
            real = real_val.toFloat();
        } else {
            return error.TypeMismatch;
        }

        if (imag_val.isFixnum()) {
            imag = @floatFromInt(imag_val.toFixnum());
        } else if (imag_val.isFloat()) {
            imag = imag_val.toFloat();
        } else {
            return error.TypeMismatch;
        }

        return primitives.complex.makeComplex(self.heap, real, imag);
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
            const n = std.fmt.parseInt(i64, digits, radix) catch return error.InvalidNumber;
            return Value.makeFixnum(n);
        }

        const n = std.fmt.parseInt(i64, text, 10) catch return error.InvalidNumber;
        return Value.makeFixnum(n);
    }

    fn parseFloat(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        const f = std.fmt.parseFloat(f64, text) catch return error.InvalidNumber;
        return Value.makeFloat(f);
    }

    fn parseRational(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        // Split on '/'
        const slash_pos = std.mem.indexOf(u8, text, "/") orelse return error.InvalidNumber;
        const num_str = text[0..slash_pos];
        const den_str = text[slash_pos + 1 ..];

        const num = std.fmt.parseInt(i64, num_str, 10) catch return error.InvalidNumber;
        const den = std.fmt.parseInt(i64, den_str, 10) catch return error.InvalidNumber;

        return primitives.rational.makeRational(self.heap, num, den);
    }


    fn parseString(self: *Parser) Error!Value {
        var text = self.current.text;
        self.advance();

        // Remove quotes
        if (text.len >= 2) {
            text = text[1 .. text.len - 1];
        }

        // Check if we need to decode escapes
        if (std.mem.indexOf(u8, text, "\\")) |_| {
            // Has escapes - decode them directly into allocated string
            return self.decodeStringEscapes(text) orelse error.OutOfMemory;
        }

        return try self.heap.allocString(text);
    }

    /// Decode escape sequences in a string, allocating result in heap's string space
    fn decodeStringEscapes(self: *Parser, text: []const u8) ?Value {
        // First pass: count output size
        var out_len: usize = 0;
        var i: usize = 0;
        while (i < text.len) {
            if (text[i] == '\\' and i + 1 < text.len) {
                out_len += 1;
                i += 2;
            } else {
                out_len += 1;
                i += 1;
            }
        }

        // Allocate string with uninitialized content
        const str_val = self.heap.allocStringUninitialized(out_len) catch return null;
        const str = str_val.toPtr(runtime.String);
        const buffer = str.mutableBytes();

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
                    '0' => 0, // null character
                    else => next, // Unknown escape - keep as-is
                };
                out_idx += 1;
                i += 2;
            } else {
                buffer[out_idx] = text[i];
                out_idx += 1;
                i += 1;
            }
        }

        return str_val;
    }

    fn parseSymbol(self: *Parser) Error!Value {
        const text = self.current.text;
        self.advance();

        // Check for nil and t (special symbols)
        if (std.mem.eql(u8, text, "nil")) {
            return Value.nil;
        }
        if (std.mem.eql(u8, text, "t")) {
            return Value.t;
        }

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

    /// Intern a symbol in a specific package
    fn internSymbolInPackage(self: *Parser, pkg_name: []const u8, sym_name: []const u8) Error!Value {
        // Uppercase package name for CL-spec case-insensitivity
        var upper_buf: [128]u8 = undefined;
        const upper_name = if (pkg_name.len <= upper_buf.len) blk: {
            for (pkg_name, 0..) |c, i| {
                upper_buf[i] = std.ascii.toUpper(c);
            }
            break :blk upper_buf[0..pkg_name.len];
        } else pkg_name; // Fallback for very long names
        // Find or create the package
        const pkg = try self.heap.findOrCreatePackage(upper_name);
        return try pkg.intern(self.heap, sym_name);
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
            const codepoint = std.fmt.parseInt(u21, hex_part, 16) catch return error.UnexpectedToken;
            return Value.makeCharacter(codepoint);
        }

        return error.UnexpectedToken;
    }

    fn advance(self: *Parser) void {
        self.current = self.lexer.next();
    }
};

// ============================================================================
// Tests
// ============================================================================

test "parse number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var parser = Parser.init(testing.allocator, &heap, "42");
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isFixnum());
    try testing.expectEqual(@as(i64, 42), val.toFixnum());
}

test "parse negative number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var parser = Parser.init(testing.allocator, &heap, "-123");
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isFixnum());
    try testing.expectEqual(@as(i64, -123), val.toFixnum());
}

test "parse nil" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var parser = Parser.init(testing.allocator, &heap, "nil");
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isNil());
}

test "parse empty list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var parser = Parser.init(testing.allocator, &heap, "()");
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isNil());
}

test "parse simple list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var parser = Parser.init(testing.allocator, &heap, "(1 2 3)");
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

    var parser = Parser.init(testing.allocator, &heap, "(1 . 2)");
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

    var parser = Parser.init(testing.allocator, &heap, "((1 2) (3 4))");
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

    var parser = Parser.init(testing.allocator, &heap, "\"hello\"");
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

    var parser = Parser.init(testing.allocator, &heap, "foo");
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isSymbol());

    const string = @import("../runtime/primitives/string.zig");
    try testing.expectEqualStrings("foo", string.symbolNameBytes(val).?);
}

test "parse keyword" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var parser = Parser.init(testing.allocator, &heap, ":test");
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isKeyword());

    const string = @import("../runtime/primitives/string.zig");
    try testing.expectEqualStrings("test", string.keywordNameBytes(val).?);
}

test "parse quote" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var parser = Parser.init(testing.allocator, &heap, "'foo");
    defer parser.deinit();

    const val = try parser.parse();
    try testing.expect(val.isCons());

    const list = @import("../runtime/primitives/list.zig");
    const quote_sym = list.car(val);
    try testing.expect(quote_sym.isSymbol());

    const string = @import("../runtime/primitives/string.zig");
    try testing.expectEqualStrings("quote", string.symbolNameBytes(quote_sym).?);
}

test "symbol interning" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var parser = Parser.init(testing.allocator, &heap, "foo foo");
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

    var parser = Parser.init(testing.allocator, &heap, "(+ 1 2)");
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

    // #x20 = 32
    var parser1 = Parser.init(testing.allocator, &heap, "#x20");
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.isFixnum());
    try testing.expectEqual(@as(i64, 32), val1.toFixnum());

    // #xFF = 255
    var parser2 = Parser.init(testing.allocator, &heap, "#xFF");
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expectEqual(@as(i64, 255), val2.toFixnum());

    // #xABCD = 43981
    var parser3 = Parser.init(testing.allocator, &heap, "#xABCD");
    defer parser3.deinit();
    const val3 = try parser3.parse();
    try testing.expectEqual(@as(i64, 43981), val3.toFixnum());

    // Case insensitive: #X1a2B = 6699
    var parser4 = Parser.init(testing.allocator, &heap, "#X1a2B");
    defer parser4.deinit();
    const val4 = try parser4.parse();
    try testing.expectEqual(@as(i64, 6699), val4.toFixnum());
}

test "parse binary number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // #b101 = 5
    var parser1 = Parser.init(testing.allocator, &heap, "#b101");
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.isFixnum());
    try testing.expectEqual(@as(i64, 5), val1.toFixnum());

    // #B11111111 = 255
    var parser2 = Parser.init(testing.allocator, &heap, "#B11111111");
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expectEqual(@as(i64, 255), val2.toFixnum());

    // #b0 = 0
    var parser3 = Parser.init(testing.allocator, &heap, "#b0");
    defer parser3.deinit();
    const val3 = try parser3.parse();
    try testing.expectEqual(@as(i64, 0), val3.toFixnum());
}

test "parse octal number" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // #o77 = 63
    var parser1 = Parser.init(testing.allocator, &heap, "#o77");
    defer parser1.deinit();
    const val1 = try parser1.parse();
    try testing.expect(val1.isFixnum());
    try testing.expectEqual(@as(i64, 63), val1.toFixnum());

    // #O755 = 493
    var parser2 = Parser.init(testing.allocator, &heap, "#O755");
    defer parser2.deinit();
    const val2 = try parser2.parse();
    try testing.expectEqual(@as(i64, 493), val2.toFixnum());

    // #o0 = 0
    var parser3 = Parser.init(testing.allocator, &heap, "#o0");
    defer parser3.deinit();
    const val3 = try parser3.parse();
    try testing.expectEqual(@as(i64, 0), val3.toFixnum());
}
