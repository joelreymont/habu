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

pub const ParseError = error{
    UnexpectedToken,
    UnterminatedList,
    InvalidNumber,
    OutOfMemory,
};

pub const Parser = struct {
    lexer: Lexer,
    heap: *Heap,
    current: Token,

    /// Interned symbols (for eq comparison)
    symbol_table: std.StringHashMap(Value),
    /// Backing allocator for symbol table
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) Parser {
        var lexer = Lexer.init(source);
        const first_token = lexer.next();

        return .{
            .lexer = lexer,
            .heap = heap,
            .current = first_token,
            .symbol_table = std.StringHashMap(Value).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Parser) void {
        // Free all interned symbol keys
        var it = self.symbol_table.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.symbol_table.deinit();
    }

    /// Parse one S-expression
    pub fn parse(self: *Parser) ParseError!Value {
        return self.parseExpr();
    }

    /// Parse all expressions until EOF
    pub fn parseAll(self: *Parser, results: *std.ArrayList(Value)) ParseError!void {
        while (self.current.kind != .eof) {
            const expr = try self.parseExpr();
            results.append(self.allocator, expr) catch return error.OutOfMemory;
        }
    }

    fn parseExpr(self: *Parser) ParseError!Value {
        switch (self.current.kind) {
            .lparen => return self.parseList(),
            .quote => return self.parseQuote("quote"),
            .backquote => return self.parseQuote("quasiquote"),
            .comma => return self.parseQuote("unquote"),
            .comma_at => return self.parseQuote("unquote-splicing"),
            .number => return self.parseNumber(),
            .string => return self.parseString(),
            .symbol => return self.parseSymbol(),
            .keyword => return self.parseKeyword(),
            .eof => return Value.nil,
            .rparen, .dot => return error.UnexpectedToken,
            .err => return error.UnexpectedToken,
        }
    }

    fn parseList(self: *Parser) ParseError!Value {
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

            return self.heap.allocCons(first, second) orelse error.OutOfMemory;
        }

        // Parse rest as proper list
        const rest = try self.parseListTail();
        return self.heap.allocCons(first, rest) orelse error.OutOfMemory;
    }

    fn parseListTail(self: *Parser) ParseError!Value {
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
        return self.heap.allocCons(car, cdr) orelse error.OutOfMemory;
    }

    fn parseQuote(self: *Parser, quote_name: []const u8) ParseError!Value {
        self.advance(); // consume quote token
        const quoted = try self.parseExpr();

        // Build (quote <expr>) or (quasiquote <expr>) etc.
        const quote_sym = try self.internSymbol(quote_name);
        const inner = self.heap.allocCons(quoted, Value.nil) orelse return error.OutOfMemory;
        return self.heap.allocCons(quote_sym, inner) orelse error.OutOfMemory;
    }

    fn parseNumber(self: *Parser) ParseError!Value {
        const text = self.current.text;
        self.advance();

        const n = std.fmt.parseInt(i64, text, 10) catch return error.InvalidNumber;
        return Value.makeFixnum(n);
    }

    fn parseString(self: *Parser) ParseError!Value {
        var text = self.current.text;
        self.advance();

        // Remove quotes
        if (text.len >= 2) {
            text = text[1 .. text.len - 1];
        }

        // TODO: Handle escape sequences properly
        return self.heap.allocString(text) orelse error.OutOfMemory;
    }

    fn parseSymbol(self: *Parser) ParseError!Value {
        const text = self.current.text;
        self.advance();

        // Check for nil and t
        if (std.mem.eql(u8, text, "nil")) {
            return Value.nil;
        }
        if (std.mem.eql(u8, text, "t")) {
            return Value.makeFixnum(1); // t is just fixnum 1
        }

        return self.internSymbol(text);
    }

    fn parseKeyword(self: *Parser) ParseError!Value {
        var text = self.current.text;
        self.advance();

        // Remove leading colon for storage
        if (text.len > 0 and text[0] == ':') {
            text = text[1..];
        }

        return self.allocKeyword(text);
    }

    /// Intern a symbol (same name = same Value)
    fn internSymbol(self: *Parser, name: []const u8) ParseError!Value {
        if (self.symbol_table.get(name)) |existing| {
            return existing;
        }

        // Allocate symbol in heap
        const sym = self.allocSymbol(name) catch return error.OutOfMemory;

        // Store in table (need to dupe the key)
        const key = self.allocator.dupe(u8, name) catch return error.OutOfMemory;
        self.symbol_table.put(key, sym) catch return error.OutOfMemory;

        return sym;
    }

    fn allocSymbol(self: *Parser, name: []const u8) !Value {
        const objects = @import("../runtime/objects.zig");

        const aligned_name_len = std.mem.alignForward(usize, name.len, 8);
        const total_size = @sizeOf(objects.Symbol) + aligned_name_len;

        const ptr = self.heap.allocRaw(total_size) orelse return error.OutOfMemory;
        const sym: *objects.Symbol = @ptrCast(@alignCast(ptr));
        const name_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.Symbol));

        @memcpy(name_ptr[0..name.len], name);

        sym.* = .{
            .name_len = name.len,
            .name_ptr = name_ptr,
            .plist = Value.nil,
            .reserved = 0,
        };

        return Value.makeSymbol(sym);
    }

    fn allocKeyword(self: *Parser, name: []const u8) ParseError!Value {
        const objects = @import("../runtime/objects.zig");

        const aligned_name_len = std.mem.alignForward(usize, name.len, 8);
        const total_size = @sizeOf(objects.Keyword) + aligned_name_len;

        const ptr = self.heap.allocRaw(total_size) orelse return error.OutOfMemory;
        const kw: *objects.Keyword = @ptrCast(@alignCast(ptr));
        const name_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.Keyword));

        @memcpy(name_ptr[0..name.len], name);

        kw.* = .{
            .name_len = name.len,
            .name_ptr = name_ptr,
            .hash = 0, // TODO: compute hash
        };

        return Value.makeKeyword(kw);
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
