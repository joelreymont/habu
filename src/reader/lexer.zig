//! Lexer for S-expressions
//!
//! Tokenizes Lisp source into tokens:
//! - Parentheses, quotes, backquote, comma
//! - Numbers (integers)
//! - Strings
//! - Symbols and keywords

const std = @import("std");

pub const TokenKind = enum {
    // Delimiters
    lparen,
    rparen,
    dot,

    // Quotes
    quote,
    backquote,
    comma,
    comma_at,

    // Literals
    number,
    string,
    symbol,
    keyword,
    character,

    // Special
    eof,
    err,
};

pub const Token = struct {
    kind: TokenKind,
    /// Source slice for this token
    text: []const u8,
    /// Line number (1-indexed)
    line: u32,
    /// Column number (1-indexed)
    column: u32,

    pub fn isEof(self: Token) bool {
        return self.kind == .eof;
    }

    pub fn isError(self: Token) bool {
        return self.kind == .err;
    }
};

pub const Lexer = struct {
    source: []const u8,
    pos: usize,
    line: u32,
    column: u32,
    /// Start position of current token
    token_start: usize,
    token_line: u32,
    token_column: u32,

    pub fn init(source: []const u8) Lexer {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
            .token_start = 0,
            .token_line = 1,
            .token_column = 1,
        };
    }

    /// Get next token
    pub fn next(self: *Lexer) Token {
        self.skipWhitespaceAndComments();

        self.token_start = self.pos;
        self.token_line = self.line;
        self.token_column = self.column;

        if (self.isAtEnd()) {
            return self.makeToken(.eof);
        }

        const c = self.advance();

        return switch (c) {
            '(' => self.makeToken(.lparen),
            ')' => self.makeToken(.rparen),
            '\'' => self.makeToken(.quote),
            '`' => self.makeToken(.backquote),
            ',' => if (self.match('@')) self.makeToken(.comma_at) else self.makeToken(.comma),
            '"' => self.readString(),
            ':' => self.readKeyword(),
            '.' => if (isDelimiter(self.peek())) self.makeToken(.dot) else self.readSymbolFromDot(),
            '-', '+' => if (isDigit(self.peek())) self.readNumber() else self.readSymbolFromSign(c),
            '#' => self.readHash(),
            else => {
                if (isDigit(c)) {
                    return self.readNumber();
                } else if (isSymbolStart(c)) {
                    return self.readSymbol();
                } else {
                    return self.makeToken(.err);
                }
            },
        };
    }

    /// Peek at current character without consuming
    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    /// Peek at next character
    fn peekNext(self: *Lexer) u8 {
        if (self.pos + 1 >= self.source.len) return 0;
        return self.source[self.pos + 1];
    }

    /// Advance and return current character
    fn advance(self: *Lexer) u8 {
        const c = self.source[self.pos];
        self.pos += 1;
        if (c == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        return c;
    }

    /// Match and consume if current char matches
    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.pos] != expected) return false;
        _ = self.advance();
        return true;
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.pos >= self.source.len;
    }

    fn skipWhitespaceAndComments(self: *Lexer) void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\t', '\r', '\n' => _ = self.advance(),
                ';' => {
                    // Comment - skip to end of line
                    while (!self.isAtEnd() and self.peek() != '\n') {
                        _ = self.advance();
                    }
                },
                else => return,
            }
        }
    }

    fn readString(self: *Lexer) Token {
        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\\' and self.peekNext() != 0) {
                _ = self.advance(); // skip backslash
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return self.makeToken(.err); // Unterminated string
        }

        _ = self.advance(); // closing quote
        return self.makeToken(.string);
    }

    fn readNumber(self: *Lexer) Token {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(.number);
    }

    fn readSymbol(self: *Lexer) Token {
        while (isSymbolChar(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(.symbol);
    }

    fn readSymbolFromDot(self: *Lexer) Token {
        while (isSymbolChar(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(.symbol);
    }

    fn readSymbolFromSign(self: *Lexer, sign: u8) Token {
        _ = sign;
        // Already consumed sign, now read rest
        while (isSymbolChar(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(.symbol);
    }

    fn readKeyword(self: *Lexer) Token {
        // Already consumed ':'
        while (isSymbolChar(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(.keyword);
    }

    fn readHash(self: *Lexer) Token {
        // Already consumed '#'
        if (self.isAtEnd()) return self.makeToken(.err);

        const c = self.peek();
        if (c == '\\') {
            // Character literal: #\a, #\newline, etc.
            _ = self.advance(); // consume backslash
            return self.readCharacter();
        }
        // Future: #( for vectors, #' for function, etc.
        return self.makeToken(.err);
    }

    fn readCharacter(self: *Lexer) Token {
        // Already consumed '#\'
        if (self.isAtEnd()) return self.makeToken(.err);

        // Read character name or single char
        const start = self.pos;
        while (!self.isAtEnd() and !isDelimiter(self.peek())) {
            _ = self.advance();
        }

        if (self.pos == start) return self.makeToken(.err);

        return self.makeToken(.character);
    }

    fn makeToken(self: *Lexer, kind: TokenKind) Token {
        return .{
            .kind = kind,
            .text = self.source[self.token_start..self.pos],
            .line = self.token_line,
            .column = self.token_column,
        };
    }

    fn isDelimiter(c: u8) bool {
        return c == 0 or c == ' ' or c == '\t' or c == '\n' or c == '\r' or
            c == '(' or c == ')' or c == '"' or c == ';' or c == '\'';
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isSymbolStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_' or c == '+' or c == '-' or c == '*' or c == '/' or
        c == '=' or c == '<' or c == '>' or c == '!' or c == '?' or
        c == '%' or c == '&' or c == '$' or c == '@' or c == '^';
}

fn isSymbolChar(c: u8) bool {
    return isSymbolStart(c) or isDigit(c) or c == '.';
}

// ============================================================================
// Tests
// ============================================================================

test "lex simple tokens" {
    const testing = std.testing;

    var lexer = Lexer.init("( ) ' ` , ,@");

    try testing.expectEqual(TokenKind.lparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.rparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.quote, lexer.next().kind);
    try testing.expectEqual(TokenKind.backquote, lexer.next().kind);
    try testing.expectEqual(TokenKind.comma, lexer.next().kind);
    try testing.expectEqual(TokenKind.comma_at, lexer.next().kind);
    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}

test "lex numbers" {
    const testing = std.testing;

    var lexer = Lexer.init("123 -45 +67");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.number, t1.kind);
    try testing.expectEqualStrings("123", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.number, t2.kind);
    try testing.expectEqualStrings("-45", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.number, t3.kind);
    try testing.expectEqualStrings("+67", t3.text);
}

test "lex symbols" {
    const testing = std.testing;

    var lexer = Lexer.init("foo bar+ list->vector");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t1.kind);
    try testing.expectEqualStrings("foo", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t2.kind);
    try testing.expectEqualStrings("bar+", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t3.kind);
    try testing.expectEqualStrings("list->vector", t3.text);
}

test "lex keywords" {
    const testing = std.testing;

    var lexer = Lexer.init(":foo :test-key");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.keyword, t1.kind);
    try testing.expectEqualStrings(":foo", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.keyword, t2.kind);
    try testing.expectEqualStrings(":test-key", t2.text);
}

test "lex strings" {
    const testing = std.testing;

    var lexer = Lexer.init("\"hello\" \"with\\\"escape\"");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.string, t1.kind);
    try testing.expectEqualStrings("\"hello\"", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.string, t2.kind);
    try testing.expectEqualStrings("\"with\\\"escape\"", t2.text);
}

test "lex expression" {
    const testing = std.testing;

    var lexer = Lexer.init("(+ 1 2)");

    try testing.expectEqual(TokenKind.lparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.symbol, lexer.next().kind);
    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.rparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}

test "skip comments" {
    const testing = std.testing;

    var lexer = Lexer.init(
        \\; this is a comment
        \\foo
        \\; another comment
        \\bar
    );

    try testing.expectEqualStrings("foo", lexer.next().text);
    try testing.expectEqualStrings("bar", lexer.next().text);
    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}

test "line and column tracking" {
    const testing = std.testing;

    var lexer = Lexer.init("foo\nbar");

    const t1 = lexer.next();
    try testing.expectEqual(@as(u32, 1), t1.line);
    try testing.expectEqual(@as(u32, 1), t1.column);

    const t2 = lexer.next();
    try testing.expectEqual(@as(u32, 2), t2.line);
    try testing.expectEqual(@as(u32, 1), t2.column);
}

test "dot token" {
    const testing = std.testing;

    var lexer = Lexer.init("(a . b)");

    try testing.expectEqual(TokenKind.lparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.symbol, lexer.next().kind);
    try testing.expectEqual(TokenKind.dot, lexer.next().kind);
    try testing.expectEqual(TokenKind.symbol, lexer.next().kind);
    try testing.expectEqual(TokenKind.rparen, lexer.next().kind);
}

test "lex characters" {
    const testing = std.testing;

    var lexer = Lexer.init("#\\a #\\newline #\\space");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.character, t1.kind);
    try testing.expectEqualStrings("#\\a", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.character, t2.kind);
    try testing.expectEqualStrings("#\\newline", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.character, t3.kind);
    try testing.expectEqualStrings("#\\space", t3.text);
}
