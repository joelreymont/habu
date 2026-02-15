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
    vector_open, // #(
    complex_open, // #C(
    struct_open, // #S(
    array_open, // #A( or #nA(
    pathname, // #P"..."
    bitvec, // #*

    // Quotes
    quote,
    backquote,
    comma,
    comma_at,
    function_quote, // #'
    read_eval, // #.

    // Literals
    number,
    bignum,
    float,
    rational,
    string,
    symbol,
    keyword,
    uninterned_symbol, // #:foo
    character,
    label_def, // #1=
    label_ref, // #1#

    // Reader conditionals
    feature_present, // #+
    feature_absent, // #-

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
            '"' => self.readString(.string),
            ':' => self.readKeyword(),
            '|' => self.readEscapedSymbol(),
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
                ' ', '\t', '\r', '\n', 0x0C => _ = self.advance(),
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

    fn readString(self: *Lexer, kind: TokenKind) Token {
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
        return self.makeToken(kind);
    }

    fn readNumber(self: *Lexer) Token {
        const start_pos = self.token_start;

        // Consume integer part
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // Check for rational (digits/digits)
        if (self.peek() == '/' and isDigit(self.peekNext())) {
            _ = self.advance(); // consume '/'
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
            // Check for trailing symbol chars (e.g., would make it a symbol)
            if (isSymbolChar(self.peek()) and !isDelimiter(self.peek())) {
                while (isSymbolChar(self.peek())) {
                    _ = self.advance();
                }
                return self.makeToken(.symbol);
            }
            return self.makeToken(.rational);
        }

        // CL spec: trailing dot on integer (e.g., "55.") means integer 55
        if (self.peek() == '.' and (isDelimiter(self.peekNext()) or self.peekNext() == 0)) {
            _ = self.advance(); // consume trailing dot
            return self.makeToken(.number);
        }

        // Check for decimal point followed by digits
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance(); // consume '.'
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
            // Check for exponent
            if (isExponentMarker(self.peek())) {
                _ = self.advance();
                if (self.peek() == '+' or self.peek() == '-') {
                    _ = self.advance();
                }
                while (isDigit(self.peek())) {
                    _ = self.advance();
                }
            }
            // Check for trailing symbol chars (e.g., 1.0+ would be a symbol)
            if (isSymbolChar(self.peek()) and !isDelimiter(self.peek())) {
                while (isSymbolChar(self.peek())) {
                    _ = self.advance();
                }
                return self.makeToken(.symbol);
            }
            return self.makeToken(.float);
        }

        // Check for exponent (scientific notation for integers becomes float)
        if (isExponentMarker(self.peek())) {
            _ = self.advance();
            if (self.peek() == '+' or self.peek() == '-') {
                _ = self.advance();
            }
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
            // Check for trailing symbol chars
            if (isSymbolChar(self.peek()) and !isDelimiter(self.peek())) {
                while (isSymbolChar(self.peek())) {
                    _ = self.advance();
                }
                return self.makeToken(.symbol);
            }
            return self.makeToken(.float);
        }

        // Check for trailing symbol chars like + or - (e.g., 1+ or 1-)
        // This makes tokens like 1+ into symbols per CL spec
        if (isSymbolChar(self.peek()) and !isDelimiter(self.peek())) {
            while (isSymbolChar(self.peek())) {
                _ = self.advance();
            }
            return self.makeToken(.symbol);
        }

        // Integer: check if it fits in fixnum range
        // Fixnums are 63-bit signed (bit 0 is tag), so range is -(2^62) to 2^62-1
        const text = self.source[start_pos..self.pos];
        const max_fixnum: u64 = (@as(u64, 1) << 62) - 1; // 4611686018427387903
        const min_fixnum_abs: u64 = max_fixnum + 1; // -2^62

        var idx: usize = 0;
        var negative = false;
        if (text.len > 0 and (text[0] == '-' or text[0] == '+')) {
            negative = text[0] == '-';
            idx = 1;
        }

        const limit: u64 = if (negative) min_fixnum_abs else max_fixnum;
        var value: u64 = 0;
        while (idx < text.len) : (idx += 1) {
            const c = text[idx];
            if (!isDigit(c)) break;
            const digit: u64 = @intCast(c - '0');
            const limit_div10 = limit / 10;
            const limit_mod10 = limit % 10;
            if (value > limit_div10 or (value == limit_div10 and digit > limit_mod10)) {
                return self.makeToken(.bignum);
            }
            value = value * 10 + digit;
        }

        return self.makeToken(.number);
    }

    fn readSymbol(self: *Lexer) Token {
        // First symbol character was already consumed in next().
        return self.readSymbolLike(.symbol, false, true, false);
    }

    fn readSymbolFromDot(self: *Lexer) Token {
        // Leading '.' was already consumed in next().
        return self.readSymbolLike(.symbol, false, true, false);
    }

    fn readSymbolFromSign(self: *Lexer, sign: u8) Token {
        _ = sign;
        // Already consumed sign, now read rest.
        return self.readSymbolLike(.symbol, false, true, false);
    }

    fn readKeyword(self: *Lexer) Token {
        // Already consumed ':'
        return self.readSymbolLike(.keyword, false, false, false);
    }

    fn readUninternedSymbol(self: *Lexer) Token {
        // Already consumed '#:'
        return self.readSymbolLike(.uninterned_symbol, false, false, false);
    }

    fn readEscapedSymbol(self: *Lexer) Token {
        // Opening '|' was already consumed as first character of token.
        return self.readSymbolLike(.symbol, true, false, true);
    }

    fn readSymbolLike(self: *Lexer, kind: TokenKind, allow_empty: bool, saw_any_init: bool, in_bar_init: bool) Token {
        var saw_any = saw_any_init;
        var in_bar = in_bar_init;

        while (!self.isAtEnd()) {
            const c = self.peek();
            if (in_bar) {
                _ = self.advance();
                if (c == '\\') {
                    if (self.isAtEnd()) return self.makeToken(.err);
                    _ = self.advance(); // escaped char inside bars
                    saw_any = true;
                } else if (c == '|') {
                    // || is a valid empty escaped symbol name.
                    if (!saw_any) saw_any = true;
                    in_bar = false;
                } else {
                    saw_any = true;
                }
                continue;
            }

            if (c == '\\') {
                _ = self.advance();
                if (self.isAtEnd()) return self.makeToken(.err);
                _ = self.advance(); // escaped char outside bars
                saw_any = true;
                continue;
            }
            if (c == '|') {
                _ = self.advance();
                in_bar = true;
                continue;
            }
            if (isSymbolChar(c)) {
                _ = self.advance();
                saw_any = true;
                continue;
            }
            break;
        }

        if (in_bar) return self.makeToken(.err); // Unterminated |...|
        if (!allow_empty and !saw_any) return self.makeToken(.err);
        return self.makeToken(kind);
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
        if (c == '.') {
            // Read-time eval: #.
            _ = self.advance(); // consume dot
            return self.makeToken(.read_eval);
        }
        if (c == '\'') {
            // Function quote: #'name
            _ = self.advance(); // consume quote
            return self.makeToken(.function_quote);
        }
        if (c == 'x' or c == 'X') {
            // Hex literal: #xABC or #XABC
            _ = self.advance(); // consume 'x'
            return self.readHexNumber();
        }
        if (c == 'b' or c == 'B') {
            // Binary literal: #b101 or #B101
            _ = self.advance(); // consume 'b'
            return self.readBinaryNumber();
        }
        if (c == 'o' or c == 'O') {
            // Octal literal: #o777 or #O777
            _ = self.advance(); // consume 'o'
            return self.readOctalNumber();
        }
        if (c == 'C' or c == 'c') {
            // Complex literal: #C(real imag)
            _ = self.advance(); // consume 'C'
            self.skipWhitespaceAndComments();
            if (self.peek() == '(') {
                _ = self.advance(); // consume '('
                return self.makeToken(.complex_open);
            }
            return self.makeToken(.err);
        }
        if (c == 'S' or c == 's') {
            // Struct literal: #S(struct-name :slot1 val1 ...)
            _ = self.advance(); // consume 'S'
            self.skipWhitespaceAndComments();
            if (self.peek() == '(') {
                _ = self.advance(); // consume '('
                return self.makeToken(.struct_open);
            }
            return self.makeToken(.err);
        }
        if (c == 'P' or c == 'p') {
            // Pathname literal: #P"path"
            _ = self.advance(); // consume 'P'
            self.skipWhitespaceAndComments();
            if (self.peek() == '"') {
                _ = self.advance(); // consume '"'
                return self.readString(.pathname);
            }
            return self.makeToken(.err);
        }
        if (c == 'A' or c == 'a') {
            // Array literal: #A(...) or general #nA(...)
            _ = self.advance(); // consume 'A'
            self.skipWhitespaceAndComments();
            if (self.peek() == '(') {
                _ = self.advance(); // consume '('
                return self.makeToken(.array_open);
            }
            return self.makeToken(.err);
        }
        if (std.ascii.isDigit(c)) {
            // Label syntax: #n= / #n#
            // Multi-dimensional array: #2A((row1)(row2))
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
            if (self.peek() == '=') {
                _ = self.advance();
                return self.makeToken(.label_def);
            }
            if (self.peek() == '#') {
                _ = self.advance();
                return self.makeToken(.label_ref);
            }
            if ((self.peek() == 'R' or self.peek() == 'r')) {
                _ = self.advance(); // consume 'R'
                if (self.peek() == '+' or self.peek() == '-') {
                    _ = self.advance();
                }
                const digits_start = self.pos;
                while (!self.isAtEnd() and !isDelimiter(self.peek())) {
                    _ = self.advance();
                }
                if (self.pos == digits_start) return self.makeToken(.err);
                return self.makeToken(.number);
            }
            if ((self.peek() == 'A' or self.peek() == 'a')) {
                _ = self.advance(); // consume 'A'
                if (self.peek() == '(') {
                    _ = self.advance(); // consume '('
                    return self.makeToken(.array_open);
                }
                // Rank-0 scalar syntax: #0Afoo
                const digits = self.source[self.token_start + 1 .. self.pos - 1];
                if (allDigitsZero(digits)) {
                    return self.makeToken(.array_open);
                }
            }
            return self.makeToken(.err);
        }
        if (c == '(') {
            // Vector literal: #(1 2 3)
            _ = self.advance(); // consume '('
            return self.makeToken(.vector_open);
        }
        if (c == '|') {
            // Block comment: #| ... |#
            _ = self.advance(); // consume '|'
            return self.readBlockComment();
        }
        if (c == '+') {
            // Reader conditional: #+feature
            _ = self.advance(); // consume '+'
            return self.makeToken(.feature_present);
        }
        if (c == '-') {
            // Reader conditional: #-feature
            _ = self.advance(); // consume '-'
            return self.makeToken(.feature_absent);
        }
        if (c == ':') {
            // Uninterned symbol: #:foo
            _ = self.advance(); // consume ':'
            return self.readUninternedSymbol();
        }
        if (c == '*') {
            // Bit vector: #*101010
            _ = self.advance(); // consume '*'
            return self.readBitVector();
        }
        // Unknown # dispatch
        return self.makeToken(.err);
    }

    fn readBitVector(self: *Lexer) Token {
        // Already consumed '#*'
        while (!self.isAtEnd() and (self.peek() == '0' or self.peek() == '1')) {
            _ = self.advance();
        }
        return self.makeToken(.bitvec);
    }

    fn readHexNumber(self: *Lexer) Token {
        // Already consumed '#x'
        if (self.peek() == '+' or self.peek() == '-') {
            _ = self.advance();
        }
        while (!self.isAtEnd() and isHexDigit(self.peek())) {
            _ = self.advance();
        }
        return self.makeToken(.number);
    }

    fn readBinaryNumber(self: *Lexer) Token {
        // Already consumed '#b'
        if (self.peek() == '+' or self.peek() == '-') {
            _ = self.advance();
        }
        while (!self.isAtEnd() and (self.peek() == '0' or self.peek() == '1')) {
            _ = self.advance();
        }
        return self.makeToken(.number);
    }

    fn readOctalNumber(self: *Lexer) Token {
        // Already consumed '#o'
        if (self.peek() == '+' or self.peek() == '-') {
            _ = self.advance();
        }
        while (!self.isAtEnd() and self.peek() >= '0' and self.peek() <= '7') {
            _ = self.advance();
        }
        return self.makeToken(.number);
    }

    fn readCharacter(self: *Lexer) Token {
        // Already consumed '#\'
        if (self.isAtEnd()) return self.makeToken(.err);

        // CL allows delimiter characters as a single-character name, e.g. #\  or #\).
        if (isDelimiter(self.peek())) {
            _ = self.advance();
            return self.makeToken(.character);
        }

        // Read character name or single char
        const start = self.pos;
        while (!self.isAtEnd() and !isDelimiter(self.peek())) {
            _ = self.advance();
        }

        if (self.pos == start) return self.makeToken(.err);

        return self.makeToken(.character);
    }

    fn readBlockComment(self: *Lexer) Token {
        // Already consumed '#|'
        // Block comments can nest: #| outer #| inner |# still outer |#
        var depth: u32 = 1;
        while (!self.isAtEnd() and depth > 0) {
            const c = self.advance();
            if (c == '|' and self.peek() == '#') {
                _ = self.advance(); // consume '#'
                depth -= 1;
            } else if (c == '#' and self.peek() == '|') {
                _ = self.advance(); // consume '|'
                depth += 1;
            }
        }
        // Block comment is just whitespace - get next token
        return self.next();
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
        return c == 0 or c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == 0x0C or
            c == '(' or c == ')' or c == '"' or c == ';' or c == '\'';
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isExponentMarker(c: u8) bool {
    return switch (c) {
        'e', 'E', 's', 'S', 'f', 'F', 'd', 'D', 'l', 'L' => true,
        else => false,
    };
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn allDigitsZero(s: []const u8) bool {
    if (s.len == 0) return false;
    for (s) |c| {
        if (c != '0') return false;
    }
    return true;
}

fn isSymbolStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_' or c == '+' or c == '-' or c == '*' or c == '/' or
        c == '=' or c == '<' or c == '>' or c == '!' or c == '?' or
        c == '%' or c == '&' or c == '$' or c == '@' or c == '^' or
        c == '~' or c == '{' or c == '}';
}

fn isSymbolChar(c: u8) bool {
    // Include ':' for package-qualified symbols like pkg:sym or pkg::sym
    return isSymbolStart(c) or isDigit(c) or c == '.' or c == ':';
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

test "lex bignum range" {
    const testing = std.testing;

    var lexer = Lexer.init("4611686018427387903 4611686018427387904 -4611686018427387904 -4611686018427387905");

    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.bignum, lexer.next().kind);
    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.bignum, lexer.next().kind);
}

test "lex symbols" {
    const testing = std.testing;

    var lexer = Lexer.init("foo bar+ list->vector format.~.1 format.{.1 format.}.1");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t1.kind);
    try testing.expectEqualStrings("foo", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t2.kind);
    try testing.expectEqualStrings("bar+", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t3.kind);
    try testing.expectEqualStrings("list->vector", t3.text);

    const t4 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t4.kind);
    try testing.expectEqualStrings("format.~.1", t4.text);

    const t5 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t5.kind);
    try testing.expectEqualStrings("format.{.1", t5.text);

    const t6 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t6.kind);
    try testing.expectEqualStrings("format.}.1", t6.text);
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

test "lex escaped symbol names" {
    const testing = std.testing;

    var lexer = Lexer.init(":|| :|a| :|1234| #:|| ||");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.keyword, t1.kind);
    try testing.expectEqualStrings(":||", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.keyword, t2.kind);
    try testing.expectEqualStrings(":|a|", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.keyword, t3.kind);
    try testing.expectEqualStrings(":|1234|", t3.text);

    const t4 = lexer.next();
    try testing.expectEqual(TokenKind.uninterned_symbol, t4.kind);
    try testing.expectEqualStrings("#:||", t4.text);

    const t5 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t5.kind);
    try testing.expectEqualStrings("||", t5.text);
}

test "lex uninterned symbols" {
    const testing = std.testing;

    var lexer = Lexer.init("#:foo #:*bar*");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.uninterned_symbol, t1.kind);
    try testing.expectEqualStrings("#:foo", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.uninterned_symbol, t2.kind);
    try testing.expectEqualStrings("#:*bar*", t2.text);
}

test "lex shared labels" {
    const testing = std.testing;

    var lexer = Lexer.init("#1=#:foo #1# #12=(a b) #12#");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.label_def, t1.kind);
    try testing.expectEqualStrings("#1=", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.uninterned_symbol, t2.kind);
    try testing.expectEqualStrings("#:foo", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.label_ref, t3.kind);
    try testing.expectEqualStrings("#1#", t3.text);

    const t4 = lexer.next();
    try testing.expectEqual(TokenKind.label_def, t4.kind);
    try testing.expectEqualStrings("#12=", t4.text);

    const t5 = lexer.next();
    try testing.expectEqual(TokenKind.lparen, t5.kind);
    try testing.expectEqual(TokenKind.symbol, lexer.next().kind);
    try testing.expectEqual(TokenKind.symbol, lexer.next().kind);
    try testing.expectEqual(TokenKind.rparen, lexer.next().kind);

    const t6 = lexer.next();
    try testing.expectEqual(TokenKind.label_ref, t6.kind);
    try testing.expectEqualStrings("#12#", t6.text);
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

    var lexer = Lexer.init("#\\a #\\newline #\\space #\\  #\\)");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.character, t1.kind);
    try testing.expectEqualStrings("#\\a", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.character, t2.kind);
    try testing.expectEqualStrings("#\\newline", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.character, t3.kind);
    try testing.expectEqualStrings("#\\space", t3.text);

    const t4 = lexer.next();
    try testing.expectEqual(TokenKind.character, t4.kind);
    try testing.expectEqualStrings("#\\ ", t4.text);

    const t5 = lexer.next();
    try testing.expectEqual(TokenKind.character, t5.kind);
    try testing.expectEqualStrings("#\\)", t5.text);
}

test "lex hex numbers" {
    const testing = std.testing;

    var lexer = Lexer.init("#x20 #xFF #xABCD #X1a2B");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.number, t1.kind);
    try testing.expectEqualStrings("#x20", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.number, t2.kind);
    try testing.expectEqualStrings("#xFF", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.number, t3.kind);
    try testing.expectEqualStrings("#xABCD", t3.text);

    const t4 = lexer.next();
    try testing.expectEqual(TokenKind.number, t4.kind);
    try testing.expectEqualStrings("#X1a2B", t4.text);
}

test "lex signed hex number" {
    const testing = std.testing;

    var lexer = Lexer.init("#x-FF #x+2A");
    try testing.expectEqualStrings("#x-FF", lexer.next().text);
    try testing.expectEqualStrings("#x+2A", lexer.next().text);
}

test "lex binary numbers" {
    const testing = std.testing;

    var lexer = Lexer.init("#b101 #B11111111 #b0");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.number, t1.kind);
    try testing.expectEqualStrings("#b101", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.number, t2.kind);
    try testing.expectEqualStrings("#B11111111", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.number, t3.kind);
    try testing.expectEqualStrings("#b0", t3.text);
}

test "lex signed binary number" {
    const testing = std.testing;

    var lexer = Lexer.init("#b-101 #B+11");
    try testing.expectEqualStrings("#b-101", lexer.next().text);
    try testing.expectEqualStrings("#B+11", lexer.next().text);
}

test "lex octal numbers" {
    const testing = std.testing;

    var lexer = Lexer.init("#o77 #O755 #o0");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.number, t1.kind);
    try testing.expectEqualStrings("#o77", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.number, t2.kind);
    try testing.expectEqualStrings("#O755", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.number, t3.kind);
    try testing.expectEqualStrings("#o0", t3.text);
}

test "lex signed octal numbers" {
    const testing = std.testing;

    var lexer = Lexer.init("#o-777 #O+12");
    try testing.expectEqualStrings("#o-777", lexer.next().text);
    try testing.expectEqualStrings("#O+12", lexer.next().text);
}

test "lex arbitrary radix numbers" {
    const testing = std.testing;

    var lexer = Lexer.init("#3r2120012102 #16rFF #16r-ff");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.number, t1.kind);
    try testing.expectEqualStrings("#3r2120012102", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.number, t2.kind);
    try testing.expectEqualStrings("#16rFF", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.number, t3.kind);
    try testing.expectEqualStrings("#16r-ff", t3.text);
}

test "lex vector literal" {
    const testing = std.testing;

    var lexer = Lexer.init("#(1 2 3)");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.vector_open, t1.kind);
    try testing.expectEqualStrings("#(", t1.text);

    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.rparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}

test "lex complex literal with whitespace after #c" {
    const testing = std.testing;

    var lexer = Lexer.init("#c (1 1)");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.complex_open, t1.kind);
    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.number, t2.kind);
    try testing.expectEqualStrings("1", t2.text);
    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.number, t3.kind);
    try testing.expectEqualStrings("1", t3.text);
    try testing.expectEqual(TokenKind.rparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}

test "lex #2A array literal" {
    const testing = std.testing;

    var lexer = Lexer.init("#2A((1 2))");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.array_open, t1.kind);
    try testing.expectEqualStrings("#2A(", t1.text);
}

test "lex #0A scalar array literal" {
    const testing = std.testing;

    var lexer = Lexer.init("#0AT");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.array_open, t1.kind);
    try testing.expectEqualStrings("#0A", t1.text);
    try testing.expectEqual(TokenKind.symbol, lexer.next().kind);
}

test "lex block comment" {
    const testing = std.testing;

    var lexer = Lexer.init("foo #| this is a comment |# bar");

    try testing.expectEqualStrings("foo", lexer.next().text);
    try testing.expectEqualStrings("bar", lexer.next().text);
    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}

test "lex nested block comment" {
    const testing = std.testing;

    var lexer = Lexer.init("foo #| outer #| inner |# still outer |# bar");

    try testing.expectEqualStrings("foo", lexer.next().text);
    try testing.expectEqualStrings("bar", lexer.next().text);
    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}

test "lex reader conditionals" {
    const testing = std.testing;

    var lexer = Lexer.init("#+habu #-cl");

    const t1 = lexer.next();
    try testing.expectEqual(TokenKind.feature_present, t1.kind);
    try testing.expectEqualStrings("#+", t1.text);

    const t2 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t2.kind);
    try testing.expectEqualStrings("habu", t2.text);

    const t3 = lexer.next();
    try testing.expectEqual(TokenKind.feature_absent, t3.kind);
    try testing.expectEqualStrings("#-", t3.text);

    const t4 = lexer.next();
    try testing.expectEqual(TokenKind.symbol, t4.kind);
    try testing.expectEqualStrings("cl", t4.text);

    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}

test "lex sharp dot reader eval" {
    const testing = std.testing;

    var lexer = Lexer.init("#.(+ 1 2)");
    try testing.expectEqual(TokenKind.read_eval, lexer.next().kind);
    try testing.expectEqual(TokenKind.lparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.symbol, lexer.next().kind);
    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.number, lexer.next().kind);
    try testing.expectEqual(TokenKind.rparen, lexer.next().kind);
    try testing.expectEqual(TokenKind.eof, lexer.next().kind);
}
