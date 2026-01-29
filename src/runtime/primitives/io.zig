//! I/O primitives
//!
//! sys-read, sys-write, sys-exit, file operations

const std = @import("std");
const fs = std.fs;
const Value = @import("../value.zig").Value;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;
const builtins_mod = @import("../builtins.zig");

const IO_BUF = 4096;
const LINE_BUF = 1024;

/// Pushback buffer for unread-char (single character)
var pushback_char: ?u8 = null;

/// *print-escape* controls whether strings/symbols print readably
/// When true (default), strings have quotes, symbols with special chars escaped
/// When false (princ mode), strings print bare, symbols print as-is
pub var print_escape: bool = true;

pub const PrintCase = enum {
    upcase,
    downcase,
    capitalize,
};

/// *print-case* controls case conversion for symbols
/// :upcase (default) - ABC
/// :downcase - abc
/// :capitalize - Abc
pub var print_case: PrintCase = .upcase;

/// *print-readably* controls readable output
/// When true, printer must output in a way that read can reconstruct
/// When false (default), output may be abbreviated or truncated
pub var print_readably: bool = false;

/// *print-length* limits max elements printed in lists/vectors
/// When non-null, print ... after this many elements
pub var print_length: ?usize = null;

/// *print-level* limits max nesting depth for lists/vectors
/// When non-null, print # when exceeding this depth
pub var print_level: ?usize = null;

/// *print-base* controls radix for integer output (2-36, default 10)
pub var print_base: u8 = 10;

/// *print-radix* controls whether to print radix prefix (#x #o #b)
pub var print_radix: bool = false;

/// *print-gensym* controls whether to print #: prefix for uninterned symbols
pub var print_gensym: bool = true;

/// *print-array* controls whether to print array contents
pub var print_array: bool = true;

pub fn writeFixnumTo(n: i64, w: anytype) !void {
    try writeFixnum(n, w);
}

fn writeFixnum(n: i64, w: anytype) !void {
    if (print_radix) {
        switch (print_base) {
            2 => try w.writeAll("#b"),
            8 => try w.writeAll("#o"),
            16 => try w.writeAll("#x"),
            else => {},
        }
    }

    switch (print_base) {
        2 => try w.print("{b}", .{n}),
        8 => try w.print("{o}", .{n}),
        10 => try w.print("{d}", .{n}),
        16 => try w.print("{x}", .{n}),
        else => {
            var buf: [65]u8 = undefined;
            const len = formatIntBase(n, print_base, &buf);
            try w.writeAll(buf[0..len]);
        },
    }
}

fn formatIntBase(n: i64, base: u8, buf: []u8) usize {
    const digits = "0123456789abcdefghijklmnopqrstuvwxyz";
    var val: u64 = if (n < 0) @as(u64, @intCast(-n)) else @as(u64, @intCast(n));
    var i: usize = buf.len;

    if (val == 0) {
        buf[i - 1] = '0';
        return 1;
    }

    while (val > 0) : (i -= 1) {
        buf[i - 1] = digits[@as(usize, @intCast(val % base))];
        val /= base;
    }

    if (n < 0) {
        i -= 1;
        buf[i] = '-';
    }

    const len = buf.len - i;
    std.mem.copyForwards(u8, buf[0..len], buf[i..]);
    return len;
}

fn writeCaseSymbol(name: []const u8, w: anytype) !void {
    switch (print_case) {
        .upcase => {
            for (name) |c| {
                try w.writeByte(std.ascii.toUpper(c));
            }
        },
        .downcase => {
            for (name) |c| {
                try w.writeByte(std.ascii.toLower(c));
            }
        },
        .capitalize => {
            var first = true;
            for (name) |c| {
                if (first and std.ascii.isAlphabetic(c)) {
                    try w.writeByte(std.ascii.toUpper(c));
                    first = false;
                } else {
                    try w.writeByte(std.ascii.toLower(c));
                }
            }
        },
    }
}

/// Write a string to stdout
pub fn sysWrite(val: Value) !void {
    if (!val.isString()) return;

    const str = val.toPtr(objects.String);
    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.writeAll(str.bytes());
    try w.flush();
}

/// Write bytes directly to stdout
pub fn sysWriteBytes(bytes: []const u8) !void {
    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.writeAll(bytes);
    try w.flush();
}

/// Write a character to stdout
pub fn sysWriteChar(char: u8) !void {
    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.writeByte(char);
    try w.flush();
}

/// Write a fixnum to stdout
pub fn sysWriteFixnum(val: Value) !void {
    if (!val.isFixnum()) return;

    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.print("{d}", .{val.toFixnum()});
    try w.flush();
}

/// Write newline to stdout
pub fn sysNewline() !void {
    try sysWriteChar('\n');
}

/// Read a line from stdin (allocates in heap)
pub fn sysReadLine(heap: *Heap) !Value {
    const stdin_file = fs.File.stdin();
    var read_buf: [IO_BUF]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    var line_buf: [LINE_BUF]u8 = undefined;
    var line_len: usize = 0;

    while (line_len < line_buf.len) {
        const byte = reader.takeByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        if (byte == '\n') break;
        line_buf[line_len] = byte;
        line_len += 1;
    }

    if (line_len == 0) return Value.nil;

    return try heap.allocBaseString(line_buf[0..line_len]);
}

/// Read a single character from stdin
pub fn sysReadChar() !i64 {
    // Check pushback buffer first
    if (pushback_char) |ch| {
        pushback_char = null;
        return @intCast(ch);
    }

    const stdin_file = fs.File.stdin();
    var read_buf: [IO_BUF]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    const byte = reader.takeByte() catch |err| switch (err) {
        error.EndOfStream => return -1,
        else => return err,
    };

    return @intCast(byte);
}

/// Peek at next character without consuming it
pub fn sysPeekChar() !i64 {
    // If already have pushback, return it
    if (pushback_char) |ch| {
        return @intCast(ch);
    }

    // Read and push back
    const stdin_file = fs.File.stdin();
    var read_buf: [IO_BUF]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    const byte = reader.takeByte() catch |err| switch (err) {
        error.EndOfStream => return -1,
        else => return err,
    };

    pushback_char = byte;
    return @intCast(byte);
}

/// Push a character back to be read again
pub fn sysUnreadChar(ch: u8) void {
    pushback_char = ch;
}

/// Read a complete S-expression from stdin into buffer
/// Returns the number of bytes read, or error
pub fn sysReadSexp(buffer: []u8) !usize {
    const stdin_file = fs.File.stdin();
    var read_buf: [IO_BUF]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    var len: usize = 0;
    var paren_depth: i32 = 0;
    var in_string = false;
    var in_escape = false;
    var started = false;

    while (len < buffer.len) {
        // Check pushback buffer first
        const byte: u8 = if (pushback_char) |ch| blk: {
            pushback_char = null;
            break :blk ch;
        } else reader.takeByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        buffer[len] = byte;
        len += 1;

        if (in_escape) {
            in_escape = false;
            continue;
        }

        if (byte == '\\' and in_string) {
            in_escape = true;
            continue;
        }

        if (byte == '"') {
            in_string = !in_string;
            started = true;
            continue;
        }

        if (in_string) continue;

        // Skip leading whitespace
        if (!started and (byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r')) {
            len -= 1;
            continue;
        }

        started = true;

        if (byte == '(') {
            paren_depth += 1;
        } else if (byte == ')') {
            paren_depth -= 1;
            if (paren_depth == 0) break; // Complete expression
        } else if (paren_depth == 0 and (byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r')) {
            // Atom terminated by whitespace
            len -= 1; // Don't include the whitespace
            break;
        }
    }

    return if (len > 0) len else error.EndOfStream;
}

/// Print a Habu value to stdout (Lisp-style)
pub fn printValue(val: Value) !void {
    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try printValueTo(val, w);
    try w.flush();
}

/// Print a Habu value to stdout without escaping (princ style)
pub fn princValue(val: Value) !void {
    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try princValueTo(val, w, 0);
    try w.flush();
}

fn princValueTo(val: Value, w: anytype, level: usize) !void {
    if (print_level) |max_level| {
        if (level >= max_level) {
            try w.writeByte('#');
            return;
        }
    }

    switch (val.typeKind()) {
        .nil => try w.writeAll("nil"),
        .t => try w.writeAll("t"),
        .unbound => try w.writeAll("#<unbound>"),
        .fixnum => try writeFixnum(val.toFixnum(), w),
        .float => try w.print("{d}", .{val.toFloat()}),
        .char => {
            const cp = val.toCharacter();
            if (cp < 128) {
                try w.writeByte(@as(u8, @intCast(cp)));
            } else {
                var utf8_buf: [4]u8 = undefined;
                const len = std.unicode.utf8Encode(@intCast(cp), &utf8_buf) catch 0;
                try w.writeAll(utf8_buf[0..len]);
            }
        },
        .cons => {
            try w.writeByte('(');
            var current = val;
            var first = true;
            var count: usize = 0;
            while (current.isCons()) {
                if (print_length) |max_len| {
                    if (count >= max_len) {
                        try w.writeAll("...");
                        break;
                    }
                }
                if (!first) try w.writeByte(' ');
                first = false;
                const cons = current.toPtr(objects.Cons);
                try princValueTo(cons.car, w, level + 1);
                current = cons.cdr;
                count += 1;
            }
            if (!current.isNil() and (print_length == null or count < print_length.?)) {
                try w.writeAll(" . ");
                try princValueTo(current, w, level + 1);
            }
            try w.writeByte(')');
        },
        .symbol => try writeCaseSymbol(val.toPtr(objects.Symbol).getName(), w),
        .string => try w.writeAll(val.toPtr(objects.String).bytes()),
        .string32 => {
            // Convert UTF-32 to UTF-8 for output
            const s32 = val.toPtr(objects.String32);
            var utf8_buf: [4]u8 = undefined;
            for (s32.codepoints()) |cp| {
                const len = std.unicode.utf8Encode(@intCast(cp), &utf8_buf) catch continue;
                try w.writeAll(utf8_buf[0..len]);
            }
        },
        .closure => try w.writeAll("#<closure>"),
        .keyword => {
            try w.writeByte(':');
            try w.writeAll(val.toPtr(objects.Keyword).getName());
        },
        .vector => {
            const vec = val.toPtr(objects.Vector);
            if (!print_array) {
                try w.print("#<VECTOR {d}>", .{vec.length});
            } else {
                try w.writeAll("#(");
                const items = vec.items();
                const max_count = if (print_length) |max_len| @min(max_len, items.len) else items.len;
                for (0..max_count) |i| {
                    if (i > 0) try w.writeByte(' ');
                    try princValueTo(items[i], w, level + 1);
                }
                if (print_length) |max_len| {
                    if (items.len > max_len) {
                        try w.writeAll(" ...");
                    }
                }
                try w.writeByte(')');
            }
        },
        .hashtable => try w.print("#<hash-table count={d}>", .{val.toPtr(objects.HashTable).count}),
        .rational => {
            const rat = val.toPtr(objects.Rational);
            try w.print("{d}/{d}", .{ rat.numerator, rat.denominator });
        },
        .complex => {
            const cplx = val.toPtr(objects.Complex);
            try w.print("#C({d} {d})", .{ cplx.real, cplx.imag });
        },
        .stream => {
            const stream = val.toPtr(objects.Stream);
            const dir = if (stream.direction == .input) "input" else "output";
            const kind = switch (stream.stream_type) {
                .string => "string",
                .file => "file",
                .stdin => "stdin",
                .stdout => "stdout",
                .stderr => "stderr",
                .broadcast => "broadcast",
                .concatenated => "concatenated",
                .echo => "echo",
                .synonym => "synonym",
                .two_way => "two-way",
            };
            try w.print("#<{s}-{s}-stream>", .{ kind, dir });
        },
        .bignum => {
            const bn = val.toPtr(objects.Bignum);
            try w.print("#<bignum size={d}>", .{bn.size});
        },
        .array => {
            const arr = val.toPtr(objects.Array);
            try w.print("#<array rank={d}>", .{arr.rank});
        },
        .pathname => try w.writeAll("#<pathname>"),
        .package => {
            const pkg = val.toPtr(objects.Package);
            const name_sym = pkg.name.toPtr(objects.Symbol);
            try w.print("#<package {s}>", .{name_sym.getName()});
        },
        .chunk => try w.writeAll("#<chunk>"),
        .condition => {
            const cond = val.toPtr(objects.Condition);
            const type_sym = cond.type_sym.toPtr(objects.Symbol);
            try w.print("#<condition {s}>", .{type_sym.getName()});
        },
        .class => {
            const cls = val.toPtr(objects.Class);
            const name_sym = cls.name.toPtr(objects.Symbol);
            try w.print("#<class {s}>", .{name_sym.getName()});
        },
        .slotdef => {
            const slotdef = val.toPtr(objects.SlotDefinition);
            const name_sym = slotdef.name.toPtr(objects.Symbol);
            try w.print("#<slot-definition {s}>", .{name_sym.getName()});
        },
        .generic_function => {
            const gf = val.toPtr(objects.GenericFunction);
            const name_sym = gf.name.toPtr(objects.Symbol);
            try w.print("#<generic-function {s}>", .{name_sym.getName()});
        },
        .method => try w.writeAll("#<method>"),
    }
}

/// Write value to any writer (for write-to-string)
pub fn writeValueToBuffer(val: Value, w: anytype) !void {
    try printValueTo(val, w);
}

/// Convert value to string (write-to-string primitive)
pub fn writeToString(heap: *Heap, val: Value) !Value {
    var buf = std.ArrayList(u8){};
    const w = buf.writer(heap.backing_allocator);
    try printValueTo(val, w.any());
    const bytes = try buf.toOwnedSlice(heap.backing_allocator);
    defer heap.backing_allocator.free(bytes);
    return try heap.allocBaseString(bytes);
}

fn printValueTo(val: Value, w: anytype) !void {
    if (print_readably or print_escape) {
        return printEscapedTo(val, w, 0);
    } else {
        return princValueTo(val, w, 0);
    }
}

fn printEscapedTo(val: Value, w: anytype, level: usize) !void {
    if (!print_readably) {
        if (print_level) |max_level| {
            if (level >= max_level) {
                try w.writeByte('#');
                return;
            }
        }
    }

    switch (val.typeKind()) {
        .nil => try w.writeAll("nil"),
        .t => try w.writeAll("t"),
        .unbound => try w.writeAll("#<unbound>"),
        .fixnum => try writeFixnum(val.toFixnum(), w),
        .float => try w.print("{d}", .{val.toFloat()}),
        .char => {
            const cp = val.toCharacter();
            if (cp == ' ') {
                try w.writeAll("#\\space");
            } else if (cp == '\n') {
                try w.writeAll("#\\newline");
            } else if (cp == '\t') {
                try w.writeAll("#\\tab");
            } else if (cp == '\r') {
                try w.writeAll("#\\return");
            } else if (cp >= 32 and cp < 127) {
                try w.print("#\\{c}", .{@as(u8, @intCast(cp))});
            } else {
                try w.print("#\\U+{X:0>4}", .{cp});
            }
        },
        .cons => {
            try w.writeByte('(');
            var current = val;
            var first = true;
            var count: usize = 0;
            while (current.isCons()) {
                if (!print_readably) {
                    if (print_length) |max_len| {
                        if (count >= max_len) {
                            try w.writeAll("...");
                            break;
                        }
                    }
                }
                if (!first) try w.writeByte(' ');
                first = false;
                const cons = current.toPtr(objects.Cons);
                try printEscapedTo(cons.car, w, level + 1);
                current = cons.cdr;
                count += 1;
            }
            if (!current.isNil()) {
                if (print_readably or print_length == null or count < print_length.?) {
                    try w.writeAll(" . ");
                    try printEscapedTo(current, w, level + 1);
                }
            }
            try w.writeByte(')');
        },
        .symbol => try writeCaseSymbol(val.toPtr(objects.Symbol).getName(), w),
        .string => {
            try w.writeByte('"');
            try w.writeAll(val.toPtr(objects.String).bytes());
            try w.writeByte('"');
        },
        .string32 => {
            // Convert UTF-32 to UTF-8 for output
            try w.writeByte('"');
            const s32 = val.toPtr(objects.String32);
            var utf8_buf: [4]u8 = undefined;
            for (s32.codepoints()) |cp| {
                const len = std.unicode.utf8Encode(@intCast(cp), &utf8_buf) catch continue;
                try w.writeAll(utf8_buf[0..len]);
            }
            try w.writeByte('"');
        },
        .closure => try w.writeAll("#<closure>"),
        .keyword => {
            try w.writeByte(':');
            try w.writeAll(val.toPtr(objects.Keyword).getName());
        },
        .vector => {
            const vec = val.toPtr(objects.Vector);
            try w.writeAll("#(");
            const items = vec.items();
            const max_count = if (!print_readably and print_length != null)
                @min(print_length.?, items.len)
            else
                items.len;
            for (0..max_count) |i| {
                if (i > 0) try w.writeByte(' ');
                try printEscapedTo(items[i], w, level + 1);
            }
            if (!print_readably) {
                if (print_length) |max_len| {
                    if (items.len > max_len) {
                        try w.writeAll(" ...");
                    }
                }
            }
            try w.writeByte(')');
        },
        .hashtable => try w.print("#<hash-table count={d}>", .{val.toPtr(objects.HashTable).count}),
        .rational => {
            const rat = val.toPtr(objects.Rational);
            try w.print("{d}/{d}", .{ rat.numerator, rat.denominator });
        },
        .complex => {
            const cplx = val.toPtr(objects.Complex);
            try w.print("#C({d} {d})", .{ cplx.real, cplx.imag });
        },
        .stream => {
            const stream = val.toPtr(objects.Stream);
            const dir = if (stream.direction == .input) "input" else "output";
            const kind = switch (stream.stream_type) {
                .string => "string",
                .file => "file",
                .stdin => "stdin",
                .stdout => "stdout",
                .stderr => "stderr",
                .broadcast => "broadcast",
                .concatenated => "concatenated",
                .echo => "echo",
                .synonym => "synonym",
                .two_way => "two-way",
            };
            try w.print("#<{s}-{s}-stream>", .{ kind, dir });
        },
        .bignum => {
            const bn = val.toPtr(objects.Bignum);
            try w.print("#<bignum size={d}>", .{bn.size});
        },
        .array => {
            const arr = val.toPtr(objects.Array);
            try w.print("#<array rank={d}>", .{arr.rank});
        },
        .pathname => try w.writeAll("#<pathname>"),
        .package => {
            const pkg = val.toPtr(objects.Package);
            const name_sym = pkg.name.toPtr(objects.Symbol);
            try w.print("#<package {s}>", .{name_sym.getName()});
        },
        .chunk => try w.writeAll("#<chunk>"),
        .condition => {
            const cond = val.toPtr(objects.Condition);
            const type_sym = cond.type_sym.toPtr(objects.Symbol);
            try w.print("#<condition {s}>", .{type_sym.getName()});
        },
        .class => {
            const cls = val.toPtr(objects.Class);
            const name_sym = cls.name.toPtr(objects.Symbol);
            try w.print("#<class {s}>", .{name_sym.getName()});
        },
        .slotdef => {
            const slotdef = val.toPtr(objects.SlotDefinition);
            const name_sym = slotdef.name.toPtr(objects.Symbol);
            try w.print("#<slot-definition {s}>", .{name_sym.getName()});
        },
        .generic_function => {
            const gf = val.toPtr(objects.GenericFunction);
            const name_sym = gf.name.toPtr(objects.Symbol);
            try w.print("#<generic-function {s}>", .{name_sym.getName()});
        },
        .method => try w.writeAll("#<method>"),
    }
}

// ============================================================================
// CL Output Primitives
// ============================================================================

/// write object &optional stream - output with *print-escape* = t (readable)
pub fn write(val: Value, stream: Value) !Value {
    _ = stream; // TODO: handle stream parameter
    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = true;

    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try printValueTo(val, w);
    try w.flush();
    return val;
}

/// prin1 object &optional stream - same as write, returns object
pub fn prin1(val: Value, stream: Value) !Value {
    return write(val, stream);
}

/// princ object &optional stream - output with *print-escape* = nil (no escaping)
pub fn princ(val: Value, stream: Value) !Value {
    _ = stream; // TODO: handle stream parameter
    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = false;

    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try printValueTo(val, w);
    try w.flush();
    return val;
}

/// print object &optional stream - output newline, prin1, space, returns object
pub fn print(val: Value, stream: Value) !Value {
    _ = stream; // TODO: handle stream parameter
    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.writeByte('\n');

    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = true;

    try printValueTo(val, w);
    try w.writeByte(' ');
    try w.flush();
    return val;
}

// ============================================================================
// Print Control Variables
// ============================================================================

/// Get *print-escape* value
pub fn getPrintEscape() Value {
    return if (print_escape) Value.t else Value.nil;
}

/// Set *print-escape* value
pub fn setPrintEscape(val: Value) void {
    print_escape = !val.isNil();
}

/// Get *print-case* value
pub fn getPrintCase(heap: *Heap) !Value {
    return switch (print_case) {
        .upcase => try heap.internKeyword("UPCASE"),
        .downcase => try heap.internKeyword("DOWNCASE"),
        .capitalize => try heap.internKeyword("CAPITALIZE"),
    };
}

/// Set *print-case* value
pub fn setPrintCase(builtins: *const builtins_mod.BuiltinSymbols, val: Value) !void {
    if (!val.isKeyword()) return error.TypeError;
    if (val.eq(builtins.kw_upcase)) {
        print_case = .upcase;
    } else if (val.eq(builtins.kw_downcase)) {
        print_case = .downcase;
    } else if (val.eq(builtins.kw_capitalize)) {
        print_case = .capitalize;
    } else {
        return error.InvalidPrintCase;
    }
}

/// Get *print-readably* value
pub fn getPrintReadably() Value {
    return if (print_readably) Value.t else Value.nil;
}

/// Set *print-readably* value
pub fn setPrintReadably(val: Value) void {
    print_readably = !val.isNil();
}

/// Get *print-base* value
pub fn getPrintBase() Value {
    return Value.makeFixnum(@intCast(print_base));
}

/// Set *print-base* value (2-36)
pub fn setPrintBase(val: Value) !void {
    if (!val.isFixnum()) return error.TypeError;
    const base = val.toFixnum();
    if (base < 2 or base > 36) return error.InvalidBase;
    print_base = @intCast(base);
}

/// Get *print-radix* value
pub fn getPrintRadix() Value {
    return if (print_radix) Value.t else Value.nil;
}

/// Set *print-radix* value
pub fn setPrintRadix(val: Value) void {
    print_radix = !val.isNil();
}

/// Get *print-gensym* value
pub fn getPrintGensym() Value {
    return if (print_gensym) Value.t else Value.nil;
}

/// Set *print-gensym* value
pub fn setPrintGensym(val: Value) void {
    print_gensym = !val.isNil();
}

/// Get *print-array* value
pub fn getPrintArray() Value {
    return if (print_array) Value.t else Value.nil;
}

/// Set *print-array* value
pub fn setPrintArray(val: Value) void {
    print_array = !val.isNil();
}

/// Exit the process
pub fn sysExit(code: i64) noreturn {
    const exit_code: u8 = @truncate(@as(u64, @bitCast(code)));
    std.process.exit(exit_code);
}

/// Read entire file contents (allocates in heap)
pub fn readFile(heap: *Heap, path: []const u8) !Value {
    const file = try fs.openFileAbsolute(path, .{});
    defer file.close();

    const stat = try file.stat();
    const size = stat.size;

    if (size > 10 * 1024 * 1024) {
        return error.FileTooLarge; // Limit to 10MB
    }

    // Allocate string in heap
    const aligned_len = std.mem.alignForward(usize, size, 8);
    const total_size = @sizeOf(objects.String) + aligned_len;

    const ptr = try heap.allocRaw(total_size);
    const str: *objects.String = @ptrCast(@alignCast(ptr));
    const data_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.String));

    // Read file contents
    const bytes_read = try file.readAll(data_ptr[0..size]);
    _ = bytes_read;

    str.* = .{
        .length = size,
        .data = data_ptr,
    };

    return Value.makeString(str);
}

/// Write string to file
pub fn writeFile(path: []const u8, content: Value) !void {
    if (!content.isString()) return error.InvalidArgument;

    const str = content.toPtr(objects.String);

    const file = try fs.createFileAbsolute(path, .{});
    defer file.close();

    try file.writeAll(str.bytes());
}

/// Check if file exists
pub fn fileExists(path: []const u8) !bool {
    if (fs.accessAbsolute(path, .{})) |_| {
        return true;
    } else |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    }
}

/// Get file size
pub fn fileSize(path: []const u8) !i64 {
    const file = try fs.openFileAbsolute(path, .{});
    defer file.close();

    const stat = try file.stat();
    return @intCast(stat.size);
}

/// Delete file
pub fn deleteFile(path: []const u8) !void {
    try fs.deleteFileAbsolute(path);
}

/// Rename file
pub fn renameFile(old_path: []const u8, new_path: []const u8) !void {
    try fs.renameAbsolute(old_path, new_path);
}

/// Probe file (check if exists)
/// Returns true if file exists, false otherwise
pub fn probeFile(path: []const u8) !bool {
    if (fs.accessAbsolute(path, .{})) |_| {
        return true;
    } else |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    }
}

/// Get file write date (modification time) as Universal Time
/// Universal Time is seconds since 1900-01-01
pub fn fileWriteDate(path: []const u8) !i64 {
    const file = try fs.openFileAbsolute(path, .{});
    defer file.close();
    const stat = try file.stat();
    // Convert from nanoseconds to seconds, then from Unix to Universal Time
    const unix_seconds: i64 = @intCast(@divFloor(stat.mtime, std.time.ns_per_s));
    // Universal Time epoch is 1900-01-01, Unix epoch is 1970-01-01
    // Difference: 70 years = 2208988800 seconds
    return unix_seconds + 2208988800;
}

/// Get current Universal Time
/// Universal Time is seconds since 1900-01-01
pub fn getUniversalTime() i64 {
    const unix_seconds = std.time.timestamp();
    // Convert Unix time to Universal Time (add 70 years in seconds)
    return unix_seconds + 2208988800;
}

/// Get internal real time in microseconds
/// Returns a monotonic timestamp suitable for measuring elapsed time
pub fn getInternalRealTime() i64 {
    return std.time.microTimestamp();
}

/// Get internal run time (process CPU time in microseconds)
pub fn getInternalRunTime() !i64 {
    const ts = try std.posix.clock_gettime(.PROCESS_CPUTIME_ID);
    return @as(i64, ts.sec) * 1_000_000 + @divTrunc(@as(i64, ts.nsec), 1000);
}

/// Decoded time components
pub const DecodedTime = struct {
    second: i64, // 0-59
    minute: i64, // 0-59
    hour: i64, // 0-23
    date: i64, // 1-31
    month: i64, // 1-12
    year: i64, // e.g., 2024
    day_of_week: i64, // 0=Monday, 6=Sunday
    daylight_p: bool, // true if daylight saving time
    zone: i64, // time zone offset in hours (negative = west of GMT)
};

/// Decode universal time to calendar components
/// Universal time is seconds since 1900-01-01 00:00:00 UTC
pub fn decodeUniversalTime(universal_time: i64, time_zone: ?i64) DecodedTime {
    // Convert to Unix timestamp
    const unix_seconds = universal_time - 2208988800;

    // Get the time zone offset (hours west of GMT)
    const tz_offset_hours: i64 = time_zone orelse 0;

    // Adjust for timezone (CL timezone is hours west, so subtract to get local)
    const local_seconds = unix_seconds - (tz_offset_hours * 3600);

    // Extract time components
    const day_seconds = @mod(local_seconds, 86400);
    const second = @mod(day_seconds, 60);
    const minute = @mod(@divFloor(day_seconds, 60), 60);
    const hour = @divFloor(day_seconds, 3600);

    // Calculate days since Unix epoch
    var days = @divFloor(local_seconds, 86400);
    // Adjust to algorithm epoch (March 1, year 0)
    days += 719468;

    // Calculate year, month, day using the civil calendar algorithm
    const era: i64 = @divFloor(if (days >= 0) days else days - 146096, 146097);
    const doe: i64 = days - era * 146097; // day of era [0, 146096]
    const yoe: i64 = @divFloor(doe - @divFloor(doe, 1460) + @divFloor(doe, 36524) - @divFloor(doe, 146096), 365);
    const y: i64 = yoe + era * 400;
    const doy: i64 = doe - (365 * yoe + @divFloor(yoe, 4) - @divFloor(yoe, 100)); // day of year [0, 365]
    const mp: i64 = @divFloor(5 * doy + 2, 153); // month index [0, 11]
    const d: i64 = doy - @divFloor(153 * mp + 2, 5) + 1; // day [1, 31]
    const m: i64 = mp + @as(i64, if (mp < 10) 3 else -9); // month [1, 12]
    const year = y + @as(i64, if (m <= 2) 1 else 0);

    // Day of week (0 = Monday)
    // January 1, 1970 was Thursday, which is day 3 in 0=Monday system
    const dow = @mod(days + 3, 7);

    return .{
        .second = second,
        .minute = minute,
        .hour = hour,
        .date = d,
        .month = m,
        .year = year,
        .day_of_week = dow,
        .daylight_p = false, // TODO: detect DST
        .zone = tz_offset_hours,
    };
}

/// Encode calendar components to universal time
pub fn encodeUniversalTime(
    second: i64,
    minute: i64,
    hour: i64,
    date: i64,
    month: i64,
    year: i64,
    time_zone: ?i64,
) i64 {
    // Get time zone offset (hours west of GMT)
    const tz_offset_hours: i64 = time_zone orelse 0;

    // Calculate days since Unix epoch using a simple algorithm
    // This handles the Gregorian calendar correctly
    const y = year - @as(i64, if (month <= 2) @as(i64, 1) else 0);
    const m_adj = month + @as(i64, if (month <= 2) @as(i64, 12) else 0);

    // Days from years
    var days: i64 = 365 * y + @divFloor(y, 4) - @divFloor(y, 100) + @divFloor(y, 400);
    // Days from months (using the offset formula)
    days += @divFloor(153 * (m_adj - 3) + 2, 5);
    // Add day of month
    days += date;
    // Adjust to Unix epoch (days from year 0 to 1970-01-01)
    days -= 719528;

    // Convert to seconds
    const day_seconds = second + minute * 60 + hour * 3600;
    var unix_seconds = days * 86400 + day_seconds;

    // Adjust for timezone (add hours west of GMT to get UTC)
    unix_seconds += tz_offset_hours * 3600;

    // Convert Unix time to Universal Time (add 70 years)
    return unix_seconds + 2208988800;
}

/// Print memory usage statistics
pub fn room(allocations: usize, bytes_allocated: usize, gc_count: usize, bytes_copied: usize) void {
    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;
    w.print("; Memory usage:\n", .{}) catch {};
    w.print(";   Allocations: {d}\n", .{allocations}) catch {};
    w.print(";   Bytes allocated: {d}\n", .{bytes_allocated}) catch {};
    w.print(";   GC collections: {d}\n", .{gc_count}) catch {};
    w.print(";   Bytes copied (last GC): {d}\n", .{bytes_copied}) catch {};
    w.flush() catch {};
}

/// Get current time in milliseconds
pub fn currentTimeMillis() i64 {
    return std.time.milliTimestamp();
}

/// Sleep for milliseconds
pub fn sleep(ms: i64) void {
    if (ms <= 0) return;
    std.Thread.sleep(@as(u64, @intCast(ms)) * std.time.ns_per_ms);
}

/// Sleep for seconds (ANSI CL SLEEP function)
pub fn sleepSeconds(seconds: Value) !void {
    if (!seconds.isFixnum()) return error.TypeError;
    const secs = seconds.toFixnum();
    if (secs <= 0) return;
    std.Thread.sleep(@as(u64, @intCast(secs)) * std.time.ns_per_s);
}

// Custom error types
const IoError = struct {
    const FileTooLarge = std.fs.File.OpenError || std.fs.File.StatError;
    const InvalidArgument = std.fs.File.OpenError;
};

// ============================================================================
// Tests
// ============================================================================

test "write bytes" {
    // This test just verifies the function compiles
    // Actual I/O testing would require mocking
    _ = sysWriteBytes;
}

test "*print-escape* flag" {
    const testing = std.testing;

    // Default is true
    try testing.expect(print_escape == true);

    // getPrintEscape returns t
    try testing.expect(getPrintEscape().eq(Value.t));

    // Set to false
    setPrintEscape(Value.nil);
    try testing.expect(print_escape == false);
    try testing.expect(getPrintEscape().eq(Value.nil));

    // Set to true
    setPrintEscape(Value.t);
    try testing.expect(print_escape == true);
    try testing.expect(getPrintEscape().eq(Value.t));
}

test "*print-case* flag" {
    const testing = std.testing;
    // Default is upcase
    try testing.expect(print_case == .upcase);

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    const builtins = try builtins_mod.BuiltinSymbols.init(&heap);

    // Test getPrintCase
    const upcase_kw = try getPrintCase(&heap);
    try testing.expect(upcase_kw.isKeyword());
    try testing.expect(std.mem.eql(u8, upcase_kw.toPtr(objects.Keyword).getName(), "UPCASE"));

    // Test setPrintCase to downcase
    try setPrintCase(&builtins, builtins.kw_downcase);
    try testing.expect(print_case == .downcase);
    const downcase_result = try getPrintCase(&heap);
    try testing.expect(std.mem.eql(u8, downcase_result.toPtr(objects.Keyword).getName(), "DOWNCASE"));

    // Test capitalize
    try setPrintCase(&builtins, builtins.kw_capitalize);
    try testing.expect(print_case == .capitalize);

    // Reset to default
    print_case = .upcase;
}

test "symbol printing with *print-case*" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    _ = try heap.intern("TeSt");
    _ = try heap.intern("TeSt");

    var buf = std.ArrayList(u8){};
    defer buf.deinit(testing.allocator);

    // Upcase
    print_case = .upcase;
    const w_up = buf.writer(testing.allocator);
    try writeCaseSymbol("TeSt", w_up.any());
    try testing.expectEqualStrings("TEST", buf.items);

    // Downcase
    buf.clearRetainingCapacity();
    print_case = .downcase;
    const w_down = buf.writer(testing.allocator);
    try writeCaseSymbol("TeSt", w_down.any());
    try testing.expectEqualStrings("test", buf.items);

    // Capitalize
    buf.clearRetainingCapacity();
    print_case = .capitalize;
    const w_cap = buf.writer(testing.allocator);
    try writeCaseSymbol("TeSt", w_cap.any());
    try testing.expectEqualStrings("Test", buf.items);

    // Reset
    print_case = .upcase;
}

test "*print-base*/*print-radix* flags" {
    const testing = std.testing;

    // Default base is 10, radix is false
    try testing.expectEqual(@as(u8, 10), print_base);
    try testing.expect(print_radix == false);

    // Test getPrintBase/setPrintBase
    try testing.expect(getPrintBase().eq(Value.makeFixnum(10)));

    try setPrintBase(Value.makeFixnum(16));
    try testing.expectEqual(@as(u8, 16), print_base);
    try testing.expect(getPrintBase().eq(Value.makeFixnum(16)));

    try setPrintBase(Value.makeFixnum(2));
    try testing.expectEqual(@as(u8, 2), print_base);

    // Invalid base should error
    try testing.expectError(error.InvalidBase, setPrintBase(Value.makeFixnum(1)));
    try testing.expectError(error.InvalidBase, setPrintBase(Value.makeFixnum(37)));

    // Type error
    try testing.expectError(error.TypeError, setPrintBase(Value.nil));

    // Test getPrintRadix/setPrintRadix
    try testing.expect(getPrintRadix().eq(Value.nil));

    setPrintRadix(Value.t);
    try testing.expect(print_radix == true);
    try testing.expect(getPrintRadix().eq(Value.t));

    setPrintRadix(Value.nil);
    try testing.expect(print_radix == false);

    // Reset
    print_base = 10;
    print_radix = false;
}

test "*print-gensym* flag" {
    const testing = std.testing;

    // Default is true
    try testing.expect(print_gensym == true);
    try testing.expect(getPrintGensym().eq(Value.t));

    // Set to false
    setPrintGensym(Value.nil);
    try testing.expect(print_gensym == false);
    try testing.expect(getPrintGensym().eq(Value.nil));

    // Set to true
    setPrintGensym(Value.t);
    try testing.expect(print_gensym == true);

    // Reset
    print_gensym = true;
}

test "*print-array* flag" {
    const testing = std.testing;

    // Default is true
    try testing.expect(print_array == true);
    try testing.expect(getPrintArray().eq(Value.t));

    // Set to false
    setPrintArray(Value.nil);
    try testing.expect(print_array == false);
    try testing.expect(getPrintArray().eq(Value.nil));

    // Set to true
    setPrintArray(Value.t);
    try testing.expect(print_array == true);

    // Reset
    print_array = true;
}

/// Check if value is a stream
pub fn streamp(val: Value) bool {
    return val.isStream();
}

/// Check if stream is input stream
pub fn inputStreamP(stream: Value) bool {
    if (!stream.isStream()) return false;
    const s = stream.toPtr(objects.Stream);
    return s.direction == .input;
}

/// Check if stream is output stream
pub fn outputStreamP(stream: Value) bool {
    if (!stream.isStream()) return false;
    const s = stream.toPtr(objects.Stream);
    return s.direction == .output;
}

/// Check if stream is interactive (tty)
pub fn interactiveStreamP(stream: Value) bool {
    if (!stream.isStream()) return false;
    const s = stream.toPtr(objects.Stream);
    if (s.stream_type != .file) return false;
    const fd: std.posix.fd_t = @intCast(s.file_fd);
    return std.posix.isatty(fd);
}

/// Check if stream is open
pub fn openStreamP(stream: Value) bool {
    if (!stream.isStream()) return false;
    const s = stream.toPtr(objects.Stream);
    return !s.closed;
}

/// Get stream element type
pub fn streamElementType(heap: *Heap, stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    return switch (s.stream_type) {
        .string, .file => heap.intern("character"),
        .byte => heap.intern("unsigned-byte"),
    };
}

/// Get file length in elements
pub fn fileLength(stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);

    switch (s.stream_type) {
        .string => return Value.makeFixnum(@intCast(s.length)),
        .file => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            const stat = try std.posix.fstat(fd);
            return Value.makeFixnum(@intCast(stat.size));
        },
        .byte => return error.NotImplemented,
    }
}

/// Create a string input stream
pub fn makeStringInputStream(heap: *Heap, str: Value, start: ?Value, end: ?Value) !Value {
    _ = start;
    _ = end;
    if (!str.isString()) return error.TypeError;
    return try heap.allocStringInputStream(str);
}

/// Create a string output stream
pub fn makeStringOutputStream(heap: *Heap) !Value {
    return try heap.allocStringOutputStream();
}

/// Get the accumulated string from an output stream
pub fn getOutputStreamString(heap: *Heap, stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .output or s.stream_type != .string) return error.TypeError;
    if (s.data_ptr == 0) return error.StreamClosed;
    const buf: *std.ArrayList(u8) = @ptrFromInt(s.data_ptr);
    return try heap.allocBaseString(buf.items);
}

/// Read one character from stream
pub fn readChar(stream: Value, eof_error: ?Value, eof_value: ?Value) !Value {
    _ = eof_error;
    _ = eof_value;
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .input) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0 or s.position >= s.length) return Value.nil;
            const data: [*]u8 = @ptrFromInt(s.data_ptr);
            const ch = data[s.position];
            s.position += 1;
            return Value.makeFixnum(@intCast(ch));
        },
        .file => {
            var buf: [1]u8 = undefined;
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            const n = try std.posix.read(fd, &buf);
            if (n == 0) return Value.nil;
            return Value.makeFixnum(@intCast(buf[0]));
        },
        else => return error.NotImplemented,
    }
}

/// Push a character back to stream
pub fn unreadChar(char: Value, stream: Value) !void {
    if (!char.isFixnum()) return error.TypeError;
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .input) return error.TypeError;
    if (s.position > 0) s.position -= 1;
}

/// Peek at next character without consuming
pub fn peekChar(peek_type: ?Value, stream: Value) !Value {
    _ = peek_type;
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .input) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0 or s.position >= s.length) return Value.nil;
            const data: [*]u8 = @ptrFromInt(s.data_ptr);
            return Value.makeFixnum(@intCast(data[s.position]));
        },
        .file => {
            var buf: [1]u8 = undefined;
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            const n = try std.posix.read(fd, &buf);
            if (n == 0) return Value.nil;
            try std.posix.lseek_CUR(fd, -1);
            return Value.makeFixnum(@intCast(buf[0]));
        },
        else => return error.NotImplemented,
    }
}

/// Check if character available (non-blocking)
pub fn listen(stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .input) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            return if (s.data_ptr == 0 or s.position >= s.length) Value.nil else Value.t;
        },
        .file => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            var pollfd = [_]std.posix.pollfd{.{ .fd = fd, .events = std.posix.POLL.IN, .revents = 0 }};
            const ready = try std.posix.poll(&pollfd, 0);
            return if (ready > 0) Value.t else Value.nil;
        },
        .byte => return error.NotImplemented,
    }
}

/// Read character if available, else nil (non-blocking)
pub fn readCharNoHang(stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .input) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0 or s.position >= s.length) return Value.nil;
            const data: [*]u8 = @ptrFromInt(s.data_ptr);
            const ch = data[s.position];
            s.position += 1;
            return Value.makeFixnum(@intCast(ch));
        },
        .file, .stdin => {
            const fd: std.posix.fd_t = if (s.stream_type == .stdin)
                std.posix.STDIN_FILENO
            else
                @intCast(s.file_fd);
            var pollfd = [_]std.posix.pollfd{.{ .fd = fd, .events = std.posix.POLL.IN, .revents = 0 }};
            const ready = try std.posix.poll(&pollfd, 0);
            if (ready == 0) return Value.nil;

            var buf: [1]u8 = undefined;
            const n = try std.posix.read(fd, &buf);
            if (n == 0) return Value.nil;
            return Value.makeCharacter(buf[0]);
        },
        .stdout, .stderr, .broadcast => return error.TypeError, // Output streams, not input
        // Compound streams: delegate or return nil for now
        .concatenated, .echo, .synonym, .two_way => {
            // TODO: Implement proper delegation to underlying streams
            return Value.nil;
        },
    }
}

/// Write one character to stream
pub fn writeChar(char: Value, stream: Value) !void {
    if (!char.isFixnum()) return error.TypeError;
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .output) return error.TypeError;

    const ch: u8 = @intCast(char.toFixnum());
    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            const buf: *std.ArrayList(u8) = @ptrFromInt(s.data_ptr);
            try buf.append(ch);
        },
        .file => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            _ = try std.posix.write(fd, &[_]u8{ch});
        },
        .byte => return error.NotImplemented,
    }
}

/// Read a line from stream
pub fn readLine(heap: *Heap, stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .input) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0 or s.position >= s.length) return Value.nil;
            const data: [*]u8 = @ptrFromInt(s.data_ptr);
            const start = s.position;
            var i = start;
            while (i < s.length and data[i] != '\n') : (i += 1) {}
            const line = data[start..i];
            s.position = if (i < s.length) i + 1 else i;
            return try heap.allocBaseString(line);
        },
        .file => {
            var buf: [LINE_BUF]u8 = undefined;
            var len: usize = 0;
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            while (len < LINE_BUF) {
                var ch: [1]u8 = undefined;
                const n = try std.posix.read(fd, &ch);
                if (n == 0) break;
                if (ch[0] == '\n') break;
                buf[len] = ch[0];
                len += 1;
            }
            return try heap.allocBaseString(buf[0..len]);
        },
        .byte => return error.NotImplemented,
    }
}

/// Write a string to stream
pub fn writeString(str: Value, stream: Value, start: ?Value, end: ?Value) !void {
    _ = start;
    _ = end;
    if (!str.isString()) return error.TypeError;
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .output) return error.TypeError;

    const string = str.toPtr(objects.String);
    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            const buf: *std.ArrayList(u8) = @ptrFromInt(s.data_ptr);
            try buf.appendSlice(string.bytes());
        },
        .file => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            _ = try std.posix.write(fd, string.bytes());
        },
        .byte => return error.NotImplemented,
    }
}

/// Write a string followed by newline
pub fn writeLine(str: Value, stream: Value) !void {
    try writeString(str, stream, null, null);
    try writeChar(Value.makeFixnum('\n'), stream);
}

/// Flush output and wait
pub fn finishOutput(stream: Value) !void {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .output) return error.TypeError;

    switch (s.stream_type) {
        .string => {}, // No-op for string streams
        .file => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            try std.posix.fsync(fd);
        },
        .byte => return error.NotImplemented,
    }
}

/// Flush output without waiting
pub fn forceOutput(stream: Value) !void {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .output) return error.TypeError;

    switch (s.stream_type) {
        .string => {}, // No-op for string streams
        .file => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            _ = fd; // Force flush already happens on write
        },
        .byte => return error.NotImplemented,
    }
}

/// Discard buffered output
pub fn clearOutput(stream: Value) !void {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .output) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            const buf: *std.ArrayList(u8) = @ptrFromInt(s.data_ptr);
            buf.clearRetainingCapacity();
        },
        .file => {}, // Can't clear OS buffer
        .byte => return error.NotImplemented,
    }
}

/// Discard buffered input
pub fn clearInput(stream: Value) !void {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .input) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            s.position = s.length;
        },
        .file => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            var pollfd = [_]std.posix.pollfd{.{ .fd = fd, .events = std.posix.POLL.IN, .revents = 0 }};
            while (true) {
                const ready = try std.posix.poll(&pollfd, 0);
                if (ready == 0) break;
                var buf: [IO_BUF]u8 = undefined;
                const n = try std.posix.read(fd, &buf);
                if (n == 0) break;
            }
        },
        .byte => return error.NotImplemented,
    }
}

/// Output newline
pub fn terpri(stream: Value) !void {
    try writeChar(Value.makeFixnum('\n'), stream);
}

/// Output newline only if not at line start
pub fn freshLine(stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (s.direction != .output) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            const buf: *std.ArrayList(u8) = @ptrFromInt(s.data_ptr);
            if (buf.items.len == 0 or buf.items[buf.items.len - 1] == '\n') {
                return Value.nil;
            }
            try buf.append('\n');
            return Value.t;
        },
        .file => {
            try writeChar(Value.makeFixnum('\n'), stream);
            return Value.t;
        },
        .byte => return error.NotImplemented,
    }
}

/// Get/set file position
pub fn filePosition(heap: *Heap, stream: Value, pos: ?Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);

    if (pos == null) {
        // Get current position
        switch (s.stream_type) {
            .string => return Value.makeFixnum(@intCast(s.position)),
            .file => {
                const fd: std.posix.fd_t = @intCast(s.file_fd);
                const cur = try std.posix.lseek_CUR_get(fd);
                return Value.makeFixnum(@intCast(cur));
            },
            else => return error.NotImplemented,
        }
    } else {
        // Set position
        const p = pos.?;
        const new_pos: i64 = if (p.isKeyword()) blk: {
            const kw_start = try heap.internKeyword("start");
            const kw_end = try heap.internKeyword("end");
            if (p.raw == kw_start.raw) {
                break :blk 0;
            } else if (p.raw == kw_end.raw) {
                break :blk -1;
            } else {
                return error.InvalidArgument;
            }
        } else if (p.isFixnum()) p.toFixnum() else return error.TypeError;

        switch (s.stream_type) {
            .string => {
                if (new_pos == -1) {
                    s.position = s.length;
                } else if (new_pos >= 0) {
                    s.position = @intCast(new_pos);
                } else {
                    return error.InvalidArgument;
                }
                return Value.t;
            },
            .file => {
                const fd: std.posix.fd_t = @intCast(s.file_fd);
                if (new_pos == -1) {
                    try std.posix.lseek_END(fd, 0);
                } else {
                    try std.posix.lseek_SET(fd, @intCast(new_pos));
                }
                return Value.t;
            },
            else => return error.NotImplemented,
        }
    }
}

/// Open a file stream
pub fn openFile(heap: *Heap, filename: Value, direction: ?Value, if_exists: ?Value, if_does_not_exist: ?Value) !Value {
    _ = if_exists;
    _ = if_does_not_exist;
    if (!filename.isString()) return error.TypeError;

    const fname = filename.toPtr(objects.String);
    const kw_input = try heap.internKeyword("input");
    const kw_output = try heap.internKeyword("output");
    const kw_io = try heap.internKeyword("io");
    const dir = if (direction) |d| d else kw_input;

    if (dir.eq(kw_output)) {
        const fd = try std.posix.open(fname.bytes(), .{ .ACCMODE = .WRONLY, .CREAT = true, .TRUNC = true }, 0o644);
        return try heap.allocFileOutputStream(fd);
    } else if (dir.eq(kw_io)) {
        const fd = try std.posix.open(fname.bytes(), .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
        return try heap.allocFileOutputStream(fd);
    } else {
        const fd = try std.posix.open(fname.bytes(), .{ .ACCMODE = .RDONLY }, 0);
        return try heap.allocFileInputStream(fd);
    }
}

/// Close a stream
pub fn closeStream(stream: Value, abort: ?Value) !void {
    _ = abort;
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);

    if (s.stream_type == .file and s.file_fd >= 0) {
        const fd: std.posix.fd_t = @intCast(s.file_fd);
        std.posix.close(fd);
        s.file_fd = -1;
    }
}

/// List files in a directory matching pathname
pub fn listDirectory(heap: *Heap, pathname: Value) !Value {
    // Get path string from pathname or string
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path_str = if (pathname.isString())
        pathname.toPtr(objects.String).bytes()
    else if (pathname.isPathname()) blk: {
        // Build namestring from pathname components
        const pn = pathname.toPtr(objects.Pathname);
        var len: usize = 0;

        // Add directory
        if (pn.directory.isCons()) {
            var dir = pn.directory;
            while (dir.isCons()) {
                const cons = dir.toPtr(objects.Cons);
                if (cons.car.isString()) {
                    const s = cons.car.toPtr(objects.String).bytes();
                    if (len > 0 and len < path_buf.len) {
                        path_buf[len] = '/';
                        len += 1;
                    }
                    const copy_len = @min(s.len, path_buf.len - len);
                    @memcpy(path_buf[len..][0..copy_len], s[0..copy_len]);
                    len += copy_len;
                }
                dir = cons.cdr;
            }
        }

        // Add name
        if (pn.name.isString()) {
            const s = pn.name.toPtr(objects.String).bytes();
            if (len > 0 and len < path_buf.len) {
                path_buf[len] = '/';
                len += 1;
            }
            const copy_len = @min(s.len, path_buf.len - len);
            @memcpy(path_buf[len..][0..copy_len], s[0..copy_len]);
            len += copy_len;
        }

        break :blk path_buf[0..len];
    } else
        return error.TypeError;

    // Handle wildcards - for now, just list all files in directory
    // Strip trailing wildcard if present
    var dir_path = path_str;
    if (std.mem.endsWith(u8, dir_path, "*.*") or std.mem.endsWith(u8, dir_path, "*")) {
        // Find last path separator
        if (std.mem.lastIndexOf(u8, dir_path, "/")) |idx| {
            dir_path = dir_path[0..idx];
        } else {
            dir_path = ".";
        }
    }

    // Open directory
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        return switch (err) {
            error.FileNotFound, error.NotDir => Value.nil,
            else => err,
        };
    };
    defer dir.close();

    // Build list of pathnames
    var result = Value.nil;
    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        // Build full path
        var full_path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const full_path = try std.fmt.bufPrint(&full_path_buf, "{s}/{s}", .{ dir_path, entry.name });

        // Parse path into pathname components
        const name_str = try heap.allocBaseString(std.fs.path.stem(entry.name));
        const type_str = if (std.fs.path.extension(entry.name).len > 0)
            try heap.allocBaseString(std.fs.path.extension(entry.name)[1..]) // Skip the '.'
        else
            Value.nil;
        const dir_str = try heap.allocBaseString(dir_path);
        const dir_list = try heap.allocCons(dir_str, Value.nil);

        const pn = try heap.allocPathname(
            Value.nil, // host
            Value.nil, // device
            dir_list, // directory
            name_str, // name
            type_str, // type
            Value.nil, // version
        );
        _ = full_path;
        result = try heap.allocCons(pn, result);
    }
    return result;
}

/// Check if pathname matches wildcard pattern
pub fn pathnameMatchP(pathname: Value, wildcard: Value) !Value {
    // Get path strings
    const pn_str = if (pathname.isString())
        pathname.toPtr(objects.String).bytes()
    else if (pathname.isPathname()) blk: {
        const pn = pathname.toPtr(objects.Pathname);
        if (pn.name.isString()) break :blk pn.name.toPtr(objects.String).bytes();
        break :blk "";
    } else
        return error.TypeError;

    const wild_str = if (wildcard.isString())
        wildcard.toPtr(objects.String).bytes()
    else if (wildcard.isPathname()) blk: {
        const wc = wildcard.toPtr(objects.Pathname);
        if (wc.name.isString()) break :blk wc.name.toPtr(objects.String).bytes();
        break :blk "*";
    } else
        return error.TypeError;

    // Simple wildcard matching: * matches anything
    if (std.mem.eql(u8, wild_str, "*")) return Value.t;

    // Check for *.ext pattern
    if (wild_str.len > 1 and wild_str[0] == '*' and wild_str[1] == '.') {
        const ext = wild_str[1..]; // includes the dot
        if (std.mem.endsWith(u8, pn_str, ext)) return Value.t;
        return Value.nil;
    }

    // Exact match
    if (std.mem.eql(u8, pn_str, wild_str)) return Value.t;

    return Value.nil;
}

test "time functions" {
    const testing = std.testing;

    const before = currentTimeMillis();
    sleep(10); // 10ms
    const after = currentTimeMillis();

    // Should have elapsed at least 10ms
    try testing.expect(after >= before + 10);

    const run_time = try getInternalRunTime();
    try testing.expect(run_time >= 0);
}

test "fileExists and probeFile" {
    const testing = std.testing;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.parent_dir.realpathAlloc(testing.allocator, &tmp.sub_path);
    defer testing.allocator.free(tmp_path);

    const file_name = "exists.txt";
    const file = try tmp.dir.createFile(file_name, .{});
    file.close();

    const exists_path = try std.fs.path.join(testing.allocator, &.{ tmp_path, file_name });
    defer testing.allocator.free(exists_path);
    try testing.expect(try fileExists(exists_path));
    try testing.expect(try probeFile(exists_path));

    const missing_path = try std.fs.path.join(testing.allocator, &.{ tmp_path, "missing.txt" });
    defer testing.allocator.free(missing_path);
    try testing.expect(!(try fileExists(missing_path)));
    try testing.expect(!(try probeFile(missing_path)));
}
