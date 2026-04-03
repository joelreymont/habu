//! I/O primitives
//!
//! sys-read, sys-write, sys-exit, file operations

const std = @import("std");
const fs = std.fs;
const runtime_mod = @import("../runtime.zig");
const Value = @import("../value.zig").Value;
const objects = @import("../objects.zig");
const heap_mod = @import("../heap.zig");
const builtins_mod = @import("../builtins.zig");
const pathname_prim = @import("pathname.zig");

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

pub const StructPrintHook = struct {
    ctx: *anyopaque,
    write_fn: *const fn (*anyopaque, Value, Value, usize) anyerror!bool,
};

pub fn writeFixnumTo(n: i64, w: anytype) !void {
    try writeFixnum(n, w);
}

/// Write a float, ensuring a decimal point is always present (CL spec: 3.0 not 3)
pub fn writeFloatTo(f: f64, w: anytype) !void {
    return writeFloat(f, w);
}

fn writeFloat(f: f64, w: anytype) !void {
    var buf: [400]u8 = undefined;
    const formatted = std.fmt.bufPrint(&buf, "{d}", .{f}) catch blk: {
        // Fall back to scientific notation for very large values
        break :blk std.fmt.bufPrint(&buf, "{e}", .{f}) catch "0.0";
    };
    try w.writeAll(formatted);
    // If no decimal point in output, append ".0"
    var has_dot = false;
    for (formatted) |c| {
        if (c == '.' or c == 'e' or c == 'E' or c == 'n' or c == 'i') {
            has_dot = true;
            break;
        }
    }
    if (!has_dot) try w.writeAll(".0");
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

pub fn formatIntBase(n: i64, base: u8, buf: []u8) usize {
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

fn readByteMaybe(reader: anytype) !?u8 {
    const ReaderT = @TypeOf(reader);
    const TargetT = switch (@typeInfo(ReaderT)) {
        .pointer => |ptr| ptr.child,
        else => ReaderT,
    };

    if (@hasDecl(TargetT, "takeByte")) {
        return if (reader.takeByte()) |byte| byte else |err| switch (err) {
            error.EndOfStream => null,
            else => return err,
        };
    }

    if (@hasDecl(TargetT, "read")) {
        var byte: [1]u8 = undefined;
        const count = try reader.read(&byte);
        if (count == 0) return null;
        return byte[0];
    }

    @compileError("readByteMaybe requires reader.takeByte or reader.read");
}

/// Read a line from stdin (allocates in heap)
pub fn sysReadLine(heap: *heap_mod.Heap) !Value {
    const stdin_file = fs.File.stdin();
    var read_buf: [IO_BUF]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    var line = std.ArrayList(u8){};
    defer line.deinit(heap.backing_allocator);
    var read_any = false;

    while (true) {
        const byte_opt: ?u8 = if (pushback_char) |ch| blk: {
            pushback_char = null;
            break :blk ch;
        } else try readByteMaybe(reader);
        if (byte_opt == null) break;
        const byte = byte_opt.?;
        read_any = true;
        if (byte == '\n') break;
        try line.append(heap.backing_allocator, byte);
    }

    if (!read_any and line.items.len == 0) return Value.nil;
    return try heap.allocBaseString(line.items);
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

    const byte_opt = try readByteMaybe(reader);
    if (byte_opt == null) return -1;
    return @intCast(byte_opt.?);
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

    const byte_opt = try readByteMaybe(reader);
    if (byte_opt == null) return -1;
    const byte = byte_opt.?;
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
        const byte_opt: ?u8 = if (pushback_char) |ch| blk: {
            pushback_char = null;
            break :blk ch;
        } else try readByteMaybe(reader);
        if (byte_opt == null) break;
        const byte = byte_opt.?;

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

    try princValueTo(val, w, 0, null, null);
    try w.flush();
}

fn princValueTo(val: Value, w: anytype, level: usize, stream: ?Value, hook: ?StructPrintHook) !void {
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
        .float => try writeFloat(val.toFloat(), w),
        .char => {
            const cp = val.toCharacter();
            if (cp < 128) {
                try w.writeByte(@as(u8, @intCast(cp)));
            } else {
                var utf8_buf: [4]u8 = undefined;
                const len = try std.unicode.utf8Encode(@intCast(cp), &utf8_buf);
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
                try princValueTo(cons.car, w, level + 1, stream, hook);
                current = cons.cdr;
                count += 1;
            }
            if (!current.isNil() and (print_length == null or count < print_length.?)) {
                try w.writeAll(" . ");
                try princValueTo(current, w, level + 1, stream, hook);
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
                const len = try std.unicode.utf8Encode(@intCast(cp), &utf8_buf);
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
                    try princValueTo(items[i], w, level + 1, stream, hook);
                }
                if (print_length) |max_len| {
                    if (items.len > max_len) {
                        try w.writeAll(" ...");
                    }
                }
                try w.writeByte(')');
            }
        },
        .structure => {
            if (stream) |out_stream| {
                if (hook) |printer| {
                    if (try printer.write_fn(printer.ctx, val, out_stream, level)) return;
                }
            }
            const obj = val.toPtr(objects.Structure);
            if (obj.class.isClass() and obj.class.toPtr(objects.Class).name.isSymbol()) {
                try w.print("#<structure {s}>", .{obj.class.toPtr(objects.Class).name.toPtr(objects.Symbol).getName()});
            } else {
                try w.writeAll("#<structure>");
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
            const stream_obj = val.toPtr(objects.Stream);
            const dir = switch (stream_obj.direction) {
                .input => "input",
                .output => "output",
                .io => "io",
            };
            const kind = switch (stream_obj.stream_type) {
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
                .byte => "byte",
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
        .native_code => try w.writeAll("#<native-code>"),
        .macro_env => try w.writeAll("#<macro-env>"),
    }
}

/// Write value to any writer (for write-to-string)
pub fn writeValueToBuffer(val: Value, w: anytype) !void {
    try printValueTo(val, w, null, null);
}

/// Convert value to string (write-to-string primitive)
pub fn writeToString(heap: *heap_mod.Heap, val: Value) !Value {
    var buf = std.ArrayList(u8){};
    const w = buf.writer(heap.backing_allocator);
    try printValueTo(val, w.any(), null, null);
    const bytes = try buf.toOwnedSlice(heap.backing_allocator);
    defer heap.backing_allocator.free(bytes);
    return try heap.allocBaseString(bytes);
}

fn printValueTo(val: Value, w: anytype, stream: ?Value, hook: ?StructPrintHook) !void {
    if (print_readably or print_escape) {
        return printEscapedTo(val, w, 0, stream, hook);
    } else {
        return princValueTo(val, w, 0, stream, hook);
    }
}

fn printEscapedTo(val: Value, w: anytype, level: usize, stream: ?Value, hook: ?StructPrintHook) !void {
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
        .float => try writeFloat(val.toFloat(), w),
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
                try printEscapedTo(cons.car, w, level + 1, stream, hook);
                current = cons.cdr;
                count += 1;
            }
            if (!current.isNil()) {
                if (print_readably or print_length == null or count < print_length.?) {
                    try w.writeAll(" . ");
                    try printEscapedTo(current, w, level + 1, stream, hook);
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
                const len = try std.unicode.utf8Encode(@intCast(cp), &utf8_buf);
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
                try printEscapedTo(items[i], w, level + 1, stream, hook);
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
        .structure => {
            if (stream) |out_stream| {
                if (hook) |printer| {
                    if (try printer.write_fn(printer.ctx, val, out_stream, level)) return;
                }
            }
            const obj = val.toPtr(objects.Structure);
            if (obj.class.isClass() and obj.class.toPtr(objects.Class).name.isSymbol()) {
                try w.print("#<structure {s}>", .{obj.class.toPtr(objects.Class).name.toPtr(objects.Symbol).getName()});
            } else {
                try w.writeAll("#<structure>");
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
            const stream_obj = val.toPtr(objects.Stream);
            const dir = switch (stream_obj.direction) {
                .input => "input",
                .output => "output",
                .io => "io",
            };
            const kind = switch (stream_obj.stream_type) {
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
                .byte => "byte",
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
        .native_code => try w.writeAll("#<native-code>"),
        .macro_env => try w.writeAll("#<macro-env>"),
    }
}

// ============================================================================
// CL Output Primitives
// ============================================================================

const StreamSink = struct {
    stream: Value,

    pub fn writeAll(self: *const StreamSink, bytes: []const u8) !void {
        try writeBytesToStream(self.stream, bytes);
    }

    pub fn writeByte(self: *const StreamSink, byte: u8) !void {
        var buf: [1]u8 = .{byte};
        try writeBytesToStream(self.stream, buf[0..]);
    }

    fn writeAny(ctx: *const anyopaque, bytes: []const u8) anyerror!usize {
        const self: *const StreamSink = @ptrCast(@alignCast(ctx));
        try self.writeAll(bytes);
        return bytes.len;
    }

    pub fn print(self: *const StreamSink, comptime fmt: []const u8, args: anytype) !void {
        const w = std.io.AnyWriter{ .context = self, .writeFn = writeAny };
        try w.print(fmt, args);
    }

    pub fn flush(self: *const StreamSink) !void {
        try finishOutput(self.stream);
    }
};

pub fn writeBytesToStream(stream: Value, bytes: []const u8) !void {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isOutput()) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            // String output streams use raw [*]u8 buffer
            // data_ptr = buffer ptr, position = capacity, length = bytes written
            const new_len = s.length + bytes.len;
            if (new_len > s.position) {
                // Grow buffer
                const new_capacity = @max(new_len * 2, 256);
                const old_buf: ?[*]u8 = if (s.data_ptr != 0)
                    @ptrFromInt(s.data_ptr)
                else
                    null;
                const new_buf = try std.heap.page_allocator.alloc(u8, new_capacity);
                if (old_buf) |old| {
                    @memcpy(new_buf[0..s.length], old[0..s.length]);
                    std.heap.page_allocator.free(old[0..s.position]);
                }
                s.data_ptr = @intFromPtr(new_buf.ptr);
                s.position = new_capacity;
            }
            if (bytes.len != 0) {
                const buf: [*]u8 = @ptrFromInt(s.data_ptr);
                @memcpy(buf[s.length..][0..bytes.len], bytes);
            }
            s.length += bytes.len;
        },
        .file, .stdout, .stderr => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            _ = try std.posix.write(fd, bytes);
        },
        .broadcast => {
            var list = s.source_value;
            while (list.isCons()) {
                const cons = list.toPtr(objects.Cons);
                try writeBytesToStream(cons.car, bytes);
                list = cons.cdr;
            }
            if (!list.isNil()) return error.InvalidArgument;
        },
        .two_way => {
            // Write to the output component of the two-way stream
            const pair = s.source_value.toPtr(objects.Cons);
            try writeBytesToStream(pair.cdr, bytes);
        },
        .synonym => {
            try writeBytesToStream(try resolveSynonymTarget(s), bytes);
        },
        .echo => {
            // Write to the output component of the echo stream
            const pair = s.source_value.toPtr(objects.Cons);
            try writeBytesToStream(pair.cdr, bytes);
        },
        .stdin, .concatenated => return error.TypeError,
        .byte => return error.NotImplemented,
    }
}

fn withOutputWriter(stream: Value, buf: *[IO_BUF]u8, val: Value, f: anytype) !void {
    if (stream.isNil()) {
        const stdout_file = fs.File.stdout();
        var file_writer = stdout_file.writer(buf);
        const w = &file_writer.interface;
        try f(w, val);
        try w.flush();
        return;
    }
    if (!stream.isStream()) return error.TypeError;
    var stream_writer = StreamSink{ .stream = stream };
    const w = &stream_writer;
    try f(w, val);
    try w.flush();
}

fn writeImpl(w: anytype, val: Value) !void {
    try printValueTo(val, w, null, null);
}

fn printImpl(w: anytype, val: Value) !void {
    try w.writeByte('\n');
    try printValueTo(val, w, null, null);
    try w.writeByte(' ');
}

pub fn writeWithHook(val: Value, stream: Value, hook: StructPrintHook) !Value {
    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = true;
    if (!stream.isStream()) return error.TypeError;
    var sink = StreamSink{ .stream = stream };
    try printValueTo(val, &sink, stream, hook);
    try sink.flush();
    return val;
}

pub fn princWithHook(val: Value, stream: Value, hook: StructPrintHook) !Value {
    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = false;
    if (!stream.isStream()) return error.TypeError;
    var sink = StreamSink{ .stream = stream };
    try printValueTo(val, &sink, stream, hook);
    try sink.flush();
    return val;
}

pub fn printWithHook(val: Value, stream: Value, hook: StructPrintHook) !Value {
    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = true;
    if (!stream.isStream()) return error.TypeError;
    var sink = StreamSink{ .stream = stream };
    try sink.writeByte('\n');
    try printValueTo(val, &sink, stream, hook);
    try sink.writeByte(' ');
    try sink.flush();
    return val;
}

pub fn writeToStringWithHook(heap: *heap_mod.Heap, val: Value, hook: StructPrintHook) !Value {
    const stream = try heap.allocStringOutputStream();
    _ = try writeWithHook(val, stream, hook);
    return try getOutputStreamString(heap, stream);
}

/// write object &optional stream - output with *print-escape* = t (readable)
pub fn write(val: Value, stream: Value) !Value {
    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = true;

    var buf: [IO_BUF]u8 = undefined;
    try withOutputWriter(stream, &buf, val, writeImpl);
    return val;
}

/// prin1 object &optional stream - same as write, returns object
pub fn prin1(val: Value, stream: Value) !Value {
    return write(val, stream);
}

/// princ object &optional stream - output with *print-escape* = nil (no escaping)
pub fn princ(val: Value, stream: Value) !Value {
    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = false;

    var buf: [IO_BUF]u8 = undefined;
    try withOutputWriter(stream, &buf, val, writeImpl);
    return val;
}

/// print object &optional stream - output newline, prin1, space, returns object
pub fn print(val: Value, stream: Value) !Value {
    const old_escape = print_escape;
    defer print_escape = old_escape;
    print_escape = true;
    var buf: [IO_BUF]u8 = undefined;
    try withOutputWriter(stream, &buf, val, printImpl);
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
pub fn getPrintCase(heap: *heap_mod.Heap) !Value {
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

fn openPath(path: []const u8, flags: fs.File.OpenFlags) !fs.File {
    if (fs.path.isAbsolute(path)) {
        return fs.openFileAbsolute(path, flags);
    }
    return fs.cwd().openFile(path, flags);
}

fn createPath(path: []const u8, flags: fs.File.CreateFlags) !fs.File {
    if (fs.path.isAbsolute(path)) {
        return fs.createFileAbsolute(path, flags);
    }
    return fs.cwd().createFile(path, flags);
}

fn accessPath(path: []const u8, flags: fs.File.OpenFlags) !void {
    if (fs.path.isAbsolute(path)) {
        return fs.accessAbsolute(path, flags);
    }
    return fs.cwd().access(path, flags);
}

fn deletePath(path: []const u8) !void {
    if (fs.path.isAbsolute(path)) {
        return fs.deleteFileAbsolute(path);
    }
    return fs.cwd().deleteFile(path);
}

fn renamePath(old_path: []const u8, new_path: []const u8) !void {
    const old_abs = fs.path.isAbsolute(old_path);
    const new_abs = fs.path.isAbsolute(new_path);
    if (old_abs and new_abs) return fs.renameAbsolute(old_path, new_path);
    if (!old_abs and !new_abs) return fs.cwd().rename(old_path, new_path);
    return error.InvalidPath;
}

/// Read entire file contents (allocates in heap)
pub fn readFile(heap: *heap_mod.Heap, path: []const u8) !Value {
    const file = try openPath(path, .{});
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

    const file = try createPath(path, .{});
    defer file.close();

    try file.writeAll(str.bytes());
}

/// Check if file exists
pub fn fileExists(path: []const u8) !bool {
    if (accessPath(path, .{})) |_| {
        return true;
    } else |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    }
}

/// Get file size
pub fn fileSize(path: []const u8) !i64 {
    const file = try openPath(path, .{});
    defer file.close();

    const stat = try file.stat();
    return @intCast(stat.size);
}

/// Delete file
pub fn deleteFile(path: []const u8) !void {
    try deletePath(path);
}

/// Rename file
pub fn renameFile(old_path: []const u8, new_path: []const u8) !void {
    try renamePath(old_path, new_path);
}

/// Probe file (check if exists)
/// Returns true if file exists, false otherwise
pub fn probeFile(path: []const u8) !bool {
    if (accessPath(path, .{})) |_| {
        return true;
    } else |err| switch (err) {
        error.FileNotFound => return false,
        else => return err,
    }
}

/// Get file write date (modification time) as Universal Time
/// Universal Time is seconds since 1900-01-01
pub fn fileWriteDate(path: []const u8) !i64 {
    const file = try openPath(path, .{});
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
    days -= 719469;

    // Convert to seconds
    const day_seconds = second + minute * 60 + hour * 3600;
    var unix_seconds = days * 86400 + day_seconds;

    // Adjust for timezone (add hours west of GMT to get UTC)
    unix_seconds += tz_offset_hours * 3600;

    // Convert Unix time to Universal Time (add 70 years)
    return unix_seconds + 2208988800;
}

/// Print memory usage statistics
pub fn room(allocations: usize, bytes_allocated: usize, gc_count: usize, bytes_copied: usize) !void {
    const stdout_file = fs.File.stdout();
    var buf: [IO_BUF]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;
    try w.print("; Memory usage:\n", .{});
    try w.print(";   Allocations: {d}\n", .{allocations});
    try w.print(";   Bytes allocated: {d}\n", .{bytes_allocated});
    try w.print(";   GC collections: {d}\n", .{gc_count});
    try w.print(";   Bytes copied (last GC): {d}\n", .{bytes_copied});
    try w.flush();
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

test "write/princ/print respect stream argument" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const stream = try makeStringOutputStream(&heap);

    _ = try write(Value.makeFixnum(42), stream);
    const out1 = try getOutputStreamString(&heap, stream);
    try testing.expect(std.mem.eql(u8, out1.toPtr(objects.String).bytes(), "42"));

    try clearOutput(stream);
    const hello = try heap.allocBaseString("hi");
    _ = try princ(hello, stream);
    const out2 = try getOutputStreamString(&heap, stream);
    try testing.expect(std.mem.eql(u8, out2.toPtr(objects.String).bytes(), "hi"));

    try clearOutput(stream);
    _ = try print(Value.makeFixnum(7), stream);
    const out3 = try getOutputStreamString(&heap, stream);
    try testing.expect(std.mem.eql(u8, out3.toPtr(objects.String).bytes(), "\n7 "));

    try closeStream(stream, null);
}

test "string output stream length and position" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const stream = try makeStringOutputStream(&heap);
    const hello = try heap.allocBaseString("abc");
    try writeString(hello, stream, null, null);

    const len_val = try fileLength(stream);
    try testing.expect(len_val.isFixnum());
    try testing.expectEqual(@as(i64, 3), len_val.toFixnum());

    const pos_val = try filePosition(&heap, stream, null);
    try testing.expect(pos_val.isFixnum());
    try testing.expectEqual(@as(i64, 3), pos_val.toFixnum());

    const out = try getOutputStreamString(&heap, stream);
    try testing.expect(std.mem.eql(u8, out.toPtr(objects.String).bytes(), "abc"));

    try clearOutput(stream);
    const len_zero = try fileLength(stream);
    try testing.expectEqual(@as(i64, 0), len_zero.toFixnum());
    const pos_zero = try filePosition(&heap, stream, null);
    try testing.expectEqual(@as(i64, 0), pos_zero.toFixnum());

    try closeStream(stream, null);
}

test "read-char-no-hang returns character for string stream" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("a");
    const stream = try makeStringInputStream(&heap, str, null, null);

    const first = try readCharNoHang(stream);
    try testing.expect(first.isCharacter());
    try testing.expectEqual(@as(u21, 'a'), first.toCharacter());

    const second = try readCharNoHang(stream);
    try testing.expect(second.isNil());
}

test "peek-char keeps file position" {
    const testing = std.testing;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("peek.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("ab");
    }

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const fd = try std.posix.openat(tmp.dir.fd, "peek.txt", .{ .ACCMODE = .RDONLY }, 0);
    const stream = try heap.allocFileInputStream(fd);

    const peek = try peekChar(null, stream);
    try testing.expect(peek.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), peek.toFixnum());

    const pos = try filePosition(&heap, stream, null);
    try testing.expect(pos.isFixnum());
    try testing.expectEqual(@as(i64, 0), pos.toFixnum());
}

test "unread-char replays file byte" {
    const testing = std.testing;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("unread.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("ab");
    }

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const fd = try std.posix.openat(tmp.dir.fd, "unread.txt", .{ .ACCMODE = .RDONLY }, 0);
    const stream = try heap.allocFileInputStream(fd);

    const first = try readChar(stream, null, null);
    try testing.expect(first.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), first.toFixnum());

    try unreadChar(Value.makeCharacter(@intCast(first.toFixnum())), stream);

    const again = try readChar(stream, null, null);
    try testing.expect(again.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), again.toFixnum());
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

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
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

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
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

test "io direction is input and output" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const file = try tmp.dir.createFile("io-dir.txt", .{ .read = true, .truncate = true });
    defer file.close();
    const dup_fd = try std.posix.dup(file.handle);
    const stream = try heap.allocStream(.io, .file, @intCast(dup_fd));

    try testing.expect(inputStreamP(stream));
    try testing.expect(outputStreamP(stream));
    try closeStream(stream, null);
}

/// Check if value is a stream
pub fn streamp(val: Value) bool {
    return val.isStream();
}

/// Check if stream is input stream
pub fn inputStreamP(stream: Value) bool {
    if (!stream.isStream()) return false;
    const s = stream.toPtr(objects.Stream);
    return switch (s.stream_type) {
        .synonym => blk: {
            const target = resolveSynonymTarget(s) catch return false;
            break :blk inputStreamP(target);
        },
        else => s.isInput(),
    };
}

/// Check if stream is output stream
pub fn outputStreamP(stream: Value) bool {
    if (!stream.isStream()) return false;
    const s = stream.toPtr(objects.Stream);
    return switch (s.stream_type) {
        .synonym => blk: {
            const target = resolveSynonymTarget(s) catch return false;
            break :blk outputStreamP(target);
        },
        else => s.isOutput(),
    };
}

/// Check if stream is interactive (tty)
pub fn interactiveStreamP(stream: Value) bool {
    if (!stream.isStream()) return false;
    const s = stream.toPtr(objects.Stream);
    if (s.stream_type == .synonym) {
        const target = resolveSynonymTarget(s) catch return false;
        return interactiveStreamP(target);
    }
    if (s.stream_type != .file and s.stream_type != .stdin and s.stream_type != .stdout and s.stream_type != .stderr) return false;
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
pub fn streamElementType(heap: *heap_mod.Heap, stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    return switch (s.stream_type) {
        .string, .file, .stdin, .stdout, .stderr => try heap.intern("character"),
        .byte => try heap.intern("unsigned-byte"),
        .broadcast, .concatenated, .echo, .synonym, .two_way => error.NotImplemented,
    };
}

/// Get file length in elements
pub fn fileLength(stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);

    switch (s.stream_type) {
        .string => return Value.makeFixnum(@intCast(s.length)),
        .file, .stdin, .stdout, .stderr => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            const stat = try std.posix.fstat(fd);
            return Value.makeFixnum(@intCast(stat.size));
        },
        .byte => return error.NotImplemented,
        .broadcast, .concatenated, .echo, .synonym, .two_way => return error.NotImplemented,
    }
}

/// Create a string input stream
const SliceBounds = struct {
    start: usize,
    end: usize,
};

fn sliceBounds(total: usize, start: ?Value, end: ?Value) !SliceBounds {
    var start_idx: usize = 0;
    if (start) |s| {
        if (!s.isFixnum()) return error.TypeError;
        start_idx = std.math.cast(usize, s.toFixnum()) orelse return error.InvalidArgument;
    }

    var end_idx = total;
    if (end) |e| {
        if (!e.isFixnum()) return error.TypeError;
        end_idx = std.math.cast(usize, e.toFixnum()) orelse return error.InvalidArgument;
    }

    if (start_idx > end_idx or end_idx > total) return error.InvalidArgument;
    return .{ .start = start_idx, .end = end_idx };
}

fn appendUtf8(out: *std.ArrayList(u8), alloc: std.mem.Allocator, cp: u32) !void {
    if (cp > std.math.maxInt(u21)) return error.TypeError;
    var char_buf: [4]u8 = undefined;
    const char_len = std.unicode.utf8Encode(@intCast(cp), &char_buf) catch return error.TypeError;
    try out.appendSlice(alloc, char_buf[0..char_len]);
}

pub fn makeStringInputStream(heap: *heap_mod.Heap, str: Value, start: ?Value, end: ?Value) !Value {
    if (str.isString()) {
        const bytes = str.toPtr(objects.String).bytes();
        const bounds = try sliceBounds(bytes.len, start, end);
        return try heap.allocStringInputStreamRange(str, bounds.start, bounds.end);
    }
    if (str.isString32()) {
        const str32 = str.toPtr(objects.String32);
        const codepoints = str32.codepoints();
        const bounds = try sliceBounds(codepoints.len, start, end);
        var out = std.ArrayList(u8){};
        defer out.deinit(heap.backing_allocator);
        for (codepoints[bounds.start..bounds.end]) |cp| {
            try appendUtf8(&out, heap.backing_allocator, cp);
        }
        const coerced = try heap.allocBaseString(out.items);
        return try heap.allocStringInputStream(coerced);
    }
    // CL: strings are vectors of characters — accept vectors and coerce
    if (str.isVector()) {
        const vec = str.toPtr(objects.Vector);
        const len: usize = @intCast(vec.getFillPointer() orelse vec.length);
        const bounds = try sliceBounds(len, start, end);
        var out = std.ArrayList(u8){};
        defer out.deinit(heap.backing_allocator);
        for (bounds.start..bounds.end) |i| {
            const elem = vec.get(i);
            if (elem.isCharacter()) {
                try appendUtf8(&out, heap.backing_allocator, elem.toCharacter());
            } else {
                return error.TypeError;
            }
        }
        const coerced = try heap.allocBaseString(out.items);
        return try heap.allocStringInputStream(coerced);
    }
    return error.TypeError;
}

/// Create a string output stream
pub fn makeStringOutputStream(heap: *heap_mod.Heap) !Value {
    return try heap.allocStringOutputStream();
}

/// Get the accumulated string from an output stream
pub fn getOutputStreamString(heap: *heap_mod.Heap, stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isOutput() or s.stream_type != .string) return error.TypeError;
    if (s.data_ptr == 0 or s.length == 0) return try heap.allocBaseString("");
    const buf: [*]u8 = @ptrFromInt(s.data_ptr);
    return try heap.allocBaseString(buf[0..s.length]);
}

fn hasPushback(s: *const objects.Stream) bool {
    return s.pushback_char != 0xFF;
}

fn takePushback(s: *objects.Stream) ?u8 {
    if (s.pushback_char == 0xFF) return null;
    const ch = s.pushback_char;
    s.pushback_char = 0xFF;
    return ch;
}

fn setPushback(s: *objects.Stream, ch: u8) !void {
    if (s.pushback_char != 0xFF) return error.InvalidArgument;
    s.pushback_char = ch;
}

fn resolveSynonymTarget(s: *objects.Stream) !Value {
    if (s.stream_type != .synonym) return error.InvalidArgument;
    if (!s.source_value.isSymbol()) return error.TypeError;
    const target = (try runtime_mod.lookupSymbolValue(s.source_value)) orelse return error.UnboundSymbol;
    if (!target.isStream()) return error.TypeError;
    return target;
}

fn streamInputComponent(s: *objects.Stream) !Value {
    return switch (s.stream_type) {
        .echo, .two_way => blk: {
            if (!s.source_value.isCons()) return error.InvalidArgument;
            break :blk s.source_value.toPtr(objects.Cons).car;
        },
        .synonym => resolveSynonymTarget(s),
        else => error.InvalidArgument,
    };
}

fn streamOutputComponent(s: *objects.Stream) !Value {
    return switch (s.stream_type) {
        .echo, .two_way => blk: {
            if (!s.source_value.isCons()) return error.InvalidArgument;
            break :blk s.source_value.toPtr(objects.Cons).cdr;
        },
        .synonym => resolveSynonymTarget(s),
        else => error.InvalidArgument,
    };
}

fn currentConcatenatedInput(s: *objects.Stream) !?Value {
    while (true) {
        if (s.source_value.isNil()) return null;
        if (!s.source_value.isCons()) return error.InvalidArgument;
        const cons = s.source_value.toPtr(objects.Cons);
        if (!cons.car.isStream()) return error.TypeError;
        return cons.car;
    }
}

fn advanceConcatenatedInput(s: *objects.Stream) !void {
    if (!s.source_value.isCons()) return error.InvalidArgument;
    s.source_value = s.source_value.toPtr(objects.Cons).cdr;
}

fn echoFixnumRead(s: *objects.Stream, ch: Value) !void {
    if (ch.isNil()) return;
    const byte = std.math.cast(u8, ch.toFixnum()) orelse return error.InvalidArgument;
    var buf = [_]u8{byte};
    try writeBytesToStream(try streamOutputComponent(s), &buf);
}

fn echoCharacterRead(s: *objects.Stream, ch: Value) !void {
    if (ch.isNil()) return;
    const byte = std.math.cast(u8, ch.toCharacter()) orelse return error.InvalidArgument;
    var buf = [_]u8{byte};
    try writeBytesToStream(try streamOutputComponent(s), &buf);
}

fn readLineViaChars(heap: *heap_mod.Heap, stream: Value) !Value {
    var line = std.ArrayList(u8){};
    defer line.deinit(heap.backing_allocator);

    var read_any = false;
    while (true) {
        const ch_val = try readChar(stream, null, null);
        if (ch_val.isNil()) break;
        read_any = true;
        const ch = std.math.cast(u8, ch_val.toFixnum()) orelse return error.InvalidArgument;
        if (ch == '\n') break;
        try line.append(heap.backing_allocator, ch);
    }

    if (!read_any and line.items.len == 0) return Value.nil;
    return try heap.allocBaseString(line.items);
}

/// Read one character from stream
pub fn readChar(stream: Value, eof_error: ?Value, eof_value: ?Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isInput()) return error.TypeError;
    if (s.closed) return error.StreamClosed;
    if (takePushback(s)) |ch| return Value.makeFixnum(@intCast(ch));

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            if (s.position >= s.length) return Value.nil;
            const data: [*]u8 = @ptrFromInt(s.data_ptr);
            const ch = data[s.position];
            s.position += 1;
            return Value.makeFixnum(@intCast(ch));
        },
        .file, .stdin => {
            if (s.file_fd < 0) return error.StreamClosed;
            var buf: [1]u8 = undefined;
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            const n = try std.posix.read(fd, &buf);
            if (n == 0) return Value.nil;
            return Value.makeFixnum(@intCast(buf[0]));
        },
        .concatenated => {
            while (true) {
                const input = try currentConcatenatedInput(s) orelse return Value.nil;
                const ch = try readChar(input, eof_error, eof_value);
                if (ch.isNil()) {
                    try advanceConcatenatedInput(s);
                    continue;
                }
                return ch;
            }
        },
        .echo => {
            const ch = try readChar(try streamInputComponent(s), eof_error, eof_value);
            try echoFixnumRead(s, ch);
            return ch;
        },
        .synonym, .two_way => return try readChar(try streamInputComponent(s), eof_error, eof_value),
        .broadcast, .stdout, .stderr => return error.TypeError,
        .byte => return error.NotImplemented,
    }
}

/// Push a character back to stream
pub fn unreadChar(char: Value, stream: Value) !void {
    if (!char.isCharacter()) return error.TypeError;
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isInput()) return error.TypeError;
    if (s.closed) return error.StreamClosed;
    const cp = char.toCharacter();
    if (cp > 0xFF) return error.InvalidArgument;

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            const pos = if (std.math.cast(usize, s.position)) |val| val else return error.InvalidArgument;
            if (pos == 0) return error.InvalidArgument;
            const data: [*]u8 = @ptrFromInt(s.data_ptr);
            if (data[pos - 1] != @as(u8, @intCast(cp))) return error.InvalidArgument;
            s.position -= 1;
        },
        .file, .stdin => try setPushback(s, @intCast(cp)),
        .concatenated => {
            const input = try currentConcatenatedInput(s) orelse return error.InvalidArgument;
            try unreadChar(char, input);
        },
        .echo, .synonym, .two_way => try unreadChar(char, try streamInputComponent(s)),
        .broadcast, .stdout, .stderr => return error.TypeError,
        .byte => return error.NotImplemented,
    }
}

/// Peek at next character without consuming
pub fn peekChar(peek_type: ?Value, stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isInput()) return error.TypeError;
    if (s.closed) return error.StreamClosed;
    if (hasPushback(s)) return Value.makeFixnum(@intCast(s.pushback_char));

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            if (s.position >= s.length) return Value.nil;
            const data: [*]u8 = @ptrFromInt(s.data_ptr);
            return Value.makeFixnum(@intCast(data[s.position]));
        },
        .file, .stdin => {
            if (s.file_fd < 0) return error.StreamClosed;
            var buf: [1]u8 = undefined;
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            const n = try std.posix.read(fd, &buf);
            if (n == 0) return Value.nil;
            try setPushback(s, buf[0]);
            return Value.makeFixnum(@intCast(buf[0]));
        },
        .concatenated => {
            while (true) {
                const input = try currentConcatenatedInput(s) orelse return Value.nil;
                const ch = try peekChar(peek_type, input);
                if (ch.isNil()) {
                    try advanceConcatenatedInput(s);
                    continue;
                }
                return ch;
            }
        },
        .echo, .synonym, .two_way => return try peekChar(peek_type, try streamInputComponent(s)),
        .broadcast, .stdout, .stderr => return error.TypeError,
        .byte => return error.NotImplemented,
    }
}

/// Check if character available (non-blocking)
pub fn listen(stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isInput()) return error.TypeError;
    if (s.closed) return error.StreamClosed;

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            if (hasPushback(s)) return Value.t;
            return if (s.position >= s.length) Value.nil else Value.t;
        },
        .file, .stdin => {
            if (hasPushback(s)) return Value.t;
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            var pollfd = [_]std.posix.pollfd{.{ .fd = fd, .events = std.posix.POLL.IN, .revents = 0 }};
            const ready = try std.posix.poll(&pollfd, 0);
            return if (ready > 0) Value.t else Value.nil;
        },
        .concatenated => {
            while (true) {
                const input = try currentConcatenatedInput(s) orelse return Value.nil;
                const ready = try listen(input);
                if (!ready.isNil()) return ready;
                const peeked = try peekChar(null, input);
                if (peeked.isNil()) {
                    try advanceConcatenatedInput(s);
                    continue;
                }
                return Value.t;
            }
        },
        .echo, .synonym, .two_way => return try listen(try streamInputComponent(s)),
        .byte => return error.NotImplemented,
        .broadcast, .stdout, .stderr => return error.TypeError,
    }
}

/// Read character if available, else nil (non-blocking)
pub fn readCharNoHang(stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isInput()) return error.TypeError;
    if (s.closed) return error.StreamClosed;
    if (takePushback(s)) |ch| return Value.makeCharacter(ch);

    switch (s.stream_type) {
        .string => {
            if (s.data_ptr == 0) return error.StreamClosed;
            if (s.position >= s.length) return Value.nil;
            const data: [*]u8 = @ptrFromInt(s.data_ptr);
            const ch = data[s.position];
            s.position += 1;
            return Value.makeCharacter(ch);
        },
        .file, .stdin => {
            if (s.file_fd < 0) return error.StreamClosed;
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
        .concatenated => {
            while (true) {
                const input = try currentConcatenatedInput(s) orelse return Value.nil;
                const ch = try readCharNoHang(input);
                if (ch.isNil()) {
                    if ((try peekChar(null, input)).isNil()) {
                        try advanceConcatenatedInput(s);
                        continue;
                    }
                }
                return ch;
            }
        },
        .echo => {
            const ch = try readCharNoHang(try streamInputComponent(s));
            try echoCharacterRead(s, ch);
            return ch;
        },
        .synonym, .two_way => return try readCharNoHang(try streamInputComponent(s)),
        .stdout, .stderr, .broadcast => return error.TypeError,
        .byte => return error.NotImplemented,
    }
}

/// Write one character to stream
pub fn writeChar(char: Value, stream: Value) !void {
    if (!char.isFixnum()) return error.TypeError;
    const ch: u8 = @intCast(char.toFixnum());
    var buf: [1]u8 = .{ch};
    try writeBytesToStream(stream, buf[0..]);
}

/// Read a line from stream
pub fn readLine(heap: *heap_mod.Heap, stream: Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isInput()) return error.TypeError;
    if (s.closed) return error.StreamClosed;

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
        .file, .stdin => {
            var line = std.ArrayList(u8){};
            defer line.deinit(heap.backing_allocator);
            const fd: std.posix.fd_t = if (s.stream_type == .stdin)
                std.posix.STDIN_FILENO
            else
                @intCast(s.file_fd);
            var read_any = false;
            if (takePushback(s)) |ch| {
                if (ch == '\n') return try heap.allocBaseString("");
                try line.append(heap.backing_allocator, ch);
                read_any = true;
            }
            while (true) {
                var ch: [1]u8 = undefined;
                const n = try std.posix.read(fd, &ch);
                if (n == 0) break;
                read_any = true;
                if (ch[0] == '\n') break;
                try line.append(heap.backing_allocator, ch[0]);
            }
            if (!read_any and line.items.len == 0) return Value.nil;
            return try heap.allocBaseString(line.items);
        },
        .concatenated, .echo, .synonym, .two_way => return try readLineViaChars(heap, stream),
        .byte => return error.NotImplemented,
        .broadcast, .stdout, .stderr => return error.TypeError,
    }
}

/// Write a string to stream
pub fn writeString(str: Value, stream: Value, start: ?Value, end: ?Value) !void {
    if (!str.isString()) return error.TypeError;
    const string = str.toPtr(objects.String);
    const bytes = string.bytes();

    var start_idx: usize = 0;
    if (start) |s| {
        if (!s.isFixnum()) return error.TypeError;
        if (std.math.cast(usize, s.toFixnum())) |idx| {
            start_idx = idx;
        } else {
            return error.InvalidArgument;
        }
    }

    var end_idx: usize = bytes.len;
    if (end) |e| {
        if (!e.isFixnum()) return error.TypeError;
        if (std.math.cast(usize, e.toFixnum())) |idx| {
            end_idx = idx;
        } else {
            return error.InvalidArgument;
        }
    }

    if (start_idx > end_idx or end_idx > bytes.len) return error.InvalidArgument;
    try writeBytesToStream(stream, bytes[start_idx..end_idx]);
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
    if (!s.isOutput()) return error.TypeError;

    switch (s.stream_type) {
        .string => {}, // No-op for string streams
        .file, .stdout, .stderr => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            try std.posix.fsync(fd);
        },
        .broadcast => {
            var list = s.source_value;
            while (list.isCons()) {
                const cons = list.toPtr(objects.Cons);
                try finishOutput(cons.car);
                list = cons.cdr;
            }
            if (!list.isNil()) return error.InvalidArgument;
        },
        .byte => return error.NotImplemented,
        .stdin, .concatenated => return error.TypeError,
        .two_way => {
            const pair = s.source_value.toPtr(objects.Cons);
            try finishOutput(pair.cdr);
        },
        .synonym => try finishOutput(try resolveSynonymTarget(s)),
        .echo => {
            const pair = s.source_value.toPtr(objects.Cons);
            try finishOutput(pair.cdr);
        },
    }
}

/// Flush output without waiting
pub fn forceOutput(stream: Value) !void {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isOutput()) return error.TypeError;

    switch (s.stream_type) {
        .string => {}, // No-op for string streams
        .file, .stdout, .stderr => {
            const fd: std.posix.fd_t = @intCast(s.file_fd);
            _ = fd; // Force flush already happens on write
        },
        .broadcast => {
            var list = s.source_value;
            while (list.isCons()) {
                const cons = list.toPtr(objects.Cons);
                try forceOutput(cons.car);
                list = cons.cdr;
            }
            if (!list.isNil()) return error.InvalidArgument;
        },
        .echo, .synonym, .two_way => try forceOutput(try streamOutputComponent(s)),
        .byte => return error.NotImplemented,
        .stdin, .concatenated => return error.TypeError,
    }
}

/// Discard buffered output
pub fn clearOutput(stream: Value) !void {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isOutput()) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            // Just reset length, keep buffer allocated
            s.length = 0;
        },
        .file, .stdout, .stderr => {}, // Can't clear OS buffer
        .broadcast => {
            var list = s.source_value;
            while (list.isCons()) {
                const cons = list.toPtr(objects.Cons);
                try clearOutput(cons.car);
                list = cons.cdr;
            }
            if (!list.isNil()) return error.InvalidArgument;
        },
        .echo, .synonym, .two_way => try clearOutput(try streamOutputComponent(s)),
        .byte => return error.NotImplemented,
        .stdin, .concatenated => return error.TypeError,
    }
}

/// Discard buffered input
pub fn clearInput(stream: Value) !void {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);
    if (!s.isInput()) return error.TypeError;
    s.pushback_char = 0xFF;

    switch (s.stream_type) {
        .string => {
            s.position = s.length;
        },
        .file, .stdin => {
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
        .concatenated => {
            if (try currentConcatenatedInput(s)) |input| {
                try clearInput(input);
            }
        },
        .echo, .synonym, .two_way => try clearInput(try streamInputComponent(s)),
        .byte => return error.NotImplemented,
        .broadcast, .stdout, .stderr => return error.TypeError,
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
    if (!s.isOutput()) return error.TypeError;

    switch (s.stream_type) {
        .string => {
            // Check last byte written
            if (s.length == 0) return Value.nil;
            const buf: [*]u8 = @ptrFromInt(s.data_ptr);
            if (buf[s.length - 1] == '\n') return Value.nil;
            try writeBytesToStream(stream, "\n");
            return Value.t;
        },
        .file, .stdout, .stderr => {
            try writeChar(Value.makeFixnum('\n'), stream);
            return Value.t;
        },
        .broadcast => return error.NotImplemented,
        .echo, .synonym, .two_way => return try freshLine(try streamOutputComponent(s)),
        .byte => return error.NotImplemented,
        .stdin, .concatenated => return error.TypeError,
    }
}

/// Get/set file position
pub fn filePosition(heap: *heap_mod.Heap, stream: Value, pos: ?Value) !Value {
    if (!stream.isStream()) return error.TypeError;
    const s = stream.toPtr(objects.Stream);

    if (pos == null) {
        // Get current position
        switch (s.stream_type) {
            .string => return Value.makeFixnum(@intCast(if (s.direction == .output) s.length else s.position)),
            .file, .stdin, .stdout, .stderr => {
                const fd: std.posix.fd_t = @intCast(s.file_fd);
                const cur = try std.posix.lseek_CUR_get(fd);
                const adj = if (s.isInput() and hasPushback(s) and cur > 0) cur - 1 else cur;
                return Value.makeFixnum(@intCast(adj));
            },
            else => return error.NotImplemented,
        }
    } else {
        // Set position
        const p = pos.?;
        const new_pos: i64 = switch (p.typeKind()) {
            .keyword => blk: {
                const kw_start = try heap.internKeyword("start");
                const kw_end = try heap.internKeyword("end");
                if (p.raw == kw_start.raw) {
                    break :blk 0;
                } else if (p.raw == kw_end.raw) {
                    break :blk -1;
                } else {
                    return error.InvalidArgument;
                }
            },
            .fixnum => p.toFixnum(),
            else => return error.TypeError,
        };

        switch (s.stream_type) {
            .string => {
                if (new_pos == -1) {
                    s.position = s.length;
                } else if (new_pos >= 0) {
                    const upos = if (std.math.cast(u64, new_pos)) |val| val else return error.InvalidArgument;
                    if (upos > s.length) return error.InvalidArgument;
                    s.position = upos;
                } else {
                    return error.InvalidArgument;
                }
                return Value.t;
            },
            .file, .stdin, .stdout, .stderr => {
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
pub fn openFile(heap: *heap_mod.Heap, filename: Value, direction: ?Value, if_exists: ?Value, if_does_not_exist: ?Value) !Value {
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
        return try heap.allocStream(.io, .file, fd);
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
    s.finalize();
}

fn valueIsKeywordNamed(heap: *heap_mod.Heap, val: Value, name: []const u8) !bool {
    if (!val.isKeyword()) return false;
    const kw = try heap.internKeyword(name);
    return val.eq(kw);
}

fn valueIsWildcard(heap: *heap_mod.Heap, val: Value) !bool {
    if (try valueIsKeywordNamed(heap, val, "wild")) return true;
    if (!val.isString()) return false;
    const s = val.toPtr(objects.String).bytes();
    return std.mem.indexOfAny(u8, s, "*?") != null;
}

fn wildcardMatchCaseInsensitive(text: []const u8, pattern: []const u8) bool {
    var ti: usize = 0;
    var pi: usize = 0;
    var star_pi: ?usize = null;
    var star_ti: usize = 0;

    while (ti < text.len) {
        if (pi < pattern.len and (pattern[pi] == '?' or std.ascii.toLower(pattern[pi]) == std.ascii.toLower(text[ti]))) {
            ti += 1;
            pi += 1;
            continue;
        }
        if (pi < pattern.len and pattern[pi] == '*') {
            star_pi = pi;
            pi += 1;
            star_ti = ti;
            continue;
        }
        if (star_pi) |sp| {
            pi = sp + 1;
            star_ti += 1;
            ti = star_ti;
            continue;
        }
        return false;
    }

    while (pi < pattern.len and pattern[pi] == '*') {
        pi += 1;
    }

    return pi == pattern.len;
}

fn appendPathComponent(buf: *std.ArrayList(u8), allocator: std.mem.Allocator, component: []const u8) !void {
    if (component.len == 0) return;
    if (buf.items.len == 0) {
        try buf.appendSlice(allocator, component);
        return;
    }
    if (buf.items[buf.items.len - 1] != '/') {
        try buf.append(allocator, '/');
    }
    try buf.appendSlice(allocator, component);
}

fn appendPathnameResult(heap: *heap_mod.Heap, full_path: []const u8, result: *Value) !void {
    const path_val = try heap.allocBaseString(full_path);
    const pn = try pathname_prim.parseNamestring(std.heap.page_allocator, heap, path_val);
    result.* = try heap.allocCons(pn, result.*);
}

fn scanDirectoryForPattern(
    heap: *heap_mod.Heap,
    dir_path: []const u8,
    name_wild: bool,
    name_pat: []const u8,
    type_wild: bool,
    type_pat: []const u8,
    result: *Value,
) !void {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        return switch (err) {
            error.FileNotFound, error.NotDir => {},
            else => err,
        };
    };
    defer dir.close();

    var iter = dir.iterate();
    var full_buf: [std.fs.max_path_bytes]u8 = undefined;
    while (try iter.next()) |entry| {
        switch (entry.kind) {
            .file, .sym_link => {},
            else => continue,
        }

        const stem = std.fs.path.stem(entry.name);
        const ext = std.fs.path.extension(entry.name);
        const ext_no_dot = if (ext.len > 0) ext[1..] else "";

        if (!name_wild and !wildcardMatchCaseInsensitive(stem, name_pat)) continue;
        if (!type_wild and !wildcardMatchCaseInsensitive(ext_no_dot, type_pat)) continue;

        const full_path = if (std.mem.eql(u8, dir_path, "."))
            entry.name
        else
            try std.fmt.bufPrint(&full_buf, "{s}/{s}", .{ dir_path, entry.name });

        try appendPathnameResult(heap, full_path, result);
    }
}

const DirPatternTag = enum {
    literal,
    single,
    recursive,
};

const DirPatternComponent = struct {
    tag: DirPatternTag,
    text: []const u8 = "",
};

fn joinDirPath(allocator: std.mem.Allocator, base: []const u8, component: []const u8) ![]u8 {
    if (std.mem.eql(u8, base, ".")) return try allocator.dupe(u8, component);
    if (std.mem.eql(u8, base, "/")) return try std.fmt.allocPrint(allocator, "/{s}", .{component});
    return try std.fmt.allocPrint(allocator, "{s}/{s}", .{ base, component });
}

fn searchDirectoryPattern(
    heap: *heap_mod.Heap,
    allocator: std.mem.Allocator,
    dir_path: []const u8,
    components: []const DirPatternComponent,
    index: usize,
    name_wild: bool,
    name_pat: []const u8,
    type_wild: bool,
    type_pat: []const u8,
    result: *Value,
) !void {
    if (index >= components.len) {
        try scanDirectoryForPattern(heap, dir_path, name_wild, name_pat, type_wild, type_pat, result);
        return;
    }

    const component = components[index];
    switch (component.tag) {
        .literal => {
            const next_path = try joinDirPath(allocator, dir_path, component.text);
            defer allocator.free(next_path);
            try searchDirectoryPattern(heap, allocator, next_path, components, index + 1, name_wild, name_pat, type_wild, type_pat, result);
        },
        .single => {
            var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
                return switch (err) {
                    error.FileNotFound, error.NotDir => {},
                    else => err,
                };
            };
            defer dir.close();

            var iter = dir.iterate();
            while (try iter.next()) |entry| {
                if (entry.kind != .directory) continue;
                if (!wildcardMatchCaseInsensitive(entry.name, component.text)) continue;
                const next_path = try joinDirPath(allocator, dir_path, entry.name);
                defer allocator.free(next_path);
                try searchDirectoryPattern(heap, allocator, next_path, components, index + 1, name_wild, name_pat, type_wild, type_pat, result);
            }
        },
        .recursive => {
            try searchDirectoryPattern(heap, allocator, dir_path, components, index + 1, name_wild, name_pat, type_wild, type_pat, result);

            var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
                return switch (err) {
                    error.FileNotFound, error.NotDir => {},
                    else => err,
                };
            };
            defer dir.close();

            var iter = dir.iterate();
            while (try iter.next()) |entry| {
                if (entry.kind != .directory) continue;
                const next_path = try joinDirPath(allocator, dir_path, entry.name);
                defer allocator.free(next_path);
                try searchDirectoryPattern(heap, allocator, next_path, components, index, name_wild, name_pat, type_wild, type_pat, result);
            }
        },
    }
}

/// List files in a directory matching pathname
pub fn listDirectory(heap: *heap_mod.Heap, pathname: Value) !Value {
    var result = Value.nil;
    const allocator = std.heap.page_allocator;

    var base_dir_buf = std.ArrayList(u8){};
    defer base_dir_buf.deinit(allocator);

    var dir_pattern_components = std.ArrayList(DirPatternComponent){};
    defer dir_pattern_components.deinit(allocator);

    var name_wild = true;
    var type_wild = true;
    var name_pat: []const u8 = "";
    var type_pat: []const u8 = "";
    var dir_has_wild = false;

    switch (pathname.typeKind()) {
        .string => {
            const path_str = pathname.toPtr(objects.String).bytes();
            var dir_path = path_str;
            if (std.mem.endsWith(u8, dir_path, "*.*") or std.mem.endsWith(u8, dir_path, "*")) {
                if (std.mem.lastIndexOf(u8, dir_path, "/")) |idx| {
                    dir_path = dir_path[0..idx];
                } else {
                    dir_path = ".";
                }
            }
            try scanDirectoryForPattern(heap, dir_path, true, "", true, "", &result);
            return result;
        },
        .pathname => {
            const pn = pathname.toPtr(objects.Pathname);

            if (try valueIsWildcard(heap, pn.name)) {
                name_wild = true;
            } else if (pn.name.isString()) {
                name_wild = false;
                name_pat = pn.name.toPtr(objects.String).bytes();
            }

            if (try valueIsWildcard(heap, pn.type)) {
                type_wild = true;
            } else if (pn.type.isString()) {
                type_wild = false;
                type_pat = pn.type.toPtr(objects.String).bytes();
            }

            if (pn.directory.isCons()) {
                var dir = pn.directory;

                if (dir.isCons() and dir.toPtr(objects.Cons).car.isKeyword()) {
                    const tag = dir.toPtr(objects.Cons).car;
                    if (try valueIsKeywordNamed(heap, tag, "absolute")) {
                        try base_dir_buf.append(allocator, '/');
                    }
                    dir = dir.toPtr(objects.Cons).cdr;
                }

                var saw_pattern = false;
                while (dir.isCons()) {
                    const cons = dir.toPtr(objects.Cons);
                    const elem = cons.car;

                    if (try valueIsKeywordNamed(heap, elem, "wild-inferiors")) {
                        try dir_pattern_components.append(allocator, .{ .tag = .recursive });
                        saw_pattern = true;
                        dir_has_wild = true;
                    } else if (try valueIsKeywordNamed(heap, elem, "wild")) {
                        try dir_pattern_components.append(allocator, .{ .tag = .single, .text = "*" });
                        saw_pattern = true;
                        dir_has_wild = true;
                    } else if (elem.isString()) {
                        const comp = elem.toPtr(objects.String).bytes();
                        const is_recursive = std.mem.eql(u8, comp, "**");
                        const is_wild = std.mem.indexOfAny(u8, comp, "*?") != null;

                        if (!saw_pattern and !is_wild) {
                            try appendPathComponent(&base_dir_buf, allocator, comp);
                        } else {
                            if (is_recursive) {
                                try dir_pattern_components.append(allocator, .{ .tag = .recursive });
                                dir_has_wild = true;
                            } else if (is_wild) {
                                try dir_pattern_components.append(allocator, .{ .tag = .single, .text = comp });
                                dir_has_wild = true;
                            } else {
                                try dir_pattern_components.append(allocator, .{ .tag = .literal, .text = comp });
                            }
                            saw_pattern = true;
                        }
                    }
                    dir = cons.cdr;
                }
            }
        },
        else => return error.TypeError,
    }

    const base_dir = if (base_dir_buf.items.len == 0) "." else base_dir_buf.items;
    if (!dir_has_wild) {
        try searchDirectoryPattern(heap, allocator, base_dir, dir_pattern_components.items, 0, name_wild, name_pat, type_wild, type_pat, &result);
        return result;
    }

    try searchDirectoryPattern(heap, allocator, base_dir, dir_pattern_components.items, 0, name_wild, name_pat, type_wild, type_pat, &result);
    return result;
}

/// Check if pathname matches wildcard pattern
pub fn pathnameMatchP(pathname: Value, wildcard: Value) !Value {
    // Get path strings
    const pn_str = switch (pathname.typeKind()) {
        .string => pathname.toPtr(objects.String).bytes(),
        .pathname => blk: {
            const pn = pathname.toPtr(objects.Pathname);
            if (pn.name.isString()) break :blk pn.name.toPtr(objects.String).bytes();
            break :blk "";
        },
        else => return error.TypeError,
    };

    const wild_str = switch (wildcard.typeKind()) {
        .string => wildcard.toPtr(objects.String).bytes(),
        .pathname => blk: {
            const wc = wildcard.toPtr(objects.Pathname);
            if (wc.name.isString()) break :blk wc.name.toPtr(objects.String).bytes();
            break :blk "*";
        },
        else => return error.TypeError,
    };

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

test "listDirectory missing dir returns nil" {
    const testing = std.testing;
    var heap = try heap_mod.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_path = try tmp.parent_dir.realpathAlloc(testing.allocator, &tmp.sub_path);
    defer testing.allocator.free(tmp_path);

    const missing_path = try std.fs.path.join(testing.allocator, &.{ tmp_path, "nope-nope" });
    defer testing.allocator.free(missing_path);

    const missing_val = try heap.allocBaseString(missing_path);
    const res = try listDirectory(&heap, missing_val);
    try testing.expect(res.isNil());
}

test "listDirectory pathname wildcard filters by type" {
    const testing = std.testing;
    var heap = try heap_mod.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.makeDir("sub");
    (try tmp.dir.createFile("keep.fasl", .{})).close();
    (try tmp.dir.createFile("drop.lsp", .{})).close();
    (try tmp.dir.createFile("sub/nested.fasl", .{})).close();

    const tmp_path = try tmp.parent_dir.realpathAlloc(testing.allocator, &tmp.sub_path);
    defer testing.allocator.free(tmp_path);

    const top_pat_s = try std.fmt.allocPrint(testing.allocator, "{s}/*.fasl", .{tmp_path});
    defer testing.allocator.free(top_pat_s);
    const top_pat_v = try heap.allocBaseString(top_pat_s);
    const top_pat = try pathname_prim.parseNamestring(testing.allocator, &heap, top_pat_v);
    const top_res = try listDirectory(&heap, top_pat);

    var top_count: usize = 0;
    var cur = top_res;
    while (cur.isCons()) {
        const cons = cur.toPtr(objects.Cons);
        const pn = cons.car.toPtr(objects.Pathname);
        try testing.expect(pn.type.isString());
        try testing.expect(std.ascii.eqlIgnoreCase("fasl", pn.type.toPtr(objects.String).bytes()));
        top_count += 1;
        cur = cons.cdr;
    }
    try testing.expectEqual(@as(usize, 1), top_count);

    const sub_pat_s = try std.fmt.allocPrint(testing.allocator, "{s}/*/*.fasl", .{tmp_path});
    defer testing.allocator.free(sub_pat_s);
    const sub_pat_v = try heap.allocBaseString(sub_pat_s);
    const sub_pat = try pathname_prim.parseNamestring(testing.allocator, &heap, sub_pat_v);
    const sub_res = try listDirectory(&heap, sub_pat);

    var sub_count: usize = 0;
    var saw_nested = false;
    var sub_cur = sub_res;
    while (sub_cur.isCons()) {
        const cons = sub_cur.toPtr(objects.Cons);
        const pn = cons.car.toPtr(objects.Pathname);
        try testing.expect(pn.name.isString());
        try testing.expect(pn.type.isString());
        if (std.ascii.eqlIgnoreCase("nested", pn.name.toPtr(objects.String).bytes())) {
            saw_nested = true;
        }
        try testing.expect(std.ascii.eqlIgnoreCase("fasl", pn.type.toPtr(objects.String).bytes()));
        sub_count += 1;
        sub_cur = cons.cdr;
    }
    try testing.expectEqual(@as(usize, 1), sub_count);
    try testing.expect(saw_nested);
}

test "listDirectory pathname wildcard supports recursive **" {
    const testing = std.testing;
    var heap = try heap_mod.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.makeDir("sub");
    try tmp.dir.makeDir("sub/deeper");
    (try tmp.dir.createFile("top.fasl", .{})).close();
    (try tmp.dir.createFile("sub/mid.fasl", .{})).close();
    (try tmp.dir.createFile("sub/deeper/deep.fasl", .{})).close();
    (try tmp.dir.createFile("sub/deeper/drop.lsp", .{})).close();

    const tmp_path = try tmp.parent_dir.realpathAlloc(testing.allocator, &tmp.sub_path);
    defer testing.allocator.free(tmp_path);

    const recursive_pat_s = try std.fmt.allocPrint(testing.allocator, "{s}/**/*.fasl", .{tmp_path});
    defer testing.allocator.free(recursive_pat_s);
    const recursive_pat_v = try heap.allocBaseString(recursive_pat_s);
    const recursive_pat = try pathname_prim.parseNamestring(testing.allocator, &heap, recursive_pat_v);
    const recursive_res = try listDirectory(&heap, recursive_pat);

    var count: usize = 0;
    var saw_top = false;
    var saw_mid = false;
    var saw_deep = false;
    var cur_recursive = recursive_res;
    while (cur_recursive.isCons()) {
        const cons = cur_recursive.toPtr(objects.Cons);
        const pn = cons.car.toPtr(objects.Pathname);
        try testing.expect(pn.name.isString());
        try testing.expect(pn.type.isString());
        const name = pn.name.toPtr(objects.String).bytes();
        if (std.ascii.eqlIgnoreCase("top", name)) saw_top = true;
        if (std.ascii.eqlIgnoreCase("mid", name)) saw_mid = true;
        if (std.ascii.eqlIgnoreCase("deep", name)) saw_deep = true;
        try testing.expect(std.ascii.eqlIgnoreCase("fasl", pn.type.toPtr(objects.String).bytes()));
        count += 1;
        cur_recursive = cons.cdr;
    }
    try testing.expectEqual(@as(usize, 3), count);
    try testing.expect(saw_top);
    try testing.expect(saw_mid);
    try testing.expect(saw_deep);
}

test "pathnameMatchP matches string and pathname" {
    const testing = std.testing;
    var heap = try heap_mod.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocBaseString("foo");
    const pn = try heap.allocPathname(Value.nil, Value.nil, Value.nil, name, Value.nil, Value.nil);

    const res1 = try pathnameMatchP(pn, name);
    try testing.expect(res1.isT());

    const star = try heap.allocBaseString("*");
    const res2 = try pathnameMatchP(pn, star);
    try testing.expect(res2.isT());

    const no = try heap.allocBaseString("bar");
    const res3 = try pathnameMatchP(pn, no);
    try testing.expect(res3.isNil());

    const wild_pn = try heap.allocPathname(Value.nil, Value.nil, Value.nil, name, Value.nil, Value.nil);
    const res4 = try pathnameMatchP(pn, wild_pn);
    try testing.expect(res4.isT());
}

test "readLine handles long file lines" {
    const testing = std.testing;
    var heap = try heap_mod.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const file_name = "long-line.txt";
    const line_len: usize = LINE_BUF + 10;
    const buf = try testing.allocator.alloc(u8, line_len + 1);
    defer testing.allocator.free(buf);
    @memset(buf[0..line_len], 'a');
    buf[line_len] = '\n';

    {
        const file = try tmp.dir.createFile(file_name, .{});
        defer file.close();
        try file.writeAll(buf);
    }

    const file = try tmp.dir.openFile(file_name, .{});
    const stream = try heap.allocFileInputStream(@intCast(file.handle));
    const line_val = try readLine(&heap, stream);
    try testing.expect(line_val.isString());
    const line = line_val.toPtr(objects.String);
    try testing.expectEqual(line_len, @as(usize, @intCast(line.length)));
}

test "openFile :io supports read and write" {
    const testing = std.testing;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("io.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("a");
    }

    const path = try tmp.dir.realpathAlloc(testing.allocator, "io.txt");
    defer testing.allocator.free(path);

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const name_val = try heap.allocBaseString(path);
    const kw_io = try heap.internKeyword("io");
    const stream = try openFile(&heap, name_val, kw_io, null, null);

    try writeChar(Value.makeFixnum('Z'), stream);
    _ = try filePosition(&heap, stream, Value.makeFixnum(0));
    const ch = try readChar(stream, null, null);
    try testing.expectEqual(@as(i64, 'Z'), ch.toFixnum());

    try closeStream(stream, null);
}

test "readLine honors unread-char for file streams" {
    const testing = std.testing;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("pushback.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("ab\nc");
    }

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const file = try tmp.dir.openFile("pushback.txt", .{});
    defer file.close();
    const dup_fd = try std.posix.dup(file.handle);
    const stream = try heap.allocFileInputStream(@intCast(dup_fd));

    const first = try readChar(stream, null, null);
    try unreadChar(Value.makeCharacter(@intCast(first.toFixnum())), stream);

    const line1 = try readLine(&heap, stream);
    try testing.expect(line1.isString());
    try testing.expect(std.mem.eql(u8, line1.toPtr(objects.String).bytes(), "ab"));

    const line2 = try readLine(&heap, stream);
    try testing.expect(line2.isString());
    try testing.expect(std.mem.eql(u8, line2.toPtr(objects.String).bytes(), "c"));
}

test "readLine returns empty string after newline pushback" {
    const testing = std.testing;

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("pushback-nl.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("\nX");
    }

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const file = try tmp.dir.openFile("pushback-nl.txt", .{});
    defer file.close();
    const dup_fd = try std.posix.dup(file.handle);
    const stream = try heap.allocFileInputStream(@intCast(dup_fd));

    const first = try readChar(stream, null, null);
    try unreadChar(Value.makeCharacter(@intCast(first.toFixnum())), stream);

    const line1 = try readLine(&heap, stream);
    try testing.expect(line1.isString());
    try testing.expect(line1.toPtr(objects.String).bytes().len == 0);

    const line2 = try readLine(&heap, stream);
    try testing.expect(line2.isString());
    try testing.expect(std.mem.eql(u8, line2.toPtr(objects.String).bytes(), "X"));
}

test "composite streams delegate input and echo operations" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const s1 = try makeStringInputStream(&heap, try heap.allocBaseString("a"), null, null);
    const s2 = try makeStringInputStream(&heap, try heap.allocBaseString("bc"), null, null);
    const cat_tail = try heap.allocCons(s2, Value.nil);
    const cat_list = try heap.allocCons(s1, cat_tail);
    const cat = try heap.allocConcatenatedStream(cat_list);

    const peek = try peekChar(null, cat);
    try testing.expect(peek.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), peek.toFixnum());

    const a = try readChar(cat, null, null);
    try testing.expect(a.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), a.toFixnum());
    try unreadChar(Value.makeCharacter('a'), cat);
    const a_again = try readChar(cat, null, null);
    try testing.expectEqual(@as(i64, 'a'), a_again.toFixnum());
    const b = try readChar(cat, null, null);
    const c = try readChar(cat, null, null);
    try testing.expectEqual(@as(i64, 'b'), b.toFixnum());
    try testing.expectEqual(@as(i64, 'c'), c.toFixnum());
    try testing.expect((try readChar(cat, null, null)).isNil());

    const echo_in = try makeStringInputStream(&heap, try heap.allocBaseString("xy\n"), null, null);
    const echo_out = try makeStringOutputStream(&heap);
    const echo = try heap.allocEchoStream(echo_in, echo_out);
    const echoed_line = try readLine(&heap, echo);
    try testing.expect(echoed_line.isString());
    try testing.expect(std.mem.eql(u8, echoed_line.toPtr(objects.String).bytes(), "xy"));
    const echoed = try getOutputStreamString(&heap, echo_out);
    try testing.expect(echoed.isString());
    try testing.expect(std.mem.eql(u8, echoed.toPtr(objects.String).bytes(), "xy\n"));

    const two_way_in = try makeStringInputStream(&heap, try heap.allocBaseString("q"), null, null);
    const two_way_out = try makeStringOutputStream(&heap);
    const two_way = try heap.allocTwoWayStream(two_way_in, two_way_out);
    const q = try readCharNoHang(two_way);
    try testing.expect(q.isCharacter());
    try testing.expectEqual(@as(u21, 'q'), q.toCharacter());
    try writeChar(Value.makeFixnum('!'), two_way);
    const out = try getOutputStreamString(&heap, two_way_out);
    try testing.expect(out.isString());
    try testing.expect(std.mem.eql(u8, out.toPtr(objects.String).bytes(), "!"));
}

test "synonym streams follow symbol value and retarget dynamically" {
    const testing = std.testing;
    const ResolverCtx = struct {
        map: std.AutoHashMap(u64, Value),
    };
    const resolver = struct {
        fn lookup(sym: Value, ctx: *anyopaque) anyerror!?Value {
            const r: *ResolverCtx = @ptrCast(@alignCast(ctx));
            return r.map.get(sym.raw);
        }
    }.lookup;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var ctx = ResolverCtx{ .map = std.AutoHashMap(u64, Value).init(testing.allocator) };
    defer ctx.map.deinit();
    const saved_resolver = runtime_mod.setSymbolValueResolver(&resolver, @ptrCast(&ctx));
    defer runtime_mod.restoreSymbolValueResolver(saved_resolver);

    const sym = try heap.intern("SYN-STREAM");
    const out1 = try makeStringOutputStream(&heap);
    const out2 = try makeStringOutputStream(&heap);
    try ctx.map.put(sym.raw, out1);

    const syn = try heap.allocSynonymStream(sym);
    try testing.expect(inputStreamP(syn) == false);
    try testing.expect(outputStreamP(syn));

    try writeChar(Value.makeFixnum('A'), syn);
    const first = try getOutputStreamString(&heap, out1);
    try testing.expect(first.isString());
    try testing.expect(std.mem.eql(u8, first.toPtr(objects.String).bytes(), "A"));

    try ctx.map.put(sym.raw, out2);
    try writeChar(Value.makeFixnum('B'), syn);
    const second = try getOutputStreamString(&heap, out2);
    try testing.expect(second.isString());
    try testing.expect(std.mem.eql(u8, second.toPtr(objects.String).bytes(), "B"));
}

test "synonym streams delegate input operations through symbol value" {
    const testing = std.testing;
    const ResolverCtx = struct {
        map: std.AutoHashMap(u64, Value),
    };
    const resolver = struct {
        fn lookup(sym: Value, ctx: *anyopaque) anyerror!?Value {
            const r: *ResolverCtx = @ptrCast(@alignCast(ctx));
            return r.map.get(sym.raw);
        }
    }.lookup;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var ctx = ResolverCtx{ .map = std.AutoHashMap(u64, Value).init(testing.allocator) };
    defer ctx.map.deinit();
    const saved_resolver = runtime_mod.setSymbolValueResolver(&resolver, @ptrCast(&ctx));
    defer runtime_mod.restoreSymbolValueResolver(saved_resolver);

    const sym = try heap.intern("SYN-IN");
    const in = try makeStringInputStream(&heap, try heap.allocBaseString("xy"), null, null);
    try ctx.map.put(sym.raw, in);

    const syn = try heap.allocSynonymStream(sym);
    try testing.expect(inputStreamP(syn));
    try testing.expect(outputStreamP(syn) == false);
    try testing.expectEqual(@as(i64, 'x'), (try peekChar(null, syn)).toFixnum());
    try testing.expectEqual(@as(i64, 'x'), (try readChar(syn, null, null)).toFixnum());
    try unreadChar(Value.makeCharacter('x'), syn);
    try testing.expectEqual(@as(i64, 'x'), (try readChar(syn, null, null)).toFixnum());
    try testing.expectEqual(@as(i64, 'y'), (try readChar(syn, null, null)).toFixnum());
    try testing.expect((try readChar(syn, null, null)).isNil());
}

test "makeStringInputStream respects string bounds" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("ABCDE");
    const stream = try makeStringInputStream(&heap, str, Value.makeFixnum(1), Value.makeFixnum(4));
    try testing.expectEqual(@as(i64, 'B'), (try readChar(stream, null, null)).toFixnum());
    try testing.expectEqual(@as(i64, 'C'), (try readChar(stream, null, null)).toFixnum());
    try testing.expectEqual(@as(i64, 'D'), (try readChar(stream, null, null)).toFixnum());
    try testing.expect((try readChar(stream, null, null)).isNil());
}

test "makeStringInputStream respects vector fill pointer and bounds" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try heap.allocVector(4, 4);
    const v = vec.toPtr(objects.Vector);
    v.setAdjustable(true);
    v.set(0, Value.makeCharacter('A'));
    v.set(1, Value.makeCharacter('B'));
    v.set(2, Value.makeCharacter('C'));
    v.set(3, Value.makeCharacter('D'));
    v.setFillPointer(3);

    const stream = try makeStringInputStream(&heap, vec, Value.makeFixnum(1), null);
    try testing.expectEqual(@as(i64, 'B'), (try readChar(stream, null, null)).toFixnum());
    try testing.expectEqual(@as(i64, 'C'), (try readChar(stream, null, null)).toFixnum());
    try testing.expect((try readChar(stream, null, null)).isNil());
}

test "makeStringInputStream rejects invalid bounds" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("ABC");
    try testing.expectError(error.InvalidArgument, makeStringInputStream(&heap, str, Value.makeFixnum(2), Value.makeFixnum(1)));
    try testing.expectError(error.InvalidArgument, makeStringInputStream(&heap, str, null, Value.makeFixnum(4)));
}

test "sysReadLine returns empty string after newline pushback" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    pushback_char = '\n';
    defer pushback_char = null;

    const line = try sysReadLine(&heap);
    try testing.expect(line.isString());
    try testing.expect(line.toPtr(objects.String).bytes().len == 0);
}

test "readByteMaybe handles EOF" {
    const testing = std.testing;

    var empty_buf: [0]u8 = .{};
    var empty_stream = std.io.fixedBufferStream(&empty_buf);
    const empty_reader = empty_stream.reader();
    try testing.expect((try readByteMaybe(empty_reader)) == null);

    var one_buf = [_]u8{'x'};
    var one_stream = std.io.fixedBufferStream(&one_buf);
    const one_reader = one_stream.reader();
    const b1 = try readByteMaybe(one_reader);
    try testing.expect(b1 != null and b1.? == 'x');
    const b2 = try readByteMaybe(one_reader);
    try testing.expect(b2 == null);
}

test "princValueTo reports invalid unicode" {
    const testing = std.testing;

    var buf = std.ArrayList(u8){};
    defer buf.deinit(testing.allocator);

    const w = buf.writer(testing.allocator);
    const val = Value.makeCharacter(0xD800);
    try testing.expectError(error.Utf8CannotEncodeSurrogateHalf, princValueTo(val, w.any(), 0, null, null));
}
