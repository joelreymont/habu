//! Diagnostic formatting for nice error messages with source context
//!
//! Produces output like:
//! ```
//! error: unknown identifier 'foo'
//!   --> input.lisp:5:10
//!    |
//!  5 | (let ((x foo))
//!    |          ^^^
//! ```

const std = @import("std");

/// A span in source code (byte offsets)
pub const Span = struct {
    start: usize,
    end: usize,

    pub fn len(self: Span) usize {
        return self.end -| self.start;
    }
};

/// Severity level for diagnostics
pub const Severity = enum {
    @"error",
    warning,
    note,
    help,

    pub fn color(self: Severity) []const u8 {
        return switch (self) {
            .@"error" => "\x1b[1;31m", // bold red
            .warning => "\x1b[1;33m", // bold yellow
            .note => "\x1b[1;36m", // bold cyan
            .help => "\x1b[1;32m", // bold green
        };
    }

    pub fn name(self: Severity) []const u8 {
        return switch (self) {
            .@"error" => "error",
            .warning => "warning",
            .note => "note",
            .help => "help",
        };
    }
};

/// Source context for formatting errors with line/column info and source snippets
pub const SourceContext = struct {
    source: []const u8,
    file_path: []const u8,
    /// Cached line start positions for fast lookup
    line_starts: std.ArrayList(usize),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, file_path: []const u8) !SourceContext {
        var line_starts = std.ArrayList(usize){};
        try line_starts.append(allocator, 0);

        for (source, 0..) |c, i| {
            if (c == '\n') {
                try line_starts.append(allocator, i + 1);
            }
        }

        return .{
            .source = source,
            .file_path = file_path,
            .line_starts = line_starts,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SourceContext) void {
        self.line_starts.deinit(self.allocator);
    }

    /// Convert a byte position to (line, column), both 1-indexed
    pub fn posToLineCol(self: SourceContext, pos: usize) struct { line: usize, col: usize } {
        const items = self.line_starts.items;

        // Binary search for the line containing this position
        var low: usize = 0;
        var high: usize = items.len;

        while (low < high) {
            const mid = low + (high - low) / 2;
            if (items[mid] <= pos) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }

        const line = if (low > 0) low - 1 else 0;
        const col = pos - items[line] + 1;
        return .{ .line = line + 1, .col = col };
    }

    /// Get the content of a specific line (1-indexed)
    pub fn getLine(self: SourceContext, line: usize) []const u8 {
        if (line == 0 or line > self.line_starts.items.len) {
            return "";
        }
        const start = self.line_starts.items[line - 1];
        const end = if (line < self.line_starts.items.len)
            self.line_starts.items[line]
        else
            self.source.len;

        // Trim trailing newline
        var result = self.source[start..end];
        if (result.len > 0 and result[result.len - 1] == '\n') {
            result = result[0 .. result.len - 1];
        }
        return result;
    }
};

/// A diagnostic message with source location
pub const Diagnostic = struct {
    severity: Severity,
    message: []const u8,
    span: Span,
    /// Optional secondary label
    label: ?[]const u8 = null,

    /// Format the diagnostic with source context
    pub fn format(
        self: Diagnostic,
        ctx: SourceContext,
        writer: anytype,
        use_color: bool,
    ) !void {
        const reset = if (use_color) "\x1b[0m" else "";
        const bold = if (use_color) "\x1b[1m" else "";
        const blue = if (use_color) "\x1b[1;34m" else "";
        const sev_color = if (use_color) self.severity.color() else "";

        const pos = ctx.posToLineCol(self.span.start);
        const line_content = ctx.getLine(pos.line);
        const span_len = @max(self.span.len(), 1);

        // Header: error: message
        try writer.print("{s}{s}{s}: {s}{s}\n", .{
            sev_color,
            self.severity.name(),
            reset,
            bold,
            self.message,
        });
        try writer.print("{s}", .{reset});

        // Location: --> file:line:col
        try writer.print("  {s}-->{s} {s}:{d}:{d}\n", .{
            blue,
            reset,
            ctx.file_path,
            pos.line,
            pos.col,
        });

        // Separator
        try writer.print("   {s}|{s}\n", .{ blue, reset });

        // Source line with number
        try writer.print("{s}{d:>3} |{s} {s}\n", .{
            blue,
            pos.line,
            reset,
            line_content,
        });

        // Underline with carets
        try writer.print("   {s}|{s} ", .{ blue, reset });
        try writer.writeByteNTimes(' ', pos.col - 1);
        try writer.print("{s}", .{sev_color});
        try writer.writeByteNTimes('^', span_len);
        try writer.print("{s}", .{reset});

        // Optional label on the underline
        if (self.label) |lbl| {
            try writer.print(" {s}", .{lbl});
        }
        try writer.print("\n", .{});
    }
};

/// Builder for creating diagnostics
pub const DiagnosticBuilder = struct {
    allocator: std.mem.Allocator,
    diagnostics: std.ArrayList(Diagnostic),

    pub fn init(allocator: std.mem.Allocator) DiagnosticBuilder {
        return .{
            .allocator = allocator,
            .diagnostics = std.ArrayList(Diagnostic){},
        };
    }

    pub fn deinit(self: *DiagnosticBuilder) void {
        self.diagnostics.deinit(self.allocator);
    }

    pub fn err(self: *DiagnosticBuilder, message: []const u8, span: Span) !void {
        try self.diagnostics.append(self.allocator, .{
            .severity = .@"error",
            .message = message,
            .span = span,
        });
    }

    pub fn errWithLabel(self: *DiagnosticBuilder, message: []const u8, span: Span, label: []const u8) !void {
        try self.diagnostics.append(self.allocator, .{
            .severity = .@"error",
            .message = message,
            .span = span,
            .label = label,
        });
    }

    pub fn warn(self: *DiagnosticBuilder, message: []const u8, span: Span) !void {
        try self.diagnostics.append(self.allocator, .{
            .severity = .warning,
            .message = message,
            .span = span,
        });
    }

    pub fn hasErrors(self: DiagnosticBuilder) bool {
        for (self.diagnostics.items) |d| {
            if (d.severity == .@"error") return true;
        }
        return false;
    }

    pub fn render(self: DiagnosticBuilder, ctx: SourceContext, writer: anytype, use_color: bool) !void {
        for (self.diagnostics.items) |d| {
            try d.format(ctx, writer, use_color);
            try writer.print("\n", .{});
        }
    }
};

test "source context line lookup" {
    const testing = std.testing;
    var ctx = try SourceContext.init(testing.allocator, "line1\nline2\nline3\n", "test.lisp");
    defer ctx.deinit();

    const pos1 = ctx.posToLineCol(0);
    try testing.expectEqual(@as(usize, 1), pos1.line);
    try testing.expectEqual(@as(usize, 1), pos1.col);

    const pos2 = ctx.posToLineCol(6);
    try testing.expectEqual(@as(usize, 2), pos2.line);
    try testing.expectEqual(@as(usize, 1), pos2.col);

    const pos3 = ctx.posToLineCol(8);
    try testing.expectEqual(@as(usize, 2), pos3.line);
    try testing.expectEqual(@as(usize, 3), pos3.col);
}

test "get line content" {
    const testing = std.testing;
    var ctx = try SourceContext.init(testing.allocator, "line1\nline2\nline3\n", "test.lisp");
    defer ctx.deinit();

    try testing.expectEqualStrings("line1", ctx.getLine(1));
    try testing.expectEqualStrings("line2", ctx.getLine(2));
    try testing.expectEqualStrings("line3", ctx.getLine(3));
}

test "diagnostic formatting" {
    const testing = std.testing;
    var ctx = try SourceContext.init(
        testing.allocator,
        "(let ((x foo))\n  x)\n",
        "test.lisp",
    );
    defer ctx.deinit();

    const diag = Diagnostic{
        .severity = .@"error",
        .message = "unknown identifier 'foo'",
        .span = .{ .start = 9, .end = 12 },
        .label = "not found",
    };

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try diag.format(ctx, stream.writer(), false);

    const output = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, output, "error:") != null);
    try testing.expect(std.mem.indexOf(u8, output, "unknown identifier") != null);
    try testing.expect(std.mem.indexOf(u8, output, "test.lisp:1:10") != null);
    try testing.expect(std.mem.indexOf(u8, output, "^^^") != null);
}
