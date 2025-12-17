//! Blame tracking for contract violations
//!
//! Racket-style blame:
//! - Positive: module that provided the value
//! - Negative: module that used the value
//! - When contract fails, blame identifies responsible party

const std = @import("std");

/// Source location for error messages
pub const SourceLoc = struct {
    file: []const u8,
    line: u32,
    column: u32,

    pub fn format(
        self: SourceLoc,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("{s}:{d}:{d}", .{ self.file, self.line, self.column });
    }
};

/// Blame label identifying a module boundary
pub const BlameLabel = struct {
    name: []const u8,
    location: ?SourceLoc = null,
};

/// Blame information for contract checking
pub const Blame = struct {
    /// Module that provided the value
    positive: BlameLabel,
    /// Module that used the value
    negative: BlameLabel,
    /// Contract context (for nested contracts)
    context: ?[]const u8 = null,

    /// Raise a contract violation error
    pub fn raise(self: Blame, expected: []const u8, got: u64) error{ContractViolation} {
        _ = self;
        _ = expected;
        _ = got;
        return error.ContractViolation;
    }

    /// Format blame for error message
    pub fn format(
        self: Blame,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("provided by {s}, used by {s}", .{
            self.positive.name,
            self.negative.name,
        });
        if (self.context) |ctx| {
            try writer.print(" in {s}", .{ctx});
        }
    }

    /// Swap positive/negative (for higher-order contracts)
    pub fn swap(self: Blame) Blame {
        return .{
            .positive = self.negative,
            .negative = self.positive,
            .context = self.context,
        };
    }
};

/// Contract violation error with blame
pub const ContractError = struct {
    expected: []const u8,
    got_type: []const u8,
    got_value: u64,
    blame: Blame,

    pub fn format(
        self: ContractError,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print(
            "Contract violation: expected {s}, got {s}\n  Blame: {f}\n",
            .{ self.expected, self.got_type, self.blame },
        );
    }
};

test "blame swap" {
    const testing = std.testing;

    const blame = Blame{
        .positive = .{ .name = "provider" },
        .negative = .{ .name = "consumer" },
    };

    const swapped = blame.swap();
    try testing.expectEqualStrings("consumer", swapped.positive.name);
    try testing.expectEqualStrings("provider", swapped.negative.name);
}
