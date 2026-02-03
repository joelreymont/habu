//! Root set types for precise GC enumeration.

const value = @import("value.zig");

pub const Value = value.Value;

pub const RootRange = struct {
    ptr: [*]Value,
    len: usize,
};

pub const RootSet = struct {
    ranges: []const RootRange,
    slots: []const *Value,

    pub fn empty() RootSet {
        return .{ .ranges = &[_]RootRange{}, .slots = &[_]*Value{} };
    }
};

