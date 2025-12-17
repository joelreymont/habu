//! Contract ADT for runtime type checking at boundaries
//!
//! Racket-style contracts:
//! - Flat: immediate predicate check
//! - Arrow: higher-order function contracts (deferred)
//! - Structural: listof, vectorof, struct

const std = @import("std");
const Type = @import("type.zig").Type;
const Blame = @import("blame.zig").Blame;

/// Contract representation
pub const Contract = union(enum) {
    /// Flat contract: immediate predicate check
    flat: struct {
        predicate: *const fn (value: u64) bool,
        type_name: []const u8,
    },

    /// Arrow contract: function with domain/range contracts
    arrow: struct {
        domain: []const *const Contract,
        range: *const Contract,
    },

    /// List contract: check each element
    listof: *const Contract,

    /// Vector contract: check each element
    vectorof: *const Contract,

    /// And combinator
    @"and": struct {
        left: *const Contract,
        right: *const Contract,
    },

    /// Or combinator
    @"or": struct {
        left: *const Contract,
        right: *const Contract,
    },
};

/// Check a value against a contract
pub fn check(value: u64, contract: *const Contract, blame: Blame) !u64 {
    switch (contract.*) {
        .flat => |f| {
            if (f.predicate(value)) {
                return value;
            }
            return blame.raise(f.type_name, value);
        },
        .arrow => {
            // Higher-order: return wrapped function (proxy)
            // TODO: implement function proxies
            return value;
        },
        .listof => |elem_ctc| {
            // Check each list element
            _ = elem_ctc;
            // TODO: traverse list and check elements
            return value;
        },
        .vectorof => |elem_ctc| {
            // Check each vector element
            _ = elem_ctc;
            // TODO: traverse vector and check elements
            return value;
        },
        .@"and" => |a| {
            const v1 = try check(value, a.left, blame);
            return try check(v1, a.right, blame);
        },
        .@"or" => |o| {
            return check(value, o.left, blame) catch
                check(value, o.right, blame);
        },
    }
}

test "flat contract" {
    // TODO: add tests once Value type is defined
}
