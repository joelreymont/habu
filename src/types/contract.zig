//! Contract ADT for runtime type checking at boundaries
//!
//! Racket-style contracts:
//! - Flat: immediate predicate check (fixnum?, cons?, etc.)
//! - Arrow: higher-order function contracts (wrap functions)
//! - Structural: listof, vectorof (check each element)
//!
//! Type-to-contract compilation:
//! - Primitive types → flat predicates
//! - (-> args ret) → arrow contract
//! - (list T) → listof contract
//! - (or T1 T2) → or combinator

const std = @import("std");
const Type = @import("type.zig").Type;
const Primitive = @import("type.zig").Primitive;
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

// ============================================================================
// Type-to-Contract Compilation
// ============================================================================

/// Runtime tag checking predicates
/// These check the 1+3 bit hybrid tagging scheme
pub const predicates = struct {
    /// Check if value is a fixnum (bit0 = 1)
    pub fn isFixnum(value: u64) bool {
        return (value & 1) == 1;
    }

    /// Check if value is nil (value = 0)
    pub fn isNil(value: u64) bool {
        return value == 0;
    }

    /// Check if value is a cons (bit0=0, bits 1-3 = 0, not nil)
    pub fn isCons(value: u64) bool {
        return value != 0 and (value & 0b1111) == 0;
    }

    /// Check if value is a symbol (bit0=0, bits 1-3 = 1)
    pub fn isSymbol(value: u64) bool {
        return (value & 0b1111) == 2;
    }

    /// Check if value is a vector (bit0=0, bits 1-3 = 2)
    pub fn isVector(value: u64) bool {
        return (value & 0b1111) == 4;
    }

    /// Check if value is a string (bit0=0, bits 1-3 = 3)
    pub fn isString(value: u64) bool {
        return (value & 0b1111) == 6;
    }

    /// Check if value is a closure (bit0=0, bits 1-3 = 4)
    pub fn isClosure(value: u64) bool {
        return (value & 0b1111) == 8;
    }

    /// Check if value is a keyword (bit0=0, bits 1-3 = 5)
    pub fn isKeyword(value: u64) bool {
        return (value & 0b1111) == 10;
    }

    /// Always true (for 'any' type)
    pub fn isAny(value: u64) bool {
        _ = value;
        return true;
    }

    /// Non-nil check
    pub fn isNonNil(value: u64) bool {
        return value != 0;
    }
};

/// Compile a type to a contract
/// Contracts are created at compile time and checked at runtime
/// Uses an arena allocator - all contracts freed when compiler is deinited
pub const ContractCompiler = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init(backing_allocator: std.mem.Allocator) ContractCompiler {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
        };
    }

    pub fn deinit(self: *ContractCompiler) void {
        self.arena.deinit();
    }

    fn allocator(self: *ContractCompiler) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Compile a type to its corresponding contract
    pub fn compile(self: *ContractCompiler, ty: *const Type) !*Contract {
        const alloc = self.allocator();
        const ctc = try alloc.create(Contract);

        switch (ty.*) {
            .primitive => |p| {
                ctc.* = .{
                    .flat = .{
                        .predicate = predicateForPrimitive(p),
                        .type_name = p.name(),
                    },
                };
            },
            .any => {
                ctc.* = .{
                    .flat = .{
                        .predicate = predicates.isAny,
                        .type_name = "any",
                    },
                };
            },
            .@"or" => |types| {
                // Build chain of or combinators: (or A (or B C))
                var result = try self.compile(types[types.len - 1]);
                var i: usize = types.len - 1;
                while (i > 0) {
                    i -= 1;
                    const left = try self.compile(types[i]);
                    const new_ctc = try alloc.create(Contract);
                    new_ctc.* = .{ .@"or" = .{ .left = left, .right = result } };
                    result = new_ctc;
                }
                return result;
            },
            .arrow => |a| {
                // Compile domain and range contracts
                const domain_ctcs = try alloc.alloc(*const Contract, a.domain.len);
                for (a.domain, 0..) |dom_ty, i| {
                    domain_ctcs[i] = try self.compile(dom_ty);
                }
                const range_ctc = try self.compile(a.range);

                ctc.* = .{
                    .arrow = .{
                        .domain = domain_ctcs,
                        .range = range_ctc,
                    },
                };
            },
            .list => |elem_ty| {
                const elem_ctc = try self.compile(elem_ty);
                ctc.* = .{ .listof = elem_ctc };
            },
            .vec => |elem_ty| {
                const elem_ctc = try self.compile(elem_ty);
                ctc.* = .{ .vectorof = elem_ctc };
            },
            .non_nil => |inner| {
                // (non-nil T) = (and non-nil? T)
                const inner_ctc = try self.compile(inner);
                const non_nil_ctc = try alloc.create(Contract);
                non_nil_ctc.* = .{
                    .flat = .{
                        .predicate = predicates.isNonNil,
                        .type_name = "non-nil",
                    },
                };
                ctc.* = .{
                    .@"and" = .{
                        .left = non_nil_ctc,
                        .right = inner_ctc,
                    },
                };
            },
        }
        return ctc;
    }

    fn predicateForPrimitive(p: Primitive) *const fn (u64) bool {
        return switch (p) {
            .fixnum => predicates.isFixnum,
            .cons => predicates.isCons,
            .symbol => predicates.isSymbol,
            .vector => predicates.isVector,
            .string => predicates.isString,
            .closure => predicates.isClosure,
            .keyword => predicates.isKeyword,
            .nil => predicates.isNil,
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "flat contract check" {
    const testing = std.testing;

    // Test fixnum predicate
    try testing.expect(predicates.isFixnum(5)); // 5 has bit0=1 in tagged form would be (5<<1)|1 = 11
    try testing.expect(predicates.isFixnum(0b11)); // 3 = fixnum 1
    try testing.expect(!predicates.isFixnum(0)); // nil
    try testing.expect(!predicates.isFixnum(0b10)); // symbol tag

    // Test nil predicate
    try testing.expect(predicates.isNil(0));
    try testing.expect(!predicates.isNil(1));

    // Test cons predicate (tag 0, but not nil)
    try testing.expect(!predicates.isCons(0)); // nil is not cons
    try testing.expect(predicates.isCons(0x1000)); // pointer with tag 0

    // Test symbol predicate (tag 2)
    try testing.expect(predicates.isSymbol(0x1002)); // pointer with tag 2
    try testing.expect(!predicates.isSymbol(0x1000)); // cons, not symbol
}

test "type to contract compilation" {
    const testing = std.testing;
    const t = @import("type.zig");

    var compiler = ContractCompiler.init(testing.allocator);
    defer compiler.deinit();

    // Compile fixnum type
    const fixnum_ctc = try compiler.compile(&t.t_fixnum);
    try testing.expect(fixnum_ctc.* == .flat);
    try testing.expectEqualStrings("fixnum", fixnum_ctc.flat.type_name);

    // Compile list type
    const list_ctc = try compiler.compile(&t.t_list_any);
    try testing.expect(list_ctc.* == .listof);
}

test "contract check" {
    const testing = std.testing;
    const t = @import("type.zig");

    var compiler = ContractCompiler.init(testing.allocator);
    defer compiler.deinit();
    const fixnum_ctc = try compiler.compile(&t.t_fixnum);

    const blame = Blame{
        .positive = .{ .name = "test" },
        .negative = .{ .name = "test" },
    };

    // Check passes for fixnum
    const result = check(0b11, fixnum_ctc, blame); // tagged fixnum 1
    try testing.expectEqual(@as(u64, 0b11), result catch unreachable);

    // Check fails for non-fixnum
    const err_result = check(0, fixnum_ctc, blame); // nil
    try testing.expectError(error.ContractViolation, err_result);
}
