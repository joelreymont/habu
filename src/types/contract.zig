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
const types = @import("type.zig");
const Blame = @import("blame.zig").Blame;
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Cons = runtime.Cons;
const Vector = runtime.Vector;

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

    /// Struct contract: check vector with struct name at index 0
    structof: struct {
        name_sym: Value, // interned struct name symbol
        type_name: []const u8, // for error messages
    },

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

    /// Negation combinator
    not: *const Contract,
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
        .arrow => |a| {
            // Higher-order: wrap function to check args/return at call time
            // For now, just verify it's a closure and return it
            // Full proxy support would require runtime function wrapping
            if (!predicates.isClosure(value)) {
                return blame.raise("function", value);
            }
            // Store arrow contract info for later checking
            // (actual proxy implementation requires VM support)
            _ = a;
            return value;
        },
        .listof => |elem_ctc| {
            // Check each list element
            const val = Value{ .raw = value };

            // Empty list (nil) is valid for listof
            if (val.isNil()) {
                return value;
            }

            // Must be a cons
            if (!val.isCons()) {
                return blame.raise("list", value);
            }

            // Traverse and check each element
            var current = val;
            while (current.isCons()) {
                const cons = current.toPtr(Cons);
                _ = try check(cons.car.raw, elem_ctc, blame);
                current = cons.cdr;
            }

            // Must be proper list (ends with nil)
            if (!current.isNil()) {
                return blame.raise("proper list", value);
            }

            return value;
        },
        .vectorof => |elem_ctc| {
            // Check each vector element
            const val = Value{ .raw = value };

            if (!val.isVector()) {
                return blame.raise("vector", value);
            }

            const vec = val.toPtr(Vector);
            const items = vec.items();
            for (items) |elem| {
                _ = try check(elem.raw, elem_ctc, blame);
            }

            return value;
        },
        .structof => |s| {
            // Struct check: (and (vectorp v) (>= (length v) 1) (eq (vec-ref v 0) name))
            const val = Value{ .raw = value };

            if (!val.isVector()) {
                return blame.raise(s.type_name, value);
            }

            const vec = val.toPtr(Vector);
            const items = vec.items();
            if (items.len < 1) {
                return blame.raise(s.type_name, value);
            }

            // Check struct name symbol at index 0 using identity
            if (items[0].raw != s.name_sym.raw) {
                return blame.raise(s.type_name, value);
            }

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
        .not => |n| {
            if (check(value, n, blame)) |_| {
                return blame.raise("(not ...)", value);
            } else |_| {
                return value;
            }
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

    /// Check if value is a float (bit63=0, bit62=1)
    pub fn isFloat(value: u64) bool {
        return ((value >> 62) & 3) == 1;
    }

    /// Check if value is a character (bit63=1)
    pub fn isCharacter(value: u64) bool {
        return (value >> 63) == 1;
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

    /// Check if value is a boxed type (tag 12)
    fn isBoxed(value: u64) bool {
        return value != 0 and (value & 0b1111) == 12;
    }

    /// Check if value is a hashtable (boxed, kind 0)
    pub fn isHashTable(value: u64) bool {
        if (!isBoxed(value)) return false;
        const ptr: [*]const u64 = @ptrFromInt(value & ~@as(u64, 0xF));
        return ptr[0] == 0; // BoxedKind.hashtable
    }

    /// Check if value is a rational (boxed, kind 1)
    pub fn isRational(value: u64) bool {
        if (!isBoxed(value)) return false;
        const ptr: [*]const u64 = @ptrFromInt(value & ~@as(u64, 0xF));
        return ptr[0] == 1; // BoxedKind.rational
    }

    /// Check if value is a complex (boxed, kind 2)
    pub fn isComplex(value: u64) bool {
        if (!isBoxed(value)) return false;
        const ptr: [*]const u64 = @ptrFromInt(value & ~@as(u64, 0xF));
        return ptr[0] == 2; // BoxedKind.complex
    }

    /// Check if value is a stream (boxed, kind 3)
    pub fn isStream(value: u64) bool {
        if (!isBoxed(value)) return false;
        const ptr: [*]const u64 = @ptrFromInt(value & ~@as(u64, 0xF));
        return ptr[0] == 3; // BoxedKind.stream
    }

    /// Check if value is a bignum (boxed, kind 4)
    pub fn isBignum(value: u64) bool {
        if (!isBoxed(value)) return false;
        const ptr: [*]const u64 = @ptrFromInt(value & ~@as(u64, 0xF));
        return ptr[0] == 4; // BoxedKind.bignum
    }

    /// Check if value is an array (boxed, kind 5)
    pub fn isArray(value: u64) bool {
        if (!isBoxed(value)) return false;
        const ptr: [*]const u64 = @ptrFromInt(value & ~@as(u64, 0xF));
        return ptr[0] == 5; // BoxedKind.array
    }

    /// Check if value is a pathname (boxed, kind 6)
    pub fn isPathname(value: u64) bool {
        if (!isBoxed(value)) return false;
        const ptr: [*]const u64 = @ptrFromInt(value & ~@as(u64, 0xF));
        return ptr[0] == 6; // BoxedKind.pathname
    }
};

/// Compile a type to a contract
/// Contracts are created at compile time and checked at runtime
/// Uses an arena allocator - all contracts freed when compiler is deinited
pub const ContractCompiler = struct {
    arena: std.heap.ArenaAllocator,
    interner: ?runtime.Interner,

    pub fn init(allocator: std.mem.Allocator) ContractCompiler {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .interner = null,
        };
    }

    pub fn initWithInterner(allocator: std.mem.Allocator, interner: runtime.Interner) ContractCompiler {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .interner = interner,
        };
    }

    pub fn deinit(self: *ContractCompiler) void {
        self.arena.deinit();
    }

    /// Compile a type to its corresponding contract
    pub fn compile(self: *ContractCompiler, ty: *const types.Type) !*Contract {
        const alloc = self.arena.allocator();
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
            .@"or" => |or_types| {
                // Build chain of or combinators: (or A (or B C))
                var result = try self.compile(or_types[or_types.len - 1]);
                var i: usize = or_types.len - 1;
                while (i > 0) {
                    i -= 1;
                    const left = try self.compile(or_types[i]);
                    const new_ctc = try alloc.create(Contract);
                    new_ctc.* = .{ .@"or" = .{ .left = left, .right = result } };
                    result = new_ctc;
                }
                return result;
            },
            .@"and" => |and_types| {
                // Build chain of and combinators: (and A (and B C))
                var result = try self.compile(and_types[and_types.len - 1]);
                var i: usize = and_types.len - 1;
                while (i > 0) {
                    i -= 1;
                    const left = try self.compile(and_types[i]);
                    const new_ctc = try alloc.create(Contract);
                    new_ctc.* = .{ .@"and" = .{ .left = left, .right = result } };
                    result = new_ctc;
                }
                return result;
            },
            .not => |inner_ty| {
                // Negation: compile inner contract
                const inner_ctc = try self.compile(inner_ty);
                ctc.* = .{ .not = inner_ctc };
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
            .@"struct" => |s| {
                // Struct contract: check vector with struct name symbol at index 0
                if (self.interner) |interner| {
                    // Use symbol identity - intern the struct name
                    const name_sym = try interner.intern(s.name);
                    ctc.* = .{
                        .structof = .{
                            .name_sym = name_sym,
                            .type_name = s.name,
                        },
                    };
                } else {
                    // Fallback without interner: just check vectorp (less precise)
                    ctc.* = .{
                        .flat = .{
                            .predicate = predicates.isVector,
                            .type_name = s.name,
                        },
                    };
                }
            },
            // Dependent types - require runtime evaluation for full support
            // For now, fall back to base type checks where possible
            .pi => {
                // Pi types are function types - check for closure
                ctc.* = .{
                    .flat = .{
                        .predicate = predicates.isClosure,
                        .type_name = "(pi ...)",
                    },
                };
            },
            .sigma => {
                // Sigma types are pair types - could be cons or custom structure
                // For now, accept any value (gradual typing escape)
                ctc.* = .{
                    .flat = .{
                        .predicate = predicates.isAny,
                        .type_name = "(sigma ...)",
                    },
                };
            },
            .refinement => |r| {
                // Refinement type: check base type, predicate checked at runtime
                // Full refinement checking requires SMT solver (Phase 3)
                return try self.compile(r.base_type);
            },
            .type_var => {
                // Unresolved type variable - accept any value
                ctc.* = .{
                    .flat = .{
                        .predicate = predicates.isAny,
                        .type_name = "type-var",
                    },
                };
            },
            .type_app => |ta| {
                // Type application - try to evaluate, fall back to func type
                // Full support requires normalizer (Phase 1)
                return try self.compile(ta.func);
            },
            .type_level => {
                // Universe types - should only appear in type-level code
                // At runtime, represents the Type type itself
                ctc.* = .{
                    .flat = .{
                        .predicate = predicates.isAny,
                        .type_name = "Type",
                    },
                };
            },
        }
        return ctc;
    }

    fn predicateForPrimitive(p: types.Primitive) *const fn (u64) bool {
        return switch (p) {
            .fixnum => predicates.isFixnum,
            .float => predicates.isFloat,
            .cons => predicates.isCons,
            .symbol => predicates.isSymbol,
            .vector => predicates.isVector,
            .string => predicates.isString,
            .closure => predicates.isClosure,
            .keyword => predicates.isKeyword,
            .nil => predicates.isNil,
            .char => predicates.isCharacter,
            .hashtable => predicates.isHashTable,
            .rational => predicates.isRational,
            .complex => predicates.isComplex,
            .stream => predicates.isStream,
            .bignum => predicates.isBignum,
            .array => predicates.isArray,
            .pathname => predicates.isPathname,
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

    var compiler = ContractCompiler.init(testing.allocator);
    defer compiler.deinit();

    // Compile fixnum type
    const fixnum_ctc = try compiler.compile(&types.t_fixnum);
    try testing.expect(fixnum_ctc.* == .flat);
    try testing.expectEqualStrings("fixnum", fixnum_ctc.flat.type_name);

    // Compile list type
    const list_ctc = try compiler.compile(&types.t_list_any);
    try testing.expect(list_ctc.* == .listof);
}

test "contract check" {
    const testing = std.testing;

    var compiler = ContractCompiler.init(testing.allocator);
    defer compiler.deinit();
    const fixnum_ctc = try compiler.compile(&types.t_fixnum);

    const blame = Blame{
        .positive = .{ .name = "test" },
        .negative = .{ .name = "test" },
    };

    // Check passes for fixnum
    const result = try check(0b11, fixnum_ctc, blame); // tagged fixnum 1
    try testing.expectEqual(@as(u64, 0b11), result);

    // Check fails for non-fixnum
    const err_result = check(0, fixnum_ctc, blame); // nil
    try testing.expectError(error.ContractViolation, err_result);
}

test "listof contract check" {
    const testing = std.testing;
    const Heap = @import("../runtime/runtime.zig").Heap;

    var compiler = ContractCompiler.init(testing.allocator);
    defer compiler.deinit();

    // Create (listof any) contract
    const listof_any = try compiler.compile(&types.t_list_any);

    // Create a heap for allocating cons cells
    var heap = Heap.init(testing.allocator, .{ .total_size = 1024 * 64 }) catch unreachable;
    defer heap.deinit();

    // Test: empty list (nil) passes
    const blame = Blame{
        .positive = .{ .name = "test" },
        .negative = .{ .name = "test" },
    };

    // Nil is valid for listof
    const nil_result = check(Value.nil.raw, listof_any, blame);
    try testing.expectEqual(Value.nil.raw, nil_result catch unreachable);

    // Build a list: (1 2 3)
    const fixnum3 = Value.makeFixnum(3);
    const pair3 = heap.allocCons(fixnum3, Value.nil) catch unreachable;
    const fixnum2 = Value.makeFixnum(2);
    const pair2 = heap.allocCons(fixnum2, pair3) catch unreachable;
    const fixnum1 = Value.makeFixnum(1);
    const pair1 = heap.allocCons(fixnum1, pair2) catch unreachable;

    // For now just test that list traversal doesn't crash
    // (since t_list_any uses isAny which always passes)
    const list_result = check(pair1.raw, listof_any, blame);
    try testing.expectEqual(pair1.raw, list_result catch unreachable);
}
