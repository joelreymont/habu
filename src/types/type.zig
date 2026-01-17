//! Type ADT for Habu's gradual type system with dependent types
//!
//! ## Simple Types
//! - Primitive: fixnum, cons, symbol, string, vector, closure, keyword, nil
//! - Compound: (or T1 T2), (-> args ret), (list T), any
//!
//! ## Dependent Types (QTT - Quantitative Type Theory)
//! - Pi: (pi (x : A) B) - dependent function, B may reference x
//! - Sigma: (sigma (x : A) B) - dependent pair, second type depends on first value
//! - Refinement: (refine T x P) - subtype of T where predicate P holds
//! - Type variables: reference to bound type parameter
//! - Type application: applying a term to a type-level function
//! - Universe levels: Type, Type1, Type2... for stratification
//!
//! ## Quantities (for proof erasure and linearity)
//! - zero: Erased at runtime (proofs, type indices)
//! - one: Linear - used exactly once (resources)
//! - many (ω): Unrestricted usage
//!
//! Tag values (1+3 bit hybrid scheme):
//!   bit0=1: fixnum (63-bit signed, value >> 1)
//!   bit0=0: pointer | tag in bits 1-3
//!     0: cons, 2: symbol, 4: vector, 6: string
//!     8: closure, 10: keyword, 14: forwarding (GC)
//!   Special: 0 = nil

const std = @import("std");

// ============================================================================
// Quantity (QTT usage annotations)
// ============================================================================

/// Quantity for Quantitative Type Theory (QTT)
/// Controls how many times a variable can be used
pub const Quantity = enum {
    /// Erased at runtime - for proofs and type indices
    /// Variables with zero quantity cannot appear in runtime terms
    zero,

    /// Linear - must be used exactly once
    /// For resource management (file handles, connections, etc.)
    one,

    /// Unrestricted (ω) - can be used any number of times
    /// Default for most values
    many,

    /// Multiply quantities: 0 * q = 0, 1 * q = q, ω * ω = ω
    pub fn mult(self: Quantity, other: Quantity) Quantity {
        return switch (self) {
            .zero => .zero,
            .one => other,
            .many => if (other == .zero) .zero else .many,
        };
    }

    /// Add quantities: 0 + q = q, 1 + 1 = ω, ω + q = ω
    pub fn add(self: Quantity, other: Quantity) Quantity {
        return switch (self) {
            .zero => other,
            .one => if (other == .zero) .one else .many,
            .many => .many,
        };
    }

    /// Check if quantity allows at least n uses
    pub fn allowsUses(self: Quantity, n: usize) bool {
        return switch (self) {
            .zero => n == 0,
            .one => n <= 1,
            .many => true,
        };
    }
};

// ============================================================================
// Primitive Types
// ============================================================================

/// Primitive types matching Habu's tagged value scheme
/// Note: enum values are NOT the runtime tags (see tag() method)
pub const Primitive = enum {
    fixnum, // bit0=1, value >> 1
    float, // bit63=0, bit62=1, f64>>2
    cons, // tag 0
    symbol, // tag 2
    vector, // tag 4
    string, // tag 6
    closure, // tag 8
    keyword, // tag 10
    nil, // value 0
    char, // bit63=1, bits 0-20 = codepoint
    // Boxed types (tag 12=boxed, discriminated by BoxedKind)
    hashtable, // boxed kind 0
    rational, // boxed kind 1 - exact fraction (numerator/denominator)
    complex, // boxed kind 2 - complex number (real+imag)
    stream, // boxed kind 3 - file/string stream
    bignum, // boxed kind 4 - arbitrary precision integer
    array, // boxed kind 5 - multi-dimensional array
    pathname, // boxed kind 6 - file path object

    /// Get the runtime tag value for pointer types
    /// Returns null for fixnum (bit0=1) and nil (special value 0)
    pub fn tag(self: Primitive) ?u4 {
        return switch (self) {
            .cons => 0,
            .symbol => 2,
            .vector => 4,
            .string => 6,
            .closure => 8,
            .keyword => 10,
            .hashtable, .rational, .complex, .stream, .bignum, .array, .pathname => 12, // boxed tag
            .fixnum, .float, .nil, .char => null, // Not pointer-tagged
        };
    }

    /// Check if this is a pointer type (vs fixnum/nil)
    pub fn isPointer(self: Primitive) bool {
        return switch (self) {
            .fixnum, .float, .nil, .char => false,
            else => true,
        };
    }

    /// Check if this is a boxed type (requires discriminator)
    pub fn isBoxed(self: Primitive) bool {
        return switch (self) {
            .hashtable, .rational, .complex, .stream, .bignum, .array, .pathname => true,
            else => false,
        };
    }

    pub fn name(self: Primitive) []const u8 {
        return @tagName(self);
    }
};

/// A field in a struct type
pub const StructField = struct {
    name: []const u8,
    type: *const Type,
};

/// Struct type definition
pub const StructType = struct {
    /// Struct name (e.g., "point", "person")
    name: []const u8,
    /// Field definitions with names and types
    fields: []const StructField,

    /// Get field type by name, returns null if not found
    pub fn getFieldType(self: StructType, field_name: []const u8) ?*const Type {
        // String comparison needed: field names are raw strings from struct type definitions
        for (self.fields) |field| {
            if (std.mem.eql(u8, field.name, field_name)) {
                return field.type;
            }
        }
        return null;
    }

    /// Get field index by name, returns null if not found
    pub fn getFieldIndex(self: StructType, field_name: []const u8) ?usize {
        // String comparison needed: field names are raw strings from struct type definitions
        for (self.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, field_name)) {
                return i;
            }
        }
        return null;
    }
};

/// Type representation
/// Supports both simple types and dependent types (QTT)
pub const Type = union(enum) {
    // ========================================================================
    // Simple Types (existing)
    // ========================================================================

    /// Primitive type
    primitive: Primitive,

    /// Union type: (or T1 T2 ...)
    @"or": []const *const Type,

    /// Intersection type: (and T1 T2 ...)
    @"and": []const *const Type,

    /// Negation type: (not T)
    not: *const Type,

    /// Function type: (-> (T1 T2 ...) R)
    /// Non-dependent: return type doesn't reference parameters
    arrow: struct {
        domain: []const *const Type,
        range: *const Type,
    },

    /// Homogeneous list: (list T)
    /// A list is nil or (cons T (list T))
    list: *const Type,

    /// Homogeneous vector: (vector T)
    vec: *const Type,

    /// Non-nil constraint: (non-nil T)
    /// Used to exclude nil from a type
    non_nil: *const Type,

    /// Dynamic type (escape hatch for gradual typing)
    /// Matches any value, no contract checking
    any,

    /// Struct type: named record with typed fields
    /// Created by defstruct, enables nominal typing
    @"struct": StructType,

    /// Satisfies type: (satisfies predicate-fn)
    /// Type of values for which predicate returns true
    /// Predicate is a runtime Value (closure)
    satisfies: *const anyopaque, // Points to Value (predicate function)

    /// Member type: (member obj1 obj2 ...)
    /// Type that matches exactly one of the given objects
    /// Objects are compared by identity (pointer equality)
    member: []const *const anyopaque, // Points to Values

    /// Eql type: (eql obj)
    /// Type that matches exactly this one object
    /// Object compared by equality (equal? semantics)
    type_eql: *const anyopaque, // Points to Value

    // ========================================================================
    // Dependent Types (QTT)
    // ========================================================================

    /// Pi type (dependent function): (pi (x : A) B)
    /// The return type B can reference the parameter x
    /// Syntax: (pi (x : nat) (vec a x)) - vector of length x
    /// With quantity: (pi (0 x : a) b) - erased parameter (proofs)
    pi: struct {
        /// Parameter name for binding in the return type
        param_name: []const u8,
        /// Type of the parameter
        param_type: *const Type,
        /// Return type (may reference param_name)
        body: *const Type,
        /// Usage quantity (0=erased, 1=linear, ω=unrestricted)
        quantity: Quantity,
    },

    /// Sigma type (dependent pair): (sigma (x : A) B)
    /// The second component's type depends on the first value
    /// Syntax: (sigma (n : nat) (vec a n)) - length-indexed vector pair
    sigma: struct {
        /// Name of the first component
        first_name: []const u8,
        /// Type of the first component
        first_type: *const Type,
        /// Type of the second component (may reference first_name)
        second_type: *const Type,
    },

    /// Refinement type: (refine T x P)
    /// Subtype of T where predicate P holds for value x
    /// Syntax: (refine int x (> x 0)) - positive integers
    /// Syntax: (refine (list a) xs (not (null xs))) - non-empty lists
    refinement: struct {
        /// Base type being refined
        base_type: *const Type,
        /// Variable name bound in predicate
        predicate_var: []const u8,
        /// Predicate expression (points to Term, stored as opaque)
        /// This is a boolean expression over predicate_var
        /// Optional: null when predicate hasn't been parsed yet
        predicate: ?*const anyopaque,
    },

    /// Type variable reference: refers to a bound type parameter
    /// Used in polymorphic types: (forall (a) (-> a a))
    type_var: []const u8,

    /// Type application: apply a term to a type-level function
    /// Used for indexed types: (vec a n) where n is a term
    type_app: struct {
        /// Type-level function being applied
        func: *const Type,
        /// Argument (term, stored as opaque pointer to Term)
        arg: *const anyopaque,
    },

    /// Universe level: Type, Type1, Type2, ...
    /// Type : Type1 : Type2 : ...
    /// Level 0 = Type (the type of small types)
    type_level: u8,

    /// Check if type matches any value (no checking needed)
    pub fn isAny(self: Type) bool {
        return self == .any;
    }

    /// Check if this is a struct type
    pub fn isStruct(self: Type) bool {
        return self == .@"struct";
    }

    /// Get struct type info if this is a struct
    pub fn getStruct(self: Type) ?StructType {
        return switch (self) {
            .@"struct" => |s| s,
            else => null,
        };
    }

    /// Check if this type could be nil
    pub fn couldBeNil(self: Type) bool {
        return switch (self) {
            .primitive => |p| p == .nil,
            .@"or" => |types| {
                for (types) |t| {
                    if (t.couldBeNil()) return true;
                }
                return false;
            },
            .@"and" => |types| {
                for (types) |t| {
                    if (!t.couldBeNil()) return false;
                }
                return true;
            },
            .not => |t| !t.couldBeNil(),
            .list => true, // Empty list is nil
            .non_nil => false,
            .any => true,
            .arrow, .vec, .@"struct", .satisfies, .member, .type_eql => false,
            // Dependent types
            .pi, .sigma => false, // Function/pair types are never nil
            .refinement => |r| r.base_type.couldBeNil(), // Depends on base type
            .type_var => true, // Unknown, assume could be nil
            .type_app => true, // Unknown until evaluated
            .type_level => false, // Universe types are never nil
        };
    }

    /// Check if type is definitely a cons (for occurrence typing)
    pub fn isCons(self: Type) bool {
        return switch (self) {
            .primitive => |p| p == .cons,
            else => false,
        };
    }

    /// Get human-readable type name
    pub fn name(self: Type) []const u8 {
        return switch (self) {
            .primitive => |p| p.name(),
            .@"or" => "(or ...)",
            .@"and" => "(and ...)",
            .not => "(not ...)",
            .arrow => "(-> ...)",
            .list => "(list ...)",
            .vec => "(vector ...)",
            .non_nil => "(non-nil ...)",
            .any => "any",
            .@"struct" => |s| s.name,
            .satisfies => "(satisfies ...)",
            .member => "(member ...)",
            .type_eql => "(eql ...)",
            // Dependent types
            .pi => "(pi ...)",
            .sigma => "(sigma ...)",
            .refinement => "(refine ...)",
            .type_var => |v| v,
            .type_app => "(type-app ...)",
            .type_level => |lvl| if (lvl == 0) "Type" else "Type+",
        };
    }

    /// Check if two types are structurally equal
    /// Note: For dependent types, this is alpha-equivalence (names don't matter)
    pub fn eql(self: Type, other: Type) bool {
        return switch (self) {
            .primitive => |p| switch (other) {
                .primitive => |op| p == op,
                else => false,
            },
            .any => other == .any,
            .@"struct" => |s| switch (other) {
                // String comparison needed: struct names are raw strings, not interned
                .@"struct" => |os| std.mem.eql(u8, s.name, os.name),
                else => false,
            },
            .arrow => |a| switch (other) {
                .arrow => |oa| {
                    if (a.domain.len != oa.domain.len) return false;
                    for (a.domain, oa.domain) |d1, d2| {
                        if (!d1.eql(d2.*)) return false;
                    }
                    return a.range.eql(oa.range.*);
                },
                else => false,
            },
            .list => |l| switch (other) {
                .list => |ol| l.eql(ol.*),
                else => false,
            },
            .vec => |v| switch (other) {
                .vec => |ov| v.eql(ov.*),
                else => false,
            },
            .non_nil => |n| switch (other) {
                .non_nil => |on| n.eql(on.*),
                else => false,
            },
            .@"or" => |types| switch (other) {
                .@"or" => |otypes| {
                    if (types.len != otypes.len) return false;
                    for (types, otypes) |t1, t2| {
                        if (!t1.eql(t2.*)) return false;
                    }
                    return true;
                },
                else => false,
            },
            .@"and" => |types| switch (other) {
                .@"and" => |otypes| {
                    if (types.len != otypes.len) return false;
                    for (types, otypes) |t1, t2| {
                        if (!t1.eql(t2.*)) return false;
                    }
                    return true;
                },
                else => false,
            },
            .not => |t| switch (other) {
                .not => |ot| t.eql(ot.*),
                else => false,
            },
            .satisfies => |p| switch (other) {
                .satisfies => |op| p == op, // Pointer equality for predicates
                else => false,
            },
            .member => |objs| switch (other) {
                .member => |oobjs| {
                    if (objs.len != oobjs.len) return false;
                    for (objs, oobjs) |o1, o2| {
                        if (o1 != o2) return false;
                    }
                    return true;
                },
                else => false,
            },
            .type_eql => |obj| switch (other) {
                .type_eql => |oobj| obj == oobj, // Pointer equality
                else => false,
            },
            // Dependent types
            .pi => |p| switch (other) {
                .pi => |op| {
                    // Alpha-equivalence: names can differ if structure matches
                    // Full check requires substitution; for now, structural check
                    return p.quantity == op.quantity and
                        p.param_type.eql(op.param_type.*) and
                        p.body.eql(op.body.*);
                },
                else => false,
            },
            .sigma => |s| switch (other) {
                .sigma => |os| {
                    return s.first_type.eql(os.first_type.*) and
                        s.second_type.eql(os.second_type.*);
                },
                else => false,
            },
            .refinement => |r| switch (other) {
                .refinement => |or_| {
                    // Note: Predicate comparison requires term equality
                    // String comparison needed: predicate var names are raw strings, not interned
                    return r.base_type.eql(or_.base_type.*) and
                        std.mem.eql(u8, r.predicate_var, or_.predicate_var) and
                        r.predicate == or_.predicate; // Pointer equality for now
                },
                else => false,
            },
            .type_var => |v| switch (other) {
                // String comparison needed: type variable names are raw strings
                .type_var => |ov| std.mem.eql(u8, v, ov),
                else => false,
            },
            .type_app => |ta| switch (other) {
                .type_app => |ota| {
                    return ta.func.eql(ota.func.*) and
                        ta.arg == ota.arg; // Pointer equality for now
                },
                else => false,
            },
            .type_level => |lvl| switch (other) {
                .type_level => |olvl| lvl == olvl,
                else => false,
            },
        };
    }

    /// Check if this is a dependent type (Pi, Sigma, or Refinement)
    pub fn isDependent(self: Type) bool {
        return switch (self) {
            .pi, .sigma, .refinement, .type_var, .type_app => true,
            else => false,
        };
    }

    /// Check if this is a Pi type
    pub fn isPi(self: Type) bool {
        return self == .pi;
    }

    /// Check if this is a Sigma type
    pub fn isSigma(self: Type) bool {
        return self == .sigma;
    }

    /// Check if this is a refinement type
    pub fn isRefinement(self: Type) bool {
        return self == .refinement;
    }

    /// Get the base type of a refinement, or self if not a refinement
    pub fn getBaseType(self: *const Type) *const Type {
        return switch (self.*) {
            .refinement => |r| r.base_type.getBaseType(),
            else => self,
        };
    }
};

// ============================================================================
// Common type constants (compile-time)
// ============================================================================

pub const t_fixnum = Type{ .primitive = .fixnum };
pub const t_float = Type{ .primitive = .float };
pub const t_cons = Type{ .primitive = .cons };
pub const t_symbol = Type{ .primitive = .symbol };
pub const t_string = Type{ .primitive = .string };
pub const t_vector = Type{ .primitive = .vector };
pub const t_closure = Type{ .primitive = .closure };
pub const t_keyword = Type{ .primitive = .keyword };
pub const t_nil = Type{ .primitive = .nil };
pub const t_char = Type{ .primitive = .char };
pub const t_hashtable = Type{ .primitive = .hashtable };
pub const t_rational = Type{ .primitive = .rational };
pub const t_complex = Type{ .primitive = .complex };
pub const t_stream = Type{ .primitive = .stream };
pub const t_bignum = Type{ .primitive = .bignum };
pub const t_array = Type{ .primitive = .array };
pub const t_pathname = Type{ .primitive = .pathname };
pub const t_any = Type{ .any = {} };

// Common compound types
pub const t_list_any = Type{ .list = &t_any };
/// Non-nil: any value that is not nil (used in else-branch of null? checks)
pub const t_non_nil = Type{ .non_nil = &t_any };

// Universe levels
/// Type : Type1 (the type of small types)
pub const t_type = Type{ .type_level = 0 };
/// Type1 : Type2
pub const t_type1 = Type{ .type_level = 1 };
/// Type2 : Type3
pub const t_type2 = Type{ .type_level = 2 };

// ============================================================================
// Type constructors (for runtime type building)
// ============================================================================

pub const TypeBuilder = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init(backing_allocator: std.mem.Allocator) TypeBuilder {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
        };
    }

    pub fn deinit(self: *TypeBuilder) void {
        self.arena.deinit();
    }

    fn allocator(self: *TypeBuilder) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Create (or T1 T2)
    pub fn makeOr(self: *TypeBuilder, types: []const *const Type) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const copy = try alloc.dupe(*const Type, types);
        t.* = .{ .@"or" = copy };
        return t;
    }

    /// Create (and T1 T2)
    pub fn makeAnd(self: *TypeBuilder, types: []const *const Type) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const copy = try alloc.dupe(*const Type, types);
        t.* = .{ .@"and" = copy };
        return t;
    }

    /// Create (not T)
    pub fn makeNot(self: *TypeBuilder, base: *const Type) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        t.* = .{ .not = base };
        return t;
    }

    /// Create (-> (domain...) range)
    pub fn makeArrow(self: *TypeBuilder, domain: []const *const Type, range: *const Type) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const dom_copy = try alloc.dupe(*const Type, domain);
        t.* = .{ .arrow = .{ .domain = dom_copy, .range = range } };
        return t;
    }

    /// Create (list T)
    pub fn makeList(self: *TypeBuilder, elem: *const Type) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        t.* = .{ .list = elem };
        return t;
    }

    /// Create (vector T)
    pub fn makeVec(self: *TypeBuilder, elem: *const Type) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        t.* = .{ .vec = elem };
        return t;
    }

    /// Create (non-nil T)
    pub fn makeNonNil(self: *TypeBuilder, inner: *const Type) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        t.* = .{ .non_nil = inner };
        return t;
    }

    /// Create a struct type
    pub fn makeStruct(self: *TypeBuilder, struct_name: []const u8, fields: []const StructField) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const name_copy = try alloc.dupe(u8, struct_name);
        const fields_copy = try alloc.alloc(StructField, fields.len);
        for (fields, 0..) |field, i| {
            fields_copy[i] = .{
                .name = try alloc.dupe(u8, field.name),
                .type = field.type,
            };
        }
        t.* = .{ .@"struct" = .{ .name = name_copy, .fields = fields_copy } };
        return t;
    }

    // ========================================================================
    // Dependent Type Constructors
    // ========================================================================

    /// Create a Pi type: (pi (x : A) B)
    /// The return type B can reference the parameter x
    pub fn makePi(
        self: *TypeBuilder,
        param_name: []const u8,
        param_type: *const Type,
        body: *const Type,
        quantity: Quantity,
    ) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const name_copy = try alloc.dupe(u8, param_name);
        t.* = .{ .pi = .{
            .param_name = name_copy,
            .param_type = param_type,
            .body = body,
            .quantity = quantity,
        } };
        return t;
    }

    /// Create a non-dependent function type using Pi
    /// Equivalent to (pi (_ : A) B) where B doesn't reference _
    pub fn makePiSimple(self: *TypeBuilder, param_type: *const Type, return_type: *const Type) !*Type {
        return self.makePi("_", param_type, return_type, .many);
    }

    /// Create a Sigma type: (sigma (x : A) B)
    /// The second component's type depends on the first value
    pub fn makeSigma(
        self: *TypeBuilder,
        first_name: []const u8,
        first_type: *const Type,
        second_type: *const Type,
    ) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const name_copy = try alloc.dupe(u8, first_name);
        t.* = .{ .sigma = .{
            .first_name = name_copy,
            .first_type = first_type,
            .second_type = second_type,
        } };
        return t;
    }

    /// Create a non-dependent pair type using Sigma
    /// Equivalent to (sigma (_ : A) B) where B doesn't reference _
    pub fn makePair(self: *TypeBuilder, first_type: *const Type, second_type: *const Type) !*Type {
        return self.makeSigma("_", first_type, second_type);
    }

    /// Create a refinement type: (refine T x P)
    /// Subtype of T where predicate P holds for value x
    pub fn makeRefinement(
        self: *TypeBuilder,
        base_type: *const Type,
        predicate_var: []const u8,
        predicate: ?*const anyopaque,
    ) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const var_copy = try alloc.dupe(u8, predicate_var);
        t.* = .{ .refinement = .{
            .base_type = base_type,
            .predicate_var = var_copy,
            .predicate = predicate,
        } };
        return t;
    }

    /// Create a type variable reference
    pub fn makeTypeVar(self: *TypeBuilder, name: []const u8) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const name_copy = try alloc.dupe(u8, name);
        t.* = .{ .type_var = name_copy };
        return t;
    }

    /// Create a type application: apply a term to a type-level function
    pub fn makeTypeApp(self: *TypeBuilder, func: *const Type, arg: *const anyopaque) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        t.* = .{ .type_app = .{ .func = func, .arg = arg } };
        return t;
    }

    /// Create a universe type at the given level
    pub fn makeTypeLevel(self: *TypeBuilder, level: u8) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        t.* = .{ .type_level = level };
        return t;
    }

    /// Create a member type: (member obj1 obj2 ...)
    pub fn makeMember(self: *TypeBuilder, objs: []const *const anyopaque) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        const copy = try alloc.dupe(*const anyopaque, objs);
        t.* = .{ .member = copy };
        return t;
    }

    /// Create an eql type: (eql obj)
    pub fn makeEql(self: *TypeBuilder, obj: *const anyopaque) !*Type {
        const alloc = self.allocator();
        const t = try alloc.create(Type);
        t.* = .{ .type_eql = obj };
        return t;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "type name" {
    const testing = std.testing;

    try testing.expectEqualStrings("fixnum", t_fixnum.name());
    try testing.expectEqualStrings("any", t_any.name());
    try testing.expectEqualStrings("(list ...)", t_list_any.name());
}

test "primitive tags" {
    const testing = std.testing;

    try testing.expectEqual(@as(?u4, 0), Primitive.cons.tag());
    try testing.expectEqual(@as(?u4, 2), Primitive.symbol.tag());
    try testing.expectEqual(@as(?u4, 6), Primitive.string.tag());
    try testing.expectEqual(@as(?u4, null), Primitive.fixnum.tag());
    try testing.expectEqual(@as(?u4, null), Primitive.nil.tag());
}

test "type properties" {
    const testing = std.testing;

    try testing.expect(t_nil.couldBeNil());
    try testing.expect(t_any.couldBeNil());
    try testing.expect(t_list_any.couldBeNil());
    try testing.expect(!t_cons.couldBeNil());
    try testing.expect(!t_fixnum.couldBeNil());

    try testing.expect(t_cons.isCons());
    try testing.expect(!t_nil.isCons());
}

test "quantity operations" {
    const testing = std.testing;

    // Multiplication: 0 * q = 0
    try testing.expectEqual(Quantity.zero, Quantity.zero.mult(.zero));
    try testing.expectEqual(Quantity.zero, Quantity.zero.mult(.one));
    try testing.expectEqual(Quantity.zero, Quantity.zero.mult(.many));

    // Multiplication: 1 * q = q
    try testing.expectEqual(Quantity.zero, Quantity.one.mult(.zero));
    try testing.expectEqual(Quantity.one, Quantity.one.mult(.one));
    try testing.expectEqual(Quantity.many, Quantity.one.mult(.many));

    // Multiplication: ω * q
    try testing.expectEqual(Quantity.zero, Quantity.many.mult(.zero));
    try testing.expectEqual(Quantity.many, Quantity.many.mult(.one));
    try testing.expectEqual(Quantity.many, Quantity.many.mult(.many));

    // Addition
    try testing.expectEqual(Quantity.zero, Quantity.zero.add(.zero));
    try testing.expectEqual(Quantity.one, Quantity.zero.add(.one));
    try testing.expectEqual(Quantity.many, Quantity.one.add(.one));
    try testing.expectEqual(Quantity.many, Quantity.many.add(.zero));

    // Usage allowance
    try testing.expect(Quantity.zero.allowsUses(0));
    try testing.expect(!Quantity.zero.allowsUses(1));
    try testing.expect(Quantity.one.allowsUses(0));
    try testing.expect(Quantity.one.allowsUses(1));
    try testing.expect(!Quantity.one.allowsUses(2));
    try testing.expect(Quantity.many.allowsUses(100));
}

test "dependent type names" {
    const testing = std.testing;

    try testing.expectEqualStrings("Type", t_type.name());
    try testing.expectEqualStrings("Type+", t_type1.name());
}

test "dependent type builder" {
    const testing = std.testing;

    var builder = TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create a simple Pi type: (pi (x : fixnum) fixnum)
    const pi_type = try builder.makePi("x", &t_fixnum, &t_fixnum, .many);
    try testing.expect(pi_type.isPi());
    try testing.expect(!pi_type.couldBeNil());
    try testing.expectEqualStrings("(pi ...)", pi_type.name());

    // Create a Sigma type: (sigma (n : fixnum) string)
    const sigma_type = try builder.makeSigma("n", &t_fixnum, &t_string);
    try testing.expect(sigma_type.isSigma());
    try testing.expect(!sigma_type.couldBeNil());

    // Create a type variable
    const type_var = try builder.makeTypeVar("a");
    try testing.expect(type_var.isDependent());
    try testing.expectEqualStrings("a", type_var.name());

    // Universe levels
    const type2 = try builder.makeTypeLevel(2);
    try testing.expect(type2.eql(t_type2));
}
