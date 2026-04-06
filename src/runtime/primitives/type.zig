const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const arith = @import("arith.zig");
const objects = @import("../objects.zig");

pub const TypeSymbols = struct {
    cons: Value,
    symbol: Value,
    null: Value,
    boolean: Value,
    integer: Value,
    fixnum: Value,
    bignum: Value,
    float: Value,
    real: Value,
    rational: Value,
    ratio: Value,
    number: Value,
    complex: Value,
    character: Value,
    string: Value,
    vector: Value,
    array: Value,
    list: Value,
    sequence: Value,
    function: Value,
    compiled_function: Value,
    closure: Value,
    keyword: Value,
    hash_table: Value,
    stream: Value,
    pathname: Value,
    package: Value,
    atom: Value,
    base_char: Value,
    standard_char: Value,
    extended_char: Value,
    base_string: Value,
    simple_string: Value,
    simple_base_string: Value,
    simple_vector: Value,
    simple_array: Value,
    bit_vector: Value,
    simple_bit_vector: Value,
    single_float: Value,
    double_float: Value,
    short_float: Value,
    long_float: Value,
    class: Value,
    standard_class: Value,
    built_in_class: Value,
    structure_class: Value,
    generic_function: Value,
    standard_generic_function: Value,
    method: Value,
    standard_method: Value,
    standard_object: Value,
    structure_object: Value,
    file_stream: Value,
    string_stream: Value,
    random_state: Value,
    restart: Value,
    method_combination: Value,
    values: Value,
    @"or": Value,
    @"and": Value,
    @"not": Value,
    satisfies: Value,
    star: Value,
    eql: Value,
    member: Value,
    mod: Value,
    signed_byte: Value,
    unsigned_byte: Value,
    t: Value,
    nil: Value,
    unbound: Value,

    pub fn init(heap: *Heap) !TypeSymbols {
        return .{
            .cons = try clSym(heap, "cons"),
            .symbol = try clSym(heap, "symbol"),
            .null = try clSym(heap, "null"),
            .boolean = try clSym(heap, "boolean"),
            .integer = try clSym(heap, "integer"),
            .fixnum = try clSym(heap, "fixnum"),
            .bignum = try clSym(heap, "bignum"),
            .float = try clSym(heap, "float"),
            .real = try clSym(heap, "real"),
            .rational = try clSym(heap, "rational"),
            .ratio = try clSym(heap, "ratio"),
            .number = try clSym(heap, "number"),
            .complex = try clSym(heap, "complex"),
            .character = try clSym(heap, "character"),
            .string = try clSym(heap, "string"),
            .vector = try clSym(heap, "vector"),
            .array = try clSym(heap, "array"),
            .list = try clSym(heap, "list"),
            .sequence = try clSym(heap, "sequence"),
            .function = try clSym(heap, "function"),
            .compiled_function = try clSym(heap, "compiled-function"),
            .closure = try clSym(heap, "closure"),
            .keyword = try clSym(heap, "keyword"),
            .hash_table = try clSym(heap, "hash-table"),
            .stream = try clSym(heap, "stream"),
            .pathname = try clSym(heap, "pathname"),
            .package = try clSym(heap, "package"),
            .atom = try clSym(heap, "atom"),
            .base_char = try clSym(heap, "base-char"),
            .standard_char = try clSym(heap, "standard-char"),
            .extended_char = try clSym(heap, "extended-char"),
            .base_string = try clSym(heap, "base-string"),
            .simple_string = try clSym(heap, "simple-string"),
            .simple_base_string = try clSym(heap, "simple-base-string"),
            .simple_vector = try clSym(heap, "simple-vector"),
            .simple_array = try clSym(heap, "simple-array"),
            .bit_vector = try clSym(heap, "bit-vector"),
            .simple_bit_vector = try clSym(heap, "simple-bit-vector"),
            .single_float = try clSym(heap, "single-float"),
            .double_float = try clSym(heap, "double-float"),
            .short_float = try clSym(heap, "short-float"),
            .long_float = try clSym(heap, "long-float"),
            .class = try clSym(heap, "class"),
            .standard_class = try clSym(heap, "standard-class"),
            .built_in_class = try clSym(heap, "built-in-class"),
            .structure_class = try clSym(heap, "structure-class"),
            .generic_function = try clSym(heap, "generic-function"),
            .standard_generic_function = try clSym(heap, "standard-generic-function"),
            .method = try clSym(heap, "method"),
            .standard_method = try clSym(heap, "standard-method"),
            .standard_object = try clSym(heap, "standard-object"),
            .structure_object = try clSym(heap, "structure-object"),
            .file_stream = try clSym(heap, "file-stream"),
            .string_stream = try clSym(heap, "string-stream"),
            .random_state = try clSym(heap, "random-state"),
            .restart = try clSym(heap, "restart"),
            .method_combination = try clSym(heap, "method-combination"),
            .values = try clSym(heap, "values"),
            .@"or" = try clSym(heap, "or"),
            .@"and" = try clSym(heap, "and"),
            .@"not" = try clSym(heap, "not"),
            .satisfies = try clSym(heap, "satisfies"),
            .star = try clSym(heap, "*"),
            .eql = try clSym(heap, "eql"),
            .member = try clSym(heap, "member"),
            .mod = try clSym(heap, "mod"),
            .signed_byte = try clSym(heap, "signed-byte"),
            .unsigned_byte = try clSym(heap, "unsigned-byte"),
            .t = try clSym(heap, "t"),
            .nil = try clSym(heap, "nil"),
            .unbound = try heap.intern("%unbound%"),
        };
    }
};

fn clSym(heap: *Heap, name: []const u8) !Value {
    return (try heap.internInPackage("COMMON-LISP", name)) orelse error.InvalidArgument;
}

fn isCharacterVector(obj: Value) bool {
    return obj.isVector() and obj.toPtr(objects.Vector).isCharacterVector();
}

/// Check if two values are eql (eq for most types, numeric for numbers)
fn valueEql(a: Value, b: Value) bool {
    if (a.raw == b.raw) return true;
    if (a.isFloat() and b.isFloat()) {
        const af = a.toFloat();
        const bf = b.toFloat();
        // NaN != NaN, but +0.0 == -0.0
        if (std.math.isNan(af) or std.math.isNan(bf)) return false;
        return af == bf;
    }
    return false;
}

const IntBound = union(enum) {
    unbounded,
    inclusive: Value,
    exclusive: Value,
};

fn parseIntBound(syms: *const TypeSymbols, spec: Value) !IntBound {
    return switch (spec.typeKind()) {
        .symbol => if (spec.eq(syms.star))
            .unbounded
        else
            error.InvalidTypeSpecifier,
        .fixnum, .bignum => .{ .inclusive = spec },
        .cons => {
            const cell = spec.toPtr(objects.Cons);
            if (!cell.cdr.isNil()) return error.InvalidTypeSpecifier;
            if (!cell.car.isFixnum() and !cell.car.isBignum()) return error.InvalidTypeSpecifier;
            return .{ .exclusive = cell.car };
        },
        else => error.InvalidTypeSpecifier,
    };
}

fn integerLowerOk(obj: Value, bound: IntBound) !bool {
    return switch (bound) {
        .unbounded => true,
        .inclusive => |lo| try arith.ge(obj, lo),
        .exclusive => |lo| try arith.gt(obj, lo),
    };
}

fn integerUpperOk(obj: Value, bound: IntBound) !bool {
    return switch (bound) {
        .unbounded => true,
        .inclusive => |hi| try arith.le(obj, hi),
        .exclusive => |hi| try arith.lt(obj, hi),
    };
}

pub fn typep(heap: *Heap, syms: *const TypeSymbols, obj: Value, type_spec: Value) !bool {
    if (type_spec.isT()) return true;
    if (type_spec.isNil()) return false;

    if (type_spec.isSymbol() or type_spec.isT() or type_spec.isNil()) {
        const sym = type_spec;

        if (sym.eq(syms.cons)) return obj.isCons();
        if (sym.eq(syms.symbol)) return obj.isSymbolLike();
        if (sym.eq(syms.null)) return obj.isNil();
        if (sym.eq(syms.boolean)) return obj.isNil() or obj.isT();
        if (sym.eq(syms.integer)) return obj.isFixnum() or obj.isBignum();
        if (sym.eq(syms.fixnum)) return obj.isFixnum();
        if (sym.eq(syms.bignum)) return obj.isBignum();
        if (sym.eq(syms.float)) return obj.isFloat();
        if (sym.eq(syms.real)) return obj.isFixnum() or obj.isBignum() or obj.isFloat() or obj.isRational();
        if (sym.eq(syms.rational)) return obj.isFixnum() or obj.isBignum() or obj.isRational();
        if (sym.eq(syms.ratio)) return obj.isRational();
        if (sym.eq(syms.number)) return obj.isFixnum() or obj.isBignum() or obj.isFloat() or obj.isRational() or obj.isComplex();
        if (sym.eq(syms.complex)) return obj.isComplex();
        if (sym.eq(syms.character)) return obj.isCharacter();
        if (sym.eq(syms.string)) return obj.isString() or isCharacterVector(obj);
        if (sym.eq(syms.vector)) return obj.isVector();
        if (sym.eq(syms.array)) return obj.isArray() or obj.isVector() or obj.isString() or isCharacterVector(obj);
        if (sym.eq(syms.list)) return obj.isNil() or obj.isCons();
        if (sym.eq(syms.sequence)) return obj.isNil() or obj.isCons() or obj.isVector() or obj.isString();
        if (sym.eq(syms.function)) return obj.isClosure() or obj.isChunk();
        if (sym.eq(syms.compiled_function)) return obj.isChunk() or obj.typeKind() == .native_code;
        if (sym.eq(syms.keyword)) return obj.isKeyword();
        if (sym.eq(syms.hash_table)) return obj.isHashTable();
        if (sym.eq(syms.stream)) return obj.isStream();
        if (sym.eq(syms.pathname)) return obj.isPathname();
        if (sym.eq(syms.package)) return obj.isPackage();
        if (sym.eq(syms.atom)) return !obj.isCons();
        if (sym.eq(syms.base_char)) return obj.isCharacter();
        if (sym.eq(syms.standard_char)) return obj.isCharacter();
        if (sym.eq(syms.extended_char)) return false; // no extended chars
        if (sym.eq(syms.base_string)) return obj.isString() or isCharacterVector(obj);
        if (sym.eq(syms.simple_string)) return obj.isString() or isCharacterVector(obj);
        if (sym.eq(syms.simple_base_string)) return obj.isString();
        if (sym.eq(syms.simple_vector)) return obj.isVector();
        if (sym.eq(syms.simple_array)) return obj.isVector() or obj.isString() or obj.isArray();
        if (sym.eq(syms.bit_vector)) return obj.isVector();
        if (sym.eq(syms.simple_bit_vector)) return obj.isVector();
        if (sym.eq(syms.single_float)) return obj.isFloat();
        if (sym.eq(syms.double_float)) return obj.isFloat();
        if (sym.eq(syms.short_float)) return obj.isFloat();
        if (sym.eq(syms.long_float)) return obj.isFloat();
        if (sym.eq(syms.class)) return obj.isClass();
        if (sym.eq(syms.standard_class)) return obj.isClass();
        if (sym.eq(syms.built_in_class)) return obj.isClass();
        if (sym.eq(syms.structure_class)) return obj.isClass();
        if (sym.eq(syms.generic_function)) return obj.isGenericFunction();
        if (sym.eq(syms.standard_generic_function)) return obj.isGenericFunction();
        if (sym.eq(syms.method)) return obj.isMethod();
        if (sym.eq(syms.standard_method)) return obj.isMethod();
        if (sym.eq(syms.standard_object)) return obj.isVector();
        if (sym.eq(syms.structure_object)) return obj.isStructure();
        if (sym.eq(syms.file_stream)) {
            if (!obj.isStream()) return false;
            const stream = obj.toPtr(@import("../objects.zig").Stream);
            return stream.stream_type != .string;
        }
        if (sym.eq(syms.string_stream)) {
            if (!obj.isStream()) return false;
            const stream = obj.toPtr(@import("../objects.zig").Stream);
            return stream.stream_type == .string;
        }
        // random-state is implemented as an integer in Habu
        if (sym.eq(syms.random_state)) return obj.isFixnum();
        if (sym.eq(syms.restart)) {
            if (!obj.isVector()) return false;
            const vec = obj.toPtr(objects.Vector);
            return vec.length > 0 and vec.data[0].eq(syms.restart);
        }
        // method-combination isn't implemented as separate type
        if (sym.eq(syms.method_combination)) return false;
        // values is a type specifier for multiple return values, not for typep
        if (sym.eq(syms.values)) return false;

        const maybe_class = heap.findLispClass(type_spec);

        // Check if it's a class name (instance type check)
        if (obj.isVector() or obj.isStructure()) {
            const class_val = blk: {
                if (obj.isStructure()) break :blk obj.toPtr(objects.Structure).class;
                const vec = obj.toPtr(objects.Vector);
                if (vec.length == 0 or !vec.data[0].isSymbol()) break :blk Value.nil;
                break :blk heap.findLispClass(vec.data[0]) orelse Value.nil;
            };
            if (!class_val.isNil()) {
                if (class_val.eq(maybe_class orelse Value.nil)) return true;
                const class = class_val.toPtr(@import("../objects.zig").Class);
                if (class.name.eq(type_spec)) return true;
                var cpl = class.cpl;
                while (cpl.isCons()) {
                    const cons = cpl.toPtr(@import("../objects.zig").Cons);
                    if (cons.car.eq(type_spec)) return true;
                    if (cons.car.isClass()) {
                        const cpl_class = cons.car.toPtr(@import("../objects.zig").Class);
                        if (cpl_class.name.eq(type_spec)) return true;
                    }
                    cpl = cons.cdr;
                }
                return false;
            }
            if (maybe_class != null) return false;
        }

        if (maybe_class) |class_val| {
            // Class objects can match by identity or by class-name in CPL.
            if (obj.isClass()) {
                if (obj.eq(class_val)) return true;
                var cpl = obj.toPtr(@import("../objects.zig").Class).cpl;
                while (cpl.isCons()) {
                    const cons = cpl.toPtr(@import("../objects.zig").Cons);
                    if (cons.car.eq(class_val) or cons.car.eq(type_spec)) return true;
                    if (cons.car.isClass()) {
                        const cpl_class = cons.car.toPtr(@import("../objects.zig").Class);
                        if (cpl_class.name.eq(type_spec)) return true;
                    }
                    cpl = cons.cdr;
                }
            }
            // Non-instance values are not of this class; return false instead of signaling unknown spec.
            return false;
        }

        return error.UnknownTypeSpecifier;
    }

    if (type_spec.isCons()) {
        const head = type_spec.toPtr(@import("../objects.zig").Cons).car;

        if (head.eq(syms.@"or")) {
            var rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                if (try typep(heap, syms, obj, spec)) return true;
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return false;
        }

        // (and t1 t2 ...) - intersection type
        if (head.eq(syms.@"and")) {
            var rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                if (!try typep(heap, syms, obj, spec)) return false;
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return true;
        }

        if (head.eq(syms.@"not")) {
            const inner = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (!inner.isCons()) return error.InvalidTypeSpecifier;
            return !try typep(heap, syms, obj, inner.toPtr(@import("../objects.zig").Cons).car);
        }

        if (head.eq(syms.satisfies)) {
            // (satisfies predicate-fn) - requires runtime evaluation
            // For now, conservatively return true (any value might satisfy)
            // Proper checking requires VM integration for predicate call
            return true;
        }

        // (cons [car-type [cdr-type]]) - proper cons type specifier
        if (head.eq(syms.cons)) {
            if (!obj.isCons()) return false;
            const obj_cons = obj.toPtr(@import("../objects.zig").Cons);
            const rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (rest.isNil()) return true;
            if (!rest.isCons()) return error.InvalidTypeSpecifier;

            const args1 = rest.toPtr(@import("../objects.zig").Cons);
            const car_spec = args1.car;
            const car_ok = try typep(heap, syms, obj_cons.car, car_spec);
            if (!car_ok) return false;

            if (args1.cdr.isNil()) return true;
            if (!args1.cdr.isCons()) return error.InvalidTypeSpecifier;
            const args2 = args1.cdr.toPtr(@import("../objects.zig").Cons);
            if (!args2.cdr.isNil()) return error.InvalidTypeSpecifier;
            return try typep(heap, syms, obj_cons.cdr, args2.car);
        }

        if (head.eq(syms.integer)) {
            if (!obj.isFixnum() and !obj.isBignum()) return false;
            const rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (rest.isNil()) return true;
            if (!rest.isCons()) return error.InvalidTypeSpecifier;

            const args1 = rest.toPtr(@import("../objects.zig").Cons);
            const low = try parseIntBound(syms, args1.car);
            if (args1.cdr.isNil()) {
                return try integerLowerOk(obj, low);
            }
            if (!args1.cdr.isCons()) return error.InvalidTypeSpecifier;

            const args2 = args1.cdr.toPtr(@import("../objects.zig").Cons);
            const high = try parseIntBound(syms, args2.car);
            if (!args2.cdr.isNil()) return error.InvalidTypeSpecifier;

            return try integerLowerOk(obj, low) and try integerUpperOk(obj, high);
        }

        // (eql value) - singleton type
        if (head.eq(syms.eql)) {
            const rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (!rest.isCons()) return error.InvalidTypeSpecifier;
            const expected = rest.toPtr(@import("../objects.zig").Cons).car;
            return valueEql(obj,expected);
        }

        // (member value...) - enumeration type
        if (head.eq(syms.member)) {
            var rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const item = rest.toPtr(@import("../objects.zig").Cons).car;
                if (valueEql(obj,item)) return true;
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return false;
        }

        // (mod n) - integers from 0 to n-1
        if (head.eq(syms.mod)) {
            if (!obj.isFixnum() and !obj.isBignum()) return false;
            const rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (!rest.isCons()) return error.InvalidTypeSpecifier;
            const n = rest.toPtr(@import("../objects.zig").Cons).car;
            if (!n.isFixnum()) return error.InvalidTypeSpecifier;
            const limit = n.toFixnum();
            const val = if (obj.isFixnum()) obj.toFixnum() else return false;
            return val >= 0 and val < limit;
        }

        // (signed-byte n) - signed integers in n bits
        if (head.eq(syms.signed_byte)) {
            if (!obj.isFixnum() and !obj.isBignum()) return false;
            const rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (!rest.isCons()) return error.InvalidTypeSpecifier;
            const n = rest.toPtr(@import("../objects.zig").Cons).car;
            if (!n.isFixnum()) return error.InvalidTypeSpecifier;
            const bits = n.toFixnum();
            if (bits <= 0 or bits > 63) return error.InvalidTypeSpecifier;
            const val = if (obj.isFixnum()) obj.toFixnum() else return false;
            const min = -(@as(i64, 1) << @intCast(bits - 1));
            const max = (@as(i64, 1) << @intCast(bits - 1)) - 1;
            return val >= min and val <= max;
        }

        // (unsigned-byte n) - unsigned integers in n bits
        if (head.eq(syms.unsigned_byte)) {
            if (!obj.isFixnum() and !obj.isBignum()) return false;
            const rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (!rest.isCons()) return error.InvalidTypeSpecifier;
            const n = rest.toPtr(@import("../objects.zig").Cons).car;
            if (!n.isFixnum()) return error.InvalidTypeSpecifier;
            const bits = n.toFixnum();
            if (bits <= 0 or bits > 63) return error.InvalidTypeSpecifier;
            const val = if (obj.isFixnum()) obj.toFixnum() else return false;
            if (val < 0) return false;
            const max = (@as(i64, 1) << @intCast(bits)) - 1;
            return val <= max;
        }

        // (values type...) - multiple values type specifier, not for typep
        if (head.eq(syms.values)) {
            // values type is for declarations, not runtime typep
            return false;
        }

        return error.UnknownTypeSpecifier;
    }

    return error.InvalidTypeSpecifier;
}

pub fn subtypep(heap: *Heap, type1: Value, type2: Value) !Value {
    const result = try subtypepCheck(heap, type1, type2);
    const bool_val = if (result.is_subtype) Value.t else Value.nil;
    const certain_val = if (result.certain) Value.t else Value.nil;
    const list = try heap.allocCons(bool_val, try heap.allocCons(certain_val, Value.nil));
    return list;
}

pub fn isSubtype(heap: *Heap, type1: Value, type2: Value) !bool {
    return (try subtypepCheck(heap, type1, type2)).is_subtype;
}

const SubtypeResult = struct {
    is_subtype: bool,
    certain: bool,
};

fn subtypepCheck(heap: *Heap, type1: Value, type2: Value) !SubtypeResult {
    if (type2.eq(Value.t)) return .{ .is_subtype = true, .certain = true };
    if (type1.eq(Value.nil)) return .{ .is_subtype = false, .certain = true };
    if (type1.eq(type2)) return .{ .is_subtype = true, .certain = true };

    if (type1.isSymbol() or type1.isT()) {
        if (type2.isSymbol() or type2.isT()) {
            return try checkSymbolSubtype(heap, type1, type2);
        }
        if (type2.isCons()) {
            const head = type2.toPtr(@import("../objects.zig").Cons).car;
            if (head.eq(try clSym(heap, "and"))) {
                var rest = type2.toPtr(@import("../objects.zig").Cons).cdr;
                while (rest.isCons()) {
                    const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                    const sub = try subtypepCheck(heap, type1, spec);
                    if (sub.is_subtype) return .{ .is_subtype = true, .certain = sub.certain };
                    rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
                }
                return .{ .is_subtype = false, .certain = true };
            }
        }
    }

    if (type1.isCons()) {
        const head = type1.toPtr(@import("../objects.zig").Cons).car;
        if (head.eq(try clSym(heap, "and"))) {
            var rest = type1.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                const sub = try subtypepCheck(heap, spec, type2);
                if (!sub.is_subtype) return .{ .is_subtype = false, .certain = sub.certain };
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return .{ .is_subtype = true, .certain = true };
        }
        if (head.eq(try clSym(heap, "or"))) {
            var rest = type1.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                const sub = try subtypepCheck(heap, spec, type2);
                if (!sub.is_subtype) return .{ .is_subtype = false, .certain = sub.certain };
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return .{ .is_subtype = true, .certain = true };
        }
        if (head.eq(try clSym(heap, "not"))) {
            const inner_cons = type1.toPtr(@import("../objects.zig").Cons).cdr;
            if (!inner_cons.isCons()) return .{ .is_subtype = false, .certain = false };
            const inner = inner_cons.toPtr(@import("../objects.zig").Cons).car;
            if (type2.isCons()) {
                const head2 = type2.toPtr(@import("../objects.zig").Cons).car;
                if (head2.eq(try clSym(heap, "not"))) {
                    const inner_cons2 = type2.toPtr(@import("../objects.zig").Cons).cdr;
                    if (!inner_cons2.isCons()) return .{ .is_subtype = false, .certain = false };
                    const inner2 = inner_cons2.toPtr(@import("../objects.zig").Cons).car;
                    return try subtypepCheck(heap, inner2, inner);
                }
            }
            return .{ .is_subtype = false, .certain = false };
        }
    }

    if (type2.isCons()) {
        const head = type2.toPtr(@import("../objects.zig").Cons).car;
        if (head.eq(try clSym(heap, "or"))) {
            var rest = type2.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                const sub = try subtypepCheck(heap, type1, spec);
                if (sub.is_subtype) return .{ .is_subtype = true, .certain = sub.certain };
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return .{ .is_subtype = false, .certain = true };
        }
        if (head.eq(try clSym(heap, "not"))) {
            return .{ .is_subtype = false, .certain = false };
        }
    }

    return .{ .is_subtype = false, .certain = false };
}

fn checkSymbolSubtype(heap: *Heap, t1: Value, t2: Value) !SubtypeResult {
    const fixnum_sym = try clSym(heap, "fixnum");
    const integer_sym = try clSym(heap, "integer");
    const rational_sym = try clSym(heap, "rational");
    const real_sym = try clSym(heap, "real");
    const number_sym = try clSym(heap, "number");
    const float_sym = try clSym(heap, "float");
    const bignum_sym = try clSym(heap, "bignum");
    const ratio_sym = try clSym(heap, "ratio");
    const complex_sym = try clSym(heap, "complex");
    const null_sym = try clSym(heap, "null");
    const symbol_sym = try clSym(heap, "symbol");
    const cons_sym = try clSym(heap, "cons");
    const list_sym = try clSym(heap, "list");
    const sequence_sym = try clSym(heap, "sequence");
    const vector_sym = try clSym(heap, "vector");
    const string_sym = try clSym(heap, "string");
    const array_sym = try clSym(heap, "array");
    const class_sym = try clSym(heap, "class");
    const standard_class_sym = try clSym(heap, "standard-class");
    const built_in_class_sym = try clSym(heap, "built-in-class");
    const structure_class_sym = try clSym(heap, "structure-class");

    if (t1.eq(fixnum_sym)) {
        if (t2.eq(integer_sym) or t2.eq(rational_sym) or t2.eq(real_sym) or t2.eq(number_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(bignum_sym)) {
        if (t2.eq(integer_sym) or t2.eq(rational_sym) or t2.eq(real_sym) or t2.eq(number_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(integer_sym)) {
        if (t2.eq(rational_sym) or t2.eq(real_sym) or t2.eq(number_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(ratio_sym)) {
        if (t2.eq(rational_sym) or t2.eq(real_sym) or t2.eq(number_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(rational_sym)) {
        if (t2.eq(real_sym) or t2.eq(number_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(float_sym)) {
        if (t2.eq(real_sym) or t2.eq(number_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(real_sym)) {
        if (t2.eq(number_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(complex_sym) and t2.eq(number_sym)) {
        return .{ .is_subtype = true, .certain = true };
    }

    if (t1.eq(null_sym)) {
        if (t2.eq(symbol_sym) or t2.eq(list_sym) or t2.eq(sequence_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(cons_sym)) {
        if (t2.eq(list_sym) or t2.eq(sequence_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(list_sym) and t2.eq(sequence_sym)) {
        return .{ .is_subtype = true, .certain = true };
    }

    if (t1.eq(vector_sym)) {
        if (t2.eq(array_sym) or t2.eq(sequence_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(string_sym)) {
        if (t2.eq(array_sym) or t2.eq(sequence_sym)) {
            return .{ .is_subtype = true, .certain = true };
        }
    }

    if (t1.eq(array_sym) and t2.eq(sequence_sym)) {
        return .{ .is_subtype = false, .certain = true };
    }

    if ((t1.eq(standard_class_sym) or t1.eq(built_in_class_sym) or t1.eq(structure_class_sym)) and t2.eq(class_sym)) {
        return .{ .is_subtype = true, .certain = true };
    }

    if (t1.eq(t2)) {
        return .{ .is_subtype = true, .certain = true };
    }

    // Check user-defined class hierarchy via CPL
    if (heap.findLispClass(t1)) |class1_val| {
        if (class1_val.isClass()) {
            const class1 = class1_val.toPtr(@import("../objects.zig").Class);
            // Check if t2 is in class1's CPL
            var cpl = class1.cpl;
            while (cpl.isCons()) {
                const cons = cpl.toPtr(@import("../objects.zig").Cons);
                if (cons.car.eq(t2)) return .{ .is_subtype = true, .certain = true };
                if (cons.car.isClass()) {
                    const cpl_class = cons.car.toPtr(@import("../objects.zig").Class);
                    if (cpl_class.name.eq(t2)) return .{ .is_subtype = true, .certain = true };
                }
                cpl = cons.cdr;
            }
            // t1 is a known class but t2 not in its CPL
            return .{ .is_subtype = false, .certain = true };
        }
    }

    return .{ .is_subtype = false, .certain = true };
}

pub fn typeOf(heap: *Heap, val: Value) !Value {
    return switch (val.typeKind()) {
        .nil => clSym(heap, "nil"),
        .t => clSym(heap, "boolean"),
        .unbound => clSym(heap, "symbol"),
        .fixnum => clSym(heap, "fixnum"),
        .float => clSym(heap, "double-float"),
        .char => clSym(heap, "character"),
        .cons => clSym(heap, "cons"),
        .symbol => clSym(heap, "symbol"),
        .vector => {
            if (val.toPtr(@import("../objects.zig").Vector).isCharacterVector()) {
                return clSym(heap, "string");
            }
            // Check if this is a class/struct instance (first element is class name symbol)
            const vec = val.toPtr(@import("../objects.zig").Vector);
            if (vec.length > 0 and vec.data[0].isSymbol()) {
                return vec.data[0];
            }
            return clSym(heap, "vector");
        },
        .structure => {
            const obj = val.toPtr(@import("../objects.zig").Structure);
            if (obj.class.isClass()) {
                return obj.class.toPtr(@import("../objects.zig").Class).name;
            }
            return clSym(heap, "structure-object");
        },
        .string => clSym(heap, "string"),
        .string32 => clSym(heap, "string"),
        .closure => clSym(heap, "closure"),
        .keyword => clSym(heap, "keyword"),
        .hashtable => clSym(heap, "hash-table"),
        .rational => clSym(heap, "ratio"),
        .complex => clSym(heap, "complex"),
        .stream => clSym(heap, "stream"),
        .bignum => clSym(heap, "bignum"),
        .array => clSym(heap, "array"),
        .pathname => clSym(heap, "pathname"),
        .package => clSym(heap, "package"),
        .readtable => clSym(heap, "readtable"),
        .chunk => clSym(heap, "compiled-function"),
        .condition => clSym(heap, "condition"),
        .class => clSym(heap, "standard-class"),
        .slotdef => clSym(heap, "slot-definition"),
        .generic_function => clSym(heap, "generic-function"),
        .method => clSym(heap, "method"),
        .native_code => clSym(heap, "compiled-function"),
        .macro_env => heap.intern("macro-env"),
    };
}

test "typep basic types" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var syms = try TypeSymbols.init(&heap);

    const fixnum = Value.makeFixnum(42);
    try testing.expect(try typep(&heap, &syms, fixnum, try heap.intern("integer")));
    try testing.expect(try typep(&heap, &syms, fixnum, try heap.intern("fixnum")));
    try testing.expect(!try typep(&heap, &syms, fixnum, try heap.intern("string")));

    const str = try heap.allocBaseString("test");
    try testing.expect(try typep(&heap, &syms, str, try heap.intern("string")));
    try testing.expect(!try typep(&heap, &syms, str, try heap.intern("integer")));

    const consval = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    try testing.expect(try typep(&heap, &syms, consval, try heap.intern("cons")));
    try testing.expect(try typep(&heap, &syms, consval, try heap.intern("list")));

    try testing.expect(try typep(&heap, &syms, Value.nil, try heap.intern("null")));
    try testing.expect(try typep(&heap, &syms, Value.nil, try heap.intern("list")));
    try testing.expect(try typep(&heap, &syms, Value.t, Value.t));
    try testing.expect(!try typep(&heap, &syms, Value.t, Value.nil));
}

test "typep compound types" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var syms = try TypeSymbols.init(&heap);

    const fixnum = Value.makeFixnum(42);

    const or_spec = try heap.allocCons(
        try heap.intern("or"),
        try heap.allocCons(
            try heap.intern("integer"),
            try heap.allocCons(try heap.intern("string"), Value.nil),
        ),
    );
    try testing.expect(try typep(&heap, &syms, fixnum, or_spec));

    const str = try heap.allocBaseString("test");
    try testing.expect(try typep(&heap, &syms, str, or_spec));

    const not_spec = try heap.allocCons(
        try heap.intern("not"),
        try heap.allocCons(try heap.intern("string"), Value.nil),
    );
    const and_spec = try heap.allocCons(
        try heap.intern("and"),
        try heap.allocCons(
            try heap.intern("integer"),
            try heap.allocCons(not_spec, Value.nil),
        ),
    );
    try testing.expect(try typep(&heap, &syms, fixnum, and_spec));
    try testing.expect(!try typep(&heap, &syms, str, and_spec));
}

test "typep cons type specifier" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var syms = try TypeSymbols.init(&heap);

    const car_val = try heap.intern("foo");
    const cdr_val = Value.makeFixnum(7);
    const pair = try heap.allocCons(car_val, cdr_val);

    const cons_sym = try heap.intern("cons");
    const sym_sym = try heap.intern("symbol");
    const int_sym = try heap.intern("integer");
    const str_sym = try heap.intern("string");

    const spec_car_only = try heap.allocCons(
        cons_sym,
        try heap.allocCons(sym_sym, Value.nil),
    );
    try testing.expect(try typep(&heap, &syms, pair, spec_car_only));

    const spec_car_cdr = try heap.allocCons(
        cons_sym,
        try heap.allocCons(
            sym_sym,
            try heap.allocCons(int_sym, Value.nil),
        ),
    );
    try testing.expect(try typep(&heap, &syms, pair, spec_car_cdr));

    const bad_spec = try heap.allocCons(
        cons_sym,
        try heap.allocCons(
            str_sym,
            try heap.allocCons(int_sym, Value.nil),
        ),
    );
    try testing.expect(!try typep(&heap, &syms, pair, bad_spec));
}

test "typep integer range" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var syms = try TypeSymbols.init(&heap);
    const integer_sym = try heap.intern("integer");

    const range_spec = try heap.allocCons(
        integer_sym,
        try heap.allocCons(
            Value.makeFixnum(0),
            try heap.allocCons(Value.makeFixnum(100), Value.nil),
        ),
    );

    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(50), range_spec));
    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(0), range_spec));
    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(100), range_spec));
    try testing.expect(!try typep(&heap, &syms, Value.makeFixnum(101), range_spec));
    try testing.expect(!try typep(&heap, &syms, Value.makeFixnum(-1), range_spec));

    const lower_only = try heap.allocCons(
        integer_sym,
        try heap.allocCons(Value.makeFixnum(0), Value.nil),
    );
    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(0), lower_only));
    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(5), lower_only));
    try testing.expect(!try typep(&heap, &syms, Value.makeFixnum(-1), lower_only));

    const upper_only = try heap.allocCons(
        integer_sym,
        try heap.allocCons(
            syms.star,
            try heap.allocCons(Value.makeFixnum(0), Value.nil),
        ),
    );
    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(-1), upper_only));
    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(0), upper_only));
    try testing.expect(!try typep(&heap, &syms, Value.makeFixnum(1), upper_only));

    const excl_low = try heap.allocCons(Value.makeFixnum(0), Value.nil);
    const open_low = try heap.allocCons(
        integer_sym,
        try heap.allocCons(
            excl_low,
            try heap.allocCons(syms.star, Value.nil),
        ),
    );
    try testing.expect(!try typep(&heap, &syms, Value.makeFixnum(0), open_low));
    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(1), open_low));

    const excl_high = try heap.allocCons(Value.makeFixnum(10), Value.nil);
    const open_high = try heap.allocCons(
        integer_sym,
        try heap.allocCons(
            syms.star,
            try heap.allocCons(excl_high, Value.nil),
        ),
    );
    try testing.expect(try typep(&heap, &syms, Value.makeFixnum(9), open_high));
    try testing.expect(!try typep(&heap, &syms, Value.makeFixnum(10), open_high));

    const max_fixnum = Value.makeFixnum((@as(i64, 1) << 62) - 1);
    const bignum = try arith.add(&heap, max_fixnum, Value.makeFixnum(1));
    try testing.expect(bignum.isBignum());
    try testing.expect(!try typep(&heap, &syms, bignum, range_spec));

    const bad_range = try heap.allocCons(
        integer_sym,
        try heap.allocCons(
            try heap.intern("foo"),
            try heap.allocCons(Value.makeFixnum(10), Value.nil),
        ),
    );
    try testing.expectError(error.InvalidTypeSpecifier, typep(&heap, &syms, Value.makeFixnum(5), bad_range));

    const bad_open = try heap.allocCons(
        integer_sym,
        try heap.allocCons(
            try heap.allocCons(try heap.intern("foo"), Value.nil),
            try heap.allocCons(syms.star, Value.nil),
        ),
    );
    try testing.expectError(error.InvalidTypeSpecifier, typep(&heap, &syms, Value.makeFixnum(5), bad_open));

    const too_many_args = try heap.allocCons(
        integer_sym,
        try heap.allocCons(
            Value.makeFixnum(0),
            try heap.allocCons(
                Value.makeFixnum(1),
                try heap.allocCons(Value.makeFixnum(2), Value.nil),
            ),
        ),
    );
    try testing.expectError(error.InvalidTypeSpecifier, typep(&heap, &syms, Value.makeFixnum(1), too_many_args));
}

test "typeOf basic types" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const fixnum = Value.makeFixnum(42);
    const result = try typeOf(&heap, fixnum);
    try testing.expect(result.isSymbol());
    const fixnum_sym = try heap.intern("fixnum");
    try testing.expect(result.eq(fixnum_sym));

    const str = try heap.allocBaseString("test");
    const str_type = try typeOf(&heap, str);
    try testing.expect(str_type.isSymbol());
    const string_sym = try heap.intern("string");
    try testing.expect(str_type.eq(string_sym));

    const consval = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const cons_type = try typeOf(&heap, consval);
    try testing.expect(cons_type.isSymbol());
    const cons_sym = try heap.intern("cons");
    try testing.expect(cons_type.eq(cons_sym));
}

test "subtypep numeric hierarchy" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result1 = try subtypep(&heap, try heap.intern("fixnum"), try heap.intern("integer"));
    try testing.expect(result1.isCons());
    const r1_car = result1.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r1_car.isT());

    const result2 = try subtypep(&heap, try heap.intern("integer"), try heap.intern("rational"));
    const r2_car = result2.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r2_car.isT());

    const result3 = try subtypep(&heap, try heap.intern("rational"), try heap.intern("real"));
    const r3_car = result3.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r3_car.isT());

    const result4 = try subtypep(&heap, try heap.intern("real"), try heap.intern("number"));
    const r4_car = result4.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r4_car.isT());

    const result5 = try subtypep(&heap, try heap.intern("string"), try heap.intern("fixnum"));
    const r5_car = result5.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r5_car.isNil());
}

test "subtypep sequence hierarchy" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result1 = try subtypep(&heap, try heap.intern("null"), try heap.intern("list"));
    const r1_car = result1.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r1_car.isT());

    const result2 = try subtypep(&heap, try heap.intern("cons"), try heap.intern("list"));
    const r2_car = result2.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r2_car.isT());

    const result3 = try subtypep(&heap, try heap.intern("list"), try heap.intern("sequence"));
    const r3_car = result3.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r3_car.isT());

    const result4 = try subtypep(&heap, try heap.intern("vector"), try heap.intern("sequence"));
    const r4_car = result4.toPtr(@import("../objects.zig").Cons).car;
    try testing.expect(r4_car.isT());
}
