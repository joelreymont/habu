const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;

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
            .cons = try heap.intern("cons"),
            .symbol = try heap.intern("symbol"),
            .null = try heap.intern("null"),
            .boolean = try heap.intern("boolean"),
            .integer = try heap.intern("integer"),
            .fixnum = try heap.intern("fixnum"),
            .bignum = try heap.intern("bignum"),
            .float = try heap.intern("float"),
            .real = try heap.intern("real"),
            .rational = try heap.intern("rational"),
            .ratio = try heap.intern("ratio"),
            .number = try heap.intern("number"),
            .complex = try heap.intern("complex"),
            .character = try heap.intern("character"),
            .string = try heap.intern("string"),
            .vector = try heap.intern("vector"),
            .array = try heap.intern("array"),
            .list = try heap.intern("list"),
            .sequence = try heap.intern("sequence"),
            .function = try heap.intern("function"),
            .compiled_function = try heap.intern("compiled-function"),
            .closure = try heap.intern("closure"),
            .keyword = try heap.intern("keyword"),
            .hash_table = try heap.intern("hash-table"),
            .stream = try heap.intern("stream"),
            .pathname = try heap.intern("pathname"),
            .package = try heap.intern("package"),
            .atom = try heap.intern("atom"),
            .base_char = try heap.intern("base-char"),
            .standard_char = try heap.intern("standard-char"),
            .extended_char = try heap.intern("extended-char"),
            .base_string = try heap.intern("base-string"),
            .simple_string = try heap.intern("simple-string"),
            .simple_base_string = try heap.intern("simple-base-string"),
            .simple_vector = try heap.intern("simple-vector"),
            .simple_array = try heap.intern("simple-array"),
            .bit_vector = try heap.intern("bit-vector"),
            .simple_bit_vector = try heap.intern("simple-bit-vector"),
            .single_float = try heap.intern("single-float"),
            .double_float = try heap.intern("double-float"),
            .short_float = try heap.intern("short-float"),
            .long_float = try heap.intern("long-float"),
            .class = try heap.intern("class"),
            .standard_class = try heap.intern("standard-class"),
            .built_in_class = try heap.intern("built-in-class"),
            .structure_class = try heap.intern("structure-class"),
            .generic_function = try heap.intern("generic-function"),
            .standard_generic_function = try heap.intern("standard-generic-function"),
            .method = try heap.intern("method"),
            .standard_method = try heap.intern("standard-method"),
            .standard_object = try heap.intern("standard-object"),
            .structure_object = try heap.intern("structure-object"),
            .file_stream = try heap.intern("file-stream"),
            .string_stream = try heap.intern("string-stream"),
            .random_state = try heap.intern("random-state"),
            .restart = try heap.intern("restart"),
            .method_combination = try heap.intern("method-combination"),
            .values = try heap.intern("values"),
            .@"or" = try heap.intern("or"),
            .@"and" = try heap.intern("and"),
            .@"not" = try heap.intern("not"),
            .satisfies = try heap.intern("satisfies"),
            .star = try heap.intern("*"),
            .eql = try heap.intern("eql"),
            .member = try heap.intern("member"),
            .mod = try heap.intern("mod"),
            .signed_byte = try heap.intern("signed-byte"),
            .unsigned_byte = try heap.intern("unsigned-byte"),
            .t = try heap.intern("t"),
            .nil = try heap.intern("nil"),
            .unbound = try heap.intern("%unbound%"),
        };
    }
};

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
        if (sym.eq(syms.string)) return obj.isString();
        if (sym.eq(syms.vector)) return obj.isVector();
        if (sym.eq(syms.array)) return obj.isArray() or obj.isVector() or obj.isString();
        if (sym.eq(syms.list)) return obj.isNil() or obj.isCons();
        if (sym.eq(syms.sequence)) return obj.isNil() or obj.isCons() or obj.isVector() or obj.isString();
        if (sym.eq(syms.function)) return obj.isClosure() or obj.isChunk();
        if (sym.eq(syms.compiled_function)) return obj.isChunk();
        if (sym.eq(syms.keyword)) return obj.isKeyword();
        if (sym.eq(syms.hash_table)) return obj.isHashTable();
        if (sym.eq(syms.stream)) return obj.isStream();
        if (sym.eq(syms.pathname)) return obj.isPathname();
        if (sym.eq(syms.package)) return obj.isPackage();
        if (sym.eq(syms.atom)) return !obj.isCons();
        if (sym.eq(syms.base_char)) return obj.isCharacter();
        if (sym.eq(syms.standard_char)) return obj.isCharacter();
        if (sym.eq(syms.extended_char)) return false; // no extended chars
        if (sym.eq(syms.base_string)) return obj.isString();
        if (sym.eq(syms.simple_string)) return obj.isString();
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
        if (sym.eq(syms.standard_object)) return obj.isVector(); // instances are vectors
        if (sym.eq(syms.structure_object)) return obj.isVector(); // structs are vectors
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
        // restart objects aren't first-class - always false for typep
        if (sym.eq(syms.restart)) return false;
        // method-combination isn't implemented as separate type
        if (sym.eq(syms.method_combination)) return false;
        // values is a type specifier for multiple return values, not for typep
        if (sym.eq(syms.values)) return false;

        const maybe_class = heap.findLispClass(type_spec);

        // Check if it's a class name (instance type check)
        if (obj.isVector()) {
            const vec = obj.toPtr(@import("../objects.zig").Vector);
            if (vec.length > 0 and vec.data[0].isSymbol()) {
                // Direct class name match
                if (vec.data[0].eq(type_spec)) return true;

                // Check class hierarchy via CPL
                if (heap.findLispClass(vec.data[0])) |class_val| {
                    if (class_val.isClass()) {
                        const class = class_val.toPtr(@import("../objects.zig").Class);
                        // Check if type_spec is in the CPL
                        var cpl = class.cpl;
                        while (cpl.isCons()) {
                            const cons = cpl.toPtr(@import("../objects.zig").Cons);
                            // CPL contains class objects or symbols - check both
                            if (cons.car.eq(type_spec)) return true;
                            if (cons.car.isClass()) {
                                const cpl_class = cons.car.toPtr(@import("../objects.zig").Class);
                                if (cpl_class.name.eq(type_spec)) return true;
                            }
                            cpl = cons.cdr;
                        }
                    }
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

        if (head.eq(syms.integer)) {
            if (!obj.isFixnum() and !obj.isBignum()) return false;
            const rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (rest.isNil()) return true;
            if (!rest.isCons()) return error.InvalidTypeSpecifier;
            const low = rest.toPtr(@import("../objects.zig").Cons).car;
            const cdr = rest.toPtr(@import("../objects.zig").Cons).cdr;
            if (!cdr.isCons()) return error.InvalidTypeSpecifier;
            const high = cdr.toPtr(@import("../objects.zig").Cons).car;

            const val = if (obj.isFixnum()) obj.toFixnum() else return true;

            const low_ok = switch (low.typeKind()) {
                .symbol => if (low.eq(syms.star)) true else return error.InvalidTypeSpecifier,
                .fixnum => val >= low.toFixnum(),
                else => return error.InvalidTypeSpecifier,
            };

            const high_ok = switch (high.typeKind()) {
                .symbol => if (high.eq(syms.star)) true else return error.InvalidTypeSpecifier,
                .fixnum => val <= high.toFixnum(),
                else => return error.InvalidTypeSpecifier,
            };

            return low_ok and high_ok;
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
            if (head.eq(try heap.intern("and"))) {
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
        if (head.eq(try heap.intern("and"))) {
            var rest = type1.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                const sub = try subtypepCheck(heap, spec, type2);
                if (!sub.is_subtype) return .{ .is_subtype = false, .certain = sub.certain };
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return .{ .is_subtype = true, .certain = true };
        }
        if (head.eq(try heap.intern("or"))) {
            var rest = type1.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                const sub = try subtypepCheck(heap, spec, type2);
                if (!sub.is_subtype) return .{ .is_subtype = false, .certain = sub.certain };
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return .{ .is_subtype = true, .certain = true };
        }
        if (head.eq(try heap.intern("not"))) {
            const inner_cons = type1.toPtr(@import("../objects.zig").Cons).cdr;
            if (!inner_cons.isCons()) return .{ .is_subtype = false, .certain = false };
            const inner = inner_cons.toPtr(@import("../objects.zig").Cons).car;
            if (type2.isCons()) {
                const head2 = type2.toPtr(@import("../objects.zig").Cons).car;
                if (head2.eq(try heap.intern("not"))) {
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
        if (head.eq(try heap.intern("or"))) {
            var rest = type2.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                const sub = try subtypepCheck(heap, type1, spec);
                if (sub.is_subtype) return .{ .is_subtype = true, .certain = sub.certain };
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return .{ .is_subtype = false, .certain = true };
        }
        if (head.eq(try heap.intern("not"))) {
            return .{ .is_subtype = false, .certain = false };
        }
    }

    return .{ .is_subtype = false, .certain = false };
}

fn checkSymbolSubtype(heap: *Heap, t1: Value, t2: Value) !SubtypeResult {
    const fixnum_sym = try heap.intern("fixnum");
    const integer_sym = try heap.intern("integer");
    const rational_sym = try heap.intern("rational");
    const real_sym = try heap.intern("real");
    const number_sym = try heap.intern("number");
    const float_sym = try heap.intern("float");
    const bignum_sym = try heap.intern("bignum");
    const ratio_sym = try heap.intern("ratio");
    const complex_sym = try heap.intern("complex");
    const null_sym = try heap.intern("null");
    const symbol_sym = try heap.intern("symbol");
    const cons_sym = try heap.intern("cons");
    const list_sym = try heap.intern("list");
    const sequence_sym = try heap.intern("sequence");
    const vector_sym = try heap.intern("vector");
    const string_sym = try heap.intern("string");
    const array_sym = try heap.intern("array");

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
        .nil => heap.intern("nil"),
        .t => heap.intern("boolean"),
        .unbound => heap.intern("symbol"),
        .fixnum => heap.intern("fixnum"),
        .float => heap.intern("double-float"),
        .char => heap.intern("character"),
        .cons => heap.intern("cons"),
        .symbol => heap.intern("symbol"),
        .vector => {
            // Check if this is a class/struct instance (first element is class name symbol)
            const vec = val.toPtr(@import("../objects.zig").Vector);
            if (vec.length > 0 and vec.data[0].isSymbol()) {
                return vec.data[0];
            }
            return heap.intern("vector");
        },
        .string => heap.intern("string"),
        .string32 => heap.intern("string"),
        .closure => heap.intern("closure"),
        .keyword => heap.intern("keyword"),
        .hashtable => heap.intern("hash-table"),
        .rational => heap.intern("ratio"),
        .complex => heap.intern("complex"),
        .stream => heap.intern("stream"),
        .bignum => heap.intern("bignum"),
        .array => heap.intern("array"),
        .pathname => heap.intern("pathname"),
        .package => heap.intern("package"),
        .chunk => heap.intern("compiled-function"),
        .condition => heap.intern("condition"),
        .class => heap.intern("standard-class"),
        .slotdef => heap.intern("slot-definition"),
        .generic_function => heap.intern("generic-function"),
        .method => heap.intern("method"),
        .native_code => heap.intern("compiled-function"),
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

test "typep integer range" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var syms = try TypeSymbols.init(&heap);

    const range_spec = try heap.allocCons(
        try heap.intern("integer"),
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

    const bad_range = try heap.allocCons(
        try heap.intern("integer"),
        try heap.allocCons(
            try heap.intern("foo"),
            try heap.allocCons(Value.makeFixnum(10), Value.nil),
        ),
    );
    try testing.expectError(error.InvalidTypeSpecifier, typep(&heap, &syms, Value.makeFixnum(5), bad_range));
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
