const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;

pub fn typep(heap: *Heap, obj: Value, type_spec: Value) !bool {
    if (type_spec.isT()) return true;
    if (type_spec.isNil()) return false;

    if (type_spec.isSymbol() or type_spec.isT() or type_spec.isNil()) {
        const sym = type_spec;

        if (sym.eq(try heap.intern("cons"))) return obj.isCons();
        if (sym.eq(try heap.intern("symbol"))) return obj.isSymbolLike();
        if (sym.eq(try heap.intern("null"))) return obj.isNil();
        if (sym.eq(try heap.intern("boolean"))) return obj.isNil() or obj.isT();
        if (sym.eq(try heap.intern("integer"))) return obj.isFixnum() or obj.isBignum();
        if (sym.eq(try heap.intern("fixnum"))) return obj.isFixnum();
        if (sym.eq(try heap.intern("bignum"))) return obj.isBignum();
        if (sym.eq(try heap.intern("float"))) return obj.isFloat();
        if (sym.eq(try heap.intern("real"))) return obj.isFixnum() or obj.isBignum() or obj.isFloat() or obj.isRational();
        if (sym.eq(try heap.intern("rational"))) return obj.isFixnum() or obj.isBignum() or obj.isRational();
        if (sym.eq(try heap.intern("ratio"))) return obj.isRational();
        if (sym.eq(try heap.intern("number"))) return obj.isFixnum() or obj.isBignum() or obj.isFloat() or obj.isRational() or obj.isComplex();
        if (sym.eq(try heap.intern("complex"))) return obj.isComplex();
        if (sym.eq(try heap.intern("character"))) return obj.isCharacter();
        if (sym.eq(try heap.intern("string"))) return obj.isString();
        if (sym.eq(try heap.intern("vector"))) return obj.isVector();
        if (sym.eq(try heap.intern("array"))) return obj.isArray() or obj.isVector() or obj.isString();
        if (sym.eq(try heap.intern("list"))) return obj.isNil() or obj.isCons();
        if (sym.eq(try heap.intern("sequence"))) return obj.isNil() or obj.isCons() or obj.isVector() or obj.isString();
        if (sym.eq(try heap.intern("function"))) return obj.isClosure() or obj.isChunk();
        if (sym.eq(try heap.intern("compiled-function"))) return obj.isChunk();
        if (sym.eq(try heap.intern("keyword"))) return obj.isKeyword();
        if (sym.eq(try heap.intern("hash-table"))) return obj.isHashTable();
        if (sym.eq(try heap.intern("stream"))) return obj.isStream();
        if (sym.eq(try heap.intern("pathname"))) return obj.isPathname();
        if (sym.eq(try heap.intern("package"))) return obj.isPackage();

        // Check if it's a class name (instance type check)
        if (obj.isVector()) {
            const vec = obj.toPtr(@import("../objects.zig").Vector);
            if (vec.length > 0 and vec.data[0].isSymbol()) {
                return vec.data[0].eq(type_spec);
            }
        }

        return error.UnknownTypeSpecifier;
    }

    if (type_spec.isCons()) {
        const head = type_spec.toPtr(@import("../objects.zig").Cons).car;

        if (head.eq(try heap.intern("or"))) {
            var rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                if (try typep(heap, obj, spec)) return true;
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return false;
        }

        if (head.eq(try heap.intern("and"))) {
            var rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            while (rest.isCons()) {
                const spec = rest.toPtr(@import("../objects.zig").Cons).car;
                if (!try typep(heap, obj, spec)) return false;
                rest = rest.toPtr(@import("../objects.zig").Cons).cdr;
            }
            return true;
        }

        if (head.eq(try heap.intern("not"))) {
            const inner = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (!inner.isCons()) return error.InvalidTypeSpecifier;
            return !try typep(heap, obj, inner.toPtr(@import("../objects.zig").Cons).car);
        }

        if (head.eq(try heap.intern("satisfies"))) {
            // (satisfies predicate-fn) - requires runtime evaluation
            // For now, conservatively return true (any value might satisfy)
            // Proper checking requires VM integration for predicate call
            return true;
        }

        if (head.eq(try heap.intern("integer"))) {
            if (!obj.isFixnum() and !obj.isBignum()) return false;
            const rest = type_spec.toPtr(@import("../objects.zig").Cons).cdr;
            if (rest.isNil()) return true;
            if (!rest.isCons()) return error.InvalidTypeSpecifier;
            const low = rest.toPtr(@import("../objects.zig").Cons).car;
            const cdr = rest.toPtr(@import("../objects.zig").Cons).cdr;
            if (!cdr.isCons()) return error.InvalidTypeSpecifier;
            const high = cdr.toPtr(@import("../objects.zig").Cons).car;

            const val = if (obj.isFixnum()) obj.toFixnum() else return true;

            const low_ok = if (low.eq(try heap.intern("*")))
                true
            else if (low.isFixnum())
                val >= low.toFixnum()
            else
                return error.InvalidTypeSpecifier;

            const high_ok = if (high.eq(try heap.intern("*")))
                true
            else if (high.isFixnum())
                val <= high.toFixnum()
            else
                return error.InvalidTypeSpecifier;

            return low_ok and high_ok;
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

    return .{ .is_subtype = false, .certain = true };
}

pub fn typeOf(heap: *Heap, val: Value) !Value {
    return switch (val.typeKind()) {
        .nil => heap.intern("nil"),
        .t => heap.intern("boolean"),
        .fixnum => heap.intern("fixnum"),
        .float => heap.intern("float"),
        .char => heap.intern("character"),
        .cons => heap.intern("cons"),
        .symbol => heap.intern("symbol"),
        .vector => heap.intern("vector"),
        .string => heap.intern("string"),
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
    };
}

test "typep basic types" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const fixnum = Value.makeFixnum(42);
    try testing.expect(try typep(&heap, fixnum, try heap.intern("integer")));
    try testing.expect(try typep(&heap, fixnum, try heap.intern("fixnum")));
    try testing.expect(!try typep(&heap, fixnum, try heap.intern("string")));

    const str = try heap.allocString("test");
    try testing.expect(try typep(&heap, str, try heap.intern("string")));
    try testing.expect(!try typep(&heap, str, try heap.intern("integer")));

    const consval = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    try testing.expect(try typep(&heap, consval, try heap.intern("cons")));
    try testing.expect(try typep(&heap, consval, try heap.intern("list")));

    try testing.expect(try typep(&heap, Value.nil, try heap.intern("null")));
    try testing.expect(try typep(&heap, Value.nil, try heap.intern("list")));
    try testing.expect(try typep(&heap, Value.t, Value.t));
    try testing.expect(!try typep(&heap, Value.t, Value.nil));
}

test "typep compound types" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const fixnum = Value.makeFixnum(42);

    const or_spec = try heap.allocCons(
        try heap.intern("or"),
        try heap.allocCons(
            try heap.intern("integer"),
            try heap.allocCons(try heap.intern("string"), Value.nil),
        ),
    );
    try testing.expect(try typep(&heap, fixnum, or_spec));

    const str = try heap.allocString("test");
    try testing.expect(try typep(&heap, str, or_spec));

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
    try testing.expect(try typep(&heap, fixnum, and_spec));
    try testing.expect(!try typep(&heap, str, and_spec));
}

test "typep integer range" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const range_spec = try heap.allocCons(
        try heap.intern("integer"),
        try heap.allocCons(
            Value.makeFixnum(0),
            try heap.allocCons(Value.makeFixnum(100), Value.nil),
        ),
    );

    try testing.expect(try typep(&heap, Value.makeFixnum(50), range_spec));
    try testing.expect(try typep(&heap, Value.makeFixnum(0), range_spec));
    try testing.expect(try typep(&heap, Value.makeFixnum(100), range_spec));
    try testing.expect(!try typep(&heap, Value.makeFixnum(101), range_spec));
    try testing.expect(!try typep(&heap, Value.makeFixnum(-1), range_spec));
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

    const str = try heap.allocString("test");
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
