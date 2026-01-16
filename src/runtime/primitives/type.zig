const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;

pub fn typeOf(heap: *Heap, val: Value) !Value {
    return switch (val.typeKind()) {
        .nil => heap.intern("null"),
        .t => heap.intern("boolean"),
        .fixnum => blk: {
            const n = val.toFixnum();
            const low = Value.makeFixnum(n);
            const high = Value.makeFixnum(n);
            const int_sym = heap.intern("integer");
            const list = try heap.allocCons(low, Value.nil);
            const list2 = try heap.allocCons(high, list);
            break :blk try heap.allocCons(int_sym, list2);
        },
        .float => heap.intern("float"),
        .char => heap.intern("character"),
        .cons => blk: {
            const c = val.toPtr(@import("../objects.zig").Cons);
            const car_type = try typeOf(heap, c.car);
            const cdr_type = try typeOf(heap, c.cdr);
            const cons_sym = heap.intern("cons");
            const list = try heap.allocCons(car_type, Value.nil);
            const list2 = try heap.allocCons(cdr_type, list);
            break :blk try heap.allocCons(cons_sym, list2);
        },
        .symbol => heap.intern("symbol"),
        .vector => heap.intern("vector"),
        .string => heap.intern("string"),
        .closure => heap.intern("function"),
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
    };
}

test "typeOf basic types" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, 1024 * 1024);
    defer heap.deinit();

    const fixnum = Value.makeFixnum(42);
    const result = try typeOf(&heap, fixnum);
    try testing.expect(result.isSymbol());

    const str = try heap.allocString("test");
    const str_type = try typeOf(&heap, str);
    try testing.expect(str_type.isSymbol());

    const cons = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const cons_type = try typeOf(&heap, cons);
    try testing.expect(cons_type.isSymbol());
}
