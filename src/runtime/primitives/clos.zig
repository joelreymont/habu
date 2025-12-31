const std = @import("std");
const Value = @import("../value.zig").Value;
const runtime = @import("../runtime.zig");
const Heap = @import("../heap.zig").Heap;
const Cons = @import("../objects.zig").Cons;
const Symbol = @import("../objects.zig").Symbol;
const Vector = @import("../objects.zig").Vector;
const Keyword = @import("../objects.zig").Keyword;

/// make-instance: (make-instance 'class-name :slot1 val1 :slot2 val2 ...)
/// Creates an instance of a class by allocating a vector #('class-name slot1-val slot2-val ...)
/// For now, this is a simplified version that requires slots to be provided in definition order
pub fn makeInstance(heap: *Heap, args: Value) !Value {
    // Parse: ('class-name :slot1 val1 :slot2 val2 ...)
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const class_name_val = cons1.car;

    // Class name should be a quoted symbol or symbol
    const class_name = if (class_name_val.isCons()) blk: {
        const quote_cons = class_name_val.toPtr(Cons);
        if (!quote_cons.cdr.isCons()) return error.InvalidArgument;
        const cdr_cons = quote_cons.cdr.toPtr(Cons);
        break :blk cdr_cons.car;
    } else class_name_val;

    if (!class_name.isSymbol()) return error.InvalidArgument;

    // Collect keyword arguments into a list
    var slot_values = std.ArrayList(Value){};
    defer slot_values.deinit(heap.allocator);

    var rest = cons1.cdr;
    while (rest.isCons()) {
        const kw_cons = rest.toPtr(Cons);
        const kw = kw_cons.car;

        // Expect keyword
        if (!kw.isKeyword()) return error.InvalidArgument;

        // Get value
        if (!kw_cons.cdr.isCons()) return error.InvalidArgument;
        const val_cons = kw_cons.cdr.toPtr(Cons);
        const val = val_cons.car;

        try slot_values.append(heap.allocator, val);
        rest = val_cons.cdr;
    }

    // Create vector: #('class-name slot1-val slot2-val ...)
    const vec_size = 1 + slot_values.items.len;
    const vec = try heap.allocVector(vec_size);
    vec.elements[0] = class_name;
    for (slot_values.items, 0..) |val, i| {
        vec.elements[1 + i] = val;
    }

    return Value.fromPtr(vec);
}

/// slot-value: (slot-value obj 'slot-name)
/// Generic slot accessor that looks up slot by name
pub fn slotValue(heap: *Heap, args: Value) !Value {
    // Parse: (obj 'slot-name)
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    if (!obj.isVector()) return error.InvalidArgument;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    var slot_name_val = cons2.car;

    // Handle quoted symbol: 'name -> name
    if (slot_name_val.isCons()) {
        const quote_cons = slot_name_val.toPtr(Cons);
        if (!quote_cons.cdr.isCons()) return error.InvalidArgument;
        const cdr_cons = quote_cons.cdr.toPtr(Cons);
        slot_name_val = cdr_cons.car;
    }

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;
    const slot_name = slot_name_val.toPtr(Symbol).getName();

    // Instance format: #(class-name slot1-val slot2-val ...)
    const vec = obj.toPtr(Vector);
    if (vec.length == 0) return error.InvalidArgument;

    const class_name_val = vec.data[0];
    if (!class_name_val.isSymbol()) return error.InvalidArgument;
    const class_name = class_name_val.toPtr(Symbol).getName();

    // Look up class metadata to find slot index
    const slot_names = heap.class_metadata.get(class_name) orelse return error.InvalidArgument;

    for (slot_names, 0..) |name, idx| {
        if (std.mem.eql(u8, name, slot_name)) {
            // Slot index in vector is idx+1 (since data[0] is class name)
            const vec_idx = idx + 1;
            if (vec_idx >= vec.length) return error.InvalidArgument;
            return vec.data[vec_idx];
        }
    }

    // Slot not found
    return error.InvalidArgument;
}

/// (setf (slot-value obj 'slot) value)
/// Set a slot value by name
pub fn setSlotValue(heap: *Heap, args: Value) !Value {
    _ = heap;
    _ = args;
    // TODO: Implement when we have setf infrastructure
    return error.NotImplemented;
}
