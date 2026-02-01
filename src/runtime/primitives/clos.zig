const std = @import("std");
const Value = @import("../value.zig").Value;
const runtime = @import("../runtime.zig");
const heap_mod = @import("../heap.zig");
const Heap = heap_mod.Heap;
const objects = @import("../objects.zig");
const Cons = objects.Cons;
const Symbol = objects.Symbol;
const Vector = objects.Vector;
const Keyword = objects.Keyword;
const Class = objects.Class;
const GenericFunction = objects.GenericFunction;

fn lookupClassMetadata(heap: *Heap, class_val: Value) ?[]const Value {
    return heap.class_metadata.get(class_val);
}

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
    defer slot_values.deinit(heap.backing_allocator);

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

        try slot_values.append(heap.backing_allocator, val);
        rest = val_cons.cdr;
    }

    // Create vector: #('class-name slot1-val slot2-val ...)
    const vec_size = 1 + slot_values.items.len;
    const vec_val = try heap.allocVector(vec_size, vec_size);
    const vec = vec_val.toPtr(Vector);
    vec.data[0] = class_name;
    for (slot_values.items, 0..) |val, i| {
        vec.data[1 + i] = val;
    }

    // Call initialize-instance if available
    // For now, just return the instance - initialize-instance hook will be added via generic functions
    return vec_val;
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
    const slot_name = slot_name_val;

    // Instance format: #(class-name slot1-val slot2-val ...)
    const vec = obj.toPtr(Vector);
    if (vec.length == 0) return error.InvalidArgument;

    const class_name_val = vec.data[0];
    if (!class_name_val.isSymbol()) return error.InvalidArgument;
    const slot_names = if (lookupClassMetadata(heap, class_name_val)) |names| names else return error.InvalidArgument;

    for (slot_names, 0..) |name, idx| {
        if (name.eq(slot_name)) {
            // Slot index in vector is idx+1 (since data[0] is class name)
            const vec_idx = idx + 1;
            if (vec_idx >= vec.length) return error.InvalidArgument;
            const val = vec.data[vec_idx];
            if (val.isUnbound()) return error.UnboundSlot;
            return val;
        }
    }

    // Slot not found
    return error.InvalidArgument;
}

/// (setf (slot-value obj 'slot) value)
/// Set a slot value by name
pub fn setSlotValue(heap: *Heap, args: Value) !Value {
    // Parse: (obj 'slot-name value)
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
    const slot_name = slot_name_val;

    // Get the value to set
    if (!cons2.cdr.isCons()) return error.InvalidArgument;
    const cons3 = cons2.cdr.toPtr(Cons);
    const new_value = cons3.car;

    // Instance format: #(class-name slot1-val slot2-val ...)
    const vec = obj.toPtr(Vector);
    if (vec.length == 0) return error.InvalidArgument;

    const class_name_val = vec.data[0];
    if (!class_name_val.isSymbol()) return error.InvalidArgument;
    const slot_names = if (lookupClassMetadata(heap, class_name_val)) |names| names else return error.InvalidArgument;

    for (slot_names, 0..) |name, idx| {
        if (name.eq(slot_name)) {
            // Slot index in vector is idx+1 (since data[0] is class name)
            const vec_idx = idx + 1;
            if (vec_idx >= vec.length) return error.InvalidArgument;
            vec.data[vec_idx] = new_value;
            return new_value;
        }
    }

    // Slot not found
    return error.InvalidArgument;
}

/// class-of: (class-of obj)
/// Return the class of an object
pub fn classOf(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    // Class objects: return their metaclass
    if (obj.isClass()) {
        return obj.toPtr(Class).metaclass;
    }

    // CLOS objects
    if (obj.isGenericFunction()) {
        const type_sym = try heap.intern("generic-function");
        return heap.findLispClass(type_sym) orelse error.InvalidArgument;
    }
    if (obj.isMethod()) {
        const type_sym = try heap.intern("method");
        return heap.findLispClass(type_sym) orelse error.InvalidArgument;
    }
    if (obj.isSlotDefinition()) {
        const type_sym = try heap.intern("slot-definition");
        return heap.findLispClass(type_sym) orelse error.InvalidArgument;
    }

    // Built-in types: lookup class from registry
    const type_name = switch (obj.typeKind()) {
        .nil, .t, .unbound => "symbol",
        .fixnum => "fixnum",
        .float => "float",
        .char => "character",
        .symbol => "symbol",
        .cons => "cons",
        .keyword => "keyword",
        .string, .string32 => "string",
        .vector => blk: {
            // Check if it's a CLOS instance (has class name symbol in slot 0)
            const vec = obj.toPtr(Vector);
            if (vec.length > 0 and vec.data[0].isSymbol()) {
                // Instance format: #(class-name slot1-val ...)
                const class_name_sym = vec.data[0];
                const class_val = heap.findLispClass(class_name_sym);
                if (class_val) |cls| return cls;
            }
            break :blk "vector";
        },
        .closure => "closure",
        .hashtable => "hash-table",
        .rational => "rational",
        .complex => "complex",
        .stream => "stream",
        .bignum => "bignum",
        .array => "array",
        .pathname => "pathname",
        .class => unreachable, // handled above
        .package => "package",
        .chunk => "chunk",
        .condition => "condition",
        .slotdef => "slot-definition",
        .generic_function => unreachable, // handled above
        .method => unreachable, // handled above
    };

    const type_sym = if ((try heap.internInPackage("CL", type_name))) |val| val else return error.InvalidArgument;
    return heap.findLispClass(type_sym) orelse error.InvalidArgument;
}

/// slot-exists-p: (slot-exists-p obj 'slot-name)
/// Check if a slot exists in an object
pub fn slotExistsP(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    if (!obj.isVector()) return Value.nil;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    var slot_name_val = cons2.car;

    if (slot_name_val.isCons()) {
        const quote_cons = slot_name_val.toPtr(Cons);
        if (!quote_cons.cdr.isCons()) return error.InvalidArgument;
        const cdr_cons = quote_cons.cdr.toPtr(Cons);
        slot_name_val = cdr_cons.car;
    }

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;
    const slot_name = slot_name_val;

    const vec = obj.toPtr(Vector);
    if (vec.length == 0) return Value.nil;

    const class_name_val = vec.data[0];
    if (!class_name_val.isSymbol()) return Value.nil;
    const slot_names = lookupClassMetadata(heap, class_name_val) orelse return Value.nil;

    for (slot_names) |name| {
        if (name.eq(slot_name)) {
            return Value.t;
        }
    }

    return Value.nil;
}

/// slot-boundp: (slot-boundp obj 'slot-name)
/// Check if slot is bound (not unbound)
pub fn slotBoundp(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    if (!obj.isVector()) return error.InvalidArgument;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    var slot_name_val = cons2.car;

    if (slot_name_val.isCons()) {
        const quote_cons = slot_name_val.toPtr(Cons);
        if (!quote_cons.cdr.isCons()) return error.InvalidArgument;
        const cdr_cons = quote_cons.cdr.toPtr(Cons);
        slot_name_val = cdr_cons.car;
    }

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;
    const slot_name = slot_name_val;

    const vec = obj.toPtr(Vector);
    if (vec.length == 0) return error.InvalidArgument;

    const class_name_val = vec.data[0];
    if (!class_name_val.isSymbol()) return error.InvalidArgument;
    const slot_names = if (lookupClassMetadata(heap, class_name_val)) |names| names else return error.InvalidArgument;

    for (slot_names, 0..) |name, idx| {
        if (name.eq(slot_name)) {
            const vec_idx = idx + 1;
            if (vec_idx >= vec.length) return error.InvalidArgument;
            const val = vec.data[vec_idx];
            if (val.isUnbound()) {
                return Value.nil;
            } else {
                return Value.t;
            }
        }
    }

    return error.InvalidArgument;
}

/// slot-makunbound: (slot-makunbound obj 'slot-name)
/// Mark slot as unbound
pub fn slotMakunbound(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    if (!obj.isVector()) return error.InvalidArgument;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    var slot_name_val = cons2.car;

    if (slot_name_val.isCons()) {
        const quote_cons = slot_name_val.toPtr(Cons);
        if (!quote_cons.cdr.isCons()) return error.InvalidArgument;
        const cdr_cons = quote_cons.cdr.toPtr(Cons);
        slot_name_val = cdr_cons.car;
    }

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;
    const slot_name = slot_name_val;

    const vec = obj.toPtr(Vector);
    if (vec.length == 0) return error.InvalidArgument;

    const class_name_val = vec.data[0];
    if (!class_name_val.isSymbol()) return error.InvalidArgument;
    const slot_names = if (lookupClassMetadata(heap, class_name_val)) |names| names else return error.InvalidArgument;

    for (slot_names, 0..) |name, idx| {
        if (name.eq(slot_name)) {
            const vec_idx = idx + 1;
            if (vec_idx >= vec.length) return error.InvalidArgument;
            vec.data[vec_idx] = Value.unbound;
            return obj;
        }
    }

    return error.InvalidArgument;
}

/// call-next-method: (call-next-method &rest args)
/// Stub implementation - will be used for method combinations
pub fn callNextMethod(heap: *Heap, args: Value) !Value {
    _ = heap;
    _ = args;
    return error.NotImplemented;
}

/// classp: (classp obj)
/// Check if object is a Class
pub fn classp(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const obj = cons.car;
    return if (obj.isClass()) Value.t else Value.nil;
}

/// find-class: (find-class name)
/// Look up a class by name symbol in the global registry
pub fn findClass(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const c = args.toPtr(Cons);
    const name = c.car;
    if (!name.isSymbol()) return error.TypeError;

    if (heap.lisp_classes.raw == Value.nil.raw) return Value.nil;
    const ht = heap.lisp_classes.toPtr(objects.HashTable);

    const hash = name.raw;
    var idx = hash % ht.capacity;
    var i: usize = 0;
    while (i < ht.capacity) : (i += 1) {
        const e = &ht.entries[idx];
        if (e.key.raw == objects.HashTable.EMPTY.raw) break;
        if (e.key.raw == name.raw) return e.value;
        idx = (idx + 1) % ht.capacity;
    }
    return Value.nil;
}

/// class-name: (class-name class)
/// Return the name of a class
pub fn className(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const class_val = cons.car;
    if (!class_val.isClass()) return error.TypeError;
    const class = class_val.toPtr(runtime.Class);
    return class.name;
}

/// class-direct-superclasses: (class-direct-superclasses class)
/// Return the list of direct superclasses
pub fn classDirectSuperclasses(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const class_val = cons.car;
    if (!class_val.isClass()) return error.TypeError;
    const class = class_val.toPtr(runtime.Class);
    return class.direct_supers;
}

/// class-precedence-list: (class-precedence-list class)
/// Return the class precedence list (linearized superclasses)
pub fn classPrecedenceList(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const class_val = cons.car;
    if (!class_val.isClass()) return error.TypeError;
    const class = class_val.toPtr(runtime.Class);
    return class.cpl;
}

/// class-direct-slots: (class-direct-slots class)
/// Return the list of direct slot definitions
pub fn classDirectSlots(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const class_val = cons.car;
    if (!class_val.isClass()) return error.TypeError;
    const class = class_val.toPtr(runtime.Class);
    return class.direct_slots;
}

/// class-slots: (class-slots class)
/// Return the list of all slot definitions (direct + inherited)
pub fn classSlots(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const class_val = cons.car;
    if (!class_val.isClass()) return error.TypeError;
    const class = class_val.toPtr(runtime.Class);
    return class.slots;
}

/// slot-definition-name: (slot-definition-name slot-def)
/// Return the name of a slot definition
pub fn slotDefinitionName(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const slot_def = cons.car;

    if (!slot_def.isSlotDefinition()) return error.TypeError;
    const sd = slot_def.toPtr(objects.SlotDefinition);
    return sd.name;
}

/// slot-definition-initform: (slot-definition-initform slot-def)
/// Return the initform of a slot definition
pub fn slotDefinitionInitform(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const slot_def = cons.car;

    if (!slot_def.isSlotDefinition()) return error.TypeError;
    const sd = slot_def.toPtr(objects.SlotDefinition);
    return sd.initform;
}

/// slot-definition-initargs: (slot-definition-initargs slot-def)
/// Return the initargs of a slot definition
pub fn slotDefinitionInitargs(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const slot_def = cons.car;

    if (!slot_def.isSlotDefinition()) return error.TypeError;
    const sd = slot_def.toPtr(objects.SlotDefinition);
    return sd.initargs;
}

/// slot-definition-readers: (slot-definition-readers slot-def)
/// Return the readers of a slot definition
pub fn slotDefinitionReaders(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const slot_def = cons.car;

    if (!slot_def.isSlotDefinition()) return error.TypeError;
    const sd = slot_def.toPtr(objects.SlotDefinition);
    return sd.readers;
}

/// slot-definition-writers: (slot-definition-writers slot-def)
/// Return the writers of a slot definition
pub fn slotDefinitionWriters(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const slot_def = cons.car;

    if (!slot_def.isSlotDefinition()) return error.TypeError;
    const sd = slot_def.toPtr(objects.SlotDefinition);
    return sd.writers;
}

/// slot-definition-allocation: (slot-definition-allocation slot-def)
/// Return the allocation type of a slot definition
pub fn slotDefinitionAllocation(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const slot_def = cons.car;

    if (!slot_def.isSlotDefinition()) return error.TypeError;
    const sd = slot_def.toPtr(objects.SlotDefinition);
    return sd.allocation;
}

/// slot-definition-type: (slot-definition-type slot-def)
/// Return the type specifier of a slot definition
pub fn slotDefinitionType(heap: *Heap, args: Value) !Value {
    _ = heap;
    if (!args.isCons()) return error.InvalidArgument;
    const cons = args.toPtr(Cons);
    const slot_def = cons.car;

    if (!slot_def.isSlotDefinition()) return error.TypeError;
    const sd = slot_def.toPtr(objects.SlotDefinition);
    return sd.slot_type;
}

/// %make-generic-function: (%make-generic-function name lambda-list)
/// Internal primitive to allocate a GenericFunction object
pub fn makeGenericFunction(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const name = cons1.car;

    if (!name.isSymbol()) return error.InvalidArgument;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const lambda_list = cons2.car;

    const gf = try heap.alloc(GenericFunction);
    gf.kind = .generic_function;
    gf.name = name;
    gf.lambda_list = lambda_list;
    gf.methods = Value.nil;
    gf.dispatcher = Value.nil;

    return Value.makeGenericFunction(gf);
}

/// %make-unbound: () -> unbound
/// Returns the unbound slot marker
pub fn makeUnbound(_: *Heap, args: Value) !Value {
    if (!args.isNil()) return error.InvalidArgument;
    return Value.unbound;
}

/// %make-method: (%make-method qualifiers specializers lambda-list function)
/// Internal primitive to allocate a Method object
pub fn makeMethod(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const qualifiers = cons1.car;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const specializers = cons2.car;

    if (!cons2.cdr.isCons()) return error.InvalidArgument;
    const cons3 = cons2.cdr.toPtr(Cons);
    const lambda_list = cons3.car;

    if (!cons3.cdr.isCons()) return error.InvalidArgument;
    const cons4 = cons3.cdr.toPtr(Cons);
    const function = cons4.car;

    const method = try heap.alloc(objects.Method);
    method.kind = .method;
    method.qualifiers = qualifiers;
    method.specializers = specializers;
    method.lambda_list = lambda_list;
    method.function = function;

    return Value.makeMethod(method);
}

/// %add-method: (%add-method generic-function method)
/// Add method to generic function's methods list
pub fn addMethod(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const gf_val = cons1.car;

    if (!gf_val.isGenericFunction()) return error.InvalidArgument;
    const gf = gf_val.toPtr(GenericFunction);

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const method_val = cons2.car;

    if (!method_val.isMethod()) return error.InvalidArgument;

    gf.methods = try heap.allocCons(method_val, gf.methods);
    return gf_val;
}

/// generic-function-name: (generic-function-name gf)
/// Return the name of a generic function
pub fn genericFunctionName(_: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const gf_val = cons1.car;

    if (!gf_val.isGenericFunction()) return error.InvalidArgument;
    const gf = gf_val.toPtr(GenericFunction);

    return gf.name;
}

/// generic-function-methods: (generic-function-methods gf)
/// Return the list of methods for a generic function
pub fn genericFunctionMethods(_: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const gf_val = cons1.car;

    if (!gf_val.isGenericFunction()) return error.InvalidArgument;
    const gf = gf_val.toPtr(GenericFunction);

    return gf.methods;
}

/// generic-function-lambda-list: (generic-function-lambda-list gf)
/// Return the lambda list of a generic function
pub fn genericFunctionLambdaList(_: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const gf_val = cons1.car;

    if (!gf_val.isGenericFunction()) return error.InvalidArgument;
    const gf = gf_val.toPtr(GenericFunction);

    return gf.lambda_list;
}

/// method-qualifiers: (method-qualifiers method)
/// Return the qualifiers list of a method
pub fn methodQualifiers(_: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const method_val = cons1.car;

    if (!method_val.isMethod()) return error.InvalidArgument;
    const method = method_val.toPtr(objects.Method);

    return method.qualifiers;
}

/// method-specializers: (method-specializers method)
/// Return the specializers list of a method
pub fn methodSpecializers(_: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const method_val = cons1.car;

    if (!method_val.isMethod()) return error.InvalidArgument;
    const method = method_val.toPtr(objects.Method);

    return method.specializers;
}

/// method-function: (method-function method)
/// Return the function (closure) of a method
pub fn methodFunction(_: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const method_val = cons1.car;

    if (!method_val.isMethod()) return error.InvalidArgument;
    const method = method_val.toPtr(objects.Method);

    return method.function;
}

// ============================================================================
// Tests
// ============================================================================

test "classp returns t for class and nil for non-class" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const class_args = try heap.allocCons(heap.standard_class, Value.nil);
    const class_res = try classp(&heap, class_args);
    try testing.expect(class_res.isT());

    const non_class_args = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const non_class_res = try classp(&heap, non_class_args);
    try testing.expect(non_class_res.isNil());
}

test "slot-value uses symbol metadata" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const class_sym = try heap.intern("FOO-CLASS");
    const slot_sym = try heap.intern("BAR");

    const slot_names = try heap.backing_allocator.alloc(Value, 1);
    slot_names[0] = slot_sym;
    try heap.class_metadata.put(heap.backing_allocator, class_sym, slot_names);

    const vec = try heap.allocVector(2, 2);
    const vec_obj = vec.toPtr(Vector);
    vec_obj.data[0] = class_sym;
    vec_obj.data[1] = Value.makeFixnum(42);

    const args_tail = try heap.allocCons(slot_sym, Value.nil);
    const args = try heap.allocCons(vec, args_tail);
    const res = try slotValue(&heap, args);
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, 42), res.toFixnum());
}

test "slot-value errors without class metadata" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const class_sym = try heap.intern("NO-CLASS");
    const slot_sym = try heap.intern("SLOT");

    const vec = try heap.allocVector(2, 2);
    const vec_obj = vec.toPtr(Vector);
    vec_obj.data[0] = class_sym;
    vec_obj.data[1] = Value.makeFixnum(1);

    const args_tail = try heap.allocCons(slot_sym, Value.nil);
    const args = try heap.allocCons(vec, args_tail);
    try testing.expectError(error.InvalidArgument, slotValue(&heap, args));
}

test "slot-exists-p returns nil for non-vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const slot_sym = try heap.intern("SLOT");
    const args_tail = try heap.allocCons(slot_sym, Value.nil);
    const args = try heap.allocCons(Value.makeFixnum(1), args_tail);
    const res = try slotExistsP(&heap, args);
    try testing.expect(res.isNil());
}
