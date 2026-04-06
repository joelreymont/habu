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

fn lookupClassMetadata(heap: *Heap, class_val: Value) !?[]const Value {
    return try heap.lookupClassMetadata(class_val);
}

fn unquoteSymbol(val: Value) !Value {
    if (!val.isCons()) return val;
    const quote_cons = val.toPtr(Cons);
    if (!quote_cons.cdr.isCons()) return error.InvalidArgument;
    return quote_cons.cdr.toPtr(Cons).car;
}

const SlotView = struct {
    class: Value,
    slots: []Value,
};

fn getSlotView(obj: Value) !SlotView {
    if (obj.isStructure()) {
        const st = obj.toPtr(objects.Structure);
        return .{ .class = st.class, .slots = st.slots[0..st.length] };
    }
    if (obj.isVector()) {
        const vec = obj.toPtr(Vector);
        if (vec.length == 0) return error.InvalidArgument;
        const class_name = vec.data[0];
        if (!class_name.isSymbol()) return error.InvalidArgument;
        return .{ .class = class_name, .slots = vec.data[1..vec.length] };
    }
    return error.InvalidArgument;
}

fn findSlotIndex(heap: *Heap, class_val: Value, slot_name: Value) !usize {
    const slot_names = if (try lookupClassMetadata(heap, class_val)) |names| names else return error.InvalidArgument;
    for (slot_names, 0..) |name, idx| {
        if (name.eq(slot_name)) return idx;
    }
    return error.InvalidArgument;
}

/// make-instance: (make-instance 'class-name :slot1 val1 :slot2 val2 ...)
/// Creates an instance of a class using class metadata order.
pub fn makeInstance(heap: *Heap, args: Value) !Value {
    // Parse: ('class-name :slot1 val1 :slot2 val2 ...)
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const class_name_val = cons1.car;

    // Class name should be a quoted symbol or symbol
    const class_name = try unquoteSymbol(class_name_val);

    if (!class_name.isSymbol()) return error.InvalidArgument;
    const class_val = heap.findLispClass(class_name) orelse return error.InvalidArgument;
    const slot_names = if (try lookupClassMetadata(heap, class_val)) |names| names else return error.InvalidArgument;

    const slot_values = try heap.backing_allocator.alloc(Value, slot_names.len);
    defer heap.backing_allocator.free(slot_values);
    for (slot_values) |*slot| slot.* = Value.unbound;

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

        const idx = blk: {
            const kw_name = kw.toPtr(Keyword).getName();
            for (slot_names, 0..) |slot_name, i| {
                if (std.ascii.eqlIgnoreCase(slot_name.getSymbolName(), kw_name)) break :blk i;
            }
            return error.InvalidArgument;
        };
        slot_values[idx] = val;
        rest = val_cons.cdr;
    }

    if (class_val.toPtr(Class).metaclass.eq(heap.structure_class)) {
        const st_val = try heap.allocStructure(class_val, slot_names.len);
        const st = st_val.toPtr(objects.Structure);
        for (slot_values, 0..) |val, i| st.slots[i] = val;
        return st_val;
    }

    const vec_val = try heap.allocVector(1 + slot_names.len, 1 + slot_names.len);
    const vec = vec_val.toPtr(Vector);
    vec.data[0] = class_name;
    for (slot_values, 0..) |val, i| vec.data[1 + i] = val;
    return vec_val;
}

/// slot-value: (slot-value obj 'slot-name)
/// Generic slot accessor that looks up slot by name
pub fn slotValue(heap: *Heap, args: Value) !Value {
    // Parse: (obj 'slot-name)
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const slot_name_val = try unquoteSymbol(cons2.car);

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;
    const view = try getSlotView(obj);
    const idx = try findSlotIndex(heap, view.class, slot_name_val);
    if (idx >= view.slots.len) return error.InvalidArgument;
    const val = view.slots[idx];
    if (val.isUnbound()) return error.UnboundSlot;
    return val;
}

/// (setf (slot-value obj 'slot) value)
/// Set a slot value by name
pub fn setSlotValue(heap: *Heap, args: Value) !Value {
    // Parse: (obj 'slot-name value)
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const slot_name_val = try unquoteSymbol(cons2.car);

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;

    // Get the value to set
    if (!cons2.cdr.isCons()) return error.InvalidArgument;
    const cons3 = cons2.cdr.toPtr(Cons);
    const new_value = cons3.car;

    const view = try getSlotView(obj);
    const idx = try findSlotIndex(heap, view.class, slot_name_val);
    if (idx >= view.slots.len) return error.InvalidArgument;
    view.slots[idx] = new_value;
    heap.writeBarrier(obj, new_value);
    return new_value;
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
        const type_sym = (try heap.internInPackage("CL", "generic-function")) orelse return error.InvalidArgument;
        return heap.findLispClass(type_sym) orelse error.InvalidArgument;
    }
    if (obj.isMethod()) {
        const type_sym = (try heap.internInPackage("CL", "method")) orelse return error.InvalidArgument;
        return heap.findLispClass(type_sym) orelse error.InvalidArgument;
    }
    if (obj.isSlotDefinition()) {
        const type_sym = (try heap.internInPackage("CL", "slot-definition")) orelse return error.InvalidArgument;
        return heap.findLispClass(type_sym) orelse error.InvalidArgument;
    }
    if (obj.isStructure()) {
        return obj.toPtr(objects.Structure).class;
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
        .readtable => "readtable",
        .chunk => "chunk",
        .native_code => "chunk",
        .structure => unreachable,
        .condition => "condition",
        .slotdef => "slot-definition",
        .generic_function => unreachable, // handled above
        .method => unreachable, // handled above
        .macro_env => "t",
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

    if (!obj.isVector() and !obj.isStructure()) return Value.nil;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const slot_name_val = try unquoteSymbol(cons2.car);

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;
    _ = findSlotIndex(heap, (try getSlotView(obj)).class, slot_name_val) catch return Value.nil;
    return Value.t;
}

/// slot-boundp: (slot-boundp obj 'slot-name)
/// Check if slot is bound (not unbound)
pub fn slotBoundp(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    if (!obj.isVector() and !obj.isStructure()) return error.InvalidArgument;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const slot_name_val = try unquoteSymbol(cons2.car);

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;
    const view = try getSlotView(obj);
    const idx = try findSlotIndex(heap, view.class, slot_name_val);
    if (idx >= view.slots.len) return error.InvalidArgument;
    return if (view.slots[idx].isUnbound()) Value.nil else Value.t;
}

/// slot-makunbound: (slot-makunbound obj 'slot-name)
/// Mark slot as unbound
pub fn slotMakunbound(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const obj = cons1.car;

    if (!obj.isVector() and !obj.isStructure()) return error.InvalidArgument;

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const slot_name_val = try unquoteSymbol(cons2.car);

    if (!slot_name_val.isSymbol()) return error.InvalidArgument;
    const view = try getSlotView(obj);
    const idx = try findSlotIndex(heap, view.class, slot_name_val);
    if (idx >= view.slots.len) return error.InvalidArgument;
    view.slots[idx] = Value.unbound;
    heap.writeBarrier(obj, Value.unbound);
    return obj;
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
    switch (name.typeKind()) {
        .symbol, .nil, .t => {},
        else => return error.TypeError,
    }
    return heap.findLispClass(name) orelse Value.nil;
}

/// set-find-class: (%set-find-class name class-or-nil)
/// Update class registry entry and return class-or-nil.
pub fn setFindClass(heap: *Heap, args: Value) !Value {
    if (!args.isCons()) return error.InvalidArgument;
    const c1 = args.toPtr(Cons);
    const name = c1.car;
    switch (name.typeKind()) {
        .symbol, .nil, .t => {},
        else => return error.TypeError,
    }
    if (!c1.cdr.isCons()) return error.InvalidArgument;

    const c2 = c1.cdr.toPtr(Cons);
    const class_or_nil = c2.car;
    if (!c2.cdr.isNil()) return error.InvalidArgument;

    if (class_or_nil.isNil()) {
        _ = heap.removeLispClass(name);
        return Value.nil;
    }

    try heap.putLispClass(name, class_or_nil);
    return class_or_nil;
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

    var valid_name = name.isSymbol();
    if (!valid_name and name.isCons()) {
        const setf_outer = name.toPtr(Cons);
        if (setf_outer.car.isSymbol() and setf_outer.cdr.isCons()) {
            const setf_inner = setf_outer.cdr.toPtr(Cons);
            if (setf_inner.car.isSymbol() and setf_inner.cdr.isNil()) {
                const setf_sym = try heap.intern("setf");
                valid_name = setf_outer.car.eq(setf_sym);
            }
        }
    }
    if (!valid_name) return error.InvalidArgument;

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
    const trace_add_method = std.posix.getenv("HABU_TRACE_ADD_METHOD") != null;
    if (!args.isCons()) return error.InvalidArgument;
    const cons1 = args.toPtr(Cons);
    const gf_val = cons1.car;

    if (!gf_val.isGenericFunction()) {
        if (trace_add_method) {
            std.debug.print("TRACE add-method invalid gf kind={s}\n", .{@tagName(gf_val.typeKind())});
            if (gf_val.isClosure()) {
                const clos = gf_val.toPtr(objects.Closure);
                if (clos.code.isChunk()) {
                    const chunk = clos.code.toPtr(objects.Chunk);
                    if (chunk.name.isSymbol()) {
                        std.debug.print("TRACE add-method invalid gf closure-name={s}\n", .{chunk.name.toPtr(Symbol).getName()});
                    } else if (chunk.name.isCons()) {
                        const outer = chunk.name.toPtr(Cons);
                        if (outer.car.isSymbol() and outer.cdr.isCons()) {
                            const inner = outer.cdr.toPtr(Cons);
                            if (inner.car.isSymbol()) {
                                std.debug.print(
                                    "TRACE add-method invalid gf closure-name=({s} {s})\n",
                                    .{ outer.car.toPtr(Symbol).getName(), inner.car.toPtr(Symbol).getName() },
                                );
                            }
                        }
                    }
                }
            }
        }
        return error.InvalidArgument;
    }
    const gf = gf_val.toPtr(GenericFunction);

    if (!cons1.cdr.isCons()) return error.InvalidArgument;
    const cons2 = cons1.cdr.toPtr(Cons);
    const method_val = cons2.car;

    if (!method_val.isMethod()) {
        if (trace_add_method) {
            std.debug.print("TRACE add-method invalid method kind={s}\n", .{@tagName(method_val.typeKind())});
        }
        return error.InvalidArgument;
    }

    if (trace_add_method) {
        const gf_name = gf.name;
        if (gf_name.isSymbol()) {
            std.debug.print("TRACE add-method gf={s}\n", .{gf_name.toPtr(Symbol).getName()});
        } else if (gf_name.isCons()) {
            const outer = gf_name.toPtr(Cons);
            if (outer.car.isSymbol() and outer.cdr.isCons()) {
                const inner = outer.cdr.toPtr(Cons);
                if (inner.car.isSymbol()) {
                    std.debug.print(
                        "TRACE add-method gf=({s} {s})\n",
                        .{ outer.car.toPtr(Symbol).getName(), inner.car.toPtr(Symbol).getName() },
                    );
                }
            }
        }
    }

    gf.methods = try heap.allocCons(method_val, gf.methods);
    heap.writeBarrier(gf_val, gf.methods);
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
    try heap.setClassMetadataForSymbol(class_sym, slot_names);

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

test "set-slot-value uses symbol metadata for vector-backed objects" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const class_sym = try heap.intern("WRITER-CLASS");
    const slot_sym = try heap.intern("BAR");

    const slot_names = try heap.backing_allocator.alloc(Value, 1);
    slot_names[0] = slot_sym;
    try heap.setClassMetadataForSymbol(class_sym, slot_names);

    const vec = try heap.allocVector(2, 2);
    const vec_obj = vec.toPtr(Vector);
    vec_obj.data[0] = class_sym;
    vec_obj.data[1] = Value.makeFixnum(7);

    const new_val = Value.makeFixnum(99);
    const tail2 = try heap.allocCons(new_val, Value.nil);
    const tail1 = try heap.allocCons(slot_sym, tail2);
    const args = try heap.allocCons(vec, tail1);

    const res = try setSlotValue(&heap, args);
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, 99), res.toFixnum());
    try testing.expectEqual(@as(i64, 99), vec_obj.data[1].toFixnum());
}

test "slot-value accepts boxed structure objects" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const class_sym = try heap.intern("BOXED-STRUCT");
    const class_ptr = try heap.alloc(objects.Class);
    class_ptr.* = .{
        .kind = .class,
        .name = class_sym,
        .direct_supers = Value.nil,
        .cpl = Value.nil,
        .direct_slots = Value.nil,
        .slots = Value.nil,
        .metaclass = heap.structure_class,
        .printer = Value.nil,
        .num_shared = 0,
        .shared_slots = undefined,
    };
    const class_val = Value.makeClass(class_ptr);
    try heap.putLispClass(class_sym, class_val);

    const slot_sym = try heap.intern("BAR");
    const slot_names = try heap.backing_allocator.alloc(Value, 1);
    slot_names[0] = slot_sym;
    try heap.setClassMetadataForSymbol(class_sym, slot_names);

    const st_val = try heap.allocStructure(class_val, 1);
    const st = st_val.toPtr(objects.Structure);
    st.slots[0] = Value.makeFixnum(42);

    const args_tail = try heap.allocCons(slot_sym, Value.nil);
    const args = try heap.allocCons(st_val, args_tail);
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

test "slot-value rejects non-slot object" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const slot_sym = try heap.intern("SLOT");
    const args_tail = try heap.allocCons(slot_sym, Value.nil);
    const args = try heap.allocCons(Value.makeFixnum(1), args_tail);
    try testing.expectError(error.InvalidArgument, slotValue(&heap, args));
}

test "slot-value rejects condition payload object" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const slot_sym = try heap.intern("FORMAT-CONTROL");
    const cond = try heap.allocCondition(try heap.intern("SIMPLE-CONDITION"), Value.nil, Value.nil);
    const args_tail = try heap.allocCons(slot_sym, Value.nil);
    const args = try heap.allocCons(cond, args_tail);
    try testing.expectError(error.InvalidArgument, slotValue(&heap, args));
}
