//! Habu Runtime
//!
//! Core runtime components:
//! - Tagged values (64-bit with 1+3 bit hybrid tagging)
//! - Object layouts (cons, symbol, vector, string, closure, keyword)
//! - Heap management (bump allocation with semispace)
//! - Garbage collection (Cheney copying collector)

pub const value = @import("value.zig");
pub const objects = @import("objects.zig");
pub const heap = @import("heap.zig");
pub const gc = @import("gc.zig");
pub const roots = @import("roots.zig");
pub const interner = @import("interner.zig");
pub const primitives = @import("primitives/primitives.zig");

// Re-export commonly used types
pub const Value = value.Value;
pub const Tag = value.Tag;
pub const TypeKind = value.TypeKind;

pub const Cons = objects.Cons;
pub const Symbol = objects.Symbol;
pub const Vector = objects.Vector;
pub const String = objects.String;
pub const String32 = objects.String32;
pub const Closure = objects.Closure;
pub const Keyword = objects.Keyword;
pub const HashTable = objects.HashTable;
pub const HashEntry = objects.HashEntry;
pub const HashTest = objects.HashTest;
pub const Rational = objects.Rational;
pub const Complex = objects.Complex;
pub const Stream = objects.Stream;
pub const StreamDirection = objects.StreamDirection;
pub const StreamType = objects.StreamType;
pub const Bignum = objects.Bignum;
pub const Array = objects.Array;
pub const Pathname = objects.Pathname;
pub const Package = objects.Package;
pub const Readtable = objects.Readtable;
pub const ReadtableCase = objects.ReadtableCase;
pub const MacroEnv = objects.MacroEnv;
pub const Chunk = objects.Chunk;
pub const NativeCode = objects.NativeCode;
pub const BoxedKind = objects.BoxedKind;
pub const Class = objects.Class;

pub const Heap = heap.Heap;
pub const UpperName = heap.UpperName;
pub const upperNameAlloc = heap.upperNameAlloc;
pub const freeUpperName = heap.freeUpperName;
pub const GC = gc.GC;

var g_heap_context: ?*Heap = null;
var g_symbol_value_resolver: ?*const fn (Value, *anyopaque) anyerror!?Value = null;
var g_symbol_value_resolver_ctx: ?*anyopaque = null;
var g_symbol_value_setter: ?*const fn (Value, Value, *anyopaque) anyerror!void = null;
var g_symbol_value_setter_ctx: ?*anyopaque = null;

pub const SymbolValueResolverState = struct {
    cb: ?*const fn (Value, *anyopaque) anyerror!?Value,
    ctx: ?*anyopaque,
};

pub const SymbolValueSetterState = struct {
    cb: ?*const fn (Value, Value, *anyopaque) anyerror!void,
    ctx: ?*anyopaque,
};

pub fn setHeapContext(h: *Heap) void {
    g_heap_context = h;
}

pub fn heapContext() ?*Heap {
    return g_heap_context;
}

pub fn setSymbolValueResolver(
    cb: ?*const fn (Value, *anyopaque) anyerror!?Value,
    ctx: ?*anyopaque,
) SymbolValueResolverState {
    const prev = SymbolValueResolverState{
        .cb = g_symbol_value_resolver,
        .ctx = g_symbol_value_resolver_ctx,
    };
    g_symbol_value_resolver = cb;
    g_symbol_value_resolver_ctx = ctx;
    return prev;
}

pub fn restoreSymbolValueResolver(state: SymbolValueResolverState) void {
    g_symbol_value_resolver = state.cb;
    g_symbol_value_resolver_ctx = state.ctx;
}

pub fn setSymbolValueSetter(
    cb: ?*const fn (Value, Value, *anyopaque) anyerror!void,
    ctx: ?*anyopaque,
) SymbolValueSetterState {
    const prev = SymbolValueSetterState{
        .cb = g_symbol_value_setter,
        .ctx = g_symbol_value_setter_ctx,
    };
    g_symbol_value_setter = cb;
    g_symbol_value_setter_ctx = ctx;
    return prev;
}

pub fn restoreSymbolValueSetter(state: SymbolValueSetterState) void {
    g_symbol_value_setter = state.cb;
    g_symbol_value_setter_ctx = state.ctx;
}

pub fn lookupSymbolValue(sym: Value) anyerror!?Value {
    const cb = g_symbol_value_resolver orelse return null;
    const ctx = g_symbol_value_resolver_ctx orelse return null;
    return try cb(sym, ctx);
}

pub fn storeSymbolValue(sym: Value, val: Value) anyerror!bool {
    const cb = g_symbol_value_setter orelse return false;
    const ctx = g_symbol_value_setter_ctx orelse return false;
    try cb(sym, val, ctx);
    return true;
}
pub const Interner = interner.Interner;
pub const RootRange = roots.RootRange;
pub const RootSet = roots.RootSet;

pub const clos = primitives.clos;

test {
    _ = value;
    _ = objects;
    _ = heap;
    _ = gc;
    _ = roots;
    _ = interner;
    _ = primitives;
}
