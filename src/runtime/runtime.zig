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
pub const primitives = @import("primitives/primitives.zig");

// Re-export commonly used types
pub const Value = value.Value;
pub const Tag = value.Tag;
pub const TypeKind = value.TypeKind;

pub const Cons = objects.Cons;
pub const Symbol = objects.Symbol;
pub const Vector = objects.Vector;
pub const String = objects.String;
pub const Closure = objects.Closure;
pub const Keyword = objects.Keyword;
pub const HashTable = objects.HashTable;
pub const HashEntry = objects.HashEntry;
pub const HashTest = objects.HashTest;

pub const Heap = heap.Heap;
pub const GC = gc.GC;
pub const RootSet = gc.RootSet;

test {
    _ = value;
    _ = objects;
    _ = heap;
    _ = gc;
    _ = primitives;
}
