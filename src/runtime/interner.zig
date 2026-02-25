//! Symbol Interner
//!
//! Wraps heap symbol interning to provide a clean interface that can be
//! passed to components needing symbol identity (ContractCompiler, TypeBuilder, etc.)

const Heap = @import("heap.zig").Heap;
const Value = @import("value.zig").Value;

/// Symbol interner backed by a heap's symbol table
pub const Interner = struct {
    heap: *Heap,

    pub fn init(heap: *Heap) Interner {
        return .{ .heap = heap };
    }

    /// Intern a string, returning the canonical symbol Value.
    pub fn intern(self: Interner, name: []const u8) error{OutOfMemory}!Value {
        return self.heap.intern(name);
    }
};
