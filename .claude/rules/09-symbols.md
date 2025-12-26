# Symbol Interning - MANDATORY

## Core Principle

**ALL string comparisons for language constructs MUST use interned symbols.**

Strings are interned once at parse time. After that, use symbol identity (pointer/Value equality), never string comparison.

## Anti-patterns (NEVER do this)

```zig
// WRONG: String comparison in hot path
if (std.mem.eql(u8, name, "if")) { ... }
if (std.mem.eql(u8, name, "let")) { ... }
if (std.mem.eql(u8, name, "lambda")) { ... }

// WRONG: Chains of string comparisons
if (std.mem.eql(u8, name, "cons")) return ...;
if (std.mem.eql(u8, name, "car")) return ...;
if (std.mem.eql(u8, name, "cdr")) return ...;
```

## Correct Pattern

```zig
// Pre-intern symbols at init
const BuiltinSymbols = struct {
    sym_if: Value,
    sym_let: Value,
    sym_lambda: Value,
    // ...

    pub fn init(heap: *Heap) BuiltinSymbols {
        return .{
            .sym_if = heap.intern("if"),
            .sym_let = heap.intern("let"),
            .sym_lambda = heap.intern("lambda"),
        };
    }
};

// Compare by identity
if (head.eq(builtins.sym_if)) { ... }
if (head.eq(builtins.sym_let)) { ... }

// Or use a HashMap(Value, Handler) for dispatch
const handlers = std.AutoHashMap(Value, *const fn(...) Error!*Ir);
```

## Dispatch Tables

For large sets of primitives, use comptime-generated tables:

```zig
const PrimitiveEntry = struct {
    symbol: Value,
    handler: *const fn(*Compiler, Value, *const Env) Error!*Ir,
};

// Build at init, lookup by symbol identity
fn findPrimitive(self: *Compiler, sym: Value) ?PrimitiveEntry {
    for (self.primitives) |entry| {
        if (sym.eq(entry.symbol)) return entry;
    }
    return null;
}
```

## Benefits

1. **Performance**: Pointer comparison is O(1), string comparison is O(n)
2. **Memory**: Single interned copy per unique symbol
3. **Correctness**: No typos in string literals scattered through code
4. **Extensibility**: Easy to add new symbols to central table
