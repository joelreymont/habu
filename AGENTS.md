# Habu Zig Rewrite - Continuation Instructions

## Current Status (2024-12-25)

Habu is being rewritten from Common Lisp to Zig.

### Completed

**Phase 1: Runtime Foundation** ✓
- `src/runtime/value.zig` - Tagged 64-bit values (1+3 bit hybrid scheme)
- `src/runtime/objects.zig` - Object layouts (Cons, Symbol, Vector, String, Closure, Keyword)
- `src/runtime/heap.zig` - Semispace heap with bump allocation
- `src/runtime/gc.zig` - Cheney copying GC
- `src/runtime/primitives/` - 87 primitive functions (list, arith, string, vector, io)

**Phase 2: Reader** ✓
- `src/reader/lexer.zig` - S-expression tokenizer
- `src/reader/parser.zig` - S-expression parser with symbol interning

**Phase 2: Compiler** ✓
- `src/compiler/ir.zig` - IR with 30+ node types (literals, variables, control flow, functions)
- `src/compiler/compile.zig` - Lisp → IR compilation with occurrence typing support
- `src/bytecode/opcodes.zig` - 50+ opcodes (stack, variables, arithmetic, control, functions)
- `src/bytecode/emit.zig` - IR → bytecode emitter
- `src/bytecode/disasm.zig` - Debug disassembler

**Phase 3: Interpreter** ✓
- `src/interp/vm.zig` - Stack-based bytecode VM
- `src/interp/repl.zig` - Interactive REPL

**Phase 4: Type System** ✓
- `src/types/type.zig` - Type ADT (primitives, or, arrow, list, vec, non-nil, any)
- `src/types/check.zig` - TypeEnv, OccurrenceCtx, TypeChecker
- `src/types/contract.zig` - Contract ADT + ContractCompiler
- `src/types/blame.zig` - Blame tracking for contract errors
- `src/compiler/ir.zig` - assert_* IR nodes for type checks
- `src/bytecode/opcodes.zig` - check_* opcodes (0xA0-0xA6)
- `src/interp/vm.zig` - Runtime type checking (TypeMismatch error)

**Running the REPL:**
```bash
zig build && ./zig-out/bin/habu
```

All tests pass.

**Phase 5: JIT** ✓
- `src/jit/stencils.zig` - ARM64 machine code templates with holes
- `src/jit/patch.zig` - Hole patching for immediates and branches
- `src/jit/jit.zig` - Bytecode → native code compiler

### Next Steps

**Phase 6: Self-Hosting**
- Compile Habu with Habu

## Key Technical Details

### Tagging Scheme
```
bit0=1: fixnum (63-bit, val >> 1)
bit0=0: pointer | tag in bits 1-3
  0: cons, 2: symbol, 4: vector, 6: string, 8: closure, 10: keyword, 14: forwarding
nil = 0 (special symbol)
t = 2 (symbol tag with address 0, special symbol)
```

### Type Dispatch Anti-Pattern

**NEVER chain if-else on type predicates.** Use `switch (val.typeKind())` instead.

```zig
// WRONG: if-else chain on type predicates
if (val.isNil()) {
    ...
} else if (val.isFixnum()) {
    ...
} else if (val.isSymbol()) {
    ...
}

// RIGHT: switch on typeKind
switch (val.typeKind()) {
    .nil => ...,
    .t => ...,
    .fixnum => ...,
    .symbol => ...,
    // exhaustive - compiler catches missing cases
}
```

Benefits:
- Exhaustive: adding new types forces handling everywhere
- Single dispatch point: typeKind() handles all type detection logic
- Faster: one computed jump vs chain of comparisons

### Zig 0.15 Patterns
- See `docs/zig-0.15-api.md` for API reference
- ArrayList is unmanaged: `var list = std.ArrayList(T){};` + pass allocator to methods
- Alignment enum: `alignedAlloc(u8, .@"16", size)`
- I/O: `std.fs.File.stdout()` not `std.io.getStdOut()`

### Import Once, Reference via Namespace
```zig
// WRONG: Multiple imports from same module
const Type = @import("type.zig").Type;
const Primitive = @import("type.zig").Primitive;

// RIGHT: Import module once, use namespace
const types = @import("type.zig");
// Then use: types.Type, types.Primitive
```

### Allocator First
Allocator is ALWAYS the first argument to any function that allocates:
```zig
// RIGHT
pub fn init(allocator: std.mem.Allocator) Self { ... }

// WRONG
pub fn init(config: Config, allocator: std.mem.Allocator) Self { ... }
```

### ArrayList Batch Append

When adding multiple known items to an ArrayList, use a static array + appendSlice:

```zig
// WRONG: Append items one by one
try list.append(allocator, a);
try list.append(allocator, b);
try list.append(allocator, c);

// RIGHT: Create static array, appendSlice once
const items = [_]T{ a, b, c };
try list.appendSlice(allocator, &items);
```

### Error Handling - NEVER MASK ERRORS (BLOCKING REQUIREMENT)

**ALL error-masking patterns are FORBIDDEN:**

```zig
// FORBIDDEN - All of these mask errors:
foo() catch unreachable;           // Crashes instead of propagating
foo() catch return;                // Silently drops error, returns void
foo() catch return null;           // Converts error to null
foo() catch |_| return;            // Same as above, discards error info
foo() orelse unreachable;          // Crashes on null
foo() orelse return error.Foo;     // Replaces actual error with generic one
foo() catch blk: { break :blk default; };  // Swallows error, uses default
```

**The ONLY correct pattern is `try`:**

```zig
// RIGHT - Always propagate errors
const result = try foo();
```

**Functions that call fallible operations MUST return error unions:**

```zig
// WRONG - Can't use try, forces error masking
pub fn process(heap: *Heap) void { ... }
fn simplify(self: *Self) void { ... }

// RIGHT - Allows proper error propagation
pub fn process(heap: *Heap) !void { ... }
fn simplify(self: *Self) !void { ... }
```

**If a function currently returns `void` but needs to call fallible operations, CHANGE IT to return `!void`.** Never work around this by masking errors.

**The only acceptable use of `unreachable`:**
- Switch cases that are logically impossible (e.g., exhaustive enum after filtering)
- Array indices proven in-bounds by prior checks
- Never for "this shouldn't fail" - if it can fail, propagate the error

### Symbol Interning - MANDATORY (BLOCKING REQUIREMENT)

## STOP! READ THIS BEFORE WRITING ANY CODE!

**`std.mem.eql(u8, ...)` for symbol/type name dispatch is FORBIDDEN.**

This is a BLOCKING requirement. If you are about to write string comparisons for:
- Symbol names (if, let, lambda, quote, defun, etc.)
- Type names (fixnum, cons, symbol, string, etc.)
- Special markers (&rest, &optional, &key, _, t, else)
- Keywords (size, test, use, export, etc.)

**STOP IMMEDIATELY** and use symbol identity or a table-driven lookup instead.

### The Rule

1. **Intern strings ONCE at parse time or initialization**
2. **After interning, compare by Value.raw identity, NEVER by string content**
3. **Use switch/case on symbol identity, NEVER if-chains on strings**

```zig
// WRONG - NEVER DO THIS:
if (std.mem.eql(u8, name, "if")) { ... }
if (std.mem.eql(u8, name, "let")) { ... }
if (std.mem.eql(u8, name, "lambda")) { ... }

// WRONG - Even in a chain:
if (std.mem.eql(u8, type_name, "fixnum")) return &t_fixnum;
if (std.mem.eql(u8, type_name, "cons")) return &t_cons;

// RIGHT - Pre-intern symbols:
const BuiltinSymbols = struct {
    sym_if: Value,
    sym_let: Value,
    pub fn init(heap: *Heap) BuiltinSymbols {
        return .{
            .sym_if = heap.intern("if"),
            .sym_let = heap.intern("let"),
        };
    }
};

// RIGHT - Compare by identity:
if (head.eq(builtins.sym_if)) { ... }
if (head.eq(builtins.sym_let)) { ... }

// RIGHT - Table-driven lookup for type names:
const TypeEntry = struct { sym: Value, ty: *const Type };
const type_table: []const TypeEntry = &.{
    .{ .sym = builtins.sym_fixnum, .ty = &t_fixnum },
    .{ .sym = builtins.sym_cons, .ty = &t_cons },
};
for (type_table) |e| if (sym.eq(e.sym)) return e.ty;
```

### Why This Matters

1. **Performance**: Pointer comparison is O(1), string comparison is O(n)
2. **Memory**: Single interned copy per unique symbol
3. **Correctness**: No typos in scattered string literals
4. **Maintainability**: Central symbol table, easy to add new ones
5. **Exhaustiveness**: Switch on enum catches missing cases at compile time

### Exceptions (rare)

String comparison is ONLY acceptable for:
- Parsing literal string content (not symbol dispatch)
- File paths, user input validation
- Debug/error message formatting
- Comparing actual string objects (not their symbolic meaning)

### DRY: Table-Driven Dispatch

Replace repetitive if/switch chains with data tables:

```zig
// WRONG: Repetitive pattern
if (node.* == .consp) { return &t_cons; }
if (node.* == .symbolp) { return &t_symbol; }

// RIGHT: Table-driven
const type_map = [_]struct { tag: Tag, ty: *const Type }{
    .{ .tag = .consp, .ty = &t_cons },
    .{ .tag = .symbolp, .ty = &t_symbol },
};
for (type_map) |e| if (tag == e.tag) return e.ty;
```

### DRY: Extract Common Patterns

When 3+ locations share logic, extract to a function:

```zig
// RIGHT: Single extraction function
fn getPredicateOperand(node: *const Ir) ?*const Ir {
    return switch (node.*) {
        .consp, .symbolp, .numberp => |p| p.operand,
        else => null,
    };
}
```

### Avoid Allocation When Possible

- Use stack arrays for small fixed-size data
- Prefer slices over ArrayList when size is known
- Use comptime for constant data

### Build/Test
```bash
zig build test  # Run all tests
```

## Files to Continue From

Start with `src/compiler/ir.zig` - define the IR representation:
- Literals: `ir-lit`, `ir-nil`, `ir-t`
- Arithmetic: `ir-add`, `ir-sub`, `ir-mul`, `ir-div`
- Comparisons: `ir-eq`, `ir-lt`, `ir-gt`
- Control flow: `ir-if`, `ir-progn`, `ir-while`
- Functions: `ir-call`, `ir-lambda`, `ir-return`
- Variables: `ir-var`, `ir-set`, `ir-let`
- List ops: `ir-cons`, `ir-car`, `ir-cdr`

Then implement `src/compiler/compile.zig` to translate parsed S-expressions to IR.
