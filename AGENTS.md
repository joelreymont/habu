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
