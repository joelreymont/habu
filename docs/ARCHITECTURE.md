# Habu Architecture

Habu is a full Common Lisp implementation written in Zig, targeting 100% compatibility with the ANSI CL specification, plus additional features including gradual typing, contracts, and native code generation via JIT.

## System Overview

```
┌──────────────────────────────────────────────────────────────────┐
│                          Source Code                              │
│                      (Lisp S-expressions)                         │
└─────────────────────────────┬────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────────┐
│                           Reader                                  │
│  src/reader/lexer.zig   - Tokenization                           │
│  src/reader/parser.zig  - S-expression parsing                   │
│  Output: Value (cons tree)                                       │
└─────────────────────────────┬────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────────┐
│                          Compiler                                 │
│  src/compiler/compile.zig - Lisp → IR                            │
│  src/compiler/ir.zig      - IR node types                        │
│  Output: IR tree                                                 │
└─────────────────────────────┬────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────────┐
│                        Bytecode Emitter                           │
│  src/bytecode/emit.zig    - IR → bytecode                        │
│  src/bytecode/opcodes.zig - Opcode definitions                   │
│  Output: Chunk (bytecode + constants)                            │
└─────────────────────────────┬────────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────────┐
│                       Virtual Machine                             │
│  src/interp/vm.zig        - Stack-based execution                │
│  src/interp/repl.zig      - Interactive REPL                     │
│  Output: Value result                                            │
└──────────────────────────────────────────────────────────────────┘
```

## Directory Structure

```
src/
├── main.zig              # Entry point
├── runtime/
│   ├── value.zig         # Tagged 64-bit values
│   ├── objects.zig       # Heap object layouts
│   ├── heap.zig          # Memory allocation
│   ├── gc.zig            # Cheney copying GC
│   └── primitives/       # Built-in operations
├── reader/
│   ├── lexer.zig         # Tokenizer
│   └── parser.zig        # S-expression parser
├── compiler/
│   ├── ir.zig            # IR node definitions
│   └── compile.zig       # Lisp → IR compiler
├── bytecode/
│   ├── opcodes.zig       # Bytecode instruction set
│   ├── emit.zig          # IR → bytecode
│   └── disasm.zig        # Disassembler
├── types/
│   ├── type.zig          # Type ADT
│   ├── check.zig         # Type checker
│   ├── contract.zig      # Runtime contracts
│   └── blame.zig         # Blame tracking
├── interp/
│   ├── vm.zig            # Bytecode interpreter
│   ├── repl.zig          # Interactive REPL
│   └── lineedit.zig      # Line editing
└── jit/
    ├── stencils.zig      # ARM64 code templates
    ├── patch.zig         # Hole patching
    └── jit.zig           # JIT compiler
```

## Value Representation

Habu uses a 64-bit tagged value scheme:

```
┌────────────────────────────────────────────────────────────┐
│ bit 63                                              bit 0  │
├────────────────────────────────────────────────────────────┤
│ Fixnum:  [   63-bit signed integer                   | 1 ] │
│          bit0 = 1 indicates fixnum                         │
├────────────────────────────────────────────────────────────┤
│ Pointer: [   aligned pointer (bits 63-4)   | tag | 0 | 0 ] │
│          bits 3-1 = type tag, bit0 = 0                     │
└────────────────────────────────────────────────────────────┘

Tag values (bits 3-1):
  0 = cons       (0x0)
  1 = symbol     (0x2)
  2 = vector     (0x4)
  3 = string     (0x6)
  4 = closure    (0x8)
  5 = keyword    (0xA)
  7 = forwarding (0xE) - used by GC

Special values:
  nil = 0x0000000000000000 (null pointer)
  t   = 0x0000000000000003 (fixnum 1)
```

See: `src/runtime/value.zig`

## Object Layouts

All heap objects are 16-byte aligned:

```
Cons cell (16 bytes):
┌─────────────┬─────────────┐
│     car     │     cdr     │
│  (8 bytes)  │  (8 bytes)  │
└─────────────┴─────────────┘

Symbol (16 bytes):
┌─────────────┬─────────────┐
│    name     │   (unused)  │
│  (pointer)  │             │
└─────────────┴─────────────┘

String (variable):
┌─────────────┬─────────────────────────┐
│   length    │      characters...      │
│  (8 bytes)  │      (length bytes)     │
└─────────────┴─────────────────────────┘

Vector (variable):
┌─────────────┬─────────────────────────┐
│   length    │      elements...        │
│  (8 bytes)  │   (length × 8 bytes)    │
└─────────────┴─────────────────────────┘

Closure (variable):
┌──────────┬──────────┬────────────────────┐
│  chunk   │ captures │   captured vals... │
│ (pointer)│ (count)  │   (count × 8)      │
└──────────┴──────────┴────────────────────┘
```

See: `src/runtime/objects.zig`

## Memory Management

### Heap Structure

Semispace copying collector with two heaps:
- Active heap: Current allocation space
- Inactive heap: Target for copying GC

```
┌────────────────────────────────┐
│         Active Heap            │
│  [objects...][free space]      │
│              ↑                 │
│           alloc_ptr            │
└────────────────────────────────┘

┌────────────────────────────────┐
│        Inactive Heap           │
│  (empty until GC)              │
└────────────────────────────────┘
```

### Allocation

Bump allocation from `alloc_ptr`:
1. Check if `alloc_ptr + size <= heap_end`
2. If not, trigger GC
3. Bump `alloc_ptr` by size
4. Return old `alloc_ptr`

### Garbage Collection

Cheney copying GC:
1. Swap active/inactive heaps
2. Copy roots (stack, globals) to new heap
3. Scan copied objects, copy their references
4. Update forwarding pointers
5. Clear old heap

See: `src/runtime/heap.zig`, `src/runtime/gc.zig`

## Compiler Pipeline

### 1. Reader

Converts source text to S-expressions:

```
"(+ 1 2)" → Cons(Symbol(+), Cons(Fixnum(1), Cons(Fixnum(2), nil)))
```

Tokens:
- `lparen`, `rparen` - Parentheses
- `quote`, `backquote`, `comma`, `comma_at` - Quote syntax
- `function_quote` - `#'` syntax
- `number`, `float`, `string`, `symbol`, `keyword`, `character`

See: `src/reader/lexer.zig`, `src/reader/parser.zig`

### 2. IR Generation

Compiles S-expressions to IR nodes:

```zig
const Ir = union(enum) {
    // Literals
    lit: Value,
    quote_sym: []const u8,

    // Arithmetic
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,

    // Control flow
    @"if": struct { cond: *Ir, then: *Ir, @"else": *Ir },
    progn: []*Ir,

    // Functions
    lambda: struct { params: [][]const u8, body: *Ir, captures: [][]const u8 },
    call: struct { func: *Ir, args: []*Ir },
    tail_call: struct { func: *Ir, args: []*Ir },

    // Variables
    variable: struct { name: []const u8, depth: u16, index: u16 },
    set: struct { name: []const u8, depth: u16, index: u16, value: *Ir },
    define: struct { name: []const u8, index: u32, value: *Ir },
    global_ref: struct { name: []const u8, index: u32 },

    // ... 50+ node types
};
```

Special forms handled:
- `if`, `cond`, `when`, `unless`
- `let`, `let*`, `letrec`, `flet`, `labels`
- `lambda`, `defun`, `defmacro`
- `and`, `or`, `not`
- `quote`, `quasiquote`, `function`
- `block`, `return-from`, `tagbody`, `go`
- `catch`, `throw`, `unwind-protect`
- `values`, `multiple-value-bind`

See: `src/compiler/ir.zig`, `src/compiler/compile.zig`

### 3. Bytecode Emission

Converts IR to stack-based bytecode:

```
Opcode categories:
  0x00-0x0F: Stack operations (push, pop, dup)
  0x10-0x1F: Arithmetic (add, sub, mul, div)
  0x20-0x2F: Comparison (eq, lt, gt)
  0x30-0x3F: Control flow (jump, branch)
  0x40-0x4F: Functions (call, return, closure)
  0x50-0x5F: Variables (load, store)
  0x60-0x6F: List operations (cons, car, cdr)
  0x70-0x7F: Type predicates (consp, symbolp)
  0x80-0x8F: I/O operations
  0xA0-0xAF: Type checks
```

Chunk structure:
```zig
const Chunk = struct {
    code: []u8,           // Bytecode
    constants: []Value,   // Constant pool
    name: []const u8,     // Function name
    arity: u8,            // Parameter count
    locals: u8,           // Local variable count
    upvalues: u8,         // Captured variable count
};
```

See: `src/bytecode/opcodes.zig`, `src/bytecode/emit.zig`

## Virtual Machine

Stack-based interpreter executing bytecode:

```
┌────────────────────────────────────────┐
│              Value Stack               │
│  [val][val][val][...]                  │
│                    ↑                   │
│                   sp                   │
├────────────────────────────────────────┤
│              Call Stack                │
│  [frame][frame][...]                   │
│               ↑                        │
│              fp                        │
└────────────────────────────────────────┘

Frame structure:
┌─────────────────────────────────────┐
│  return_chunk  │  return_ip  │ base │
└─────────────────────────────────────┘
```

Execution loop:
```zig
while (true) {
    const op = chunk.code[ip];
    ip += 1;
    switch (op) {
        .push_const => push(chunk.constants[readU16()]),
        .add => push(pop() + pop()),
        .call => { /* setup frame, jump */ },
        .ret => { /* restore frame, return */ },
        // ...
    }
}
```

See: `src/interp/vm.zig`

## Type System

Gradual typing with occurrence typing:

### Type Syntax

```lisp
;; Function with typed parameters and return
(defun (add -> fixnum) ((a fixnum) (b fixnum))
  (+ a b))

;; Typed let binding
(let (((x fixnum) 10))
  (+ x 1))
```

### Available Types

Primitives:
- `fixnum`, `float`, `char`
- `cons`, `symbol`, `string`, `vector`, `closure`, `keyword`
- `nil`

Compound:
- `(or T1 T2 ...)` - Union type
- `(list T)` - List of T
- `(-> (T1 T2) R)` - Function type
- `non-nil` - Any non-nil value
- `any` - Top type

### Occurrence Typing

Type refinement in conditionals:

```lisp
(if (consp x)
    ;; x is known to be cons here
    (car x)
    ;; x is known to be non-cons here
    x)
```

See: `src/types/type.zig`, `src/types/check.zig`

## REPL Features

Interactive features:
- Multi-line input (auto-detected)
- Readline-style editing (Ctrl-A/E, arrows, history)
- `,` commands: `,h` help, `,q` quit, `,load` file
- Macro expansion
- Error reporting with source locations

See: `src/interp/repl.zig`, `src/interp/lineedit.zig`

## Standard Library

`lib/stdlib.habu` provides:

- **Control flow**: `when`, `unless`
- **Mutation**: `incf`, `decf`, `push`, `pop`
- **List ops**: `map`, `filter`, `reduce`, `foldl`, `foldr`
- **Predicates**: `null?`, `atom?`, `list?`
- **HOF wrappers**: `plus`, `minus`, `odd?`, `even?`
- **Utilities**: `zip`, `partition`, `take-while`, `drop-while`

## Building

```bash
zig build          # Build executable
zig build test     # Run all tests
./zig-out/bin/habu # Start REPL
```

## JIT Compilation

ARM64 JIT compiler (in progress):
- Stencil-based code generation
- Runtime constant pool loads
- W^X code buffer + icache flush
- Branch patching

Planned:
- Hot loop detection
- Deoptimization / OSR hooks
- Stack maps / GC safepoints
- Calls, locals, globals, closures

See: `src/jit/`, `docs/cranelift-parity.md`
