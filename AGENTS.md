# Habu Lisp Compiler - Agent Instructions

## Project Vision

**Ultimate Goal**: A fully self-hosting Common Lisp compiler that:
- Generates native ARM64 code (x86_64 planned)
- Matches or exceeds SBCL performance on ARM64
- Implements full Common Lisp specification
- Requires no external Lisp system after bootstrap

**Roadmap**:
1. Self-hosting (Stage 1 -> Stage 2 -> Stage 3 fixed point)
2. SBCL independence (native eval, reader conditionals)
3. Performance parity (register allocator, optimizations)
4. Full CL spec (CLOS, conditions, packages, multiple values)

## Project Overview

Habu is a self-hosted Lisp compiler that generates native ARM64 machine code.

**Key Characteristics:**
- Native code generation (no bytecode interpreter)
- Minimal C runtime (only for bootstrapping)
- Full Common Lisp specification as the goal
- ARM64 first, x86_64 second
- Bootstrapped via SBCL

## Architecture

### Components

1. **Bootstrap Compiler** (`bootstrap/compiler.lisp`)
   - Compiles Lisp to IR
   - Runs in SBCL during bootstrap phase
   - Functions in HABU package (public API) and SYS package (internal)

2. **ARM64 Assembler** (`arm64/asm.lisp`)
   - Pure ARM64 instruction encoding
   - `:arm64` package with clean API
   - No external dependencies

3. **ARM64 Codegen** (`arm64/codegen-sbcl.lisp`)
   - IR to ARM64 machine code
   - SBCL-specific helpers for bootstrap

4. **Mach-O Linker** (`macho-linker.lisp`)
   - Generates standalone macOS executables
   - Chained fixups for dynamic linking

5. **Standalone Interpreter** (`habu0.lisp`)
   - Self-contained Lisp interpreter
   - Runs without SBCL
   - Entry point for self-hosting

6. **C Runtime** (`runtime/`)
   - Garbage collector
   - Basic I/O operations
   - Only used during initial bootstrap

### File Organization

```
habu/
  bootstrap/       - SBCL bootstrap compiler
  arm64/           - ARM64 instruction encoding and codegen
  runtime/         - Minimal C runtime
  common/          - Shared Lisp utilities
  tests/           - Test suite
  docs/            - Documentation
  bin/             - CLI tools
```

## Development Guidelines

### Problem Solving

- **Ultrathink for significant efforts**: When facing complex tasks, multi-step implementations, or difficult bugs, use extended thinking to plan thoroughly before acting
- **Take no shortcuts**: Always identify the root cause of bugs
- Investigate systematically, don't patch symptoms
- Understand WHY something fails before attempting fixes
- Add tests that reproduce the bug before fixing it

### Session Management

1. **CONTEXT.md** - The primary context file for session progress
   - This is THE ONLY file to update with session progress and current state
   - Current development phase and active tasks
   - Recent changes and bug fixes (this session)
   - Known issues and workarounds
   - Test status and results
   - Update after each major step or milestone
   - DO NOT use SESSION.md (historical log only)
   - DO NOT create SESSION_SUMMARY.md (use CONTEXT.md instead)

2. **SESSION.md** - Historical log only
   - Large append-only log file (27MB+)
   - Contains past session transcripts
   - DO NOT write session summaries here
   - Only append if explicitly instructed

3. **Commits** - One logical feature per commit:
   - Include tests with implementation
   - Short, descriptive summary
   - No separate "fixed this" commits

### Code Style

- **Hex numbers**: Use `#x` prefix for addresses, offsets, constants
- **No emojis**: Never in code, commits, or docs
- **No marketing language**: Use technical facts
- **Tests**: Minimal, purpose comment at top, no color output
- **Warnings as errors**: ASDF treats all warnings as errors. Fix warnings immediately.
- **No inline loads**: Use ASDF for dependencies, never `(load ...)` in source files

### Code Generation Policy

When adding new ARM64 instructions:

1. Add intrinsics to `arm64/asm.lisp` in `:arm64` package
2. Use existing ARM64 intrinsics wherever possible
3. Prefer direct ARM64 calls over wrapping in helper functions

**IMPORTANT - No Wrapper Functions in codegen.lisp**:
- DO NOT add new `#+sbcl` / `#-sbcl` wrapper functions in `codegen.lisp`
- Instead, add reader conditionals directly in `arm64/asm.lisp` intrinsics
- The `arm64/asm.lisp` functions use keyword args (`:imm t`, `:reg t`, `:offset N`)
- Put the `#-sbcl` native encoding directly in the intrinsic function
- This keeps all ARM64 encoding in one place and reduces codegen.lisp complexity

### Debugging

- Use Lisp `trace` facility for debugging
- Check CONTEXT.md for known issues
- Use lldb with function symbols (now embedded in binaries)
- Common exit codes:
  - 139 = SIGSEGV (memory access error)
  - 137 = SIGKILL (often codesign issue on macOS)

### Slash Commands

Available commands for Habu development (use `/command-name`):

**Build & Test:**
1. **`/habu-build-test`** - Compile and test workflow
   - Compiles source, runs binary, reports results
   - Automatically runs error analysis on failures

2. **`/habu-run-tests [pattern]`** - Run test suite
   - Runs all tests or matches pattern
   - Reports PASS/FAIL with semantic context

3. **`/habu-stage <N|verify>`** - Self-compilation stages
   - Builds Stage 1/2/3 compilation
   - Verifies fixed-point achievement

**Debugging:**
4. **`/habu-debug <binary>`** - Debug crashes
   - Loads binary in lldb with SIGSEGV handling
   - Shows crash location with function context

5. **`/habu-analyze <error>`** - Structured error analysis
   - Forms hypotheses before attempting fixes
   - Tests each hypothesis systematically

6. **`/habu-disasm <binary> [function]`** - Disassemble binaries
   - Lists all functions with addresses
   - Shows ARM64 instructions with annotations

**Inspection:**
7. **`/habu-ir <source>`** - Inspect compiler IR
   - Shows intermediate representation
   - Displays defun definitions and main IR
   - Useful for debugging compilation issues

8. **`/habu-compare <bin1> <bin2>`** - Compare binaries
   - Byte-by-byte comparison
   - Shows differences with context
   - Used for fixed-point verification

9. **`/habu-hexdump <binary> [range]`** - Hex dump with annotations
   - Section-aware hex dump
   - Shows Mach-O structure
   - Function boundaries marked

10. **`/habu-profile <binary> [duration]`** - Profile running binary
    - Uses macOS `sample` tool to identify hot functions
    - Maps sample addresses to function names via nm
    - Reports top functions by sample count
    - Binary must have embedded symbols (deliver-v3)

**System:**
11. **`/habu-load`** - Load compiler via ASDF
    - Loads all bootstrap files with correct dependencies
    - Handles compilation order automatically
    - Use for interactive REPL development

### Symbol Table Support

Habu binaries now include function symbols in LC_SYMTAB:
- `nm <binary>` shows all functions
- `lldb -o "disassemble -n FUNCTION"` works directly
- Backtraces show function names instead of addresses

## TAC (Three-Address Code) Pipeline

The register allocator uses a nanopass architecture in `bootstrap/reg-alloc.lisp`:

### Passes

1. **ir-to-tac** - Tree IR to linear TAC
   - Input: `(add (var 0) (mul (lit 2) (var 1)))`
   - Output: `((tac-var v0 0) (tac-lit v1 2) (tac-var v2 1) (tac-binop v3 mul v1 v2) (tac-binop v4 add v0 v3))`

2. **compute-liveness** - Backward dataflow analysis
   - Computes live-in/live-out sets for each instruction

3. **compute-intervals** - Live intervals
   - Output: `((vreg start end) ...)`

4. **linear-scan** - Register allocation
   - Allocates x9-x15 (7 registers)
   - Spills to stack when exhausted

5. **tac-codegen** - TAC + allocation to ARM64 (TODO)

### TAC Instruction Formats

```lisp
(tac-lit vreg value)           ; vreg = literal
(tac-var vreg offset)          ; vreg = env[offset]
(tac-setvar offset vreg)       ; env[offset] = vreg
(tac-binop vreg op vr1 vr2)    ; vreg = vr1 op vr2
(tac-call vreg fn args)        ; vreg = fn(args...)
(tac-if vreg then-lbl else-lbl); conditional branch
(tac-label name)               ; label
(tac-goto label)               ; unconditional jump
(tac-return vreg)              ; return value
(tac-move vreg1 vreg2)         ; vreg1 = vreg2
```

### ARM64 Package

All ARM64 instructions use the `arm64` package (`arm64/asm.lisp`):

```lisp
(arm64:add rd rn rm)              ; register
(arm64:add rd rn imm :imm t)      ; immediate
(arm64:ldr rt rn :offset off)     ; load with offset
(arm64:str rt rn :offset off)     ; store with offset
(arm64:ldrb rt rn off)            ; load byte immediate
(arm64:ldrb rt rn rm :reg t)      ; load byte register
(arm64:cmp rn rm)                 ; compare register
(arm64:cmp rn imm :imm t)         ; compare immediate
(arm64:b.eq offset)               ; branch if equal (instruction count)
```

## Self-Hosting Path

### Current Status

1. **habu0** - Standalone interpreter (working)
   - Reads and parses Lisp
   - Interprets via h0-eval
   - Compiles to IR via h0-compile
   - Generates native code via h0-codegen

2. **Native executables** - Generated programs run without SBCL

3. **Full compiler** - Still needs SBCL for:
   - `defmacro` (uses SBCL `eval` for expanders)
   - Some reader features

### Blockers for Full Self-Hosting

1. Macro expansion at compile time needs native eval
2. Complex nested function calls in certain patterns

## Testing

**IMPORTANT**: Always use ASDF for loading and testing. Never use direct `(load ...)` calls.

### Running Tests

Run the full test suite via ASDF:
```lisp
(asdf:test-system :habu)
```

Or load and run interactively:
```lisp
(asdf:load-system :habu/tests)
(habu-test:run-all-tests)
```

### Test Organization

Tests are defined in `bootstrap/habu.asd` as the `habu/tests` system:
- `bootstrap/test-harness.lisp` - Test utilities (HABU-TEST package)
- `tests/test-core.lisp` - Core compiler tests
- `tests/test-keyword-args.lisp` - Keyword argument tests
- `tests/test-packages.lisp` - Package system tests

### Writing Tests

Use the test harness macros in HABU-TEST package:
```lisp
(in-package :habu-test)

(define-test-suite "Feature Name"
  (test "test-name" "source-code" expected-exit-code)
  (test-full "test-name" "(full source with sys-exit)" expected))
```

### Test Naming

- ASDF test files: `test-feature.lisp` (hyphenated)
- Legacy standalone tests: `test_feature.lisp` (underscored)

### Key Points

1. Never `(load ...)` source files in tests - use ASDF dependencies
2. Tests run when their files are loaded during `asdf:test-system`
3. Use `:depends-on` in ASDF to ensure proper load order
4. Test harness provides counters: `*pass-count*`, `*fail-count*`, `*skip-count*`

## Reference

- Common Lisp HyperSpec: https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm
- Git author: Joel Reymont <18791+joelreymont@users.noreply.github.com>

## Key Patterns in Habu Code

### Compiler Quirks

1. **List expressions with function calls**
   - Pre-compute function calls in `let` bindings before placing in lists
   - Direct calls in `(list (fn arg) ...)` may crash in native code

2. **Tagged values**
   - Fixnums: `value << 4`, tag 0
   - Cons: `pointer | 1`
   - Symbols: `pointer | 2`

3. **Register usage**
   - x20: Environment frame base
   - x28: Heap bump pointer
   - x26: Code base (for native executables)
   - x27: Heap base

## When Stuck

1. Check CONTEXT.md for similar past issues
2. Use `trace` to debug function calls
3. Ask for help - it's allowed and encouraged
