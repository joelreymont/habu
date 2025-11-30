# Habu Lisp Compiler - Agent Instructions

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

### Code Generation Policy

When adding new ARM64 instructions:

1. Add intrinsics to `arm64/asm.lisp` in `:arm64` package
2. Use existing ARM64 intrinsics wherever possible
3. Prefer direct ARM64 calls over wrapping in helper functions

### Debugging

- Use Lisp `trace` facility for debugging
- Check CONTEXT.md for known issues
- Common exit codes:
  - 139 = SIGSEGV (memory access error)
  - 137 = SIGKILL (often codesign issue on macOS)

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

Run tests with:
```bash
cd tests && sbcl --script run_tests.lisp
```

Test file naming: `test_<feature>.lisp`

Test structure:
```lisp
;;; Test <feature> - short description
(assert (= (some-function) expected-value))
```

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
