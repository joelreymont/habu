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

## Issue Tracking

**Use beads (bd) for ALL work tracking.** Do not use markdown TODOs or TodoWrite for task lists.

```bash
bd ready                    # Show unblocked work
bd create "title" -t task -p 2   # Create issue
bd update <id> --status in_progress  # Claim work
bd close <id>               # Complete work
```

See `bd --help` for full command reference.

## MCP Server

**Always use the Habu MCP server for Lisp evaluation and testing.**

The project has an MCP server configured in `.mcp.json` that provides:
- `lisp_eval` - Evaluate Habu Lisp expressions
- `lisp_compile` - Compile to native ARM64 code
- `lisp_disasm` - Disassemble compiled code
- `lisp_jit` - JIT compile and execute
- `lisp_trace` - Trace function execution
- `lisp_inspect` - Inspect compiler internals
- `lisp_apropos` - Search for symbols

Use these tools instead of launching SBCL manually with `--load` or `--eval`.

## Development Guidelines

### Problem Solving

- **Ultrathink for significant efforts**: Use extended thinking for complex tasks
- **Take no shortcuts**: Always identify root cause of bugs
- Investigate systematically, don't patch symptoms
- Add tests that reproduce bugs before fixing

### Session Management

1. **CONTEXT.md** - Codebase knowledge (architecture, conventions, technical reference)
   - Update only when architecture or conventions change
   - Do NOT track tasks here - use beads instead

2. **beads (bd)** - Work items (bugs, features, tasks)
   - All tasks tracked via `bd create/update/close`
   - Check `bd ready` for available work

3. **SESSION.md** - Historical log (auto-populated by hooks)
   - Do NOT write here manually

4. **Commits** - One logical feature per commit
   - Include tests with implementation
   - Short, descriptive summary

### Code Style

- **Hex numbers**: Use `#x` prefix for addresses, offsets, constants
- **No emojis**: Never in code, commits, or docs
- **No marketing language**: Use technical facts
- **Warnings as errors**: ASDF treats warnings as errors. Fix immediately.
- **No inline loads**: Use ASDF, never `(load ...)` in source files
- **Naming**: Always use `reg-alloc` (hyphenated), never `regalloc`

### Code Generation Policy

**IMPORTANT - No Wrapper Functions in codegen.lisp**:
- Use `arm64:*` intrinsics directly with keyword args (`:imm t`, `:offset N`)
- Add new instruction variants to `arm64/asm.lisp`, not wrapper functions
- Example: `(arm64:orr rd rn 1 :imm t)` not `(orr-imm rd rn 1)`

**Branch Offset Convention**:
- All branches take instruction counts, not bytes
- Convert from bytes: `(ash byte-offset -2)`

### Debugging

- Use Lisp `trace` facility
- Use `lldb` with embedded function symbols
- Use `slot-debug.lisp` for stack slot collision diagnosis
- Exit codes: See CONTEXT.md

## Testing

**Always use ASDF for loading and testing.**

```lisp
(asdf:test-system :habu)           ; Run all tests
(asdf:load-system :habu/tests)     ; Load test system
```

Test organization in `bootstrap/habu.asd`:
- `bootstrap/test-harness.lisp` - Test utilities
- `tests/test-*.lisp` - Test files

## Slash Commands

**Build & Test:**
- `/habu-build-test` - Compile and test workflow
- `/habu-run-tests [pattern]` - Run test suite
- `/habu-stage <N|verify>` - Self-compilation stages

**Debugging:**
- `/habu-debug <binary>` - Debug with lldb
- `/habu-analyze <error>` - Structured error analysis
- `/habu-disasm <binary> [function]` - Disassemble

**Inspection:**
- `/habu-ir <source>` - Inspect compiler IR
- `/habu-compare <bin1> <bin2>` - Compare binaries
- `/habu-hexdump <binary> [range]` - Hex dump
- `/habu-profile <binary> [duration]` - Profile

**System:**
- `/habu-load` - Load compiler via ASDF

## TAC (Three-Address Code) Pipeline

Register allocator in `bootstrap/reg-alloc.lisp`:

1. **ir-to-tac** - Tree IR to linear TAC
2. **compute-liveness** - Backward dataflow analysis
3. **compute-intervals** - Live intervals
4. **linear-scan** - Register allocation (x9-x15)
5. **tac-codegen** - TAC to ARM64

TAC instruction formats:
```lisp
(tac-lit vreg value)           ; vreg = literal
(tac-var vreg offset)          ; vreg = env[offset]
(tac-binop vreg op vr1 vr2)    ; vreg = vr1 op vr2
(tac-call vreg fn args)        ; vreg = fn(args...)
(tac-if vreg then-lbl else-lbl); conditional branch
(tac-label name)               ; label
(tac-return vreg)              ; return value
```

## Reference

- Common Lisp HyperSpec: https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm
- Git author: Joel Reymont <18791+joelreymont@users.noreply.github.com>

## Compiler Quirks

1. **List expressions with function calls**: Pre-compute in `let` bindings
2. **Tagged values**: See CONTEXT.md for full layout
3. **Register usage**: See CONTEXT.md for full mapping

## When Stuck

1. Check CONTEXT.md for technical reference
2. Check `bd list` for related issues
3. Use `trace` to debug function calls
4. Ask for help
