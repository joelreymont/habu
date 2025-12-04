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

The project has an MCP server configured in `.mcp.json`. Use these tools instead of launching SBCL manually or using Grep/Bash for Lisp operations.

### Complete Tool Reference (21 Tools)

#### Evaluation & Compilation

| Tool | Purpose | Key Parameters |
|------|---------|----------------|
| `lisp_eval` | General Lisp evaluation | `code` (required), `timeout` (optional, default 60s) |
| `lisp_traced_eval` | Evaluate with function tracing | `code`, `functions` (space-separated list) |
| `lisp_compile` | Compile Habu source to ARM64 | `source` (defun or expression) |
| `lisp_jit` | Compile and execute via mmap | `expr` (expression to run) |

#### Disassembly & Inspection

| Tool | Purpose | Key Parameters |
|------|---------|----------------|
| `lisp_disasm` | Disassemble ARM64 hex to assembly | `hex` (hex string, spaces OK) |
| `lisp_inspect` | Describe object/symbol with docs | `object` (evaluated first) |
| `lisp_apropos` | Search for symbols by substring | `pattern`, `package` (optional) |
| `lisp_hexdump` | Hex dump file bytes | `file`, `offset`, `length`, `width` |

#### Binary Execution

| Tool | Purpose | Key Parameters |
|------|---------|----------------|
| `lisp_run` | Run binary, capture output/exit | `binary`, `args`, `timeout` (default 30s) |
| `lisp_debug` | Run under lldb, get crash info | `binary`, `args` |
| `lisp_codesign` | Ad-hoc sign Mach-O binary | `binary` |

#### Debugging & Tracing

| Tool | Purpose | Key Parameters |
|------|---------|----------------|
| `lisp_trace` | Enable/disable function tracing | `function`, `enable` (boolean) |
| `lisp_paren_check` | Check paren balance in file | `file` (path to .lisp file) |
| `lisp_lldb_script` | Generate lldb debug script | `binary`, `break_on_gc`, `watch_env` |

#### Tagged Values & GC Analysis

| Tool | Purpose | Key Parameters |
|------|---------|----------------|
| `lisp_tagged_value` | Decode Habu tagged value | `value` (integer) |
| `lisp_check_ptr` | Validate tagged pointer | `ptr` (hex), `x27` (optional) |
| `lisp_heap_info` | Show heap layout reference | (no parameters) |
| `lisp_gc_roots_info` | Explain GC root handling | (no parameters) |
| `lisp_gc_analyze` | Analyze GC crash state | `x27`, `x28` (hex), `crash_addr` (optional) |
| `lisp_env_slots` | Show environment slot layout | `x20` (hex), `count` |
| `lisp_stack_frames` | Walk stack frames | `binary`, `fp`, `sp` (hex), `depth` |

### Token Efficiency

**CRITICAL: Minimize token usage in all tool calls.**

- **NEVER trace low-level functions** that get called thousands of times (e.g., `temp-slot`, `emit-byte`)
- **Trace high-level entry points only** (e.g., `habu:deliver`, `habu:codegen-fn`)
- **Use targeted queries** - don't dump entire data structures
- **Limit output** - use `head_limit` in Grep, small `count` values
- **Read specific line ranges** - use `offset`/`limit` in Read tool
- **Avoid redundant reads** - don't re-read files you've already seen

### When to Use Which Tool

**CRITICAL: For finding Lisp identifiers** (functions, variables, macros, IR tags):
- **ALWAYS use `lisp_apropos`** - searches the live Lisp image across ALL packages
- This includes: HABU package, ARM64 package, SYS package, CL package
- Works for: `buffer-byte-set`, `arm64:ldrb`, `codegen`, `compile-expr`, etc.
- **NEVER use Grep** for Lisp symbol lookup - apropos is faster and more accurate
- Examples:
  - Finding ARM64 instructions: `lisp_apropos pattern="ldrb"` -> finds `ARM64:LDRB`
  - Finding compiler functions: `lisp_apropos pattern="codegen"` -> finds all codegen variants
  - Finding IR tags: `lisp_apropos pattern="buffer"` -> finds buffer-related symbols

**For debugging compiler issues**:
- Use `lisp_traced_eval` with functions like `habu:codegen`, `habu:lift-lambdas`
- Example: `functions: "habu:codegen habu:compile-expr"` to trace multiple functions
- Trace output shows call depth, arguments, and return values

**For paren errors**:
- Use `lisp_paren_check` - reports exact line/column with context
- Shows unclosed parens and extra close parens with surrounding code

**For running/testing binaries**:
- Use `lisp_run` or `lisp_debug` instead of Bash
- NOTE: These tools redirect stdin from /dev/null - programs requiring interactive input will see EOF immediately
- For programs needing stdin, use Bash with `echo "input" | ./binary` or heredocs
- `lisp_run` shows exit codes and signal info (SIGILL=132, SIGSEGV=139, etc.)

**For GC crash debugging**:
1. Run `lisp_debug` to get register values from crash
2. Use `lisp_gc_analyze` with x27/x28 values to understand heap state
3. Use `lisp_check_ptr` to validate suspicious pointers
4. Use `lisp_tagged_value` to decode specific values
5. Use `lisp_gc_roots_info` to understand why pointers weren't updated

### Tool Usage Examples

```
# Evaluate Lisp code
lisp_eval code="(+ 1 2 3)"

# Trace function calls during evaluation
lisp_traced_eval code="(habu:compile-forms '((+ 1 2)))" functions="habu:codegen"

# Compile and see ARM64 hex
lisp_compile source="(defun add1 (x) (+ x 1))"

# Disassemble hex to assembly
lisp_disasm hex="D2800020 91000400"

# JIT compile and execute (returns result)
lisp_jit expr="(* 6 7)"

# Find symbols
lisp_apropos pattern="codegen" package="HABU"

# Describe a function
lisp_inspect object="#'habu:codegen"

# Check paren balance
lisp_paren_check file="/path/to/file.lisp"

# Build and run a binary
lisp_run binary="/tmp/test-bin"
lisp_debug binary="/tmp/test-bin"

# Decode tagged value (42 as fixnum = 42 << 4 = 672)
lisp_tagged_value value=672

# Analyze GC crash
lisp_gc_analyze x27="0x100000000" x28="0x100001000"
```

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
