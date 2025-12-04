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

**CRITICAL: Always commit changes BEFORE closing a bead with `bd close`.** This ensures all work is tracked in git history.

See `bd --help` for full command reference.

## MCP Server - MANDATORY

**You MUST use the Habu MCP server for ALL Lisp operations.** Never launch SBCL manually or use Grep/Bash for Lisp operations.

### Imperative Rules

1. **ALWAYS use `lisp_apropos`** to find Lisp symbols - never use Grep
2. **ALWAYS use `lisp_eval`** for Lisp evaluation - never spawn SBCL via Bash
3. **ALWAYS use `lisp_run`/`lisp_debug`** for running binaries - never use Bash unless stdin is needed
4. **ALWAYS use `lisp_hexdump`** for hex dumps - never use xxd via Bash
5. **ALWAYS use `lisp_disasm`** for disassembly - never use lldb disas via Bash
6. **ALWAYS use `lisp_paren_check`** for syntax errors - never parse manually

### Creating New MCP Tools

**Create new MCP tools when it saves tokens.** If you find yourself:
- Running the same Lisp code pattern repeatedly
- Parsing complex output from existing tools
- Needing specialized analysis that produces verbose output

Add a new tool to `mcp-server/habu-mcp.lisp`. The tool should:
- Accept focused parameters
- Return concise, structured output
- Avoid dumping large data structures

### Complete Tool Reference (21 Tools)

| Tool | Use When | Parameters |
|------|----------|------------|
| **Evaluation** |||
| `lisp_eval` | Evaluate any Lisp code | `code`, `timeout` |
| `lisp_traced_eval` | Debug with function tracing | `code`, `functions` |
| `lisp_compile` | See ARM64 output for expression | `source` |
| `lisp_jit` | Execute compiled code in-process | `expr` |
| **Symbol Lookup** |||
| `lisp_apropos` | Find ANY Lisp symbol | `pattern`, `package` |
| `lisp_inspect` | Get function/var documentation | `object` |
| **Binary Analysis** |||
| `lisp_run` | Run binary, get exit code | `binary`, `args`, `timeout` |
| `lisp_debug` | Debug crash with lldb | `binary`, `args` |
| `lisp_codesign` | Sign macOS binary | `binary` |
| `lisp_disasm` | Disassemble hex to ARM64 | `hex` |
| `lisp_hexdump` | Dump file bytes | `file`, `offset`, `length` |
| **Debugging** |||
| `lisp_trace` | Toggle function tracing | `function`, `enable` |
| `lisp_paren_check` | Find mismatched parens | `file` |
| `lisp_lldb_script` | Generate debug script | `binary`, `break_on_gc` |
| **Tagged Values** |||
| `lisp_tagged_value` | Decode tagged pointer | `value` |
| `lisp_check_ptr` | Validate pointer | `ptr`, `x27` |
| `lisp_heap_info` | Show memory layout | (none) |
| **GC Analysis** |||
| `lisp_gc_analyze` | Analyze GC crash | `x27`, `x28`, `crash_addr` |
| `lisp_gc_roots_info` | Explain root scanning | (none) |
| `lisp_env_slots` | Show env frame layout | `x20`, `count` |
| `lisp_stack_frames` | Walk stack frames | `binary`, `fp`, `sp` |

### Token Efficiency - MANDATORY

- **NEVER trace low-level functions** (e.g., `temp-slot`, `emit-byte`)
- **Trace only entry points** (e.g., `habu:deliver`, `habu:codegen-fn`)
- **Use targeted queries** - never dump entire data structures
- **Limit output** - use `head_limit` in Grep, small `count` values
- **Read specific ranges** - use `offset`/`limit` in Read tool
- **Never re-read files** you've already seen in this session

### Mandatory Tool Selection

| Task | MUST Use | NEVER Use |
|------|----------|-----------|
| Find Lisp symbols | `lisp_apropos` | Grep |
| Evaluate Lisp | `lisp_eval` | Bash + sbcl |
| Run binaries | `lisp_run` / `lisp_debug` | Bash (unless stdin needed) |
| Hex dump files | `lisp_hexdump` | xxd via Bash |
| Disassemble | `lisp_disasm` | lldb via Bash |
| Check syntax | `lisp_paren_check` | Manual parsing |
| Trace functions | `lisp_traced_eval` | Print statements |

### GC Crash Debugging Workflow

1. `lisp_debug` - get register values from crash
2. `lisp_gc_analyze` - analyze x27/x28 heap state
3. `lisp_check_ptr` - validate suspicious pointers
4. `lisp_tagged_value` - decode specific values
5. `lisp_gc_roots_info` - understand root scanning

### Quick Reference

```lisp
;; Symbol lookup (NEVER use Grep for this)
lisp_apropos pattern="codegen"

;; Trace compilation
lisp_traced_eval code="(habu:compile-forms ...)" functions="habu:codegen"

;; Decode tagged value (42 as fixnum = 672)
lisp_tagged_value value=672

;; Run and debug binaries
lisp_run binary="/tmp/test"
lisp_debug binary="/tmp/test"
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
