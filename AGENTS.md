# Habu Lisp Compiler - Agent Instructions

## MCP Tools - USE THESE FIRST

**STOP. Before using Bash, Grep, or any other tool, CHECK if an MCP tool exists.**

MCP tools are faster, produce cleaner output, and save tokens. You MUST use them.

**WHEN YOU ADD A NEW MCP TOOL, UPDATE THIS SECTION IMMEDIATELY.**

### Absolute Rules - NO EXCEPTIONS

| Task | USE THIS | NEVER USE |
|------|----------|-----------|
| Find Lisp symbols | `lisp_apropos` | Grep |
| Evaluate Lisp | `lisp_eval` | Bash + sbcl |
| Run binaries | `lisp_run` / `lisp_debug` | Bash |
| Hex dump files | `lisp_hexdump` | xxd via Bash |
| Disassemble | `lisp_disasm` | lldb via Bash |
| Check syntax | `lisp_paren_check` | Manual parsing |
| Trace functions | `lisp_traced_eval` | Print statements |
| Find available work | `bd_ready` | Bash + bd |
| List issues | `bd_list` | Bash + bd |
| Show issue details | `bd_show` | Bash + bd |
| Create issues | `bd_create` | Bash + bd |
| Update issues | `bd_update` | Bash + bd |
| Close issues | `bd_close` | Bash + bd |

### Complete Tool Reference (27 Tools)

#### Issue Tracking (bd_*)
| Tool | Use When | Parameters |
|------|----------|------------|
| `bd_ready` | Find available tasks | (none) |
| `bd_list` | List issues by status | `status` (optional) |
| `bd_show` | View issue details | `id` |
| `bd_create` | Create new issue | `title`, `type`, `priority`, `description` |
| `bd_update` | Update status/add notes | `id`, `status`, `note` |
| `bd_close` | Close completed issue | `id`, `note` |

#### Lisp Evaluation
| Tool | Use When | Parameters |
|------|----------|------------|
| `lisp_eval` | Evaluate any Lisp code | `code`, `timeout` |
| `lisp_traced_eval` | Debug with function tracing | `code`, `functions` |
| `lisp_compile` | See ARM64 output for expression | `source` |
| `lisp_jit` | Execute compiled code in-process | `expr` |

#### Symbol Lookup
| Tool | Use When | Parameters |
|------|----------|------------|
| `lisp_apropos` | Find ANY Lisp symbol | `pattern`, `package` |
| `lisp_inspect` | Get function/var documentation | `object` |

#### Binary Analysis
| Tool | Use When | Parameters |
|------|----------|------------|
| `lisp_run` | Run binary, get exit code | `binary`, `args`, `stdin`, `timeout` |
| `lisp_debug` | Debug crash with lldb | `binary`, `args` |
| `lisp_codesign` | Sign macOS binary | `binary` |
| `lisp_disasm` | Disassemble hex to ARM64 | `hex` |
| `lisp_hexdump` | Dump file bytes | `file`, `offset`, `length` |

#### Debugging
| Tool | Use When | Parameters |
|------|----------|------------|
| `lisp_trace` | Toggle function tracing | `function`, `enable` |
| `lisp_paren_check` | Find mismatched parens | `file` |
| `lisp_lldb_script` | Generate debug script | `binary`, `break_on_gc` |

#### Tagged Values & GC
| Tool | Use When | Parameters |
|------|----------|------------|
| `lisp_tagged_value` | Decode tagged pointer | `value` |
| `lisp_check_ptr` | Validate pointer | `ptr`, `x27` |
| `lisp_heap_info` | Show memory layout | (none) |
| `lisp_gc_analyze` | Analyze GC crash | `x27`, `x28`, `crash_addr` |
| `lisp_gc_roots_info` | Explain root scanning | (none) |
| `lisp_env_slots` | Show env frame layout | `x20`, `count` |
| `lisp_stack_frames` | Walk stack frames | `binary`, `fp`, `sp` |

### GC Crash Debugging Workflow

1. `lisp_debug` - get register values from crash
2. `lisp_gc_analyze` - analyze x27/x28 heap state
3. `lisp_check_ptr` - validate suspicious pointers
4. `lisp_tagged_value` - decode specific values
5. `lisp_gc_roots_info` - understand root scanning

### Creating New MCP Tools

**Create new MCP tools when it saves tokens.** If you find yourself:
- Running the same Lisp code pattern repeatedly
- Parsing complex output from existing tools
- Needing specialized analysis that produces verbose output

Add a new tool to `mcp-server/habu-mcp.lisp`. The tool should:
- Accept focused parameters
- Return concise, structured output
- Avoid dumping large data structures

**IMMEDIATELY update this section after adding any new tool.**

---

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

**Use `bd_*` MCP tools for ALL work tracking.** Do not use Bash + bd commands.

**CRITICAL: Always commit changes BEFORE closing an issue with `bd_close`.**

## Development Guidelines

### Problem Solving

- **Ultrathink for significant efforts**: Use extended thinking for complex tasks
- **Take no shortcuts**: Always identify root cause of bugs
- Investigate systematically, don't patch symptoms
- Add tests that reproduce bugs before fixing

### FIX LIMITATIONS IMMEDIATELY

**CRITICAL: When you discover a limitation, bug, or missing feature - FIX IT NOW.**

Do NOT:
- Document it as a "known limitation" and move on
- Create an issue and defer to later
- Work around it with hacks
- Say "this is expected" or "this is a known issue"

Do:
- Stop what you're doing
- Investigate the root cause
- Implement the fix immediately
- Add a test to prevent regression
- Then continue with the original task

**Every limitation is a blocker for self-hosting.** The goal is a fully working compiler, not a compiler with a list of known issues. If the native reader crashes on `defpackage`, fix the native reader. If arm64 functions are missing at runtime, add them. No exceptions.

### Session Management

1. **Project Reference** (end of this file) - Architecture, conventions, technical reference
   - Update only when architecture or conventions change
   - Do NOT track tasks here - use beads instead

2. **beads (bd)** - Work items (bugs, features, tasks)
   - All tasks tracked via `bd create/update/close`
   - Check `bd ready` for available work

3. **Commits** - One logical feature per commit
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
- Exit codes: See Project Reference section

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
2. **Tagged values**: See Project Reference section for full layout
3. **Register usage**: See Project Reference section for full mapping

## When Stuck

1. Check Project Reference section for technical reference
2. Check `bd list` for related issues
3. Use `trace` to debug function calls
4. Ask for help

---

## Project Reference

### File Structure
```
bootstrap/
  compiler-sbcl.lisp  - SBCL bootstrap compiler
  compiler.lisp       - Habu compiler (no SBCL dependencies)
  optimize.lisp       - Optimization passes (TCO)
  codegen.lisp        - ARM64 code generator
  gc.lisp             - Garbage collector (Cheney's copying GC)
  gen-gc.lisp         - Generational GC runtime
  macho.lisp          - Mach-O linker (#+sbcl versions)
  macho-utils.lisp    - Mach-O utilities (#-sbcl native versions)
  reader.lisp         - Habu reader
  reg-alloc.lisp      - Register allocator (TAC pipeline)
arm64/
  asm.lisp            - ARM64 instruction encoders (canonical API)
```

### Tagged Value Representation
- Fixnum: `value << 4`, tag 0
- Cons: `pointer | 1`
- Symbol: `pointer | 2`
- Vector: `pointer | 3`
- String: `pointer | 4`
- Closure: `pointer | 5`
- Nil: `0x06` (tag 6)

### ARM64 Register Usage
- x0-x7: Arguments and return value
- x20: Environment frame base
- x24: Closure environment pointer
- x26: Code base register
- x27: GC globals base (memory layout below)
- x28: Heap bump pointer

### Memory Layout at x27
Simple GC mode:
- `[x27+0]`: intern_table (tagged pointer)
- `[x27+8]`: lambda_counter (untagged integer)
- `[x27+16]`: from_end (GC trigger address)
- `[x27+24]`: half_heap_size (constant)
- `[x27+32]`: space_flag (0 or half_heap_size)
- `[x27+40]`: gc_state (0=idle)
- `[x27+48]`: symbol_counter
- `[x27+56]`: symbol_table
- `[x27+64]`: argc (command-line argument count)
- `[x27+72]`: argv (command-line argument vector)
- `[x27+80]`: packages (package list for native reader)
- `[x27+88]`: current-package (current package name)
- `[x27+96]`: stack_base (initial SP for stack scanning)
- `[x27+104]`: reserved (for 16-byte alignment)
- `[x27+112]`: heap data starts (MUST be 16-byte aligned for tag masking)

Generational GC mode (extends above):
- `[x27+128]`: nursery-start
- `[x27+136]`: nursery-end (also old-space-start)
- `[x27+144]`: card-table-start
- `[x27+152]`: old-space-half-size
- `[x27+160]`: old-space-flag
- `[x27+168]`: old-space-alloc
- `[x27+176]`: heap data starts

### Key Conventions

**ARM64 Instructions**: Always use `arm64:` intrinsics directly with keyword arguments.
```lisp
(arm64:add rd rn imm :imm t)      ; ADD immediate
(arm64:ldr rt rn :offset off)     ; LDR with offset
(arm64:b.eq 5)                    ; Branch (instruction count, not bytes)
```
DO NOT create wrapper functions. See `arm64/asm.lisp` for full API.

**Branch Offsets**: All branch instructions take instruction counts, not bytes.
When computing from `code-size` (bytes): `(ash byte-offset -2)`

**GC Triggers**: Toggle via `*use-generational-gc*` in codegen.lisp:406.
Write barriers in: setcar-ir (:1412), setcdr-ir (:1434), vector-set-ir (:1266).

### Current Blockers (FIX THESE - DO NOT ADD MORE)

These are bugs that MUST be fixed, not "known limitations" to work around:

1. Max 8 arguments per function - IMPLEMENT varargs
2. 64KB file limit for native-read-file - INCREASE buffer or stream
3. Inlining disabled (variable capture bug) - FIX the bug
4. Stack frame: 2KB per call - REDUCE or make dynamic
5. arm64 package missing at Stage 1 runtime - INCLUDE in build

**When you encounter one of these, fix it. Do not add to this list.**

### Debugging Reference

- Exit 132 = SIGILL (check code alignment, branch targets)
- Exit 137 = SIGKILL (codesign issue on macOS)
- Exit 138 = SIGBUS (often stack slot collision - check spill-end in codegen.lisp:240)
- Exit 139 = SIGSEGV (stack overflow or bad pointer)

Use `lldb` with function symbols (LC_SYMTAB embedded in binaries).
Use `slot-debug.lisp` for stack slot collision diagnosis.
