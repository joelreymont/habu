# Habu Lisp Compiler - Agent Instructions

## Rules

### Issue Tracking with Beads

**Create a bead BEFORE starting ANY work.** No exceptions.
- `bd-create --type bug|feature|task`
- Close beads when done, commit immediately after
- TodoWrite = session tracking; beads = permanent record

### MCP Tools - MANDATORY

**ALWAYS use MCP tools. NEVER use Bash/Grep when an MCP tool exists.**

If MCP is broken or restarted, FIX IT or wait for it to recover. Do NOT fall back to Bash.

| Task | USE THIS (required) | NEVER use |
|------|---------------------|-----------|
| Find Lisp symbols | `apropos` | Grep |
| Evaluate Lisp | `eval` | Bash + sbcl |
| Run/debug binaries | `run` / `debug` | Bash |
| Hex dump | `hexdump` | xxd |
| Disassemble | `disasm` | lldb |
| Check parens | `paren-check` | Manual |
| Trace functions | `traced-eval` | Print statements |
| Issue tracking | `bd-*` tools | Bash + bd |
| Complex problems | `ask-oracle` | Manual research |
| Test native code | `habu0-eval` | Manual testing |

**If an MCP tool fails**: Report the error, investigate, fix the MCP server code if needed.
**Update this file when adding new MCP tools.**

### ASDF for Dependencies - MANDATORY

**ALWAYS use ASDF to load Habu. NEVER use `(load ...)` for bootstrap files.**

```lisp
;; CORRECT - use ASDF
(asdf:load-system :habu)

;; WRONG - never do this
(load "bootstrap/compiler.lisp")  ; Files have complex dependencies!
```

The habu.asd file is in `bootstrap/`. ASDF handles the correct load order.
MCP server loads via: `(asdf:load-system :habu)`

### Warnings = Errors (CRITICAL)

**NEVER use `(declare (ignore ...))` to silence warnings. This is a hard rule.**

When you see an unused variable warning:
- **WRONG**: `(declare (ignore header constants))` - This hides the problem
- **RIGHT**: Use the values, or fix the API to not return unused values

When a function returns values you don't need:
- **WRONG**: Destructure all values and ignore some
- **RIGHT**: Only destructure what you need, or fix the function

Examples of violations that MUST be fixed immediately:
```lisp
;; BAD - ignoring return values
(multiple-value-bind (a b c) (foo) (declare (ignore c)) ...)

;; BAD - underscore convention to ignore
(let ((_ (some-side-effect))) ...)

;; GOOD - use what you need
(let ((a (foo))) ...)  ; if foo returns multiple values, fix foo or use them
```

Every warning indicates a design problem. Fix the design, not the symptom.

### Fix Limitations Immediately

When you discover a bug or limitation: STOP, investigate root cause, fix it, add test, continue.
Do not document as "known limitation" or defer to later.

### Testing

Every feature and bug fix MUST have tests. No exceptions.
- Bug fix: write failing test first, then fix
- Feature: unit tests + property tests where valuable (serialization, encoders, parsers)
- Use `(asdf:test-system :habu)`

### Code Style

- Hex: `#x` prefix always
- No emojis, no marketing language
- **Use ASDF, never `(load ...)`** - see ASDF section above
- Naming: `reg-alloc` (hyphenated)

### ARM64 Codegen

- Use `arm64:*` intrinsics directly with kwargs (`:imm t`, `:offset N`)
- Add variants to `arm64/asm.lisp`, not wrapper functions
- Branch offsets = instruction count, not bytes: `(ash bytes -2)`

---

## MCP Tool Reference

### Issue Tracking
| Tool | Parameters |
|------|------------|
| `bd-ready` | (none) |
| `bd-list` | `status` (opt) |
| `bd-show` | `id` |
| `bd-create` | `title`, `type`, `priority`, `description` |
| `bd-update` | `id`, `status`, `note` |
| `bd-close` | `id`, `note` |

### Lisp Evaluation
| Tool | Parameters |
|------|------------|
| `eval` | `code`, `timeout` |
| `traced-eval` | `code`, `functions` |
| `compile` | `source` |
| `jit` | `expr` |
| `habu0-eval` | `code` |

### Symbol/Binary
| Tool | Parameters |
|------|------------|
| `apropos` | `pattern`, `package` |
| `inspect` | `object` |
| `run` | `binary`, `args`, `stdin`, `timeout` |
| `debug` | `binary`, `args` |
| `codesign` | `binary` |
| `disasm` | `hex` |
| `hexdump` | `file`, `offset`, `length` |

### Debugging/GC
| Tool | Parameters |
|------|------------|
| `trace` | `function`, `enable` |
| `paren-check` | `file` |
| `lldb-script` | `binary`, `break-on-gc` |
| `tagged-value` | `value` |
| `check-ptr` | `ptr`, `x27` |
| `heap-info` | (none) |
| `gc-analyze` | `x27`, `x28`, `crash-addr` |
| `gc-roots-info` | (none) |
| `env-slots` | `x20`, `count` |
| `stack-frames` | `binary`, `fp`, `sp` |

### AI
| Tool | Parameters |
|------|------------|
| `ask-oracle` | `question`, `context` (opt) |

**GC Crash Workflow**: `debug` -> `gc-analyze` -> `check-ptr` -> `tagged-value` -> `gc-roots-info`

---

## Technical Reference

### File Structure
```
habu0.lisp            - **STAGE 1 SOURCE** - THE native Habu interpreter
bootstrap/
  compiler-sbcl.lisp  - SBCL bootstrap compiler
  compiler.lisp       - Habu compiler (no SBCL deps)
  optimize.lisp       - Optimization passes (TCO)
  codegen.lisp        - ARM64 code generator
  gc.lisp             - Cheney's copying GC
  gen-gc.lisp         - Generational GC runtime
  macho.lisp          - Mach-O linker
  reader.lisp         - Habu reader
  reg-alloc.lisp      - Register allocator (TAC)
arm64/
  asm.lisp            - ARM64 instruction encoders
```

### Stage 1 = habu0.lisp (CRITICAL)

**habu0.lisp IS Stage 1. Period. No other file.**

- Stage 1 binary is compiled from `habu0.lisp` by SBCL
- Test with `habu0-eval` MCP tool
- The binary is at project root as `habu0`
- DO NOT look for or create any other "stage1" source files
- There is NO habu-stage1-src.lisp - if you see one, DELETE IT

### FASL Build System

**NEVER concatenate source files.** Use the proper FASL system:

```lisp
;; Step 1: Compile each module to FASL
(habu:compile-to-fasl forms "module.fasl" :exports '(fn1 fn2))

;; Step 2: Link FASLs into executable
(habu:link-fasls '("prelude.fasl" "compiler.fasl" "main.fasl")
                 "/tmp/habu_stage1")
```

**Stage 1 Build** (SBCL compiles Habu):
```lisp
;; Compile each bootstrap file to FASL
(habu:compile-file "bootstrap/prelude.lisp" "prelude.fasl")
(habu:compile-file "bootstrap/expand.lisp" "expand.fasl")
(habu:compile-file "bootstrap/compiler.lisp" "compiler.fasl")
;; ... other modules ...

;; Link into Stage 1 binary
(habu:link-fasls '("prelude.fasl" "expand.fasl" "compiler.fasl" ...)
                 "/tmp/habu_stage1" :include-gc t)
```

**Key functions**:
- `compile-to-fasl` - Compile forms to .fasl file
- `compile-file` - Read and compile source file to .fasl
- `link-fasls` - Link .fasl files into Mach-O executable
- `load-fasl-file` - Load .fasl for introspection

### Tagged Values
| Type | Encoding |
|------|----------|
| Fixnum | `value << 4`, tag 0 |
| Cons | `ptr \| 1` |
| Symbol | `ptr \| 2` |
| Vector | `ptr \| 3` |
| String | `ptr \| 4` |
| Closure | `ptr \| 5` |
| Nil | `0x06` |

### Registers
- x0-x7: Args/return
- x20: Env frame base
- x24: Closure env
- x26: Code base
- x27: GC globals base
- x28: Heap bump ptr

### Memory at x27
| Offset | Field |
|--------|-------|
| 0 | intern_table |
| 8 | lambda_counter |
| 16 | from_end (GC trigger) |
| 24 | half_heap_size |
| 32 | space_flag |
| 40 | gc_state |
| 48 | symbol_counter |
| 56 | symbol_table |
| 64 | argc |
| 72 | argv |
| 80 | packages |
| 88 | current-package |
| 96 | stack_base |
| 104 | reserved |
| 112 | heap start (16-byte aligned) |

### Exit Codes
- 132 = SIGILL (alignment, branch target)
- 137 = SIGKILL (codesign)
- 138 = SIGBUS (slot collision)
- 139 = SIGSEGV (stack overflow, bad ptr)

### Current Blockers
1. Max 8 args - implement varargs
2. 64KB file limit - increase buffer
3. Inlining disabled - fix capture bug
4. 2KB stack frame - reduce/dynamic
5. arm64 missing at Stage 1 - include in build

---

## Project Vision

Self-hosting CL compiler: ARM64 native, SBCL-level performance, full CL spec.

**Roadmap**: Stage 1->2->3 fixed point -> SBCL independence -> performance -> full CL

## Reference Links
- [CL HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm)
- Git: Joel Reymont <18791+joelreymont@users.noreply.github.com>
