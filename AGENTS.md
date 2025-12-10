# Habu Lisp Compiler - Agent Instructions

## ZERO TOLERANCE RULES

### Test Failures
**ALL TEST FAILURES ARE BUGS. NO EXCEPTIONS.**
- NEVER dismiss failures as "pre-existing" or "expected"
- Fix IMMEDIATELY - 100% green before any commit
- Warnings are errors

### Silent Fallbacks
**NEVER write code that silently falls back to different behavior.**

Silent fallbacks MASK BUGS. **CRASH LOUDLY WITH A STACK TRACE.**

```lisp
WRONG: (handler-case (x) (error () default))  ; Hides bugs
WRONG: (if (can-x) (x) (silently-y))          ; Different behavior
RIGHT: (x)                                     ; Let it crash
RIGHT: (error "Failed: ~A" reason)            ; Explicit failure
```

**Ask: "What bug am I hiding?"**

---

## PARALLEL WORK - MANDATORY

Launch 2-4 agents simultaneously. Serial work is UNACCEPTABLE.

```bash
# Worktrees for parallel file changes
git worktree add -b feature /tmp/habu-feature HEAD
# Work, commit, cherry-pick back, remove worktree
```

---

## MCP TOOLS

| Task | Tool | NOT |
|------|------|-----|
| Crash | `crash-analyze` | lldb |
| Symbols | `apropos` | grep |
| Lisp | `eval` | bash |
| Run | `run`/`debug` | manual |
| Build | `build-habu0` | bash |

**Crash Flow**: crash-analyze → gc-analyze → check-ptr → tagged-value

---

## QUICK REFERENCE

### Tags
`val<<4`:fixnum | `ptr|1`:cons | `ptr|2`:sym | `ptr|3`:vec | `ptr|4`:str | `ptr|5`:closure | `0x06`:nil

### Registers
x0-x7:args | x20:env | x24:closure | x27:GC | x28:heap

### x27 Memory Layout
```
0:intern 8:lambda_ctr 16:from_end 24:half_heap 32:space_flag
40:gc_state 48:sym_ctr 56:sym_table 64:argc 72:argv 80:packages
88:current-pkg 96:stack_base 104:global_vars 112:symtab_ptr
120:symtab_count 128:keyword_table 144:heap
```

### Exit Codes
132=SIGILL | 137=SIGKILL | 138=SIGBUS | 139=SIGSEGV

### Key Files
- `habu0.lisp` - Stage 1 compiler
- `bootstrap/*.lisp` - SBCL bootstrap
- `arm64/asm.lisp` - ARM64 encoders (SINGLE SOURCE - no duplicates)

---

## ARM64

**ALL encoders in `arm64/asm.lisp`. NO DUPLICATES.**

```lisp
(arm64:ldr :x0 :sp :offset 8)  ; Correct
(logior #x39000000 ...)         ; WRONG - hand-encoding
(defun movz ...)                ; WRONG - duplicate
```

Registers: keywords only (`:x0`, `:env`, `:heap`)

---

## Vision

Self-hosting CL: ARM64 native, SBCL-level perf, full CL spec.
