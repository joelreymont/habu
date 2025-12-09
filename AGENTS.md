# Habu Lisp Compiler - Agent Instructions

## TEST FAILURES - ZERO TOLERANCE

**ALL TEST FAILURES ARE BUGS IN OUR CODE. NO EXCEPTIONS.**

- NEVER dismiss a test failure as "pre-existing" or "unrelated"
- NEVER say "2 failures are expected" - they are NOT
- Every failure MUST be investigated and fixed
- If a test fails, the code is broken - FIX IT

```
WRONG: "2 FASL linker failures are pre-existing issues"
RIGHT: "2 FASL linker failures - investigating and fixing now"
```

**Test suite must be 100% green before any commit.**

---

## PARALLEL AGENTS - MANDATORY

Launch 2-4 agents simultaneously. Serial investigation is UNACCEPTABLE.

```
WRONG: Search A → wait → search B → wait → analyze
RIGHT: Launch agents for A, B, C simultaneously → combine results
```

**Parallel patterns:**
- Bug hunting: oracle + codebase search + binary analysis
- Code changes: implement + test + review
- Debugging: crash-analyze + disassemble + trace
- Features: git worktrees for parallel implementation

---

## Git Worktrees for Parallel Development

```bash
git worktree add -b feature/foo ../habu-foo HEAD
# Work in separate directories, cherry-pick when done
git cherry-pick <commit>
git worktree remove ../habu-foo
```

---

## Oracle Reviews - MANDATORY

Ask oracle for complex problems. Launch in background.

- Give file paths, not code: "Review /path/to/file.lisp:100-200"
- Ask when: stuck >5 min, before commits, unsure about design

---

## MCP Tools - MANDATORY

| Task | USE THIS | NEVER |
|------|----------|-------|
| Crash debug | `crash-analyze` | lldb |
| Find symbols | `apropos` | grep |
| Evaluate Lisp | `eval` | bash+sbcl |
| Run/debug | `run`/`debug` | bash |
| Hex/disasm | `hexdump`/`disasm` | xxd |
| Test native | `habu0-eval` | manual |
| Build | `build-habu0` | bash |
| Issues | `bd-*` | manual |

**Crash Flow**: crash-analyze → gc-analyze (if GC) → check-ptr → tagged-value

---

## Issue Tracking

Create bead BEFORE work. Close + commit immediately after.

---

## Quick Reference

### Tagged Values
Fixnum: `val<<4` | Cons: `ptr|1` | Symbol: `ptr|2` | Vector: `ptr|3` | String: `ptr|4` | Closure: `ptr|5` | Nil: `0x06`

### Registers
x0-x7: args | x20: env | x24: closure | x26: code | x27: GC globals | x28: heap ptr

### Exit Codes
132=SIGILL | 137=SIGKILL | 138=SIGBUS | 139=SIGSEGV

### Key Files
- `habu0.lisp` - Stage 1 compiler
- `bootstrap/*.lisp` - SBCL bootstrap compiler
- `arm64/asm.lisp` - ARM64 encoders

### Memory at x27
0:intern | 8:lambda_ctr | 16:from_end | 24:half_heap | 32:space_flag | 40:gc_state | 48:sym_ctr | 56:sym_table | 64:argc | 72:argv | 80:packages | 88:current-pkg | 96:stack_base | 112:heap

---

## ARM64 Instructions - CRITICAL

**ALL assembler intrinsics live in `arm64/asm.lisp`. NO DUPLICATES.**

- `arm64/asm.lisp` is the SINGLE SOURCE for all ARM64 encoding
- NEVER define movz, movk, ldr, str, add, sub, etc. elsewhere
- Use `arm64:*` prefix in bootstrap code, bare names in habu0.lisp (via fenv)

```lisp
;; CORRECT - use arm64/asm.lisp encoders
(arm64:ldr :x0 :sp :offset 8)
(arm64:add :x0 :x0 4 :imm t)

;; WRONG - hand-encoding
(logior #x39000000 ...)

;; WRONG - duplicate definition
(defun movz (rd imm) ...)  ; DELETE THIS - use arm64:movz
```

Registers MUST be keywords: `:x0`, `:x1`, `:env`, `:heap`, etc.

---

## Vision

Self-hosting CL: ARM64 native, SBCL-level perf, full CL spec.
