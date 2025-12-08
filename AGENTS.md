# Habu Lisp Compiler - Agent Instructions

## PARALLEL AGENTS - MANDATORY

**ALWAYS launch parallel agents. Serial investigation is UNACCEPTABLE.**

When debugging or exploring:
- Launch 2-4 agents simultaneously for different investigation paths
- Use `run_in_background: true` for long-running tasks
- NEVER wait for one search to complete before starting another

```
WRONG: Search file A → wait → search file B → wait → analyze
RIGHT: Launch agents for A, B, C simultaneously → combine results
```

**Parallel patterns:**
- Bug hunting: oracle review + codebase search + binary analysis (3 agents)
- Code changes: implement + test + review (3 agents)
- Debugging: crash-analyze + disassemble + trace (3 agents)
- **Feature development: Use git worktrees for parallel implementation**

## Git Worktrees for Parallel Development - CRITICAL

**For independent features, ALWAYS use separate git worktrees:**

```bash
# Create worktrees for parallel work
git worktree add -b feature/lambda ../habu-lambda HEAD
git worktree add -b feature/labels ../habu-labels HEAD
git worktree add -b feature/cond ../habu-cond HEAD
```

**Then launch parallel agents with specific worktree paths:**
```
Agent 1: Work in /Users/joel/Work/habu-lambda on LAMBDA
Agent 2: Work in /Users/joel/Work/habu-labels on LABELS
Agent 3: Work in /Users/joel/Work/habu-cond on COND
```

**After agents complete, cherry-pick or merge changes:**
```bash
# Option 1: Cherry-pick specific commits (preferred for avoiding conflicts)
git cherry-pick <commit-hash-from-feature-branch>

# Option 2: Merge branches (may have conflicts)
git merge feature/lambda

# Clean up worktrees when done
git worktree remove ../habu-lambda
git branch -d feature/lambda
```

**IMPORTANT: Cherry-pick is preferred** when agents make overlapping changes to the same file. This avoids merge conflicts.

**Benefits:**
- No file conflicts between agents
- Each agent commits independently
- Easy to discard failed attempts
- Parallel speedup for multi-feature work

---

## Oracle Reviews - MANDATORY

**ALWAYS ask oracle for complex problems. Launch in PARALLEL background agent.**

- **NEVER paste code** - give file paths: "Review /Users/joel/Work/habu/bootstrap/codegen.lisp:100-200"
- Ask oracle when: stuck >5 min, before commits, unsure about design, after fixes

---

## MCP Tools - MANDATORY

**ALWAYS use MCP tools. NEVER use Bash/Grep alternatives.**

| Task | USE THIS | NEVER |
|------|----------|-------|
| Crash debug | `crash-analyze` | lldb |
| Find symbols | `apropos` | grep |
| Evaluate Lisp | `eval` | bash+sbcl |
| Run/debug | `run`/`debug` | bash |
| Hex/disasm | `hexdump`/`disasm` | xxd/lldb |
| Trace | `traced-eval` | print |
| Issues | `bd-*` | manual |
| Test native | `habu0-eval` | manual |
| Build | `build-habu0` | bash |

**Crash Flow**: crash-analyze → gc-analyze (if GC) → check-ptr → tagged-value

---

## Issue Tracking

**Create bead BEFORE work. Close + commit immediately after.**
- `bd-create --type bug|feature|task`
- `bd-close id note`

---

## Quick Reference

### Tagged Values
Fixnum: `val<<4` | Cons: `ptr|1` | Symbol: `ptr|2` | Vector: `ptr|3` | String: `ptr|4` | Closure: `ptr|5` | Nil: `0x06`

### Registers
x0-x7: args | x20: env | x24: closure | x26: code | x27: GC globals | x28: heap ptr

### Exit Codes
132=SIGILL | 137=SIGKILL | 138=SIGBUS | 139=SIGSEGV

### Key Files
- `habu0.lisp` - Stage 1 interpreter
- `bootstrap/*.lisp` - SBCL bootstrap compiler
- `arm64/asm.lisp` - ARM64 encoders

### Memory at x27
0:intern | 8:lambda_ctr | 16:from_end | 24:half_heap | 32:space_flag | 40:gc_state | 48:sym_ctr | 56:sym_table | 64:argc | 72:argv | 80:packages | 88:current-pkg | 96:stack_base | 112:heap

---

## Code Quality

- Use ASDF: `(asdf:load-system :habu)`
- No ignored warnings - fix the design
- Tests required for every change

### ARM64 Instructions - CRITICAL

**ALWAYS use `arm64/asm.lisp` encoders. NEVER hand-encode instructions.**

- Use `arm64:*` functions with keyword arguments
- Branch offsets in instructions (not bytes): `(ash bytes -2)`
- Registers MUST be keywords: `:x0`, `:x1`, `:env`, `:heap`, etc.
- Check `arm64/asm.lisp` exports before adding new instructions

```lisp
;; CORRECT - use arm64:* with keyword args
(arm64:strb :x0 :x1 0)           ; store byte
(arm64:ldr :x0 :sp :offset 8)    ; load from stack
(arm64:add :x0 :x0 4 :imm t)     ; add immediate

;; WRONG - never hand-encode or use raw numbers
(a64-strb #x0 #x1 0)             ; NO - wrong API
(logior #x39000000 ...)          ; NO - use arm64:strb
```

If an instruction doesn't exist in `arm64/asm.lisp`, add it there first.

---

## Vision

Self-hosting CL: ARM64 native, SBCL-level perf, full CL spec.
Stage 1→2→3 fixed point → SBCL independence → performance → full CL
