# Habu Lisp Compiler

## Parallel Work
Launch 2-4 agents simultaneously. Use git worktrees for parallel file changes.

## MCP Tools
| Task | Tool | NOT |
|------|------|-----|
| Crash | `crash-analyze` | lldb |
| Symbols | `apropos` | grep |
| Lisp | `eval` | bash |
| Run | `run`/`debug` | manual |
| Build | `build-habu0` | bash |

**Crash flow:** crash-analyze → gc-analyze → check-ptr → tagged-value

**Static analysis:**
| Tool | Purpose |
|------|---------|
| `tag-check <fn>` | Show input/output tag expectations, detect mismatches |
| `layout-of <type>` | Memory layout for cons, symbol, string, vector, closure, keyword |
| `find-unguarded <file>` | Find dereferences without nil/type checks |
| `register-audit <fn>` | Verify callee-saved register handling |

Example: `tag-check keyword-name` → warns if function expects wrong tag

## Tags & Registers (Hybrid 1+3 bit, 16-byte aligned)
```
bit0=1: fixnum (63-bit, val>>1)
bit0=0: ptr|tag (nil=0)
  0:cons  2:sym  4:vec  6:str  8:closure  10:keyword  14:forward
```

x0-x7:args | x20:env | x24:closure | x27:GC | x28:heap

## x27 Layout
```
0:intern 8:lambda_ctr 16:from_end 24:half_heap 32:space_flag 40:gc_state
48:sym_ctr 56:sym_table 64:argc 72:argv 80:packages 88:current-pkg
96:stack_base 104:global_vars 112:symtab_ptr 120:symtab_count 128:keyword_table 144:heap
```

## Exit Codes
11=SIGSEGV (macOS) | 132=SIGILL | 137=SIGKILL | 138=SIGBUS | 139=SIGSEGV (Linux)

**Note:** On macOS, exit code 11 = SIGSEGV crash. Always run `crash-analyze` when you see exit=11.

## Key Files
`habu0.lisp` (Stage 1) | `bootstrap/*.lisp` (SBCL) | `arm64/asm.lisp` (encoders - SINGLE SOURCE)

## ARM64
ALL encoders in `arm64/asm.lisp`. NO duplicates. NO hand-encoding. Registers as keywords (`:x0`, `:env`).

## Vision
Self-hosting CL: ARM64 native, SBCL-level perf, full CL spec.
