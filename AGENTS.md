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

## Tags & Registers
`val<<4`:fixnum | `ptr|1`:cons | `ptr|2`:sym | `ptr|3`:vec | `ptr|4`:str | `ptr|5`:closure | `0x06`:nil

x0-x7:args | x20:env | x24:closure | x27:GC | x28:heap

## x27 Layout
```
0:intern 8:lambda_ctr 16:from_end 24:half_heap 32:space_flag 40:gc_state
48:sym_ctr 56:sym_table 64:argc 72:argv 80:packages 88:current-pkg
96:stack_base 104:global_vars 112:symtab_ptr 120:symtab_count 128:keyword_table 144:heap
```

## Exit Codes
132=SIGILL | 137=SIGKILL | 138=SIGBUS | 139=SIGSEGV

## Key Files
`habu0.lisp` (Stage 1) | `bootstrap/*.lisp` (SBCL) | `arm64/asm.lisp` (encoders - SINGLE SOURCE)

## ARM64
ALL encoders in `arm64/asm.lisp`. NO duplicates. NO hand-encoding. Registers as keywords (`:x0`, `:env`).

## Vision
Self-hosting CL: ARM64 native, SBCL-level perf, full CL spec.
