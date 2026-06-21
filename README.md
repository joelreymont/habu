# habu — Checked Forth

A complete, **checked Forth** with a native self-hosted macOS ARM64 engine.
Checked code is ordinary Forth that fails to compile unless its body's inferred stack effect
unifies with its declared effect — so an LLM (or human) writing Forth gets the
stack discipline enforced by the compiler, not by hand.

```forth
: SQUARE ( i64 -- i64 ) DUP * ;        \ checks: DUP * has effect ( i64 -- i64 )
: BAD    ( i64 -- i64 ) DUP ;          \ REJECTED: inferred ( i64 -- i64 i64 )
```

The checker is a static pass in front of `:`. There are **no runtime type tags
and no GC** — accepted code runs under the native `bin/hb` engine.

## Requirements

macOS ARM64 and a trusted native `hb` seed if `bin/hb` is not already present.
See [`docs/seed.md`](docs/seed.md).

## Use

Two tiers ship in this repo:

**The native engine** — `bin/hb` is a standalone macOS ARM64 Forth (no gforth,
no C) that JIT-compiles to machine code, type-checks definitions with its
built-in checker, and **rebuilds itself byte-for-byte** (stage2 fixpoint):

```sh
./tools/seed.sh /path/to/hb-seed
                       # recover bin/hb from a trusted native seed
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/build.f tools/build-fixpoint.f tools/build-fixpoint-main.f -- install
                       # daily rebuild: bin/hb recompiles itself, no gforth
echo ': SQ dup * ; 7 SQ .' | bin/hb     # batch: program from stdin
bin/hb script.f arg...                  # script: program from file; args via
                                        #   SCRIPT-ARGC / SCRIPT-ARGV$
printf DATA | bin/hb --load lib/source.f tool.f --
                                        # multi-source script; stdin remains data
bin/hb                                  # tty: checked REPL with line editing,
                                        #   history, breakpoints, `step`, and
                                        #   verification of typed definitions
                                        #   against their ( in -- out )
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/source.f lib/build.f tools/build-fixpoint.f tools/hb-build-lib.f tools/hb-build.f -- prog.f -o prog
                                        # AOT: signed binary, tree-shaken to MAIN
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/source.f lib/build.f tools/build-fixpoint.f tools/hb-build-lib.f tools/hb-build.f -- --repl prog.f -o prog-repl
                                        # checked source bundle + interactive REPL
```

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f
                       # default gate: habu-native, no gforth
```

## The type system

Full Hindley–Milner-style unification with **type variables and first-class row
variables**, over both the data and return stacks, with let-polymorphic
generalization and typed quotations.

Signature grammar `( in -- out )` (case-insensitive, like Forth):

| Form | Meaning |
| ---- | ------- |
| `i64 u8 u32 cell bool char str addr` | concrete types |
| `a b c …` (lower-case, 1 letter) | type variables |
| `R S T …` (upper-case, 1 letter) | row variables (a leading one = the stack tail) |
| `ptr a` | pointer to `a` |
| `[ in -- out ]` | a quotation carrying a stack effect |
| `… | rin -- rout` | optional return-stack clause |

Examples: `DUP : ( R a -- R a a )`, `= : ( R a a -- R bool )`,
`@ : ( R ptr a -- R a )`, `>R : ( R a -- R | S -- S a )`,
`EXECUTE : ( R [ R -- S ] -- S )`. (The return clause follows the data
effect: `Din -- Dout | Rin -- Rout`.)

Supported by the native checker: typed `:` definitions, literals, the primitive
set, polymorphic signatures, `IF/ELSE/THEN`,
`BEGIN…UNTIL/WHILE…REPEAT/AGAIN`, `?DO…LOOP/+LOOP`, `RECURSE`/`EXIT`, typed
locals (`{: a b :}` in checked definitions),
quotations (`[: ;]`), `'`/`['] ` (xt typed
as quot), the return stack (`>R R> R@`), pointer/memory ops, and `TRUSTED:`
annotations for words whose effect can't be inferred (FFI, metaprogramming).

The native engine's built-in checker now models the full surface its engine
compiles: prims, literals, all control flow (`IF`/`BEGIN`/`DO`/`?DO`/`+LOOP`/
`EXIT`/`RECURSE`), the return stack (`>R R> R@ 2>R 2R> 2R@`, balance enforced), typed
locals (`{: a:n :}`), quotations (`[: ;]` + typed `execute`), `trust`
declarations, and prints reject diagnostics to stderr. The toolchain's own
source self-checks clean — see [`STATUS.md`](STATUS.md) for the current count.

The checked native engine is **`bin/hb`**. On a tty it starts the REPL; on a
pipe it reads and runs stdin. It **verifies a definition's body against its own
declared `( in -- out )`** and rejects a mismatch — `: SQ ( i64 -- i64 ) dup ;`
is dropped with a diagnostic, `dup *` is accepted. Untyped definitions stay
infer-only — so LLM-generated definitions should declare signatures and be
verified with `CHECK!` (verify body-vs-sig), not just `CHECK` (infer). The native
sig grammar handles named
row vars (`R S T`), quotation sub-sigs (`[ in -- out ]`), and records
quot-bearing sigs as scheme-strings (so combinator call sites — `dip`, `keep`
— are checked against them). The native grammar also handles distinct concrete
types (`i64 u8 u32 cell char str addr bool`, with `n` the generic int that
subsumes them), the `| rin -- rout` return-stack clause, nested quotations, and
native parsing words (`s"`, `c"`, `."`, `[char]`). The native checker also models
the **parametric** `ptr a` pointer type: memory, path, process, and byte-buffer
primitives consume typed pointers, and `ptr` without an inner type is rejected.

## Layout

- [`PLAN.md`](PLAN.md) — the checker design (type system, unification, pipeline).
- [`docs/forth.md`](docs/forth.md) — Forth coding standards for this repo.
- [`LESSONS.md`](LESSONS.md) — build recipe + findings (the project's memory).
- `src/` — the NATIVE toolchain source the engine compiles (and re-checks) when
  rebuilding itself: `src/core/` (checker, render, sha256), `src/arch/arm64/`
  (encoders, assembler, disassembler, mnemonics), `src/habu/` (engine builder
  parts, jit, profiler, crash, stage2 driver), `src/os/macos/` (Mach-O,
  signing).
- `test/` — `T{ … }T` tests. `test/run.f` is the DEFAULT gate, habu-native
  end to end: lints + self-rebuild fixpoint + engine suite + checked `hb` +
	  tty REPL + hb-build (runs with gforth absent). `tools/` also holds
	  seed/build-fixpoint/hb-build/imgdump/
	  jitdump/clobber-lint/shadow-lint/repl-lint; snapshot refreshes run through
  the checked `tools/build-fixpoint.f snap|install` driver.

## Combinators

`EXECUTE DIP KEEP BI TRI TIMES EACH MAP FOLD` are both **typed** (effects in the
DB, with higher-order library rows pinned by `TRUST`) and **runnable**, so a
checked program can use them and run:

```forth
: PM1 ( i64 -- i64 i64 ) [: 1+ ;] [: 1- ;] BI ;   \ 7 PM1 → 6 8
```

## Self-host & trusted code

Real checker logic is re-checked **through habu's own checker**
(`test/run.f`) and verified to compute the native result:
- the complete **term encoding** (type- and stack-term bit encodings),
- **concrete unification** (`C-UNICON` — the concrete case of `UNIFY-TYPE`),
- **binding resolution** (`C-RESOLVE` — the full `BEGIN/WHILE/REPEAT` chase loop,
  the real `RESOLVE-TYPE` algorithm).

Each is written as a checked `: NAME ( typed-effect ) … ;`, so the checker
type-checks its own algorithm at definition time; a deliberately-wrong
re-implementation is rejected. Host array access (`TV@`) is `TRUSTED:` — a
self-hosting compiler always trusts its runtime.

The checker's algorithmic core (the unifier, the string tokenizer, the wordlist
DB) uses address arithmetic, `parse-name`, and `CREATE` — code whose effect is
not statically inferable. That is exactly what **`TRUSTED:`** is for: you assert
the effect, the checker trusts it, and call sites are still checked. Trusted
definers can also declare `CREATES ( created-effect )`; native `hb` records that
effect for each word the definer creates, and checks a `DOES>` body with the
created word's data-field pointer on the stack. Every word is either checked or
explicitly trusted.

## Notes

- The `:` override checks a definition when its `( … )` parses as a typed
  signature; a plain stack comment is the ordinary Forth colon. A checked body
  that uses words the checker doesn't model falls back to the native colon, so
  the override never breaks valid Forth — it checks what it can.
- Body capture is span-aware: string literals/parsing words (`s"`, `c"`, `."`),
  comments (`( \`), and char literals (`[char] char`) keep their contents (an
  embedded `;` or `(` never breaks a definition).
