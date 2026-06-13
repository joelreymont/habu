# habu — Checked Forth

A complete, **checked Forth** hosted by and self-hosted in Gforth. Checked code
is ordinary Forth that fails to compile unless its body's inferred stack effect
unifies with its declared effect — so an LLM (or human) writing Forth gets the
stack discipline enforced by the compiler, not by hand.

```forth
: SQUARE ( i64 -- i64 ) DUP * ;        \ checks: DUP * has effect ( i64 -- i64 )
: BAD    ( i64 -- i64 ) DUP ;          \ REJECTED: inferred ( i64 -- i64 i64 )
```

The checker is a static pass in front of `:`. There are **no runtime type tags
and no GC** — accepted code compiles to ordinary Gforth.

## Requirements

Gforth **0.7.9** (dev). Homebrew ships 0.7.3; build 0.7.9 from source — recipe in
[`LESSONS.md`](LESSONS.md). Keep `~/.local/bin` ahead of `/opt/homebrew/bin` on
`PATH`.

## Use

Two tiers ship in this repo:

**The native engine** — `bin/hb` is a standalone macOS ARM64 Forth (no gforth,
no C) that JIT-compiles to machine code, type-checks definitions with its
built-in checker, and **rebuilds itself byte-for-byte** (stage2 fixpoint):

```sh
./tools/bootstrap.sh   # build bin/hb from nothing but gforth (once)
./tools/build.sh       # daily rebuild: bin/hb recompiles itself, no gforth
echo ': SQ dup * ; 7 SQ .' | bin/hbi    # batch: program from stdin
bin/hbi                                 # on a tty: interactive REPL
                                        #   (line editing, history, error recovery)
./tools/hb-build.sh prog.f -o prog      # AOT: standalone signed binary (~17 KB:
                                        #  tree-shaken to the program's words)
```

**The gforth-hosted checker** (bootstrap tier — the full row-polymorphic
checker, quotations/combinators included):

```sh
gforth bootstrap/habu.fs       # loads the checker + the ':' override
```
Then `: NAME ( typed-effect ) body ;` is checked; `: NAME body ;` (no typed
effect) is the ordinary Forth colon, untouched.

```sh
gforth bootstrap/examples.fs   # runnable checked programs
( cd test && ./run.sh )        # default gate: habu-native, no gforth, <10 s
( cd test && ./run.sh full )   # + tools/oracle.sh: the gforth differential
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
`@ : ( R ptr a -- R a )`, `>R : ( R a | S -- R | S a )`,
`EXECUTE : ( R [ R -- S ] -- S )`.

Supported (gforth-hosted checker): typed `:` definitions, literals, the
primitive set, polymorphic signatures, `IF/ELSE/THEN`,
`BEGIN…UNTIL/WHILE…REPEAT/AGAIN`, `?DO…LOOP/+LOOP`, `RECURSE`/`EXIT`, typed
locals (`{: a b :}`, `{ a:u8 -- }`), quotations (`[: ;]`), `'`/`['] ` (xt typed
as quot), the return stack (`>R R> R@`), pointer/memory ops, and `TRUSTED:`
annotations for words whose effect can't be inferred (FFI, metaprogramming).

The native engine's built-in checker now models the full surface its engine
compiles: prims, literals, all control flow (`IF`/`BEGIN`/`DO`/`?DO`/`+LOOP`/
`EXIT`/`RECURSE`), the return stack (`>R R> R@`, balance enforced), typed
locals (`{: a:n :}`), quotations (`[: ;]` + typed `execute`), `trust`
declarations, and prints reject diagnostics to stderr. The toolchain's own
source self-checks at **783 certified / 0 uncheckable / 0 rejected**.

The warm habu environment (the AOT snapshot, `tools/snap-hb.sh`) **verifies a
definition's body against its own declared `( in -- out )`** and rejects a
mismatch — `: SQ ( i64 -- i64 ) dup ;` is dropped with a diagnostic, `dup *`
is accepted. Untyped definitions stay infer-only. The native sig grammar matches the
gforth tier: named row vars (`R S T`), quotation sub-sigs (`[ in -- out ]`),
and recording of quot-bearing sigs as scheme-strings (so combinator call
sites — `dip`, `keep` — are checked against them).

## Layout

- [`PLAN.md`](PLAN.md) — the checker design (type system, unification, pipeline).
- [`CODEGEN-PLAN.md`](CODEGEN-PLAN.md) — the native backend / self-host design.
- [`docs/forth.md`](docs/forth.md) — Forth coding standards for this repo.
- [`LESSONS.md`](LESSONS.md) — build recipe + findings (the project's memory).
- `bootstrap/src/` — the gforth-hosted full checker, one file per concern;
  `bootstrap/habu.fs` adds the `:` override. `bootstrap/cg/` — the gforth-hosted
  engine builder (ICode, encoders, Mach-O, jit, disassembler, profiler, crash
  handler).
- `src/` — the NATIVE toolchain source the engine compiles (and re-checks) when
  rebuilding itself: `src/core/` (checker, render, sha256), `src/arch/arm64/`
  (encoders, assembler, disassembler, mnemonics), `src/habu/` (engine builder
  parts, jit, profiler, crash, stage2 driver), `src/os/macos/` (Mach-O,
  signing).
- `test/` — `T{ … }T` tests. `test/run.sh` is the DEFAULT gate, habu-native
  end to end: lints + self-rebuild fixpoint + hb-suite + warm-snapshot boot +
  tty REPL + hb-build (runs with gforth absent). `tools/oracle.sh` is the
  gforth differential (the gforth-hosted suite + the boot-vs-port goldens) —
  run it before pushing emitter changes, or `run.sh full` for both. `tools/`
  also holds bootstrap/build/hb-build/probe/imgdump/jitdump/parity-lint/
  clobber-lint/shadow-lint, and `snap-hb.sh` for the AOT snapshot binary
  (boots the whole toolchain warm in ~3 ms).

## Combinators

`EXECUTE DIP KEEP BI TRI TIMES EACH MAP FOLD` are both **typed** (effects in the
DB) and **runnable** (`bootstrap/src/runtime.fs`), so a checked program can use them and
run:

```forth
: PM1 ( i64 -- i64 i64 ) [: 1+ ;] [: 1- ;] BI ;   \ 7 PM1 → 6 8
```

## Self-host & trusted code

Real checker logic is re-checked **through habu's own checker**
(`test/t-selfhost.fs`) and verified to compute the native result:
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
the effect, the checker trusts it, and call sites are still checked. This is a
designed feature of the maximal scope, not a gap — every word is either checked
or explicitly trusted.

## Notes

- The `:` override checks a definition when its `( … )` parses as a typed
  signature; a plain stack comment is the ordinary Forth colon. A checked body
  that uses words the checker doesn't model falls back to the native colon, so
  the override never breaks valid Forth — it checks what it can.
- Body capture is span-aware: string literals (`S" ." C" S\" .(`), comments
  (`( \`), and char literals (`[char] char`) keep their contents (an embedded
  `;` or `(` never breaks a definition).
