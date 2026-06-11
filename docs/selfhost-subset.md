# The self-hosting Forth subset

The bootstrap target: the minimal Forth the **standalone** (the native engine built by
`bootstrap/cg/forth.fs`) must accept so that it can compile its **own compiler's source**. This
document defines that subset and records the proof that the current compiler source lives
inside it.

## Why it matters

Self-hosting = the standalone compiles the Forth program that *is* the compiler
(codegen + checker + Mach-O emitter + self-signer), then that output compiles the same
source again to a byte-identical binary (the fixpoint). For that to even start, every
word the compiler source uses must be one the standalone already provides or one the
source defines itself. If the source reached for a word the standalone lacks, the
bootstrap is impossible. So the subset is a *closure* property, not a wish list.

## The subset

**Number literals** — signed decimal and `$hex` (e.g. `255`, `$FF`, `-$2A`,
`$deadBEEF`; hex is case-insensitive). The standalone's `NUMBER?` (`emit-num`) parses
both, so source may use whichever reads best.

**Defining words** — `:` … `;`, `VARIABLE`, `CONSTANT`, `CREATE`, `ALLOT`, `,`, `C,`,
and `{: a b :}` read-only locals.

**Control flow** (case-folded, so UPPER-CASE source matches) — `IF` `THEN` `ELSE`,
`BEGIN` `UNTIL` `AGAIN` `WHILE` `REPEAT`, `DO` `LOOP` `I`.

**String / tick** — `S" …"`, `['] NAME`.

**Primitives** (registered in `emit-prims`, `forth.fs`):
`+ - * / MOD`, `1+ 1-`, `AND OR XOR INVERT NEGATE LSHIFT RSHIFT`,
`= <> < > <= >= 0= 0<`,
`DUP DROP SWAP NIP OVER TUCK ROT -ROT 2DUP 2DROP`,
`@ ! C@ C! CELLS HERE ALLOT , C,`,
`. .S TYPE EXECUTE DIE PROF-ON PROF-REPORT`,
`F+ F- F* F/ FNEGATE FABS FSQRT F< F> F= F0< F0= S>F F>S F.` (doubles as raw
IEEE754 bit-cells on the data stack; literals `d.d`; checker type `r`),
`OPEN WRITE READ CLOSE RBASE`,
`CATCH THROW`,
`WORDLIST GET-CURRENT SET-CURRENT SEARCH-WL SET-CHECK`.

Everything else a source file uses is defined *within* that file (or an earlier file in
the load order) as a `:`/`VARIABLE`/`CONSTANT`/`CREATE` word.

## What is deliberately NOT in the subset

`." …"` (dot-quote), `MOVE`, `FILL`, `EMIT`, `+!`, `2@`, `2!`, `>R`/`R>`,
`?DUP`, `MIN`/`MAX`/`ABS`, `U<`, `WITHIN`, floating point. The compiler source avoids all
of these — where a primitive is missing it is open-coded (e.g. a store-then-reload
instead of `+!`, an explicit byte loop instead of `MOVE`).

## Proof the source is closed under the subset

Tokenise every compiler-source file, strip comments and string bodies, remove decimal
literals, remove names the file defines (`:`/`VARIABLE`/`CONSTANT`/`CREATE`/locals), and
remove the subset words above. The remainder must be empty.

Files checked: `sha256.f util.f asm.f icode.f mnem.f macho.f sign2.f
checker.f render.f disasm.f habu1.f habu2.f vsjit.f rt.f crash.f prof.f stage2.f` — **408 defined words, residual gap = 0**.

The guard is `test/t-sh-coload.fs`, wired into `test/selfhost-all.fs`: it concatenates
the codegen-layer sources and compiles them under the standalone itself. The standalone
errors (exit 70) on any undefined word — in both compile and interpret mode — so a
future edit that reaches outside the subset fails the gate immediately. The standalone
is the enforcement, not a separate checker script.

## Semantic deltas (same name, different behaviour than gforth)

Vocabulary closure is necessary but not sufficient — these standalone words *behave*
differently, and subset source must be written for the standalone's semantics:

- `DO … LOOP` is do-while: a zero-trip loop (`0 0 DO`) runs once. Guard with
  `n 0 > IF … THEN` when the count can be zero.
- `FIND` takes the **newest** matching definition (scans to the last match), so
  redefinition shadows correctly — but earlier callers are not retro-bound.
- `,` lays an 8-byte cell. Instructions are 4-byte; build them with `C,` bytes.
- A local named `i` is shadowed by the loop-index keyword `I` — name locals `ix`.
- `{: … :}` may appear once per definition, before any `IF`/loop; locals declared
  inside control flow corrupt the frame.
- Number output (`.`) is decimal only; input accepts decimal and `$hex`.

## The fixpoint (achieved 2026-06-11)

The subset proved sufficient: the complete compiler is written in it (`asm icode mnem
util walk rt crash macho engine engine2 stage2`), and `test/t-sh-stage2.fs` is the
standing gate — stage1 (the gforth-built standalone carrying that source as its
program) reads the same source back as data, compiles it with the ported `EMIT-FORTH`,
wraps it with the ported `BUILD-MACHO`, and the emitted stage2 image is byte-identical
to gforth's build of the same source. Any edit that breaks the subset, the emitter
parity, or the fixpoint fails the gate.
