---
title: Capture compiler source tape
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:57:02.984231+02:00\""
blocks:
  - habu-freeze-compiler-baseline-b9777eee
---

Full context: design section 7.1 requires the exact checked token stream before HIR. Capture token kind, byte span, spelling slice, literal value, parser mode, and origin once; no full syntax tree or AArch64 access. Acceptance: source/check/elaboration share one tape digest; immediate words are classified intrinsic, sealed compile-time computation, or named rejection; byte/span/origin corruption rejects. Dependencies: frozen Wave 0 baseline and sealed shared substrate.

Claim: agent=srctape workspace=.jj-ws/habu-capture-compiler-src-01c8f962 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

## Result (srctape, measured)

**What landed.** Two files, two packages, both new.

`src/compiler/native/tape.f` opens package NTAPE and owns the stage N0 tape.
A token row is exactly the six fields design section 7.1 names: kind, byte
span, resolved spelling, literal value where the kind has one, parser mode,
and origin. Public surface: `NEW`, the four minting words `NAME-TOKEN`,
`STRING-TOKEN`, `INT-TOKEN` and `CHAR-TOKEN`, the appenders `PUSH` and
`PUSH-FROM`, the live counter `PUSHED`, `SEAL`, the sealed readers `TOKENS`,
`KIND@`, `MODE@`, `SPAN@`, `SPELL@`, `LIT@`, `TOKEN@`, `EXPANDED?`, `ORIGIN@`
and `DEPTH`, the structural `CHECK`, and `DIGEST` with `VERIFY`.

`src/compiler/native/immediate.f` opens package NIMM and owns the three
classes of compile-time immediate word: `DECLARE` for an intrinsic or for
compile-time computation, `DECLARE-UNMODELED` for a named boundary that must
say which capability it waits for, then `DECLARED`, `CLASS@`, `ADMIT`,
`REASON@`, `AT` for the inventory walk, and `ADMIT-TOKEN`, which applies the
gate to a name token of a sealed tape and is NIMM's first consumer of NTAPE.

**Four decisions that later leaves inherit.**

1. The resolved spelling is an interned IR-SYM symbol, not a second byte
   slice. The byte span already says where the token was read from; a second
   range would repeat it and still make every reader re-lex to learn the name.
   A string literal's spelling is its body, which is exactly what a raw span
   cannot separate from the quoting syntax.
2. Origin is the expansion parent TOKEN, not the parent source. IR-SOURCE
   already holds the parent of a whole source; this is the same relation one
   level down, and it is acyclic for the same reason - a parent must already
   be appended, so ordinals strictly decrease and every walk terminates.
3. Whether a token carries a literal is a property of its kind. There is no
   stored flag that a second piece of state could contradict, there are four
   minting words instead of one so a name token carrying a value cannot be
   asked for, and reading a literal from a kind that has none is refused
   rather than answered with the zero the row stores.
4. The digest excludes the module serial, which is allocated per process. Two
   structurally identical tapes digest identically, so a cached result can
   outlive a run. Per-token record digests are chained the way IR-SCHEMA
   chains its schema table, so no buffer grows with the tape.

**Appending takes no module key.** IR-SOURCE:REGISTER needs one because it
MINTS an identity. This file only stores identities, and a source id or a
symbol id already carries its owning module, so the tape checks the token's
own identities against the module serial in its header. That is stronger than
a presented key, because a caller cannot supply the wrong one, and it is one
argument fewer.

**Error codes.** Two new sub-blocks in the compiler growth region, both taken
from the range lib/errors.f already reserves for the native back end:
-8200..-8219 for NTAPE (ten codes: STATE, OWNER, BOUND, CAP, KIND, MODE,
LITERAL, ORIGIN, ROOT, DIGEST) and -8220..-8239 for NIMM (seven: STATE, OWNER,
BOUND, CAP, CLASS, DUP, UNMODELED). Byte spans are IR-SOURCE's concern and
reject with its own E-IR-SRC-SPAN, so no span code was minted here.
`src/compiler/digest.f` gains two domain-separation tags, TAG-TAPE-TOKEN and
TAG-TAPE.

**Measured results, all in this workspace over parent d3322be5:**

- `bin/hb --load test/compiler/native-tape.f` - exit 0, `test: ok`, 65 cases.
- `bin/hb --load test/compiler/native-immediate.f` - exit 0, `test: ok`.
- The eighteen landed compiler suites (ir-verify, ir-source, ir-symbol,
  ir-arena, ir-context, ir-encode, ir-canon, ir-schema, ir-attr, ir-type,
  ir-id, ir-op, ir-fun, ir-build, ir-render, ir-diff and the two new ones) -
  all exit 0.
- `tools/error-code-lint.f` - exit 0, `1359 file(s), 922 claim(s), 41
  reservation(s), 0 finding(s)`.
- `tools/suite-coverage-lint.f` - exit 0, `176 suite(s), 0 finding(s)`; both
  new suites are registered in test/gate-stdlib-cases.f and routed in
  test/gate-stdlib-inline-lib.f.
- `tools/typed-local-diff-lint.f` and `tools/package-diff-lint.f` on the
  `jj diff --git` artifact - exit 0. The package lint was falsified first: it
  rejected `TAPE-MAGIC` with E-REDUNDANT-FILE-PREFIX until it was renamed.
- `tools/dot-dep-lint.f` - exit 0, `1637 dot(s), 1167 blocker(s), 0
  finding(s)`.
- The load path is fail-closed: giving `PUSHED` a return type its body does
  not satisfy makes `bin/hb --load src/compiler/native/tape.f` print
  `habu: in pushed: at 'CNT' expected: ir-arena:view<> actual: n` and
  `hook: non-certified definition`, exit 70.
- `test/gate-stdlib.f` was not run: the orchestrator owns the loaded-host gate.

**Mutation matrix: 46 single-change mutations, 45 killed.** Each applies one
substitution to the source, runs the owning suite, and restores; the harness
is a scratchpad script and is not committed. Groups: the per-kind literal rule
(2), the append-time rechecks for module ownership, spans and spellings (4),
the origin rules (2), the header tag and row shape (2), capacity (2), reader
bounds and the literal guard (3), all six field rechecks inside CHECK (6), the
digest (11), VERIFY (1), and the contract table (13).

The one survivor is the token count in the chain seed, and it is documented as
such in the file. The fold is fixed-arity, so there is no concatenation
ambiguity a length prefix would resolve, and telling the seeded chain from the
unseeded one apart would take a SHA-256 collision. It is kept because the seed
should state the tape's length and because the chain then has exactly the
shape of IR-SCHEMA's, not because a test can falsify it.

Three earlier survivors were real gaps and were closed rather than explained
away. The header tag check was invisible because every fixture that reached it
already died on the row shape, so a three-cell arena with the wrong tag - the
one shape the tag alone can reject - was added. The token kind was invisible in
the digest because the only kind variant also changed the literal, so the
variant now switches between two kinds that carry the same literal, span and
spelling. And the token's own ordinal in the record preimage could not be
falsified at all: the chain already binds a row's position by where its digest
enters the fold, so the slot was REMOVED rather than kept as unprovable state.

**Best long-term fix, or a patch?** Long-term. Every rule here rests on a
structural invariant, not on a value heuristic. Ownership is proved by asking
the module's own tables - the source registry for a span, the interner for a
spelling - so a forged id fails an existence check rather than a range test.
The three same-typed arenas that meet at PUSH are separated by each package's
own header tag, which is a capability probe, not a lucky value, and there is a
fixture that swaps two of them. The literal rule has no flag to corrupt because
presence is derived from the kind. Origins are acyclic by construction rather
than by a cycle detector. And the digest is falsified field by field rather
than asserted. The one thing a reviewer should push on if they disagree is the
choice to make the tape read-only after SEAL - IR-SOURCE offers live and frozen
readers of everything, and this file offers live reads of the count only. The
argument is design rule 5.1: a tape that can still grow has no digest worth
sharing, and the three stages that must agree all read it after it is sealed.

**Honest gaps, for the orchestrator to weigh:**

1. **No producer.** Nothing builds a tape from real Habu source yet; the only
   producers are the tests. That is the leaf's stated scope - capture the
   record once, without a full syntax tree - but "source, check and elaboration
   share one tape digest" is proved as a mechanism (one sealed tape, one
   digest, VERIFY refuses any other) and not yet as a fact about a real
   compilation. Dotted as `habu-feed-the-src-f7ed8733`.
2. **The compile-time class is declared, not sealed.** Section 7.1 says such an
   immediate may reach the program only through a sealed builder capability.
   There is no builder yet, so NIMM records the intent and cannot enforce it.
   Dotted as `habu-seal-the-compile-5f56e5e9`.
3. **A token ordinal is a plain integer.** IR-ID has no token identity family
   and design section 6.1's families are the IR substrate's, which sits above
   this stage. Every ordinal is bound-checked at use, so an ordinal from
   another tape is caught whenever the counts or the modules differ - but two
   in-range ordinals from two tapes of the same module and length would not be.
   Not dotted; it would mean adding a family to landed substrate.
4. **The checker cannot bind a multi-cell structure local.** This shaped the
   interfaces: NTAPE:token puts its span field last and PUSH-FROM takes its
   parent ordinal above the token, both so the value can be unmade at entry.
   Dotted as `habu-bind-multi-cell-d2e153ed`, with the acceptance that these
   workarounds can be removed without changing a test.
5. **The digest does not reach the bytes.** It covers the cells the tape owns.
   The bytes behind a span and behind a spelling are the source registry's and
   the interner's own content digests, and a stage that needs content identity
   has to bind all three. That is the same authority split encode.f made, but
   it does mean the tape digest alone is a structural identity, not a content
   one.
6. **Both suites run in several harness contexts, not one.** A module here owns
   four arenas against IR-ARENA's sixty-four slots, and a fixture that throws
   holds its context until the enclosing harness exits. One harness ran the
   registry dry. The split is explained in both suites, but it is a fixture
   constraint a reader should know about before adding cases.

AMENDED AT THE NIMM DELETION MERGE (2026-08-20, master b62b8dd5; the claim
agent=srctape has no live lane - amendment made at merge per the same
treatment as 5f56e5e9): the acceptance clause "immediate words are classified
intrinsic, sealed compile-time computation, or named rejection" is
unsatisfiable as written - NIMM and its three-class table are deleted (the
ruling and sized list live on habu-delete-the-nimm-329100c9's closure).
Re-derive the clause against HIR-WORD's admission path, which is the sole
production classifier; the measured line naming test/compiler/
native-immediate.f is likewise dead.
