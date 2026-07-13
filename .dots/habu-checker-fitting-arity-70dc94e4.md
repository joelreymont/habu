---
title: "checker: fitting-arity immediates certify with wrong runtime certificate (p5 soundness hole)"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-13T10:37:06.567985+02:00\""
---

HIGH. Found by the typed-top-level design probes (2026-07-13, doc docs/typed-top-level.md sec 5 sub-dot 1; workspace probe p5): an IMMEDIATE word with fitting declared arity CERTIFIES inside a checked definition, but executes at COMPILE time (reading below base at compile-time stack state) and leaves an EMPTY runtime body under the declared certificate - e.g. a ( n -- n n ) certificate over a body that does nothing at runtime. Fully-checked source produces a wrong certificate: downstream checked callers unify against effects the runtime body does not deliver. This is not a depth-guard residual - it is a checker MODEL error (immediates' compile-time effects are modeled as runtime effects). Fix shape per the design doc: model immediate execution as compile-time (the certificate must describe the RUNTIME body; immediate tokens contribute their compile-time expansion, not their declared effect), or reject immediates in checked bodies pending the model (fail-closed interim, mirroring the opener treatment) - the doc's sub-dot 1 chooses the model fix with the reject as the interim. Reproducer in the design workspace probes; reduce to a minimal committed fixture FIRST, negative regression, then fix. Type-system lane; blocks tier-1 of the typed top level.

## Parked 2026-07-13 (session limit)
Worker (p5imm) terminated by API session limit before delivering the fix. Only
the reproducer landed: commit b5ecba4 "Add immediate-in-checked-body reproducer
test" in workspace .jj-ws/fable-p5imm. The checker MODEL fix (or fail-closed
reject interim) was NOT implemented. Claim released. Resume: build on that
reproducer, implement the model fix/reject, rebuild fixpoint, add negative
regression. HIGH soundness — take first when the campaign resumes.

## Implementation path (mapped 2026-07-13, orchestrator)
Reproducer committed: test/immediate-model-test.f (commit b5ecba4, workspace
.jj-ws/fable-p5imm) — asserts `: IMM2 ( n -- n n ) dup ; immediate  : USER
( n -- n n ) IMM2` REJECTS. Currently the checker MODELS IMM2 as a runtime
`( n -- n n )` step (unsound: IMM2 runs at compile time, runtime body empty).

Exact fix per docs/typed-top-level.md sec 5 sub-dot 1 ("consult DNAME-IMM"):
- Token step path is DO-TOK (src/core/checker.f:5584). It resolves the token to
  CURSYM (:5589 CHECKER-FIND-ACTIVE-SYM) then applies its stored effect
  (:5592 FEP-HIT? -> EFF-APPLY). Insert an immediate reject BETWEEN resolve and
  apply: if the resolved word is an unmodeled immediate, `E-IMMEDIATE-BODY throw`
  (new named code, lib/errors.f) with a pre-execution named diagnostic.
- Modeled control immediates (if/then/else/[:/;]/s"/do/loop) never reach DO-TOK:
  DO-TOK1 (:7226) filters CF-TOK? (:7244), string openers (:7253-7255), RS-TOK?,
  construct/match/{: BEFORE DO-TOK. So any immediate reaching DO-TOK is unmodeled.
- Immediate detection: the live dict has the word marked immediate by the time
  its caller is checked (source loads in order), DNAME-IMM = $1000000000000000
  (bit 60, src/habu/layout.f:33). The engine reads it in LFIND assembly
  (habu2.f:3858 -> flag bit 1; habu1.f:2654/2687). Two options: (A) add a
  Forth-callable `TOK-IMM?` primitive wrapping the find-with-flags and call it in
  DO-TOK (needs habu1/habu2 asm + fixpoint rebuild), OR (B) checker-side: add an
  IMM-SYMS set mirroring UNSAFE-SYMS (checker.f:5181), mark the last-defined
  symbol when the checker processes the top-level `immediate` token (find where
  interpret-mode `immediate` is handled + last CHECKER-RECORD-NAME symbol).
  Prefer (B) — no engine asm, snapshot-stable, matches the UNSAFE-SYMS precedent.

BLOCKING follow-through (acceptance names these — they use immediates in checked
bodies and MUST stay green, re-authored as needed): postpone; src/core/include.f
(require/include); engine-suite IM5/P5/TPNI fixtures. The fixpoint rebuild + full
test/run.f + engine-suite will surface every site; each must be re-authored or
moved to an audited boundary. Then byte-identical fixpoint x2 and wire
test/immediate-model-test.f into a gate. Cost MEDIUM per the design doc; this is
a codebase-wide soundness tightening, not a local edit — do it deliberately, not
at a session tail. Claim released.
