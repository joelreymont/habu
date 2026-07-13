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

## Instruction-level implementation (derived 2026-07-13, orchestrator)
The checker CANNOT track immediate-ness itself: CHECK (checker.f:7413) processes
ONE definition's body via DO-TOK1; the top-level `immediate` token that marks a
word lives between definitions and is handled by the engine interpret loop, not
CHECK. And CHECKER-FIND-ACTIVE-SYM (4600) resolves to a CHECKER symbol, not the
engine dict record that carries DNAME-IMM. So "consult DNAME-IMM" needs a
live-dict query exposed to the checker as an engine primitive (the word IS marked
immediate by the time its caller is checked - source loads in order).

STEP 1 - engine primitive `tok-imm?` in src/habu/habu2.f (after LFIND is defined,
in the FPRIM registration block near :5782). Body emitter + registration:
  : BTOKIMM ( -- )   \ ( ptr u8 n -- n )  n = LFIND-flags & 2 (2 = immediate, 0 = not)
     10 G-POP  9 G-POP            \ x10=len (TOS), x9=addr  (LFIND's input regs)
     LFIND LABEL@ BL,             \ x13 = found|imm(bit1)|min-in|int flags
     9 13 2 ANDI,                 \ x9 = flags & 2
     A G-PUSH ;                   \ push x9   (A=x9; G-PUSH stores [XDS], XDS+=8)
  s" tok-imm?" ['] BTOKIMM 1 GDEREF-F   \ FRAMED (LFIND is a BL): saves x30
  VERIFY: XDS register must survive LFIND (LFIND clobbers x5-x16, habu2.f:2640-
  2690). Confirm XDS != x5..x16 in src/habu/layout.f (DATA=x20; find XDS). If XDS
  is in the clobber range, save/restore it around the BL. Data-stack string is
  ( ptr u8 n ) = (addr,len) 2 cells, top=len (see BTYPE habu1.f:1474: 2 G-POP 1
  G-POP). Bool convention: return raw 0/2 as `n`; the caller tests 0<>.

STEP 2 - checker reject in src/core/checker.f DO-TOK (5584). After CURSYM is
resolved (5589) and BEFORE the effect apply (5592 FEP @ EFF-APPLY), insert:
     a u TOK-IMM? 0<> IF  -1 UNDEFERR ! -1 UNCK !  <named reject>  EXIT THEN
  Add E-IMMEDIATE-BODY to lib/errors.f. Modeled control immediates (if/then/else/
  [:/;]/s"/do/loop) never reach DO-TOK - DO-TOK1 (7226) filters CF-TOK? (7244),
  string openers (7253-55), RS-TOK?, construct/match/{: first - so any immediate
  reaching DO-TOK is an unmodeled user/engine immediate that must reject. Note
  the checker-boot region loads with the hook silenced (fixsrc's BFR-CHECK-OFF),
  so tok-imm? used in DO-TOK needs no self-check effect for the stage compile;
  the staged pre-pass (VERIFY:SOURCE-BUF) checks with the PRIOR engine which
  already has tok-imm? once the fixpoint ties.

STEP 3 - fixpoint rebuild (docs/bootstrap.md full prelude, install --force) x2
byte-identical. If the prior engine lacks tok-imm?, bootstrap once via Gforth
(HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh) then refresh.

STEP 4 - REPAIR the breakage the acceptance names: postpone; src/core/include.f
(require/include if used in checked bodies); engine-suite IM5/P5/TPNI fixtures.
Run test/run.f + engine-suite; each rejected site is either re-authored, moved to
an audited TRUSTED boundary, or (if its compile-time expansion is modelable)
handled like POSTPONE. This is a codebase-wide soundness tightening - the reason
it must be done deliberately with debugger-backed RCA, not trial-and-error.

STEP 5 - wire test/immediate-model-test.f (commit b5ecba4, workspace fable-p5imm)
into a gate; it must now PASS (the two IMT-REJECTS assertions go green). Add
E-IMMEDIATE-BODY negative regressions. Byte-identical fixpoint before merge.
