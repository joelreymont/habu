---
title: "TFAM 12: interpret-mode wide layout values"
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T22:21:10.423427+02:00"
---

Interpret-mode (top-level, outside a colon body) transports of layout/bundle values are reachable TODAY via a TRUSTED maker at the unchecked REPL and SILENTLY CORRUPT instead of failing closed. Repro: TRUSTED: MK ( -- pp<n,n> ) 7 9 ; then top-level  MK dup . . .  prints  9 9 7  (exit 0) — interpret-mode dup copies only the top/tag cell of the 2-cell bundle, not the whole group, and no reject/die fires. This is independent of items 8/9 (constructors) — the gap exists now through any TRUSTED boundary that yields a wide value at the REPL. Mechanism options: (a) interpret-mode width tagging — carry the checker/registry logical width on the interpret data stack so top-level stack ops (dup/drop/swap/...) move whole groups, mirroring the compiled pass-2 lowering; or (b) a top-level check hook that types interpret-mode lines and either width-aware-lowers or fails closed on a wide transport at the unchecked REPL. Acceptance: top-level MK dup either yields the correct 4-cell copy (9 7 9 7) or fails closed with a named diagnostic; a committed interpret-mode regression covers dup/drop/swap over a TRUSTED-seeded bundle. Referenced by habu-tfam-12-layout-057181a9 REMAINING item (2), whose stale 'unreachable' rationale was corrected.

## Mechanism decision (2026-07-08, engine lane)

Re-proven on 44efc694 (worse than above): `MK dup . . . .` prints
`9 9 7 <garbage>` rc 0 — the 4th `.` reads below the seeded cells; `swap`
moves only the tag (`5 GE-WMK swap . . .` -> `7 9 5`); `drop` pops one cell.

CHOSEN: fail-closed interpret gate (the fail-closed core of option (b)),
not option (a) width tagging.

- Option (a) — carrying logical widths on the interpret stack so top-level
  ops move whole groups — is dynamic typing of the REPL: every interpret-
  executed word's effect must be applied at runtime (row-polymorphic effects
  included: top-level `dup` is `( a -- a a )`, so correct lowering needs the
  runtime width of the actual operand, i.e. a shadow width stack maintained
  from recorded effects per executed word). That reimplements the checker
  dynamically and is disproportionate; the dot allows fail-closed where
  lowering is not expressible.
- Chosen shape: a wide value must never LAND on the untyped interpret stack.
  Executing a word whose recorded effect mentions a wider-than-cell layout
  value (any row, producers and consumers) at interpret level fails closed
  with `hb: interpret-mode layout value: <name>` on fd 2 and the established
  interpret-reject rc 70 (LUNDEF pattern: eval rollback + tty REPL recovery
  preserved). Checked definitions remain the way to work with bundles —
  matching the compiled pass-2 lowering story; interpret lowering can lift
  the gate later without changing the flag plumbing.

ARCHITECTURE (two halves, split by current file-lane holds):

1. ENGINE half (src/habu + mirror, this lane): `DNAME-WIDE` dict-record name
   flag (bit 62; 60=IMM, 61=EXT, 62/63 were free); LFIND extracts it into
   x13 bit 3 at both match sites (all existing x13 consumers CBZ/CBNZ or
   `13 2 ANDI`, audited safe); EM-INTERPRET-FIND and the interpret `'`
   (C-TICK) gate on the bit (tick is gated because `' MK execute` would
   launder the produce past the dispatch gate; BEXEC on a raw xt cannot be
   gated). xref.f gains the marking/query helpers (baked prefix, engine-
   owned records). Stage0 mirror gets the same dispatch test (its flag is
   never set — no checker — so behavior is unchanged; parity only).
2. CHECKER half (src/core/checker.f — HELD by the item lane, SEQUENCED): at
   the signature-record choke point (E-REC-START flows: hook certify,
   trust-pend, TRUST, checker-defer), walk the four effect row terms with
   the existing T-WIDTH and, when any term is a layout value wider than one
   cell (including inside quotation sub-sigs — a quotation value can be
   `execute`d at top level), mark the pending/named dict record via a new
   engine prim (+ its PES row). The checker owns this fact: it alone has the
   resolved terms, qualified-name symbols, and the single record choke
   point; deriving it engine-side would duplicate sig semantics (rejected:
   an engine-lexical `<`-scan cannot know widths and false-flags width-1
   cell families; an xref-side USIGS record walk duplicates the EN-node
   encoding and cannot cover `TRUST` or package-qualified syms cleanly).

RESIDUAL (documented, not closed by the gate): unchecked top-level code that
obtains a raw xt via `find`/`search-wl` and BEXECs it bypasses any static
flag; quotation values bound into wide defers are covered (the defer's own
record is flagged), but a bare `execute` of a hand-built xt is unchecked
territory by definition.

REGRESSION (committed RED first, per TDD): test/gate-engine-lib.f
GE-INTERP-LAYOUT in the engine runtime slice — dup/drop/swap legs over a
TRUSTED-seeded 2-cell bundle assert rc 70 + `interpret-mode layout value`
on stderr (RED today: rc 0, silent corruption), plus a guard leg proving a
checked definition calling the same TRUSTED maker still runs at top level
(rc 0, `9 7`).

## Engine half LANDED (2026-07-08, engine lane); checker half SEQUENCED

Implemented per the decision above, with these findings:

- The dict region is READ-ONLY at runtime: a Forth-level record-flag store
  (xref.f raw `!`) SIGBUSes. Marking is therefore the engine prim
  `wide-mark` ( -- ) (habu1.f BWIDEMARK, FPRIM): sets DNAME-WIDE on the
  NEWEST PUBLISHED record (ndict-1) inside the same LPROT RW/RX mprotect
  bracket the `immediate` flag write uses (habu2.f C-IMMEDIATE). xref.f
  keeps the read query `XREF-WIDE? ( ptr a -- bool )`.
- LFIND folds the bit into x13 bit 3 at both final match sites (hash +
  linear); the qualified-name FIND-NMATCH restart is not a final match. All
  pre-existing x13 consumers CBZ/CBNZ or mask with `13 2 ANDI` - audited.
- EM-INTERPRET-FIND and interpret `'` (C-TICK) branch to LWIDE, which shares
  LUNDEF's recovery tail via a new LDIAGRET label (eval-frame rollback, tty
  REPL recovery, else exit 70): diagnostic
  `hb: interpret-mode layout value: <token>` on fd 2. Inside `evaluate` the
  behavior matches undefined-word recovery (diagnostic + EVALERR + resume) -
  the rc-0-after-load-error question is the separate
  habu-standalone-support-load-7c3d9f16.
- No new TRUST rows: the LWIDE block lives inside the existing trusted
  em-compile-undef emitter; the prim is an FPRIM registration (PES row for
  checked-code callers is checker.f work, sequenced).
- Stage0 mirror: DNAME-WIDE, find-bit extraction, dispatch + tick gates
  (exit 70; inert - stage0 never marks), and a real BWIDEMARK.
- Growth watermark: the concatenated stage2 engine source crossed
  S2-SOURCE-CAP $C0000 by 33 bytes ('stage2: source exceeds buffer', rc 74,
  loud). S2-SOURCE-CAP and MK-SOURCE-CAP bumped to $100000 in step.
- Regression test/gate-engine-lib.f GE-INTERP-LAYOUT (engine runtime slice):
  dup/drop/swap/tick legs assert rc 70 + the diagnostic; guard leg proves a
  checked definition compiling a call to the SAME marked word still runs at
  top level (9 7, rc 0). STAND-IN: the fixture marks the maker itself with
  `wide-mark` until the checker half lands - then DELETE that line so the
  legs pin the checker-computed flag (the dot's true acceptance).

CHECKER HALF (blocked on src/core/checker.f, held by the item lane):
at the signature-record choke point (hook certify, trust-pend, TRUST,
checker-defer), walk the four effect row terms with T-WIDTH (including
quotation sub-sigs - a quotation value can be `execute`d at top level) and
mark the target dict record when any term is a layout value wider than one
cell. NOTE the prim's semantics: `wide-mark` targets the newest PUBLISHED
record, but the record flows run in the PEND window (before ndict++), so the
integration needs either (a) a pend-variant prim marking index ndict, or
(b) a checker-set DATA cell consumed by the engine publish tails after
ndict++ (the EM-P2-TRIGGER `wf-wide?` C-FIND-GLOBAL pattern), plus a PES row
for the prim and the regression stand-in removal. Residual to keep dotted:
`TRUST`-declared wide effects recorded AFTER a word was already
interpret-executed, and raw-xt laundering (`find`/`search-wl` + `execute`)
in fully unchecked code.

## Audit vs tip db88a576 (2026-07-09)

ENGINE half LANDED (confirmed in tip source; the copied `bin/hb` was stale and
did NOT bake it — a fixpoint refresh was required for the seal/tail-process gate
to go green): DNAME-WIDE dict flag + LFIND bit + `wide-mark`/BWIDEMARK + XREF-WIDE?
+ EM-INTERPRET-FIND/C-TICK LWIDE gate sharing LDIAGRET recovery + diagnostic
`interpret-mode layout value` + stage0 mirror + S2/MK source-cap bump; regression
test/gate-engine-lib.f GE-INTERP-LAYOUT (dup/drop/swap/tick legs rc 70 + guard
leg) with the maker-self `wide-mark` STAND-IN at gate-engine-lib.f:460.

## CHECKER half LANDED (2026-07-09, commit "TFAM 12: checker-computed interpret wide marking")

Prim-target choice: option (b)-variant — NO new engine prim and NO new DATA
cell. The checker owns both the fact and its publication: E-ADD-EFFECT (the
single creator of USER effect records, so every flow funnels there — hook
certify via CHECKER-USIG-CERT-ADD, trust-pend and top-level TRUST via
CHECKER-USIG-ADD, defer effects via C-DEFER's trust-pend) computes ROW-WIDE?
over the four rows (T-WIDTH per term; recursion into T-QUOT sub-effects via
Q>DIN/Q>DOUT/Q>RIN/Q>ROUT; pointers deref'd only to reach nested quotations —
`ptr <layout>` is one cell and stays unmarked) and stores the verdict BY VALUE
into the RECW latch. REC-WIDE-PUBLISH consumes the latch (read+clear) and,
when set, calls the EXISTING `wide-mark` prim — the engine publish tails
invoke it AFTER ndict++ (EM-REC-WIDE-PUBLISH, hook-guarded, in
EM-COMPILE-PUBLISH-TRUSTED / EM-COMPILE-PUBLISH-HOOKED / C-DEFER), where
wide-mark's newest-published target (ndict-1) is exactly the record whose
effect was just recorded. Rationale vs the alternatives: a pend-variant prim
duplicates the mprotect bracket for a timing problem the tails solve for
free; a raw DATA cell splits ownership of the fact. Latch staleness is
impossible at a consumer: every consuming publish path runs its own record
flow last (hook / trust-pend / defer trust-pend), CHECK-RESET zeroes RECW for
hook paths that certify without recording, and USIG-ADD-BAD / E-ADD-DELETED
zero it. PES rows added for `wide-mark` and REC-WIDE-PUBLISH; both censused
AX-NOEXEC-C.

Regression: GE-INTERP-LAYOUT STAND-IN deleted — the dup/drop/swap/tick legs
now pin the CHECKER-computed flag (TRUSTED wide maker, rc 70,
`hb: interpret-mode layout value: <name>`), plus new legs: a hook-certified
wide producer (`: GE-WMK2 ( -- gewide<n,n> ) GE-WMK ;` then top-level call),
a wide defer (`defer GE-WD ( -- gewide<n,n> )` then top-level call), and the
scalar negative control (TRUSTED `( -- n )` maker interprets, rc 0, `42`).

Blast-radius finding: the type-layout goldens ticked their wide-effect
subjects (`' TLP-DUP`) — the interpret tick gate now CORRECTLY fails closed
on them. The goldens only read code bytes, so the xt acquisition moved into
the suite's TRUSTED introspection boundary (TLP-XT via raw `search-wl` — the
documented raw-xt residual) with a TRUSTED.md row + inventory count bump.

RESIDUALS (unchanged, stay dotted here): a top-level `s" name" s" effect"
TRUST` row that WIDENS an already-published word's effect does not mark its
record (no publish tail follows; same class as the TRUST-after-execution
ordering residual), and raw-xt laundering (`find`/`search-wl` + `execute`)
in fully unchecked code bypasses any static flag by definition.

Gate tails for the checker-half commit (2026-07-09, verbatim, all true-rc):
- fixpoint refresh (install --force): `bin/hb refresh OK: compiler fixpoint` /
  `bin/hb ready (small checked engine, tty REPL + stdin)` rc 0
- full gate `bin/hb --load test/run.f`: `PASS: native test suite (fixpoint +
  engine suite + checked hb + repl + hb-build) (10428ms <= 40000ms budget)`
  rc 0, zero RED lines (the first gate run correctly went RED on the golden
  ticks — `FAIL: type-layout suite on Habu-under-test`,
  `hb: interpret-mode layout value: TLP-DUP` — fixed via TLP-XT)
- end-to-end smokes: TRUSTED wide maker `GE-WMK dup . . . .` rc 70
  `hb: interpret-mode layout value: GE-WMK` (no manual marking); checked
  producer GE-WMK2 rc 70; wide defer GE-WD rc 70; checked guard GE-WRUN rc 0
  `9 7`; scalar control GE-WN rc 0 `42`
- test/type-decl-suite.f `ok`; test/type-layout-lower-pending.f (stdin) `ok`;
  test/type-family-suite.f `ok`; test/type-ctor-suite.f `ok` — all rc 0
- maki/test.f: `test: ok` / `PASS: maki/device-smoke.f (3ms)` rc 0
- test/prop-test.f (stdin): `prop-test: sweep OK — 8` (census OK) rc 0
- tools/trust-lint.f: `trust-lint: 479 TRUST site(s), 543 manifest row(s), 0
  finding(s)`; tools/trusted-inventory-test.f: `test: ok` rc 0
- tools/dot-dep-lint.f: `dot-dep-lint: 162 dot(s), 13 blocker(s), 0
  finding(s)` rc 0; typed-local-diff-lint on the diff: rc 0
