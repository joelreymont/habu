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
