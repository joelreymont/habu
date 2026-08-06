---
title: Complete the chain dialect to the engine surface
status: active
priority: 2
issue-type: task
created-at: "2026-08-06T16:07:09.755260+02:00"
---

Claim: agent=census workspace=.jj-ws/habu-complete-the-chain-5aab8cee

CENSUS 2026-08-06 — tools/chain-census.f, the tranching instrument this leaf
asked for. It drives every plain-`:` definition of a file through held
compilation (NMIGRATE:DEFINE-HELD) under a fresh name in its own reopened
package, taking the arity from the checker and the refusal from the chain, and
counts the refusals by shape.

THE INSTRUMENT IS VALIDATED BY MUTATION, not by its own say-so. Unregistering
DEF-DUP in hir-word.f (WORDS 61->60, PICK-CELLS 15->13) moved the histogram
exactly as it must: `dup` appeared at count 5 where it had been absent, compiled
fell 3->2, and the spellings that used to refuse later (E-A-BOUNDS, 0<) vanished
because those bodies now refuse at `dup` first. Reverted. It measures the chain,
not itself.

---- lib/ (58 files) ----------------------------------------------------------
  files 58   examined 1597   compiled 47 (2.9%)   refused 1550
  not-a-colon-definition 1646   no-certified-effect 0
  in-a-package-the-census-cannot-reopen 321
  self-check: arity disagreements 0, unresolved names 9, rename refusals 0,
              stale elaborator records 0

  by reason: 1356 E-HIR-UNMODELED   129 E-NELAB-LOCAL   45 E-NMIGRATE-TEXT
             9 engine-refused-name(70)   7 unlisted -8405 (E-NFEED-LITERAL)
             2 unlisted -8401 (E-NFEED-SCAN)   1 E-NMIGRATE-STATE
             1 E-NELAB-CTRL
  by class:  dialect 1495   pressure 0   instrument 46   self-check 9

  the E-HIR-UNMODELED bucket by shape (687 distinct spellings, sum 1356):
    call to a tree word          725   53.5%
    data word (variable/create)  270   19.9%
    named constant               243   17.9%
    syntax/literal                70    5.2%  (s" 36, MATCH 27, [: 6, construct 1)
    unclassified                  36    2.7%
    MISSING PRIMITIVE             11    0.8%  (TRUE 6, FALSE 2, mod 1, cell+ 1, 0< 1)
    return-stack                   1    0.1%  (>r 1)

---- src/core + src/habu (67 files) -------------------------------------------
  files 67   examined 755   compiled 21 (2.8%)   refused 734
  not-a-colon-definition 2546   no-certified-effect 2154
  in-a-package-the-census-cannot-reopen 285
  self-check: arity disagreements 0, unresolved names 114, rename refusals 0,
              stale elaborator records 0

  by reason: 485 E-HIR-UNMODELED   114 engine-refused-name(70)
             67 E-NFEED-STATE   26 E-NMIGRATE-TEXT   26 E-NELAB-LOCAL
             8 unlisted -8405   7 unlisted -8401   1 E-NELAB-CTRL
  by class:  dialect 527   pressure 0   instrument 93   self-check 114

  the E-HIR-UNMODELED bucket by shape (277 distinct spellings, sum 485):
    call to a tree word          247   50.9%
    data word (variable/create)   99   20.4%
    unclassified                  40    8.2%
    named constant                34    7.0%
    DIALECT WORD IN CAPS          30    6.2%  (IF 15, BEGIN 15)
    syntax/literal                28    5.8%  (s" 14, [: 9, parse-name 5)
    return-stack                   6    1.2%  (>r 6)
    MISSING PRIMITIVE              1    0.2%  (execute 1)

---- THREE WARNINGS FOR ANYONE READING THESE NUMBERS --------------------------
1. IN src/, no-certified-effect (2154) DWARFS examined (755). The checker's
   effect store answers only for the bare name, inside the word's own package,
   for a file loaded in this process — and most of src/ is compiled into the
   engine image with its names stripped past the seal. Those 2154 definitions
   were never OFFERED to the chain. The src/ percentages describe the third of
   src/ the instrument can reach, not src/.
2. engine-refused-name is 15% of src/ examined (114/755). That is the census
   reporting on ITSELF, not a chain refusal: a body whose callee was visible
   only through a file-level `using`, which does not survive the file. Subtract
   it before reading any dialect share.
3. E-NFEED-STATE (67 in src/) IS NOT A DIALECT GAP. It means no tape was filled
   — the definition was not checked, so held compilation cannot see it at all.
   It is classed instrument for that reason and must never be counted as a
   missing capability.

---- THE TRANCHE ORDER, MEASURED ----------------------------------------------
1. NAME RESOLUTION — 91% of lib/'s unmodeled bucket, 78% of src/'s. Calls, data
   words and constants are one missing capability, not three: the chain
   resolving a name off the dictionary itself instead of the caller staging it.
   Converges with habu-resolve-a-callee-0340dfde and src/compiler/native/dict.f,
   which already owns the resolver walk. Needs NO new IR kind —
   HIR-MEANING:FIXED and DECLARE-CALLABLE exist; what is missing is that
   migrate.f makes the CALLER state them (CALLEES-MAX 4, one data word).
2. THE LOCALS CEILING — E-NELAB-LOCAL, 129 in lib/ and 26 in src/. An
   elaborator rule gap, not a vocabulary one.
3. SYNTAX AND LITERALS — string literals (s" 36 + 14), quotations ([: 6 + 9),
   MATCH/ADT (27, needs the aggregate substrate: habu-give-the-ir-f0cfa96a).
   Also cheap and unlisted anywhere: DIALECT WORDS IN CAPS, 30 definitions in
   src/ blocked on `IF` and `BEGIN` alone. hir-word.f interns each control word
   in exactly one case while the engine's dictionary matches case-insensitively
   (XREF-STR=CI), so a body that writes `IF` is refused for its spelling. A
   folding rule at the dialect lookup, no new rows.
4. MISSING PRIMITIVES — LAST. 0.8% of lib/, 0.2% of src/.

THIS IS NOT THE ORDER THIS LEAF ASSUMED, and the gap is two orders of magnitude.
The leaf planned to add dialect spellings biggest-first and named "ordinary
primitives (negate 0< mod abs min max +! ...)" as a tranche. Measured, `mod`
blocks ONE definition in lib/ and none in src/; `negate`, `abs`, `min` and `max`
never appear at all. So NO dialect tranche was landed here: doing that work
first would have been the 0.8% first, which is exactly what the measure-first
rule exists to prevent.

READ EVERY SHAPE BELOW THE FIRST AS A LOWER BOUND. The chain stops at the FIRST
token it cannot compile, so a definition blocked by a call at token 3 may also
use `mod` at token 9 and is counted once, against the call. The ORDERING
survives that: even counting every textual occurrence of the missing primitives
across the whole scope (max 89, mod 66, min 54, negate 21, 0< 12, abs 10) they
stay an order of magnitude below name resolution. The honest way to get a
tranche's true delta is to land it and re-run this census. That is what the tool
is for.

---- CORRECTIONS TO THIS LEAF'S OWN DESCRIPTION, all measured ------------------
- String and char literals are NOT E-HIR-KIND. That code is UNREACHABLE:
  feed.f APPEND only ever writes name/int/real kinds, so the tape's string and
  char kinds have no producer at all. A string literal is E-HIR-UNMODELED at the
  token spelled `s"`. Either fill those kinds or retire them.
- Named constants are a refusal shape this leaf does not list, and they are
  17.9% of lib/'s unmodeled bucket — twenty times the primitives it does list.
  sha256's `: M32 ( n -- n ) W32 and ;` refuses on W32.
- E-A64EFF-SEQ (-8209) refuses any definition declaring more than twelve inputs.
  An unlisted dialect ceiling nobody predicted, found by the census's raw-code
  path.
- Register pressure is ZERO across both scopes at an 18-register budget. The
  frame is not a blocker at this stage.

---- WHAT THE CENSUS CANNOT SEE -----------------------------------------------
606 definitions across the two scopes sit in packages it cannot reopen. Two
independent engine gates make a package unreopenable (habu2.f
C-PACKAGE-SEAL-GUARD): the protected-WID bitmap, and a sealed reserved-name
table (RESTAB-BUF — tfam, type, match, checker-cert, lower-cert,
lower-cert-hook, engine-error). Reopening either ends the PROCESS with exit 84
and no catch, so the census probes both before opening and mirrors the engine's
table under a test that re-derives it from habu2.f. The census also measures
bodies under fresh names, so it cannot see recursion written by name, any
caller-side effect of the real name, or a callee visible only through `using`.

The chain models 61 spellings vs the engine's 70 compile-path keyword rows over 174 primitives (thecut audit, hir-word.f:938). Missing, by measured refusal: string/char literals (E-HIR-KIND), case/of/endof/endcase, ADT match/construct (needs the aggregate substrate), quotations, does>, plain do/+loop/leave/j, >r/r>/r@, execute, and ordinary primitives (negate 0< mod abs min max +! ...). Tranche the work by refusal count over the real stdlib (measure which gaps block the most definitions — compile the tree through the chain in no-emit mode once it exists and count refusals by shape), land tranches biggest-first, each with corpus rows per the measure-first rule. Blocks the cut.
