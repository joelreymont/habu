---
title: Complete the chain dialect to the engine surface
status: active
priority: 2
issue-type: task
created-at: "2026-08-06T16:07:09.755260+02:00"
---

Claim: agent=nameres workspace=.jj-ws/habu-complete-the-chain-5aab8cee

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

---- TRANCHE 1 LANDED 2026-08-07: NAME RESOLUTION -----------------------------
The chain no longer refuses a body token for not being in its dialect until it
has ASKED THE ENGINE about it. src/compiler/native/elaborate.f RESOLVE-SCAN is a
new pass, run right after the locals pass and before every pass that READS the
word model, that puts each unmodelled NAME to src/compiler/native/dict.f. A
spelling the engine resolves to a callable record AND the checker has certified a
sizeable effect for becomes a CALLABLE row, and the token compiles into a call.
A caller states a NAME and nothing else.

WHY IT IS A PASS AND NOT A QUESTION ASKED DURING THE WALK. MEM-SCAN and
CROSS-SCAN decide from the model whether an order must be minted and whether the
body calls at all. Resolving lazily during the walk made the walk emit calls the
pre-scans had already concluded could not happen, and CALL-LIVE / CALL-CROSS-CK
refused exactly that disagreement - correctly. Measured: with lazy resolution the
lib census showed E-NELAB-CALL 877 and compiled UNCHANGED at 70. One model,
complete before the first reader, is the only arrangement that works.

---- THE METER, SAME INSTRUMENT AND SAME INVOCATION BEFORE AND AFTER ----------
`bin/hb --load tools/chain-census.f -- lib` and `-- src/core src/habu`.
(The lib scope here is 228 files, the whole tree under lib/. The 58-file lib
figure in the census above was a narrower path set; src/ reproduces the earlier
run exactly - 67 files, 755 examined, 21 compiled - so the two src columns are
directly comparable.)

  lib/ (228 files, examined 2591)        BEFORE      AFTER
    compiled                                 70        128
    refused                                2521       2463
    E-HIR-UNMODELED                        2061       1183   -878  -43%
    E-NCLOB-CAP (-8568)                       0        628   NEW
    E-NELAB-LOCAL                           268        268
    E-A64SEL-CALL (-8551)                     0        126   NEW
    E-NMIGRATE-TEXT                         119        119
    engine-refused-name(70)                  36         36
    E-A64RAV-DKEEP (-8611)                    0         23   NEW
    E-NELAB-CTRL                             21         21
    E-A64RA-SPILL                             0         19   NEW
    E-A64RA-POOL                              0         18   NEW
    unlisted -8405                           11         11
    E-NELAB-JOIN                              0          5   NEW
    E-NELAB-ARITY                             2          3
    unlisted -8401                            2          2
    E-NMIGRATE-STATE                          1          0
    by class: dialect 2363->2268  pressure 0->37  instrument 122->122
              self-check 36->36

  src/core + src/habu (67 files, examined 755)
                                           BEFORE      AFTER
    compiled                                 21        128
    refused                                 734        627
    E-HIR-UNMODELED                         485        271   -214  -44%
    engine-refused-name(70)                 114        114
    E-NFEED-STATE                            67         67
    E-NCLOB-CAP (-8568)                       0         64   NEW
    E-A64SEL-CALL (-8551)                     0         36   NEW
    E-NMIGRATE-TEXT                          26         26
    E-NELAB-LOCAL                            26         26
    unlisted -8405                            8          8
    unlisted -8401                            7          7
    E-A64RAV-DKEEP (-8611)                    0          4   NEW
    E-A64RA-SPILL                             0          1   NEW
    E-NELAB-CTRL                              1          1
    E-A64RA-POOL                              0          1   NEW
    unlisted -8209                            0          1
    by class: dialect 527->418  pressure 0->2  instrument 93->93
              self-check 114->114

READ `compiled` AS A CEILING AND NOT A MEASUREMENT. Both scopes answer EXACTLY
128, which is src/compiler/native/clobber.f ROWS-MAX - the number of live
published routines the clobber record holds. The census publishes a routine per
compiled definition in ONE process, so it saturates that table, and every further
definition is refused E-NCLOB-CAP by NCLOB:RECORD-CK inside publish.f
VALIDATE-EMISSION - which runs AFTER selection, allocation, register-allocation
validation and emission have all succeeded. Those definitions compiled
completely and were refused a table slot, nothing more. So the honest count of
definitions the chain now compiles is compiled + E-NCLOB-CAP:

    lib/   70 -> 756   (10.8x)        src/   21 -> 192   (9.1x)

and the true figure is at least that, because the pressure and selector refusals
below are measured on a saturated process. Dot habu-census-saturates-the-f1ada10f
carries giving the instrument a compile-and-discard mode so the number can be
read directly.

THE REFUSALS MOVED DOWNSTREAM, WHICH IS WHAT A LANDED TRANCHE LOOKS LIKE. Before,
the chain did not know what a name WAS. Now it does, and the remaining refusals
are about lowering the call it now knows how to describe: E-A64SEL-CALL 126+36
(the selector has no lowering for the call shape), E-A64RAV-DKEEP 23+4, and
register pressure, which was ZERO in both scopes before and is now 37 and 2 -
the first time this campaign has measured any.

---- WHAT IS STILL REFUSED, AND THE NEXT TRANCHES IT NAMES --------------------
The E-HIR-UNMODELED bucket by spelling now reads, biggest first:
  lib/  s" 197, MAX-SYMS 32, MATCH 31, E-STR-BOUNDS 21, E-FS-PATH 18, [: 17,
        is 16, BYTE-LEN>N 14, E-RX-SYNTAX 14, E-PROC-OUTPUT 13, E-OBJ-SCHEMA 12
  src/  IF 70, s" 33, BEGIN 26, [: 10, >r 6, DEV-FAMILY-USE 5, xt! 5,
        EVENT-FIELD@ 4, -rot 3, DEV-K-FIELD 3, DO 3, S-FAMILY 3

1. NAMED CONSTANTS, and they are now the single biggest RESOLVABLE shape left.
   Every `E-*` spelling above is an error-code constant, and MAX-SYMS and
   BYTE-LEN>N are constants too. They resolve to a record perfectly well and are
   refused for one reason: a `constant`'s published effect is `-- a`, a bare raw
   type variable, and the checker's exported family enum reports EFAM-GRAY for
   it - a width dict.f cannot state, so SPELL-ARITY declines rather than guess.
   Dot habu-export-the-checker-2bbc831c publishes the checker's own ROW-CELLS and
   retires the restriction. This is the cheapest large tranche left.
2. SYNTAX AND LITERALS - s" 197+33, [: 17+10, MATCH 31. Unchanged by this work.
3. DIALECT WORDS IN CAPS - IF 70, BEGIN 26, DO 3 in src/, and note IF has GROWN
   from 15 to 70 because bodies that used to refuse earlier now reach their IF.
   Still the case-folding rule at the dialect lookup, still cheap.
4. Ordinary missing spellings: is 16, >r 6, -rot 3, xt! 5.

---- SOUNDNESS: WHAT WAS PROVED AND HOW ---------------------------------------
THE ARITY IS THE CHECKER'S IN CELLS, PROVED TERM BY TERM. The checker publishes
an effect as a count of TERMS; a call site moves CELLS. They are not the same
number, and the checker's own ROW-CELLS cannot be reused because it walks the SPA
payload representation while the exported query state holds USIGS EN-node offsets.
So dict.f SPELL-ARITY takes the term counts and REFUSES unless every fixed term
carries a family the enum resolves - EN-CON, EN-PTR, EN-QUOT, each exactly one
cell - which makes terms == cells by per-term verification rather than by luck.
Probed: `( ptr u8 n -- )` is 2 terms [SCALAR, POINTER] = 2 cells; `( ptr u8 n n
-- n )` is 3; a `variable` is 1 POINTER; a `constant` is 1 GRAY and is declined.

THE EFFECT CANNOT TELL A DATA WORD FROM A COLON WORD, so nothing is folded. The
first design folded a resolved data word to a literal by RUNNING it, on the
strength of `variable` publishing `-- ptr a` and `constant` publishing `-- a`.
Falsified by probe: `: FORGE-PTR ( -- ptr a ) VX ;` answers din 0, dout 1,
family POINTER - byte-identical to `variable VX` - and `: FORGE-A ( -- a ) 12345
;` is identical to a constant. Folding would therefore EXECUTE arbitrary user
code at compile time and bake whatever it returned into the routine. So every
resolved name becomes a CALL, which is correct for all three shapes: a call to a
`variable` pushes its address, which is exactly what folding would have produced.
The explicit DEFINE-DATA fold stays as the named, caller-asserted boundary and
its corpus rows are untouched.

A CALL MAY NOT BRANCH INTO A COMPILE-TIME WORD. dict.f SPELL-CALLABLE? refuses a
record that is retired, DNAME-IMM or DNAME-INT - publish.f's own rule about
publish.f's own flags. It is not vacuous, and that is measured rather than
argued: walking every record of a loaded engine, 805 words resolve AND size, and
among them `include` and `require`, both IMMEDIATE, plus three RETIRED records.
Without the guard a body naming `include` compiles into a routine that branches,
at run time, into the word that loads a file while the compiler is reading one.
A hand-built immediate (`: IMR-NOOP ( -- ) ; immediate`) is refused earlier, by
the ENGINE - "compile-time immediate has no modeled expansion" - so the checked
path reaches the guard only through an immediate that HAS an expansion.

MUTATION 1 - the derived in-arity. `RESOLVE-CALLABLE` changed to declare
`in 1 +`. test/compiler/native-migrate.f dies with an uncaught -8550
(E-NELAB-CALL, CALL-LIVE refusing a site whose vector holds fewer values than
the callee declares). Gate red. Reverted.

MUTATION 2 - the derived out-arity. `SPELL-ARITY` changed to answer `din din`.
Same suite dies with an uncaught -8303 (E-NELAB-ARITY). Gate red. Reverted.

THE STALENESS CLASS has a case of its own, STALE-CASE: a callee is migrated
against, then `undefine`d and redefined with different arithmetic, and a SECOND
migration of the same body text must follow the NEW callee. It answers 51 then
501. A remembered address could not do that.

THE CALLER-STATED ARITY IS NOT YET GONE EVERYWHERE. `NMIGRATE:CALLEE` still
takes an entry and an effect from its caller, so the lie habu-resolve-a-callee
-0340dfde demonstrated is still writable THROUGH THE STAGING PATH. Resolution
closes it for every name that is not staged, which is now every name in a body
by default. Deleting the staging path outright is the remaining half and is
scoped below.

---- THE CORPUS: RESOLUTION LOWERS TO THE SAME BYTES --------------------------
tools/codegen-compare-test.f: 0 finding(s) before and after, so no existing row
moved. Then, as the measure-first row this tranche owes, the whole of
tools/codegen-compare-migrated5.f was converted from staged callees to resolved
names - all seven CALLEE stagings deleted, DEFINE-CALLING replaced by DEFINE -
covering a tail call, work-before-tail, a non-tail call, a call chain, arity
(2 -> 2) and two calls in one body. Re-measured: 0 finding(s), and the emitted
sizes are IDENTICAL row for row - TAIL-BIG 4, TAIL-MID 4, TAIL-CHAIN 4,
TAIL-PAIR 4, TAIL-WORK 16, TAIL-AFTER 24 bytes - with identical outputs. Only
wall-clock timings differ. Resolving a name produces the same program correct
staging produced, which is the evidence that the staging can be deleted rather
than an argument that it should be.

---- WHAT WAS DELETED -------------------------------------------------------
M-CALLS, CALLS-CK and NMIGRATE:DEFINE-CALL. The flag said "this definition
calls" and a body that called without it was refused. It was never a fact about
the definition: ROUTINE picks the frame from NELAB:CALLED?, which is what the
WALK found, so the check only tested whether the caller had predicted the walk.
Once a body's names resolve, no caller can predict it - and the prediction was
redundant all along.

---- FIXTURES THAT WERE MEASURING THE OLD WORLD -------------------------------
Several suites used `mod` (and `negate`) as their stand-in for "a word the
dialect cannot compile". Those words now compile, so the fixtures were re-based
on a named constant, which is still refused and for a reason the leaf can name.
Deliberately coupled to habu-export-the-checker-2bbc831c: when constants become
compilable those fixtures must change again, which is what a fixture is for.

---- STILL OPEN, SCOPED --------------------------------------------------------
- Delete the CALLEE / DEFINE-CALLING staging path outright: 87 CALLEE sites and
  76 DEFINE-CALLING sites across 11 files. Corpus 5 is converted and proves the
  conversion is byte-neutral; the rest is mechanical. Until it lands, a caller
  can still state a callee's arity wrongly through the staging entry.
- habu-export-the-checker-2bbc831c: cell widths, which unblocks constants.
- habu-census-saturates-the-f1ada10f: the instrument's 128-routine ceiling.
- habu-native-reach-assert-a148e90c: native-reach assert 41 is RED ON MASTER,
  measured at 1c664f2c and at 77115b79 with no local changes, and with
  RESOLVE-SCAN disabled. Not caused by this work.
- habu-walk-the-used-96694010 stays refused fail-closed, untouched.

TRANCHE PROGRESS 2026-08-09: strings landed (30a7121b, merged 3ba76ff0),
locals-groups landed (merged 858150df: E-NELAB-LOCAL 309->57), clobber/census
ceilings landed (merged 601a79cd). Census on 858150df-era tree: compiled 2065
of 3460. The stale 'Claim: agent=nameres' above is DEAD (that lane merged and
closed); this leaf is unclaimed and serves as the tranche ledger.
