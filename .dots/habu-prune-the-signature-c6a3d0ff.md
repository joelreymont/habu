---
title: The dictionary record carries its signature
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:38:29.083141+02:00"
---

User challenge 2026-08-18: the 245KB signature text serves checked code naming BAKED words - but only PUBLIC words are nameable. Measured split: 1606 public / 5192 private / 0 global. Private words live in sealed packages; no checked program can open or compile into one (the seal, checked-habu threat model), so their rows serve nobody BY CONSTRUCTION - the producibility argument. Prune the capture to public rows only: expected 245KB -> ~60KB. CASCADE, handle honestly: (a) the capture-side audit currently requires EVERY checked window record to carry a row - its clause set gains 'private = exempt' with the seal as the argument, and the partition assertion (6798=94+checked) re-derives; (b) verify the intake never needs a private row (the chain's own internal calls resolve by the SEED's relocation, not the checker pool - confirm by measurement); (c) SIGSCOPE's bare-family fixture uses an EXPORTed public word - unaffected; (d) a mutation: a private row smuggled in reds by name (the pool carries only what is nameable). Rides the buffers-at-startup landing or follows it - same artifact surgery region.

SUPERSEDED BY USER RULING 2026-08-18 (the prune was the wrong
fix - the user's question "runtime dispatch uses xts, why carry
the pool?" is correct and terminal): the pool serves ONLY the
compilation of new checked code against the baked API - that is
a HEADER FILE's job, not binary payload. NEW SHAPE: at build
time, GENERATE a declarations file (chain-decl.f or kin) from
the checker's certified store - the ~1606 public signatures,
one row each, through the existing render path (2003/2003
round-trip proven). A program calling the compiler API requires
it like a C header; the build payload adds one require. THE
BINARY CARRIES ZERO SIGNATURE BYTES. DELETES: the baked pool +
its artifact sections (VERSION bump), the lazy intake at
DO-TOK's miss leg, the CHECK-RETRY loop, the AOT-SIG cells -
the subsystem was correct engineering against an invented
requirement (self-describing binary). KEEPS: the capture at the
three producers and the certified store (they GENERATE the
file); the window audit (every public checked word has a row IN
THE FILE); SIGSCOPE's scope discipline applies to the file's
rows (qualified spelling in the generated text makes it moot -
generate QUALIFIED, the TDGEN precedent). Probe first: TRUST-
row semantics for package-qualified names in a consumer (the
one-row experiment that started all this used a global); if
qualified TRUST rows need machinery, checkpoint. The registry
(46KB types) question rides along: families the API's
signatures NAME must exist in the consumer - measure whether
public signatures reference in-window families (the census
knows) and whether the decl file must carry NEWTYPE rows too.
Acceptance: the T2-class reproducers green VIA THE FILE in both
engines; the binary's signature bytes ZERO; the subsystem
deletion attributed; a consumer without the require gets the
honest E-UNDEFINED.

TRUST REFUSED, USER RULING 2026-08-18 (the header-as-TRUST-rows
design is DEAD): TRUST is being retired (c5d41af6 - every
remaining row carries the pointer) and 1600 new rows would be
the largest unchecked-surface expansion in the project. PRIM: is
worse - the axiom list is the trusted computing base; widening
it was refused even for a test. THE FINAL SHAPE: the signatures
are a STAMPED SIDECAR DATA FILE - the artifact's signature
section standalone, written by the build from the checker's
CERTIFIED store, stamped with the producer sha - and the
EXISTING lazy intake reads it at the checker's miss point:
production parser, row-package scope, real checker effects, no
TRUST, no axioms. Provenance verified once at file-open (the
stamp), never asserted per-row. The binary carries ZERO
signature bytes (the buffers-at-startup ruling holds). DELETES:
the baked pool sections from the artifact + the seed's sig emit
+ the AOT-SIG cells. KEEPS: the capture at three producers, the
certified store, the miss-leg intake + retry + SIGSCOPE (re-
aimed at the file), the public-only prune (private rows still
serve nobody - sealed packages). Open probes unchanged: the
registry/NEWTYPE rider; plus one new: the file's OPEN path and
search order (beside the binary? env? the driver-owns-paths
rule applies - probe what the tree's existing sidecar
precedents do, if any).

FINAL DESIGN, USER RULING 2026-08-18 (the fourth and terminal
simplification - baked subsystem -> header -> sidecar ->
DICTIONARY FIELD): the record already carries partial type info
(DNAME-MIN-IN arity bits, DKIND bits in [16]) - COMPLETE IT. A
signature pool sits beside the name pool (same capture, same
offset relocation); each record carries a sig reference as it
carries its name reference; the checker's miss path finds the
record (the wordlist-aware LFIND it already uses) and parses
the signature off the record IN THE RECORD'S OWN WID SCOPE
(also already on the record). Provenance is the binary's own -
certified at capture, traveling in the same verified artifact
as the code. DELETES vs the sidecar: the file, its open/search
problem, the stamp ceremony, the miss-queue file plumbing.
KEEPS: the three-producer capture, the production parser, the
scope discipline. SHIPS: ~60KB public signatures as DICTIONARY
DATA (the public-only prune stands - private records simply
carry no sig ref, the producibility argument). Format: a sig-
offset field in the record (spare bits in [16] or the [24..40)
scheme's unused range for package records - probe the layout);
compact record widens or reuses - VERSION bump either way,
rides the buffers-at-startup artifact surgery. Acceptance: the
T2-class reproducers green THROUGH THE RECORD PATH in both
engines; binary sig bytes = the pool's true size and nothing
else; a record without a ref gives the honest E-UNDEFINED; the
capture audit becomes "every PUBLIC checked window record
carries a sig ref."

FORMAT RULING 2026-08-18 (the user's pointer question): the
field is an OFFSET INTO A SIG POOL - the name-pool scheme
exactly. The pooled element is TEXT, not a typed array: an
array is a second encoding of the checker's whole term grammar
(roles, qualified families, R-clauses, quotation effects,
tyvars, linearity) - two authorities, format migration on every
grammar extension, the five-times-killed drift class. The parse
costs ~12us once per word ever named, through the production
parser, 2003/2003 round-trip proven. THE ARRAY'S SIZE WIN COMES
FROM INTERNING INSTEAD: signatures repeat heavily (( n -- n )
alone: hundreds) - the pool stores unique signatures once,
records share offsets, the name pool's own dedup discipline.
Measure the interned pool size in the implementation; expected
well under the naive 60KB.

STRUCTURE-VS-WIRE 2026-08-18 (the user's structure-pointer
framing, adopted with the measurement): the checker's effect
store IS the signature structure, and post-first-touch the
design converges to it - the intake mints a REAL effect row and
every later lookup hits the structure, never the text. The text
is the WIRE format only, and the trade is measured: the effect
store grew 7.56MB for 6798 words (~1.1KB/word, full term graphs
+ node links) vs ~37B/word rendered text before interning - 30x.
Shipping the structure was also historically impossible (sym
string pointers; foreign family ids - the latter fixed by the
eager registry install, the former still real). DWARF-shaped
division: compact certified encoding on disk, real structure
inflated once on first touch. The record's offset points at the
text; the materialized row then serves all subsequent lookups
through the normal sym path - no re-parse, no second miss.

