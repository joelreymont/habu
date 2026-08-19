---
title: "CAST: reads the next definition off the stream"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T15:52:11.871693+02:00"
---

USER-RULED FINAL (2026-08-19, supersedes both earlier designs on this leaf's
history): `CAST: NAME ( from -- to )` manufactures a TYPE SIGNATURE and nothing
else. The checker resolves the two declared signature terms in the current
scope, runs the five CAST-CERTIFY structural refusals on them (7129 arity, 7130
class, 7131 family, 7135 owner, 7137 linear - properties of the terms, never of
a body), and publishes NAME carrying that signature with identity behavior. No
definition is manufactured anywhere: no generated source text, no evaluate
crossing, no CAST-PEND window, no second spelling.

SUPERSEDED, with the evidence that killed each: (1) the original sketch
"CAST: delegates to : on the live stream" - impossible: `:` is a reader
construct, not an executable word (stepper.f:63-76), and no input-cursor
restore exists before layout.f loads, so lookahead is destructive at roles.f's
row. (2) CAST-NEXT: (a second stream spelling) - rejected as ceremony: it kept
the definition ritual and added a form; the user ruling is one CAST:, zero
definitions.

WHAT THE BUILD DELETES (the payoff, to be listed in the landing report): the
text generator in roles.f, CAST-EVAL, the CAST-PEND window machinery in
checker.f, the generated-text fidelity caveat, and the measured drift hazard
(a name-keyed window with no positional bound certifies against a definition
arriving arbitrarily later) - which ceases to exist rather than being repaired.

OPEN QUESTIONS the probe answers before building: (a) the checker's signature
resolver (the path that turns a definition's stack comment into SGIN/SGOUT rows
during CHECK, proven live at roles.f's row 894 by the boot probe) - what does
it need to run OUTSIDE a definition, as a contained checker.f change;
(b) the body census over the ~100 live CAST: sites - production casts are
expected empty-body; a cast with code is not a cast (it is a checked word plus
a cast, written as two things), and test-fixture shuffles belong to phase 7
(dc125344); (c) publication - reuse the axiom-path spine (name+effect without a
checked body) with certify in front, identity xt from the engine;
(d) whether existing sites' trailing `;` becomes unnecessary and the mechanical
sweep count if so.

First consumer: roles.f's 34 TRUSTED: casts - no load-order problem remains
because there is no evaluate left to arm. Then the rest of 1f5980b8's sweep.

MEASURED RATIO (trusted-3, 2026-08-19) — recorded before any engine edit, after
ruling A, because the cost side grew three times UNDER the ruling and the
Simplify-Relentlessly test inverted. Escalated to the user; nothing built.

BENEFIT, re-derived from the code (not from the plan):
 - SAFETY ADDED: none. CAST: is ALREADY structural, not a trusted spelling: five
   named refusals through checker.f CAST-CERTIFY (7129 E-CAST-ARITY, 7130
   E-CAST-CLASS, 7131 E-CAST-FAM, 7135 E-CAST-OWNER, 7137 E-CAST-LINEAR). The
   direct form applies the SAME five refusals to the SAME resolved rows.
 - SPEED ADDED: none. Measured with cp@ deltas around three definitions - no
   cast, one cast, three casts: 84 / 84 / 84 bytes. An empty colon body is
   STRUCTURALLY free, not merely cheap: INL-MAX $28 (habu2.f:283) and
   C-CALL-PROLOGUE-SPAN (habu2.f:305) give an empty definition clen 16, so the
   inlined span is empty and the call site emits ZERO instructions.
 - THE EVALUATE CROSSING: removed for casts only. It survives in the tree
   regardless, for five other definers - sumtype constructors (sumtype.f:1504),
   STRUCTURE (structures.f:62), LAYOUT-BUFFER (layout-buffer.f:160), and
   TYPED-VARIABLE / TYPED-BUFFER (layout-buffer.f:445 via LBUF-EVAL!). The
   single-crossing invariant sumtype.f:923-927 documents is NOT restored, only
   made unused by one consumer of six.
 - THE DRIFT HAZARD: real and measured (an armed window survived two unrelated
   definitions and still certified a later same-named one), but it is ~10 lines
   of checker.f to close, with or without this change.
 - ACTUALLY DELIVERED: roles.f's 34 casts unblocked, ~45 lines of CAST-GEN
   deleted, and 77 trailing semicolons.

COST, as now measured:
 - A new interpret-loop reader keyword in src/habu/habu2.f (label, CF-ENTRY row,
   KEEP? treeshake entry, emitter beside EM-INTERPRET-COLON habu2.f:5945).
 - The SAME keyword mirrored into the 10,910-line frozen, audited recovery seed
   bootstrap/cg/forth.fs (its own keyword table, forth.fs:2592). src/core/roles.f
   is the FIRST entry in bootstrap.sh SRC_COMMON, so until the seed is mirrored
   the documented recovery path cannot read the tree.
 - ONE ATOMIC COMMIT for capability + all 79 site conversions, because keywords
   are matched BEFORE dictionary FIND (EM-INTERPRET-DEFINE-KEYWORDS runs ahead of
   EM-INTERPRET-FIND): the instant the engine knows `cast:` it intercepts every
   existing `CAST: X ( a -- b ) ;`, stops at `)`, and strands the `;` at
   interpret state. No green intermediate state exists. This fact governs EVERY
   future reader-keyword landing, not just this one.
 - A one-way seed break: bin/hb is UNTRACKED (jj file list bin/ is empty), so
   every peer session's binary, and the user's, stops reading the tree at the
   merge commit.
 - No gate proves the mirror. bootstrap-mirror-lint's name oversells its rule -
   see the dot below; a manual bootstrap run is today's only proof.

STATUS: on hold at the user's desk. If the ratio stands, the cheapest correct
path in the tree is the ~10-line drift fix in checker.f plus option C's prefix
reorder, which unblocks roles.f without touching the engine or the frozen seed.
