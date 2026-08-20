---
title: "External review 2026-08-20: verify and execute"
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T13:14:16.853147+02:00"
---

The user-supplied review: 5 correctness holes (IR row appends not transactional - source.f:207 six pushes after one capacity check, probe left registry unreadable E-IR-SRC-STATE; freeze skips multi-successor SSA arg validation verify.f:418 - regalloc-verify.f:793 is first authority; effect tokens not domain-checked verify.f:718; tensor-value.f placeholder pointer reachable via TV-AT@/TV-MATERIALIZE 439-470 + no bounds; sched-key.f:272 inserts unprobed omitting the live ptx toolchain digest toolchain.f:275) + a simplification table (NIMM no-consumer 261+462 lines, PTX text optimizer 660 off-by-default, NMIGRATE DEFINE-HELD-only, one-SSA-layer ruling, opcode table dedup, suite manifest unification, context reclaim, measurement stores, SUMTYPE/PRODUCT cutover completion, tracker GC ~1500 dots) + doc pins (PLAN.md hash stale, README 165KB claim wrong - it is hb-host's size, softmax gradcheck overclaim). MANDATE: every claim VERIFIED against the tree before acting (the audit-refutation discipline); each verified item gets its own dot with the failing path; the two stop-ships are already dotted separately. The 5 correctness holes are priority; the simplification rows each need the probe-first gate; tracker GC per no-governance-ledgers.

TWO SIMPLIFICATION CLAIMS ADJUDICATED (probe, 2026-08-20; full evidence in
the probe report):
1. NIMM (261+462 lines): reachability CONFIRMED zero consumers - but the
   tree already ruled REWIRE NOT DELETE (habu-give-the-immediate-73cb0a49:
   NELAB:STEP should route declared immediates to NIMM so the compile-time
   class keeps its capability name; design 7.1 class 2, held by 5f56e5e9).
   NIMM has two things HIR-WORD lacks (the compile-time class; interner-
   verified declaration). COUNTER-FINDING against the review: HIR-WORD's
   reason tables are TEST-ONLY too (REASON@/DECLARE-UNMODELED callers only
   in native-hir.f) - "HIR-WORD owns reasons" is false; only its admission
   path is live. The delete-vs-rewire decision belongs to 73cb0a49's own
   gate: the compile-time class needs a named first consumer or NIMM deletes
   with the ~740-line list the probe sized.
2. PTX text optimizer (660+192 lines): off-by-default and caller-less
   CONFIRMED - but habu-adjudicate-dormant-ptx-482310bc carries production-
   emitter measurements (real kernels shrink: matmul -276B, layernorm -41B)
   AND a live bug deletion would bury (attention render GROWS 4B, falsifying
   opt-ir.f:24's never-exceeds-input capacity invariant). The ruled decision
   procedure stands: sm_87/sm_121 SASS measurement decides mandatory-at-the-
   boundary vs delete; the probe's 852+13-line deletion list is banked on
   that dot for the delete branch. The review's 660 is current; the dot's
   622 is stale.
Both: the review's reachability method was right; its conclusions skipped
the tree's own open adjudications. Verify-first held.

SUPERSEDED BY USER RULING (2026-08-20, hard cut): the two "deletions refused"
above are OVERTURNED. The refusals deferred to open dots' opinions as if they
were evidence - they are plans; the tree's evidence is zero consumers in both
cases. NIMM's parked capability fails the Simplify gate's own words (no named
first consumer); the PTX optimizer's win-branch was always a rebuild at the
canonical boundary, never preservation of the dormant pass; the 4-byte bug
dies with the code. Version control is the cache for maybe-futures. Deletion
lane prune-2 dispatched with the probe's sized lists (~1,600 lines) plus the
verify-then-delete of HIR-WORD's test-only reason machinery the probe
surfaced. The reachability probe itself stands - its lists are the map.
