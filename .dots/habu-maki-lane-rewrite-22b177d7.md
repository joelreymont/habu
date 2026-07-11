---
title: "Maki lane: rewrite 26 finder call sites for the string-finder option migration"
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T18:23:29.847199+02:00"
---

Coordination dot FOR THE MAKI LANE (tfam is barred from maki/*). The wave-A string-finder finale (full plan + caller census in .dots/habu-switchover-wave-a-54edcee6.md) is blocked ONLY on maki-owned call sites: STR>NUMBER? callers maki/store.f:267 maki/cad.f:334 maki/golden-artifact.f:262 maki/saved.f:65; FIND-SUB callers in maki/lower-{ew,mm,red,mv}-test.f (13 sites) + maki/ablate-ptx.f; INDEX-OF callers maki/store.f (3) maki/cad.f (5) maki/golden-artifact.f (1). When the maki lane rewrites these to MATCH (per the plan's commit A/B/D shapes), the tfam lane completes: INDEX-OF + FIND-SUB + CONTAINS? -> option<idx>, STR>NUMBER? -> option<n>, deletes the temporary adapters STR>NUMBER-UNWRAP + STR>OPTION (lib/string.f) and the orphan tools/string.f. Until then the adapters are the documented boundary (behavior byte-identical, tested). tfam side is boundary-complete; coordinate the window via this dot.

MAKI WINDOW EXECUTED 2026-07-11 (fable 9609501d): the 4 STR>NUMBER? sites are
rewritten to the plan's transitional shape (STR>NUMBER? STR>OPTION MATCH option
none->same-throw some->passthrough; store.f STORE-PARSE-INT, cad.f PARSE-INT,
saved.f SV-FBR-PARSE, golden-artifact.f GA-PARSE-INT-VAL) with new mutation-
tested miss-path negatives; gates green (maki 77/77 + 6 lints + tld). The 22
INDEX-OF/FIND-SUB sites are PROVABLY BLOCKED maki-side (fail-closed probe:
MATCH option over the raw-n finders rejects E-UNDEFINED 'option'; no
n->option<idx> bridge exists in lib), and the wave-A plan Q4 mandates commits
A/B as ATOMIC single-lane operations ("NO temporary adapters for A/B; all
callers rewritten in-commit").

ORCHESTRATOR DECISION (scoped lane-split waiver): the tfam lane is AUTHORIZED
to rewrite the 22 maki index-finder call sites IN commits A and B themselves
(atomic flip + all callers, per the plan's own design), scoped to exactly those
call-site hunks; the maki orchestrator reviews the maki hunks post-merge. This
beats a throwaway IDX>OPTION adapter the plan explicitly forbids. Residual maki
follow-up at commit D time: delete the one STR>OPTION token at the 4 rewritten
sites (maki micro-commit; the structural MATCH rewrite is already done).

TFAM SIDE COMPLETE (tfam-finale workspace, 2026-07-11): commits A/B/D landed
under the lane-split waiver. The 22 maki index-finder call sites were rewritten
in commits A/B exactly as censused (store.f x3 + cad.f x5 + golden-artifact.f
x1 INDEX-OF; lower-{ew,mm,red,mv}-test.f x12 + ablate-ptx.f x1 FIND-SUB), and
commit D removed the one STR>OPTION token at the 4 STR>NUMBER? sites
(store.f STORE-PARSE-INT, cad.f PARSE-INT, saved.f SV-FBR-PARSE,
golden-artifact.f GA-PARSE-INT-VAL) — the residual maki micro-commit is now
IN commit D, not outstanding. Both adapters (STR>NUMBER-UNWRAP + STR>OPTION)
and the orphan tools/string.f are deleted. maki/test.f 77/77 rc 0 on every
commit. Remaining action on this dot: the maki orchestrator's post-merge
review of the maki hunks; close after that review.
