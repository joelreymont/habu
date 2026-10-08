---
title: Land L-B without the owned mark
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.788327+02:00"
---

Problem: the unpushed L-B stack (change ids, on master 9413eed4) has ten commits; four exist only to carry the owned mark, which ruling 1 (system package privacy) makes unnecessary: ytqquyyx "Add the owned-mark primitive", zqmowxov "Carry DNAME-OWNED through the AOT capture", uxswkvlr "Stamp owner-only prefix words owned", pxolrlpu "Refuse owned calls from unchecked bodies". Its gate lbj-g2 passed build, two-generation, full suite and census, but Gforth recovery exits 70: hb-stage0 compiling stage2-src stops at `xref-search-wl` (master 4628f37a's gate passed it; trace ~/.cache/tmp/heron-arm64/evidence/rev-tr-lbi/gf-trace.log).
Ruling (Joel, 2026-10-08): drop the four owned-mark commits; keep a remaining commit only if it stands without the mark.
Acceptance: each of xsmqutkt "Refuse set-check while a definition is open", rtonnmpz "Enforce the feed verdict at tier 1", vrmnronu "Publish on the checker's certificate" (reads OWNED-BOUND?), mlytkmsv, uxllpuov, spllluxk "Count the source-boundary search once" is either rebased onto master without the four and landed (reviewed, full suite green on the built hb) or dropped with the reason recorded; the Gforth failure is diagnosed on whatever lands.
Files: per commit. Verify: full suite on the built hb; Gforth recovery once for the landed set.
Depends: none. Ownership: carl. Claim: unassigned.
