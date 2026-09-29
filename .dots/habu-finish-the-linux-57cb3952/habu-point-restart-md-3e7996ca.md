---
title: Point RESTART.md at master and INTEL.md
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:29.017350+03:00"
closed-at: "2026-09-29T13:40:31.256316+03:00"
close-reason: RESTART.md points at master and INTEL.md
---

Problem: `RESTART.md` names `hazel/integration` and `cedar` as accepted heads.
Acceptance: `RESTART.md`: the accepted head is `master`, read with `jj --ignore-working-copy log -r master@origin`; the `hazel/integration`/`cedar` names dropped; the x86 lane pointed at `INTEL.md`.
Files: `RESTART.md`.
Verify: read-through; the command named runs.
Depends: none.
Route: direct (documentation no build loads; the Alder route exists for files a macOS build loads).
Ownership: krait (Intel lane).
Claim: unassigned.
