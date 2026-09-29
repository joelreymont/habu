---
title: Write snapshots and --repl app images on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.817662+03:00"
blocks:
  - habu-represent-x86-live-729a7ac6
  - habu-resolve-x86-entry-cb671d4d
  - habu-run-bin-hb-6378f297
  - habu-build-stripped-images-b27cfad7
---

Problem: `SNAP:PERSIST` (`src/habu/snap-lib.f`) writes the engine's text and regions, and the ARM64 restore relocates through `snap-rebase` and the `SNAP-RELOC` passes (`EMIT-CALLS`, `EMIT-MARK`, `EMIT-XT`, `EMIT-ADDRS`, `src/habu/habu2.f:6350-6847`) because the ARM64 region is not at a fixed address. The full `snap-rebase` contract (`habu1.f:3994`, `snap-lib.f:130-158`) is read first.
Acceptance: with the region and DATA fixed on x86, `SNAP:PERSIST` writes the linked image shape and restore is the ordinary boot; `snap-rebase` and the `SNAP-RELOC` passes are not emitted on x86; the `app-image`, `snapshot-writer` and `snap` rows green on the ThinkPad.
Files: `src/habu/snap-lib.f`, `src/habu/link-x64.f`.
Verify: ThinkPad: the `app-image`, `snapshot-writer` and `snap` rows.
Depends: habu-represent-x86-live-729a7ac6 (P2), habu-resolve-x86-entry-cb671d4d (X4d), habu-run-bin-hb-6378f297 (X6), habu-build-stripped-images-b27cfad7 (R6).
Route: Alder (shared: src/habu/snap-lib.f).
Ownership: krait (Intel lane).
Claim: unassigned.
