---
title: "Release link-band.f's tables when a build closes"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T05:34:10.942194+02:00"
---

Problem (found by the F1 landing review, 2026-11-07): src/habu/link-band.f declares DYNAMIC-BUFFERs OFFSETS, ENTRY-KEYS, ENTRY-USED, ENTRY-RECS, EM-FIRST, REC-NEXT, PLACED, LIVE, PLACE-WORK, LIVE-WORK and UNSAFE-RECS (:57-91) and no word releases them, so after a build that writes a program they stay reserved until the process exits. src/habu/linker.f DISPOSE-OBJECTS releases every table of its own at the build's close (CLOSE's objects step). Retained memory only; nothing reads stale rows, as each link reserves and fills them again. Acceptance: the build's close releases link-band.f's tables through one link-band.f word that linker.f's close calls; no other change. Files: src/habu/link-band.f, src/habu/linker.f. Verify: the four suites (linker arm64-link-data arm64-link-run file-objects). Depends: none. Ownership: carl. Claim: unassigned.
