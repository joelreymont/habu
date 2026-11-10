---
title: "A program's PUBLICATION-USE changes the running hb: warm differs from cold"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T00:02:21.387751+02:00"
---

Problem: CHECKER-REG:PUBLICATION-USE (src/core/checker.f:18178, public; PUBLICATION-CLEAR :18174) stores an observer xt into the running hb's owner record, and every later definition's publish runs it (RECORD-PUBLISHED :18192). No object carries the store, so a part that calls it at top level and links on the warm build leaves it unset: probe ~/.cache/tmp/carl-aud2/p3 prints 4 1 2 3 cold and 0 1 2 3 warm, rc 0 both. Neither word has a caller in Habu, Zen, Tender, Maki or Etch; src/habu/aot-capture.f:720-721 keeps them by name. Acceptance: both public words, PUBLICATION-CLEAR-OWNER and CAST: PUBLICATION-CELL are gone (checker.f:18164-18191, RECORD-PUBLISHED stays private) with the two keep-list rows; a program that says either word is refused as undefined; the engine reaches a fixpoint. Waiting case, not this dot: the slot's readers (checker.f:18195-18199, TRANSFER-PUBLICATION :29461-29466, habu2.f:3520-3522, kernel-x64.f RECORD-PUBLISHED,) go with the hook removal. Files: src/core/checker.f, src/habu/aot-capture.f. Verify: the chain check; the probe refused. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-pub. Found by the audit, carl-layout/.out/handoff-notes-22.md.
