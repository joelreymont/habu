---
title: "TFAM 2b-v: protected-WID registry + 2c persistence remainder"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T02:07:47.916998+02:00"
---

Sealing slice 5. The u32 WID field + WIDN advance already landed (aot-capture.f ACAP-W32@/AOT-P32!/ACAP-WID-SELFTEST; habu2.f EM-AOT-REGISTER-RECS). Remaining: the protected-WID registry itself (which WIDs are sealed/generated), its AOT seed capture/restore persistence, reject sealed/generated WIDs in record registration/relocation/bootrun, snap-rebase friend-gating, boot-time integration test with captured wid>255 protected records. Closes habu-tfam-2c-aot-d79c1fd0 remainder. Depends: 2b-i, 2b-ii.
