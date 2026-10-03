---
title: Keep LASTC on a live record after a forget
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-30T18:38:44.308861+02:00\\\"\""
closed-at: "2026-10-01T12:54:48.161517+02:00"
close-reason: Fixed by tvnszzzm 45b43804 (r4-repl lane, review 127 ACCEPT)
---

Problem: src/habu/xref.f:587 FORGET-DEFS-FROM lowers ndict (idx ndict! at :592) and never touches LASTC-CELL, so after a forget LASTC names a record slot past the last live record. The next definition reuses that slot, and a following does> patches that unrelated live record. Reduced (d40cc36d engine, rc 102, hb: stack bounds exceeded (data)): create R4-KEEP 7 ,  : R4-MARK ( -- ) ;  create R4-GONE 5 ,  s" R4-MARK" FORGET-DEFS-FROM  : R4-BEHAVE ( -- ) does> ( -- n ) @ ;  R4-BEHAVE. After the forget LASTC still points at R4-GONE's slot; R4-BEHAVE's ;does companion reuses it; does> patches it. Found by the --repl reproducibility lane. Acceptance: after a forget, LASTC names the last surviving created record or none, so a does> with no live created record refuses with a named error instead of patching; the reduced case is a test through the real load path, seen to fail first. Files: src/habu/xref.f, the owning test. Verify: the new case, the forget and xref suites, native build and two-generation chain. Depends: habu-make-two-repl-bde9f316 (same cell, same lane). Ownership: LASTC across a forget. Claim: agent=kestrel workspace=.jj-ws/r4-repl.
