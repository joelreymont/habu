---
title: Delete the internal-record lookup tests left after c3186136
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:02:17.288901+01:00"
---

Problem: src/habu/interpreter.f POLICY-CHECK-REC's 'rec XREF-WORDLIST OWNER-API-PRI-WID = if ... POLICY-REFUSE' line looks unreachable (wordlist 2 never gets an admission bit, so the wordlist test above rejects first), and src/compiler/native/dict.f VISIBLE-RECORD?'s DNAME-INT branch (:40-43) may answer as its fallthrough does: in a TRUSTED: body both give true; in a checked body the checker rejects first (E-CAP-TRUSTED); a top-level tick gives E-UNSAFE (probes in the c3186136 lane). Acceptance: each is deleted with its comment, or the program whose answer it changes is named. Depends: none. Claim: unassigned.
