---
title: A link replays a retirement before a late-private entry the compile made first
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T21:01:24.039046+02:00"
closed-at: "2026-11-10T23:37:09.207803+02:00"
close-reason: "landed in 'Replay a retirement only before a record's entry'; suite 273 of 273 and the measure green on the landing tip (Maki, Zen, Tender warm equal to cold)"
---

Problem: src/habu/linker.f LINK-OBJECT runs REPLAY-DUE before every entry, KIND-PRIVATE ones included, so a retirement whose count equals the records so far replays ahead of a LINK-PRIVATE the compile made first (feature review 2, finding 1, by reading). It matters only when the retired record is that package's own record: PACKAGES:PACKAGE-PRIVATE then throws E-COMP-STATE and the warm build fails once. undefine cannot reach a package record (XREF-FIND-TARGET-INDEX looks in the current wordlist or a package's public one; package records live in BOOT:NAMESPACE-WL), and a file starting 'BOOT:NAMESPACE-WL set-current' exits 83, so no program is known to reach it. Acceptance: REPLAY-DUE runs only before entries that make a record; file-objects passes. Files: src/habu/linker.f LINK-OBJECT. Verify: file-objects; a fixture if a reaching program is found. Depends: none. Ownership: unassigned. Claim: unassigned.
