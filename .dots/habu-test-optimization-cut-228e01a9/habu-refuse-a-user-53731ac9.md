---
title: Refuse a user X;does before X does>
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:24:58.103796+02:00"
---

Problem: the duplicate wall is asymmetric for a definer's clause record. `: MKV;does ( -- n ) 5 ;` then `: MKV ... does> ... ;` loads with both rows live ($HOME/.cache/tmp/kestrel-r4-rev178/dup-before.f): MKV;does answers the user's 5 while a word made by MKV runs the clause (7); the reverse order is refused rc 78 (dup-after.f). habu1.f:3225-3245 states the hash probe's precondition (at most one live row per folded name per wid, kept by C-REJECT-DUP-DEFINITION), and J-DOES publishes the clause without that wall (aot-capture.f:1658); the AOT seed resolves the clause by name in another engine (habu2.f:3305), so it would answer the user's word. Found by review 178 (acap-redef). Acceptance: the definer refuses at does>/clause publish with E-DUPLICATE-DEFINITION naming X;does, rc 78 with a located diagnostic, on both the native path (publish.f PUBLISH-PENDING-DOES) and the x64 twin (kernel-x64.f DOES-RECORD,) if that path publishes the clause; dup-before.f refused, dup-after.f unchanged; a definer whose clause name is free unchanged; failing case first through the real load path; rebuild, g1 = g2 with .names, two-generation build. Base: after acap-redef (dot f2de994d). Files: src/habu/habu2.f DOES-REC, src/compiler/native/publish.f, its test.
