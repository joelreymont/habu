---
title: Test a same-name record shifting later counts
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T20:52:56.438355+02:00"
closed-at: "2026-11-10T23:37:09.169466+02:00"
close-reason: "landed in 'Test a same-name record shifting counts'; suite 273 of 273 and the measure green on the landing tip (Maki, Zen, Tender warm equal to cold)"
---

Problem: No suite test covers a same-name record an earlier part gains or loses (linker.f IDENTIFY): a wrong binding in a warm build would be silent (feature review, owed test). Acceptance: a file-objects case whose program output names the X each user reached, asserted after cold and warm builds of both variants. Files: test/file-objects.f. Verify: file-objects passes. Brief: .out/briefs/t2-same-name-shift.md. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-t2.
