---
title: Install literals as the pool places them
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T21:55:04.636218+02:00"
---

Problem: test/native-file-auto-e2e.f SEGMENT-EVENT-CASE (125 127 132) on the lane 11 tip: a warm install meets a string segment the file's own top level split since cold; DEF-READ-LITERALS (tools/native-unit-object.f) BADs on the recorded literal sharing (-8597) and the build dies rc 67. Triage: carl-fauto/.out/triage/auto-triage.md section S. Acceptance: an install over any pool state gives the image a live compile gives; no BAD for a pool the object did not see; -- segment passes; P0 warm == cold at OWN's counts. Files: tools/native-unit-object.f (dc SPIKE-HOOK only if needed). Verify: seg-repro red/green, -- segment, check.f, p0.sh. Depends: OWN (zxztprpo). Ownership: lane S. Workspace: .jj-ws.noindex/carl-flit, brief carl-dots/.out/carl-file/brief-fix-litshare.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-flit.
