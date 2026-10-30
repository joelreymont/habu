---
title: "Install TRUSTED: definitions from file objects"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:12:08.077397+02:00"
---

Problem: warm-diag.md T: the TRUSTED: head (habu2.f:5512-5521) never runs C-BODY-HOOK and dc:571 admits it only as trusted; 48 defs in 16 reused files compile live. Acceptance: TRUSTED: payloads install, head kind in the row, warm == cold. Files: src/habu/habu2.f, tools/native-definition-cache.f, tools/native-unit-object.f. Verify: P0 on the lane, the lane's head-kind case. Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-fix-t.md. Depends: none. Ownership: TRUSTED: head, dc dispatch. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-frt
