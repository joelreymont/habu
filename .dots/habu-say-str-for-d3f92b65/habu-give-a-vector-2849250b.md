---
title: "Give a vector's bytes as a str with VEC:BYTES"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-25T09:54:04.572457+01:00"
closed-at: "2026-11-25T09:54:04.587270+01:00"
---

Problem: 202 sites in 80 files read a vector<u8>'s bytes as a str with VEC:SPAN SPAN:$. Joel 2026-11-25 (interview) named the word VEC:BYTES. Acceptance: VEC:BYTES ( ^vector<u8> -- str ) in src/lib/vector.f; no VEC:SPAN SPAN:$ left in src, tools or test. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-vbytes.
