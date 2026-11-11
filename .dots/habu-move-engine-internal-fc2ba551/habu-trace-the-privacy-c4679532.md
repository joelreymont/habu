---
title: "Trace the privacy design's seven untraced items"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T08:59:02.505246+02:00"
closed-at: "2026-11-11T09:25:46.115925+02:00"
close-reason: "done: both census reports read; results in .jj-ws.noindex/jerry-wasm/.out/privacy-design.md (last section) and in step 1's brief"
---

Problem: the privacy design (.jj-ws.noindex/jerry-wasm/.out/privacy-design.md, 'Not determined') lists seven items nobody traced: whether a built engine keeps the kernel seed table whole and whether a cell locates it; how a primitive new to the host gets bound; what the Wasm link reads for kernel words; whether the open's private records are written to the target; which checker sites assume a build's prefix holds the host's primitive rows; whether arm64's package runs in packages.f or machine code; tools/check.f's static package model. Steps 1 and 2 cannot be briefed without them. Acceptance: each item answered with path:line or a probe, observed kept apart from inferred. Files: none changed (read-only). Verify: the two reports. Briefs: .jj-ws.noindex/jerry-wasm/.out/briefs/priv-census-a.md (build side, items 3-6), priv-census-b.md (checker and packages, items 1, 2, 7). Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/priv-census-a and .jj-ws.noindex/priv-census-b.
