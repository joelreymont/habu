---
title: "Keep a carried record through finally's cleanup"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.364799+01:00"
closed-at: "2026-11-16T04:07:35.777648+01:00"
---

Problem: src/habu/habu1.f BFINALLY and BC2-INVOKE and src/arch/wasm/link-routines.f FINALLY-BODY treat any move of the throw count as a cleanup throw, so a cleanup that throws and catches inside drops the body's carried record. Acceptance: the body's record prints when the cleanup catches its own throw, on arm64 and Wasm, pinned by tests. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
