---
title: "Seal the engine's packages at the build boundary"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:44:39.049164+02:00"
---

Problem: no build step seals engine packages (tools/hb-build-core.f bodies), so 256 of hb's 318 top-level packages reopen and a program compiled in a build republishes a private (squat.f: CHECKER-RESOLVE and xref-search-wl). Acceptance: every package the engine files made is sealed before program source in ENGINE-BODY, REPL-BODY and APP-BODY; native app builds bake task.f and ffi-abi.f first; p1.f, squat.f and e2.f (/tmp/claude-501/seal/) exit 84; cold then warm builds are byte-equal. Design: .jj-ws.noindex/jerry-seal/.out/seal-design.md. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/seal-pass.
