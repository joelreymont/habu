---
title: Load one engine-library list in every build
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T17:48:39.120700+01:00"
closed-at: "2026-11-18T17:52:51.522636+01:00"
---

Problem: tools/hb-build-core.f loads ffi-abi, task and c2-memory before the seal only in a stripped program (APP-FILES), and fork-child, codegen and ffi-callback before it in no build, so their packages cannot take private rows (3499c957). Acceptance: hb, hb with a program and a stripped program load one engine-library list before SEAL-ENGINE. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-d4.
