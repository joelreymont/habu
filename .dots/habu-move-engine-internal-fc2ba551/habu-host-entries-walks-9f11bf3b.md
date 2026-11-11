---
title: "HOST-ENTRIES walks the host's seed table"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T10:22:50.466468+02:00"
---

Problem: privacy step 2a (.jj-ws.noindex/jerry-wasm/.out/privacy-design.md Design 2): src/arch/arm64/link-kernel.f HOST-ENTRIES walks the running hb's dictionary to map each kernel-object primitive row to the host's body, so a host whose private names are stripped cannot link. Acceptance: it walks the host kernel's seed table through PRIM-TABLE-CELL (name and wordlist to ROW-OF unchanged); a host with no table dies 74. Files: src/arch/arm64/link-kernel.f. Verify: an engine linked by the old walk (built by the installed hb) equals one linked by the new walk (built by that engine) up to the signature, maps equal; test/file-objects.f rc 0. Depends: step 1 (15fa640e, installed). Ownership: the lane. Claim: agent=jerry workspace=.jj-ws.noindex/priv-2a (brief .jj-ws.noindex/jerry-wasm/.out/briefs/priv-step2a.md).
