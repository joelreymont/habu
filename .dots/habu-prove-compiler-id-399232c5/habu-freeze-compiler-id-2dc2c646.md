---
title: Freeze compiler ID manifest
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T13:50:14.622291+02:00"
blocks:
  - habu-add-compiler-ir-21e976fc
---

Scope: freeze the exact IR-0.1 identities only: IR-ID:ir-module-key and every IR-ID ID family. Implement one checked Habu canonical schema manifest, its digest, and one shared artifact containing valid and hostile reachable numeric key/serial/owner/local/bound vectors. Keep wrong-family rejection as a separate checked static fixture, and keep require replay as a separate executable load-path fixture; neither is encoded as a runtime numeric vector. Acceptance: deterministic bytes and digest; every IR-0.1 family appears exactly once; hostile runtime vectors cover every reachable numeric guard; the static wrong-family fixture is named; focused checked manifest/vector test passes. Ownership: checked ID manifest, digest, shared numeric vector artifact, and the static fixture reference only. Excludes Rocq model/theorems/parity gate, allocator proof, replay implementation, shared records, tables, opcodes, general witnesses, dialects, native/GPU, and maki. Depends on habu-add-compiler-ir-21e976fc.
