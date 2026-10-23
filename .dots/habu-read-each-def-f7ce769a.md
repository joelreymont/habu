---
title: Read each definition once
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:43.070920+03:00"
---

Target: docs/compilation.md 'Defining a word' (one reader; the checker records checked events; the platform codegen compiles from them). Today a definition is read three times: the machine-code reader compiles it through tier 0 (src/habu/habu2.f, src/habu/jit.f, pass-2 replay), the check hook re-reads the source into a token tape (src/core/check-hook.f, src/compiler/native/tape.f), and tier 1 rebuilds its structure (src/compiler/native/elaborate.f, hir-word.f). Control-word meaning is written about nine times. ARM64 keeps both tiers (Joel, 2026-10-23); both must read the same events. Acceptance: one reading per definition; tape token stream, second check pass, elaborator and pass-2 replay deleted; native suite and two-generation build green; tools/compile-floor.f before and after. Needs a design before any lane (Codex Astra round 2026-10-23, ~/.cache/tmp/jerry-onereader/). Claim: unassigned.
