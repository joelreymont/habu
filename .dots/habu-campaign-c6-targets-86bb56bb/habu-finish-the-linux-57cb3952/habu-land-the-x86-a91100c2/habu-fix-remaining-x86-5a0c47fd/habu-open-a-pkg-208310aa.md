---
title: Open a package part on x86-64 without exit 83
status: active
priority: 2
issue-type: task
created-at: "2026-11-03T12:01:47.413276+02:00"
---

Problem: on x86-64 opening a package part exits 83 (ENGINE-ERROR:SEAL-VIOLATION) with no output: `1 . package OI-A private 2 . package OI-B 3 .` prints `1 2` (arm64: `1 2 3`), through the engine loop and the Habu loop (found under habu-fix-remaining-x86-5a0c47fd). Inferred, not proven: the same exit stops `hb --load tools/hb-build.f` on x86 at tools/cold-load.f staged line 102 (native-runtime.f's cold reload, whose T0 files open the COMP parts), which blocks every x86 app and engine build, ffi-cabi's whitebox engine build, engine-stack-machine's stripped case and ffi-callback's CASE-DIRECT/CASE-STRIPPED/transcript case (lane cbx86's finding, 2026-11-03). Acceptance: the one-liner prints `1 2 3` rc 0 on pulsar; outer-interpret and compiler-native-string-forms pass their part cases; whether hb-build's 83 was the same cause is recorded. Files: src/habu/kernel-x64.f (silent SEAL-VIOLATION exits) or the layer the root cause names. Verify: x86 cross-build, the rows on joel@pulsar-ua-106; arm64 candidate and the rows if an arm64-built file changes. Depends: none. Brief /tmp/jerry-big/x86part-brief.md. Ownership: the files the fix names. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x86part.
