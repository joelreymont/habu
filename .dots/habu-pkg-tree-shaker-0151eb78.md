---
title: Package tree shaker
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:29:19.020770+02:00"
---

src/habu/treeshake.f:7-158 exposes 32 package-less definitions, including SHK-* plus generic SCAN, KEEP?, NEXT-TOK and mutable lexical/closure state. It is active in bootstrap/fixpoint build-time closure reduction, but callers need only whole-source shake entry points. Put it in package TREESHAKE, export FROM/TOPLEVEL or the exact minimal public API proven by callers, keep scanning, token, keep-set, buffers, and traversal state private, and update build callers without forwarding globals. Preserve tokenization, reachability/keep decisions, source order, comments/literals, malformed-input errors, emitted source bytes, and resulting engine fixpoint. Add old-global/private rejects, qualified public positives, adversarial lexer/closure cases, and exact source/output goldens. Measure dictionary-name bytes, JIT/DATA/CODELEN, scratch storage, closure size, and shake/build latency before/after. Verify treeshake/source-closure/bootstrap/recovery/fixpoint/AOT/snapshot gates, package/host/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c.
