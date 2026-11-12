---
title: Compile once when host and target share an instruction set
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:56:46.373634+02:00"
---

Problem: src/habu/linker.f CROSS? holds whenever the target's profile differs from hb's (src/compiler/target/model.f SAME?), so a linux-aarch64 build on a macOS arm64 host compiles every definition twice through the same arm64 emitter (src/habu/t0.f LOWER: SHADOWED, then HOSTED). Source: Joel, 2026-11-12: 'the fact that we are doing arm-to-arm via xcomp is our own self-limitation'. Not measured: how many of a build's two emissions are byte-identical, and what in the rest differs. Acceptance: that count is measured on the engine's linux-aarch64 build; a build whose target shares the host's instruction set compiles a definition once wherever the two emissions agree, and its linux-aarch64 engine passes the Linux proofs it passes today. Depends: none. Claim: unassigned.
