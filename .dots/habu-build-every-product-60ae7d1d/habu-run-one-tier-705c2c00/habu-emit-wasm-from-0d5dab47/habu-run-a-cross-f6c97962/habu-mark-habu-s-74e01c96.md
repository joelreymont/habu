---
title: "Mark Habu's sources with host sections"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T10:43:46.177347+02:00"
---

Problem: once a cross-build refuses a target word at top level, every library word a build runs must sit in a host section. Acceptance: Maki's viewer builds for Wasm and the engine builds for linux-aarch64 with the refusal on, each writing what it wrote before; no host section holds a word no top level or host word names. Depends: the refusal child. Claim: unassigned.
