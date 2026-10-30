---
title: "shadow: compile for the host alone while a shadow is open"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:41:06.921186+02:00"
---

Problem: five rows (compiler-shadow, x86-64-link-records, aot-shadow-capture, wasm-backend, wasm-capture) die rc 67 on E-T0-SHADOW (-9605): T0:RUN refuses any definition while a target shadow is open. Rule: a tier-0 colon definition carries no shadow routine, as under the JIT. Acceptance: the five rows rc 0. Brief /tmp/jerry-big/t0-shadow-brief.md. Base mnzvrsym (stack tip). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-shadow
