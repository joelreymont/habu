---
title: Say str for include words policy tests call
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T15:51:49.890447+01:00"
---

Problem: test/policy files have no split-str line, so SOURCE-ROOT ENGINE$ RESOLVE RUN-PROGRAM, included, required and SCRIPT-ARGV$ keep ^u8 n rows. Acceptance: those rows say str, the policy files pass one str value, the engine builds and vocabulary passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-incl
