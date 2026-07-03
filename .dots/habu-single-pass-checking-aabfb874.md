---
title: "Single-pass checking: kill body re-parse"
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:31:41.054634+02:00"
---

Every definition is parsed twice: JIT compiles tokens while LBCAP/EMIT-BCAP (src/habu/habu1.f:1798-1817) copies body source into BODYBUF (appended per token, habu2.f:2626); at ; the publish hook hands the raw text to the checker which re-tokenizes and re-FINDs everything (EM-COMPILE-PUBLISH-HOOKED habu2.f:2539-2551). Fix: feed the checker an interned token/xt stream recorded during the single compile pass (tok -> resolved xt + effect ptr already known at compile time), so the checker unifies over pre-resolved refs instead of re-parsing text. Big constant-factor win on every checked load; keeps fail-closed property (capture overflow still fatal, habu1.f:1795-1810).
