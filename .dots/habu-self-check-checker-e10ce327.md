---
title: Self-check checker via fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:34:55.593845+02:00"
---

EMIT-HOST-LOAD-PREFIX (src/habu/habu2.f:412-415) zeroes HOOK-CELL and loads util/structures/checker/render UNCHECKED (hook lands after, load row habu2.f:358). TCB = ~5900 lines of asm-emitting builder Forth + ~4400 lines checker/renderer + 226 TRUSTED: defs and ~307 TRUST rows repo-wide (91 in habu2.f, 34 in roles.f); any TRUST row typo is an unchecked soundness assumption. Fix: stage the prefix load so the previous fixpoint binary CHECKS checker.f/render.f before baking them (fixpoint infra already rebuilds bin/hb from source); machine-audit TRUST rows - generate the trusted-boundary inventory and enforce a test per row. Would have caught the sig-clobber class earlier.
