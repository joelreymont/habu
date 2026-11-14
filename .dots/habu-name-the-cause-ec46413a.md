---
title: Name the cause when an engine too old for its tree stops a build
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T21:05:33.577905+01:00"
---

Problem: an engine built before the item definers (the installed master engine) building the tree at qspnuwzq 'Build FUNCTION:'s word from items' (src/lib/ffi-abi.f) stops with the bare line 'hb-build: src/lib/c2-owner.f threw 7103' (E-SCHEMA-BAD): no record says what failed, where, or that the tree needs a newer engine (docs/bootstrap.md staged hosts). Acceptance: the same build prints a Rust-style record naming the place, the source line and the cause, with the fix; the root cause of the throw is reported. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-t7103.
