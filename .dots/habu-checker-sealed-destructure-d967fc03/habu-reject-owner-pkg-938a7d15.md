---
title: Reject owner package reopen
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:48:17.715170+02:00"
blocks:
  - habu-parse-owner-construction-d876c9ef
---

Problem: source can write package NAME after an owner-construction product has closed, making the attacker appear to be the declaring owner. Result: the package compiler uses canonical XREF and the existing family metadata to reject reopening any package that owns a DRV-CONSTRUCT-OWNER product. Initial declaration remains legal while that package is active; after end-package there is no reopen path. Direct, qualified, mixed-case, evaluated-string, included-source, replay, JIT, and AOT forms use this one compile-time rule. Add no runtime registry, protected WID table, snapshot payload, persisted owner ID, source allowlist, friend latch, exit code, or compatibility exception. Owner: package declaration checking plus existing XREF/family queries only. Production red: package MDLCFG can be reopened after its owner product closes. Acceptance: packages without owner products still reopen; every owner-package reopen rejects with one named checker diagnostic before current-wordlist mutation; initial owner source compiles; hostile comments, strings, numeric WIDs, and nested evaluate cannot bypass; package, XREF, declaration, AOT, native fixpoint, and exact diff gates pass. Claim: unassigned.
