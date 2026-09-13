---
title: Preserve verified input constraints when publishing JIT definitions
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-13T16:34:56.307345+03:00\""
blocks:
  - habu-keep-a-row-f2c4f3d4
---

Problem: tier0 republishes textual declarations and erases inferred fixed input cells/types, while tier1 preserves the verified graph. Safe compile-only reproduction on host28e11361: 0 set-tier; : ROW-ADD ( R -- R ) 1 + ; : WRONG ( ptr u8 -- ptr u8 ) ROW-ADD ; is accepted. Changing tier to1 rejects WRONG with expected n actual ptr u8. No wrong body or empty-stack call was executed. RB1 ( R -- R ) dup drop likewise records min-input0 in JIT versus1 in native. Fix JIT definition publication to retain the same verified cell/type/minimum-input constraints and declared row-kind semantics as the native path, without NEW/reparse erasing live facts. Do not simply copy the original scheme or weaken checker rejection. Acceptance: all provider/caller tier pairs reject the wrong-type caller, retain minimum-input guards, and preserve higher-order named-row positives/anonymous-window restrictions; ordinary REPL/load remains JIT. Files: checker.f effect publication, check-hook/source-owner/JIT finish publication path as proven; test/compiler/native-provider-rows.f plus actual REPL/load checks. Depends: provider-row fixf2c4f3d4; coordinate pending owner/tier1dc23a17. Ownership: unassigned. Verify: fresh rebuild, focused matrix, native suite. Use one verified publication contract; no duplicate helper or lint workaround.

Claim: Astra JIT-publication lane; Cedar independent review/integration. Source scope: tier0 verified-effect publication and parity regressions; coordinate checker.f with source-owner lane.
