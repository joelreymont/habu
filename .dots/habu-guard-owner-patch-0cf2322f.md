---
title: Guard owner patch spans
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T06:25:51.547371+02:00"
---

Problem: checked TRUSTED patch32 wrappers, including breakpoint helpers, can overwrite owner marker fields, the canonical namespace, or their shared external name bytes. Result: reopen package OWNER-GUARD and add `SPAN? ( -- )`, emitted with target x9 and byte length x7 and boolean output x13. It rejects addition overflow and returns true exactly when the half-open span intersects a live marker record, its earlier same-name canonical namespace record, or their shared DNAME-EXT bytes. BPATCH32 calls it for its four-byte target before mprotect or store and exits through the existing uncatchable protected-region path on overlap. XREF retirement, breakpoint helpers, and every future wrapper share this primitive sink. Owner: OWNER-GUARD span predicate and native patch32 sink only; bootstrap uses the same builder path. Production red: BP+ accepts XREF-REC-ADDR plus the marker flag offset and clears DNAME-INT. Acceptance: marker record, canonical record, external-name, and overflow targets fail closed in child processes before authority bytes or permissions change; adjacent spans and ordinary executable breakpoint targets still patch and restore; generic debugger behavior, snapshot, inline AOT, refresh, and fixpoint pass. Forbidden: debugger API migration, catchable partial failure, broad code-range rejection, new registry, compatibility, or lint. Smallest owning check: BP+ targeting marker flags and long-name bytes fails without authority mutation while a real word XT succeeds. Claim: unassigned.
