---
title: "TFAM 2b: sealed system packages + friend latch + provenance"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.915549+02:00\""
---

PLAN.md item 2 (sealing half). Sealed TFAM/TYPE/MATCH packages; boot-latch friend capability set during canonical tools/srclist.f engine load, sealed before any user source (user --source-list never friend). Wordlist-layer guards for the full mutator census (set-current/search-wl/tick/execute/postpone/compile,/XREF*/CHECKER-*/raw stores/atomics/here-allot/cp@/immediate/undefine paths); pointer-provenance rejection for syscall/FFI writers vs protected regions; case-insensitive; native+habu1+Gforth mirrors. Fixtures per item 2 acceptance. Gate 17b. Depends: TFAM 2a.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim. The TFAM 2b umbrella was dissolved, not delivered (commit 150be3a2f archived TFAM 2b-iii); a claimant must re-derive its leaves before dispatch.
