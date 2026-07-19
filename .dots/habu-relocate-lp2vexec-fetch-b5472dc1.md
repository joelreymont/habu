---
title: Relocate LP2VEXEC fetch call in stripped AOT images
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-19T16:31:42.620271+02:00\\\"\""
---

Found during the PROT-GUARD:CALL slice audit of every C-CALL-EMIT-ABSOLUTE site: a stripped AOT image whose MAIN does a layout-value fetch (@ through the pass-2 wide-fetch lowering LP2VEMIT, src/habu/habu2.f:~4829) crashes SIGSEGV (exit 139) because the emitted absolute movz/movk+blr targets LP2VEXEC, which is a bare unregistered label — FINDADDR cannot resolve it, so the stripped linker's abs-to-BL collapse (COPY-COMPACT-BLOB DENSE path) never rewrites it and the build-time engine address ships. Pre-existing on master, independent of the guard fold; the store path (LP2STORE to the registered (PROT-SPAN)) is covered and pinned by the GAP-LAYOUT-STORE regression in test/gate-aot-positive-lib.f. Fix is more than registration: LP2VEXEC's body writes its invalid-tag diagnostic via ADR to engine message data (LVPBADMSG/LOPENNL), which cannot be relocated in a stripped image, so the fix must register LP2VEXEC as an engine helper AND make those message references relocatable (for example inline the message bytes within its record). Add a negative-turned-positive regression: an AOT MAIN doing a layout-value fetch must run clean, mirroring GAP-LAYOUT-STORE. Verify: the gate-aot positive slice plus a stripped-image scan proving zero un-collapsed blr remnants.
Claim: agent=lp2vexec workspace=.jj-ws/habu-relocate-lp2vexec-fetch-b5472dc1
Claim: agent=lp2vexec workspace=.jj-ws/habu-relocate-lp2vexec-fetch-b5472dc1
