---
title: Check an enum fetch without a descriptor walk
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T09:08:12.438511+02:00"
closed-at: "2026-11-10T21:50:20.544631+02:00"
close-reason: "done: landed as 0d4c03bf \"Check a one-row tag inline in tier 0\" (src/habu/t0.f:1178-1202 TAG-INLINE; test/layout-valid-w1-neg-bad.f keeps BAD-TAG); the probe's share was not measured again"
---

Problem: a fetch of an enum- or sum-typed cell runs NFETCH-CHECK:TAGS (src/compiler/native/fetch-check.f), whose RUN-ROW walks the frozen layout descriptor through SLOT@ -> LE:U64@ -> NATIVE-SLOT?, about 30 calls per fetch. Measured on carl's scale probe (.jj-ws.noindex/carl-dots/.out/kept/c5a2/scale: big.f, 863 KB, 30,000 words in 300 packages, built cold then warm by .jj-ws.noindex/carl-dots/.out/kept/c5a2/scale/build.f with /tmp/carl-c5a2c/hb from .jj-ws.noindex/carl-c5a2): NFETCH-CHECK:TAGS is 146 ms of a 758 ms warm build, from NSHADOW:OPEN-CK/OPEN?/EM-CK (src/compiler/native/shadow.f SH-STATE @ MATCH) on every emission access; LINKER's reader had the same cost (READER @ per u64) and stopped asking per read on 2026-11-07. Acceptance: a fetch whose descriptor has no field rows checks its tag with one bound compare, or the descriptor's cells are read as native cells; the probe's TAGS share falls below 2%; a fetch of an out-of-range tag still dies ENGINE-ERROR:BAD-TAG. Files: src/compiler/native/fetch-check.f (and T0's call site if it changes). Verify: the probe's profile (prof.f there); the bad-tag test the suite has. Depends: none. Ownership: fetch-check.f's owner (T0: jerry). Claim: unassigned.
