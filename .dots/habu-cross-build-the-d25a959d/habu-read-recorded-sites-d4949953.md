---
title: Read recorded sites in the capture
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.983183+03:00"
closed-at: "2026-09-30T09:26:25.115454+03:00"
close-reason: X2a landed as c0e2d857 (interdiff empty)
---

Problem: `src/habu/aot-capture.f:1-14,62-80,1673-1684` discovers calls by decoding `BL` (`ACAP-CALL?`, `ACAP-TGT`, `ACAP-ZERO-IMM`, `ACAP-SCAN-CALLS`) and reads address chains as MOVZ/MOVK.
Acceptance: `ACAP-SCAN-CALLS` and the chain scan are replaced by `SITES:EACH-IN-SPAN` over the window (ARM64 arm: the bitmap bits, decoding only the value at a recorded site); target identity through `ACAP-TGT>REC` as today; the `.names` sidecar and captured bytes identical for the same window (chain gen 5 byte-identical); the tier-0 window of the stdin route captures identically (both compilers record the bits: `habu2.f:658-676,6838`).
Files: `src/habu/aot-capture.f`, `src/habu/sites.f`.
Verify: spark: rebuild; chain to gen 5 byte-identical; the stdin-route capture compared; gate.
Depends: habu-represent-x86-live-729a7ac6 (P2). Serialise with X2b and I7 on `aot-capture.f`.
Route: Alder (shared: src/habu/aot-capture.f, src/habu/sites.f).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-read-recorded-sites-d4949953.
Preflight note from P2: the ARM64 bitmap arm of `SITES:EACH-IN-SPAN` yields region-to-text calls only (`habu2.f:659-668`, `5565-5567`), while `aot-capture.f:1679-1690` (`ACAP-SITE-HERE`/`ACAP-BRANCH-HERE`) also resolves in-region, out-of-window calls and B branches by name. Replacing `ACAP-SCAN-CALLS` with the bitmap arm must account for those before claiming a byte-identical capture.

Preflight corrections (these override the lines above where they differ):
- Acceptance: the chain scan (`ACAP-CHAIN-BIT?` in `ACAP-SCAN-DSITES`/`ACAP-SCAN-CSITES`, `aot-capture.f:1702-1705,1798,1859`) is replaced by `SITES:EACH-IN-SPAN` over `bstart AOT-DBASE-N -` / `AOT-BLOB-LEN @`, consuming its `SNAP-RELOC:SITE-ADDR` yields; the two sweeps agree bit for bit (`sites.f:39,55-62`) and both walk ascending. Quotations may not touch enclosing locals (`docs/forth-card.md`), so `bstart`/`bend` move to variables as `d0`/`d1` already are (`aot-capture.f:1795`).
- `ACAP-SCAN-CALLS` keeps its BL/B decode on ARM64. No writer records a region-to-region call (tier 0 `habu2.f:672-673`, the seed patch `5566-5567`, tier 1 `publish.f:67-68` gate on `EXTERNAL?`), nor the does> `B` that LDOESPATCH writes at run time (`habu2.f:3059-3076`), and those are most of the chain's call sites (`aot-capture.f:91-92`). Recording them would change engine bytes and the `EM-SNAPSHOT-REBASE-CALLS` contract (`habu2.f:652-654`); that is not this leaf. `SITE-CALL` yields are not consumed on ARM64; x86 calls come from the shadow rows (X2b).
- The address-map writers are `EMIT-ADDR-SITE`/`C-CODE-ADDR` (`habu2.f:6845`) and `publish.f:73-75` (not `habu2.f:6838`).
- Files add: `tools/build-fixpoint.f`: `BF-EMIT-STDIN-RUN-SOURCE` (`:1144-1153`) gains `lib/le.f` and `src/habu/sites.f` `BF-APPEND-MODULE` rows before `aot-capture.f`, as `terminal-call.f` has (`:1151`).
- Verify (spark, workspace tree, `bin/hb` = master's `a3a6224b…`): `HB_TMP=$T bin/hb --load tools/two-generation-build.f -- bin/hb` prints `two-gen: ok gen 3 matches gen 2` and `two-gen: ok gen 5 matches gen 4 byte for byte`; every generation's sha256 equals `a3a6224b…` (a capture-rule change already shows in gen 1, `docs/bootstrap.md:281-285`); `cmp` gen 5's `.names` against the same run on master. Stdin route: build `hb-stdin` (`tools/build-fixpoint.f:1643-1652`) on master and on the change under separate `HB_TMP`s; `cmp` equal. Then the gate.
- Base: master. Route: Alder.
