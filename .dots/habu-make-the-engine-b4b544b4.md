---
title: Make the engine build independent of its driver
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T18:54:49.899009+03:00"
---

Problem: two runs of tools/native-build.f are byte-identical, but adding a single `require lib/fs.f` to the driver changes the built engine at byte 143718 (a constant in (LP2VEXEC)), so tools/build-profile.f, which requires the profiler around the same driver, builds a valid engine that is not byte-identical to the shipping one (profiler lane, 2026-09-16). An engine's bytes should be a function of the source tree and the host engine, never of which tool invoked the build. Acceptance: find what the driver's own require closure leaks into the image (the constant, its owner, and why the driver's loaded state is visible to capture), make capture independent of it (or refuse a build whose driver state would differ from the shipping driver, by name), and prove it: an engine built by tools/build-profile.f equals one built by tools/native-build.f byte for byte; tools/two-generation-build.f and the fixpoint unaffected. Files: src/habu/aot-capture.f, habu2.f, tools/native-build*.f, tools/build-profile.f, test/. Verify: the equality; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: capture. Claim: unassigned.

Read-only reduction claim: alder, .jj-ws/alder-driver-purity on 1afd910c. The
capture implementation remains Hazel's; no engine/compiler edits are authorized
by this claim. Re-run the current build-profile entry against a private copy of
native host 31be3fb0 and compare its product to the two byte-identical native
builds just measured on this source. Source comments in build-profile.f and
docs/debugging.md still describe driver-dependent bytes; verify before changing
that statement or proposing a capture change. Artifacts /tmp/alder-driver-purity.

Current reduction (1afd910c, private host 31be3fb0, same scratch tree/cwd):

- Native entry: 31be3fb0d4764aa90841f387489756a1248a4e9def3ad3a837567c6cfe61ed4d.
- Profiling entry: 11a2036ab7d35178604cbd1cfb167dae176d3bd967b9a8f6784d41bb5588431f.
- Removing prof-on/prof-off gives exactly the same profiled engine. Sampling
  itself is not the cause. Adding only require lib/fs.f to the current native
  entry changes nothing (the original report's minimal reproducer is stale).
- Minimal current reproducer: copy tools/native-build.f and insert `16 allot`
  after `s" tools/native-build-core.f" required`, before invoking the driver.
  This product is BYTE-IDENTICAL to the profiling entry. Its SAVED-OUT and
  REPORT-FD variables (tools/build-profile.f:42-43) allocate those 16 bytes.
- All four relevant builds exit 0 and pass native-build's product smoke; both
  product classes are 4,456,640 bytes. Every .names map is byte-identical.
  No full gate was run on the profiled product.

Concrete leaked value: IMK-NDICT0's DATA-address literal, at file offset143260,
encodes 0x340f90f98 in native-control and 0x340f90fa8 in profile-off (+16).
SEQ's following literal changes from 0x340f90fa0 to 0x340f90fb0. The Habu
imagedisasm decoder pins both; these are the first two rows of the identical
.names maps, defined in src/core/util.f. The first differing file byte is5853,
an emitted address of the later CODE-SPAN table; that is a layout consequence,
not the initial source of the driver dependence.

Responsible layer: src/habu/aot-capture.f ACAP-SCAN-DSITES stores live d0 in
AOT-DATA-D0 and records in-window chains without normalizing their values.
ACAP-SCAN-DEFER-SITES likewise records raw DATA cells. By contrast,
ACAP-SCAN-CSITES already normalizes code literals and sets AOT-CODE-B0 to0.
The writer emits AOT-DATA-D0 (habu2.f EMIT-AOT-SEED), and EM-AOT-RELOC-DATA
adds runtimeDP minus that captured base, so absolute host addresses and the
matching base cancel at boot but remain observable in the file. Capture-base
modulo8 is part of the existing alignment contract and must survive any repair.
LOGICAL-RESET intentionally retains the callable driver and never rewinds DP;
rewinding into the live driver would be the wrong layer.

Artifacts /tmp/alder-driver-purity: host, native-control, profiled, profile-off,
native-fs, native-allot, their logs and .names maps; tree/tools contains the
scratch probes. native-first.txt/profile-first.txt contain the decoder output.
One extra control defining two variables BEFORE loading the driver shifts by24
(bytes, including earlier allocation effects), so the exact reproducer places
16 allot AFTER the dependency load. No engine/capture implementation changed;
Hazel owns that seam and this dot remains open for the repair and its gates.

Repair claim: alder, .jj-ws/alder-capture-data on 18d59ba4. Hazel released
src/habu/aot-capture.f and the DATA relocation loader, plus tests: replace
absolute DATA-site values and D0 with window coordinates, retaining D0's
modulo-eight alignment. No new format fields or driver-name refusal. Proof:
native/profile products with equal SHA, all AOT/stripped/whitebox rows, three
generations. The filesystem refusal repair precedes this lane.

Coordinate repair: both scans classify the original live addresses, then one
linear pass over the DATA-site rows stores value - live-D0 + (live-D0 & 7),
including deferred metadata cells. D0 becomes its 8-residue. The existing seed
delta and artifact READ/MERGE arithmetic handle this coordinate system without
new format fields. Astra's concern about a normalized DATA value colliding with
the later live CODE scan is resolved by doing normalization after both scans.
The matrix fixture compares every captured DATA site against its live source:
rc 76 before, merge=ok after. Astra's follow-up review is clear.

Equality exposed one further owner: all 32,766 pointer cells in the retired
SYMS-BOOT (16,383 rows, PKG-A and NAME-A) differ by +16 under the profile driver.
The sparse DATA expansion in /tmp/alder-capture-data/data-diff.f identifies only
this owner. Native/profile run bytes are 975,616/975,628. Hazel released zeroing
the retired table in SYM-GROW as a separate commit after this coordinate repair;
the dot stays open until the combined equality and gates pass.
