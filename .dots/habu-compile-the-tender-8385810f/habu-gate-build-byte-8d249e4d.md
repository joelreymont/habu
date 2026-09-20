---
title: Check same-source build identity in the existing chain tool
status: closed
priority: 2
issue-type: task
created-at: "2026-09-12T18:19:40.670747+03:00"
closed-at: "2026-09-20T04:24:27.914635+03:00"
close-reason: "landed d8175ae0 (alder: tools/two-generation-core.f with --same-host and --compare, first-difference offset and its .names owner; clean same-host pair byte-identical, a deliberately varied driver refuses at offset 5852), gate DG 461/461, integrated d8175ae0 (engine unchanged)"
blocks:
  - habu-let-the-chain-9fe66f8e
  - habu-build-the-compiler-c348eab0
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own comparison mode in two-generation-build.f, dirty-transient fixture and bootstrap docs after private-output API. Two same-host uncached builds compare complete bytes; report offsets and owners where metadata supports. Clean product passes, baked timestamp/transient refuses, product-hosted chain converges. Record actual runtime; no assumed minute budget, second gate framework or rebuild for docs/tracker edits.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Claim: alder, .jj-ws/alder-same-host on 1afd910c. The private-output API is
landed. Add a same-host comparison mode to the existing chain driver, preserve
its five-generation mode, and test mismatch offsets/refusal as well as real
uncached builds. Reuse the image reader for metadata rather than parsing the
image layout a second time. No engine/compiler edits; Hazel owns the full gate.

Implemented --same-host [seed] in the existing driver: two uncached production
builds execute the same private seed, then compare every file byte. --compare
checks an existing pair, including empty files and unequal lengths. A mismatch
reports the zero-based first offset; same-host builds also consult the complete
.names map when that offset is in captured code. The image reader supplies the
blob coordinates. Version-1 map lengths are raw CODE-SPAN values, not plain
byte counts; the reader decodes both legacy and exact spans through CODE-SPAN.
The native-build-core comment now states the format it has always emitted.
The driver core is require-able so the new gate row exercises byte boundaries,
map columns and actual CLI exit behavior without starting builds per gate.

Validation on the private 1afd910c tree with release host 31be3fb0:
- Clean same-host pair: rc 0, 146.85 s, both products SHA256
  31be3fb0d4764aa90841f387489756a1248a4e9def3ad3a837567c6cfe61ed4d.
- Private dirty-tree proof: the same driver source allots 8/16 bytes after
  loading native-build-core according to the output path's final digit. The
  two builds differ by 874,055 bytes; rc 1, first offset 5,852, 148.67 s.
  That offset has no .names owner and is reported as unavailable. This exposes
  the driver-coordinate leak tracked by b4b544b4, not a repair of that defect.
- Existing five-generation mode: rc 0, 367.38 s; shape 2 == 3 and complete
  bytes 2 == 3, 3 == 4, 4 == 5. All products remain private.
- Real metadata probe: blob+8 names IMK-NDICT0; blob+4788 names stripped
  SPEC-RC. Before CODE-SPAN decoding the latter incorrectly named DEFER-UNSET.
- two-generation-fixtures, engine-size-fixtures and the complete
  hb-build-fixtures row pass. Astra xhigh review and the focused decoding
  follow-up are clear after adding the explicit CODE-SPAN require.
Evidence: /tmp/alder-same-host. Full gate belongs to Hazel's serial chain;
no engine source changes or shared-engine writes. Close after integration.
