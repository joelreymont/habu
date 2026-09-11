---
title: Rebuild an engine from its own product to a fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T18:15:57.448451+03:00"
---

Problem: on root 0c3099ea the seed hb-stdin builds B1 (rc 0), B1 builds B2 (rc 0, 30 s), but B2 cannot build B3: tools/native-build.f dies 'hb: data space out of range' (rc 76) inside LOAD-TARGET after 3.1 s. Measured heap top (here data-base -) at boot: seed 6,179,255 / B1 9,959,254 / B2 12,187,478; after the prelude 18.1 / 21.1 / 23.4 MB against the DP-CHECK cap DATA-SIZE minus PROF-CNT-BYTES (habu1.f:1605, DATA-SIZE $2000000). B2's image carries 2.2 MB more DATA and 3,370 more XTCELL rows than B1 (21,607 to 24,977) when the host is a native-build product instead of the seed; the window DATA is rebased to a canonical base (IMK-NDICT0 at 1,482,568 in both), so it is not host-heap accretion but retained content. Pre-existing (the parent dies at B1 to B2), exposed by 0c3099ea's reset fix; the byte-for-byte fixpoint docs/bootstrap.md requires is unverified, and the optimizing selfbuild (stage 2 built by a stage-1 product) sits exactly on this path. Acceptance: name what the extra rows and DATA are (host definitions retained across the warm-capture reset? duplicated prefix? XTCELL rows the reset does not drop?), fix at the responsible layer so an engine built by a product is the same size as one built by the seed, B2 builds B3 and B3 equals B2 byte for byte (the documented fixpoint), with a test tool or gate that runs the two-generation build. Files: src/habu/aot-capture.f, src/habu/aot-arm.f, src/core/internal-mark.f, tools/native-build.f. Verify: the two-generation build; compare XTCELL counts and DATA sizes per generation. Depends: none. Ownership: rowan (selfbuild lane). Claim: unassigned.
