---
title: Make snapshot image builds reproducible
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:58:09.147271+02:00"
---

Full context: agent snapreloc measured that two successive snapshot image builds from the SAME tree differ in 9608 bytes, so the image is not reproducible and the SND-QUARANTINE offset table (src/habu/snap-lib.f, twenty DATA-START-relative offsets of cells that must be zeroed) can silently drift off its cells whenever builder sources change. The quarantine list is a value-position heuristic where a structural declaration is possible: the cells it zeroes should be DECLARED by the code that owns them (same design as the SNAP-RELOC XTCELL table: registration at the site that creates the cell, dot habu-relocate-persisted-defer-7aa681c4) instead of pinned by build-time offsets. Two parts: (1) find the source of the 9608-byte build-to-build drift with evidence (diff the two images, attribute the differing bytes to their producers); (2) replace the hardcoded quarantine offsets with owner-side declaration, then delete SND-QUARANTINE. Related owners: habu-fix-persisted-dangling-a520f7b4, habu-canonicalise-data-region-72628eaa.

Sharpened 2026-07-30 (agent relocproof, measured): the drift is run-to-run
nondeterminism with an UNCHANGED binary - three consecutive
build-fixpoint-refresh -- snap runs from one bin/hb produced three different
images (~9,613 of 15,318,112 bytes differ; first difference at offset
384,935). Two consequences: image byte-counts are unusable as a mutation
proxy (mutation deltas overlap the ~9.6 KB noise floor), and the two-build
byte-compare in the snap flow can only be passing by comparing something
narrower than the whole image - check what it actually compares. Part of the
drift is the address-literal class (habu-relocate-persisted-region-47de06b9
canonicalizes those sites); measure the residual after that lands before
attributing the rest.
