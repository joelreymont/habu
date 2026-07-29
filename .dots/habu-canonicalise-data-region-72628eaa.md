---
title: Canonicalise DATA-region snapshot pointers
status: active
priority: 2
issue-type: task
created-at: "2026-07-29T19:58:29.556568+02:00"
---

Full context: PRIORITY 1, blocks habu-fix-owner-wid-e2bc360c. EM-SNAPSHOT-COPY-DATA (src/habu/habu2.f:4057-4063) restores the DATA payload verbatim, and only RBASE-CELL, S0-CELL, ARGC/ARGV/ENVP, NDICT and CP are re-stored afterwards. Any other DATA cell holding a live region pointer keeps the WRITER's address — harmless while the region lived at a fixed VA, broken under SNAP v4. Evidence: with the region-to-text displacement matched between writer and reader, a plain snapshot image STILL crashes, indicating at least one further un-canonicalised class. Audit every DATA cell that can hold a region address, canonicalise it to the RBASE-VA sentinel on write and rebase it on restore, and add a checked regression that boots a snapshot image and asserts each such cell points inside the live region bounds.

Claim: agent=snaprel workspace=.jj-ws/habu-relocate-snapshot-region-752042fe

PREMISE CONTRADICTED 2026-07-29 (agent=snaprel). The structural observation in
this dot is correct - `EM-SNAPSHOT-COPY-DATA` restores the DATA payload verbatim
and only a named list of cells is re-stored afterwards, so any other DATA cell
holding a live region address keeps the writer's value. But the evidence quoted
for it is wrong. A plain snapshot image exits 0 in 13 of 200 bare runs, and the
runs that succeed are exactly the runs where the mapping succeeded AND the
reader drew the same region-to-text distance as the writer. Since the text base
is randomised on every run, a stale DATA cell pointing into the writer's region
would be wrong in ALL 200 runs, including those 13. So no DATA cell holding a
region pointer is dereferenced on the boot path today, and the claim that a
displacement-matched image "STILL crashes" does not hold.

This dot therefore is not a blocker for habu-fix-owner-wid-e2bc360c and should
not be scheduled beside it. Keep it open as a latent-soundness item, and note
that its cost depends on how habu-relocate-snapshot-region-752042fe is resolved:
if the region keeps a run-varying base, DATA cells holding region pointers must
be enumerated and canonicalised, and enumerating them by scanning DATA for
values that fall inside the region's address band is a value heuristic that the
fix review gate should reject. The honest version of this work is to give the
few DATA cells that can hold a region address a declared relocation kind at the
point they are stored, the same principle prereq A of the direct-BL campaign
(dot habu-identify-code-pointers-b973e6cc) applied to code pointers. Add the
regression this dot already asks for - boot a snapshot image and assert each such
cell points inside the live region bounds - since that is what would have
falsified the original claim.
