---
title: Keep renderer working cells out of saved images
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T06:10:21.999908+02:00"
closed-at: "2026-11-04T11:48:50.454483+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the cross-build whose product is data (DECISIONS \"how Habu builds Habu\"). Render/checker scratch cells leaking into saved images; a product written as data never captures checker cells."
---

Problem: found by the refrec lane's review. render.f JNBUF/JNV/JNN (~526, JNUM's digit buffer and counters), JLOC-L/C/B/E (~1080, JLOCATE's result) and checker.f DOS-OFF/LN/CL/P (~21182, DIAG-POS's counters) are plain cells a rendering writes and nothing resets; they are locals in disguise. STGR-LOCATE calls DIAG-ORIGIN-SPAN!, so a storage refusal inside the verifier window overwrites DIAGL0/DIAGC0/DIAGB0, which later JLOC-ORIGIN placements read. A --load build with no DIAG-SOURCE armed returns from JLOCATE before writing, and the reviewer found no build path that saves an image after the verifier window rendered, so no image difference is shown yet. Acceptance: first show whether any build path saves an image after a rendering that writes these cells (tools/check.f, native-build, MULTI-ERR in a capture); if one does, the counters become locals and results go back to their caller, with a saved-image cell reading proving 0; the DIAGL0 overwrite: show a storage refusal followed by a placed diagnostic in one window, and whether its place is wrong. Files: src/core/render.f, src/core/checker.f. Verify: rows diag-position, native-gate-diagnostics, load-reject-diag; probe diff byte-identical. Depends: habu-hand-every-refused-ed537a7c (lands the pattern). Ownership: src/core/render.f, src/core/checker.f. Claim: unassigned.
