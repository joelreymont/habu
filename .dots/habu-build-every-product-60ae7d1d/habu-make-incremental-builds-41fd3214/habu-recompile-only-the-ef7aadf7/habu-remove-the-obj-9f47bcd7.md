---
title: "Remove the object's USES section and its words"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T13:37:54.728942+02:00"
closed-at: "2026-11-10T23:38:05.182678+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
blocks:
  - habu-follow-a-run-8117bc1b
---

Problem: once the run rule follows words (dot habu-follow-a-run-8117bc1b), nothing reads a piece's USES entries: linker.f USES, USES-N, USES-FROM, BUILT-USES (240-243), USES-AT (336), USE+ and USES+ (1978-1991, called at 2018), the BUILT-USES fill in the piece's BUILT row (2116-2119), USES-CK (3007, called at 3023) and USES-IN (4131, called at 4160) only write, check and reload a section with no reader. Line numbers are at sxyoypmz 2696ac72 and move with lane 2. Acceptance: those words, their buffers' release and reset (410, 450), the object's USES section and every comment naming them are gone; the object format drops the section with no version or reader for the old shape (hard cutover: an old object fails its check and recompiles); cold = warm unsigned; file-objects sequences unchanged. Files: src/habu/linker.f; docs/architecture.md 'Builds' and the linker.f header where they list the object's sections. Verify: ev.sh (cold/warm cmp, file-objects); grep finds no USES name in linker.f. Depends: habu-follow-a-run-8117bc1b merged into carl-words. Ownership: carl. Also USED (240), whose only reader is USES+. Brief: .out/briefs/lane-remove-uses.md (line numbers at 0fbd7181). Claim: agent=carl workspace=.jj-ws.noindex/carl-uses.

Lane reported 2026-11-10: commit caba839b "Remove the object's USES section" on the integration tip 0fbd7181 (workspace carl-uses retired). linker.f 7 insertions, 62 deletions; USED went with it; LINK-FILE's comment lost "references,". ev.sh: all rc=0, warm rewritten 0, cold = warm unsigned, probe rewritten 3, restored = cold, file-objects PASS. No USES, USE+ or USED word left in linker.f.
