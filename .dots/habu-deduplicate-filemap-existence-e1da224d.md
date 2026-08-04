---
title: Deduplicate filemap existence check
status: closed
priority: 2
issue-type: task
created-at: "2026-07-27T09:38:47.215580+02:00"
closed-at: "2026-08-02T16:55:10.947184+02:00"
close-reason: "Obsolete in exact ancestry: 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8 deleted the sole filemap-lint subject."
blocks:
  - habu-pkg-filemap-lint-5d7baf5c
---

tools/filemap-lint.f keeps a private FM-EXISTS? ( ptr u8 n -- bool ) at lines 57-58 that is a near-duplicate of EXISTS? in lib/fs.f:180. Both are path-existence checks and both are literally the same system call, `access(path, F_OK) == 0`; the filemap copy differs only in how it null-terminates the path, using LINT-PATHZ and the shared PATHBUF from tools/lint/text.f instead of FS-PATHZ and its own buffer. Owned result: delete FM-EXISTS? and call the library word, or prove a real behavioral difference and name the word for that difference.

PREMISE CORRECTED (2026-07-27): the earlier text said the owning package "landed in the vecmem lane wall commit". That is wrong. Packaging tools/filemap-lint.f is DELIVERED IN-LANE ONLY, as lane commit 2b87f9df under habu-pkg-filemap-lint-5d7baf5c, and that commit is not reachable from master. On master the file is still unpackaged and every word in it is a raw global, so this leaf cannot start until the packaging lands. That is why habu-pkg-filemap-lint-5d7baf5c is recorded above as a blocker. Note also that the packaging commit deliberately KEPT the FM- prefix on FM-EXISTS? because a bare EXISTS? tail collides with a live word - so after packaging the word is FILEMAP-LINT:FM-EXISTS? and the duplication this leaf removes is still exactly the duplication described here.

FROZEN SEMANTICS, MEASURED (both words read on master, not assumed). Whichever existence check survives, its behavior on symlinks and on failed checks is part of this contract and must be stated in the surviving word's stack comment and pinned by regression:
(1) Dangling symlink: `access` with mode F_OK follows symlinks, so a symlink whose target does not exist reports NOT EXISTING. Both words behave identically here today. If the retained word keeps that behavior, say so; if the filemap lint actually wants "the link itself is present", that is a different word and needs a different name and its own check.
(2) Error results: `access` returning nonzero for any reason - permission denied on a parent directory, a path component that is not a directory, a name that is too long, an I/O error - is currently reported as "does not exist". Neither word propagates the reason. The leaf must state whether silently folding those errors into "absent" is the intended contract for a stale-path lint, or whether the lint must distinguish "absent" from "cannot tell", and it must not quietly inherit the current behavior without saying which it chose.
(3) The one real divergence measured between the two implementations is the path-length limit and what happens at it: FS-PATHZ (lib/fs.f:170-178) throws E-FS-PATH when the length exceeds FS-PATH-CAP, which is 1024; LINT-PATHZ (tools/lint/text.f:73-80) calls `die` with exit status 1 and the message "lint: path too long" when length plus one exceeds 1024. So a path of exactly 1024 bytes is accepted by the library word and fatal in the lint word. The leaf must pick one and say why.
(4) Shared-buffer side effect: LINT-PATHZ writes the process-wide PATHBUF and sets RPATH-U, which LINT-READ-DIE later reads to name the offending file in a capacity or I/O error message. Switching the existence check to the library word stops updating those, so the leaf must confirm no later lint diagnostic depends on the existence check having last written PATHBUF.

Owner: package FILEMAP-LINT (as delivered in-lane; the file is still global on master). This changes behavior surface, so it stays its own leaf rather than riding the packaging commit. Acceptance: the filemap-lint production gate green with identical path and finding counts on the same tree (1107 paths, 0 findings is the in-lane baseline); fixture suite green; regressions proving the chosen semantics on a dangling-symlink fixture, on an unreadable-parent-directory fixture, and at the 1024-byte path boundary; both diff lints.
