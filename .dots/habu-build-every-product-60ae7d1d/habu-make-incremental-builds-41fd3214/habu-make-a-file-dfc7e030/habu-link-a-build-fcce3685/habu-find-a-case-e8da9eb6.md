---
title: "Find a case that fails without PKG-RECOVER's checker resync"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T01:15:56.380717+02:00"
closed-at: "2026-11-06T01:22:09.672107+02:00"
close-reason: "No case exists: once xref.f installs LIVE (xref.f:294-297) nothing reads the checker's package mirror, so PKG-RECOVER's resync (packages.f:266) has no observable effect (probes /tmp/jerry-rsync/probe1-*, probe2-*: mirror stale, behaviour equal). Removal tracked in habu-remove-the-checker-dc4c2dc5."
---

Problem: src/habu/packages.f:266 PKG-RECOVER ends with `NCOMP-DISPATCH:DECL-PKG-RESYNC-OFF PKG-NOTIFY`, which runs src/core/checker.f CHECKER-RESYNC:MIRROR (:12305) so the checker's package mirror (CHECKER-PACKAGE-MODE and name) matches the restored scope. With it removed, test/linker.f passed rc 0 (S5b1b review probe). WITHOUT-PACKAGE (:445) resyncs to no package before a build, so without the recover's resync the mirror says no package after LINKER:WITH while the engine has the caller's open. No test notices. Acceptance: a test/linker.f case, run after a build inside a host package, that fails with :266's resync removed and passes with it; or a report that the mirror is never read in that state, with the reads named, and the resync's comment says so. Files: test/linker.f (case), src/habu/packages.f (comment only if no case). Verify: bin/hb --load test/run.f -- linker, and the same with :266's resync removed in the working copy only. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rsync (brief /tmp/jerry-rsync/brief.md).
