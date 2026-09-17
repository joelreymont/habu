---
title: "Route the recovery prologue's USIGS reset checked"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T14:18:09.014143+03:00"
---

Problem: tools/bootstrap.sh BOOT-USIG-END-PTR names USIGS inside a TRUSTED: body and BOOT-USIGS-RESET is called by the launcher; the launcher feeds one prologue to the gforth stage 0 and to the sealed native stages after it, so a name-stripped native stage fails the recovery prelude exactly the way the build chain failed before habu-give-the-build-4b825045 (chain-callees lane, 2026-09-17). Acceptance: the native stages reach the signature-store reset through a declared route (a checker row like CHECKER-BOUND REWIND's, or the native stages skipping a reset the boundary rewind already performs) and no TRUSTED: body in tools/bootstrap.sh names a DNAME-INT word; tools/bootstrap.sh check green; test/internal-word-gate.f pins the route. Files: tools/bootstrap.sh, src/core/checker.f, test/internal-word-gate.f. Verify: tools/bootstrap.sh check; bin/hb --load test/internal-word-gate.f. Depends: habu-give-the-build-4b825045. Ownership: bootstrap. Claim: unassigned.
