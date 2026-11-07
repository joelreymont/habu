---
title: "L4 lane W: link a fresh object in RUN-FILE"
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T01:29:13.928753+02:00"
---

Problem: nothing calls the link words yet. Acceptance: LINK-OBJECT (LINK-CLEAR, then entries in order by ENTRY-OFFS: KIND-DEF LINK-DEF, KIND-PACKAGE LINK-PACKAGE, KIND-ALIAS LINK-ALIAS, KIND-PRIVATE LINK-PRIVATE, KIND-DOES skipped; then LINK-DATA); the MODE machine ENUM link-mode none compiled linked with -9692 LINKER:E-STALE; RUN-FILE's choice: FRESH? -> loading, PUSH-KEY, LINK-OBJECT under ['] POP-KEY finally, FILE-END, BUILT+ with the read object, open (design-l4 Settled 2-3). Verify: test/file-objects.f PASS (its six waiting asserts 9, 10, 29, 30, 31, 51) plus linker, arm64-link-run, arm64-link-data. Depends: lanes C and D merged. Ownership: worker lane. Claim: agent=worker workspace=.jj-ws.noindex/carl-l4lw (brief .out/brief-l4-link.md there; on rwyzklot, the C and D merge).
