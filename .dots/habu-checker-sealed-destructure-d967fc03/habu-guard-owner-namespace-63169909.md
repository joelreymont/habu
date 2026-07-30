---
title: Guard owner namespace retirement
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T05:52:49.672851+02:00"
blocks:
  - habu-record-owner-namespace-8ca3b072
---

Problem: XREF retirement can erase the only owner marker, after which package reopen or saved-WID publication can bypass authority. Result: consume only XREF-OWNER:REC?, XREF-OWNER:NAME?, and XREF-OWNER:WID? from the record leaf. Delete the checked-callable XREF-PATCH32 wrapper. XREF retirement keeps one narrow trusted helper that changes only the retired wordlist and status cells after predicate checks and calls the centrally guarded patch32 primitive shared with legitimate debugger and code-emission callers. XREF-RETIRE and XREF-RETIRE-WL reject a marker or its canonical namespace. HIDE-DEFS-FROM and FORGET-DEFS-FROM reject a suffix crossing either record before signature truncation. Build refresh gets no bypass: prove its fresh compiler process has no marker at BFR truncation. WORDS uses XREF-OWNER:REC? to omit markers. Owner: XREF mutation and display paths only. Production red: retire the canonical namespace, reopen it, and publish HACK successfully; XREF-PATCH32 can also rewrite the marker directly. Acceptance: retirement and direct patch-wrapper use reject before checker, XREF, NDICT, CP, or signature mutation; later-word retirement remains legal; native refresh and fixpoint stay green. Forbidden: another classifier, native sink edit, new rollback owner, guarded general patch wrapper, refresh latch or primitive, effect removal, allowlist, table, trailer, version, compatibility reader, or lint. Smallest owning check: the exact namespace-retirement reopen reproducer fails without changing state, and no checked symbol named XREF-PATCH32 remains.
