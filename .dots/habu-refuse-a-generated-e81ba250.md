---
title: Refuse a generated name a user word holds
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T15:48:07.366787+01:00"
---

Problem: src/core/sumtype.f TDINIT-FREE asks only the xref preflight, not CHECKER-DEFINED-HERE? as DERIVE addr's TDECL-ADDR-FREE does, so a user word already in the wordlist a DERIVE init accessor lands in (PO-A@ private before an OPAQUE family) dies 'xref: generated declaration already exists' (src/habu/xref.f CHECK), rc 76, where constructors and DERIVE addr refuse E-TDECL-NAME, rc 70; its replay also calls the xref preflight, which addr's replay does not. Acceptance: the clash is refused E-TDECL-NAME, rc 70, live and under tools/check.f; a word in the other section still passes. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-pref.
