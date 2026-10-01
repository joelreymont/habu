---
title: Read package public wordlists by their field name
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T11:46:44.949361+02:00"
---

Problem: package-row readers fetch the public wordlist with XREF-START, the word-record entry accessor; it works only because both accessors share one body, and xref.f:72-75 names this lapse. r4-acap (6fea6d34) renamed three readers; review 124 found the rest: src/habu/xref.f:225 LIVE-PKG (and :228 reads the private wid through XREF-LEN, not XREF-PKG-PRIVATE), src/compiler/native/compiler.f:324 QUALIFIED-RECORD-NAME?, src/core/internal-mark.f:124 IMK-PKG-PUBLICS, tools/tier-census.f:225 NS-INDEX, tools/pkg-wid-probe.f:24,37,57, test/compiler/ir-id.f:442,468, test/gate-aot-positive-lib.f:211,213 (generated source), test/stripped-address-cases.f:91, test/compiler/native-qualified-name.f:25. Acceptance: every package-row read of a wordlist uses XREF-PKG-PUBLIC or XREF-PKG-PRIVATE (rg XREF-START over package rows finds none); built engine differs from the previous one only in branch targets and the signature hash (cmp listing), g1 == g2; the touched tests pass.
