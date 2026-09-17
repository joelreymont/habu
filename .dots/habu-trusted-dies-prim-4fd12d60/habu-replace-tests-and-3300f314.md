---
title: Replace tests and tools tied to retired native paths
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-09-11T09:05:33.554037+03:00\\\""
closed-at: "2026-09-16T14:34:50.923443+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Residue: the remaining fixtures and tools that still assume retired native paths must run through bin/hb or be deleted."
---

Owner: Cedar; test helpers and affected tools, excluding agent-owned checker/compiler storage. Replace observed bin/hb-host and retired IR/build assumptions with the existing bin/hb native load, published code and APP-IMAGE path; delete obsolete wrappers instead of restoring a second engine. Keep real semantic and rejection coverage. Acceptance: affected tests execute through the current native runtime and verify their original behavior; final suite acceptance remains in the existing integration child. Bootstrap pipeline replacement belongs to the native-build child.

Current handoff (2026-09-11): owner Cedar. Reviewed cleanup69c53ded removes private UEND/USIGS-RESTORE-END/UTERM fixture dependencies, uses existing CHECKER-SCOPE-START/DONE before dictionary/code rewind, fixes byte-list and PTY scalar effects. Full runtime regression28.61s passes, repeated same-name scoped effects and real PTY pass. Native test children select tier1 via817922ca, general runtime children remain tier0. Do not restore hb-host or obsolete compiler-state inspection.

Known remaining full-suite blockers on current cold metadata: IR-ID fixtures cannot resolve public TFAM-PKG$; tools/check-test and check-cli-boundary/tool-boundary-doc-public fail TYPE-RESERVED? effect publication. Native final rebuild/private checker migration may be responsible; reduce public API failure before rewriting fixtures. Earlier fce2329d public-TFAM fixture candidate is still unvalidated; do not relabel complete. Bootstrap-codegen test now passes after hide.f41df9051. Full suite remains pending in integration dot.
