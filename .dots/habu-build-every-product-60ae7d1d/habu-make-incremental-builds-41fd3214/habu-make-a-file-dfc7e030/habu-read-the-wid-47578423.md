---
title: Read the wid cells in the shared lookups
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T15:10:32.917977+02:00"
---

Problem: shared Habu code that runs on both machines still names the package marker and the global wordlist as constants: src/habu/xref.f:21 XREF-NAMESPACE-WL (DICT-WL:NAMESPACE), :110 XREF:RETIRED-WL? (negative and not NAMESPACE), :362/:371 the bare lookup's wid 0, :654 DEF-OCC CALLABLE; src/habu/outer.f:543 LOOKUP-GO's global-hit test; src/core/internal-mark.f:113/:190. While a build writes the next hb the cells BOOT-LAYOUT:GLOBAL-WL-CELL and NAMESPACE-WL-CELL name the product's wordlists, so these sites must read them (cross-build step 3c). Acceptance: XREF-NAMESPACE-WL reads NAMESPACE-WL-CELL; RETIRED-WL? is RETIRED or RETIRED-NAMESPACE; the bare lookup and LOOKUP-GO use GLOBAL-WL-CELL; CALLABLE and internal-mark compare with XREF-NAMESPACE-WL; no behaviour change (cells hold 0 and -1). Files: src/habu/xref.f, src/habu/outer.f, src/core/internal-mark.f. Verify: brief /tmp/jerry-big/wlcell3c-brief.md proof (arm64 tests against /tmp/jerry-3c-base/base.out, probe on arm64 and pulsar, x86-64 package-seal and export-package). Depends: habu-write-the-wid-ddf18263. Ownership: those three files' named sites. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wlcell3c; brief /tmp/jerry-big/wlcell3c-brief.md.
