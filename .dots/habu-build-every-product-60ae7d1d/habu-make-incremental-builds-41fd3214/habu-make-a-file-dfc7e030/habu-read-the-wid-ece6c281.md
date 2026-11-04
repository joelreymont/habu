---
title: "Read the wid cells through BOOT's readers"
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T20:07:54.813245+02:00"
---

Problem: Joel 2026-11-04 (DECISIONS "how Habu builds Habu") chose "BOOT:NAMESPACE-WL (Recommended), the owner's reader in a file loaded before the checker, which every file calls in place of its own read; BOOT:GLOBAL-WL reads the global wid beside it". Today src/habu/xref.f:22 XREF-NAMESPACE-WL reads the marker cell for 49 uses in 24 files, and internal-mark.f:36, outer.f:543 and xref.f:364/:374 read GLOBAL-WL-CELL inline. Acceptance: src/habu/boot-cells.f gains BOOT:NAMESPACE-WL ( -- n ) and BOOT:GLOBAL-WL ( -- n ); XREF-NAMESPACE-WL goes and every use calls BOOT:NAMESPACE-WL; the four inline reads call BOOT:GLOBAL-WL; no behaviour change (cells hold -1 and 0). Files: boot-cells.f, xref.f and the use sites in /tmp/jerry-big/readers-brief.md. Verify: that brief's proof (native build with a 3c host, eleven focused tests, the wid probe, the x86-64 cross-build); the full suite at landing with the checker lane. Depends: habu-move-the-boot-42e58e9f. Ownership: the named sites. Claim: agent=worker workspace=.jj-ws.noindex/jerry-boot2; brief /tmp/jerry-big/readers-brief.md.
