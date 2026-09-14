---
title: Relocate quotation code literals in stripped AOT executables
status: open
priority: 1
issue-type: task
created-at: "2026-09-14T14:21:49.185604+03:00"
---

Audit C5 independently reproduced on source54e9ae7c and native hb SHA c37b51ff: /tmp/cedar-audit-quotation-app.f defines INC, APPLY ( n [ n -- n ] -- n ) execute, and MAIN calling 41 [: INC ;] APPLY. tools/hb-build.f default mode reports AOT engine stripped success, but executing /tmp/cedar-audit-quotation-app exits139. Core PID1521112 PC/x9=0x1865de0 unmapped, LR=0x401310 immediately after blr x9 at 0x40130c; artifact mappings start0x400000. This is an unrelocated maker code address in a quotation literal, not OOM or the Maki finally fix. src/habu/aot-lib.f COPY-COMPACT-BLOB/RELOC-W32 copy MOVZ/MOVK address literals verbatim; the existing ABS-CHAIN guard catches only old x16 direct-call chains. Follow address-map CODE provenance, include address-only callable targets in closure, rewrite marked literals into position-independent output addresses, and refuse unsupported/missing mappings before publishing. Preserve scalar literals and DATA references. Regress anonymous and named ticks through a helper, nested quotations, an address-only target, and invalid relocation metadata through a real default stripped build/run. Avoid switching applications to --repl as a fix. Existing stored-DATA quotation relocation dot is a separate case. Logs /tmp/cedar-audit-quotation-{build,run}.log; extracted core was destroyed after analysis. Cedar owns; peer review before landing.
