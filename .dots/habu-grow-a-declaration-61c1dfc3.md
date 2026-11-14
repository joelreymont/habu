---
title: "Grow a declaration's body past 4 KiB"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T17:26:06.650921+01:00"
---

Problem: src/core/sumtype.f TDECL-CAP ($1000) bounds a declaration's buffered body. A SUMTYPE of 200 VARIANTs each holding one n is rejected ('declaration too long', E-TDECL-CAP, rc 70), and tools/check.f rejects the same file with 'source preverify failed before run' and no reason. The cap is a limit Habu set itself. Acceptance: a declaration's body has no fixed byte cap; the 200-variant SUMTYPE with DERIVE eq hash loads, answers EQ right and passes tools/check.f, as tests. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-dcap.
