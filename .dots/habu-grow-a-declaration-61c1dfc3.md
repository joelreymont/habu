---
title: "Grow a declaration's body past 4 KiB"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T17:26:06.650921+01:00"
---

Problem: src/core/sumtype.f TDECL-CAP ($1000) bounds a declaration's buffered body. A SUMTYPE of 200 VARIANTs each holding one n, with DERIVE eq hash, is rejected, and the load prints only 'hb: uncaught throw code 7118' (rc 70): no place, line or reason. The cap is a limit Habu set itself. Acceptance: a declaration's body has no fixed byte cap; a declaration rejected for any reason prints its place, the source line and the reason; a test declares the 200-variant SUMTYPE and calls its EQ. Depends: none. Claim: unassigned.
