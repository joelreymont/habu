---
title: "Keep a trusted: signature past a REPL refill"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T22:12:49.691760+02:00"
---

Problem: C-SIG-CAPTURE-TSIG (habu2.f) points TSIG-A into the input line buffer, and DEF-TRUST:REGISTER and C-DECLARED-LOG-APPEND read it at ;. The REPL refills one line buffer per line (EM-REPL-READ, RD-LINE into LBUF), so a trusted: definition spanning REPL lines reads an overwritten signature. Fix: point TSIG-A at the BODYBUF copy C-SIG-FULL$ already keeps (LBCS), as lane R1 Revision 6 does for the does> signature. Acceptance: a multi-line trusted: definition at the REPL registers its declared signature; regression row through the REPL. Files: src/habu/habu2.f (and definers.f if OUTER keeps its own pointer). Verify: the REPL row, tools/check.f. Depends: lane R1 lands first (same mechanism). Claim: unassigned.
