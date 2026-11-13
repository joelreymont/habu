---
title: Make the compiler dispatch owner cells private
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.907880+01:00"
---

Problem: src/habu/layout.f declares NCOMP-DISPATCH:DECL-A and TARGET-DECL-A as public reserved ptr u8 cells, so any program can redirect the compiler's declaration owner: `: P10 ( ptr u8 -- ) NCOMP-DISPATCH:DECL-A ! ;` and `: P13 ( -- ) 0 NCOMP-DISPATCH:DECL-A @ c! ;` certify on the installed hb. Acceptance: both programs are refused; the owner's own writes still compile. Depends: none. Claim: unassigned.
