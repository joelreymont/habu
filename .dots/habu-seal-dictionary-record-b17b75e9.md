---
title: "Seal: dictionary-record raw-write hardening"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:30:09.224461+02:00"
---

2b-i worker deferral (narrow-A dictionary hardening). PROT-GUARD only protects the DATA-region friend arena; dictionary records live in the DBASE region and are raw-writable: forging DNAME-IMM ( flag), DNAME-EXT, xt cells, or wid fields via ! can mark words immediate (compile-time code execution => TRUSTED-CELL forge via habu2.f:3076 close path) or redirect dispatch. Decide mechanism: range-guard the dict band, provenance on dict pointers, or checker-side rejection of DBASE-derived addresses. Depends on 2b-i; overlaps 2b-iii immediate/DNAME-IMM gating (habu-tfam-2b-iii-d8af2634).
