---
title: Register native/REPL storage definers RAW
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-15T00:37:09.808603+02:00\""
blocks:
  - habu-migrate-maki-raw-636b07f2
---

Follow-up from habu-nominal-storage-raw-a3430ef2 (landed 085cf242): the checker-side TVK-RAW seal is complete and the verify-source gate path (fixpoint build, check-all-errors, build preverify) enforces it, but on plain --load/REPL the definer effects for create/variable/constant are registered by the native codegen (habu2.f C-CALL-TRUST-LASTC-*, shared HOOK-CELL, baked sig strings '-- ptr a'/'-- a') and mirrored in bootstrap/cg/forth.fs - both currently register non-RAW, so interactive --load can still mint a family through raw storage. Fix (small two-stage): point the native definer hook at a raw-mode registration (SIG-RAW-DEFINER! is ready in src/core/checker.f) and mirror the entry in forth.fs. DISPATCH-GATED on the live owner-persist lane 1f23e205 (tfam) releasing habu2.f AOT/bootstrap territory - same gate as typed-top snapshot daa8989a and effect-read API 95e853eb. Acceptance: the pointer-storage-test mint negatives reject identically on a plain --load path (no verify-source); fixpoint x2; bootstrap parity. Files: src/habu/habu2.f definer hook region, bootstrap/cg/forth.fs. Ownership: engine codegen + bootstrap mirror (owner-persist coordination).


ENGINE FIX PROVEN, LANDING BLOCKED 2026-07-15 (engmirror lane): the habu2.f
seal is correct and proven on a scratch engine - LSIGRAWDEF label +
s" sig-raw-definer!" baked string + C-EMIT-SIG-RAW-CALL bracketing
C-CALL-TRUST-LASTC-PTR-A/-A inside the HOOK-CELL guard (mirrors verify-source
RAW-TRUST-NEXT; both funnel through USIG-ADD/VAR-OF honoring SIG-RAW-MODE).
Probes: all three RSV mints reject rc 70 with the family named; scalar
round-trip certifies; fixpoint x2 byte-identical AND a full gforth bootstrap
reproduced the sealed engine byte-identical; regression test
test/raw-storage-native-test.f written (spawned --load children). BLOCKED
because the seal breaks maki pervasively (family handles in raw create pools -
see the prerequisite dot habu-migrate-maki-raw-636b07f2, now this dot's
blocker). Re-derive the patch from the design above (the lane's patch file was
session-temp); re-apply after the migration lands.
