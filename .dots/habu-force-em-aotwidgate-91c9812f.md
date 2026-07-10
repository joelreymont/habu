---
title: Force EM-AOTWIDGATE reject for a red-first fixture
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T15:40:28.145223+02:00"
---

EM-AOTWIDGATE (habu2.f, the AOT/snapshot boot-pass protected-WID gate) is now LABELED on fd 2 (LPROTAOT 'hb: AOT protected-WID gate reject') before exit 84, message baked into bin/hb and proven by-construction (identical ADR+write+exit as the firing LPROTPUB publish label and LSRCFULL/BPROTWIDADD). It fires only when a baked AOT call-site (habu2.f:2943 reloc), bootrun entry (:3027), or snapshot rebased call (:5000) resolves into a protected WID -- not reachable from user source. An automated red-first fixture needs a crafted-AOT-image / pwid-variant build harness (bake a call/bootrun into a protected WID, then boot -> forced reject), analogous to the habu-tfam-2b-v-9cbd0019 'pwid-variant, wids 300+70000 baked' harness which is not in the repo. Build that variant-engine builder (a build-fixpoint seam that bakes a protected-WID target into an AOT call/bootrun), forge the boot, and assert rc 84 + CONTAINS 'hb: AOT protected-WID gate reject'. Related: deferred habu-aot-protected-wid-08716547 (batch pwid restore timing) touches the same LAOTPWID/EM-AOT-REGISTER-PROT-WIDS machinery.
