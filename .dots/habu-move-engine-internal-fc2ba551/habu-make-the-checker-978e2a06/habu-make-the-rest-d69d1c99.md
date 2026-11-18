---
title: "Make the rest of TFAM's internals private"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T16:25:27.290940+01:00"
---

Problem: src/core/type-family.f package TFAM keeps its mutators public, so a program changes the type registry: ENTRY! MEMBER-SYM! PF-ADD PF-ENSURE PF-SCRUB SUMV-ADD SUMV-CTOR-PKG! SUMV-CTOR-SYM! SVX-ENSURE TFAM-DERIVE-ADDR! TFAM-DERIVE-EQ! TFAM-DERIVE-HASH! TFAM-DERIVE-INIT! TFAM-DERIVE-TAG! TFAM-KIND! TFAM-OPAQUE! TFAM-SCRATCH! TFAM-TAGW! TF-INTERN TFM-EPOCH+ TF-RBF-ENSURE; `: P ( -- ) 0 5 TFAM:TFAM-KIND! ;` passes tools/check.f -- --verify-only, rc 0. TYPE-WORD is public too; the build's linker finds it by name (src/habu/linker.f TYPE-SOURCE), which looks in TFAM's private words first and its public ones second, so a source with TYPE-WORD private links only once an hb with that TYPE-SOURCE is installed. Acceptance: each word is E-UNDEFINED outside TFAM and engine callers take it by EXPORT; TYPE-WORD is private and TYPE-SOURCE looks only in TFAM's private words. Depends: an installed hb whose TYPE-SOURCE looks in TFAM's private words. Claim: unassigned.
