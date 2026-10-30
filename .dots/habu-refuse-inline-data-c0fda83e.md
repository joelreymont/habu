---
title: Refuse inline data words in an AOT code member
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:05:37.913332+02:00"
---

Problem: the AOT linker (COPY-COMPACT-BLOB) decodes every word of an ARM64 code member through RELOC-W32 and nothing records data spans, so a member that carries text or table bytes relinks wrongly whenever a byte word happens to decode as B, BL, B.cond, CBZ, TBZ or ADR: the W^X stack's FORK death text (' map' = $70616D20, an ADR with delta +798119) failed the hb-build-stripped row with 'aot: ADR target outside its member site=fork', fixed in that stack by building the text from LIT64 immediates (ERR-TEXT,). Members with inline data today, by the fix worker's census (unverified): BCALLABLE 'hb: unset quotation' (emitted into BEXEC, which stripped images reach), CB-DIE, BNDSET, BSET*, BPROTWIDADD, EMIT-HIDX; they pass only because their bytes do not decode as a branch or ADR. Acceptance: the lead decides between (a) every member holds only instructions, enforced where a member is emitted (an inline BYTES in a code member is refused at emit time) with the named members converted to immediates, and (b) the sites record carrying data spans the linker skips; a regression fixture with a member whose inline text decodes as ADR relinks correctly or is refused at emit time; the census is verified first. Files: the linker (rg COPY-COMPACT-BLOB RELOC-W32 in src/habu), src/habu/habu1.f and habu2.f emitters named. Verify: the fixture, the hb-build-stripped row, the full suite if the emit path changes. Depends: none. Ownership: AOT linker and the named emitters. Claim: unassigned.
