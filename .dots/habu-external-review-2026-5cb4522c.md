---
title: "External review 2026-08-20: verify and execute"
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T13:14:16.853147+02:00"
---

The user-supplied review: 5 correctness holes (IR row appends not transactional - source.f:207 six pushes after one capacity check, probe left registry unreadable E-IR-SRC-STATE; freeze skips multi-successor SSA arg validation verify.f:418 - regalloc-verify.f:793 is first authority; effect tokens not domain-checked verify.f:718; tensor-value.f placeholder pointer reachable via TV-AT@/TV-MATERIALIZE 439-470 + no bounds; sched-key.f:272 inserts unprobed omitting the live ptx toolchain digest toolchain.f:275) + a simplification table (NIMM no-consumer 261+462 lines, PTX text optimizer 660 off-by-default, NMIGRATE DEFINE-HELD-only, one-SSA-layer ruling, opcode table dedup, suite manifest unification, context reclaim, measurement stores, SUMTYPE/PRODUCT cutover completion, tracker GC ~1500 dots) + doc pins (PLAN.md hash stale, README 165KB claim wrong - it is hb-host's size, softmax gradcheck overclaim). MANDATE: every claim VERIFIED against the tree before acting (the audit-refutation discipline); each verified item gets its own dot with the failing path; the two stop-ships are already dotted separately. The 5 correctness holes are priority; the simplification rows each need the probe-first gate; tracker GC per no-governance-ledgers.
