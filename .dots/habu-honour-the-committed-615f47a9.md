---
title: "Honour the committed plan on the interner's prototype path"
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:11:06.107844+03:00"
---

Problem: src/compiler/ir/build.f SYM-NEW's prototype path clones the interner with IR-SYM:NEW-FROM and ignores the committed plan's ceilings (P-SYMS / P-SBYTES), while NEW-BUILDER-FROM's comment says the plan and every check are the ordinary ones; inert today because PLAN-DEFAULT is IR-SYM:CAP-MAX / BYTE-MAX, live for any smaller plan (the tests' PLAN-SMALL, 16 symbols). Acceptance: NEW-FROM takes the committed ceilings and refuses by name a plan smaller than the prototype's occupancy; ir-symbol and ir-build cases for both. Files: src/compiler/ir/symbol.f, src/compiler/ir/build.f, test/compiler/ir-symbol.f, test/compiler/ir-build.f. Verify: the suites. Depends: none. Ownership: hazel. Claim: unassigned.
