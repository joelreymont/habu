---
title: root plan documents contradict the tree
status: closed
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.044480+02:00"
closed-at: "2026-09-16T14:34:51.295690+03:00"
close-reason: "superseded by habu-campaign-c7-learning-b18a8259: MODEL-CAD-V2-PLAN.md is already gone and PLAN.md is now honestly the native-compiler plan; MISSING.md, TYPE-FIXES-PLAN.md, LLM.md and RESTART.md still sit at the root"
---

Problem: MODEL-CAD-V2-PLAN.md (4020 lines) says at :85-90 STRUCTURE/ENUM do not exist (maki alone has 42/135 sites), its §19 layout and §18 CAD: API never existed, and the parts that did land (maki/db, evidence, experiment) were ruled deleted; TYPE-FIXES-PLAN.md presents a wave-1 cutover the tree has not executed (SUMTYPE/PRODUCT/DEFTYPE definers intact, MINT- in 45 files, prot-wid in 77, item 37 undone) and carries an unresolved 'open decision for Joel' at :20; MISSING.md describes foundations that all landed (docs/forth.md:43-56, 750-757, 809-816, 847-850) and admits its table is stale (:284); PLAN.md is the compiler-IR plan under a repo-wide name. Acceptance: MODEL-CAD-V2-PLAN.md archived beside cad-plan.md with R3/R4 and the corrected §3.1 extracted into docs/type-families.md; TYPE-FIXES rulings folded into dots with a per-item status stamp in docs/type-system.md and the root copy removed; MISSING.md deleted (B2 becomes a dot); PLAN.md moved to docs/compiler-ir-plan.md; README's Plans list updated. Files: the four root documents, docs/. Verify: ls *.md at the root shows README, CLAUDE/AGENTS, LESSONS only. Depends: the README dot. Ownership: docs. Claim: unassigned.
