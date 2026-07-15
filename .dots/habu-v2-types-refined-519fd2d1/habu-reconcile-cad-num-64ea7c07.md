---
title: Reconcile CAD-NUM V2 plan state
status: closed
priority: 1
issue-type: task
created-at: "2026-07-15T06:41:26.288476+02:00"
closed-at: "2026-07-15T07:21:42.869860+02:00"
close-reason: "Merged 2c36db56 on master: B5 now names the unlanded STRUCTURE/ENUM migration, landed checker-path TVK-RAW at 085cf242, open native/REPL residual, real seal dot, and current design/typed-definer status; reviewed clean and full stdlib/native/Maki gates green."
---

Full context: MODEL-CAD-V2-PLAN.md:1258, :1277, and :1296 still say TVK-RAW is open and its laundering probe is accepted even though habu-nominal-storage-raw-a3430ef2 closed at 085cf242; :1454 says the numeric-role design still needs an amendment already present; :1251 names removed SUMTYPE machinery. Cause: implementation and plan state advanced independently. Fix: reconcile only current proven state and replace removed-syntax wording with unified ENUM/STRUCTURE intent without claiming unlanded enforcement. Acceptance: every TVK-RAW/status statement matches closed-dot evidence; no stale accepted probe remains; the hard-cutover wording names only STRUCTURE/ENUM; no runtime, TRUSTED inventory, or consumer code changes. Files: MODEL-CAD-V2-PLAN.md only. Verify: exact source/dot cross-check plus host/filemap/dot/status lints. Claim: agent=cad-num-plan workspace=.jj-ws/habu-reconcile-cad-num-64ea7c07.
