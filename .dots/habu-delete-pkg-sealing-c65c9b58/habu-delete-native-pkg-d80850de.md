---
title: Delete native package-name wall
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T22:10:14.562565+02:00"
---

Remove the native and bootstrap reserved package-name wall. Delete RESTAB and LRESTAB data, C-SEAL-PACKAGE-FAIL, C-SEAL-MATCH, C-QUALIFY-SEAL-GUARD, C-PACKAGE-SEAL-GUARD, their bootstrap mirrors, and every call from qualified definition, POSTPONE, tick, backtick, package, and export paths. Retain C-PACKAGE-PROT-GUARD and all protected-WID registry state; make that guard fail directly with hard-renamed ENGINE-ERROR:PROTECTED-WID code 84. Rename SEAL-PACKAGE to PROTECTED-WID in src/core/engine-error.f and src/core/effects.f with no alias. Owner and files: src/habu/habu1.f, src/habu/habu2.f, src/habu/bootstrap.fs, src/core/engine-error.f, src/core/effects.f. Pre-change owning probes: package ENGINE-ERROR exits 84 and qualified publication into it rejects. Acceptance before M17: exact absence census for deleted labels/tables and renamed error, representative production positive for opening/publishing into the formerly reserved package, constructor protection unchanged by inspection, exact typed-local/package/trust diff gates. No suite, registry deletion, ordinary owner registration changes, compatibility, lint, or unrelated seal edits.
