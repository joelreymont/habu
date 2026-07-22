---
title: Package checked-boundary lint test library
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T15:48:08.449091+02:00"
---

Why: tools/checked-boundary-lint-test-lib.f is a legacy global-vocabulary module (CBLT-* stems, no package declaration), so ANY edit inside it — e.g. registering a new file via s" ..." CHECKED-BOUNDARY-LINT-FILE inside CBLT-RUN-CURRENT — trips the package-first diff gate with E-PACKAGE-OWNERSHIP on the changed word. This currently blocks the host-lint policy commit (its required inventory line edits that word) and will block every future boundary-inventory registration. Owned result: the whole module moves into a real package (e.g. package CBLT) with short local tails; the CBLT-* global stems are renamed to package-local names; every caller (the call site near checked-boundary-lint-test-lib.f:289 and the checked-boundary-lint* consumers) is migrated to qualified or using-based calls; no compatibility global forwarders remain. Acceptance: package-diff-lint green on a probe diff that edits CBLT-RUN-CURRENT's body (the exact shape that is red today); the checked-boundary lint suite and every consumer's owning load path green; rg proves zero remaining CBLT-* global references outside the package. Owning gates: the checked-boundary lint suite via bin/hb plus package-diff-lint on the migration diff itself. Depends: none; the host-lint combined commit re-roots on top of this. Claim: agent=claude workspace=.jj-ws/habu-package-cblt.
