---
title: Apply the trusted-only rule at top level
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:08:58.210446+01:00"
---

Problem: top-level code is exempt from the trusted-only rule: src/core/checker.f CALL-AUTHORITY (`TOP-STEP @ IF CALL-PRIMS ELSE CALL-REFUSED THEN`) and TRY-PRIMS admit an ETRUSTED-ONLY primitive at top level, as the comment above the trusted-only rows states. A file whose one line is '0 execute-floor drop' crashes hb (rc 139, habu-crash), while ': F ( n -- f ) execute-floor ;' is E-CAP-TRUSTED, rc 70. Acceptance: a top-level call of a trusted-only primitive is rejected at its place under the load and check.f, as in a body, its label and help (src/core/render.f, CAP-HELP) worded for top level; hb's build, the suite and Zen, Tender and Maki still load. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w177.
