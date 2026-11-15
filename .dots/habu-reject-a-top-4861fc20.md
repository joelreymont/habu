---
title: Apply the trusted-only rule at top level
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:08:58.210446+01:00"
---

Problem: top-level code is exempt from the trusted-only rule: src/core/checker.f CALL-AUTHORITY (`TOP-STEP @ IF CALL-PRIMS ELSE CALL-REFUSED THEN`) and TRY-PRIMS admit an ETRUSTED-ONLY primitive at top level, as the comment above the trusted-only rows states. A file with ': MY-HOOK ( ^u8 n n n -- ) 2drop 2drop ;' then "' MY-HOOK set-top-check" loads with rc 0, while the same call in a body is E-CAP-TRUSTED. Acceptance: a top-level call of a trusted-only primitive is rejected at its place under the load and check.f, as in a body, its label and help (src/core/render.f, CAP-HELP) worded for top level; hb's build, the suite and Zen, Tender and Maki still load. Depends: none. Claim: unassigned; an unproven partial change waits in .jj-ws.noindex/jerry-w129 (tplszlwx) for a fresh worker.
