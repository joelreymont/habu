---
title: Help a DEFTYPE base by an edit that loads
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T23:45:20.584799+01:00"
closed-at: "2026-11-16T04:07:35.946740+01:00"
---

Problem: src/core/structure-decl.f SD-ONE-NAME resolves a family base outside the wrap (TFAM-SIG-RESOLVE) and offers NEWTYPE BASE<n,...> for any arity: 'DEFTYPE rev3 Option', 'DEFTYPE dtk ptr' and 'DEFTYPE dtv read-view' get helps the loader rejects; the one-name help for 'option<n>' and '^u8' bases leads to a second rejection (re-review 9a findings 1, 2, 4c). Acceptance: every DEFTYPE base record's help is an edit that loads, pinned on the load and verifier paths in test/deftype-suite.f. Depends: none. Claim: agent=W142 workspace=.jj-ws.noindex/jerry-w142
