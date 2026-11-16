---
title: Agree on a section keyword evaluate renders
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T14:17:31.472094+01:00"
---

Problem: inside a package, 's" global" evaluate' (/tmp/carl-d2/q/a05-eval-global.f) and 's" public" evaluate' (/tmp/carl-d2/p/eval-public.f) switch the section under --load (rc 0), while tools/check.f -- and --verify-only reject the next use of the word defined after it E-UNDEFINED, rc 70: the check leaves the rendered source to the run but still resolves later names in the old section. Acceptance: the three readers give the same records and rc for both probes. Depends: a design. Claim: unassigned.
