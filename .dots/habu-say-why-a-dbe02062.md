---
title: Say why a checked body may not use a barred token
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T13:02:03.741910+02:00"
---

Finding (census tc2, 2026-10-31): when a checked body uses a token the checker bars from checked bodies (TRUST-DECL, TRUST, set-check, the CHECKER-* registrars), the build prints only "habu: in X: at 'TRUST-DECL'", with no error code or reason. Acceptance: the line names the code and says the token is reserved for TRUSTED: bodies, as other refusals do. Evidence: /tmp/jerry-big/trusted-census-tc2.md. Ownership: unassigned.
