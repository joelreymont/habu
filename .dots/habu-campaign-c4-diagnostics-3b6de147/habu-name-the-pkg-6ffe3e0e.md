---
title: Name the package-name rejection in forth.md
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.205474+01:00"
closed-at: "2026-11-16T04:07:35.324540+01:00"
---

Problem: a part path over 255 bytes is rejected E-PKG-CONTEXT while a package name over 255 is E-PACKAGE-NAME-CAP, one cause with two codes; docs/forth.md:161 names E-PKG-CONTEXT. Acceptance: both print E-PACKAGE-NAME-CAP and the doc says so. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
