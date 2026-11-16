---
title: Take a package name of any length
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:18:54.994296+01:00"
---

Problem: a package name has a fixed row of PACKAGE-PATH:CAP (256) bytes (src/habu/layout.f:1296; copied at src/core/checker.f:11902, the kernel's head-row lookup src/habu/habu1.f:4850-4864, check-walk.f's rollback rows 1433-1472), while other names have no length limit. A longer `package` name throws 7154 unrendered at checker.f:12860 (bare 'uncaught throw code 7154', exit 67); a declaration's constructor package past it is rejected by type-family.f:1155 with no place, its text kept twice (generated-declaration.f:219, sumtype.f:853). Acceptance: a package name of any length is taken, by `package`, a part and a declaration's constructor package, under both loaders; E-PACKAGE-NAME-CAP and its texts are gone; test/name-length-test.f's PACKAGE-IT row states a long package name is taken. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-pub.
