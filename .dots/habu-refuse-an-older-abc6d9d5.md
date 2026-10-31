---
title: Refuse an older host plainly
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T23:21:05.618276+02:00"
---

Problem: a host engine older than 4b54ed21 (no NCOMP-DISPATCH:BODY-HOOK-CELL, src/habu/layout.f $3838; COMP-CELL was $3838 before) building a current tree fails as 'uncaught throw code -8597' (E-NUNIT-PROFILE) from DFIELD (tools/native-unit-dependency.f:220) on src/core/util.f's first definition: its compiled ':' never calls the body hook, so no definition frame opens. The tree's tools resolve BODY-HOOK-CELL to $3838 and store the hook xt there, which in the old host is COMP-CELL (effect unproven). docs/bootstrap.md 98-117 names only E-UNDEFINED for an older host. Evidence: dot habu-build-master-with-ccb5492f (public hb 19:53 fails on master b7d767f5 and facda3aa-, builds 4b54ed21-). Acceptance: before the build driver stores anything into a layout cell the host may not have, it detects a host whose engine lacks the body hook and stops with a message naming the cause and the older-host route in docs/bootstrap.md; bootstrap.md names that message; a regression case runs the detection against an engine without the hook. Files: tools/native-definition-cache.f (hook arming), tools/native-build.f or native-build-core.f, docs/bootstrap.md. Depends: none. Ownership: those files. Claim: unassigned.
