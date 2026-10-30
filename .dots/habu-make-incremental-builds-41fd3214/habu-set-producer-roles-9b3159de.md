---
title: Set producer roles after a cold load
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T21:55:04.650059+02:00"
closed-at: "2026-10-31T00:32:04.710007+02:00"
close-reason: landed 365c9e95 on master
---

Problem: master 97e8a61e COLD-LOAD:RUN (tools/cold-load.f) reloads src/core/source-producer.f and compiler.f fresh, leaving CHECKER-KNOWN/COMPILER-KNOWN 0; only native-build-core.f PREFIX-COMPILER-FINISH calls INSTALL-ROLES, into the target. Lane 11 rebased onto master refuses every definition at capture (14298 unknown-role, 10157 compiler-producer-unknown, 4221 checker-producer-unknown); probes /tmp/carl-l11r/roles.f (known 32) vs roles2.f (unknown 0). Acceptance: after a cold load the host's roles are the digests of what it loaded (BUILD-PRODUCER's rule), installed through INSTALL-ROLES; a warm build after cold load installs definitions; an e2e row catches the regression. Files: tools/cold-load.f, tools/build-producer.f / tools/native-build-core.f as needed. Verify: brief-fix-coldroles.md proof. Depends: none (master). Blocks lane 11 landing. Ownership: master fix lane. Workspace: .jj-ws.noindex/carl-fcold. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fcold.
