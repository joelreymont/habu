---
title: Model engine-builder asm effects under hard hook
status: open
priority: 2
issue-type: task
created-at: "2026-06-24T17:47:16.441946+02:00"
---

Root cause: generated fixpoint sources now install the hard CHECK! hook, but src/habu/rt.f through src/habu/habu2.f are engine-builder/raw-asm emitters whose label/register/xt-execute effects are not expressible by the current checker. Fix now: tools/build-fixpoint.f and tools/bootstrap.sh emit an explicit 0 set-check / ' HOOK set-check bracket around that audited boundary; target image/sign/layout boundary effects are pinned with TRUST rows. Long-term fix: add typed checker/compiler support for the asm/codegen effect vocabulary so this generated boundary can be removed. Evidence: tools/build-fixpoint-test.f passes after the explicit boundary and trust rows.
