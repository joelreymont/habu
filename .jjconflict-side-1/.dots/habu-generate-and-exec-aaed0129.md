---
title: Generate and execute one bootstrap recovery recipe
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:03:51.659946+02:00"
---

Bootstrap guidance names deleted paths, a nonexistent seed launcher, and two incompatible no-binary sources; one route cannot build from the published archive without an existing Gforth. Define one checked machine-readable recovery recipe as the sole source of commands, inputs, pinned identities, expected artifacts, and transition to the native fixpoint. Generate docs/bootstrap.md, self-host subset guidance, LLM-facing instructions, and the bootstrap skill view from that recipe. The audited tools/bootstrap.sh remains only the minimal no-binary launcher and must implement exactly the recipe boundary; no second shell workflow. Add a hermetic fixture that removes bin/hb and any ambient Gforth as applicable, executes every published command against the pinned distribution source, verifies the recovered binary, then performs two native fixpoint refreshes and byte identity. Validate every referenced path and artifact at generation time. Keep the trusted native seed route separate and explicit. Tests cover offline cache, corrupt/missing source, wrong archive type, deleted path mutation, partial recovery, and cleanup. Files: checked bootstrap recipe/generator/test, generated docs/views, audited launcher only where parity requires. Verify no-binary recovery, bootstrap check, fixpoint, path lints, host/dot lints, and full native gate.
