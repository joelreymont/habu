---
title: Pass binary parameters and results through PG
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T10:48:30.962467+03:00"
---

Problem: PG:TEXT+ carries text only, so binary data cannot be passed (checked 2026-10-14). Maki's project store therefore base64-encodes every scene artifact through PostgreSQL encode/decode on every import and every /scene read. Acceptance: PG binds bytea parameters and reads bytea results as bytes (binary format), with a round-trip test of arbitrary bytes including NUL. Files: Habu PG library (owner decides). Verify: the PG suite. Depends: none. Ownership: Habu PG owner. Caller: Maki src/project-store.f scene artifacts. Claim: unassigned.
