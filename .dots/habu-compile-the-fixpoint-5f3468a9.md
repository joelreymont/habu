---
title: "Compile the fixpoint's stage 2 with the chosen engine"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T14:18:09.018198+03:00"
---

Problem: tools/build-fixpoint.f BF-BOOTSTRAP-STAGE compiles stage 2 with the literal bin/hb while every other path goes through BF-ENGINE$; HABU_FIXPOINT_ENGINE only re-targets the install, so a fixpoint run against a promoted private engine fails until that binary is copied over bin/hb (chain-callees lane, 2026-09-17), and a fully sandboxed fixpoint is impossible. Acceptance: stage 2 compiles with BF-ENGINE$; a fixpoint run with HABU_FIXPOINT_ENGINE set and a stale bin/hb succeeds and installs the new engine; docs/bootstrap.md states which engine each stage uses. Files: tools/build-fixpoint.f, docs/bootstrap.md. Verify: a fixpoint run with HABU_FIXPOINT_ENGINE pointing at a private engine and a deliberately stale bin/hb. Depends: none. Ownership: build tools. Claim: unassigned.
