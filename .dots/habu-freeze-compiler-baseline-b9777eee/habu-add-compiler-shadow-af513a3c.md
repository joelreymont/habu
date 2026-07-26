---
title: Add compiler shadow plumbing
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:56.939403+02:00"
blocks:
  - habu-add-compiler-capability-8dee4e19
---

Full context: design sections 14.2 and 15 require comparison-only native/GPU shadow plumbing after the disabled capability exists and before new artifacts exist. Add named coverage, old/new result envelopes, diagnostics, metrics, and unsupported-capability reporting with no publisher authority. Acceptance: unsupported inputs report a named open capability; absent new artifacts cannot alter old-path output; a fixture proves a hidden fallback or publication attempt fails. Dependency: the sibling Add compiler capability record dot.
