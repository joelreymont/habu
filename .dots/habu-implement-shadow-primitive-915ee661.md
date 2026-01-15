---
title: Implement shadow primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:40.429024+02:00"
---

src/runtime/primitives/package.zig: Add shadow_symbols(names, package). Create internal symbols, add to shadow-list. Dependencies: habu-implement-import-primitive-575601fb. Verify: (shadow 'foo) shadows foo.
