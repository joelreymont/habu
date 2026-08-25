---
title: Extend type kinds for layout tensor regclass
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T20:56:48.026294+02:00"
---

Full context: design section 6.3 lists further type kinds — tuple/layout, target-register-class, tensor/memref — that need the attribute/layout substrate before they can be modeled. Extend IR-TYPE's kind ENUM, wire codes, interning, render, and fixtures when their owning stages land. Blocked by the attribute substrate (habu-intern-compiler-attrs-37cfbca5).
